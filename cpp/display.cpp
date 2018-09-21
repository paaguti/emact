#if     !defined(lint)
static  char rcsid[] = "$Id: display.cpp,v 1.33 2018/09/04 16:02:31 jullien Exp $";
#endif

/*
 * This  program  is  free  software;  you can redistribute it and/or
 * modify  it  under  the  terms of the GNU General Public License as
 * published  by  the  Free  Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This  program  is  distributed in the hope that it will be useful,
 * but  WITHOUT ANY WARRANTY;  without  even the implied  warranty of
 * MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You  should have received a copy of the GNU General Public License
 * along  with  this  program;  if  not,  write  to the Free Software
 * Foundation,  Inc.,  59  Temple  Place  -  Suite  330,  Boston,  MA
 * 02111-1307, USA.
 */

/*
 * The functions in this file handle redisplay.  There are two halves,
 * the ones that update the virtual display screen, and the ones that
 * make the physical display screen the same as the virtual display
 * screen.  These functions use hints that are left in the windows by
 * the commands.
 */

#include "emacs.h"

static void computecursor();
static void updateline(int row, EMCHAR* vline, EMCHAR* pline);

Terminal* tt{nullptr};

bool          DISPLAY::_mouse{false}; // Mouse flag
int           DISPLAY::_currow{0};
int           DISPLAY::_curcol{0};
EMCHAR        DISPLAY::_curchar;
DISPLAY::Sync DISPLAY::_sgarbf{DISPLAY::Sync::GARBAGE};

extern const EMCHAR* version;           /* Current version              */

/*
 *VIDEO structure used by redisplay.
 */

class VIDEO {
 public:
  bool    changed; // Flags
  EMCHAR* text;    // Screen data.

 public:
  explicit VIDEO(size_t size) {
    text = new EMCHAR[size];
    for (size_t i{0}; i < size; ++i) {
      text[i] = (EMCHAR)' ';
    }
    changed = false;
  }

  ~VIDEO() {
    delete[] text;
  }

  EMCHAR get(int n) {
    return text[n];
  }
  void putc(int n, EMCHAR c) {
    text[n] = c;
  }

  /*
   * Write a character to the virtual screen.  The virtual row and
   * column are updated.  If the line is too long put a "\" in the
   * last column.  This routine only puts printing characters into
   * the  virtual  terminal  buffers.  Only  column  overflow   is
   * checked.
   */

  static void
  vtputc(int c) {
    auto vp(VIDEO::vscreen[VIDEO::row]);
  
    if (VIDEO::col >= TTYncol) {
      vp->putc(TTYncol - 1, '\\');
    } else if (c == '\t') {
      do {
        VIDEO::vtputc((int)' ');
      } while ((VIDEO::col % opt::tab_display) && VIDEO::col < TTYncol);
    } else if (!self_insert(c)) {
      if (opt::set_show_graphic || (DISPLAY::_mouse && (c == 24 || c == 25))) {
        vp->putc(VIDEO::col++, (EMCHAR)c);
      } else {
        VIDEO::vtputc((int)'^');
        VIDEO::vtputc((int)c ^ 0x40);
      }
    } else {
      vp->putc(VIDEO::col++, (EMCHAR)c);
    }
  }

  /*
   * Set the virtual cursor to the specified row and column on the
   * virtual  screen.  There  is no checking for nonsense  values;
   * this might be a good idea during the early stages.
   */
  static void
  vtmove(int newRow, int newCol) {
    VIDEO::row = newRow;
    VIDEO::col = newCol;
  }

  /*
   * Erase  from the end of the software cursor to the end of  the
   * line on which the software cursor is located.
   */
  static void
  vteeol() {
    auto vp(VIDEO::vscreen[VIDEO::row]);
  
    while (VIDEO::col < TTYncol) {
      vp->putc(VIDEO::col++, ' ');
    }
  }

  static constexpr const short CHANGED = 0x0001; /* Changed. */

  static bool    ok;         /* VT ready to run */
  static int     row;        /* Row location of SW cursor    */
  static int     col;        /* Column location of SW cursor */
  static VIDEO** vscreen;    /* Virtual screen.              */
  static VIDEO** pscreen;    /* Physical screen.             */
};

bool    VIDEO::ok{false};
int     VIDEO::row{0};
int     VIDEO::col{0};
VIDEO** VIDEO::vscreen{nullptr};
VIDEO** VIDEO::pscreen{nullptr};

#define OFFSET          2
#define GRIPCHAR        '#'
#define FOOTCHAR        '-'
#define VERSION_LENGTH  6       /* the six letters  'E' 'm' 'A' 'C' 'T' ':' */

long    current_row;                   /* Current row text position    */
long    current_col;                   /* Current col text position    */

/*
 * Initialize the data structures used by the display code.  The edge
 * vectors used to access the screens are set up.  The operating
 * system's terminal I/O channel is set up.  All the other things get
 * initialized at compile time.  The original window has "WFCHG" set,
 * so that it will get completely redrawn on the first call to
 * "update".
 */

DISPLAY::DISPLAY() {
  VIDEO::vscreen = new VIDEO*[TTYnrow + 1];
  VIDEO::pscreen = new VIDEO*[TTYnrow + 1];

  for (int i(0); i <= TTYnrow; ++i) {
    VIDEO::vscreen[i] = new VIDEO(TTYncol + 1);
    VIDEO::pscreen[i] = new VIDEO(TTYncol + 1);
  }
  
  VIDEO::ok = true;
  
  mlerase();
}

DISPLAY::~DISPLAY() {
  if (VIDEO::vscreen == nullptr || VIDEO::pscreen == nullptr) {
    return;
  }
  
  for (int i(0); i <= TTYnrow; ++i) {
    delete VIDEO::vscreen[i];
    delete VIDEO::pscreen[i];
  }
  
  delete VIDEO::vscreen;
  delete VIDEO::pscreen;
  
  VIDEO::ok = false;
}

/*
 * Return VIDEO::vscreen (may be need by some terminal)
 */

const EMCHAR*
DISPLAY::text(int y) const noexcept {
  return VIDEO::vscreen[y]->text;
}

/*
 * Check if vt has been initialized.  Return T is so or NIL otherwise.
 */

bool
DISPLAY::running() const noexcept {
  return VIDEO::ok;
}

/*
 * Clean up the virtual terminal system, in anticipation for a return
 * to the operating system.  Move up to the first line and clear it
 * out (the next system prompt will be written in the line). Shut down
 * the channel to the terminal.
 */

void
DISPLAY::tidy() const noexcept {
  TTYmove(0, 0);
  TTYeop();
  TTYflush();
  TTYclose();
}

void
DISPLAY::statputc(int n, int c) {
  if (n < TTYncol) {
    VIDEO::vscreen[TTYnrow]->putc(n, (EMCHAR)c);
    VIDEO::vscreen[TTYnrow]->changed = true;
  }
}

static void
computecursor() {
  const auto* lp = curwp->topline();
  DISPLAY::_currow = curwp->toprow();
  
  const auto& dot(curwp->getDot());
  const auto clp(dot.line());
  const auto cbo(dot.pos());

  while (lp != clp) {
    ++DISPLAY::_currow;
    lp = lp->forw();
  }
  
  DISPLAY::_curcol = 0;
  
  for (int i(0); i < cbo; ++i) {
    auto c = lp->get(i);
    if (c == '\t') {
      do {
        ++DISPLAY::_curcol;
      } while (DISPLAY::_curcol % opt::tab_display);
    } else {
      if (!opt::set_show_graphic && !self_insert(c)) {
        ++DISPLAY::_curcol;
      }
      ++DISPLAY::_curcol;
    }
  }
  
  if (DISPLAY::_curcol >= TTYncol) {
    DISPLAY::_curcol = TTYncol - 1;
  }
  
  DISPLAY::_curchar = VIDEO::vscreen[DISPLAY::_currow]->get(DISPLAY::_curcol);
}

/*
 * Make  sure that the display is right.  This is a  three  part
 * process.  First,  scan through all of the windows looking for
 * dirty  ones.  Check  the  framing,  and refresh  the  screen.
 * Second,  make sure that "_currow" and "curcol" are correct for
 * the  current window.  Third,  make the virtual  and  physical
 * screens the same.
 */

void
DISPLAY::refresh(WINSCR* wp) {
  EDLINE* lp;
  bool    out = false;
  int     i;
  int     j;
  
  /*
   * If not force reframe, check the framing.
   */
  
  if ((wp->getFlags() & WINSCR::WFFORCE) == 0) {
    lp = wp->topline();
    for (i = 0; i < wp->rows(); ++i) {
      if (lp == wp->line()) {
        out = true; /* line inside window */
        break;
      }

      if (lp == wp->buffer()->lastline()) {
        break;
      }

      lp = lp->forw();
    }
  }
  
  /*
   * Not acceptable, better compute a new value for the line at the
   * top of the window. Then set the "WINSCR::WFHARD" flag to force full
   * redraw.
   */
  
  if (out == false) {
    if ((i = wp->force()) > 0) {
      if (--i >= wp->rows()) {
        i = wp->rows() - 1;
      }
    } else if (i < 0 && (i += wp->rows()) < 0) {
      i = 0;
    } else {
      i = wp->rows() / 2;
    }
    
    lp = wp->line();
    while (i-- && lp->back() != wp->buffer()->lastline()) {
      lp = lp->back();
    }
    
    wp->setTopline(lp);
    wp->setFlags(WINSCR::WFHARD);   /* Force full.  */
  }
  
  /*
   * Try to use reduced update.  Mode line update has its own special
   * flag. The fast update is used if the only thing to do is within
   * the line editing.
   */
  
  lp = wp->topline();
  i  = (int)wp->toprow();
  if ((wp->getFlags() & ~WINSCR::WFMODE) == WINSCR::WFEDIT) {
    j = wp->rows() + i;
    while ((lp != wp->line()) && (j > i)) {
      ++i;
      lp = lp->forw();
    }
    if (j > i) {
      VIDEO::vscreen[i]->changed = true;
      VIDEO::vtmove(i, 0);
      for (j = 0; j < lp->length(); ++j) {
        VIDEO::vtputc((int)lp->get(j));
      }
      VIDEO::vteeol();
    }
  } else if ((wp->getFlags() & (WINSCR::WFEDIT|WINSCR::WFHARD)) != 0) {
    while (i < ((int)wp->toprow() + (int)wp->rows())) {
      VIDEO::vscreen[i]->changed = true;
      VIDEO::vtmove(i, 0);
      if (lp != wp->buffer()->lastline()) {
        for (j = 0; j < lp->length(); ++j) {
          VIDEO::vtputc((int)lp->get(j));
        }
        lp = lp->forw();
      }
      VIDEO::vteeol();
      ++i;
    }
  }
  
  display->modeline(wp);
  wp->setFlags(WINSCR::WFCLEAR);
}

void
DISPLAY::update(DISPLAY::Mode mode) {
  static BUFFER* oldbp{nullptr};
  
  if (mode == Mode::REFRESH) {
    DISPLAY::garbaged();
  }

  if (!VIDEO::ok) {
    return;
  }
  
  for (auto wp : WINSCR::list()) {
    /*
     * Look at any window with update flags set on.
     */
    if (wp->getFlags() != 0) {
      refresh(wp);
    }
  }
  
  /*
   * Always recompute the row and column number of the hardware
   * cursor.  This is the only update for simple moves.
   */
  
  if (mode == Mode::MINIBUF) {
    DISPLAY::_curcol = mlcursor(); // minibuf cursor position.
    DISPLAY::_currow = TTYnrow;
    DISPLAY::_curchar = ' ';
  } else {
    computecursor();
  }
  
  /*
   * Special hacking if the screen is garbage.  Clear the hardware
   * screen, and update your copy to agree with it.  Set all the
   * virtual screen change bits, to force a full update.
   */
  
  if (_sgarbf != Sync::SYNCHRONIZED) {
    for (int i = 0; i <= TTYnrow; ++i) {
      VIDEO::vscreen[i]->changed = true;
      auto vp1(VIDEO::pscreen[i]);
      for (int j = 0; j < TTYncol; ++j) {
        vp1->putc(j, ' ');
      }
    }
    if (_sgarbf != Sync::EXPOSE) {
      TTYmove(0, 0);
      TTYeop();
    }
    _sgarbf = Sync::SYNCHRONIZED;   /* Erase-page clears    */
  }
  
  /*
   * Make sure that the physical and virtual displays agree.  Unlike
   * before, the updateline code is only called with a line that has
   * been updated for sure.
   */
  
  for (int i = 0; i <= TTYnrow; ++i) {
    auto vp1(VIDEO::vscreen[i]);
    if (vp1->changed) {
      auto vp2(VIDEO::pscreen[i]);
      updateline(i, vp1->text, vp2->text);
      vp1->changed = false;
    }
  }
  
  /*
   * Update the current buffer name when needed
   */
  
  if (curbp != oldbp) {
    (void)WDGtitle(curbp->bufname(), curbp->filename());
    oldbp = curbp;
  }
  
  /*
   * Finally, update the hardware cursor, char at cursor and flush out
   * buffers
   */
  
  TTYmove(DISPLAY::_currow, DISPLAY::_curcol);
  TTYflush();
}

/*
 * Update a single line. This does not know how to use insert or
 * delete character sequences.  Update the physical row and column
 * variables.
 */

static void
updateline(int row, EMCHAR* nline, EMCHAR* pline) {
#if defined(_UNICODE)
  int     stflag;
#if defined(_POSIX_C_SOURCE)
  static constexpr size_t MAX_OUTPUT{1024};
  char outbuf[MAX_OUTPUT];
  emwcstombs(outbuf, nline, MAX_OUTPUT);
#else
  auto outbuf = nline;
#endif
  
  (void)pline;
  
  stflag = (emstrncmp(nline + OFFSET, version, VERSION_LENGTH) == 0);
  TTYmove(row, 0);
  
  if (stflag) {
    TTYinverse();
    TTYputs(outbuf, TTYncol);
    TTYnormal();
  } else {
    TTYputs(outbuf, TTYncol);
  }
#else
  auto cp1 = nline;
  auto cp2 = pline;
  EMCHAR* cp3;
  EMCHAR* cp4;
  EMCHAR* cp5;
  int     count;
  int     stflag;
  
  stflag = (emstrncmp(cp1+OFFSET, version, VERSION_LENGTH) == 0);
  
  /*
   * Compute the left match.
   */
  
  while (cp1 != (nline + TTYncol) && cp1[0] == cp2[0]) {
    ++cp1;
    ++cp2;
  }
  
  if (cp1 == (nline + TTYncol)) {
    /*
     * Easy  an update is made outside the visible bounds
     * of screen.  This can still happen,  even though we
     * only  call  this routine on changed lines.  A hard
     * update  is  always  done  when  a  line splits,  a
     * massive  change is done,  or a buffer is displayed
     * twice.  This  optimizes  out  most  of  the excess
     * updating.  A  lot of computes are used,  but these
     * tend  to  be  hard  operations  that  do  a lot of
     * update. Nothing to do.
     */
    return;
  }
  
  /*
   * Compute right match and flag non blank changes
   */
  
  auto nbflag = false;
  cp3 = nline + TTYncol;
  cp4 = pline + TTYncol;
  
  while (cp3[-1] == cp4[-1]) {
    --cp3;
    --cp4;
    if (cp3[0] != ' ') {  /* Note if any nonblank */
      nbflag = true;      /* in right match.      */
    }
  }
  
  cp5 = cp3;
  
  if (!nbflag) {
    /*
     * Can we perform an erase to EOL ?
     */
    
    while ((cp5 - cp1) && (cp5[-1] == ' ')) {
      --cp5;
    }
    
    /*
     * Usefull only if erase is fewer characters.
     */
    
    if ((cp3 - cp5) <= 3) {
      cp5 = cp3;
    }
  }
  
  /*
   * Go to start of line.
   */
  
  TTYmove(row, (int)(cp1 - nline));
  
  if (stflag) {
    TTYinverse();
  }
  
  if ((count = (int)(cp5 - cp1)) > 0) {
    /*
     * Display changes and update old line.
     */
    TTYputs(cp1, count);
    
    while (count--) {
      *cp2++ = *cp1++;
    }
  }
  
  if (cp5 != cp3) {
    /*
     * Erase and update old line.
     */
    TTYeol();
    while (cp1 - cp3) {
      *cp2++ = *cp1++;
    }
  }
  
  if (stflag) {
    TTYmove(row + 1, 0);
    TTYnormal();
  }
#endif
}

void
DISPLAY::modeline(WINSCR* wp) {
  EMCHAR  buf[8];
  int     i;
  
  auto pos = 0; /* Number of chars    */

  auto modeputc = [&pos](int c) {
    VIDEO::vtputc((int)c);
    ++pos;
  };

  auto modeputs = [&pos](const EMCHAR* text) {
    while (*text != 0) {
      VIDEO::vtputc((int)*text++);
      ++pos;
    }
  };

  auto row = (int)wp->toprow() + (int)wp->rows(); /* Location.          */
  VIDEO::vscreen[row]->changed = true;              /* Redraw next time.  */
  VIDEO::vtmove(row, 0);                            /* Seek to right line.*/

  modeputc(FOOTCHAR);
  modeputc(' ');
  modeputs(version);
  
  auto bp = wp->buffer();

  switch (bp->editMode()) {
  case EDITMODE::ASMODE      : modeputs(ECSTR(" (Assembler"));   break;
  case EDITMODE::BUFFERMODE  : modeputs(ECSTR(" (Buffer Menu")); break;
  case EDITMODE::CMODE       : modeputs(ECSTR(" (C"));           break;
  case EDITMODE::CPPMODE     : modeputs(ECSTR(" (C++"));         break;
  case EDITMODE::CSHARPMODE  : modeputs(ECSTR(" (C#"));          break;
  case EDITMODE::DIRED       : modeputs(ECSTR(" (Dired"));       break;
  case EDITMODE::SGMLMODE    : modeputs(ECSTR(" (SGML"));        break;
  case EDITMODE::FORTRANMODE : modeputs(ECSTR(" (Fortran"));     break;
  case EDITMODE::JAVAMODE    : modeputs(ECSTR(" (Java"));        break;
  case EDITMODE::LISPMODE    : modeputs(ECSTR(" (Lisp"));        break;
  case EDITMODE::PASCALMODE  : modeputs(ECSTR(" (Pascal"));      break;
  case EDITMODE::PROLOGMODE  : modeputs(ECSTR(" (Prolog"));      break;
  case EDITMODE::PERLMODE    : modeputs(ECSTR(" (Perl"));        break;
  case EDITMODE::SHELLMODE   : modeputs(ECSTR(" (Shell"));       break;
  default                    : modeputs(ECSTR(" (Fundamental")); break;
  }
  
  if (opt::replace_mode) {
    modeputs(ECSTR(" Ovwrt"));
  }
  
  if (opt::auto_fill_mode) {
    modeputs(ECSTR(" Fill"));
  }

  switch (bp->encoding()) {
  case ENCODING::EMASCII:
    if (bp->binary()) {
      modeputs(ECSTR(" Bin"));
    }
    break;
  case ENCODING::EMUTF8:
    modeputs(ECSTR(" UTF-8"));
    break;
  case ENCODING::EMUTF16:
    modeputs(ECSTR(" UTF-16"));
    break;
  default:
    break;
  }
  
  modeputs(ECSTR(") "));
  
  if (bp->readonly()) {
    /*
     * "%" if readonly.
     */
    modeputc('%');
  }
  
  if (bp->isChanged()) {
    /*
     * "*" if changed.
     */
    modeputc('*');
  }
  
  modeputs(bp->bufname());
  
  if (opt::line_number_mode) {
    EMCHAR num[32];  // enough for a 64bit integer.
    
    auto maxlen = 1L;
    auto curlen = 0L;
    
    for (auto lp(bp->firstline()); lp != bp->lastline(); lp = lp->forw()) {
      if (lp == curwp->line()) {
        curlen = maxlen;
      }
      maxlen++;
    }
    
    modeputs(ECSTR(" - L"));
    
    if (curlen != 0) {
      (void)emsprintf1(num, ECSTR("%ld "), curlen);
    } else {
      (void)emsprintf1(num, ECSTR("%ld "), maxlen);
    }
 
    /*
     * take care of UNICODE
     */
    
    for (i = 0; num[i] != '\000'; ++i) {
      buf[i] = (EMCHAR)num[i];
    }
    
    buf[i] = '\000';
    
    modeputs(buf);
    
    if (curlen == 0L) {
      current_row = maxlen;
      modeputs(ECSTR("(100%)"));
    } else {
      current_row = curlen;
      auto average = (int)((curlen * 100) / maxlen);
      modeputc('(');
      modeputc((EMCHAR)(average / 10 + (int)'0'));
      modeputc((EMCHAR)(average % 10 + (int)'0'));
      modeputc('%');
      modeputc(')');
    }
  }
  
#if defined(_DISPLAY_FILENAME)
  if (bp->filename()) {
    /*
     *      Complete file name with PATH.
     */
    
    modeputs(ECSTR(" - "));
    modeputs(bp->filename());
  }
  
  modeputc(' ');
#endif
  
  if (_mouse) {
    while (pos < (TTYncol - 8)) {
      modeputc(' ');
    }
    
    VIDEO::col = pos = TTYncol - 8;
    
    modeputs(ECSTR(" Up Dn "));
    modeputc((int)GRIPCHAR);
  } else {
    while (pos < (TTYncol - 1)) {
      modeputc(' ');
    }
    
    VIDEO::col = TTYncol - 1;
    modeputc((int)FOOTCHAR);
  }
}
