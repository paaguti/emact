#if     !defined(lint)
static auto rcsid("$Id: buffer.cpp,v 1.23 2018/09/04 05:13:08 jullien Exp $");
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
 * Buffer  management.  Some of the functions are internal,  and
 * some are actually attached to user keys.  Like everyone else,
 * they set hints for the display system.
 */

#include "./emacs.h"
#include "./Editor.h"
#include "./Buffer.h"
#include "./Completion.h"
#include "./EditWindow.h"
#include "./Line.h"
#include "./Search.h"
#include "./Terminal.h"
#include "./Widget.h"

extern Completion complete;  // Automatic completion
extern Widget* widget;       // Widgets tools

static EMCHAR* bufmatch(const EMCHAR* prompt, EMCHAR* buffer);
static Buffer* getbpcmd(EMCHAR* buf);
static void    longtostrtr(EMCHAR* buf, int width, size_t num);
static bool    makelist(Buffer* blp);
static bool    savebname(const EMCHAR* bname);

std::list<Buffer*> Buffer::_blist;

static constexpr auto BufferPOS(13);  // Buffer name is at pos 13

#define Buffer_DEBUG    1

Line*
Buffer::firstline() {
  return _linep->forw();
}

Buffer::Buffer(const EMCHAR* bname, bool bflag, EDITMODE mode)
  : _emode{mode},
    _flag{bflag},
    _binary{opt::binary_mode} {
  auto lp(Line::alloc());

  setDot(lp, 0);
  _linep = lp;

  (void)emstrcpy(_fname, ECSTR(""));
  (void)emstrcpy(_bname, bname);

  _blist.push_front(this);
}

/*
 * This function is called by EditWindow::[dis]connect to ensure that
 * buffers count() are always correct.  Valid only when Buffer_DEBUG
 * has been defined.
 */

void
Buffer::validitycheck(const char* msg) {
#if defined(Buffer_DEBUG)
  for (auto bp : Buffer::list()) {
    int count = 0;

    for (auto wp : EditWindow::list()) {
      if (wp->buffer() == bp) {
        ++count;
      }
    }

    if (count != bp->count()) {
      printf("%s: wrong buffer computed count %d vs. bp->count %d\n",
             msg, count, bp->count());
      Editor::error(__FILE__, __LINE__, ECSTR("wrong buffer count"));
    }
  }
#endif
}

/*
 * Make  newbp the current buffer and reorder the buffer list so
 * that  kill-buffer and switch-buffer can select default buffer
 * to kill or to use.
 */

void
Buffer::ontop() noexcept {
  /*
   *      restack buffers (add the new buffer on top)
   */
  _blist.remove(this);
  _blist.push_front(this);

  curbp = this;
}

/*
 * Show  an  internal  buffer  'bp'.  If  this  buffer in not on
 * screen,  split  the  current  window  and  display  it.  This
 * function  returns  the window that display the buffer or nullptr
 * if it fails.
 */

EditWindow*
Buffer::show() noexcept {
  if (this->count() == 0) { /* Not on screen yet. */
    auto wp = EditWindow::popup();
    if (wp != nullptr) {
      (void)wp->connect(this);
    }
    return wp;
  } else {
    for (auto wp : EditWindow::list()) {
      if (wp->buffer() == this) {
        (void)wp->connect(this);
        return wp;
      }
    }
  }

  return nullptr;
}

/*
 * Update all windows when a buffer change it's edit mode.
 */

void
Buffer::updatemodes() noexcept {
  int flag = 0;

  if (curbp->count() != 1) {
    /* Ensure hard.         */
    flag |= EditWindow::WFHARD;
  }

  flag |= EditWindow::WFMODE; /* update mode lines.   */

  for (auto wp : EditWindow::list()) {
    if (wp->buffer() == curbp) {
      wp->setFlags(flag);
    }
  }
}

Buffer*
Buffer::find(const EMCHAR* bname, bool cflag, EDITMODE mode) {
  for (auto bp : Buffer::list()) {
    if (emstrcmp(bname, bp->bufname()) == 0) {
      return bp;
    }
  }

  if (cflag) {
    auto bp = new Buffer(bname, false, mode);
    if (bp == nullptr) {
      WDGwrite(ECSTR("Can't create buffer %s"), bname);
      return nullptr;
    }

    return bp;
  } else {
    return nullptr;
  }
}

/*
 * This  routine blows away all of the text in a buffer.  If the
 * buffer  is  marked as changed then we ask if it is ok to blow
 * it  away;  this is to save the user the grief of losing text.
 * The  window chain is nearly always wrong if this gets called;
 * the  caller  must  arrange for the updates that are required.
 * Return true if everything looks good.
 */

bool
Buffer::clear() noexcept {
  Line* lp;

  if (this->isChanged() && WDGyn(ECSTR("Discard changes? ")) != T) {
    return false;
  }

  this->setChanged(false);

  while ((lp = firstline()) != lastline()) {
    Line::free(lp);
  }

  setDot(_linep, 0);    // Fix "."
  setMark(nullptr, 0);  // Invalidate "mark"

  return true;
}

/*
 * This  command  makes  the window with buffer "bp" the current
 * window. Used internally by other functions.
 */

bool
Buffer::usewindow() const noexcept {
  for (auto wp : EditWindow::list()) {
    if (wp->buffer() == this) {
      wp->current();
      return true;
    }
  }

  return false;
}

/*
 * Routine  that  really  implement  the kill-buffer,  'bp' is a
 * valid pointer to a buffer.
 */

bool
Buffer::discard() noexcept {
  if (this->isChanged()) {
    EMCHAR buf[NLINE];

    (void)emstrcpy(buf, ECSTR("Discard changes made to buffer "));
    (void)emstrcat(buf, this->bufname());
    (void)emstrcat(buf, ECSTR("?"));
    if (WDGyn(buf) != T) {
      return false;
    } else {
      this->setChanged(false);   /* Don't complain! */
    }
  }

  /*
   * Try  to  find in bp1 the first buffer not displayed or use
   * the first buffer in the list which is not this.
   */

  auto bp1 = this;
  auto bp2 = this;

  for (auto bp : Buffer::list()) {
    bp2 = bp;
    if (bp2 == this) {
      continue;
    }
    if (bp2->count() == 0) {
      bp1 = bp2; /* not displayed best guest */
      break;
    }
    if (bp1 == this) {
      bp1 = bp2; /* bp2 is a better guest    */
    }
  }

  if (bp1 == this) {
    /*
     * Only one buffer
     */
    term->beep();
    WDGmessage(ECSTR("Only one buffer"));
    return false;
  }

  /*
   * Replace   "this"  with  "bp1"  for  windows  all  on  screen
   * containing buffer "this".
   */

  for (auto wp : EditWindow::list()) {
    if (wp->buffer() == this) {
      (void)wp->connect(bp1);
    }
  }

  if (!this->clear()) {
    return false;
  }

  Line::dispose(this->_linep);          /* Release header line. */

  _blist.remove(this);
  delete this;
  return true;
}

/*
 * This  routine  gets  called  when  a  character is changed in
 * place  in the current buffer.  It updates all of the required
 * flags  in  the  buffer  and  window system.  The flag used is
 * passed  as  an  argument; if the buffer is being displayed in
 * more  than  1 window we change EDIT to HARD.  Set MODE if the
 * mode line needs to be updated (the "*" has to be set).
 */

void
Buffer::change(int flag) {
  if (curbp->count() != 1) {
    /* Ensure hard. */
    flag = EditWindow::WFHARD;
  }

  if (!curbp->isChanged()) {    /* First change, so     */
    flag |= EditWindow::WFMODE;     /* update mode lines.   */
    curbp->setChanged(true);
  }

  for (auto wp : EditWindow::list()) {
    if (wp->buffer() == curbp) {
      wp->setFlags(flag);
    }
  }
}

/*
 * Look through the list of buffers.  Returns true if there are any
 * changed buffers.  Buffers that hold magic internal stuff are not
 * considered; who cares if the list of buffer names is hacked. Return
 * false if no buffers have been changed.
 */
bool
Buffer::anycb(ANYCB flag) {
  static constexpr auto ANYHLP =
    ECSTR("y = save, n = skip, ! = save all, . = save and exit, q = exit");

  bool res = false;
  auto alert = false;
  auto saveall = false;

  for (auto bp : Buffer::list()) {
    if (!bp->isChanged()) {
      continue;
    }

#if defined(_IGNORE_SCRATCH)
    if (emstrcmp(bp->bufname(), BUF_SCRATCH) == 0) {
      continue;
    }
#endif

    if (flag == ANYCB::CHECK) {
      return true;
    }

    if ((flag == ANYCB::PROMPT) || opt::confirm_unsaved_buffer) {
      EMCHAR buf[NFILEN];
      auto valid = false;

      if (saveall) {
        savebname(bp->bufname());
        continue;
      }

      if (!alert) {
        alert = true; /* one beep only the first time */
        term->beep();
      }

      while (!valid) {
        (void)emstrcpy(buf, ECSTR("Save File "));
        (void)emstrcat(buf, bp->filename());
        (void)emstrcat(buf, ECSTR(" ? (y, n, !, ., q)"));

        WDGwrite(ECSTR("%s"), buf);
        switch (term->get()) {
        case 0x07:
          (void)Editor::ctrlg();
          WDGmessage(ECSTR("Quit"));
          return true;
        case ' ' :
        case 'y' :
        case 'Y' :
          valid = true;
          savebname(bp->bufname());
          break;
        case 'n' :
        case 'N' :
          valid = true;
          res = true;
          break;
        case '!' :
          valid = true;
          savebname(bp->bufname());
          saveall = true;
          break;
        case '.' :
          savebname(bp->bufname());
          return res;
        case 'q' :
        case 'Q' :
        case 0x1B:
          return res;
        default  :
          (void)Editor::ctrlg();
          WDGwrite(ANYHLP);
          Search::wait(5);
        }
      }
    } else {
      return true;
    }
  }

  return res;
}

/*
 * Long integer to ascii conversion (right justified).
 */

static void
longtostrtr(EMCHAR* buf, int width, size_t num) {
  buf[width] = 0;                         /* End of string.       */
  while (num >= 10 && width >= 2) {
    buf[--width] = (EMCHAR)((num % 10) + '0');
    num /= 10;
  }
  buf[--width] = (EMCHAR)(num + '0');
  while (width) {
    buf[--width] = ' ';
  }
}

/*
 * This routine rebuilds the text in the special secret buffer that
 * holds the buffer list.  It is called by the list buffers command.
 * Return true if everything works.  Return false if there is an error
 * (if there is no memory).
 */

static bool
makelist(Buffer *blp) {
  EMCHAR* cp1;
  EMCHAR* cp2;
  size_t  nbytes;
  int     c;
  EMCHAR  len[6+1];
  EMCHAR  line[NLINE];

  blp->setChanged(false);          /* Don't complain!      */
  blp->setReadonly(true);          /* Can't modify buffer  */

  if (!blp->clear()) {
    /* Blow old text away   */
    return false;
  }

  (void)emstrcpy(blp->filename(), ECSTR(""));

  Line::append(blp, ECSTR(" MRBE   Size Buffer           Edit-Mode   File"));
  Line::append(blp, ECSTR(" ----   ---- ------           ---------   ----"));

  for (auto bp : Buffer::list()) {
    cp1 = &line[0];                 /* Start at left edge   */
    if (bp == curbp) {
      *cp1++ = '.';
    } else {
      *cp1++ = ' ';
    }
    if (bp->isChanged()) {
      /* "*" if changed       */
      *cp1++ = '*';
    } else {
      *cp1++ = ' ';
    }
    if (bp->readonly()) {
      /* "%" if read only     */
      *cp1++ = '%';
    } else {
      *cp1++ = ' ';
    }
    if (bp->binary()) {
      /* "b" if binary mode   */
      *cp1++ = 'b';           /* "b" if binary mode   */
    } else {
      *cp1++ = ' ';
    }
#if defined(UNICODE)
    if (bp->encoding() == ENCODING::EMUTF16) {      /* "w" if UTF-16 mode   */
      *cp1++ = 'w';
    } else if (bp->encoding() == ENCODING::EMUTF8) {
      /* "u" if UTF-8 mode    */
      *cp1++ = 'u';
    } else {
      *cp1++ = 'a';           /* "a" if ascii mode    */
    }
#else
    *cp1++ = ' ';
#endif
    *cp1++ = ' ';                   /* Gap.                 */
    nbytes = 0;                     /* Count bytes in buf.  */
    for (auto lp = bp->firstline(); lp != bp->lastline(); lp = lp->forw()) {
      nbytes += lp->length() + 1;
    }
    /* 6 digit buffer size. */
    longtostrtr(len, 6, (size_t)nbytes * sizeof(EMCHAR));
    cp2 = &len[0];
    while ((c = *cp2++) != 0) {
      *cp1++ = (EMCHAR)c;
    }
    *cp1++ = ' ';                   /* Gap.                 */

    if (cp1 != (&line[0] + BufferPOS)) {
      term->beep();
      WDGwrite(ECSTR("#<BufferPOS: invalid constant>"));
      term->get();
    }

    cp2 = bp->bufname();
    while ((c = *cp2++) != 0) {
      *cp1++ = (EMCHAR)c;
    }

    while (cp1 - &line[5 + 1 + 6 + 1 + Buffer::NBUFN + 1]) {
      *cp1++ = ' ';
    }

    switch (bp->editMode()) {
    case EDITMODE::ASMODE      : cp2 = ECSTR("Assembler   "); break;
    case EDITMODE::BufferMODE  : cp2 = ECSTR("Buffer Menu "); break;
    case EDITMODE::CMODE       : cp2 = ECSTR("C           "); break;
    case EDITMODE::CPPMODE     : cp2 = ECSTR("C++         "); break;
    case EDITMODE::CSHARPMODE  : cp2 = ECSTR("C#          "); break;
    case EDITMODE::DIRED       : cp2 = ECSTR("Dired       "); break;
    case EDITMODE::FORTRANMODE : cp2 = ECSTR("Fortran     "); break;
    case EDITMODE::JAVAMODE    : cp2 = ECSTR("Java        "); break;
    case EDITMODE::LISPMODE    : cp2 = ECSTR("Lisp        "); break;
    case EDITMODE::PASCALMODE  : cp2 = ECSTR("Pascal      "); break;
    case EDITMODE::PROLOGMODE  : cp2 = ECSTR("Prolog      "); break;
    case EDITMODE::PYTHONMODE  : cp2 = ECSTR("Python      "); break;
    case EDITMODE::PERLMODE    : cp2 = ECSTR("Perl        "); break;
    case EDITMODE::SGMLMODE    : cp2 = ECSTR("SGML        "); break;
    case EDITMODE::SHELLMODE   : cp2 = ECSTR("Shell       "); break;
    default                    : cp2 = ECSTR("Fundamental ");
    }

    while (*cp2) {
      *cp1++ = *cp2++;
    }

    cp2 = bp->filename();

    if (*cp2) {
      while ((c = *cp2++) != 0) {
        if (cp1 < &line[NLINE - 1]) {
          *cp1++ = (EMCHAR)c;
        }
      }
    }

    *cp1 = '\0';
    Line::append(blp, line);
  }
  return true;
}

static bool
savebname(const EMCHAR* bname) {
  Buffer* newbp;
  auto oldbp = curbp;
  auto res  = true;

  if ((newbp = Buffer::find(bname, false)) == nullptr) {
    return false;
  }

  curbp = newbp;
  if (Editor::fileSave() != T) {
    res = false;
  }

  curbp = oldbp;

  return res;
}

/*
 * Function to select a buffer from minibuffer
 */

static EMCHAR*
bufmatch(const EMCHAR* prompt, EMCHAR* buffer) {
  auto len = emstrlen(buffer);

  for (auto bp : Buffer::list()) {
    if (len == 0 || !emstrncmp(bp->bufname(), buffer, len)) {
      WDGupdate(prompt, bp->bufname());
      switch (term->get()) {
      case 0x07:
        complete.setStatus(Completion::Status::COMPLETE_ABORT);
        WDGwrite(ECSTR("Quit"));
        return nullptr;
      case 0x0D:
      case 0x0A:
      case 'y' :
      case 'Y' :
        return bp->bufname();
      }
    }
  }

  complete.setStatus(Completion::Status::COMPLETE_AGAIN);
  term->beep();
  return buffer;
}

/*
 * Attach a buffer to a window.  The values of dot and mark come from
 * the buffer if the use count is 0.  Otherwise, they come from some
 * other window.
 */

CMD
Buffer::usebuffer() {
  CMD     s;
  EMCHAR  bufn[Buffer::NBUFN];
  EMCHAR  prompt[NLINE];

  complete = bufmatch;

  auto bp1 = Buffer::list().front();
  for (auto bp : Buffer::list()) {
    if (bp->count() == 0) {
      /* Not on screen yet.   */
      bp1 = bp;
      break;
    }
  }

  (void)emstrcpy(prompt, ECSTR("Switch to buffer: (default "));
  (void)emstrcat(prompt, bp1->bufname());
  (void)emstrcat(prompt, ECSTR(") "));

  bufn[0] = '\000';

  if ((s = WDGedit(prompt, bufn, sizeof(bufn))) == ABORT) {
    return s;
  }

  Buffer* bp;
  if (s == NIL) {
    bp = bp1;
  } else if ((bp = Buffer::find(bufn, false)) == nullptr) {
    return NIL;
  }

  if (bp->count() > 0) {
    /*
     * buffer is already on screen
     */
    if (bp->usewindow()) {
      return T;
    }
  }

  return curwp->connect(bp) ? T : NIL;
}

/*
 * Process buffer commands 'd', 'f', 'k, 's', 'u', 'x' or TAB.
 */

static Buffer*
getbpcmd(EMCHAR* buf) {
  /*
   * get buffer pointer, internally used by buffercmd
   */

  int     j = BufferPOS;
  int     i;
  EMCHAR  c;

  if (curwp->line()->length() < BufferPOS) {
    return nullptr;
  }

  for (i = 0; (c = curwp->line()->get(j++)) != ' '; ++i) {
    buf[i] = c;
  }

  buf[i] = '\000';

  return Buffer::find(buf, false);
}

CMD
Buffer::buffercmd(int cmd) {
  EMCHAR  buf[NPAT];
  Buffer  *bp;
  int     asked = 0;

  if (cmd != 'x' && cmd != 'n' && cmd != 'p' && cmd != 0x08) {
    auto lp1 = curwp->line();
    if (lp1 == curbp->lastline()  ||        /* At the end       */
        lp1 == curbp->firstline() ||        /* At the beginning */
        lp1 == curbp->firstline()->forw()     /* At 2nd line      */
      ) {
      return Editor::ctrlg();
    }
  }

  switch (cmd) {
  case 'f' :
    if ((bp = getbpcmd(buf)) == nullptr) {
      return NIL;
    }

    if (bp->count() > 0) {
      /*
       * buffer is already on screen
       */
      if (bp->usewindow()) {
        return T;
      }
    }

    return curwp->connect(bp) ? T : NIL;
  case 'd':
  case 'k':
    if (curwp->line()->length() == 0) {
      return Editor::ctrlg();
    }

    curbp->setReadonly(false);
    if (curwp->line()->get(0) == 'D') {
      curwp->line()->put(0, ' ');
    } else {
      curwp->line()->put(0, 'D');
    }

    Buffer::change(EditWindow::WFEDIT);
    (void)Editor::forwline();

    curbp->setReadonly(true);
    curbp->setChanged(false);
    return T;

  case 'n':
    return Editor::forwline();

  case 'p':
    return Editor::backline();

  case 's':
    if (curwp->line()->length() == 0) {
      return Editor::ctrlg();
    }

    curbp->setReadonly(false);
    if (curwp->line()->get(1) == 'S') {
      curwp->line()->put(1, ' ');
    } else {
      curwp->line()->put(1, 'S');
    }

    Buffer::change(EditWindow::WFEDIT);
    (void)Editor::forwline();

    curbp->setReadonly(true);
    curbp->setChanged(false);
    return T;

  case 'u':
    if ((bp = getbpcmd(buf)) == nullptr) {
      return NIL;
    }

    curbp->setReadonly(false);
    if (curwp->line()->get(0) == 'D') {
      curwp->line()->put(0, ' ');
    }

    if (curwp->line()->get(1) == 'S') {
      curwp->line()->put(1, ' ');
    }

    if (bp->readonly()) {
      curwp->line()->put(2, '%');
    } else {
      curwp->line()->put(2, ' ');
    }

    Buffer::change(EditWindow::WFEDIT);

    curbp->setReadonly(true);
    curbp->setChanged(false);
    return T;

  case '%':
    if ((bp = getbpcmd(buf)) == nullptr) {
      return NIL;
    }

    curbp->setReadonly(false);

    if (curwp->line()->get(2) == '%') {
      curwp->line()->put(2, ' ');
    } else {
      curwp->line()->put(2, '%');
    }

    Buffer::change(EditWindow::WFEDIT);
    (void)Editor::forwline();
    curbp->setReadonly(true);
    curbp->setChanged(false);

    return T;

  case 'x':
    curbp->setReadonly(false);
    (void)Editor::gotobob();
    (void)Editor::forwline();
    (void)Editor::forwline();

    do {
      if ((bp = getbpcmd(buf)) == nullptr) {
        continue;
      }

      if (curwp->line()->get(1) == 'S') {
        Buffer* oldbp = curbp;
        curbp = bp;
        if (Editor::fileSave() == T) {
          curwp->line()->put(1, ' ');
          Buffer::change(EditWindow::WFEDIT);
        }
        curbp = oldbp;
      }

      if (curwp->line()->get(2) == '%') {
        bp->setReadonly(true);
      } else {
        bp->setReadonly(false);
      }

      /*
       * case delete should be the last one of the do loop since it
       * calls discard that makes bp an invalid pointer.
       */

      if (curwp->line()->get(0) == 'D') {
        asked++;
        if (bp->discard()) {
          (void)Editor::gotobol();
          (void)Line::remove(curwp->line()->length() + 1);
          Buffer::change(EditWindow::WFEDIT);
          (void)Editor::backline();
          (void)Editor::gotobol();
        }
      }
    } while (curwp->line() != curbp->lastline() && Editor::forwline() == T);

    curbp->setReadonly(true);
    curbp->setChanged(false);

    if (asked == 0) {
      WDGwrite(ECSTR("(No deletions requested)"));
    }

    return T;

  case 0x08 : /* ^H */
    if (Editor::backline() == T && Editor::gotobol() == T) {
      if ((bp = getbpcmd(buf)) == nullptr) {
        return NIL;
      }

      curbp->setReadonly(false);
      if (curwp->line()->get(0) == 'D') {
        if (bp == curbp) {
          curwp->line()->put(0, '.');
        } else {
          curwp->line()->put(0, ' ');
        }
        Buffer::change(EditWindow::WFEDIT);
      }
      if (curwp->line()->get(1) == 'S') {
        if (bp->isChanged()) {
          curwp->line()->put(1, '*');
        } else {
          curwp->line()->put(1, ' ');
        }
        Buffer::change(EditWindow::WFEDIT);
      }
      if (curwp->line()->get(2) == '%') {
        if (bp->readonly()) {
          curwp->line()->put(2, '%');
        } else {
          curwp->line()->put(2, ' ');
        }
        Buffer::change(EditWindow::WFEDIT);
      }
      curbp->setReadonly(true);
      curbp->setChanged(false);
    }
    return T;

  default :
    return Editor::ctrlg();
  }
}

/*
 * Dispose of a buffer,  by name.   Ask for the name. Look it up
 * (don't  get too upset if it isn't there at all!).  Get  quite
 * upset if the buffer is being displayed. Clear the buffer (ask
 * if  the buffer has been changed).  Then free the header  line
 * and the buffer header. Bound to "C-X K".
 */

CMD
Buffer::kill() {
  CMD     s;
  EMCHAR  bufn[Buffer::NBUFN];
  EMCHAR  prompt[NLINE];

  complete = bufmatch;

  (void)emstrcpy(prompt, ECSTR("Kill buffer: (default "));
  (void)emstrcat(prompt, curbp->bufname());
  (void)emstrcat(prompt, ECSTR(") "));

  bufn[0] = '\000';

  if ((s = WDGedit(prompt, bufn, sizeof(bufn))) == ABORT) {
    return s;
  }

  Buffer* bp;

  if (s == NIL) {
    bp = curbp;
  } else if ((bp = Buffer::find(bufn, false)) == nullptr) {
    return NIL;
  }

  return bp->discard() ? T : NIL;
}

/*
 * List  all  of  the  active buffers.  First update the special
 * buffer that holds the list.  Next make sure at least 1 window
 * is  displaying the buffer list,  splitting the screen if this
 * is  what  it takes.  Lastly,  repaint all of the windows that
 * are displaying the list. Bound to "C-XC-B".
 */

CMD
Buffer::listbuffers() {
  auto bp(find(BUF_LIST, true, EDITMODE::BufferMODE));

  if (bp == nullptr) {
    return NIL;
  }

  if (!makelist(bp)) {
    return NIL;
  }

  return (bp->show() == nullptr) ? T : NIL;
}
