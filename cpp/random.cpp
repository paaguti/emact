#if     !defined(lint)
static  char rcsid[] = "$Id: random.cpp,v 1.26 2018/09/08 14:12:50 jullien Exp $";
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
 * This  file  contains  the  command  processing functions for a
 * number  of  random  commands.  There is no functional grouping
 * here, for sure.
 */

#include "emacs.h"

static int  getccol();
static CMD  addprefix();
static bool prefixlinep(const EDLINE* line, int len);

static constexpr int JUSTLEFT{0x00};    /* left justification (default) */
static constexpr int JUSTFULL{0x10};    /* full justification           */

static int justmode{JUSTFULL};          /* Justification mode           */

/*
 * Display  the current position of the cursor,  in origin 1 X-Y
 * coordinates,  the  character  that  is  under  the cursor (in
 * octal),  and  the  fraction  of  the  text that is before the
 * cursor.  The displayed column is not the current column,  but
 * the  column  that would be used on an infinite width display.
 * Normally this is bound to "C-X =".
 */

CMD
showcpos() {
  long nch = 0;
  int  cbo = 0;
  int  cac = MAX_EMCHAR;
  int  nbl = 1;
  int  col = getccol();
  long nbc = 0L;

  for (auto clp = curbp->firstline();;) {
    const auto& dot(curwp->getDot());
    const auto dotp(dot.line());
    const auto doto(dot.pos());
    if (clp == dotp && cbo == doto) {
      nbc = nch;
      if (cbo == clp->length()) {
        cac = (int)'\n';
      } else {
        cac = (int)((unsigned int)clp->get(cbo));
      }
    }
    if (cbo == clp->length()) {
      if (cac == MAX_EMCHAR) {
        ++nbl;
      }
      if (clp == curbp->lastline()) {
        break;
      }
      clp = clp->forw();
      cbo = 0;
    } else {
      ++cbo;
    }
    ++nch;
  }

  cac &= MAX_EMCHAR;

  if (cac < 32) {
    WDGwrite(ECSTR("at line %d: col=%d row=%d code=(%d, %x) (%d%%)"),
             nbl, col, DISPLAY::_currow, cac, cac,
             (nch==0) ? 0 : (int)((100*nbc)/nch));
  } else {
    EMCHAR buf[2];
    buf[0] = (EMCHAR)cac;
    buf[1] = '\000';
    WDGwrite(ECSTR("at line %d: col=%d row=%d char='%s' code=(%d, 0x%x) (%d%%)"),
             nbl, col, DISPLAY::_currow, buf, cac, cac,
             (nch==0)?0:(int)((100*nbc)/nch));
  }

  return T;
}

/*
 * Return current column.
 */

static int
getccol() {
  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto doto(dot.pos());
  auto col(0);

  for (auto i(0); i < doto; ++i) {
    auto c(dotp->get(i));
    if (c == '\t') {
      do {
        ++col;
      } while (col % opt::tab_display);
    } else {
      if (!self_insert(c)) {
        ++col;
      }
      ++col;
    }
  }

  return col;
}

/*
 * Twiddle the two characters on either side of dot.  If dot is at the
 * end of the line twiddle the two characters before it.  Return with
 * an error if dot is at the beginning of line; it seems to be a bit
 * pointless to make this work.  This fixes up a very common typo with
 * a single stroke.  Normally bound to "C-T".  This always works
 * within a line, so "WINSCR::WFEDIT" is good enough.
 */

CMD
twiddle() {
  if (freadonly()) {
    return NIL;
  }

  /*
   * if dot is at the end, backchar
   */

  const auto& dot(curwp->getDot());
  auto dotp(dot.line());
  auto doto(dot.pos());

  if (doto == dotp->length()) {
    --doto;
  }

  /*
   * if dot is at the beginning, nothing to do.
   */

  if (doto <= 0) {
    return NIL;
  }

  auto cd = dotp->get(doto);
  auto cl = dotp->get(doto - 1);

  dotp->put(doto - 1, cd);
  dotp->put(doto, cl);

  if (curwp->pos() < dotp->length()) {
    (void)forwchar();
  }

  BUFFER::change(WINSCR::WFEDIT);

  return T;
}

/*
 * Quote the next character,  and insert it into the buffer. All
 * the  characters  are  taken literally,  with the exception of
 * the  newline,  which  always  has its line splitting meaning.
 * The  character  is  always  read,  even  if  it is inserted 0
 * times,  for regularity. Bound to "C-Q" (for Rich, and only on
 * systems that don't need XON-XOFF).
 */

CMD
quotechar() {
  auto n = Editor::_repeat;
  int  c;

  if ((c = TTYgetc()) == '\n') {
    while (EDLINE::newline() && --n) {
      continue;
    }
    return T;
  }
  return EDLINE::linsert(c, n) ? T : NIL;
}

/*
 * Set  tab  size  if  given  non-default  argument  (n  <>  1).
 * Otherwise,  insert a tab into file.  If given argument, n, of
 * zero,  change to true tabs. If n > 1, simulate tab stop every
 * n-characters  using  spaces.  This  has  to  be  done in this
 * slightly  funny  way  because  the  tab  (in  ASCII) has been
 * turned into "C-I" (in 10 bit code) already. Bound to "C-I".
 */

CMD
tab() {
  if (Editor::_repeat == 0) {
    opt::tab_size = 8;   /* restore to default */
    return T;
  }

  if (Editor::_repeat != 1) {
    opt::tab_size = Editor::_repeat;
    return T;
  }

  if (Editor::_lastflag & CFTAB) {
    /*
     * Last indent fails, force a tab.
     */
    Editor::_thisflag &= ~CFTAB;
    return tabexpand();
  }

  if ((curbp->editMode() != EDITMODE::FUNDAMENTAL)
      && (curwp->pos() == 0)) {
    auto s = tabindent();
    if (curwp->pos() == 0) {
      /*
       * Intdentation is still at 0 after trying to indent
       */
      Editor::_thisflag |= CFTAB;
    }
    return s;
  }

  return tabexpand();
}

CMD
tabexpand() {
  bool res;
  if (opt::tab_size == 8 &&
      curbp->editMode() != EDITMODE::JAVAMODE &&
      curbp->editMode() != EDITMODE::CSHARPMODE) {
    res = EDLINE::linsert('\t');
  } else {
    res = EDLINE::linsert(' ', opt::tab_size - (getccol() % opt::tab_size));
  }

  return res ? T : NIL;
}

/*
 * Open  up  some blank space.  The basic plan is to insert a bunch of
 * newlines  and  then  back  up over them.  Everything is done by the
 * subcommand  processors.  They  even  handle the looping.  When this
 * command  is  executed at the beginning of the a line it also insert
 * the fill-prefix when it this one is defined. Normally this is bound
 * to "C-O".
 */

CMD
openline() {
  for (auto i = 0; i < Editor::_repeat; i++) {
    if (curwp->pos() == 0 && opt::fill_prefix[0]) {
      for (auto j = 0; opt::fill_prefix[j]; j++) {
        if (!EDLINE::linsert(opt::fill_prefix[j])) {
          return NIL;
        }
      }
    }

    if (!EDLINE::newline()) {
      return NIL;
    }
  }

  if (backline() != T) {
    return NIL;
  }

  return gotoeol();
}

/*
 * Insert a newline.  Bound to "C-M".  Simply insert new line or
 * indent  depending  of  selected  mode (Fundamental,  C/C++ or
 * Lisp).
 */

CMD
endline() {
  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
  case EDITMODE::CSHARPMODE:
  case EDITMODE::SGMLMODE:
  case EDITMODE::JAVAMODE:
  case EDITMODE::FORTRANMODE:
  case EDITMODE::PASCALMODE:
  case EDITMODE::PROLOGMODE:
  case EDITMODE::PERLMODE:
  case EDITMODE::SHELLMODE:
  case EDITMODE::LISPMODE:
    return newlineindent();
  case EDITMODE::DIRED: {
    const auto dotp(curwp->line());

    if (dotp == curbp->lastline() || dotp->length() == 0) {
      return ctrlg();
    }

    EMCHAR buf[NFILEN];
    (void)emstrncpy(buf, dotp->text(), dotp->length());
    buf[dotp->length()] = '\000';
    return newfile(buf + DIREDMARK) ? T : NIL;
  }
  default:
    break;
  }
  return newline();
}

/*
 * Insert  a newline.  Bound to "C-M".  If you are at the end of
 * the  line  and the next line is a blank line,  just move into
 * the  blank line.  This makes "C-O" and "C-X C-O" work nicely,
 * and  reduces  the  ammount  of  screen  update that has to be
 * done.  This  would not be as critical if screen update were a
 * lot more efficient.
 */

CMD
newline() {
  auto n = Editor::_repeat;

  while (n--) {
    const auto& dot(curwp->getDot());
    const auto dotp(dot.line());
    const auto doto(dot.pos());
    auto s(T);
    if ((dotp->length() == doto)
        && (dotp != curbp->lastline())
        && (dotp->forw()->length() > 0)) {
      if ((s = forwchar()) != T) {
        return s;
      }
    } else if (!EDLINE::newline()) {
      return NIL;
    }
  }
  return T;
}

/*
 * Delete  blank  lines  around  dot.  What  this  command  does
 * depends if dot is sitting on a blank line.  If dot is sitting
 * on  a  blank  line,  this command deletes all the blank lines
 * above  and below the current line.  If it is sitting on a non
 * blank  line  then it deletes all of the blank lines after the
 * line.  Normally  this  command  is  bound  to "C-X C-O".  Any
 * argument is ignored.
 */

CMD
deblank() {
  auto lp1 = curwp->line();

  EDLINE* lp2;
  while (lp1->length() == 0 && (lp2 = lp1->back()) != curbp->lastline()) {
    lp1 = lp2;
  }

  int nld = 0;
  for (lp2 = lp1; (lp2=lp2->forw())!=curbp->lastline()&& lp2->length()==0;) {
    ++nld;
  }

  if (nld == 0) {
    return T;
  }

  curwp->setDot(lp1->forw(), 0);

  return EDLINE::ldelete(nld) ? T : NIL;
}

/*
 * Delete forward.  This is real easy,  because the basic delete
 * routine does all of the work.  If any argument is present, it
 * kills  rather than deletes,  to prevent loss of text if typed
 * with a big argument. Normally bound to "C-D".
 */

CMD
forwdel() {
  return EDLINE::ldelete(Editor::_repeat) ? T : NIL;
}

/*
 * Delete  backwards.  This is quite easy too,  because it's all
 * done  with  other functions.  Just move the cursor back,  and
 * delete  forwards.  Like delete forward,  this actually does a
 * kill  if  presented with an argument.  Bound to both "RUBOUT"
 * and "C-H".
 */

CMD
backdel() {
  if (curbp->readonly()) {
    if (curbp->editMode() == EDITMODE::BUFFERMODE) {
      return buffercmd(0x08);
    }

    if (curbp->editMode() == EDITMODE::DIRED) {
      return diredcmd(0x08);
    }
  }

  const auto& dot(curwp->getDot());
  auto dotp(dot.line());
  auto doto(dot.pos());

  if ((curbp->editMode() != EDITMODE::FUNDAMENTAL)
      && (Editor::_repeat == 1)
      && (doto > 0)) {
    /*
     * Try to delete past a tab, expand tab before deleting.
     */
    if (dotp->get(doto - 1) == '\t') {
      int pos;

      /*
       * Delete 'tab'
       */

      if (backchar() == T) {
        (void)EDLINE::ldelete(1);
      }

      pos = getccol();

      /*
       *      Replace by blanks
       */

      do {
        (void)EDLINE::linsert(' ');
      } while (++pos % opt::tab_display);
    }
  }

  if (backchar() == T) {
    return EDLINE::ldelete(Editor::_repeat) ? T : NIL;
  } else {
    return NIL;
  }
}

/*
 * Kill text.  If called without an argument,  it kills from dot
 * to the end of the line,  unless it is at the end of the line,
 * when  it kills the newline.  If called with an argument of 0,
 * it kills from the start of the line to dot.  If called with a
 * positive  argument,  it  kills  from  dot  forward  over that
 * number of newlines.
 */

CMD
killtext() {
  if ((Editor::_lastflag & CFKILL) == 0) {
    /* Clear kill buffer if */
    kdelete(); /* last wasn't a kill.  */
  }

  Editor::_thisflag |= CFKILL;

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto doto(dot.pos());
  auto chunk = dotp->length() - doto;

  if (chunk == 0) {
    chunk = 1;
  }

  return EDLINE::ldelete(chunk, true) ? T : NIL;
}

/*
 * Yank text back from the kill buffer.  This is really easy.  All of
 * the work is done by the standard insert routines.  All you do is
 * run the loop, and check for errors.  Bound to "C-Y".  The blank
 * lines are inserted with a call to "newline" instead of a call to
 * "EDLINE::newline" so that the magic stuff that happens when you
 * type a carriage return also happens when a carriage return is
 * yanked back from the kill buffer.  As a special case we check if
 * yank is done at the end of buffer.  If so, insert push a new line
 * at the end of buffer that must be removed.
 */

CMD
yank() {
  auto n          = Editor::_repeat;
  auto save       = Editor::_repeat;
  auto lastlflag  = (curwp->line() == curbp->lastline());
  auto firstlflag = (curwp->line() == curbp->firstline());

  Editor::_repeat = 1;

  WDGclippaste();

  while (n--) {
    int c;

    for (auto i = 0; (c = kremove(i)) >= 0; ++i) {
      if (c == '\n') {
        if (!EDLINE::newline()) {
          return NIL;
        }
      } else if (!EDLINE::linsert(c)) {
        return NIL;
      }
    }
  }

  Editor::_repeat = save;

  if (lastlflag) {
    /*
     * yank  was done at the end of the buffer,  kill the
     * last char
     */

    (void)forwdel();

    if (firstlflag) {
      /*
       * yank was done on an empty buffer, force redisplay
       * for all active buffer.
       */

      for (auto wp : WINSCR::list()) {
        if (wp->buffer() == curbp) {
          wp->setFlags(WINSCR::WFFORCE);
        }
      }
    }
  }

  return T;
}

/*
 * Position  the CFKILL flag for the variable "Editor::_thisflag" so that
 * the  next  command can append text to the kill buffer.  Bound
 * to M-C-W.
 */

CMD
appendnextkill() {
  WDGmessage(ECSTR("If the next command is a kill, it will append"));
  Editor::_thisflag |= CFKILL;
  return T;
}

/*
 * Internal  function that returns T if the line passed as first
 * argument  match  the  prefix  string  of length 'len' and NIL
 * otherwise.
 */

static bool
prefixlinep(const EDLINE *line, int len) {
  auto l = line->length();

  if (l == 0 || l < len) {
    return false;
  } else {
    return emstrncmp(line->text(), opt::fill_prefix, (size_t)len) == 0;
  }
}

/*
 * Set  the  fill-column at the value of cursor current location
 * or to the value set by the Editor::_repeat command. Bound to C-X-F.
 */

CMD
setfillcolumn() {
  auto newfill = ((Editor::_repeat == 1) ? curwp->pos() : Editor::_repeat);

  if (newfill < 3) {
    WDGwrite(ECSTR("fill-column can't be less than 3"));
    return NIL;
  }

  opt::fill_column = newfill;

  if (!kbdm.isPlaying()) {
    WDGwrite(ECSTR("fill-column set to %d"), opt::fill_column);
  }

  return T;
}

/*
 * Set  the  fill-prefix from the text beginning the line and up
 * to the value of cursor current location. Bound to C-X-.
 */

CMD
setfillprefix() {
  if (curwp->pos() == 0) {
    if (!kbdm.isPlaying()) {
      WDGwrite(ECSTR("fill-prefix cancelled."));
    }

    opt::fill_prefix[0] = '\0';
    return T;
  }

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto doto(dot.pos());

  int i;
  for (i = 0; i < doto; i++) {
    opt::fill_prefix[i] = dotp->get(i);
  }

  opt::fill_prefix[i] = '\0';

  if (!kbdm.isPlaying()) {
    WDGwrite(ECSTR("fill-prefix: \"%s\""), opt::fill_prefix);
  }

  return T;
}

/*
 * Add  the  fill-prefix  string  to the current line and remove
 * spaces at the beginning of the line.
 */

static CMD
addprefix() {
  const auto dotp(curwp->line());
  const auto len(emstrlen(opt::fill_prefix));

  curwp->setDotPos(0);

  if (!prefixlinep(dotp, len)) {
    for (auto i = 0; opt::fill_prefix[i]; i++) {
      if (!EDLINE::linsert(opt::fill_prefix[i])) {
        return NIL;
      }
    }
  }

  curwp->setDotPos(len);

  while ((curwp->pos() < curwp->line()->length())
         && curwp->getChar() == ' ') {
    (void)EDLINE::ldelete(1);
  }

  return T;
}

/*
 * Fill  paragraph.  Insert  the fill-prefix string on each line
 * of  the  current paragraph that does not currently start with
 * this  prefix  and  limit  the  line  length  to  the value of
 * fill-column variable. Bound to M-Q.
 */

CMD
fillparagraph() {
  if (freadonly()) {
    return NIL;
  }

  auto len     = emstrlen(opt::fill_prefix);
  auto oldmode = curbp->editMode();

  curbp->setEditMode(EDITMODE::FUNDAMENTAL);

  (void)backparagraph();

  curwp->setDotPos(0);
  if (prefixlinep(curwp->line(), len)) {
    (void)EDLINE::ldelete(len);
  }

  /*
   * Convert paragraph in only one line removing previous prefix.
   */

  while (curwp->line()->forw()->length() > len
         && prefixlinep(curwp->line()->forw(), len)) {
    (void)gotoeol();
    (void)EDLINE::ldelete(1);
    (void)EDLINE::linsert(' ');
    (void)EDLINE::ldelete(len);
  }

  /*
   * Remove duplicated spaces
   */

  curwp->setDotPos(0);
  while (curwp->pos() < curwp->line()->length()) {
    bool nbspace;
    if (curwp->getChar() == ' ') {
      nbspace = true;
    } else {
      nbspace = false;
    }

    (void)forwchar();

    if (nbspace) {
      while (curwp->pos() < curwp->line()->length()) {
        if (curwp->getChar() != ' ') {
          break;
        }
        (void)EDLINE::ldelete(1);
      }
    }
  }

  /*
   *  Split this line within the fill_column and fill_prefix.
   */

  (void)addprefix();

  curwp->setDotPos(0);
  while (curwp->line()->position() >= opt::fill_column) {
    if (splitlinetofill() != T) {
      break;
    }
  }

  (void)gotoeol();

  /*
   * Reset the _curcol to the current position (any better solution ?)
   */

  DISPLAY::_curcol = Editor::_curgoal = getccol();

  curbp->setEditMode(oldmode);

  return T;
}

/*
 * Internal   function   used  to  split  the  current  line  to
 * fill-column.  Go  back  to the first non-space character less
 * than  the  value  of  fill-column and break the current line.
 * Then the fill-prefix is added on the new line.
 */

CMD
splitlinetofill() {
  int  bpos = 0;                        /* break position      */
  int  dpos = 0;                        /* display position    */
  int  fpos = 0;                        /* forced break        */
  int  fflg = 1;                        /* fill prefix flag    */

  /*
   * Compute the break position in 'bpos'.
   */

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto lmax(dotp->length()); // max position
  for (int i = 0; i < lmax; i++) {
    auto c(dotp->get(i));

    if (c == '\t') {
      do {
        ++dpos;
      } while (dpos % opt::tab_display);
    } else {
      ++dpos;
    }

    /*
     * Break pos should be after the fill prefix
     */

    if (fflg) {
      fflg = (c == opt::fill_prefix[i]);
    }

    if (c == ' ' && fflg == 0) {
      if (dpos < opt::fill_column) {
        bpos = i;
      } else if (fpos == 0) {
        fpos = i;
      }
    }
  }

  if (bpos == 0 && fpos == 0) {
    fpos = lmax - 1;
  }

  if (dpos >= opt::fill_column) {
    if ((fpos - bpos) > opt::fill_column) {
      /*
       * Event  a break at this point will not make
       * the next line fit in fill_column.  Force a
       * break at fpos.
       */
      curwp->setDotPos(fpos);
    } else {
      curwp->setDotPos(bpos);
    }

    (void)endline();
    if (justmode != JUSTLEFT) {
      /*
       * assumes full-justify
       */
      (void)backline();
      if (curbp->editMode() == EDITMODE::SGMLMODE) {
        (void)justifycomment();
      } else {
        (void)justifycurline();
      }
      (void)forwline();
    }
    (void)addprefix();
  }

  return T;
}

/*
 * Justify   current   line  by  adding  space  to  fill  up  to
 * fill-column.  It  assumes  that  the  current  line length is
 * already less than the value of fill-column.
 */

CMD
justifycurline() {
  bool justifyed;
  int maxspace;

  if (curwp->line()->position() == opt::fill_column) {
    return T;
  }

  auto len     = emstrlen(opt::fill_prefix);
  auto fillmax = opt::fill_column - 1;

  curwp->setDotPos(0);

  if (prefixlinep(curwp->line(), len)) {
    /*
     * Current  line  have  prefix,  move  to  the first character
     * after the prefix string.
     */
    curwp->setDotPos(len);
  }

  /*
   * Try to add spaces after '.' and ',' first.
   */

  while (curwp->pos() < curwp->line()->length()) {
    EMCHAR c = curwp->getChar();

    if (c == '.' || c == ',') {
      (void)forwchar();
      if ((curwp->pos() < (curwp->line()->length() - 1))
          && curwp->getChar() == ' ') {
        (void)EDLINE::linsert(' ');
      }
    } else {
      (void)forwchar();
    }

    if (curwp->line()->position() >= fillmax) {
      return T;
    }
  }

  /*
   * Add spaces up to fill_column.
   */

  justifyed = true;
  maxspace = 1;

  while (curwp->line()->position() < fillmax && justifyed) {
    justifyed = false;

    maxspace++;

    curwp->setDotPos(0);

    if (prefixlinep(curwp->line(), len)) {
      /*
       * Current line have prefix, move to the first character after
       * the prefix string.
       */
      curwp->setDotPos(len);
    }

    while (curwp->pos() < curwp->line()->length()) {
      if (curwp->getChar() == ' ') {
        int nbspace = 0;

        do {
          if (curwp->getChar() != ' ') {
            break;
          }
          (void)forwchar();
          nbspace++;
        } while (curwp->pos() < curwp->line()->length());

        if (nbspace < maxspace) {
          (void)EDLINE::linsert(' ');
          justifyed = true;
        }
      } else {
        (void)forwchar();
      }

      if (curwp->line()->position() >= fillmax) {
        return T;
      }
    }
  }

  return T;
}

/*
 * Move to the beginning of paragraph. Bound to M-{
 */

CMD
backparagraph() {
  auto len = emstrlen(opt::fill_prefix);

  while (curwp->line()->back() != curbp->lastline() &&
         backline() == T &&
         curwp->line()->length() > len &&
         prefixlinep(curwp->line(), len)) {
    /* empty loop*/
    continue;
  }

  if (curwp->line() != curbp->firstline()) {
    (void)forwline();
  }

  if (curwp->line()->length() > len) {
    curwp->setDotPos(len);
  } else {
    curwp->setDotPos(0);
  }

  return T;
}

/*
 * Move to the end of paragraph. Bound to M-}
 */

CMD
forwparagraph() {
  auto len = emstrlen(opt::fill_prefix);

  while (curwp->line() != curbp->firstline() &&
         forwline() == T &&
         curwp->line()->length() > len &&
         prefixlinep(curwp->line(), len)) {
    /* empty loop*/
    continue;
  }

  if (curwp->line() != curbp->lastline()) {
    (void)backline();
  }

  (void)gotoeol();

  return T;
}

/*
 * Mark the paragraph. Bound to M-h
 */

CMD
markparagraph() {
  (void)backparagraph();
  (void)setmark();
  (void)forwparagraph();

  return T;
}

/*
 * Set  the  justification mode to 'left' and adjust the current
 * paragraph using this mode. Unbound.
 */

CMD
setjustifyleft() {
  justmode = JUSTLEFT;

  return fillparagraph();
}

/*
 * Set the justification mode to 'full' and adjust the current
 * paragraph using this mode. Unbound.
 */

CMD
setjustifyfull() {
  justmode = JUSTFULL;

  return fillparagraph();
}

/*
 * Justify a comment (for programs like C/C++ ..).  Start at the
 * beginning  of  the  line  and find the first character of the
 * comment.  At this point,  set the fill prefix and justify the
 * current paragraph. Bound to to M-;
 */

CMD
justifycomment() {
  static constexpr EMCHAR skip[] = { ' ', '\t', '#', '*', '/', '-', ';', 0 };

  curwp->setDotPos(0);

  auto len(curwp->line()->length());
  while (curwp->pos() < len) {
    auto c = curwp->getChar();

    int i;
    for (i = 0; skip[i]; i++) {
      if (c == skip[i]) {
        break;
      }
    }

    if (skip[i] == 0) {
      break;
    }

    (void)forwchar();
  }

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto doto(dot.pos());

  if (doto >= opt::fill_column) {
    /*
     *  Too hard, fill-prefix > fill-column
     */
    return NIL;
  }

  for (int i = 0; i < doto; ++i) {
    opt::fill_prefix[i] = dotp->get(i);
  }

  opt::fill_prefix[doto] = '\0';
  (void)fillparagraph();
  opt::fill_prefix[0] = '\0';

  return T;
}

/*
 * Insert  the  current  value  of counter at dot before incrent
 * it. Bound to C-X-$-$
 */

static int     cntval        = 0;
static EMCHAR  cntfmt[NPAT]  = { '%', 'd', 0 };

CMD
counterinsert() {
  EMCHAR buf[NPAT];

  (void)emsprintf1(buf, &cntfmt[0], cntval);

  for (auto s = &buf[0]; *s; s++) {
    (void)EDLINE::linsert(*s);
  }

  return T;
}

/*
 * Increment counter. Bound to C-X-$-'+'
 */

CMD
counterincr() {
  cntval += Editor::_repeat;

  return T;
}

/*
 * Decrement counter. Bound to C-X-$-'-'
 */

CMD
counterdecr() {
  cntval -= Editor::_repeat;

  return T;
}

/*
 * Set the value of counter. Bound to C-X-$-S
 */

CMD
counterset() {
  cntval = Editor::_repeat;

  return T;
}

/*
 * Change the format of counter from default (%d). Bound to C-X-$-F
 */

CMD
counterformat() {
  CMD s;

  if ((s = WDGedit(ECSTR("Counter format: "), cntfmt, NPAT)) != T) {
    return s;
  } else {
    return T;
  }
}

/*
 * No undo yet !
 */

CMD
undo() {
  TTYbeep();
  WDGwrite(ECSTR("'undo' not yet implemented ! Sorry."));
  return NIL;
}

/*
 * Force an error to go to debugger.
 */

CMD
enterdebug() {
  int* p;
  int* bug = nullptr;

  if (Editor::_repeat == 666) {
    p  = bug;
    *p = 1;
  }

  WDGwrite(ECSTR("enter-debug needs to be called with 666."));
  return NIL;
}
