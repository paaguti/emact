#if !defined(lint)
static auto rcsid("$Id: basic.cpp,v 1.14 2018/09/08 14:12:50 jullien Exp $");
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
 * The  routines  in  this file move the cursor  around  on  the
 * screen.  They compute a new value for the cursor, then adjust
 * ".".  The display code always updates the cursor location, so
 * only  moves between lines,  or functions that adjust the  top
 * line in the window and invalidate the framing, are hard.
 */

#include "./emacs.h"

#include "./Buffer.h"
#include "./Completion.h"
#include "./Counter.h"
#include "./EditWindow.h"
#include "./Editor.h"
#include "./Kbdm.h"
#include "./Indent.h"
#include "./KillBuf.h"
#include "./Line.h"
#include "./MiniBuf.h"
#include "./Point.h"
#include "./Redisplay.h"
#include "./Terminal.h"
#include "./Widget.h"

extern Widget* widget;       // Widgets tools

/*
 * Move the cursor to the beginning of the current line.
 */

CMD
Editor::gotobol() {
  curwp->setDotPos(0);
  return T;
}

/*
 * Move the cursor backwards by "n" characters.  Compute the new
 * cursor location.  Error if you try and move out of the buffer. Set
 * the flag if the line pointer for dot changes.
 */

CMD
Editor::backchar() {
  auto n = Editor::_repeat;

  while (n--) {
    if (curwp->pos() == 0) {
      Line* lp;
      if ((lp = curwp->line()->back()) == curbp->lastline()) {
        return NIL;
      }
      curwp->setDot(lp, lp->length());
      curwp->setFlags(EditWindow::WFMOVE);
    } else {
      curwp->setDotPos(curwp->pos() - 1);
    }
  }

  return T;
}

/*
 * Move the cursor to the end of the current line.
 */

CMD
Editor::gotoeol() {
  curwp->setDotPos(curwp->line()->length());
  return T;
}

/*
 * Move  the cursor forwards by "n" characters.  Compute the new
 * cursor location,  and move ".". Error if you try and move off
 * the  end of the buffer.  Set the flag if the line pointer for
 * dot changes.
 */

CMD
Editor::forwchar() {
  int n = Editor::_repeat;

  while (n--) {
    if (curwp->pos() == curwp->line()->length()) {
      if (curwp->line() == curbp->lastline()) {
        return NIL;
      }
      curwp->setDot(curwp->line()->forw(), 0);
      curwp->setFlags(EditWindow::WFMOVE);
    } else {
      curwp->setDotPos(curwp->pos() + 1);
    }
  }

  return T;
}

/*
 * Goto the beginning of the buffer.  Massive adjustment of dot.
 * This is considered to be hard motion;  it really isn't if the
 * original  value  of dot is the same as the new value of  dot.
 * Normally bound to "M-<".
 */

CMD
Editor::gotobob() {
  curwp->setDot(curbp->firstline(), 0);
  curwp->setFlags(EditWindow::WFHARD);
  return T;
}

/*
 * Move to the end of the buffer.   Dot is always put at the end
 * of the file.  The standard screen code does most of  the hard
 * parts of update. Bound to "M->".
 */

CMD
Editor::gotoeob() {
  curwp->setDot(curbp->lastline(), 0);
  curwp->setFlags(EditWindow::WFHARD);
  return T;
}

/*
 * Prompt for a line number and jump to  that position.  If line
 * is out buffer display error message and exit.
 * Bound to "M-G" and "M-N".
 */

CMD
Editor::gotoline() {
  int n = Editor::_repeat;

  if (n <= 1) {
    EMCHAR buf[20];
    if (MiniBuf::reply(ECSTR("Goto line: "), buf, 20) == ABORT) {
      return ABORT;
    }
    n = emstrtoi(buf);
  }

  (void)gotobob();

  auto clp = curwp->line();
  while (--n && clp != curbp->lastline()) {
    clp = clp->forw();
  }

  curwp->setDot(clp, 0);
  curwp->setFlags(EditWindow::WFMOVE);

  if (n <= 0) {
    return T;
  }

  WDGmessage(ECSTR("No such line."));
  return NIL;
}

/*
 * Move forward by full lines. The last command controls how the
 * goal column is set. Bound to "C-N". No errors are possible.
 */

CMD
Editor::forwline() {
  Line* dlp;
  int     n = Editor::_repeat;

  if ((Editor::_lastflag & CFCPCN) == 0) {
    /* Reset goal if the last isn't C-P or C-N */
    Editor::_curgoal = Redisplay::_curcol;
  }
  Editor::_thisflag |= CFCPCN;
  if ((dlp = curwp->line()) == curbp->lastline()) {
    return NIL;
  }

  while (n-- && dlp != curbp->lastline()) {
    dlp = dlp->forw();
  }

  curwp->setDot(dlp, dlp->getgoal(Editor::_curgoal));
  curwp->setFlags(EditWindow::WFMOVE);

  return T;
}

/*
 * This  function is like "forwline",  but goes  backwards.  The
 * scheme is  exactly the same. Figure out the new line and call
 * "movedot" to perform the motion.  No errors are possible.
 * Bound to "C-P".
 */

CMD
Editor::backline() {
  if ((Editor::_lastflag & CFCPCN) == 0) {
    /* Reset goal if the last isn't C-P, C-N  */
    Editor::_curgoal = Redisplay::_curcol;
  }

  Editor::_thisflag |= CFCPCN;
  auto dlp  = curwp->line();
  if (dlp->back() == curbp->lastline()) {
    term->beep();
    return NIL;
  }

  auto n = Editor::_repeat;

  while (n-- && dlp->back() != curbp->lastline()) {
    dlp = dlp->back();
  }

  curwp->setDot(dlp, dlp->getgoal(Editor::_curgoal));
  curwp->setFlags(EditWindow::WFMOVE);

  return T;
}

/*
 * Scroll  forward by a specified number of lines,  or by a full
 * page  if  no  argument.   Bound to "C-V".   The  "2"  in  the
 * arithmetic  on the window size is the overlap;  this value is
 * the  default overlap value in ITS EMACS.   Because this  zaps
 * the  top  line in the display window,  we have to do  a  hard
 * update.
 */

CMD
Editor::forwpage() {
  int l;

  if ((l = curwp->rows() - 2) <= 0) {
    /* Forget the overlap   */
    l = 1; /* if tiny window.      */
  }

  auto lp = curwp->topline();
  while (l-- && lp != curbp->lastline()) {
    lp = lp->forw();
  }

  curwp->setTopline(lp);
  curwp->setDot(lp, 0);
  curwp->setFlags(EditWindow::WFHARD);

  return T;
}

/*
 * This command is like "forwpage", but on the other window. The
 * value is from the GNU EMACS manual. Bound to "M-C-V.
 */

CMD
Editor::forwother() {
  if (EditWindow::next() == T && forwpage() == T) {
    return EditWindow::previous();
  } else {
    return NIL;
  }
}

/*
 * This command is like "forwpage",  but it goes backwards.  The
 * "2",  like above, is the overlap between the two windows. The
 * value is from the ITS EMACS manual.  Bound to "M-V".  We do a
 * hard update for exactly the same reason.
 */

CMD
Editor::backpage() {
  int l;

  if ((l = curwp->rows() - 2) <= 0) {
    /* Don't blow up if the window is tiny. */
    l = 1;
  }

  auto lp = curwp->topline();
  while (l-- && (lp->back() != curbp->lastline())) {
    lp = lp->back();
  }

  curwp->setTopline(lp);
  curwp->setDot(lp, 0);
  curwp->setFlags(EditWindow::WFHARD);

  return T;
}

/*
 * Set  the  mark  in  the current window to the value of "." in
 * the window. No errors are possible. Bound to "C-SPC".
 */

CMD
Editor::setmark() {
  curwp->setMark(curwp->getDot());
  WDGmessage(ECSTR("Mark set."));
  return T;
}

/*
 * Set the mark in the current window to the entire buffer. After
 * this command,  "." is set at the end of the buffer.  No errors
 * are possible. Bound to "C-XH".
 */

CMD
Editor::markwholebuffer() {
  /*
   * Set mark at the end of the buffer.
   */

  curwp->setMark(curbp->lastline(), 0);

  /*
   * Go to the beginning of the buffer.
   */

  (void)gotobob();

  WDGmessage(ECSTR("Mark set."));
  return T;
}

/*
 * Swap the values of "." and "mark" in the current window. This
 * is pretty easy, bacause all of the hard work gets done by the
 * standard routine that moves the mark about. The only possible
 * error is "no mark". Bound to "C-XC-X".
 */

CMD
Editor::swapmark() {
  const auto& mark = curwp->getMark();

  if (mark.line() == nullptr) {
    WDGmessage(ECSTR("No mark set in this buffer!"));
    return NIL;
  }

  const auto& dot = curwp->getDot();

  curwp->setDot(mark);
  curwp->setMark(dot);
  curwp->setFlags(EditWindow::WFMOVE);

  return T;
}

extern Kbdm kbdm;

static int  getccol();
static CMD  addprefix();
static bool prefixlinep(const Line* line, int len);

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
Editor::showcpos() {
  int nch = 0;
  int cbo = 0;
  int cac = MAX_EMCHAR;
  int nbl = 1;
  int col = getccol();
  int nbc = 0;

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
    WDGwrite(
      ECSTR("at line %d: col=%d row=%d code=(%d, %x) (%d%%)"),
      nbl, col, Redisplay::_currow, cac, cac,
      (nch == 0) ? 0 : (int)((100 * nbc) / nch));
  } else {
    EMCHAR buf[2];
    buf[0] = (EMCHAR)cac;
    buf[1] = '\000';
    WDGwrite(
      ECSTR("at line %d: col=%d row=%d char='%s' code=(%d, 0x%x) (%d%%)"),
      nbl, col, Redisplay::_currow, buf, cac, cac,
      (nch == 0) ? 0 : (int)((100 * nbc)/ nch));
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
 * within a line, so "EditWindow::WFEDIT" is good enough.
 */

CMD
Editor::twiddle() {
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
    (void)Editor::forwchar();
  }

  Buffer::change(EditWindow::WFEDIT);

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
Editor::quotechar() {
  auto n = Editor::_repeat;
  int  c;

  if ((c = term->get()) == '\n') {
    while (Line::newline() && --n) {
      continue;
    }
    return T;
  }
  return Line::insert(c, n) ? T : NIL;
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
Editor::tab() {
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
    return Editor::tabexpand();
  }

  if ((curbp->editMode() != EDITMODE::FUNDAMENTAL)
      && (curwp->pos() == 0)) {
    auto s = Indent::tabIndent();
    if (curwp->pos() == 0) {
      /*
       * Intdentation is still at 0 after trying to indent
       */
      Editor::_thisflag |= CFTAB;
    }
    return s;
  }

  return Editor::tabexpand();
}

CMD
Editor::tabexpand() {
  bool res;
  if (opt::tab_size == 8 &&
      curbp->editMode() != EDITMODE::JAVAMODE &&
      curbp->editMode() != EDITMODE::PYTHONMODE &&
      curbp->editMode() != EDITMODE::CSHARPMODE) {
    res = Line::insert('\t');
  } else {
    res = Line::insert(' ', opt::tab_size - (getccol() % opt::tab_size));
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
Editor::openline() {
  for (auto i = 0; i < Editor::_repeat; ++i) {
    if (curwp->pos() == 0 && opt::fill_prefix[0]) {
      for (auto j = 0; opt::fill_prefix[j]; ++j) {
        if (!Line::insert(opt::fill_prefix[j])) {
          return NIL;
        }
      }
    }

    if (!Line::newline()) {
      return NIL;
    }
  }

  if (Editor::backline() != T) {
    return NIL;
  }

  return Editor::gotoeol();
}

/*
 * Insert a newline.  Bound to "C-M".  Simply insert new line or
 * indent  depending  of  selected  mode (Fundamental,  C/C++ or
 * Lisp).
 */

CMD
Editor::endline() {
  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
  case EDITMODE::CSHARPMODE:
  case EDITMODE::FORTRANMODE:
  case EDITMODE::JAVAMODE:
  case EDITMODE::LISPMODE:
  case EDITMODE::PASCALMODE:
  case EDITMODE::PERLMODE:
  case EDITMODE::PROLOGMODE:
  case EDITMODE::PYTHONMODE:
  case EDITMODE::SGMLMODE:
  case EDITMODE::SHELLMODE:
    return Indent::newlineIndent();
  case EDITMODE::DIRED: {
    const auto dotp(curwp->line());

    if (dotp == curbp->lastline() || dotp->length() == 0) {
      return Editor::ctrlg();
    }

    EMCHAR buf[NFILEN];
    (void)emstrncpy(buf, dotp->text(), dotp->length());
    buf[dotp->length()] = '\000';
    return newfile(buf + DIREDMARK) ? T : NIL;
  }
  default:
    break;
  }
  return Editor::newline();
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
Editor::newline() {
  auto n = Editor::_repeat;

  while (n--) {
    const auto& dot(curwp->getDot());
    const auto dotp(dot.line());
    const auto doto(dot.pos());
    auto s(T);
    if ((dotp->length() == doto)
        && (dotp != curbp->lastline())
        && (dotp->forw()->length() > 0)) {
      if ((s = Editor::forwchar()) != T) {
        return s;
      }
    } else if (!Line::newline()) {
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
Editor::delblank() {
  auto lp1 = curwp->line();

  Line* lp2;
  while (lp1->length() == 0 && (lp2 = lp1->back()) != curbp->lastline()) {
    lp1 = lp2;
  }

  int nld = 0;
  for (lp2 = lp1;
       (lp2 = lp2->forw()) != curbp->lastline() && lp2->length() == 0;) {
    ++nld;
  }

  if (nld == 0) {
    return T;
  }

  curwp->setDot(lp1->forw(), 0);

  return Line::remove(nld) ? T : NIL;
}

/*
 * Delete forward.  This is real easy,  because the basic delete
 * routine does all of the work.  If any argument is present, it
 * kills  rather than deletes,  to prevent loss of text if typed
 * with a big argument. Normally bound to "C-D".
 */

CMD
Editor::forwdel() {
  return Line::remove(Editor::_repeat) ? T : NIL;
}

/*
 * Delete  backwards.  This is quite easy too,  because it's all
 * done  with  other functions.  Just move the cursor back,  and
 * delete  forwards.  Like delete forward,  this actually does a
 * kill  if  presented with an argument.  Bound to both "RUBOUT"
 * and "C-H".
 */

CMD
Editor::backdel() {
  if (curbp->readonly()) {
    if (curbp->editMode() == EDITMODE::BufferMODE) {
      return Buffer::buffercmd(0x08);
    }

    if (curbp->editMode() == EDITMODE::DIRED) {
     return Completion::diredCommand(0x08);
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

      if (Editor::backchar() == T) {
        (void)Line::remove(1);
      }

      pos = getccol();

      /*
       *      Replace by blanks
       */

      do {
        (void)Line::insert(' ');
      } while (++pos % opt::tab_display);
    }
  }

  if (Editor::backchar() == T) {
    return Line::remove(Editor::_repeat) ? T : NIL;
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
Editor::killtext() {
  if ((Editor::_lastflag & CFKILL) == 0) {
    /* Clear kill buffer if */
    KillBuf::clear(); /* last wasn't a kill.  */
  }

  Editor::_thisflag |= CFKILL;

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto doto(dot.pos());
  auto chunk = dotp->length() - doto;

  if (chunk == 0) {
    chunk = 1;
  }

  return Line::remove(chunk, true) ? T : NIL;
}

/*
 * Yank text back from the kill buffer.  This is really easy.  All of
 * the work is done by the standard insert routines.  All you do is
 * run the loop, and check for errors.  Bound to "C-Y".  The blank
 * lines are inserted with a call to "newline" instead of a call to
 * "Line::newline" so that the magic stuff that happens when you
 * type a carriage return also happens when a carriage return is
 * yanked back from the kill buffer.  As a special case we check if
 * yank is done at the end of buffer.  If so, insert push a new line
 * at the end of buffer that must be removed.
 */

CMD
Editor::yank() {
  auto n          = Editor::_repeat;
  auto save       = Editor::_repeat;
  auto lastlflag  = (curwp->line() == curbp->lastline());
  auto firstlflag = (curwp->line() == curbp->firstline());

  Editor::_repeat = 1;

  WDGclippaste();

  while (n--) {
    int c;

    for (auto i = 0; (c = KillBuf::remove(i)) >= 0; ++i) {
      if (c == '\n') {
        if (!Line::newline()) {
          return NIL;
        }
      } else if (!Line::insert(c)) {
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

    (void)Editor::forwdel();

    if (firstlflag) {
      /*
       * yank was done on an empty buffer, force redisplay
       * for all active buffer.
       */

      for (auto wp : EditWindow::list()) {
        if (wp->buffer() == curbp) {
          wp->setFlags(EditWindow::WFFORCE);
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
Editor::appendNextKill() {
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
prefixlinep(const Line *line, int len) {
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
Editor::setFillColumn() {
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
Editor::setFillPrefix() {
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
  for (i = 0; i < doto; ++i) {
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
    for (auto i = 0; opt::fill_prefix[i]; ++i) {
      if (!Line::insert(opt::fill_prefix[i])) {
        return NIL;
      }
    }
  }

  curwp->setDotPos(len);

  while ((curwp->pos() < curwp->line()->length())
         && curwp->getChar() == ' ') {
    (void)Line::remove(1);
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
Editor::fillParagraph() {
  if (freadonly()) {
    return NIL;
  }

  auto len     = emstrlen(opt::fill_prefix);
  auto oldmode = curbp->editMode();

  curbp->setEditMode(EDITMODE::FUNDAMENTAL);

  (void)Editor::backParagraph();

  curwp->setDotPos(0);
  if (prefixlinep(curwp->line(), len)) {
    (void)Line::remove(len);
  }

  /*
   * Convert paragraph in only one line removing previous prefix.
   */

  while (curwp->line()->forw()->length() > len
         && prefixlinep(curwp->line()->forw(), len)) {
    (void)Editor::gotoeol();
    (void)Line::remove(1);
    (void)Line::insert(' ');
    (void)Line::remove(len);
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

    (void)Editor::forwchar();

    if (nbspace) {
      while (curwp->pos() < curwp->line()->length()) {
        if (curwp->getChar() != ' ') {
          break;
        }
        (void)Line::remove(1);
      }
    }
  }

  /*
   *  Split this line within the fill_column and fill_prefix.
   */

  (void)addprefix();

  curwp->setDotPos(0);
  while (curwp->line()->position() >= opt::fill_column) {
    if (Editor::splitLineToFill() != T) {
      break;
    }
  }

  (void)Editor::gotoeol();

  /*
   * Reset the _curcol to the current position (any better solution ?)
   */

  Redisplay::_curcol = Editor::_curgoal = getccol();

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
Editor::splitLineToFill() {
  int  bpos = 0;                        /* break position      */
  int  dpos = 0;                        /* display position    */
  int  fpos = 0;                        /* forced break        */
  int  fflg = 1;                        /* fill prefix flag    */

  /*
   * Compute the break position in 'bpos'.
   */

  const auto& dot(curwp->getDot());
  const auto dotp(dot.line());
  const auto lmax(dotp->length());  // max position
  for (int i = 0; i < lmax; ++i) {
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

    (void)Editor::endline();
    if (justmode != JUSTLEFT) {
      /*
       * assumes full-justify
       */
      (void)Editor::backline();
      if (curbp->editMode() == EDITMODE::SGMLMODE) {
        (void)Editor::justifyComment();
      } else {
        (void)Editor::justifyCurLine();
      }
      (void)Editor::forwline();
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
Editor::justifyCurLine() {
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
      (void)Editor::forwchar();
      if ((curwp->pos() < (curwp->line()->length() - 1))
          && curwp->getChar() == ' ') {
        (void)Line::insert(' ');
      }
    } else {
      (void)Editor::forwchar();
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
          (void)Editor::forwchar();
          nbspace++;
        } while (curwp->pos() < curwp->line()->length());

        if (nbspace < maxspace) {
          (void)Line::insert(' ');
          justifyed = true;
        }
      } else {
        (void)Editor::forwchar();
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
Editor::backParagraph() {
  auto len = emstrlen(opt::fill_prefix);

  while (curwp->line()->back() != curbp->lastline() &&
         Editor::backline() == T &&
         curwp->line()->length() > len &&
         prefixlinep(curwp->line(), len)) {
    /* empty loop*/
    continue;
  }

  if (curwp->line() != curbp->firstline()) {
    (void)Editor::forwline();
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
Editor::forwParagraph() {
  auto len = emstrlen(opt::fill_prefix);

  while (curwp->line() != curbp->firstline() &&
         Editor::forwline() == T &&
         curwp->line()->length() > len &&
         prefixlinep(curwp->line(), len)) {
    /* empty loop*/
    continue;
  }

  if (curwp->line() != curbp->lastline()) {
    (void)Editor::backline();
  }

  (void)Editor::gotoeol();

  return T;
}

/*
 * Mark the paragraph. Bound to M-h
 */

CMD
Editor::markParagraph() {
  (void)Editor::backParagraph();
  (void)Editor::setmark();
  (void)Editor::forwParagraph();

  return T;
}

/*
 * Insert only one blank around two words. Bound to M-SPACE
 */

CMD
Editor::justOneSpace() {
  EMCHAR c;

  if (curwp->pos() == curwp->line()->length()) {
    /*
     * At the end, back one char.
     */
    (void)Editor::backchar();
    if ((c = curwp->getChar()) != ' ' && c != '\t') {
      (void)Editor::forwchar();
      (void)Line::insert(' ');
      return T;
    }
  }

  do {
    if (Editor::backchar() == NIL) {
      break;
    }
  } while (curwp->pos() >= 0 && ((c = curwp->getChar()) == ' ' || c == '\t'));

  if ((c = curwp->getChar()) != ' ' && c != '\t') {
    (void)Editor::forwchar();
  }

  (void)Line::insert(' ');

  while ((curwp->pos() < curwp->line()->length()) &&
         ((c = curwp->getChar()) == ' ' || c == '\t')) {
    (void)Editor::forwdel();
  }

  return T;
}

/*
 * Set  the  justification mode to 'left' and adjust the current
 * paragraph using this mode. Unbound.
 */

CMD
Editor::setJustifyLeft() {
  justmode = JUSTLEFT;

  return Editor::fillParagraph();
}

/*
 * Set the justification mode to 'full' and adjust the current
 * paragraph using this mode. Unbound.
 */

CMD
Editor::setJustifyFull() {
  justmode = JUSTFULL;

  return Editor::fillParagraph();
}

/*
 * Justify a comment (for programs like C/C++ ..).  Start at the
 * beginning  of  the  line  and find the first character of the
 * comment.  At this point,  set the fill prefix and justify the
 * current paragraph. Bound to to M-;
 */

CMD
Editor::justifyComment() {
  static constexpr EMCHAR skip[] = { ' ', '\t', '#', '*', '/', '-', ';', 0 };

  curwp->setDotPos(0);

  auto len(curwp->line()->length());
  while (curwp->pos() < len) {
    auto c = curwp->getChar();

    int i;
    for (i = 0; skip[i]; ++i) {
      if (c == skip[i]) {
        break;
      }
    }

    if (skip[i] == 0) {
      break;
    }

    (void)Editor::forwchar();
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
  (void)Editor::fillParagraph();
  opt::fill_prefix[0] = '\0';

  return T;
}

/*
 * No Editor::Editor::undo yet !
 */

CMD
Editor::Editor::undo() {
  term->beep();
  WDGwrite(ECSTR("'Editor::Editor::undo' not yet implemented ! Sorry."));
  return NIL;
}

/*
 * Force an error to go to debugger.
 */

CMD
Editor::enterDebug() {
  int* p;
  int* bug = nullptr;

  if (Editor::_repeat == 666) {
    p  = bug;
    *p = 1;
  }

  WDGwrite(ECSTR("enter-debug needs to be called with 666."));
  return NIL;
}
