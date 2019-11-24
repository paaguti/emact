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
