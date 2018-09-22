#if     !defined(lint)
static  char rcsid[] = "$Id: window.cpp,v 1.19 2018/09/04 05:13:09 jullien Exp $";
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
 * Window  management.  Some of the functions are internal,  and
 * some are attached to keys that the user actually types.
 */

#include "emacs.h"

std::list<WINSCR*> WINSCR::_wlist;

WINSCR::WINSCR(BUFFER* bp) noexcept
  : _ntrows{TTYnrow - 1} {
	if (_wlist.empty()) {
    curwp = this;
    _wlist.push_front(this);
	}
	if (bp != nullptr) {
		connect(bp, false);
	}
}

WINSCR::~WINSCR() {
  disconnect();
  _wlist.remove(this);
}

/*
 * Pick  a  window  for  a pop-up.  Split the screen if there is
 * only  one  window.  Pick  the uppermost window that isn't the
 * current window.
 */
WINSCR*
WINSCR::popup() noexcept {
  if (WINSCR::list().size() == 1) {
    if (splitwind() == NIL) {
      return nullptr;
    }
  }

  for (auto wp : WINSCR::list()) {
    if (wp != curwp) {
      return wp;
    }
  }

  return nullptr;
}

/*
 * Redisplay  the  screen  after  a resize.  Useful on Windowing
 * system. This command is not bound to any key stroke.
 */

bool
WINSCR::resize() noexcept {
  BUFFER* bp{nullptr};

  for (auto wp : WINSCR::list()) {
    if (wp != curwp) {
      bp = wp->buffer();
      break;
    }
  }

  (void)onlywind();

  auto head(WINSCR::list().front());

  if (head == nullptr) {
    return false;
  }

  head->_ntrows = (TTYnrow - 1);
  display->modeline(curwp);

  (void)WDGtitle(curbp->bufname(), curbp->filename());

  if (bp != nullptr && (TTYnrow >= 4)) {
    auto wp = WINSCR::popup();
    if (wp == nullptr) {
      return false;
    } else {
      return wp->connect(bp);
    }
  } else {
    return true;
  }
}

/*
 * Disconnect  the  buffer  associated  to the window pointed by
 * "wp".  If the buffer display count equals 0 (meaning that the
 * buffer  is  no  more  displayed on the screen) then copy mark
 * and   dot  values  in  the  buffer.  This  function  is  used
 * internally.
 */

void
WINSCR::disconnect() noexcept {
  auto bp(this->buffer());

  if (bp != nullptr) {
    if (bp->count() == 1) {
      bp->setDot(getDot());
      bp->setMark(getMark());
      bp->decr();
    } else {
      bp->decr();
    }
  }
  this->_bufp = nullptr;
}

/*
 * Connect  the  buffer  "bp" to the window pointed by "wp".  If
 * another  window  point  to  the same buffer copy mark and dot
 * values in the buffer. This function is used internally.
 */

bool
WINSCR::connect(BUFFER* bp, bool check) noexcept {
  this->disconnect();

  if (this == curwp) {
    bp->ontop();
  }

  this->_bufp = bp;
  this->_toplinep = bp->lastline();  /* For macros, ignored. */
  this->setFlags(WINSCR::WFMODE|WINSCR::WFFORCE|WINSCR::WFHARD);

  bp->incr();

  if (bp->count() == 1) {
    /*
     * First use, get the values from the buffer.
     */
    this->setDot(bp->getDot());
    this->setMark(bp->getMark());
  } else {
    /*
     * get the current values of the window already on screen.
     */
    for (auto wp : WINSCR::list()) {
      if (wp != curwp && wp->buffer() == bp) {
        this->setDot(wp->getDot());
        this->setMark(wp->getMark());
        break;
      }
    }
  }

  if (check) {
    BUFFER::validitycheck(__func__);
  }
  return true;
}

/*
 * Make the window pointed by wp the current window.  It restack
 * the  buffer so that it becomes on top of the list for command
 * switch-buffer or kill-buffer.
 */

void
WINSCR::current() noexcept {
  curwp = this;
  this->buffer()->ontop();
}

/*
 * Reposition dot in the current window. Bound to "M-!"
 */

CMD
reposition() {
  curwp->_force = Editor::_repeat;
  curwp->setFlags(WINSCR::WFFORCE);
  return T;
}

/*
 * Reposition  dot  in  the  middle  of  the  current window and
 * refresh the entire screen. Bound to "C-L".
 */

CMD
recenter() {
  curwp->_force = curwp->rows() / 2;
  curwp->setFlags(WINSCR::WFFORCE);
  redrawscreen();

  return T;
}

/*
 * The  command  make the next window the current window.  There
 * are  no  real  errors,  although  the command does nothing if
 * there is only 1 window on the screen. Bound to "C-XC-N".
 */

CMD
nextwind() {
  auto next(curwp->down());

  if (next == nullptr) {
    next = WINSCR::list().front();
  }

  next->current();

  return T;
}

/*
 * This  command  makes  the previous window the current window.
 * There  arn't  any errors,  although the command does not do a
 * lot if there is 1 window. Bound to "C-XO".
 */

CMD
prevwind() { 
  auto prev(curwp->up());

  if (prev == nullptr) {
    prev = WINSCR::list().back();
  }

  prev->current();

  return T;
}

/*
 * This  command makes the top window the current window.  There
 * arn't  any errors,  although the command does not do a lot if
 * there is 1 window. Bount to C-XT
 */

CMD
topwind() {
  for (auto wp : WINSCR::list()) {
    if (wp->toprow() == 0) {
      wp->current();
      return T;
    }
  }

  return NIL;
}

/*
 * This  command  moves  the current window down by "arg" lines.
 * Recompute  the  top line in the window.  Most of the work has
 * to  do with reframing the window,  and picking a new dot.  We
 * share  the code by having "move down" just be an interface to
 * "move up". Bound to "C-XC-N".
 */

CMD
mvdnwind() {
  auto save = Editor::_repeat;

  Editor::_repeat = -Editor::_repeat;
  (void)mvupwind();
  Editor::_repeat = save;

  return T;
}

/*
 * Move the current window up by "arg" lines.  Recompute the new
 * top  line of the window.  Look to see if "." is still on  the
 * screen.  If it is, you win.  If it isn't,  then move  "."  to
 * center it in the new framing of the window (this command does
 * not really move "."; it moves the frame). Bound to "C-XC-P".
 */

CMD
mvupwind() {
  auto lp = curwp->topline();
  auto n = Editor::_repeat;

  if (n < 0) {
    while (n++ && lp != curbp->lastline()) {
      lp = lp->forw();
    }
  } else {
    while (n-- && lp->back() != curbp->lastline()) {
      lp = lp->back();
    }
  }

  curwp->setTopline(lp);
  curwp->setFlags(WINSCR::WFHARD);

  for (int i = 0; i < curwp->rows(); ++i) {
    if (lp == curwp->line()) {
      return T;
    }
    if (lp == curbp->lastline()) {
      break;
    }
    lp = lp->forw();
  }

  lp = curwp->topline();
  for (int i = curwp->rows() / 2; i != 0 && lp != curbp->lastline(); --i) {
    lp = lp->forw();
  }

  curwp->setDot(lp, 0);
  return T;
}

/*
 * This  command makes the current window the only window on the
 * screen.  Bound to "C-X1".  Try to set the framing so that "."
 * does  not  have  to move on the display.  Some care has to be
 * taken  to  keep  the  values  of  dot  and mark in the buffer
 * structures  right  if  the  distruction  of  a window makes a
 * buffer become undisplayed.
 */

CMD
onlywind() {
  for (auto wp : WINSCR::list()) {
    if (wp != curwp) {
      delete wp;
    }
  }

  WINSCR::list().erase(WINSCR::list().begin(), WINSCR::list().end());
  WINSCR::list().push_front(curwp);

  auto lp = curwp->_toplinep;

  for (int i = curwp->toprow(); i!=0 && lp->back()!=curbp->lastline(); --i) {
    lp = lp->back();
  }

  curwp->_toprow   = 0;
  curwp->_ntrows   = (TTYnrow - 1);
  curwp->_toplinep = lp;
  curwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  return T;
}

/*
 * Delete  current  window.  Do  nothing  if  there  is only one
 * window. Bound to C-X0.
 */

CMD
delwind() {
  if (WINSCR::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  WINSCR* wp;

  if (curwp == WINSCR::list().front()) {
    wp = curwp->down();
    wp->_toprow = 0;
  } else {
    wp = curwp->up();
  }

  wp->_ntrows += curwp->rows() + 1;
  wp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);

  delete curwp;

  wp->current();

  return T;
}

/*
 * Split  the  current  window.  A  window  smaller than 3 lines
 * cannot  be split.  The only other error that is possible is a
 * "malloc"   failure  allocating  the  structure  for  the  new
 * window. Bound to "C-X2".
 */

CMD
splitwind() {
  if (curwp->rows() < 3) {
    WDGmessage(ECSTR("You can't have windows smaller than 2 lines high"));
    return NIL;
  }

  auto wp = new WINSCR{curbp};
  wp->setDot(curwp->getDot());
  wp->setMark(curwp->getMark());
  wp->setFlags(WINSCR::WFCLEAR);

  auto ntru = (curwp->rows() - 1) / 2;         /* Upper size           */
  auto ntrl = (curwp->rows() - 1) - ntru;      /* Lower size           */
  auto ntrd = 0;

  EDLINE* lp;

  for (lp = curwp->topline(); lp != curwp->line(); ++ntrd) {
    lp = lp->forw();
  }

  lp = curwp->topline();

  if (ntrd <= ntru) {                    /* Old is upper window. */
    if (ntrd == ntru) {
      /* Hit mode line. */
      lp = lp->forw();
    }
    curwp->_ntrows = ntru;

    auto insertIt = std::find(WINSCR::list().begin(),
                              WINSCR::list().end(),
                              curwp);
    if (insertIt != WINSCR::list().end()) {
      WINSCR::list().insert(++insertIt, wp);
    }        

    wp->_toprow = curwp->toprow() + ntru + 1;
    wp->_ntrows = ntrl;
  } else {                               /* Old is lower window  */
    auto insertIt = std::find(WINSCR::list().begin(),
                              WINSCR::list().end(),
                              curwp);
    if (insertIt != WINSCR::list().end()) {
      WINSCR::list().insert(insertIt, wp);
    } else {
      WINSCR::list().push_front(wp);
    }

    wp->_toprow = curwp->toprow();
    wp->_ntrows = ntru;
    ++ntru;                         /* Mode line.           */
    curwp->_toprow += ntru;
    curwp->_ntrows  = ntrl;
    while (ntru--) {
      lp = lp->forw();
    }
  }

  curwp->setTopline(lp);          /* Adjust the top lines */
  curwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  wp->setTopline(lp);
  wp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);

  return T;
}

/*
 * Enlarge  the  current  window.  Find  the  window  that loses
 * space.  Make  sure it is big enough.  If so,  hack the window
 * descriptions,  and ask redisplay to do all the hard work. You
 * don't just set "force reframe" because dot would move.  Bound
 * to "C-XZ".
 */

CMD
enlargewind() {
  if (WINSCR::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  auto adjwp = curwp->down();
  bool below{true};

  if (adjwp == nullptr) {
    below = false;
    adjwp = WINSCR::list().front();
  }

  if (adjwp->rows() <= Editor::_repeat) {
    WDGmessage(ECSTR("Window too small for splitting"));
    return NIL;
  }

  if (below) {
    /*
     * Shrink below.
     */
    auto lp = adjwp->topline();
    for (int i(0);
         i < Editor::_repeat && lp != adjwp->buffer()->lastline();
         ++i) {
      lp = lp->forw();
    }
    adjwp->_toplinep = lp;
    adjwp->_toprow  += Editor::_repeat;
  } else {
    /*
     * Shrink above.
     */
    auto lp = curwp->topline();
    for (int i(0);
         i < Editor::_repeat && lp->back() != curbp->lastline();
         ++i) {
      lp = lp->back();
    }
    curwp->_toplinep = lp;
    curwp->_toprow  -= Editor::_repeat;
  }

  curwp->_ntrows += Editor::_repeat;
  curwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  adjwp->_ntrows -= Editor::_repeat;
  adjwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  return T;
}

/*
 * Shrink the current window.  Find the window that gains space.
 * Hack at the window descriptions.  Ask the redisplay to do all
 * the hard work. Bound to "C-XC-Z".
 */

CMD
shrinkwind() {
  if (WINSCR::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  auto adjwp = curwp->down();
  bool below{true};

  if (adjwp == nullptr) {
    below = false;
    adjwp = WINSCR::list().front();
  }

  if (curwp->rows() <= Editor::_repeat) {
    WDGmessage(ECSTR("Window too small for splitting"));
    return NIL;
  }

  if (below) {
    /*
     * Grow below.
     */
    auto lp = adjwp->topline();
    for (int i(0);
         i < Editor::_repeat && lp->back() != adjwp->buffer()->lastline();
         ++i) {
      lp = lp->back();
    }
    adjwp->_toplinep = lp;
    adjwp->_toprow  -= Editor::_repeat;
  } else {
    /*
     * Grow above.
     */
    auto lp = curwp->topline();
    for (int i(0); i < Editor::_repeat && lp != curbp->lastline(); ++i) {
      lp = lp->forw();
    }
    curwp->_toplinep = lp;
    curwp->_toprow  += Editor::_repeat;
  }
  curwp->_ntrows -= Editor::_repeat;
  curwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  adjwp->_ntrows += Editor::_repeat;
  adjwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
  return T;
}

/*
 * This portion of code deal with the mouse click.
 */

CMD
findwind() {
  EDLINE* lp{nullptr};
  auto wx = mevent.x;
  auto wy = mevent.y;
  auto l  = wy;
  int  i;

  for (auto wp : WINSCR::list()) {
    auto top  = wp->toprow();
    auto nrow = wp->rows();
    if (top <= l && top + nrow >= l) {
      wp->current();
      lp = wp->topline();
      if ((top + nrow) == l) {
        auto resizep = NIL;
        if (wx == (TTYncol - 4) || wx == (TTYncol-3)) {
          (void)forwpage();
        } else if (wx==(TTYncol-7) || wx==(TTYncol-6)) {
          (void)backpage();
        } else {
          switch (mevent.button) {
          case MEvent::MButton1 :
            resizep = shrinkwind();
            break;
          case MEvent::MButton2 :
            resizep = enlargewind();
            break;
          default :
            WDGmessage(ECSTR("Not such window!"));
            (void)ctrlg();
            return NIL;
          }
        }

        if (resizep == NIL) {
          waitmatch(1);
        }

        return T;
      }

      for (l = top; l < wy && lp != wp->line(); ++l) {
        lp = lp->forw();
      }

      if (l < wy) {
        while (l++ < wy) {
          (void)forwline();
        }
      } else {
        while (lp != wp->line()) {
          (void)backline();
        }
      }

      wp->setDotPos(0);
      l = 0;
      i = 0;

      const auto& dot(curwp->getDot());
      while (l < wx && i < dot.line()->length()) {
        if (dot.line()->get(i++) == '\t') {
          do {
            ++l;
          } while (l % opt::tab_display);
        } else {
          ++l;
        }
        (void)forwchar();
      }

      if (wp->buffer()->editMode() == EDITMODE::BUFFERMODE) {
        return buffercmd('f');
      } else if (wp->buffer()->editMode() == EDITMODE::DIRED) {
        return diredcmd('f');
      }

      switch (mevent.button) {
      case MEvent::MButton1:
        /* mouse-track */
        (void)setmark();
        break;
      case MEvent::MButton2:
      case MEvent::MButton3:
        /* x-set-point-and-insert-selection */
        WDGclippaste();
        (void)yank();
        display->update();
        break;
      case MEvent::MButton7:
        /* mouse-track insert */
        (void)copyregion();
        (void)swapmark();
        WDGclipcopy();
        break;
      case MEvent::MButton8:
        /* x-mouse-kill */
        (void)killregion();
        WDGclipcopy();
        break;
      case MEvent::MButton4:
        /* mouse-track-adjust */
        (void)copyregion();
        WDGclipcopy();
        break;
      }

      Editor::_thisflag = CFCPCN;
      Editor::_lastflag = CFUNSET;

      return T;
    }
  }

  return T;
}

CMD
adjust() {
  WDGadjust();
  return T;
}

