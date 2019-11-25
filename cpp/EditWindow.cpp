#if !defined(lint)
static auto rcsid("$Id: window.cpp,v 1.19 2018/09/04 05:13:09 jullien Exp $");
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

#include "./emacs.h"
#include "./EditWindow.h"
#include "./Completion.h"
#include "./Line.h"
#include "./Redisplay.h"
#include "./Search.h"
#include "./TextRegion.h"

std::list<EditWindow*> EditWindow::_wlist;

EMCHAR
EditWindow::getChar() const {
  return _dot.line()->get(_dot.pos());
}

void
EditWindow::setChar(int c) {
  _dot.line()->put(_dot.pos(), c);
}

EditWindow::EditWindow(Buffer* bp) noexcept
  : _ntrows{term->getNbRows() - 1} {
  if (_wlist.empty()) {
    curwp = this;
    _wlist.push_front(this);
  }
  if (bp != nullptr) {
    connect(bp, false);
  }
}

EditWindow::~EditWindow() {
  disconnect();
  _wlist.remove(this);
}

/*
 * Pick  a  window  for  a pop-up.  Split the screen if there is
 * only  one  window.  Pick  the uppermost window that isn't the
 * current window.
 */
EditWindow*
EditWindow::popup() noexcept {
  if (_wlist.size() == 1) {
    if (split() == NIL) {
      return nullptr;
    }
  }

  for (auto wp : _wlist) {
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
EditWindow::resize() noexcept {
  Buffer* bp{nullptr};

  /*
   * Search for the first buffer that may exist on another window.
   */
  for (auto wp : _wlist) {
    if (wp != curwp) {
      bp = wp->buffer();
      break;
    }
  }

  /*
   * Kill all windows except the current one.
   */
  (void)onlywind();

  auto head(_wlist.front());  // head should be the same as curwp

  if (head == nullptr) {
    return false;
  }

  /*
   * Update head and force a complete redraw.
   */
  head->_ntrows = (term->getNbRows() - 1);
  redisplay->modeline(head);
  head->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);

  (void)WDGtitle(curbp->bufname(), curbp->filename());

  /*
   * Try to create a second window connected to bp (if it exist).
   */
  if (bp != nullptr && (term->getNbRows() >= 4)) {
    auto wp = EditWindow::popup();
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
EditWindow::disconnect() noexcept {
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
EditWindow::connect(Buffer* bp, bool check) noexcept {
  this->disconnect();

  if (this == curwp) {
    bp->ontop();
  }

  this->_bufp = bp;
  this->_toplinep = bp->lastline();  /* For macros, ignored. */
  this->setFlags(EditWindow::WFMODE|EditWindow::WFFORCE|EditWindow::WFHARD);

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
    for (auto wp : _wlist) {
      if (wp != curwp && wp->buffer() == bp) {
        this->setDot(wp->getDot());
        this->setMark(wp->getMark());
        break;
      }
    }
  }

  if (check) {
    Buffer::validitycheck(__func__);
  }
  return true;
}

/*
 * Make the window pointed by wp the current window.  It restack
 * the  buffer so that it becomes on top of the list for command
 * switch-buffer or kill-buffer.
 */

void
EditWindow::current() noexcept {
  curwp = this;
  this->buffer()->ontop();
}

/*
 * Reposition dot in the current window. Bound to "M-!"
 */

CMD
EditWindow::reposition() {
  curwp->_force = Editor::_repeat;
  curwp->setFlags(EditWindow::WFFORCE);
  return T;
}

/*
 * Reposition  dot  in  the  middle  of  the  current window and
 * refresh the entire screen. Bound to "C-L".
 */

CMD
EditWindow::recenter() {
  curwp->_force = curwp->rows() / 2;
  curwp->setFlags(EditWindow::WFFORCE);
  Editor::redrawscreen();

  return T;
}

/*
 * The  command  make the next window the current window.  There
 * are  no  real  errors,  although  the command does nothing if
 * there is only 1 window on the screen. Bound to "C-XC-N".
 */

CMD
EditWindow::next() {
  auto next(curwp->down());

  if (next == nullptr) {
    next = EditWindow::list().front();
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
EditWindow::previous() {
  auto prev(curwp->up());

  if (prev == nullptr) {
    prev = EditWindow::list().back();
  }

  prev->current();

  return T;
}

/*
 * This  command  moves  the current window down by "arg" lines.
 * Recompute  the  top line in the window.  Most of the work has
 * to  do with reframing the window,  and picking a new dot.  We
 * share  the code by having "move down" just be an interface to
 * "move up". Bound to "C-XC-N".
 */

CMD
EditWindow::moveDown() {
  auto save = Editor::_repeat;

  Editor::_repeat = -Editor::_repeat;
  (void)moveUp();
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
EditWindow::moveUp() {
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
  curwp->setFlags(EditWindow::WFHARD);

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
EditWindow::onlywind() {
  if (EditWindow::list().size() == 1) {
    return T;  // already a single Window exists.
  }

  /*
   * keep curwp out of Window list.
   */
  EditWindow::list().remove(curwp);

  /*
   * remove all other Window still in list.
   */

#if 1
  for (std::list<EditWindow*>::iterator it = EditWindow::list().begin();
       it != EditWindow::list().end();) {
    delete *it++;
  }
#else
  for (bool loop{true}; loop; loop = false) {
    for (auto wp : EditWindow::list()) {
      delete wp;
      loop = true;
      break;
    }
  }
#endif

  /*
   * push curwp in Window list.
   */
  EditWindow::list().push_front(curwp);

  auto lp = curwp->_toplinep;

  for (int i{curwp->toprow()}; i != 0 && lp->back() != curbp->lastline(); --i) {
    lp = lp->back();
  }

  curwp->_toprow   = 0;
  curwp->_ntrows   = (term->getNbRows() - 1);
  curwp->_toplinep = lp;
  curwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);

  return T;
}

/*
 * Delete  current  window.  Do  nothing  if  there  is only one
 * window. Bound to C-X0.
 */

CMD
EditWindow::delwind() {
  if (EditWindow::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  EditWindow* wp;

  if (curwp == EditWindow::list().front()) {
    wp = curwp->down();
    wp->_toprow = 0;
  } else {
    wp = curwp->up();
  }

  wp->_ntrows += curwp->rows() + 1;
  wp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);

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
EditWindow::split() {
  if (curwp->rows() < 3) {
    WDGmessage(ECSTR("You can't have windows smaller than 2 lines high"));
    return NIL;
  }

  auto wp = new EditWindow{curbp};
  wp->setDot(curwp->getDot());
  wp->setMark(curwp->getMark());
  wp->setFlags(EditWindow::WFCLEAR);

  auto ntru = (curwp->rows() - 1) / 2;         /* Upper size           */
  auto ntrl = (curwp->rows() - 1) - ntru;      /* Lower size           */
  auto ntrd = 0;

  Line* lp;

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

    auto insertIt = std::find(EditWindow::list().begin(),
                              EditWindow::list().end(),
                              curwp);
    if (insertIt != EditWindow::list().end()) {
      EditWindow::list().insert(++insertIt, wp);
    }

    wp->_toprow = curwp->toprow() + ntru + 1;
    wp->_ntrows = ntrl;
  } else {                               /* Old is lower window  */
    auto insertIt = std::find(EditWindow::list().begin(),
                              EditWindow::list().end(),
                              curwp);
    if (insertIt != EditWindow::list().end()) {
      EditWindow::list().insert(insertIt, wp);
    } else {
      EditWindow::list().push_front(wp);
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
  curwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);
  wp->setTopline(lp);
  wp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);

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
EditWindow::enlarge() {
  if (EditWindow::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  auto adjwp = curwp->down();
  bool below{true};

  if (adjwp == nullptr) {
    below = false;
    adjwp = EditWindow::list().front();
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
  curwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);
  adjwp->_ntrows -= Editor::_repeat;
  adjwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);
  return T;
}

/*
 * Shrink the current window.  Find the window that gains space.
 * Hack at the window descriptions.  Ask the redisplay to do all
 * the hard work. Bound to "C-XC-Z".
 */

CMD
EditWindow::shrink() {
  if (EditWindow::list().size() == 1) {
    WDGmessage(ECSTR("Only one window"));
    return NIL;
  }

  auto adjwp = curwp->down();
  bool below{true};

  if (adjwp == nullptr) {
    below = false;
    adjwp = EditWindow::list().front();
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
  curwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);
  adjwp->_ntrows += Editor::_repeat;
  adjwp->setFlags(EditWindow::WFMODE|EditWindow::WFHARD);
  return T;
}

/*
 * This portion of code deal with the mouse click.
 */

CMD
EditWindow::find() {
  Line* lp{nullptr};
  auto wx = mevent.x;
  auto wy = mevent.y;
  auto l  = wy;
  int  i;

  for (auto wp : EditWindow::list()) {
    auto top  = wp->toprow();
    auto nrow = wp->rows();
    if (top <= l && top + nrow >= l) {
      wp->current();
      lp = wp->topline();
      if ((top + nrow) == l) {
        auto resizep = NIL;
        if (wx == (term->getNbCols() - 4)
            || wx == (term->getNbCols() - 3)) {
          (void)Editor::forwpage();
        } else if (wx == (term->getNbCols() - 7)
                   || wx == (term->getNbCols() - 6)) {
          (void)Editor::backpage();
        } else {
          switch (mevent.button) {
          case MEvent::MButton1 :
            resizep = shrink();
            break;
          case MEvent::MButton2 :
            resizep = enlarge();
            break;
          default :
            WDGmessage(ECSTR("Not such window!"));
            (void)Editor::ctrlg();
            return NIL;
          }
        }

        if (resizep == NIL) {
          Search::wait(1);
        }

        return T;
      }

      for (l = top; l < wy && lp != wp->line(); ++l) {
        lp = lp->forw();
      }

      if (l < wy) {
        while (l++ < wy) {
          (void)Editor::forwline();
        }
      } else {
        while (lp != wp->line()) {
          (void)Editor::backline();
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
        (void)Editor::forwchar();
      }

      if (wp->buffer()->editMode() == EDITMODE::BufferMODE) {
        return Buffer::buffercmd('f');
      } else if (wp->buffer()->editMode() == EDITMODE::DIRED) {
        return Completion::diredCommand('f');
      }

      switch (mevent.button) {
      case MEvent::MButton1:
        /* mouse-track */
        (void)Editor::setmark();
        break;
      case MEvent::MButton2:
      case MEvent::MButton3:
        /* x-set-point-and-insert-selection */
        WDGclippaste();
        (void)Editor::yank();
        redisplay->update();
        break;
      case MEvent::MButton7:
        /* mouse-track insert */
        (void)TextRegion::copy();
        (void)Editor::swapmark();
        WDGclipcopy();
        break;
      case MEvent::MButton8:
        /* x-mouse-kill */
        (void)TextRegion::kill();
        WDGclipcopy();
        break;
      case MEvent::MButton4:
        /* mouse-track-adjust */
        (void)TextRegion::copy();
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
EditWindow::adjust() {
  WDGadjust();
  return T;
}

