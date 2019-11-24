/*
 * static auto rcsid("$Id: ./emacs.h,v 1.66 2018/09/09 07:25:14 jullien Exp $");
 */

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

#if !defined(__REDISPLAY_H)
#define __REDISPLAY_H

#include "./emacs.h"

class EditWindow;

/**
 * Class that handles display.
 */
class Redisplay {
 public:
  enum class Mode {
    DELTA   = 0,
    MINIBUF = 0x0200,  // Minibuffer
    REFRESH = 0x0400   // Full refresh
  };
  enum class Sync {
    GARBAGE      = 0,
    SYNCHRONIZED = 1,
    EXPOSE       = 2
  };
  Redisplay();
  ~Redisplay();
  bool running() const noexcept;
  void tidy() const noexcept;
  const EMCHAR* text(int y) const noexcept;
  void update(Mode mode = Mode::DELTA);
  void statputc(int n, int c) const noexcept;
  void modeline(const EditWindow* wp) noexcept;
  void garbaged() { _sgarbf = Sync::GARBAGE; }
  bool isGarbaged() const noexcept { return _sgarbf == Sync::GARBAGE; }
  void synchronized() { _sgarbf = Sync::SYNCHRONIZED; }
  void exposed() { _sgarbf = Sync::EXPOSE; }

  int    _currow{0};                        // Cursor row
  int    _curcol{0};                        // Cursor column
  EMCHAR _curchar{0};                       // Char at cursor
  bool   _mouse{false};                     // mouse flags

 private:
  Sync _sgarbf{Redisplay::Sync::GARBAGE}; // screen is garbage

  void refresh(EditWindow* wp);
  void computecursor();
  void updateline(int row, EMCHAR* vline, EMCHAR* pline);
};
#endif /* __REDISPLAY_H */
