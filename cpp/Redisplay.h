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
  static void garbaged() { _sgarbf = Sync::GARBAGE; }
  static void synchronized() { _sgarbf = Sync::SYNCHRONIZED; }
  static void exposed() { _sgarbf = Sync::EXPOSE; }

  static int    _currow;   // Cursor row
  static int    _curcol;   // Cursor column
  static EMCHAR _curchar;  // Char at cursor
  static Sync   _sgarbf;   // screen is garbage
  static bool   _mouse;    // mouse flags

 private:
  void refresh(EditWindow* wp);
  static void computecursor();
  static void updateline(int row, EMCHAR* vline, EMCHAR* pline);
};
#endif /* __REDISPLAY_H */
