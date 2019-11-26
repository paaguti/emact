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

#if !defined(__COUNTER_H)
#define __COUNTER_H
#include "./emacs.h"

/**
 * Counter class is used to manage an integer counter that can be incremented,
 * decremented and inserted in current buffer. It is mainly used in a macro.
 */
class Counter {
 public:
  /**
   * Insert the current value of counter at dot before increment
   * it. Bound to C-X-$-$
   * @return T
   */
  static CMD
  insert();

  /**
   * Increment counter. Bound to C-X-$-'+'
   * @return T
   */
  static CMD
  incr() noexcept {
    _val += Editor::_repeat;

    return T;
  }

  /**
   * Decrement counter. Bound to C-X-$-'-'
   * @return T
   */
  static CMD
  decr() {
    _val -= Editor::_repeat;

    return T;
  }

  /**
   * Set the value of counter. Bound to C-X-$-S
   * @return T
   */
  static CMD
  set() {
    _val = Editor::_repeat;

    return T;
  }

  /**
   * Change the format of counter from default (%d). Bound to C-X-$-F
   * @return CMD
   */
  static CMD
  format();

 private:
  static int    _val;
  static EMCHAR _fmt[NPAT];
};
#endif /* __COUNTER_H */
