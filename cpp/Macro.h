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

#if !defined(__MACRO_H)
#define __MACRO_H
#include "./emacs.h"

class LispEngine;

class Macro {
  friend class LispEngine;
 public:
  Macro() = default;

  template<typename T>
  void
  set(T keyCode, EMCHAR* cmdName, int indx) {
    _code  = static_cast<int>(keyCode);
    _name  = cmdName;
    _index = indx;
  }

  const EMCHAR*
  name() const noexcept {
    return _name;
  }

  int
  index() const noexcept {
    return _index;
  }

  int
  code() const noexcept {
    return _code;
  }

 private:
  int*    _exec{nullptr};  // Code
  EMCHAR* _name{nullptr};  // Macro name
  int     _code{0};        // Key bind
  int     _index{0};       // Index in macro key container.
};
#endif /* __MACRO_H */
