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

#if !defined(__EDITOR_COMMAND_H)
#define __EDITOR_COMMAND_H
#include "./emacs.h"
/*
 * Commands table.
 */
class EditorCommand final {
 private:
  using CB = CMD (*)();

 public:
  constexpr EditorCommand(int keyCode, CB fp, const EMCHAR* keyName)
    : k_code{keyCode},
      k_fp{fp},
      k_name{keyName} {
  }

  bool
  operator==(const EditorCommand& rhs) const noexcept {
    return k_code == rhs.k_code;
  }

  const EMCHAR*
  name() const noexcept {
    return k_name;
  }

  int
  code() const noexcept {
    return k_code;
  }

  void
  unset() noexcept {
    k_code = UNBOUND;
  }

  CMD
  operator()() const noexcept {
    return k_fp();
  }

  CB
  callback() const noexcept {
    return k_fp;
  }

 private:
  int           k_code;          // Key code
  CB            k_fp;            // Routine to handle it
  const EMCHAR* k_name;          // Name of the command
};
#endif /* __EDITOR_COMMAND_H */
