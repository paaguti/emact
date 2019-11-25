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

#if !defined(__COMPLETION_H)
#define __COMPLETION_H
#include "./emacs.h"
/**
 * Handle completion.
 */
class Completion {
 private:
  using Callback = EMCHAR* (*)(const EMCHAR* prompt, EMCHAR* buf);

 public:
  enum class Status {
    COMPLETE_ONE,
    COMPLETE_AGAIN,
    COMPLETE_ABORT,
    COMPLETE_FAIL
  };

  const EMCHAR*
  operator()(const EMCHAR* prompt, EMCHAR* buf) {
    return _fn(prompt, buf);
  }

  Completion&
  operator=(Callback cb) {
    _fn = cb;
    return *this;
  }

  bool
  operator==(Callback cb) {
    return _fn == cb;
  }

  bool
  operator!=(Callback cb) {
    return _fn != cb;
  }

  Status
  status() const noexcept {
    return _status;
  }

  void
  setStatus(Status newStatus) noexcept {
    _status = newStatus;
  }

  static CMD     diredCommand(int c);
  static EMCHAR* fileMatch(const EMCHAR* prompt, EMCHAR* file);
  static EMCHAR* fileAccept(const EMCHAR* prompt, EMCHAR* file);
  static bool    diredBuffer(const EMCHAR* fmatch);

  static CMD dired();

 private:
  Status _status;
  Callback _fn;
};
#endif /* __COMPLETION_H */
