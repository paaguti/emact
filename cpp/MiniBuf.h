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

#if !defined(__MINIBUF_H)
#define __MINIBUF_H
#include "./emacs.h"

class MiniBuf {
 public:
  static void erase();
  static int cursor();
  static CMD yn(const EMCHAR* prompt);
  static CMD yesno(const EMCHAR* prompt);
  static CMD confirm(const EMCHAR* prompt);
  static CMD edit(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
  static CMD reply(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
  static bool allowComplete(bool flag);
  static void write(const EMCHAR* fmt, ...);
  static void error(const EMCHAR* msg);
  static EMCHAR * title(EMCHAR* buffer,EMCHAR* fname);
  static CMD change(const EMCHAR* msg, EMCHAR* opat, EMCHAR* npat, int length);
  static void play(int flag);
  static void wait();
  static void message(const EMCHAR* msg);
  static void adjust();
  static void update(const EMCHAR* prompt, EMCHAR* buf);
  static void clipCopy();
  static void clipPaste();
  static void lpPrint();
};
#endif /* __MINIBUF_H */
