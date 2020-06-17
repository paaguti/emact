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

#if !defined(__SEARCH_H)
#define __SEARCH_H
#include "./emacs.h"

class Search {
 public:
  static bool next();
  static void wait(int n);
  static void substitute(int length, const EMCHAR* newstr);
  static bool matchBackward(int patc, bool printflag = true);
  static bool matchForward(int patc, bool printflag = true);
  static bool autoMatch(int c, bool printflag = true);

  /*
   * Editor commands bound to key:
   */
  static CMD forward();
  static CMD backward();
  static CMD globalReplace();
  static CMD queryReplace();
  static CMD definition();
  static CMD rightParent();
  static CMD leftParent();
  static CMD rightCurly();
  static CMD leftCurly();
  static CMD rightBracket();
  static CMD leftBracket();
  static CMD complete();
  static CMD diffWindows();
  static CMD compareWindows();

 private:
  static bool prev();
};
#endif /* __SEARCH_H */
