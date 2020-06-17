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

#if !defined(__PROCESS_H)
#define __PROCESS_H
#include "./emacs.h"
/*
 * Process.cpp
 */
class Process {
 public:
  static bool syscompile(const EMCHAR* cmd, int flag);

  static CMD spawncli();
  static CMD spawn();
  static CMD makefile();
  static CMD man();
  static CMD grep();
  static CMD perl();
  static CMD sed();
  static CMD compile();
  static CMD compilecurrent();
  static CMD ccompile();
  static CMD javacompile();
  static CMD assemble();
  static CMD evalbuf();
  static CMD getcommand();
  static CMD changedir();
};
#endif /* __PROCESS_H */
