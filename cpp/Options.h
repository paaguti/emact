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

#if !defined(__OPTIONS_H)
#define __OPTIONS_H
/**
 * This class manages EmACT options.
 */
class Options {
 public:
  /*
   * options.cpp
   */

  static CMD describekey();
  static CMD help();
  static CMD setvar();
  static CMD uncompile();
  static CMD findtag();
  static CMD tagsloopcont();

  static int completeintag(int tagnext, const EMCHAR* tagname, EMCHAR* tagcomp);

  static int tagfound;
};
#endif /* __OPTIONS_H */
