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

#if !defined(__INDENT_H)
#define __INDENT_H
class Line;
class Indent {
 public:
  static bool unindent(int c, bool f = true);
  static int lastCPos(Line* line);
  static int lastLispPos(Line* line);

  static CMD indentLine();
  static CMD tabIndent();
  static CMD newlineIndent();
  static CMD backToIndent();
  static CMD beginLispExpr();
  static CMD endLispExpr();

  static void save(Line* line, int pos);
};
#endif /* __INDENT_H */
