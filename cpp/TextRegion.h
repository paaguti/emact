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

#if !defined(__TEXT_REGION_H)
#define __TEXT_REGION_H
#include "./emacs.h"

class Line;

/**
 * The starting position of a region, and the size of the region
 * in  characters,  is kept in a region structure.   Used by the
 * region commands.
 */
class TextRegion {
 public:
  /*
   * Editor commands bound to key:
   */
  static CMD kill();
  static CMD copy();
  static CMD lower();
  static CMD upper();
  static CMD write();
  static CMD fill();
  static CMD indent();
  static CMD shiftright();
  static CMD shiftleft();

private:
  TextRegion();
  Line* _linep{nullptr};  // Origin Line address
  int   _offset{0};       // Origin Line offset
  int   _size{0};         // Length in characters
  int   _lines{0};        // Number of lines
  bool  _empty{true};     // empty or not set.

  bool empty() { return _empty; }
};
#endif /* __TEXT_REGION_H */
