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

#if !defined(__POINT_H)
#define __POINT_H
class Line;
/**
 * A Point is a position of a character in an Line.
 */
class Point final {
 public:
  explicit Point(Line* pointLine = nullptr, int pointPos = 0)
    : _line{pointLine},
      _pos{pointPos} {
  }

  ~Point() = default;
  Point(const Point& p) = default;
  Point& operator=(const Point& p) = default;

  friend bool
  operator==(const Point& lhs, const Point& rhs) {
    return lhs._line == rhs._line && lhs._pos == rhs._pos;
  }

  friend bool
  operator!=(const Point& lhs, const Point& rhs) {
    return !(lhs == rhs);
  }

  void
  set(Line* newLine, int newPos) {
    _line = newLine;
    _pos  = newPos;
  }

  void
  setLine(Line* newLine) {
    _line = newLine;
  }

  void
  setPos(int newPos) {
    _pos  = newPos;
  }

  Line*
  line() const noexcept {
    return _line;
  }

  int
  pos() const noexcept {
    return _pos;
  }

 private:
  Line* _line{nullptr};
  int   _pos{0};
};
#endif /* __POINT_H */
