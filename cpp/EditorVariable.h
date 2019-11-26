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

#if !defined(__EDITOR_VARIABLE_H)
#define __EDITOR_VARIABLE_H
/*
 * EditorVariables table.
 */
enum EMVAR {
  BOOLVAL = 0x0000,          // Boolean type
  FIXVAL  = 0x0001,          // Integer type
  STRING  = 0x0002           // String type
};

class EditorVariable {
 public:
  template<typename T>
  constexpr EditorVariable(T& val, EMCHAR* varName, EMVAR varType)
  : f_val{&val},
    f_name{varName},
    f_type{varType} {
  }

  template<typename T>
  constexpr EditorVariable(T& val, EMCHAR* varName, int strSize)
  : f_val{&val},
    f_name{varName},
    f_type{STRING},
    f_size{static_cast<size_t>(strSize)} {
  }

  EMCHAR*
  name() const noexcept {
    return f_name;
  }

  int*
  intp() const noexcept {
    return reinterpret_cast<int*>(f_val);
  }

  bool*
  boolp() const noexcept {
    return reinterpret_cast<bool*>(f_val);
  }

  EMCHAR*
  string() const noexcept {
    return reinterpret_cast<EMCHAR*>(f_val);
  }

  EMVAR
  type() const noexcept {
    return f_type;
  }

  size_t
  size() const noexcept {
    return f_size;
  }

  static std::vector<EditorVariable> vartab;

 private:
  void*   f_val;                 // Flag address
  EMCHAR* f_name;                // Flag name
  EMVAR   f_type;                // Type of the variable
  size_t  f_size{0};             // Size of the variable
};
#endif /* __EDITOR_VARIABLE_H */
