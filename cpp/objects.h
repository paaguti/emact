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

#if !defined(__OBJECTS_H)
#define __OBJECTS_H

#include <vector>
#include <list>
#include <array>
#include <algorithm>
#include <string>
#include <limits>
#include <memory>

#include "./Point.h"

/*
 * This  file  is  the  general header file for all components of the
 * EMACS   display  editor.  It  contains  C++ classes definitions  used  by
 * everyone.
 */

enum class CMD {
  /*
   * False, no, bad, etc.
   */
  CST_NIL   = 0x00,
  /*
   * True, yes, good, etc.
   */
  CST_T     = 0x10,
  /*
   * Death, ^G, abort, etc.
   */
  CST_ABORT = 0x30
};

static constexpr auto NIL(CMD::CST_NIL);
static constexpr auto T(CMD::CST_T);
static constexpr auto ABORT(CMD::CST_ABORT);

/*
 * Emacs pseudo-types.
 */

enum class ENCODING {
  EMASCII = 0,
  EMUTF8  = 1,
  EMUTF16 = 2
};

/**
 * Supported edit modes
 */
enum class EDITMODE {
  /** Fundamental mode */
  FUNDAMENTAL,
  /** Electric C mode */
  CMODE,
  /** Lisp mode */
  LISPMODE,
  /** Prolog mode */
  PROLOGMODE,
  /** Assembler mode */
  ASMODE,
  /** Directory mode */
  DIRED,
  /** Pascal mode */
  PASCALMODE,
  /** Electric C++ mode */
  CPPMODE,
  /** Java mode */
  JAVAMODE,
  /** Fortran mode */
  FORTRANMODE,
  /** Buffer mode */
  BufferMODE,
  /** SGML mode */
  SGMLMODE,
  /** Perl mode */
  PERLMODE,
  /** C# mode */
  CSHARPMODE,
  /** Python mode */
  PYTHONMODE,
  /** shell mode */
  SHELLMODE
};

class Buffer;
class Completion;
class EditWindow;
class EditorCommand;
class Line;
class Redisplay;
class Terminal;
class Widget;
#endif /* __OBJECTS_H */
