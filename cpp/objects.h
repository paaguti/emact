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
class Kbdm;
class Line;
class Redisplay;
class Terminal;
class Widget;

/*
 * Variables table.
 */

enum EMVAR {
  BOOLVAL = 0x0000,          // Boolean type
  FIXVAL  = 0x0001,          // Integer type
  STRING  = 0x0002           // String type
};

class Variable {
 public:
  template<typename T>
  constexpr Variable(T& val, EMCHAR* varName, EMVAR varType)
  : f_val{&val},
    f_name{varName},
    f_type{varType} {
  }

  template<typename T>
  constexpr Variable(T& val, EMCHAR* varName, int strSize)
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

  static std::vector<Variable> vartab;

 private:
  void*   f_val;                 // Flag address
  EMCHAR* f_name;                // Flag name
  EMVAR   f_type;                // Type of the variable
  size_t  f_size{0};             // Size of the variable
};

class LispEngine;
class Macro {
  friend class LispEngine;
 public:
  Macro() = default;

  template<typename T>
  void
  set(T keyCode, EMCHAR* cmdName, int indx) {
    m_code  = static_cast<int>(keyCode);
    m_name  = cmdName;
    m_index = indx;
  }

  const EMCHAR*
  name() const noexcept {
    return m_name;
  }

  int
  index() const noexcept {
    return m_index;
  }

  int
  code() const noexcept {
    return m_code;
  }

 private:
  int*    m_exec{nullptr};  // Code
  EMCHAR* m_name{nullptr};  // Macro name
  int     m_code{0};        // Key bind
  int     m_index{0};       // Index in macro key container.
};

/**
 * Class for mouse driver (if any).
 */
class MEvent {
 public:
  static constexpr auto SHIFTBUTTON = 0x10;
  static constexpr auto CTRLBUTTON  = 0x20;

  static constexpr auto MButton1    = 0x01;
  static constexpr auto MButton2    = 0x02;
  static constexpr auto MButton3    = 0x03;

  static constexpr auto MButton4    = (MButton1 | SHIFTBUTTON);
  static constexpr auto MButton5    = (MButton2 | SHIFTBUTTON);
  static constexpr auto MButton6    = (MButton3 | SHIFTBUTTON);

  static constexpr auto MButton7    = (MButton1 | CTRLBUTTON);
  static constexpr auto MButton8    = (MButton2 | CTRLBUTTON);
  static constexpr auto MButton9    = (MButton3 | CTRLBUTTON);

  unsigned int button;
  int x;
  int y;
};

class Kbdm {
 public:
  class BufferFullException {};
  Kbdm() {
    reset();
  }

  /*
   * Playing methods
   */
  void
  startPlaying() noexcept {
    _kbdmop = &_kbdm[0];
  }

  void
  stopPlaying() noexcept {
    _kbdmop = nullptr;
  }

  bool
  isPlaying() noexcept {
    return _kbdmop != nullptr;
  }

  int
  play() noexcept {
    return *_kbdmop++;
  }

  /*
   * Recording methods
   */
  void
  startRecording() noexcept {
    _kbdmip = &_kbdm[0];
  }

  void
  stopRecording() noexcept {
    _kbdmip = nullptr;
  }

  bool
  isRecording() const noexcept {
    return _kbdmip != nullptr;
  }

  void
  record(int c) {
    if (_kbdmip >= &_kbdm[NKBDM]) {
      reset();
      throw BufferFullException{};
    } else {
      *_kbdmip++ = c;
    }
  }

  void
  reset() noexcept {
    _kbdmip = nullptr;
    _kbdm[0] = -1;
  }

  bool
  exist() const noexcept {
    return _kbdm[0] != -1;
  }

 private:
  static constexpr size_t NKBDM = 512;  // # of strokes, keyboard macro
  int  _kbdm[NKBDM];                    // Holds keyboard macro data
  int* _kbdmip{nullptr};                // Input pointer for above
  int* _kbdmop{nullptr};                // Output pointer for above
};

#include "./EditorCommand.h"

class Editor {
 public:
  template<typename T>
  Editor(int argc, T* argv[])
    : Editor{argc, args(argc, argv), true} {
  }

  ~Editor() {
    for (int i = 0; i < _argc; ++i) {
      delete[] _argv[i];
    }
  }

  static bool separatorp(int c);
  static bool charp(int c);

  void
  engine();

  /*
   * basic.cpp
   */
  static CMD gotobol();
  static CMD backchar();
  static CMD gotoeol();
  static CMD forwchar();
  static CMD gotobob();
  static CMD gotoeob();
  static CMD gotoline();
  static CMD forwline();
  static CMD backline();
  static CMD forwpage();
  static CMD forwother();
  static CMD backpage();
  static CMD setmark();
  static CMD markwholebuffer();
  static CMD swapmark();

  /*
   * file.cpp
   */
  static CMD ansiToOem();
  static CMD oemToAnsi();
  static CMD macToAnsi();
  static CMD macToOem();
  static CMD toggleRead();
  static CMD fileRead();
  static CMD fileAlternate();
  static CMD fileInsert();
  static CMD fileWrite();
  static CMD fileSave();
  static CMD findFile();
  static CMD revertBuffer();
  static CMD saveSomeBuffers();
  static CMD unlinkFile();
  static CMD printBuffer();

  /*
   * random.cpp
   */
  static CMD newline();
  static CMD tab();
  static CMD showcpos();
  static CMD twiddle();
  static CMD quotechar();
  static CMD tabexpand();
  static CMD openline();
  static CMD endline();
  static CMD delblank();
  static CMD forwdel();
  static CMD backdel();
  static CMD killtext();
  static CMD yank();
  static CMD appendNextKill();
  static CMD setFillColumn();
  static CMD setFillPrefix();
  static CMD backParagraph();
  static CMD forwParagraph();
  static CMD markParagraph();
  static CMD fillParagraph();
  static CMD splitLineToFill();
  static CMD justOneSpace();
  static CMD justifyCurLine();
  static CMD setJustifyLeft();
  static CMD setJustifyFull();
  static CMD justifyComment();
  static CMD undo();
  static CMD enterDebug();

  /*
   *  emacs.cpp
   */

  static CMD emacsversion();
  static CMD killemacs();
  static CMD exitemacs();
  static CMD ctlxlp();
  static CMD ctlxrp();
  static CMD ctlxe();
  static CMD ctrlg();
  static CMD insertunicode();
  static CMD binaryfile();
  static CMD utf8encoding();
  static CMD utf16encoding();
  static CMD systemencoding();
  static CMD switchfund();
  static CMD switchcc();
  static CMD switchcpp();
  static CMD switchsgml();
  static CMD switchjava();
  static CMD switchlisp();
  static CMD switchshell();
  static CMD switchperl();
  static CMD switchprolog();
  static CMD switchpython();
  static CMD switchas();
  static CMD switchfortran();
  static CMD redrawscreen();

  /*
   * This is the general command execution routine. It handles the
   * fake binding of all the keys to "self-insert". It also clears
   * out  the  "_thisflag"  word,  and arranges to move it  to  the
   * "_lastflag",  so that the next command can look at it.
   * @param [in] c code to execute
   * @param [in] n repeat count
   * @return the status of command.
   */
  static CMD
  execute(int c, int n = 1);

  static const EMCHAR*
  getName() {
    return _name;
  }

 public:
  static bool
  isEqual(const EMCHAR* s1,
          const EMCHAR* s2,
          int len,
          bool insensitive = false) {
    if (!insensitive) {
      return emstrncmp(s1, s2, len) == 0;
    }

    while (*s1 != 0 && *s2 != 0 && len > 0) {
      int c1{*s1++};
      int c2{*s2++};
      --len;
      if (c1 != c2) {
        if (c1 <= 0xFF && std::isalpha(c1)) {
          c1 = std::tolower(c1);
        }
        if (c2 <= 0xFF && std::isalpha(c2)) {
          c2 = std::tolower(c2);
        }
      }
      if (c1 != c2) {
        return false;
      }
    }
    return len == 0;
  }

  static EMCHAR*
  searchBuffer() noexcept {
    return &_search[0];
  }

  static void
  setSearchBuffer(const EMCHAR* val) noexcept {
    (void)emstrcpy(&_search[0], val);
  }

  /*
   * Read in a key. Do the standard keyboard preprocessing.
   */
  static int
  getkey();

  static std::vector<Macro>&
  getMacros() {
    return _macros;
  }

  /**
   * Display an internal error.
   * @param [in] file file where error occurs.
   * @param [in] line line where error occurs.
   * @param [in] msg message of this error.
   */
  static void
  error(const char* file, int line, const EMCHAR* msg);

  static std::vector<EditorCommand> _keytab;
  /* User macros table */
  static std::vector<Macro> _macros;
  static int _thisflag;  // Flags, this command
  static int _lastflag;  // Flags, last command
  static int _repeat;    // Repeat count
  static int _curgoal;   // Goal for C-P, C-N
  static Point _found;   // Position of last search

 private:
  Editor(int argc, EMCHAR* argv[], bool);

  /**
   * Convert array of T* to array of EMCHAR*
   * tparam T character type
   */
  template<typename T>
  EMCHAR**
  args(int argc, T* argv[]) {
    auto p(new EMCHAR*[argc + 1]);
    auto cvt = [](const T* str) -> EMCHAR* {
                 size_t len = 0;
                 while (str[len] != 0) {
                   ++len;
                 }
                 auto res = new EMCHAR[len + 1];

                 for (int i = 0; i < static_cast<int>(len); ++i) {
                   res[i] = (EMCHAR)str[i];
                 }
                 res[len] = '\000';

                 return res;
               };
    for (int i = 0; i < argc; ++i) {
      p[i] = cvt(argv[i]);
    }

    p[argc] = nullptr;
    return p;
  }

  int _argc{0};
  std::unique_ptr<EMCHAR*[]> _argv{nullptr};

  static const EMCHAR* _name;
  static EMCHAR _search[NPAT];
};

/**
 * Hi level interface to pseudo-lisp extension language.
 */
class MLisp {
 public:
  static bool customize();
  static CMD eval(int expr);
  static CMD evalBuffer();
  static CMD evalExpression();

  /*
   * Editor commands bound to key:
   */
  static CMD readFile();
};

class KillBuf {
 public:
  static void clear();
  static bool insert(int c);
  static int  remove(int n);
  static const std::pair<const EMCHAR*, size_t> get();
};
#endif /* __OBJECTS_H */
