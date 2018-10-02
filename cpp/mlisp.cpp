#if !defined(lint)
static auto rcsid("$Id: mlisp.cpp,v 1.30 2018/09/09 07:21:10 jullien Exp $");
#endif

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

/*
 * This  file implements the macro definition of Emacs.  A macro
 * file  is  a  text  file with macro definition in Lisp syntax.
 * The body is defined by a set of command name.
 */

#if defined(_MLISP)

#include "./emacs.h"
#include <array>

class MLisp {
 private:
  enum class SpecialForm {
    NOFUNCTION      = 0,
    FUNCTION        = -1,
    READFIRST       = -2,
    READSECOND      = -3,
    READTHIRD       = -4,
    LOADMACRO       = -5,
    WRITEMINIBUF    = -6,
    FINDNEXT        = -7,
    REPEAT          = -8,
    SETQ            = -9,
    INSERTNAME      = -10,
    INSERTBASENAME  = -11,
    UPDATE          = -12,
    INSERTCOMMAND   = -13,
    DEFUN           = -14,
    BINDTOKEY       = -15,
    INSERTSTRING    = -16,
    INPACKAGE       = -17,
    /*
     * last two entries
     */
    NOTFOUND        = -18,
    FREE            = -19
  };

 public:
  static bool loadmacro(const EMCHAR* macfile);
  static bool eval(int expr, size_t depth);

 private:
  static bool    getfun();
  static void    fillcommand(SpecialForm key);
  static int     nextchar();
  static const EMCHAR* getword();
  static int     decrypt(const EMCHAR* string);
  static EMCHAR* duplicate(const EMCHAR* s);
  static int     getcode(const EMCHAR* s, int* indx);
  static void    readerror(const EMCHAR* msg, const EMCHAR* arg);
  static void    fillmacro(int key);
  template<typename T>
  static void    fillmacro(T arg) {
    fillmacro(static_cast<int>(arg));
  }
  static int     lispgetc(FILE* fd);

 public:
  static constexpr int MAXKBD{1024};
  static std::array<int, MAXKBD> _macrotab;
  static FILE* _mfile;
  static EMCHAR _library[NPAT];  // Library Path

 private:
  static constexpr size_t ARGLEN{64};
  static constexpr size_t MAX_DEPTH{8};

  static constexpr const EMCHAR* ARG1 = ECSTR("arg1");
  static constexpr const EMCHAR* ARG2 = ECSTR("arg2");
  static constexpr const EMCHAR* ARG3 = ECSTR("arg3");
  static EMCHAR _arg1[ARGLEN];
  static EMCHAR _arg2[ARGLEN];
  static EMCHAR _arg3[ARGLEN];
  static bool _insideComment;
  static bool _insideString;
  static int _msize;
};

/*
 * Define static variables
 */
int MLisp::_msize;
FILE* MLisp::_mfile{nullptr};
std::array<int, MLisp::MAXKBD> MLisp::_macrotab;
bool MLisp::_insideComment{false};
bool MLisp::_insideString{false};
EMCHAR MLisp::_library[NPAT];
EMCHAR MLisp::_arg1[MLisp::ARGLEN];
EMCHAR MLisp::_arg2[MLisp::ARGLEN];
EMCHAR MLisp::_arg3[MLisp::ARGLEN];

/**
 * read next valid lisp character skipping comments.
 */
int
MLisp::lispgetc(FILE* fd) {
  for (;;) {
    auto c = std::fgetc(fd);
    switch (c) {
    case EOF:
      return c;
    case '\n':
    case '\r':
      _insideComment = false;
      return c;
    case ';':
      if (!_insideString) {
        _insideComment = true;
      } else {
        return c;
      }
      break;
    case '\"':
      if (!_insideComment) {
        _insideString = !_insideString;
        return c;
      }
      break;
    default:
      if (!_insideComment) {
        return c;
      }
    }
  }
}

/*
 * Read  a new function from the current input file and return T
 * if the definition has been read without error.
 */

bool
MLisp::getfun() {
  EMCHAR* name = nullptr;
  int     lpar  = 0;
  auto    code = SpecialForm::NOTFOUND;
  int     c;
  int     i;

  if (Editor::_macros.size() == NMAX) {
    readerror(ECSTR("Macro workspace is full."), nullptr);
  }

  _msize = 0;

  while ((c = lispgetc(_mfile)) != EOF && c != '(') {
    continue;
  }

  if (c == EOF) {
    return false;
  }

  auto word = getword();
  auto pmain = false;

  if (!emstrcmp(word, ECSTR("defun"))) {
    code = SpecialForm::FUNCTION;
    name = duplicate(getword());
    while (nextchar() != '(') {
      continue;
    }
    while (nextchar() != ')') {
      continue;
    }
  } else if (!emstrcmp(word, ECSTR("in-package"))) {
    while (nextchar() != ')') {
      continue;
    }
    return true;
  } else if (!emstrcmp(word, ECSTR("main")) ||
             !emstrcmp(word, ECSTR("progn"))) {
    pmain = true;
  } else {
    readerror(ECSTR("Error in macro description file: "), word);
  }

  size_t indx{0};
  if (code != SpecialForm::FUNCTION) {
    for (auto& macro : Editor::getMacros()) {
      if (macro.code() == static_cast<int>(SpecialForm::FREE)) {
        /*
         * Found a free slot
         */
        break;
      } else if (macro.code() == static_cast<int>(code)) {
        /*
         * Found an existing slot (redefinition)
         */
        delete[] macro.name();
        delete[] macro.m_exec;
        break;
      }
      ++indx;
    }
  } else {
    for (auto& macro : Editor::getMacros()) {
      if (macro.code() == static_cast<int>(SpecialForm::FREE)) {
        /*
         * Found a free slot
         */
        break;
      } else if (name && !emstrcmp(macro.name(), name)) {
        /*
         * Delete previous definition
         */
        delete[] macro.m_exec;
        break;
      }
      ++indx;
    }
  }

  if (indx == Editor::_macros.size()) {
    /*
     * It's a new macro.
     */
    Editor::_macros.emplace_back(MACTAB());
  }

  auto& macro(Editor::_macros[indx]);

  macro.set(code, name, static_cast<int>(indx));

  i = 0;

  while ((c = nextchar()) != EOF) {
    if (c == ')') {
      if (lpar--) {
        continue;
      } else {
        break;
      }
    }
    if (c == '(') {
      word = getword();
      if (!emstrcmp(word, ECSTR("insert-string"))) {
        fillcommand(SpecialForm::INSERTSTRING);
      } else if (!emstrcmp(word, ECSTR("load-macro"))) {
        fillcommand(SpecialForm::LOADMACRO);
      } else if (!emstrcmp(word, ECSTR("write-minibuffer"))) {
        fillcommand(SpecialForm::WRITEMINIBUF);
      } else if (!emstrcmp(word, ECSTR("find-next-pattern"))) {
        fillmacro(SpecialForm::FINDNEXT);
      } else if (!emstrcmp(word, ECSTR("read-first-argument"))) {
        fillcommand(SpecialForm::READFIRST);
      } else if (!emstrcmp(word, ECSTR("read-second-argument"))) {
        fillcommand(SpecialForm::READSECOND);
      } else if (!emstrcmp(word, ECSTR("read-third-argument"))) {
        fillcommand(SpecialForm::READTHIRD);
      } else if (!emstrcmp(word, ECSTR("setq"))) {
        fillcommand(SpecialForm::SETQ);
      } else if (!emstrcmp(word, ECSTR("bind-to-key"))) {
        fillcommand(SpecialForm::BINDTOKEY);
      } else if (!emstrcmp(word, ECSTR("insert-buffer-name"))) {
        fillmacro(SpecialForm::INSERTNAME);
      } else if (!emstrcmp(word, ECSTR("insert-base-name"))) {
        fillmacro(SpecialForm::INSERTBASENAME);
      } else if (!emstrcmp(word, ECSTR("in-package"))) {
        fillmacro(SpecialForm::INPACKAGE);
      } else if (!emstrcmp(word, ECSTR("update-screen"))) {
        fillmacro(SpecialForm::UPDATE);
      } else if (!emstrcmp(word, ECSTR("insert-system-command"))) {
        fillcommand(SpecialForm::INSERTCOMMAND);
      } else if (!emstrcmp(word, ECSTR("repeat"))) {
        fillmacro(SpecialForm::REPEAT);
        word = getword();
        while (*word) {
          fillmacro(*word++ - '0');
        }
        fillmacro(-1);
        lpar++;
        continue;
      } else if ((c = getcode(word, &i))
                 == static_cast<int>(SpecialForm::NOTFOUND)) {
        readerror(ECSTR("Invalid macro name: "), word);
      } else if (c == static_cast<int>(SpecialForm::FUNCTION)) {
        fillmacro(SpecialForm::FUNCTION);
        fillmacro(i);
      } else {
        fillmacro(c);
      }

      while (nextchar() != ')') {
        continue;
      }
    }
    if (c == ';') {
      while (nextchar() != '\n') {
        continue;
      }
    }
  }

  {
    auto buf = new int[_msize + 1];
    for (c = 0; c < _msize; ++c) {
      buf[c] = _macrotab[c];
    }
    buf[c] = 0;

    macro.m_exec = buf;
  }

  if (pmain) {
    (void)mlinternaleval(static_cast<int>(indx));
    macro.m_code = static_cast<int>(SpecialForm::FREE);
    delete[] macro.m_exec;
  }

  return true;
}

/*
 * Fill  the  current  macro definition with 'key' and check for
 * macro overflow.
 */

void
MLisp::fillmacro(int key) {
  if (_msize < MAXKBD) {
    _macrotab[_msize++] = key;
  } else {
    readerror(ECSTR("Macro bigger than 1024 keys."), nullptr);
  }
}

/*
 * Fill the current macro with a specific command.
 */

void
MLisp::fillcommand(SpecialForm key) {
  auto word = getword();
  int  i;

  fillmacro(key);

  switch (key) {
  case SpecialForm::SETQ:
    {
      i = 0;
      for (const auto& var : VARTAB::vartab) {
        if (var.name() && !emstrcmp(var.name(), word)) {
          break;
        }
        ++i;
      }

      if (i >= (int)VARTAB::vartab.size()) {
        readerror(ECSTR("Unknown variable. "), word);
      }

      auto& var(VARTAB::vartab[i]);

      fillmacro(i);
      word = getword();
      if (var.type() == BOOLVAL) {
        if (!emstrcmp(word, ECSTR("nil"))) {
          fillmacro(0);
        } else {
          fillmacro(1);
        }
        fillmacro(0);
        return;
      } else if (var.type() == FIXVAL) {
        fillmacro(std::strtoll((char *)word, nullptr, 0) & 0xff);
        fillmacro(0);
        return;
      }
    }
    break;
  case SpecialForm::BINDTOKEY:
    i = -1;
    for (const auto& macro : Editor::getMacros()) {
      if (macro.name() && !emstrcmp(macro.name(), word)) {
        i = macro.index();
        break;
      }
    }

    if (i == -1) {
      readerror(ECSTR("Unknown function. "), word);
    }

    fillmacro(i);
    word = getword();
    while (*word) {
      fillmacro(*word++);
    }
    fillmacro(0);
    return;
  case SpecialForm::INPACKAGE:
    while (*word++) {
      continue;
    }
    return;
  default:
    break;
  }

  if (word == ARG1) {
    fillmacro(0xff);
    fillmacro(1);
  } else if (word == ARG2) {
    fillmacro(0xff);
    fillmacro(2);
  } else if (word == ARG3) {
    fillmacro(0xff);
    fillmacro(3);
  } else {
    do {
      fillmacro(*word & 0xff);
    } while (*word++);
  }
}

/*
 * Read and return the next char available from macro file.
 */

int
MLisp::nextchar() {
  int c;

  if ((c = lispgetc(_mfile)) == EOF) {
    readerror(ECSTR("Error EOF encounter in macro file."), nullptr);
  }

  return c;
}

/*
 * Read and return the next word avalaible from the macro file.
 */

const EMCHAR*
MLisp::getword() {
  static EMCHAR workbuf[NPAT];
  int    c;

  while ((c = nextchar()) != 0 && (separatorp(c) || c == '\'') && c != '"') {
    continue;
  }

  auto word = &workbuf[0];

  if (c == '"') {
    while ((c = nextchar()) != '"') {
      if (c == '\\') {
        c = (EMCHAR)nextchar();
        if (c == '\\') {
          c = (EMCHAR)nextchar();
        }
        switch (c) {
        case 'n': *word++ = '\n'; break;
        case 't': *word++ = '\t'; break;
        default : *word++ = (EMCHAR)c;
        }
      } else {
        *word++ = (EMCHAR)c;
      }
    }
    *word = '\0';
    return workbuf;
  }

  (void)ungetc(c, _mfile);

  while (((c = nextchar()) != 0) && charp(c)) {
    *word++ = (EMCHAR)c;
  }

  *word = '\0';

  (void)ungetc(c, _mfile);

  if (!emstrcmp(ARG1, workbuf)) {
    return ARG1;
  }
  if (!emstrcmp(ARG2, workbuf)) {
    return ARG2;
  }
  if (!emstrcmp(ARG3, workbuf)) {
    return ARG3;
  } else {
    return workbuf;
  }
}

/*
 * Given a command string, return the associated code.
 */

int
MLisp::decrypt(const EMCHAR* strg) {
  int     meta  = 0;
  int     ctrl  = 0;
  int     ctrlx = 0;
  int     ctrlz = 0;
  int     spcl  = 0;

  switch (*strg++) {
  case 'M':
    if (*strg++ == '-') {
      meta = META;
    } else {
      readerror(ECSTR("Bad command for a macro: "), strg - 2);
    }
    break;
  case 'C':
    if (strg[0] == '-') {
      if (strg[1] == 'X') {
        ctrlx = CTLX;
        strg += 2;
      } else if (strg[1] == 'C') {
        ctrlz = CTLC;
        strg += 2;
      }
    } else {
      ctrl = Ctrl;
      strg++;
    }
    break;
  case 'F':
    if (*strg++ == '-') {
      spcl = SPCL;
    }
  }

  if (strg[0] == 'C' && strg[ 1] == '-') {
    strg += 2;
    ctrl = Ctrl;
  }

  return *strg | meta | ctrlx | ctrlz | spcl | ctrl;
}

/*
 * Return a copy of a pre-allocated string.
 */

EMCHAR*
MLisp::duplicate(const EMCHAR* s) {
  return emstrcpy(new EMCHAR[emstrlen(s) + 1], s);
}

/*
 * Return the code of a command name.
 */

int
MLisp::getcode(const EMCHAR* s, int* indx) {
  /*
   * Look in macro table.
   */

  for (auto& macro : Editor::getMacros()) {
    if (macro.name() && !emstrcmp(s, macro.name())) {
      *indx = macro.index();
      return macro.code();
    }
  }

  /*
   * Look in key table.
   */

  for (auto& ktp : Editor::_keytab) {
    if (!emstrcmp(s, ktp.name())) {
      return ktp.code();
    }
  }

  return static_cast<int>(SpecialForm::NOTFOUND);
}

/*
 * Evaluate a macro definition.
 */

bool
MLisp::eval(int expr, size_t depth) {
  extern Point found;

  EMCHAR  string[NPAT];
  int     c;
  int     i;
  int     code;

  if (depth > MAX_DEPTH) {
    WDGmessage(ECSTR("Too many recursion levels when calling macro."));
    return false;
  }

  auto bufcmd = Editor::_macros[expr].m_exec;
  auto s = T;
  auto n = 1;

  while ((c = *bufcmd) != 0 && s == T) {
    if (n <= 0) {
      n = 1;
    }

    SpecialForm key = static_cast<SpecialForm>(c);

    switch (key) {
    case SpecialForm::READFIRST:
    case SpecialForm::READSECOND:
    case SpecialForm::READTHIRD:
      bufcmd++;
      for (i = 0; *bufcmd; ++i) {
        string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
      }
      string[i] = '\0';
      switch (key) {
      case SpecialForm::READFIRST:
        if (mlreply(string, _arg1, ARGLEN) != T) {
          return false;
        }
        break;
      case SpecialForm::READSECOND:
        if (mlreply(string, _arg2, ARGLEN) != T) {
          return false;
        }
        break;
      case SpecialForm::READTHIRD:
        if (mlreply(string, _arg3, ARGLEN) != T) {
          return false;
        }
        break;
      default:
        break;
      }
      break;
    case SpecialForm::INSERTSTRING:
      bufcmd++;
      if (*bufcmd == 0xff) {
        const EMCHAR* arg = nullptr;

        switch (*++bufcmd) {
        case 1:
          arg = _arg1;
          break;
        case 2:
          arg = _arg2;
          break;
        case 3:
          arg = _arg3;
          break;
        }

        while (n-- > 0 && s == T) {
          for (auto str = arg; (*str && s == T); ++str) {
            switch (*str) {
            case '\t':
              s = tabexpand();
              break;
            case '\n':
              s = endline();
              break;
            default:
              s = EDLINE::linsert(*str) ? T : NIL;
            }
          }
        }
      } else {
        auto cbuf = bufcmd;

        while (n-- > 0 && s == T) {
          bufcmd = cbuf;
          while (*bufcmd && s == T) {
            switch (*bufcmd) {
            case '\t':
              s = tabexpand();
              break;
            case '\n':
              s = endline();
              break;
            case '\\':
              if (*(bufcmd + 1) != 'n') {
                s = EDLINE::linsert(*bufcmd) ? T : NIL;
              } else {
                ++bufcmd;
                s = endline();
              }
              break;
            default:
              s = EDLINE::linsert(*bufcmd) ? T : NIL;
            }
            ++bufcmd;
          }
        }
      }
      break;
    case SpecialForm::LOADMACRO:
      {
        auto file = emstrcpy(string, _library);
        bufcmd++;
        for (i = emstrlen(file); *bufcmd; ++i) {
          file[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
        }
        file[i] = '\0';
        WDGwrite(ECSTR("%s"), file);
        s = loadmacro(file) ? T : NIL;
      }
      break;
    case SpecialForm::WRITEMINIBUF:
      bufcmd++;
      for (i = 0; *bufcmd; ++i) {
        string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
      }
      string[i] = '\0';
      WDGwrite(ECSTR("%s"), string);
      break;
    case SpecialForm::FINDNEXT:
      s = T;
      while (n--) {
        if (ffindstring()) {
          curwp->setDot(found);
          curwp->setFlags(WINSCR::WFMOVE);
        } else {
          s = NIL;
          break;
        }
      }
      break;
    case SpecialForm::REPEAT:
      n = 0;
      bufcmd++;
      do {
        n = n * 10 + *bufcmd++;
      } while (*bufcmd != -1);
      ++bufcmd;
      continue;
    case SpecialForm::SETQ:
      {
        bufcmd++;
        c = *bufcmd++;
        auto& var(VARTAB::vartab[c]);

        switch (var.type()) {
        case BOOLVAL:
          {
            auto p = var.boolp();
            *p = (*bufcmd++ ? true : false);
          }
          break;
        case FIXVAL:
          {
            auto p = var.intp();
            *p = *bufcmd++;
          }
          break;
        default:
          {
            auto p = var.string();
            while (*bufcmd) {
              *p++ = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
            }
            *p = '\0';
          }
        }

        s = T;
      }
      break;
    case SpecialForm::BINDTOKEY:
      bufcmd++;
      c = *bufcmd++;
      for (i = 0; *bufcmd; ++i) {
        string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
      }
      string[i] = '\0';
      code = decrypt(string);
      /*
       * Delete previous binding of standard key.
       */
      for (auto& ktp : Editor::_keytab) {
        if (ktp.code() == code) {
          ktp.unset();
          break;
        }
      }
      Editor::_macros[c].m_code = code;
      break;
    case SpecialForm::FUNCTION:
      if (*++bufcmd == expr) {
        bufcmd = Editor::_macros[expr].m_exec;
        continue;
      } else {
        while (n-- && s == T) {
          s = eval(*bufcmd, depth + 1) ? T : NIL;
        }
      }
      break;
    case SpecialForm::INSERTNAME:
      for (auto name = curbp->bufname(); *name && s == T; ++name) {
        s = EDLINE::linsert(*name) ? T : NIL;
      }
      break;
    case SpecialForm::INSERTBASENAME:
      for (auto name = curbp->bufname(); *name && s == T; ++name) {
        if (*name == '.') {
          break;
        } else {
          s = EDLINE::linsert(*name) ? T : NIL;
        }
      }
      break;
    case SpecialForm::UPDATE:
      display->update();
      break;
    case SpecialForm::INSERTCOMMAND:
      bufcmd++;
      for (i = 0; *bufcmd; ++i) {
        string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
      }
      string[i] = '\0';
      WDGwrite(ECSTR(": execute '%s'"), string);
      s = syscompile(string, SYSCOMP_NOERROR) ? T : NIL;
      break;
    case SpecialForm::INPACKAGE:
      bufcmd++;
      break;
    case SpecialForm::NOFUNCTION:
      break;
    default:
      {
        auto sv = Editor::_repeat;
        s = Editor::execute(c, n);
        Editor::_repeat = sv;
      }
    }
    n = 1;
    bufcmd++;
  }

  return *bufcmd == 0;
}

/*
 * An error occurs while reading a macro definition.
 */

void
MLisp::readerror(const EMCHAR* msg, const EMCHAR* arg) {
  EMCHAR buf[NPAT];

  (void)emstrcpy(buf, msg);
  (void)emstrcat(buf, arg);
  (void)WDGmessage(buf);
  throw false;
}

/*
 * Load interactively a macro definition file.
 */

bool
MLisp::loadmacro(const EMCHAR* macfile) {
  if (ffaccess(macfile) != FIOSUC) {
    return false;
  }

  auto res = true;

  try {
    _insideComment = false;
    _insideString = false;
    if ((_mfile = ffopen(macfile, ECSTR("r"))) != 0) {
      while (getfun()) {
        continue;
      }
    } else {
      return false;
    }
  } catch(...) {
    res = false;
  }

  (void)std::fclose(_mfile);
  _mfile = nullptr;
  return res;
}

/*
 * Load the default macro definition.
 */

bool
mlcustomize() {
  EMCHAR* p;
  EMCHAR  init[NPAT];
  EMCHAR  base[NPAT];

  if ((p = ffgetenv(ECSTR("EMACSLIB"))) == nullptr) {
    (void)emstrcpy(MLisp::_library, PATH);
  } else {
    (void)emstrcpy(MLisp::_library, p);
  }

  (void)emstrcat(MLisp::_library, ECSTR("/"));
  (void)emstrcpy(init, MLisp::_library);
  (void)emstrcat(init, ECSTR("emacs.lsp"));

  /*
   * Try to find load emacs.lsp
   */

  if ((MLisp::_mfile = ffopen(init, ECSTR("r"))) != 0) {
    /*
     * The PATH is at the standard place.
     */
    (void)std::fclose(MLisp::_mfile);
    MLisp::_mfile = nullptr;
  } else {
    /*
     * Set the _library PATH where the binary is found.
     */
    (void)emstrcpy(MLisp::_library, Editor::getName());
    (void)updir(MLisp::_library, SLASH);
  }

  makename(base, Editor::getName());

  for (p = base; *p; ++p) {
    if (*p == '.') {
      *p = '\000';
      break;
    }
  }

  (void)emstrcat(base, ECSTR(".lsp"));
  (void)emstrcpy(init, MLisp::_library);
  (void)emstrcat(init, base);

  return MLisp::loadmacro(base) || MLisp::loadmacro(init);
}

/*
 * Evaluate a macro definition n times.
 */

CMD
mlinternaleval(int expr) {
  for (decltype(Editor::_repeat) i{0}; i < Editor::_repeat; ++i) {
    if (MLisp::eval(expr, 1) != true) {
      return NIL;
    }
  }

  return T;
}

/*
 * Read  a  macro  file into the current macro buffer.  Find the
 * name of the file,  and call the standard loadmacro code.  The
 * file   is   search   in   current  directory  first  then  in
 * /usr/local/emact/lib  or  EMACSLIB  directory  if  not found.
 * /Bound to "M-C-M".
 */

CMD
getmacfile() {
  CMD    s;
  EMCHAR fname[NFILEN];

  complete = filematch;

  s = mlreply(ECSTR(": macro-file "), fname, NFILEN);

  return ((s == T) && MLisp::loadmacro(fname) ? T : NIL);
}

CMD
lispevalbuffer() {
  WDGerror(ECSTR("Not implemented !"));
  return NIL;
}

CMD
evalexpression() {
  WDGerror(ECSTR("Not implemented !"));
  return NIL;
}
#endif
