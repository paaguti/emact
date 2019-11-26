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

#if !defined(__EDITOR_H)
#define __EDITOR_H
#include "./CharType.h"
#include "./EditorCommand.h"
#include "./Macro.h"
#include "./Point.h"

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
#endif /* __EDITOR_H */
