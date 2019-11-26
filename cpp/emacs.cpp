#if !defined(lint)
static auto rcsid("$Id: emacs.cpp,v 1.52 2018/09/09 07:21:09 jullien Exp $");
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
 * This  file  contains  the  main  driving  routine,  and  some
 * keyboard  processing code,  for the EmACT screen editor.  The
 * general  structure  is based on program originally written by
 * Dave G. Conroy.
 *
 *    Please note that Emacs stand for "Editing MACroS" and not for
 *           "Eventually malloc()'s All Computer Storage"
 *              "Eight Mega-bytes And Continue to Swap"
 *                  "Escape-Meta-Alt-Control-Shift"
 */

#define _CRT_SECURE_NO_WARNINGS
#include "./emacs.h"
#include "./build.h"
#include "./Buffer.h"
#include "./Completion.h"
#include "./Counter.h"
#include "./EditWindow.h"
#include "./EditorCommand.h"
#include "./Error.h"
#include "./Line.h"
#include "./MiniBuf.h"
#include "./Options.h"
#include "./Process.h"
#include "./TextRegion.h"
#include "./Redisplay.h"
#include "./Search.h"
#include "./Terminal.h"
#include "./Word.h"
#include "./Widget.h"

static constexpr auto BACKDEL(0x7F);
static bool    editflag{false};  // Edit flag

static bool    linenump(const EMCHAR* s);
static int     getctl();
static bool    latexinsert(int n, int c);
static CMD     again();

extern const EMCHAR* version;   /* Current version              */

static bool initflag = false;   /* Init flag                    */

Terminal*   term{nullptr};
Redisplay*  redisplay{nullptr}; /* Redisplay global object      */
Buffer*     curbp{nullptr};     /* Current buffer               */
EditWindow* curwp{nullptr};     /* Current window               */
MEvent      mevent;             /* Mouse event (if any)         */
Kbdm        kbdm;               /* Keyboad Macro                */

int    Editor::_curgoal;              // Goal column
int    Editor::_repeat{1};            // Repeat count
int    Editor::_thisflag{CFUNSET};    // Flags, this command
int    Editor::_lastflag{CFUNSET};    // Flags, last command
EMCHAR Editor::_search[NPAT];         // Internal search buffer
std::vector<Macro> Editor::_macros;   // User macros table
Point  Editor::_found;                // Position of last search

/*
 * Internal counter variables.
 */
int    Counter::_val{0};
EMCHAR Counter::_fmt[NPAT] = { '%', 'd', 0 };

CMD
Counter::insert() {
  EMCHAR buf[NPAT];

  (void)emsprintf(buf, &_fmt[0], _val);

  for (auto s = &buf[0]; *s; ++s) {
    (void)Line::insert(*s);
  }

  return T;
}

/*
 * Command table.  This table is *roughly* in ASCII order, left
 * to right across the characters of the command.
 */

std::vector<EditorCommand> Editor::_keytab = {
  {
     Ctrl|'@',
     setmark,
     ECSTR("set-mark-command")
  },
  {
     Ctrl|'A',
     gotobol,
     ECSTR("beginning-of-line")
  },
  {
     Ctrl|'B',
     backchar,
     ECSTR("backward-char")
  },
  {
     Ctrl|'D',
     forwdel,
     ECSTR("delete-char")
  },
  {
     Ctrl|'E',
     gotoeol,
     ECSTR("end-of-line")
  },
  {
     Ctrl|'F',
     forwchar,
     ECSTR("forward-character")
  },
  {
     Ctrl|'G',
     Editor::ctrlg,
     ECSTR("illegal-operation")
  },
  {
     Ctrl|'H',
     backdel,
     ECSTR("delete-backward-char")
  },
  {
     Ctrl|'I',
     Editor::tab,
     ECSTR("indent-for-tab-command")
  },
  {
     Ctrl|'J',
     newlineindent,
     ECSTR("newline-and-indent")
  },
  {
     Ctrl|'K',
     killtext,
     ECSTR("kill-line")
  },
  {
     Ctrl|'L',
     EditWindow::recenter,
     ECSTR("recenter")
  },
  {
     Ctrl|'M',
     endline,
     ECSTR("newline")
  },
  {
     Ctrl|'N',
     forwline,
     ECSTR("next-line")
  },
  {
     Ctrl|'O',
     openline,
     ECSTR("open-line")
  },
  {
     Ctrl|'P',
     backline,
     ECSTR("previous-line")
  },
  {
     Ctrl|'Q',
     Editor::quotechar,
     ECSTR("quoted-insert")
  },
  {
     Ctrl|'R',
     Search::backward,
     ECSTR("backward-search")
  },
  {
     Ctrl|'S',
     Search::forward,
     ECSTR("forward-search")
  },
  {
     Ctrl|'T',
     Editor::twiddle,
     ECSTR("transpose-chars")
  },
  {
     Ctrl|'V',
     forwpage,
     ECSTR("scroll-up")
  },
  {
     Ctrl|'W',
     TextRegion::kill,
     ECSTR("kill-region")
  },
  {
     Ctrl|'Y',
     yank,
     ECSTR("yank")
  },
  {
     Ctrl|'Z',
     Process::spawncli,
     ECSTR("suspend-emacs")
  },
  {
     Ctrl|']',
     Search::complete,
     ECSTR("dabbrev-expand")
  },
  {
     Ctrl|'_',
     Editor::Editor::undo,
     ECSTR("Editor::Editor::undo")
  },
  {
     CTLX|METACH,
     again,
     ECSTR("repeat-last-command")
  },
  {
     CTLX|Ctrl|'B',
     Buffer::listbuffers,
     ECSTR("list-buffers")
  },
  {
     CTLX|Ctrl|'C',
     Editor::exitemacs,
     ECSTR("save-buffers-kill-emacs")
  },
  {
     CTLX|Ctrl|'D',
     Process::changedir,
     ECSTR("cd")
  },
  {
     CTLX|Ctrl|'E',
     Process::evalbuf,
     ECSTR("eval-buffer")
  },
  {
     CTLX|Ctrl|'F',
     Editor::findFile,
     ECSTR("find-file")
  },
  {
     CTLX|Ctrl|'I',
     indentline,
     ECSTR("indent-rigidily")
  },
  {
     CTLX|Ctrl|'L',
     TextRegion::lower,
     ECSTR("downcase-region")
  },
  {
     CTLX|Ctrl|'M',
     Process::makefile,
     ECSTR("execute-makefile")
  },
  {
     CTLX|Ctrl|'N',
     EditWindow::moveDown,
     ECSTR("scroll-one-line-down")
  },
  {
     CTLX|Ctrl|'O',
     Editor::delblank,
     ECSTR("delete-blank-lines")
  },
  {
     CTLX|Ctrl|'P',
     EditWindow::moveUp,
     ECSTR("scroll-one-line-up")
  },
  {
     CTLX|Ctrl|'Q',
     Editor::toggleRead,
     ECSTR("toggle-read-only")
  },
  {
     CTLX|Ctrl|'R',
     Editor::fileRead,
     ECSTR("find-file-read-only")
  },
  {
     CTLX|Ctrl|'S',
     Editor::fileSave,
     ECSTR("save-buffer")
  },
  {
     CTLX|Ctrl|'T',
     Line::twiddle,
     ECSTR("transpose-lines")
  },
  {
     CTLX|Ctrl|'U',
     TextRegion::upper,
     ECSTR("upcase-region")
  },
  {
     CTLX|Ctrl|'V',
     Editor::fileAlternate,
     ECSTR("find-alternate-file")
  },
  {
     CTLX|Ctrl|'W',
     Editor::fileWrite,
     ECSTR("write-file")
  },
  {
     CTLX|Ctrl|'X',
     swapmark,
     ECSTR("exchange-point-and-mark")
  },
  {
     CTLX|Ctrl|'Z',
     EditWindow::shrink,
     ECSTR("shrink-window")
  },
  {
     CTLX|' ',
     setmark,
     ECSTR("set-mark-command")
  },
  {
     CTLX|'!',
     Process::getcommand,
     ECSTR("get-command-in-buffer")
  },
  {
     CTLX|'%',
     Process::spawn,
     ECSTR("execute-monitor-command")
  },
  {
     CTLX|'=',
     Editor::showcpos,
     ECSTR("what-cursor-position")
  },
  {
     CTLX|'(',
     Editor::ctlxlp,
     ECSTR("start-remembering")
  },
  {
     CTLX|')',
     Editor::ctlxrp,
     ECSTR("stop-remembering")
  },
  {
     CTLX|'.',
     Editor::setFillPrefix,
     ECSTR("set-fill-prefix")
  },
  {
     CTLX|'0',
     EditWindow::delwind,
     ECSTR("delete-window")
  },
  {
     CTLX|'1',
     EditWindow::onlywind,
     ECSTR("delete-other-windows")
  },
  {
     CTLX|'2',
     EditWindow::split,
     ECSTR("split-window-vertically")
  },
  {
     CTLX|'8',
     EditWindow::adjust,
     ECSTR("adjust-to-80-columns")
  },
  {
     CTLX|'>',
     TextRegion::shiftright,
     ECSTR("shift-region-right")
  },
  {
     CTLX|'<',
     TextRegion::shiftleft,
     ECSTR("shift-region-left")
  },
  {
     CTLX|'`',
     Error::next,
     ECSTR("next-error")
  },
  {
     CTLX|'A',
     Process::assemble,
     ECSTR("assemble-file")
  },
  {
     CTLX|'B',
     Buffer::usebuffer,
     ECSTR("use-buffers")
  },
  {
     CTLX|'C',
     Process::compilecurrent,
     ECSTR("compile-current-buffer")
  },
  {
     CTLX|'D',
     Completion::dired,
     ECSTR("dired")
  },
  {
     CTLX|'E',
     Editor::ctlxe,
     ECSTR("execute-keyboard-macro")
  },
  {
     CTLX|'F',
     Editor::setFillColumn,
     ECSTR("set-fill-column")
  },
  {
     CTLX|'H',
     markwholebuffer,
     ECSTR("mark-whole-buffer")
  },
  {
     CTLX|'I',
     Editor::fileInsert,
     ECSTR("insert-file")
  },
  {
     CTLX|'J',
     Process::javacompile,
     ECSTR("java-compile")
  },
  {
     CTLX|'K',
     Buffer::kill,
     ECSTR("kill-buffer")
  },
  {
     CTLX|'N',
     EditWindow::next,
     ECSTR("next-window")
  },
  {
     CTLX|'O',
     EditWindow::previous,
     ECSTR("other-window")
  },
  {
     CTLX|'P',
     EditWindow::previous,
     ECSTR("previous-window")
  },
  {
     CTLX|'R',
     Options::setvar,
     ECSTR("global-rebind")
  },
  {
     CTLX|'S',
     Editor::saveSomeBuffers,
     ECSTR("save-some-buffers")
  },
  {
     CTLX|'U',
     Editor::Editor::undo,
     ECSTR("Editor::Editor::undo")
  },
  {
     CTLX|'W',
     TextRegion::write,
     ECSTR("write-region")
  },
  {
     CTLX|'Z',
     EditWindow::enlarge,
     ECSTR("enlarge-window")
  },
  {
     META|METACH,
     MLisp::evalExpression,
     ECSTR("eval-expression")
  },
  {
     META|Ctrl|'\\',
     TextRegion::indent,
     ECSTR("indent-region")
  },
  {
     META|Ctrl|'A',
     Editor::switchas,
     ECSTR("assembler-mode")
  },
  {
     META|Ctrl|'B',
     blispexpr,
     ECSTR("beginning-of-expression")
  },
  {
     META|Ctrl|'C',
     Editor::switchcc,
     ECSTR("c-mode")
  },
  {
     META|Ctrl|'D',
     Search::diffWindows,
     ECSTR("diff-windows")
  },
  {
     META|Ctrl|'E',
     elispexpr,
     ECSTR("end-of-expression")
  },
  {
     META|Ctrl|'F',
     Search::definition,
     ECSTR("get-definition")
  },
  {
     META|Ctrl|'H',
     Word::delBackward,
     ECSTR("backward-kill-word")
  },
  {
     META|Ctrl|'L',
     Editor::switchlisp,
     ECSTR("lisp-mode")
  },
  {
     META|Ctrl|'J',
     Editor::switchjava,
     ECSTR("java-mode")
  },
  {
     META|Ctrl|'M',
     MLisp::readFile,
     ECSTR("prompt-for-macro-file")
  },
  {
     META|Ctrl|'N',
     Editor::switchfund,
     ECSTR("fundamental-mode")
  },
  {
     META|Ctrl|'O',
     Editor::switchfortran,
     ECSTR("fortran-mode")
  },
  {
     META|Ctrl|'P',
     Editor::switchprolog,
     ECSTR("prolog-mode")
  },
  {
     META|Ctrl|'R',
     Editor::revertBuffer,
     ECSTR("revert-buffer")
  },
  {
     META|Ctrl|'U',
     Editor::insertunicode,
     ECSTR("insert-unicode")
  },
  {
     META|Ctrl|'V',
     forwother,
     ECSTR("scroll-other-window")
  },
  {
     META|Ctrl|'W',
     Editor::appendNextKill,
     ECSTR("append-next-kill")
  },
  {
     META|' ',
     Editor::justOneSpace,
     ECSTR("just-one-space")
  },
  {
     META|'+',
     Editor::switchcpp,
     ECSTR("c++-mode")
  },
  {
     META|'!',
     EditWindow::reposition,
     ECSTR("reposition")
  },
  {
     META|'$',
     Search::compareWindows,
     ECSTR("compare-windows")
  },
  {
     META|'%',
     Search::queryReplace,
     ECSTR("query-replace")
  },
  {
     META|'&',
     Search::globalReplace,
     ECSTR("replace-string")
  },
  {
     META|'(',
     Search::leftParent,
     ECSTR("match-left-parenthesis")
  },
  {
     META|')',
     Search::rightParent,
     ECSTR("match-right-parenthesis")
  },
  {
     META|',',
     Options::tagsloopcont,
     ECSTR("tags-loop-continue")
  },
  {
     META|'.',
     Options::findtag,
     ECSTR("find-tag")
  },
  {
     META|'>',
     gotoeob,
     ECSTR("end-of-buffer")
  },
  {
     META|'<',
     gotobob,
     ECSTR("beginning-of-buffer")
  },
  {
     META|'?',
     Options::describekey,
     ECSTR("apropos")
  },
  {
     META|'/',
     Search::complete,
     ECSTR("dabbrev-expand")
  },
  {
     META|';',
     Editor::justifyComment,
     ECSTR("justify-comment")
  },
  {
     META|'B',
     Word::backward,
     ECSTR("backward-word")
  },
  {
     META|'C',
     Word::capitalize,
     ECSTR("capitalize-word")
  },
  {
     META|'D',
     Word::delForward,
     ECSTR("delete-forward")
  },
  {
     META|'E',
     Error::next,
     ECSTR("next-error")
  },
  {
     META|'F',
     Word::forward,
     ECSTR("forward-word")
  },
  {
     META|'G',
     gotoline,
     ECSTR("goto-line")
  },
  {
     META|'I',
     Line::instoggle,
     ECSTR("toggle-insert")
  },
  {
     META|'H',
     Editor::markParagraph,
     ECSTR("mark-paragrah")
  },
  {
     META|'L',
     Word::lower,
     ECSTR("downcase-word")
  },
  {
     META|'M',
     backtoindent,
     ECSTR("back-to-indent")
  },
  {
     META|'N',
     gotoline,
     ECSTR("goto-line")
  },
  {
     META|'Q',
     Editor::fillParagraph,
     ECSTR("fill-paragraph")
  },
  {
     META|'R',
     Search::backward,
     ECSTR("backward-search")
  },
  {
     META|'S',
     Search::forward,
     ECSTR("forward-search")
  },
  {
     META|'T',
     Word::twiddle,
     ECSTR("transpose-words")
  },
  {
     META|'U',
     Word::upper,
     ECSTR("upcase-word")
  },
  {
     META|'V',
     backpage,
     ECSTR("scroll-down")
  },
  {
     META|'W',
     TextRegion::copy,
     ECSTR("kill-ring-save")
  },
  {
     META|'X',
     Options::setvar,
     ECSTR("eval-function")
  },
  {
     META|'[',
     Search::leftBracket,
     ECSTR("match-left-bracket")
  },
  {
     META|']',
     Search::rightBracket,
     ECSTR("match-right-bracket")
  },
  {
     META|'{',
     Editor::backParagraph,
     ECSTR("backward-paragraph")
  },
  {
     META|'}',
     Editor::forwParagraph,
     ECSTR("forward-paragraph")
  },
  {
     META|'~',
     Line::notmodified,
     ECSTR("not-modified")
  },
  {
     META|BACKDEL,
     Word::delBackward,
     ECSTR("backward-kill-word")
  },
  {
     CXDR|'$',
     Counter::insert,
     ECSTR("counter-insert")
  },
  {
     CXDR|'+',
     Counter::incr,
     ECSTR("counter-incr")
  },
  {
     CXDR|'-',
     Counter::decr,
     ECSTR("counter-decr")
  },
  {
     CXDR|'S',
     Counter::set,
     ECSTR("counter-set")
  },
  {
     CXDR|'F',
     Counter::format,
     ECSTR("counter-format")
  },
  {
     BACKDEL,
     backdel,
     ECSTR("delete-previous-character")
  },
  {
     MEVT,
     EditWindow::find,
     ECSTR("find-window")
  },

  /*
   * unbound functions (called with M-x)
   */

  {
     UNBOUND,
     Editor::ansiToOem,
     ECSTR("ansi-to-oem")
  },
  {
     UNBOUND,
     Editor::binaryfile,
     ECSTR("binary-file")
  },
  {
     UNBOUND,
     Search::compareWindows,
     ECSTR("compare-windows")
  },
  {
     UNBOUND,
     Process::compile,
     ECSTR("compile")
  },
  {
     UNBOUND,
     Editor::emacsversion,
     ECSTR("emacs-version")
  },
  {
     UNBOUND,
     Editor::exitemacs,
     ECSTR("exit-emacs")
  },
  {
     UNBOUND,
     TextRegion::fill,
     ECSTR("fill-region")
  },
  {
     UNBOUND,
     Process::grep,
     ECSTR("grep")
  },
  {
     UNBOUND,
     Options::help,
     ECSTR("help")
  },
  {
     UNBOUND,
     Editor::switchsgml,
     ECSTR("sgml-mode")
  },
  {
     UNBOUND,
     Editor::justifyCurLine,
     ECSTR("justify-current-line")
  },
  {
     UNBOUND,
     Editor::killemacs,
     ECSTR("kill-emacs")
  },
  {
     UNBOUND,
     Editor::macToAnsi,
     ECSTR("mac-to-ansi")
  },
  {
     UNBOUND,
     Editor::macToOem,
     ECSTR("mac-to-oem")
  },
  {
     UNBOUND,
     Process::man,
     ECSTR("man")
  },
  {
     UNBOUND,
     Search::leftCurly,
     ECSTR("match-left-curly-bracket")
  },
  {
     UNBOUND,
     Search::rightCurly,
     ECSTR("match-right-curly-bracket")
  },
  {
     UNBOUND,
     Editor::oemToAnsi,
     ECSTR("oem-to-ansi")
  },
  {
     UNBOUND,
     Editor::printBuffer,
     ECSTR("print-buffer")
  },
  {
     UNBOUND,
     Process::perl,
     ECSTR("perl")
  },
  {
     UNBOUND,
     Editor::switchperl,
     ECSTR("perl-mode")
  },
  {
     UNBOUND,
     Editor::switchpython,
     ECSTR("python-mode")
  },
  {
     UNBOUND,
     Editor::redrawscreen,
     ECSTR("redraw-screen")
  },
  {
     UNBOUND,
     Process::sed,
     ECSTR("sed")
  },
  {
     UNBOUND,
     Editor::setJustifyLeft,
     ECSTR("set-justification-left")
  },
  {
     UNBOUND,
     Editor::setJustifyFull,
     ECSTR("set-justification-full")
  },
  {
     UNBOUND,
     Options::uncompile,
     ECSTR("uncompile-macro")
  },
  {
     UNBOUND,
     Editor::unlinkFile,
     ECSTR("unlink-file")
  },
  {
     UNBOUND,
     Editor::utf8encoding,
     ECSTR("utf8-encoding")
  },
  {
     UNBOUND,
     Editor::utf16encoding,
     ECSTR("utf16-encoding")
  },
  {
     UNBOUND,
     Editor::systemencoding,
     ECSTR("system-encoding")
  },
  {
     UNBOUND,
     Editor::enterDebug,
     ECSTR("enter-debug")                 }
};

namespace opt {
/*
 * Custom variables
 */

bool    append_process_buffer   = false;
bool    auto_encoding_mode      = true;
bool    auto_fill_mode          = false;
int     background_color        = 0;
bool    backup_before_writing   = true;
bool    binary_mode             = false;
bool    black_on_white          = false;
bool    bold_font               = false;
bool    case_sensitivity        = true;
bool    compile_in_buffer       = true;
bool    confirm_unsaved_buffer  = true;
bool    date_completion         = true;
bool    display_command         = false;
bool    fast_redisplay          = true;
int     fill_column             = 70;
int     foreground_color        = 7;
bool    gnu_compatible          = true;
bool    sgml_mode               = false;
bool    latex_mode              = false;
bool    line_number_mode        = true;
bool    mouse_avoidance_mode    = true;
int     mouse_avoidance_nudge   = 3;
bool    mouse_flag              = true;
bool    monochrome_monitor      = false;
bool    pipe_process            = true;
int     screen_height           = 25;
int     screen_width            = 80;
bool    set_show_graphic        = false;
bool    show_menu               = true;
int     tab_display             = 2;
int     tab_size                = 2;
int     java_indent             = 2;
bool    replace_mode            = false;
bool    system_colors           = true;

EMCHAR  as_arg[NPAT];                 // Assembler arguments
EMCHAR  as_name[NCMDN];               // Assembler name
EMCHAR  cc_arg[NPAT];                 // Compiler arguments
EMCHAR  cc_name[NCMDN];               // Compiler name
EMCHAR  fill_prefix[NPAT];            // Fill prefix string
EMCHAR  java_comp_args[NPAT];         // Java compiler name
EMCHAR  java_comp_name[NCMDN];        // Java compiler args.
EMCHAR  java_exec_args[NPAT];         // Java exec name
EMCHAR  java_exec_name[NCMDN];        // Java exec args.
EMCHAR  make_arg[NPAT];               // Make args
EMCHAR  make_name[NCMDN];             // Make name
EMCHAR  helpfile1[NPAT];              // help file 1 (F11)
EMCHAR  helpfile2[NPAT];              // help file 2 (F12)
EMCHAR  helpfile3[NPAT];              // help file 3 (Alt-F11)
EMCHAR  helpfile4[NPAT];              // help file 4 (Alt-F12)
}  // namespace opt

std::vector<Variable> Variable::vartab = {
  { opt::append_process_buffer,  ECSTR("append-process-buffer"),     BOOLVAL },
  { opt::as_arg,                 ECSTR("assembler-arguments"),       NPAT    },
  { opt::as_name,                ECSTR("assembler-name"),            NCMDN   },
  { opt::auto_encoding_mode,     ECSTR("auto-encoding-mode"),        BOOLVAL },
  { opt::auto_fill_mode,         ECSTR("auto-fill-mode"),            BOOLVAL },
  { opt::background_color,       ECSTR("background-color"),          FIXVAL  },
  { opt::backup_before_writing,  ECSTR("backup-before-writing"),     BOOLVAL },
  { opt::binary_mode,            ECSTR("binary-mode"),               BOOLVAL },
  { opt::black_on_white,         ECSTR("black-on-white"),            BOOLVAL },
  { opt::bold_font,              ECSTR("bold-font"),                 BOOLVAL },
  { opt::case_sensitivity,       ECSTR("case-fold-search"),          BOOLVAL },
  { opt::compile_in_buffer,      ECSTR("compile-in-buffer"),         BOOLVAL },
  { opt::confirm_unsaved_buffer, ECSTR("confirm-unsaved-buffer"),    BOOLVAL },
  { opt::cc_arg,                 ECSTR("compiler-arguments"),        NPAT    },
  { opt::cc_name,                ECSTR("compiler-name"),             NCMDN   },
  { opt::date_completion,        ECSTR("date-completion"),           BOOLVAL },
  { opt::display_command,        ECSTR("display-command"),           BOOLVAL },
  { opt::fast_redisplay,         ECSTR("fast-redisplay"),            BOOLVAL },
  { opt::foreground_color,       ECSTR("foreground-color"),          FIXVAL  },
  { opt::fill_column,            ECSTR("fill-column"),               FIXVAL  },
  { opt::fill_prefix,            ECSTR("fill-prefix"),               NPAT    },
  { opt::gnu_compatible,         ECSTR("gnu-compatible"),            BOOLVAL },
  { opt::helpfile1,              ECSTR("help-file1"),                NPAT    },
  { opt::helpfile2,              ECSTR("help-file2"),                NPAT    },
  { opt::helpfile3,              ECSTR("help-file3"),                NPAT    },
  { opt::helpfile4,              ECSTR("help-file4"),                NPAT    },
  { opt::case_sensitivity,       ECSTR("isearch-toggle-case-fold"),  BOOLVAL },
  { opt::java_comp_args,         ECSTR("java-compiler-arguments"),   NCMDN   },
  { opt::java_comp_name,         ECSTR("java-compiler-name"),        NCMDN   },
  { opt::java_exec_args,         ECSTR("java-executable-arguments"), NCMDN   },
  { opt::java_exec_name,         ECSTR("java-executable-name"),      NCMDN   },
  { opt::java_indent,            ECSTR("java-indent"),               FIXVAL  },
  { opt::line_number_mode,       ECSTR("line-number-mode"),          BOOLVAL },
  { opt::latex_mode,             ECSTR("latex-mode"),                BOOLVAL },
  { opt::make_arg,               ECSTR("make-arguments"),            NPAT    },
  { opt::make_name,              ECSTR("make-name"),                 NCMDN   },
  { opt::monochrome_monitor,     ECSTR("monochrome-monitor"),        BOOLVAL },
  { opt::mouse_flag,             ECSTR("mouse-flag"),                BOOLVAL },
  { opt::mouse_avoidance_mode,   ECSTR("mouse-avoidance-mode"),      BOOLVAL },
  { opt::mouse_avoidance_nudge,  ECSTR("mouse-avoidance-nudge-var"), FIXVAL  },
  { opt::pipe_process,           ECSTR("pipe-process"),              BOOLVAL },
  { opt::replace_mode,           ECSTR("replace-mode"),              BOOLVAL },
  { opt::screen_height,          ECSTR("screen-height"),             FIXVAL  },
  { opt::screen_width,           ECSTR("screen-width"),              FIXVAL  },
  { opt::set_show_graphic,       ECSTR("set-show-graphic"),          BOOLVAL },
  { opt::show_menu,              ECSTR("show-menu"),                 BOOLVAL },
  { opt::system_colors,          ECSTR("system-colors"),             BOOLVAL },
  { opt::tab_display,            ECSTR("tabulation-display"),        FIXVAL  },
  { opt::tab_size,               ECSTR("tabulation-size"),           FIXVAL  }
};

/*
 * Fill widgets with default behavior
 */

Widget* widget = new Widget {
  MiniBuf::yn,
  MiniBuf::yesno,
  MiniBuf::confirm,
  MiniBuf::error,
  MiniBuf::title,
  MiniBuf::reply,
  MiniBuf::edit,
  MiniBuf::change,
  MiniBuf::play,
  MiniBuf::wait,
  MiniBuf::message,
  MiniBuf::write,
  MiniBuf::adjust,
  MiniBuf::update,
  MiniBuf::clipCopy,
  MiniBuf::clipPaste,
  MiniBuf::lpPrint
};

static int  clast = 0;      /* last executed command        */
static int  nlast = 1;      /* last executed Editor::_repeat count   */
static bool isoset;         /* ISO 8859-1 char set          */

const EMCHAR* Editor::_name{nullptr};

Editor::Editor(int argc, EMCHAR* argv[], bool)
  : _argc{argc},
    _argv{argv} {
  int     curarg   = 1;
  int     lineinit = 1;
  int     i        = 0;

  Editor::_lastflag = CFUNSET;

  _name    = _argv[0];
  editflag = true;
  isoset   = true;

  if (_argc > curarg) {
    if (linenump(_argv[curarg])) {
      lineinit = emstrtoi(_argv[curarg]);
      curarg++;
    }
  }

  if (!initflag) {
    (void)emstrcpy(opt::cc_name,        ECSTR("cc"));
    (void)emstrcpy(opt::java_comp_name, ECSTR("javac"));
    (void)emstrcpy(opt::java_exec_name, ECSTR("java"));
    (void)emstrcpy(opt::as_name,        ECSTR("masm"));
    (void)emstrcpy(opt::make_name,      ECSTR("make"));

    _search[0] = '\000';

    (void)MLisp::customize();

    opt::background_color &= 0x07;
    opt::foreground_color &= 0x07;

    term = Terminal::getInstance();

    /* First buffer */
    auto bp = Buffer::find(BUF_SCRATCH, true, EDITMODE::LISPMODE);

    if (bp == nullptr) {
      exit(0);
    }

    new EditWindow{bp};  // Allocated Window is managed by an internal list.

    kbdm.reset();
    redisplay = new Redisplay;

    if (_argc > curarg) {
      i++;
      redisplay->update(Redisplay::Mode::REFRESH);
      (void)newfile(_argv[curarg++]);
    }

    if (_argc > curarg) {
      i++;
      (void)EditWindow::split();
      (void)newfile(_argv[curarg++]);
      (void)EditWindow::previous();
    }

    while (_argc > curarg) {
      i++;
      (void)newfile(_argv[curarg++]);
    }

    if (i > 2) {
      (void)Buffer::listbuffers();
      (void)EditWindow::previous();
    }

    /*
     * Search for a Lisp macro named 'emacs-init' to execute at
     * startup.
     */

    for (const auto& macro : Editor::getMacros()) {
      /* Look in macro table. */
      if (macro.name() && !emstrcmp(macro.name(), ECSTR("emacs-init"))) {
        (void)MLisp::eval(macro.index());
        break;
      }
    }

    initflag = true;
  } else {
    (void)MLisp::customize();

    opt::background_color &= 0x07;
    opt::foreground_color &= 0x07;

    term = Terminal::getInstance();
    redisplay->update(Redisplay::Mode::REFRESH);
    if (_argc > curarg) {
      (void)newfile(_argv[curarg]);
    }
  }

  if (lineinit > 1) {
    Editor::_repeat = lineinit;
    (void)gotobob();
    (void)gotoline();
  }

  if (_argc == 1) {
    emacsversion();
  }
}

/*
 * Infinite  loop  that  read a character and execute associated
 * command.
 */

extern bool mpresf;  // true when stuff in message line.

void
Editor::engine() {
  while (editflag) {
    int n;

    redisplay->update();

    auto c = getkey();

    if (c == (Ctrl|'U')) {
      auto mflag = false;
      n = 4;
      WDGwrite(ECSTR("Arg: 4"));
      while (((c = getkey()) >= '0' && c<= '9') || c == (Ctrl|'U')) {
        if (c == (Ctrl|'U')) {
          n *= 4;
        } else {
          if (!mflag) {
            n = 0;
            mflag = true;
          }
          n = 10 * n + c - '0';
        }
        WDGwrite(ECSTR("Arg: %d"), n);
      }
    } else {
      n = 1;
    }

    if (kbdm.isRecording()) {          /* Save macro strokes.  */
      try {
        if (n > 1) {
          kbdm.record((Ctrl|'U'));
          kbdm.record(n);
        }
        kbdm.record(c);
      } catch (const Kbdm::BufferFullException&) {
        WDGwrite(ECSTR("Macro buffer is full."));
        (void)ctrlg();
        continue;
      }
    }

    if (mpresf) {
      MiniBuf::erase();
      redisplay->update();
    }

    if (c == (CTLX|METACH)) {
      /* Editor::_repeat previous action. */
      c = clast;
      n = nlast;
    } else {
      clast = c;
      nlast = n;
    }
    (void)Editor::execute(c, n);
  }
}

/*
 * This is the general command execution routine. It handles the
 * fake binding of all the keys to "self-insert". It also clears
 * out  the  "_thisflag"  word,  and arranges to move it  to  the
 * "_lastflag",  so that the next command can look at it.  Return
 * the status of command.
 */

CMD
Editor::execute(int c, int n) {
  CMD status;

  Editor::_repeat = n;
  resetfreadonly();  // check for readonly is made only once for each command

  if (c > MAX_EMCHAR || c == BACKDEL) {
    /*
     * Look in macro table.
     */
    for (const auto& macro : Editor::getMacros()) {
      if (macro.code() == c) {
        if (opt::display_command) {
          WDGwrite(ECSTR("%s"), macro.name());
        }
        Editor::_thisflag = CFUNSET;
        status = MLisp::eval(macro.index());
        Editor::_lastflag = Editor::_thisflag;
        return status;
      }
    }

    /* Look in key table.   */

    for (const auto& ktp : Editor::_keytab) {
      if (ktp.code() == c) {
        if (opt::display_command) {
          WDGwrite(ECSTR("%s"), ktp.name());
        }
        if (c & SPCL) {
          Editor::_thisflag = CFFKEY;
        } else {
          Editor::_thisflag = CFUNSET;
        }
        status = ktp();  // execute command
        Editor::_lastflag = Editor::_thisflag;
        return status;
      }
    }

    if (c != (Ctrl|'Q') && c != (Ctrl|'S')) {
      return ctrlg();
    }
  }

  if (self_insert(c)) {
    if (n <= 0) {
      Editor::_lastflag = CFUNSET;
      return Editor::_repeat < 0 ? NIL : T;
    }
    Editor::_thisflag = CFUNSET;

#if defined(_POSIX_C_SOURCE)
    /*
     *      Check for F-KEY ^[[?~
     */

    if ((c == '~') && (Editor::_lastflag & CFFKEY)) {
      Editor::_lastflag = CFUNSET;
      return T;
    }
#endif

    auto emode(curbp->editMode());

    if ((c == '}') &&
        (emode == EDITMODE::CMODE      ||
         emode == EDITMODE::CPPMODE    ||
         emode == EDITMODE::CSHARPMODE ||
         emode == EDITMODE::PYTHONMODE ||
         emode == EDITMODE::PERLMODE   ||
         emode == EDITMODE::JAVAMODE)) {
      status = (unindent(c) ? T : NIL);
      Editor::_lastflag = Editor::_thisflag;
      return status;
    }

    if (c > 0x7F && (opt::latex_mode || emode == EDITMODE::SGMLMODE)) {
      status = (latexinsert(Editor::_repeat, c) ? T : NIL);
    } else if (!opt::replace_mode) {
      status = Line::insert(c, Editor::_repeat) ? T : NIL;
    } else {
      status = Line::replace(c, Editor::_repeat) ? T : NIL;
    }

    Editor::_lastflag = Editor::_thisflag;

    if (!kbdm.isPlaying()) {
      if ((c == ')' || c == '}' || c == ']')
          && emode != EDITMODE::FUNDAMENTAL) {
        return Search::autoMatch(c) ? T : NIL;
      }

      if (c == '>' && emode == EDITMODE::SGMLMODE) {
        return Search::autoMatch(c) ? T : NIL;
      }

      if (opt::auto_fill_mode) {
        if (c == ' ' && curwp->line()->position() > opt::fill_column) {
          (void)setmark();
          (void)Editor::splitLineToFill();
          (void)swapmark();
          (void)gotoeol();
        }
      }
    }

    return status;
  }

  Editor::_lastflag = CFUNSET;
  return NIL;
}

/*
 * Read in a key. Do the standard keyboard preprocessing.
 */

int
Editor::getkey() {
  static constexpr auto CTLXCH(0x18);         // Ctrl-X prefix
  static constexpr auto CTLCCH(0x03);         // Ctrl-C prefix
  static constexpr auto CSICODE(0x8F);        // 8 bits version of ESC [
  static constexpr auto SS3CODE(0x9B);        // Application mode of CSI

  auto c = term->get();

  switch (c) {
#if defined(_POSIX_C_SOURCE)
  case METACH :
    WDGwrite(ECSTR("M-"));
    if ((c = getctl()) == (Ctrl|'G')) {
      return c;
    }

    if (c == '[' || c == 'O') {
      return SPCL | term->get();
    } else {
      return META | c;
    }
  case CSICODE :
  case SS3CODE :
    return SPCL | term->get();
#else
  case METACH :           /* M- is prefix         */
    WDGwrite(ECSTR("M-"));
    if ((c = getctl()) == (Ctrl|'G')) {
      return c;
    } else {
      return META | c;
    }
#endif
  case CTLXCH :           /* ^X is a prefix       */
    WDGwrite(ECSTR("C-x-"));
    if ((c = getctl()) == (Ctrl|'G')) {
      return c;
    }

    if (c == '$') {
      WDGwrite(ECSTR("C-x-$-"));
      if ((c = getctl()) == (Ctrl|'G')) {
        return c;
      }

      return CXDR | c;
    }

    return CTLX | c;
  case CTLCCH :           /* ^C is a prefix       */
    WDGwrite(ECSTR("C-c-"));
    if ((c = getctl()) == (Ctrl|'G')) {
      return c;
    } else {
      return CTLC | c;
    }
  default :               /* C0 control -> C-     */
    if (c >= 0x00 && c <= 0x1F) {
      c = Ctrl | (c+'@');
    }
  }

  return c;
}

static CMD
again() {
  return Editor::execute(clast, nlast);
}

CMD
Editor::emacsversion() {
#if defined(UNICODE)
  WDGwrite(ECSTR("%s (%s / UNICODE - Build: %d) of %s - C. Jullien."),
           version,
           SYSTEMNAME,
           EMBUILD,
           ECSTR("" __DATE__));
#else
  WDGwrite(ECSTR("%s (%s / ASCII - Build: %d) of %s - C. Jullien."),
           version,
           SYSTEMNAME,
           EMBUILD,
           ECSTR("" __DATE__));
#endif

  return T;
}

/*
 * Get a key. Apply control modifications to the read key.
 */

static int
getctl() {
  auto c = term->get();

  if (c == METACH || c == BACKDEL) {
    return c;
  } else if (std::iscntrl(c)) {
    return (Ctrl|(c + '@'));
  }

  if (std::isalpha(c) && std::islower(c)) {
    /* Force to upper       */
    c = std::toupper(c);
  }

  return c;
}

/*
 * CAUTION !! Hard exit quit Emacs without saving and without warning
 * Not bound.
 */

CMD
Editor::killemacs() {
  redisplay->tidy();
  editflag = false;
  return T;
}

/*
 * Quit command.  If an argument, always quit. Otherwise confirm
 * if  a buffer has been changed and not written  out.  Normally
 * bound to "C-X C-C".
 */

CMD
Editor::exitemacs() {
  static EMCHAR* msg = ECSTR("Modified buffers exist; exit anymawy? ");

  auto res = Buffer::anycb(Buffer::ANYCB::PROMPT) ? T : NIL;

  if (res == ABORT) {
    return NIL;
  }

  if (opt::confirm_unsaved_buffer) {
    if (res == NIL || WDGyesno(msg) == T) {
      return killemacs();
    }
  } else if (res == NIL || WDGyn(msg) == T) {
    return killemacs();
  }

  return NIL;
}

/*
 * Begin  keyboard  macro.   Error  if not at the top level in
 * keyboard processing. Set up variables and return.
 */

CMD
Editor::ctlxlp() {
  if (kbdm.isRecording() || kbdm.isPlaying()) {
    WDGmessage(ECSTR("Already defining kbd macro."));
    return NIL;
  }
  WDGplay(false);
  WDGmessage(ECSTR("Defining kbd macro..."));
  kbdm.startRecording();
  return T;
}

/*
 * End  keyboard  macro.  Check for the same limit conditions as
 * the above routine. Set up the variables and return.
 */

CMD
Editor::ctlxrp() {
  if (!kbdm.isRecording()) {
    WDGmessage(ECSTR("Not defining a kbd macro."));
    return NIL;
  }
  WDGplay(true);
  WDGmessage(ECSTR("Keyboard macro defined."));
  kbdm.stopRecording();
  return T;
}

/*
 * Execute a macro.  The command argument is the number of times
 * to loop.  Quit as soon as a command gets an error.  Returns T
 * if all ok, else NIL.
 */

CMD
Editor::ctlxe() {
  const auto n = Editor::_repeat;

  if (!kbdm.exist()) {
    WDGmessage(ECSTR("No keyboard macro to execute."));
    return NIL;
  }

  if (kbdm.isRecording() || kbdm.isPlaying()) {
    WDGmessage(ECSTR("You can't call the keyboard macro while defining it."));
    return NIL;
  }

  auto s = T;

  for (decltype(Editor::_repeat) i = 0; (i < n) && (s == T); ++i) {
    auto save = Editor::_repeat;
    int c;
    int an;

    kbdm.startPlaying();
    do {
      if ((c = kbdm.play()) == (Ctrl|'U')) {
        an = kbdm.play();
        c  = kbdm.play();
      } else {
        an = 1;
      }
    } while (c != (CTLX|')') && (s = Editor::execute(c, an)) == T);
    kbdm.stopPlaying();

    Editor::_repeat = save;
  }

  return s;
}

/*
 * Abort,  beep the beeper. Kill off any keyboard macro, etc.,
 * that is in progress.   Sometimes called as a routine, to do
 * general aborting of stuff.
 */

CMD
Editor::ctrlg() {
  term->beep();

  if (kbdm.isRecording()) {
    kbdm.reset();
  }

  return ABORT;
}

/*
 * Insert unicode character from code
 */

CMD
Editor::insertunicode() {
  EMCHAR buf[2];
  EMCHAR unicode_buffer[NPAT]; /* Main pattern             */

  if (WDGedit(ECSTR("Insert unicode from code: "),
              unicode_buffer,
              NPAT) == ABORT) {
    return NIL;
  }

  auto c = emstrtoi(unicode_buffer);

  if (c < 0 || c > MAX_EMCHAR) {
    WDGerror(ECSTR("Code not in range [0, MAX_EMCHAR]"));
    return NIL;
  }

  buf[0] = (EMCHAR)c;
  buf[1] = '\000';
  WDGwrite(ECSTR("Unicode='%s', code(%d, 0x%x)"), buf, c, c);
  Line::insert(c, Editor::_repeat);
  return T;
}

/*
 * Switch between binary and text mode.
 */

CMD
Editor::binaryfile() {
  curbp->setBinary(!curbp->binary());

  Buffer::change(EditWindow::WFEDIT);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to utf8 encoding. (requires UNICODE version).
 */

CMD
Editor::utf8encoding() {
#if defined(UNICODE)
  curbp->setEncoding(ENCODING::EMUTF8);

  Buffer::change(EditWindow::WFEDIT);
  Buffer::updatemodes();

  return T;
#else
  WDGwrite(ECSTR("This command requires an UNICODE enabled version."));
  return NIL;
#endif
}

/*
 * Switch to utf16 encoding. (requires UNICODE version).
 */

CMD
Editor::utf16encoding() {
#if defined(UNICODE)
  curbp->setEncoding(ENCODING::EMUTF16);

  Buffer::change(EditWindow::WFEDIT);
  Buffer::updatemodes();
  return T;
#else
  WDGwrite(ECSTR("This command requires an UNICODE enabled version."));
  return NIL;
#endif
}

/*
 * Switch to system default encoding.
 */

CMD
Editor::systemencoding() {
#if defined(UNICODE)
  curbp->setEncoding(ENCODING::EMASCII);

  Buffer::change(EditWindow::WFEDIT);
  Buffer::updatemodes();
  return T;
#else
  /*
   * this is the default behavior for non-UNICODE version?
   */
  return T;
#endif
}

/*
 * Switch back to fundamental mode.
 */

CMD
Editor::switchfund() {
  curbp->setEditMode(EDITMODE::FUNDAMENTAL);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to C mode. Active auto match-parentheses.
 */

CMD
Editor::switchcc() {
  curbp->setEditMode(EDITMODE::CMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to C++ mode. Active auto match-parentheses.
 */

CMD
Editor::switchcpp() {
  curbp->setEditMode(EDITMODE::CPPMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Java mode. Active auto match-parentheses.
 */

CMD
Editor::switchjava() {
  curbp->setEditMode(EDITMODE::JAVAMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Fortran mode. Active auto match-parentheses.
 */

CMD
Editor::switchfortran() {
  curbp->setEditMode(EDITMODE::FORTRANMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Lisp mode. Active auto match-parentheses.
 */

CMD
Editor::switchlisp() {
  curbp->setEditMode(EDITMODE::LISPMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Perl mode. Active auto match-parentheses.
 */

CMD
Editor::switchperl() {
  curbp->setEditMode(EDITMODE::PERLMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to SGML mode.
 */

CMD
Editor::switchsgml() {
  curbp->setEditMode(EDITMODE::SGMLMODE);
  opt::sgml_mode      = true;
  opt::auto_fill_mode = true;
  opt::latex_mode     = false;

  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Prolog mode. Active auto match-parentheses.
 */

CMD
Editor::switchprolog() {
  curbp->setEditMode(EDITMODE::PROLOGMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Python mode. Active auto match-parentheses.
 */

CMD
Editor::switchpython() {
  curbp->setEditMode(EDITMODE::PYTHONMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to assembler mode.
 */

CMD
Editor::switchas() {
  curbp->setEditMode(EDITMODE::ASMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Switch to Shell mode. Active auto match-parentheses.
 */

CMD
Editor::switchshell() {
  curbp->setEditMode(EDITMODE::SHELLMODE);
  Buffer::updatemodes();
  return T;
}

/*
 * Returns true if c is a separator, false otherwise.
 */

bool
Editor::separatorp(int c) {
  return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r');
}

bool
Editor::charp(int c) {
  return isalnum(c) || (c == '-') || (c == '_') || (c == '+');
}

static bool
linenump(const EMCHAR* s) {
  if (*s == '+') {
    s++;
  }

  while (*s && std::isdigit((int)*s)) {
    s++;
  }

  return !*s;
}

/*
 * Display an internal error.
 */

void
Editor::error(const char* file, int line, const EMCHAR* msg) {
  term->beep();
  WDGwrite(ECSTR("Internal error '%s' file %s at line %d."), msg, file, line);
  term->get();
}

/*
 * Given a 'foreign' character, replace by its LaTeX equivalent.
 */

static bool
latexinsert(int n, int c) {
  static struct {
    int     oem;
    int     iso8859;
    EMCHAR* latex;
    EMCHAR* sgml;
  } convtab[] = {
    /*
     * OEM  ISO   LaTeX              SGML
     */
    { 0x85, 0xE0, ECSTR("\\`a"),     ECSTR("&agrave;") },
    { 0x83, 0xE2, ECSTR("\\^a"),     ECSTR("&acirc;")  },
    { 0x87, 0xE7, ECSTR("\\c{c}"),   ECSTR("&ccedil;") },
    { 0x82, 0xE9, ECSTR("\\'e"),     ECSTR("&eacute;") },
    { 0x88, 0xEA, ECSTR("\\^e"),     ECSTR("&ecirc;")  },
    { 0x89, 0xEB, ECSTR("\\\"e"),    ECSTR("&euml;")   },
    { 0x8A, 0xE8, ECSTR("\\`e"),     ECSTR("&egrave;") },
    { 0x8B, 0xEF, ECSTR("\\\"\\i "), ECSTR("&iuml;")   },
    { 0x8C, 0xEE, ECSTR("\\^\\i "),  ECSTR("&icirc;")  },
    { 0x93, 0xF4, ECSTR("\\^o"),     ECSTR("&ocirc;")  },
    { 0x94, 0xF6, ECSTR("\\\"o"),    ECSTR("&ouml;")   },
    { 0x81, 0xFC, ECSTR("\\\"u"),    ECSTR("&uuml;")   },
    { 0x96, 0xFB, ECSTR("\\^u"),     ECSTR("&ucirc;")  },
    { 0x97, 0xF9, ECSTR("\\`u"),     ECSTR("&ugrave;") }
  };

  for (int i = 0; i < (int)(sizeof(convtab) / sizeof(convtab[0])); ++i) {
    if (c == (isoset ? convtab[i].iso8859 : convtab[i].oem)) {
      if (curbp->editMode() == EDITMODE::SGMLMODE) {
        while (n-- > 0) {
          for (auto p = convtab[i].sgml; *p; ++p) {
            (void)Line::insert(*p);
          }
        }
      } else {
        while (n-- > 0) {
          for (auto p = convtab[i].latex; *p; ++p) {
            (void)Line::insert(*p);
          }
        }
      }
      return true;
    }
  }

  return Line::insert(c, n);
}

/*
 * Refresh the entire screen. Not bound.
 */
CMD
Editor::redrawscreen() {
  Redisplay::garbaged();
  return T;
}
