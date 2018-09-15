#if !defined(lint)
static  char rcsid[] = "$Id: emacs.cpp,v 1.52 2018/09/09 07:21:09 jullien Exp $";
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
#include <vector>
#include "emacs.h"
#include "build.h"

#define internaleval(i)       mlinternaleval(i)
#define customize()           mlcustomize()

static constexpr auto BACKDEL(0x7F);
static bool    editflag{false}; // Edit flag


static void    edinit(const EMCHAR* bname);
static void    editloop();
static int     linenump(const EMCHAR* s);
static int     getctl();
static bool    latexinsert(int n, int c);
static CMD     again();

extern const EMCHAR* version;   /* Current version              */

DISPLAY* display{nullptr};
Emacs*   emact{nullptr};

bool    initflag  = false;      /* Init flag                    */
int     nmactab   = 0;          /* Number of user macros        */
int     repeat    = 1;          /* Repeat count                 */

int     thisflag;               /* Flags, this command          */
int     lastflag;               /* Flags, last command          */
int     eargc;                  /* Argc                         */
EMCHAR** eargv;                 /* Argv                         */
BUFFER* curbp;                  /* Current buffer               */
WINSCR* curwp;                  /* Current window               */
MACTAB* pmactab;                /* User macros table pointer    */
MEvent  mevent;                 /* Mouse event (if any)         */
Kbdm    kbdm;                   /* Keyboad Macro                */

static MACTAB mactab[NMAX];     /* User macros table        */
EMCHAR search_buffer[NPAT];     // Internal search buffer

/*
 * Command table.  This table is *roughly* in ASCII order, left
 * to right across the characters of the command.
 */

std::vector<KEYTAB> KEYTAB::keytab = {
  { Ctrl|'@',      setmark,        ECSTR("set-mark-command")            },
  { Ctrl|'A',      gotobol,        ECSTR("beginning-of-line")           },
  { Ctrl|'B',      backchar,       ECSTR("backward-char")               },
  { Ctrl|'D',      forwdel,        ECSTR("delete-char")                 },
  { Ctrl|'E',      gotoeol,        ECSTR("end-of-line")                 },
  { Ctrl|'F',      forwchar,       ECSTR("forward-character")           },
  { Ctrl|'G',      ctrlg,          ECSTR("illegal-operation")           },
  { Ctrl|'H',      backdel,        ECSTR("delete-backward-char")        },
  { Ctrl|'I',      tab,            ECSTR("indent-for-tab-command")      },
  { Ctrl|'J',      newlineindent,  ECSTR("newline-and-indent")          },
  { Ctrl|'K',      killtext,       ECSTR("kill-line")                   },
  { Ctrl|'L',      recenter,       ECSTR("recenter")                    },
  { Ctrl|'M',      endline,        ECSTR("newline")                     },
  { Ctrl|'N',      forwline,       ECSTR("next-line")                   },
  { Ctrl|'O',      openline,       ECSTR("open-line")                   },
  { Ctrl|'P',      backline,       ECSTR("previous-line")               },
  { Ctrl|'Q',      quotechar,      ECSTR("quoted-insert")               },
  { Ctrl|'R',      backsearch,     ECSTR("backward-search")             },
  { Ctrl|'S',      forwsearch,     ECSTR("forward-search")              },
  { Ctrl|'T',      twiddle,        ECSTR("transpose-chars")             },
  { Ctrl|'V',      forwpage,       ECSTR("scroll-up")                   },
  { Ctrl|'W',      killregion,     ECSTR("kill-region")                 },
  { Ctrl|'Y',      yank,           ECSTR("yank")                        },
  { Ctrl|'Z',      spawncli,       ECSTR("suspend-emacs")               },
  { Ctrl|']',      completeword,   ECSTR("dabbrev-expand")              },
  { Ctrl|'_',      undo,           ECSTR("undo")                        },
  { CTLX|METACH,   again,          ECSTR("repeat-last-command")         },
  { CTLX|Ctrl|'B', listbuffers,    ECSTR("list-buffers")                },
  { CTLX|Ctrl|'C', exitemacs,      ECSTR("save-buffers-kill-emacs")     },
  { CTLX|Ctrl|'D', changedir,      ECSTR("cd")                          },
  { CTLX|Ctrl|'E', evalbuf,        ECSTR("eval-buffer")                 },
  { CTLX|Ctrl|'F', findfile,       ECSTR("find-file")                   },
  { CTLX|Ctrl|'I', indentline,     ECSTR("indent-rigidily")             },
  { CTLX|Ctrl|'L', lowerregion,    ECSTR("downcase-region")             },
  { CTLX|Ctrl|'M', makefile,       ECSTR("execute-makefile")            },
  { CTLX|Ctrl|'N', mvdnwind,       ECSTR("scroll-one-line-down")        },
  { CTLX|Ctrl|'O', deblank,        ECSTR("delete-blank-lines")          },
  { CTLX|Ctrl|'P', mvupwind,       ECSTR("scroll-one-line-up")          },
  { CTLX|Ctrl|'Q', toggleread,     ECSTR("toggle-read-only")            },
  { CTLX|Ctrl|'R', fileread,       ECSTR("find-file-read-only")         },
  { CTLX|Ctrl|'S', filesave,       ECSTR("save-buffer")                 },
  { CTLX|Ctrl|'T', ltwiddle,       ECSTR("transpose-lines")             },
  { CTLX|Ctrl|'U', upperregion,    ECSTR("upcase-region")               },
  { CTLX|Ctrl|'V', filealternate,  ECSTR("find-alternate-file")         },
  { CTLX|Ctrl|'W', filewrite,      ECSTR("write-file")                  },
  { CTLX|Ctrl|'X', swapmark,       ECSTR("exchange-point-and-mark")     },
  { CTLX|Ctrl|'Z', shrinkwind,     ECSTR("shrink-window")               },
  { CTLX|' ',      setmark,        ECSTR("set-mark-command")            },
  { CTLX|'!',      getcommand,     ECSTR("get-command-in-buffer")       },
  { CTLX|'%',      spawn,          ECSTR("execute-monitor-command")     },
  { CTLX|'=',      showcpos,       ECSTR("what-cursor-position")        },
  { CTLX|'(',      ctlxlp,         ECSTR("start-remembering")           },
  { CTLX|')',      ctlxrp,         ECSTR("stop-remembering")            },
  { CTLX|'.',      setfillprefix,  ECSTR("set-fill-prefix")             },
  { CTLX|'0',      delwind,        ECSTR("delete-window")               },
  { CTLX|'1',      onlywind,       ECSTR("delete-other-window")         },
  { CTLX|'2',      splitwind,      ECSTR("split-window-vertically")     },
  { CTLX|'8',      adjust,         ECSTR("adjust-to-80-columns")        },
  { CTLX|'>',      shiftright,     ECSTR("shift-region-right")          },
  { CTLX|'<',      shiftleft,      ECSTR("shift-region-left")           },
  { CTLX|'`',      nexterror,      ECSTR("next-error")                  },
  { CTLX|'A',      assemble,       ECSTR("assemble-file")               },
  { CTLX|'B',      usebuffer,      ECSTR("use-buffers")                 },
  { CTLX|'C',      compilecurrent, ECSTR("compile-current-buffer")      },
  { CTLX|'D',      dired,          ECSTR("dired")                       },
  { CTLX|'E',      ctlxe,          ECSTR("execute-keyboard-macro")      },
  { CTLX|'F',      setfillcolumn,  ECSTR("set-fill-column")             },
  { CTLX|'H',      markwholebuffer,ECSTR("mark-whole-buffer")           },
  { CTLX|'I',      fileinsert,     ECSTR("insert-file")                 },
  { CTLX|'J',      javacompile,    ECSTR("java-compile")                },
  { CTLX|'K',      killbuffer,     ECSTR("kill-buffer")                 },
  { CTLX|'N',      nextwind,       ECSTR("next-window")                 },
  { CTLX|'O',      prevwind,       ECSTR("other-window")                },
  { CTLX|'P',      prevwind,       ECSTR("previous-window")             },
  { CTLX|'R',      setvar,         ECSTR("global-rebind")               },
  { CTLX|'S',      savesomebuffers,ECSTR("save-some-buffers")           },
  { CTLX|'T',      topwind,        ECSTR("top-window")                  },
  { CTLX|'U',      undo,           ECSTR("undo")                        },
  { CTLX|'W',      writeregion,    ECSTR("write-region")                },
  { CTLX|'Z',      enlargewind,    ECSTR("enlarge-window")              },
  { META|METACH,   evalexpression, ECSTR("eval-expression")             },
  { META|Ctrl|'\\',indentregion,   ECSTR("indent-region")               },
  { META|Ctrl|'A', switchas,       ECSTR("assembler-mode")              },
  { META|Ctrl|'B', blispexpr,      ECSTR("beginning-of-expression")     },
  { META|Ctrl|'C', switchcc,       ECSTR("c-mode")                      },
  { META|Ctrl|'D', diffwindows,    ECSTR("diff-windows")                },
  { META|Ctrl|'E', elispexpr,      ECSTR("end-of-expression")           },
  { META|Ctrl|'F', getdefinition,  ECSTR("get-definition")              },
  { META|Ctrl|'H', delbword,       ECSTR("backward-kill-word")          },
  { META|Ctrl|'L', switchlisp,     ECSTR("lisp-mode")                   },
  { META|Ctrl|'J', switchjava,     ECSTR("java-mode")                   },
  { META|Ctrl|'M', getmacfile,     ECSTR("prompt-for-macro-file")       },
  { META|Ctrl|'N', switchfund,     ECSTR("fundamental-mode")            },
  { META|Ctrl|'O', switchfortran,  ECSTR("fortran-mode")                },
  { META|Ctrl|'P', switchprolog,   ECSTR("prolog-mode")                 },
  { META|Ctrl|'R', revertbuffer,   ECSTR("revert-buffer")               },
  { META|Ctrl|'U', insertunicode,  ECSTR("insert-unicode")              },
  { META|Ctrl|'V', forwother,      ECSTR("scroll-other-window")         },
  { META|Ctrl|'W', appendnextkill, ECSTR("append-next-kill")            },
  { META|' ',      justonespace,   ECSTR("just-one-space")              },
  { META|'+',      switchcpp,      ECSTR("c++-mode")                    },
  { META|'!',      reposition,     ECSTR("reposition")                  },
  { META|'$',      comparewindows, ECSTR("compare-windows")             },
  { META|'%',      query,          ECSTR("query-replace")               },
  { META|'&',      global,         ECSTR("replace-string")              },
  { META|'(',      matchlpar,      ECSTR("match-left-parenthesis")      },
  { META|')',      matchrpar,      ECSTR("match-right-parenthesis")     },
  { META|',',      tagsloopcont,   ECSTR("tags-loop-continue")          },
  { META|'.',      findtag,        ECSTR("find-tag")                    },
  { META|'>',      gotoeob,        ECSTR("end-of-buffer")               },
  { META|'<',      gotobob,        ECSTR("beginning-of-buffer")         },
  { META|'?',      describekey,    ECSTR("apropos")                     },
  { META|'/',      completeword,   ECSTR("dabbrev-expand")              },
  { META|';',      justifycomment, ECSTR("justify-comment")             },
  { META|'B',      backword,       ECSTR("backward-word")               },
  { META|'C',      capword,        ECSTR("capitalize-word")             },
  { META|'D',      delfword,       ECSTR("delete-forward")              },
  { META|'E',      nexterror,      ECSTR("next-error")                  },
  { META|'F',      forwword,       ECSTR("forward-word")                },
  { META|'G',      gotoline,       ECSTR("goto-line")                   },
  { META|'I',      instoggle,      ECSTR("toggle-insert")               },
  { META|'H',      markparagraph,  ECSTR("mark-paragrah")               },
  { META|'L',      lowerword,      ECSTR("downcase-word")               },
  { META|'M',      backtoindent,   ECSTR("back-to-indent")              },
  { META|'N',      gotoline,       ECSTR("goto-line")                   },
  { META|'Q',      fillparagraph,  ECSTR("fill-paragraph")              },
  { META|'R',      backsearch,     ECSTR("backward-search")             },
  { META|'S',      forwsearch,     ECSTR("forward-search")              },
  { META|'T',      wtwiddle,       ECSTR("transpose-words")             },
  { META|'U',      upperword,      ECSTR("upcase-word")                 },
  { META|'V',      backpage,       ECSTR("scroll-down")                 },
  { META|'W',      copyregion,     ECSTR("kill-ring-save")              },
  { META|'X',      setvar,         ECSTR("eval-function")               },
  { META|'[',      matchlbra,      ECSTR("match-left-bracket")          },
  { META|']',      matchrbra,      ECSTR("match-right-bracket")         },
  { META|'{',      backparagraph,  ECSTR("backward-paragraph")          },
  { META|'}',      forwparagraph,  ECSTR("forward-paragraph")           },
  { META|'~',      notmodified,    ECSTR("not-modified")                },
  { META|0x7F,     delbword,       ECSTR("backward-kill-word")          },
  { CXDR|'$',      counterinsert,  ECSTR("counter-insert")              },
  { CXDR|'+',      counterincr,    ECSTR("counter-incr")                },
  { CXDR|'-',      counterdecr,    ECSTR("counter-decr")                },
  { CXDR|'S',      counterset,     ECSTR("counter-set")                 },
  { CXDR|'F',      counterformat,  ECSTR("counter-format")              },
  { BACKDEL,       backdel,        ECSTR("delete-previous-character")   },
  { MEVT,          findwind,       ECSTR("find-window")                 },

  /*
   *      unbound functions (called with M-x)
   */

  { UNBOUND,       ansitooem,      ECSTR("ansi-to-oem")                 },
  { UNBOUND,       binaryfile,     ECSTR("binary-file")                 },
  { UNBOUND,       comparewindows, ECSTR("compare-windows")             },
  { UNBOUND,       compile,        ECSTR("compile")                     },
  { UNBOUND,       emacsversion,   ECSTR("emacs-version")               },
  { UNBOUND,       exitemacs,      ECSTR("exit-emacs")                  },
  { UNBOUND,       fillregion,     ECSTR("fill-region")                 },
  { UNBOUND,       grep,           ECSTR("grep")                        },
  { UNBOUND,       help,           ECSTR("help")                        },
  { UNBOUND,       switchsgml,     ECSTR("sgml-mode")                   },
  { UNBOUND,       justifycurline, ECSTR("justify-current-line")        },
  { UNBOUND,       killemacs,      ECSTR("kill-emacs")                  },
  { UNBOUND,       mactoansi,      ECSTR("mac-to-ansi")                 },
  { UNBOUND,       mactooem,       ECSTR("mac-to-oem")                  },
  { UNBOUND,       man,            ECSTR("man")                         },
  { UNBOUND,       matchlcur,      ECSTR("match-left-curly-bracket")    },
  { UNBOUND,       matchrcur,      ECSTR("match-right-curly-bracket")   },
  { UNBOUND,       oemtoansi,      ECSTR("oem-to-ansi")                 },
  { UNBOUND,       printbuffer,    ECSTR("print-buffer")                },
  { UNBOUND,       perl,           ECSTR("perl")                        },
  { UNBOUND,       switchperl,     ECSTR("perl-mode")                   },
  { UNBOUND,       redrawscreen,   ECSTR("redraw-screen")               },
  { UNBOUND,       sed,            ECSTR("sed")                         },
  { UNBOUND,       setjustifyleft, ECSTR("set-justification-left")      },
  { UNBOUND,       setjustifyfull, ECSTR("set-justification-full")      },
  { UNBOUND,       uncompile,      ECSTR("uncompile-macro")             },
  { UNBOUND,       unlinkfile,     ECSTR("unlink-file")                 },
  { UNBOUND,       utf8encoding,   ECSTR("utf8-encoding")               },
  { UNBOUND,       utf16encoding,  ECSTR("utf16-encoding")              },
  { UNBOUND,       systemencoding, ECSTR("system-encoding")             },
  { UNBOUND,       enterdebug,     ECSTR("enter-debug")                 }
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
} // namespace opt

std::vector<VARTAB> VARTAB::vartab = {
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

WIDGET widget = {
  mlyn,
  mlyesno,
  mlconfirm,
  mlerror,
  mltitle,
  mlreply,
  mledit,
  mlchange,
  mlplay,
  mlwait,
  mlmessage,
  mlwrite,
  mladjust,
  mlupdate,
  mlclipcopy,
  mlclippaste,
};

EMPRINT printer = {
  mllpprint
};

static int     clast = 0;      /* last executed command        */
static int     nlast = 1;      /* last executed repeat count   */
static bool    isoset;         /* ISO 8859-1 char set          */

bool
self_insert(int c) {
  return (((unsigned int)c & MAX_EMCHAR) >= 0x20 && c != BACKDEL);
}

int
emacsascii(int argc, char* argv[]) {
  std::unique_ptr<EMCHAR*[]> argvu(new EMCHAR*[argc + 1]);
  auto cvt = [](const char* str) -> EMCHAR* {
    auto len = strlen(str);
    auto res = new EMCHAR[len + 1];

    for (int i = 0; i < (int)len; ++i) {
      res[i] = (EMCHAR)str[i];
    }
    res[len] = '\000';

    return res;
  };

  for (int i = 0; i < argc; ++i) {
    argvu[i] = cvt(argv[i]);
  }

  argvu[argc] = nullptr;

  auto res = emacs(argc, argvu.get());

  for (int i = 0; i < argc; ++i) {
    delete[] argvu[i];
  }

  return res;
}

int
emacs(int argc, EMCHAR* argv[]) {
  int     curarg   = 1;
  int     lineinit = 1;
  int     i        = 0;
  EMCHAR  bname[BUFFER::NBUFN];

  emact = new Emacs;

  eargc    = argc;
  eargv    = argv;
  lastflag = 0;
  editflag = true;
  isoset   = true;

  if (eargc > curarg) {
    if (linenump(eargv[curarg])) {
      lineinit = emstrtoi(eargv[curarg]);
      curarg++;
    }
  }

  if (!initflag) {
    pmactab = (MACTAB*)&mactab[0];

    (void)emstrcpy(bname,               BUF_SCRATCH);
    (void)emstrcpy(opt::cc_name,        ECSTR("cc"));
    (void)emstrcpy(opt::java_comp_name, ECSTR("javac"));
    (void)emstrcpy(opt::java_exec_name, ECSTR("java"));
    (void)emstrcpy(opt::as_name,        ECSTR("masm"));
    (void)emstrcpy(opt::make_name,      ECSTR("make"));

    search_buffer[0] = '\000';

    (void)customize();

    opt::background_color &= 0x07;
    opt::foreground_color &= 0x07;

    TTYopen();

    kbdm.reset();

    display = new DISPLAY;
    edinit(bname);

    if (eargc > curarg) {
      i++;
      display->update(DISPLAY::Mode::REFRESH);
      (void)newfile(eargv[curarg++]);
    }

    if (eargc > curarg) {
      i++;
      (void)splitwind();
      (void)newfile(eargv[curarg++]);
      (void)prevwind();
    }

    while (eargc > curarg) {
      i++;
      (void)newfile(eargv[curarg++]);
    }

    (void)curwp->connect(curbp);

    if (i > 2) {
      (void)listbuffers();
      (void)prevwind();
    }

    /*
     *  Search for a Lisp macro named 'emacs-init'
     *  to execute at startup.
     */

    for (i = 0; i < nmactab; ++i) {
      if (MACname(i) && emstrcmp(MACname(i), ECSTR("emacs-init")) == 0) {
        (void)internaleval(i);
        break;
      }
    }

    initflag = true;
  } else {
    (void)customize();

    opt::background_color &= 0x07;
    opt::foreground_color &= 0x07;

    TTYopen();
    display->update(DISPLAY::Mode::REFRESH);
    if (eargc > curarg) {
      (void)newfile(eargv[curarg]);
    }
  }

  if (lineinit > 1) {
    repeat = lineinit;
    (void)gotobob();
    (void)gotoline();
  }

  if (eargc == 1) {
    emacsversion();
  }

  editloop();

  delete emact;
  return 0;
}

/*
 * Initialize all of the buffers and windows. The buffer name is
 * passed down as an argument, because the main routine may have
 * been told to read in a file by default.
 */

static void
edinit(const EMCHAR* bname) {
  /* First buffer */
  auto bp = BUFFER::find(bname, true, EDITMODE::LISPMODE);
  auto wp = new WINSCR;

  if (bp == nullptr || wp == nullptr) {
    exit(0);
  }

  (void)wp->connect(bp);
}

/*
 * Infinite  loop  that  read a character and execute associated
 * command.
 */

extern bool mpresf; // true when stuff in message line.

static void
editloop() {
  while (editflag) {
    int n;

    display->update();

    auto c = getkey();

    if (c == (Ctrl|'U')) {
      auto mflag = false;
      n = 4;
      WDGwrite(ECSTR("Arg: 4"));
      while (((c=getkey())>='0' && c<='9') || c==(Ctrl|'U')) {
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
      mlerase();
      display->update();
    }

    if (c == (CTLX|METACH)) {
			/* repeat previous action. */
      c = clast;
      n = nlast;
    } else {
      clast = c;
      nlast = n;
    }
    (void)execute(c, n);
  }
}

/*
 * This is the general command execution routine. It handles the
 * fake binding of all the keys to "self-insert". It also clears
 * out  the  "thisflag"  word,  and arranges to move it  to  the
 * "lastflag",  so that the next command can look at it.  Return
 * the status of command.
 */

CMD
execute(int c, int n) {
  CMD status;

  repeat = n;
  resetfreadonly(); // check for readonly is made only once for each command

  if (c > MAX_EMCHAR || c == BACKDEL) {
    /*
     * Look in macro table.
     */
    for (int i = 0; i < nmactab; ++i) {
      if (MACcode(i) == c) {
        if (opt::display_command) {
          WDGwrite(ECSTR("%s"), MACname(i));
        }
        thisflag = 0;
        status   = internaleval(i);
        lastflag = thisflag;
        return status;
      }
    }

    /* Look in key table.   */

    for (const auto& ktp : KEYTAB::keytab) {
      if (ktp.code() == c) {
        if (opt::display_command) {
          WDGwrite(ECSTR("%s"), ktp.name());
        }
        if (c & SPCL) {
          thisflag = CFFKEY;
        } else {
          thisflag = 0;
        }
        status   = ktp.execute();
        lastflag = thisflag;
        return status;
      }
    }

    if (c != (Ctrl|'Q') && c != (Ctrl|'S')) {
      return ctrlg();
    }
  }

  if (self_insert(c)) {
    if (n <= 0) {
      lastflag = 0;
      return repeat < 0 ? NIL : T;
    }
    thisflag = 0;

#if defined(_POSIX_C_SOURCE)
    /*
     *      Check for F-KEY ^[[?~
     */

    if ((c == '~') && (lastflag & CFFKEY)) {
      lastflag = 0;
      return T;
    }
#endif

    auto emode(curbp->editMode());

    if ((c=='}') &&
        (emode == EDITMODE::CMODE      ||
         emode == EDITMODE::CPPMODE    ||
         emode == EDITMODE::CSHARPMODE ||
         emode == EDITMODE::PERLMODE   ||
         emode == EDITMODE::JAVAMODE)) {
      status   = (unindent(c) ? T : NIL);
      lastflag = thisflag;
      return status;
    }

    if (c > 0x7F && (opt::latex_mode || emode == EDITMODE::SGMLMODE)) {
      status = (latexinsert(repeat, c) ? T : NIL);
    } else if (!opt::replace_mode) {
      status = linsert(c, repeat) ? T : NIL;
    } else {
      status = lreplace(c, repeat) ? T : NIL;
    }

    lastflag = thisflag;

    if (!kbdm.isPlaying()) {
      if ((c == ')' || c == '}' || c == ']')
          && emode != EDITMODE::FUNDAMENTAL) {
        return automatch(c) ? T : NIL;
      }

      if (c == '>' && emode == EDITMODE::SGMLMODE) {
        return automatch(c) ? T : NIL;
      }

      if (opt::auto_fill_mode) {
        if (c == ' ' && curwp->line()->position() > opt::fill_column) {
          (void)setmark();
          (void)splitlinetofill();
          (void)swapmark();
          (void)gotoeol();
        }
      }
    }

    return status;
  }

  lastflag = 0;
  return NIL;
}

static CMD
again() {
  return execute(clast, nlast);
}

/*
 * Read in a key. Do the standard keyboard preprocessing.
 */

int
getkey() {
  static constexpr auto CTLXCH(0x18);         // Ctrl-X prefix
  static constexpr auto CTLCCH(0x03);         // Ctrl-C prefix
  static constexpr auto CSICODE(0x8F);        // 8 bits version of ESC [
  static constexpr auto SS3CODE(0x9B);        // Application mode of CSI

  auto c = TTYgetc();

  switch (c) {
#if defined(_POSIX_C_SOURCE)
  case METACH :
    WDGwrite(ECSTR("M-"));
    if ((c = getctl()) == (Ctrl|'G')) {
      return c;
    }

    if (c == '[' || c == 'O') {
      return SPCL | TTYgetc();
    } else {
      return META | c;
    }
  case CSICODE :
  case SS3CODE :
    return SPCL | TTYgetc();
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

CMD
emacsversion() {
#if defined(_UNICODE)
  WDGwrite(ECSTR("%s (%s / UNICODE - Build: %d) of %s - C. Jullien."),
           version,
           SYSTEMNAME,
           EMBUILD,
#if defined(TEXT)
           TEXT(__DATE__)
#else
           ECSTR("unknown")
#endif
    );
#else
  WDGwrite(ECSTR("%s (%s / ANSI - Build: %d) of %s - C. Jullien."),
           version,
           SYSTEMNAME,
           EMBUILD,
           __DATE__);
#endif
  return T;
}

/*
 * Get a key. Apply control modifications to the read key.
 */

static int
getctl() {
  auto c = TTYgetc();

  if (c == METACH) {
    return c;
  } else if (c >= 0x00 && c <= 0x1F) {
    return Ctrl | (c+'@');
  }

  if (isalpha(c) && islower(c)) {
    /* Force to upper       */
    c = toupper(c);
  }

  return c;
}

/*
 * CAUTION !! Hard exit quit Emacs without saving and without warning
 * Not bound.
 */

CMD
killemacs() {
  display->tidy();
  editflag = false;
  return T;
}

/*
 * Quit command.  If an argument, always quit. Otherwise confirm
 * if  a buffer has been changed and not written  out.  Normally
 * bound to "C-X C-C".
 */

CMD
exitemacs() {
  static EMCHAR* msg = ECSTR("Modified buffers exist; exit anymawy? ");

  auto res = anycb(ANYCB::PROMPT);

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
ctlxlp() {
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
ctlxrp() {
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
ctlxe() {
  auto n = repeat;
  int     c;
  int     an;

  if (!kbdm.exist()) {
    WDGmessage(ECSTR("No keyboard macro to execute."));
    return NIL;
  }

  if (kbdm.isRecording() || kbdm.isPlaying()) {
    WDGmessage(ECSTR("You can't call the keyboard macro while defining it."));
    return NIL;
  }

  auto s = T;

  do {
    auto save = repeat;
    kbdm.startPlaying();
    do {
      if ((c = kbdm.play()) == (Ctrl|'U')) {
        an = kbdm.play();
        c  = kbdm.play();
      } else {
        an = 1;
      }
    } while (c != (CTLX|')') && (s = execute(c, an)) == T);

    kbdm.stopPlaying();
    repeat = save;
  } while (s == T && --n);

  return s;
}

/*
 * Abort,  beep the beeper. Kill off any keyboard macro, etc.,
 * that is in progress.   Sometimes called as a routine, to do
 * general aborting of stuff.
 */

CMD
ctrlg() {
  TTYbeep();

  if (kbdm.isRecording()) {
    kbdm.reset();
  }

  return ABORT;
}

/*
 * Insert unicode character from code
 */

CMD
insertunicode() {
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
  linsert(c, repeat);
  return T;
}

/*
 * Switch between binary and text mode.
 */

CMD
binaryfile() {
  curbp->setBinary(!curbp->binary());

  lchange(WINSCR::WFEDIT);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to utf8 encoding. (requires UNICODE version).
 */

CMD
utf8encoding() {
#if defined(_UNICODE)
  curbp->setEncoding(ENCODING::EMUTF8);

  lchange(WINSCR::WFEDIT);
  BUFFER::updatemodes();

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
utf16encoding() {
#if defined(_UNICODE)
  curbp->setEncoding(ENCODING::EMUTF16);

  lchange(WINSCR::WFEDIT);
  BUFFER::updatemodes();
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
systemencoding() {
#if defined(_UNICODE)
  curbp->setEncoding(ENCODING::EMASCII);

  lchange(WINSCR::WFEDIT);
  BUFFER::updatemodes();
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
switchfund() {
  curbp->setEditMode(EDITMODE::FUNDAMENTAL);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to C mode. Active auto match-parentheses.
 */

CMD
switchcc() {
  curbp->setEditMode(EDITMODE::CMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to C++ mode. Active auto match-parentheses.
 */

CMD
switchcpp() {
  curbp->setEditMode(EDITMODE::CPPMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Java mode. Active auto match-parentheses.
 */

CMD
switchjava() {
  curbp->setEditMode(EDITMODE::JAVAMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Fortran mode. Active auto match-parentheses.
 */

CMD
switchfortran() {
  curbp->setEditMode(EDITMODE::FORTRANMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Lisp mode. Active auto match-parentheses.
 */

CMD
switchlisp() {
  curbp->setEditMode(EDITMODE::LISPMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Perl mode. Active auto match-parentheses.
 */

CMD
switchperl() {
  curbp->setEditMode(EDITMODE::PERLMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to SGML mode.
 */

CMD
switchsgml() {
  curbp->setEditMode(EDITMODE::SGMLMODE);
  opt::sgml_mode      = true;
  opt::auto_fill_mode = true;
  opt::latex_mode     = false;

  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Prolog mode. Active auto match-parentheses.
 */

CMD
switchprolog() {
  curbp->setEditMode(EDITMODE::PROLOGMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to assembler mode.
 */

CMD
switchas() {
  curbp->setEditMode(EDITMODE::ASMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Switch to Shell mode. Active auto match-parentheses.
 */

CMD
switchshell() {
  curbp->setEditMode(EDITMODE::SHELLMODE);
  BUFFER::updatemodes();
  return T;
}

/*
 * Returns 1 if c is a separator, 0 otherwise.
 */

bool
separatorp(int c) {
  return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r');
}

bool
charp(int c) {
  return isalnum(c) || (c == '-') || (c == '_') || (c == '+');
}

static int
linenump(const EMCHAR* s) {
  if (*s == '+') {
    s++;
  }

  while (*s && isdigit((int)*s)) {
    s++;
  }

  return !*s;
}

/*
 * Display an internal error.
 */

void
emacserror(const EMCHAR* msg, const char *file, int n) {
  TTYbeep();
  if (file != nullptr || n != 0) {
    WDGwrite(ECSTR("Internal error '%s' file %s at line %d."), msg, file, n);
  } else {
    WDGwrite(ECSTR("Internal error '%s'."), msg);
  }
  TTYgetc();
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
     *        OEM   ISO   LaTeX       SGML
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

  for (int i = 0; i < (int)(sizeof(convtab) / sizeof(convtab[0])); i++) {
    if (c == (isoset ? convtab[i].iso8859 : convtab[i].oem)) {
      if (curbp->editMode() == EDITMODE::SGMLMODE) {
        while (n-- > 0) {
          for (auto p = convtab[i].sgml; *p; p++) {
            (void)linsert(*p);
          }
        }
      } else {
        while (n-- > 0) {
          for (auto p = convtab[i].latex; *p; p++) {
            (void)linsert(*p);
          }
        }
      }      
      return true;
    }
  }

  return linsert(c, n);
}
