#if	!defined( lint )
static	char rcsid[] = "$Id: emacs.c,v 1.19 2015/10/25 15:22:27 jullien Exp $";
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
 *	This  file  contains  the  main  driving  routine,  and  some
 *	keyboard  processing code,  for the EmACT screen editor.  The
 *	general  structure  is based on program originally written by
 *	Dave G. Conroy.
 *
 *	Please note that Emacs stand for "Editing MACroS" and not for
 *	       "Eventually malloc()'s All Computer Storage"
 *	          "Eight Mega-bytes And Continue to Swap"
 *		      "Escape-Meta-Alt-Control-Shift"
 */

#include	"emacs.h"
#include	"build.h"

static  void    _define(edinit,(EMCHAR *bname));
static	void	_define(editloop,(void));
static	int	_define(linenump,(EMCHAR *s));
static	int	_define(getctl,(void));
static	int	_define(latexinsert,(int n, int c));
static	CMD	_define(again,(void));

int	initflag  = 0;		/* Init flag			*/
int	nmactab	  = 0;		/* Number of user macros	*/
int	mouseflag = 0;		/* Mouse flag			*/
int	repeat	  = 1;		/* Repeat count			*/
int	justmode  = JUSTFULL;	/* Justification mode		*/

int	errflag;		/* Error flag			*/
int	curflag;		/* Working cursor flag		*/
int	frdflag;		/* flag for freadonly		*/
int	currow;			/* Working cursor row		*/
int	curcol;			/* Working cursor column	*/
EMCHAR	curchar;		/* Working character		*/
int	thisflag;		/* Flags, this command		*/
int	lastflag;		/* Flags, last command		*/
int	curgoal;		/* Goal column			*/
int	editflag;		/* Edit flag			*/
int	nkeytab;		/* Number of commands in keytab	*/
int	nvartab;		/* Number of variables in vartab*/
int	average;		/* Mode line average		*/
int	eargc;			/* Argc				*/
char	**aargv;		/* ASCII Argv			*/
EMCHAR	**eargv;		/* Argv				*/
BUFFER	*curbp;			/* Current buffer		*/
WINSCR	*curwp;			/* Current window		*/
BUFFER	*bheadp;		/* BUFFER listhead		*/
WINSCR	*wheadp;		/* WINSCR listhead		*/
KEYTAB	*pkeytab;		/* Key table pointer		*/
VARTAB	*pvartab;		/* Var table pointer		*/
MACTAB	*pmactab;		/* User macros table pointer	*/
MEvent	mevent;			/* Mouse event (if any)		*/
TERM	*pterm;			/* Terminal information		*/
int	kbdm[ NKBDM ];		/* Macro			*/
int	*kbdmip;		/* Input  for above		*/
int	*kbdmop;		/* Output for above		*/
long	maxlen;			/* Number of lines		*/

static	MACTAB	mactab[ NMAX ];	/* User macros table		*/

/*
 *	Command table.  This table is *roughly* in ASCII order, left
 *	to right across the characters of the command.
 */

static	KEYTAB	keytab[] = {
	{ Ctrl|'@',	setmark,	ECSTR("set-mark-command")	     },
	{ Ctrl|'A',	gotobol,	ECSTR("beginning-of-line")	     },
	{ Ctrl|'B',	backchar,	ECSTR("backward-char")		     },
	{ Ctrl|'D',	forwdel,	ECSTR("delete-char")		     },
	{ Ctrl|'E',	gotoeol,	ECSTR("end-of-line")		     },
	{ Ctrl|'F',	forwchar,	ECSTR("forward-character")	     },
	{ Ctrl|'G',	ctrlg,		ECSTR("illegal-operation")	     },
	{ Ctrl|'H',	backdel,	ECSTR("delete-backward-char")	     },
	{ Ctrl|'I',	tab,		ECSTR("indent-for-tab-command")      },
	{ Ctrl|'J',	newlineindent,	ECSTR("newline-and-indent")	     },
	{ Ctrl|'K',	killtext,	ECSTR("kill-line")		     },
	{ Ctrl|'L',	recenter,	ECSTR("recenter")		     },
	{ Ctrl|'M',	endline,	ECSTR("newline")		     },
	{ Ctrl|'N',	forwline,	ECSTR("next-line")		     },
	{ Ctrl|'O',	openline,	ECSTR("open-line")		     },
	{ Ctrl|'P',	backline,	ECSTR("previous-line")		     },
	{ Ctrl|'Q',	quotechar,	ECSTR("quoted-insert")		     },
	{ Ctrl|'R',	backsearch,	ECSTR("backward-search")	     },
	{ Ctrl|'S',	forwsearch,	ECSTR("forward-search")		     },
	{ Ctrl|'T',	twiddle,	ECSTR("transpose-chars")	     },
	{ Ctrl|'V',	forwpage,	ECSTR("scroll-up")		     },
	{ Ctrl|'W',	killregion,	ECSTR("kill-region")		     },
	{ Ctrl|'Y',	yank,		ECSTR("yank")			     },
	{ Ctrl|'Z',	spawncli,	ECSTR("suspend-emacs")		     },
	{ Ctrl|']',	completeword,	ECSTR("dabbrev-expand")		     },
	{ Ctrl|'_',	undo,		ECSTR("undo")			     },
	{ CTLX|METACH,	again,		ECSTR("repeat-last-command")	     },
	{ CTLX|Ctrl|'B',listbuffers,	ECSTR("list-buffers")		     },
	{ CTLX|Ctrl|'C',exitemacs,	ECSTR("save-buffers-kill-emacs")     },
	{ CTLX|Ctrl|'D',changedir,	ECSTR("cd")			     },
	{ CTLX|Ctrl|'E',evalbuf,	ECSTR("eval-buffer")		     },
	{ CTLX|Ctrl|'F',findfile,	ECSTR("find-file")		     },
	{ CTLX|Ctrl|'I',indentline,	ECSTR("indent-rigidily")	     },
	{ CTLX|Ctrl|'L',lowerregion,	ECSTR("downcase-region")	     },
	{ CTLX|Ctrl|'M',makefile,	ECSTR("execute-makefile")	     },
	{ CTLX|Ctrl|'N',mvdnwind,	ECSTR("scroll-one-line-down")	     },
	{ CTLX|Ctrl|'O',deblank,	ECSTR("delete-blank-lines")	     },
	{ CTLX|Ctrl|'P',mvupwind,	ECSTR("scroll-one-line-up")	     },
	{ CTLX|Ctrl|'Q',toggleread,	ECSTR("toggle-read-only")	     },
	{ CTLX|Ctrl|'R',fileread,	ECSTR("find-file-read-only")	     },
	{ CTLX|Ctrl|'S',filesave,	ECSTR("save-buffer")		     },
	{ CTLX|Ctrl|'T',ltwiddle,	ECSTR("transpose-lines")	     },
	{ CTLX|Ctrl|'U',upperregion,	ECSTR("upcase-region")		     },
	{ CTLX|Ctrl|'V',filealternate,	ECSTR("find-alternate-file")	     },
	{ CTLX|Ctrl|'W',filewrite,	ECSTR("write-file")		     },
	{ CTLX|Ctrl|'X',swapmark,	ECSTR("exchange-point-and-mark")     },
	{ CTLX|Ctrl|'Z',shrinkwind,	ECSTR("shrink-window")		     },
	{ CTLX|' ',	setmark,	ECSTR("set-mark-command")	     },
	{ CTLX|'!',	getcommand,	ECSTR("get-command-in-buffer")	     },
	{ CTLX|'%',	spawn,		ECSTR("execute-monitor-command")     },
	{ CTLX|'=',	showcpos,	ECSTR("what-cursor-position")        },
	{ CTLX|'(',	ctlxlp,		ECSTR("start-remembering")	     },
	{ CTLX|')',	ctlxrp,		ECSTR("stop-remembering")	     },
	{ CTLX|'.',	setfillprefix,	ECSTR("set-fill-prefix")	     },
	{ CTLX|'0',	delwind,	ECSTR("delete-window")		     },
	{ CTLX|'1',	onlywind,	ECSTR("delete-other-window")	     },
	{ CTLX|'2',	splitwind,	ECSTR("split-window-vertically")     },
	{ CTLX|'8',	adjust,		ECSTR("adjust-to-80-columns")	     },
	{ CTLX|'>',	shiftright,	ECSTR("shift-region-right")	     },
	{ CTLX|'<',	shiftleft,	ECSTR("shift-region-left")	     },
	{ CTLX|'`',	nexterror,	ECSTR("next-error")		     },
	{ CTLX|'A',	assemble,	ECSTR("assemble-file")		     },
	{ CTLX|'B',	usebuffer,	ECSTR("use-buffers")		     },
	{ CTLX|'C',	compilecurrent,	ECSTR("compile-current-buffer")      },
	{ CTLX|'D',	dired,		ECSTR("dired")			     },
	{ CTLX|'E',	ctlxe,		ECSTR("execute-keyboard-macro")      },
	{ CTLX|'F',	setfillcolumn,	ECSTR("set-fill-column")	     },
	{ CTLX|'H',	markwholebuffer,ECSTR("mark-whole-buffer")	     },
	{ CTLX|'I',	fileinsert,	ECSTR("insert-file")		     },
	{ CTLX|'J',	javacompile,	ECSTR("java-compile")		     },
	{ CTLX|'K',	killbuffer,	ECSTR("kill-buffer")		     },
	{ CTLX|'N',	nextwind,	ECSTR("next-window")		     },
	{ CTLX|'O',	prevwind,	ECSTR("other-window")		     },
	{ CTLX|'P',	prevwind,	ECSTR("previous-window")	     },
	{ CTLX|'R',	setvar,		ECSTR("global-rebind")		     },
	{ CTLX|'S',	savesomebuffers,ECSTR("save-some-buffers")	     },
	{ CTLX|'T',	topwind,	ECSTR("top-window")		     },
	{ CTLX|'U',	undo,		ECSTR("undo")			     },
	{ CTLX|'W',	writeregion,	ECSTR("write-region")                },
	{ CTLX|'Z',	enlargewind,	ECSTR("enlarge-window")		     },
	{ META|METACH,	evalexpression,	ECSTR("eval-expression")	     },
	{ META|Ctrl|'\\',indentregion,	ECSTR("indent-region")		     },
	{ META|Ctrl|'A',switchas,	ECSTR("assembler-mode")		     },
	{ META|Ctrl|'B',blispexpr,	ECSTR("beginning-of-expression")     },
	{ META|Ctrl|'C',switchcc,	ECSTR("c-mode")			     },
	{ META|Ctrl|'D',diffwindows,	ECSTR("diff-windows")		     },
	{ META|Ctrl|'E',elispexpr,	ECSTR("end-of-expression")	     },
	{ META|Ctrl|'F',getdefinition,	ECSTR("get-definition")		     },
	{ META|Ctrl|'H',delbword,	ECSTR("backward-kill-word")	     },
	{ META|Ctrl|'L',switchlisp,	ECSTR("lisp-mode")	             },
	{ META|Ctrl|'J',switchjava,	ECSTR("java-mode")		     },
	{ META|Ctrl|'M',getmacfile,	ECSTR("prompt-for-macro-file")	     },
	{ META|Ctrl|'N',switchfund,	ECSTR("fundamental-mode")	     },
	{ META|Ctrl|'O',switchfortran,	ECSTR("fortran-mode")		     },
	{ META|Ctrl|'P',switchprolog,	ECSTR("prolog-mode")	             },
	{ META|Ctrl|'R',revertbuffer,	ECSTR("revert-buffer")		     },
	{ META|Ctrl|'U',insertunicode,	ECSTR("insert-unicode")		     },
	{ META|Ctrl|'V',forwother,	ECSTR("scroll-other-window")	     },
	{ META|Ctrl|'W',appendnextkill,	ECSTR("append-next-kill")	     },
	{ META|' ',	justonespace,	ECSTR("just-one-space")		     },
	{ META|'+',	switchcpp,	ECSTR("c++-mode")	             },
	{ META|'!',	reposition,	ECSTR("reposition")		     },
	{ META|'$',	comparewindows,	ECSTR("compare-windows")	     },
	{ META|'%',	query,		ECSTR("query-replace")		     },
	{ META|'&',	global,		ECSTR("replace-string")		     },
	{ META|'(',	matchlpar,	ECSTR("match-left-parenthesis")      },
	{ META|')',	matchrpar,	ECSTR("match-right-parenthesis")     },
	{ META|',',	tagsloopcont,	ECSTR("tags-loop-continue")	     },
	{ META|'.',	findtag,	ECSTR("find-tag")		     },
	{ META|'>',	gotoeob,	ECSTR("end-of-buffer")		     },
	{ META|'<',	gotobob,	ECSTR("beginning-of-buffer")	     },
	{ META|'?',	describekey,	ECSTR("apropos")		     },
	{ META|'/',	completeword,	ECSTR("dabbrev-expand")		     },
	{ META|';',	justifycomment,	ECSTR("justify-comment")	     },
	{ META|'B',	backword,	ECSTR("backward-word")		     },
	{ META|'C',	capword,	ECSTR("capitalize-word")	     },
	{ META|'D',	delfword,	ECSTR("delete-forward")		     },
	{ META|'E',	nexterror,	ECSTR("next-error")		     },
	{ META|'F',	forwword,	ECSTR("forward-word")		     },
	{ META|'G',	gotoline,	ECSTR("goto-line")		     },
	{ META|'I',	instoggle,	ECSTR("toggle-insert")		     },
	{ META|'H',	markparagraph,	ECSTR("mark-paragrah")		     },
	{ META|'L',	lowerword,	ECSTR("downcase-word")		     },
	{ META|'M',	backtoindent,	ECSTR("back-to-indent")		     },
	{ META|'N',	gotoline,	ECSTR("goto-line")		     },
	{ META|'Q',	fillparagraph,	ECSTR("fill-paragraph")		     },
	{ META|'R',	backsearch,	ECSTR("backward-search")	     },
	{ META|'S',	forwsearch,	ECSTR("forward-search")		     },
	{ META|'T',	wtwiddle,	ECSTR("transpose-words")	     },
	{ META|'U',	upperword,	ECSTR("upcase-word")		     },
	{ META|'V',	backpage,	ECSTR("scroll-down")		     },
	{ META|'W',	copyregion,	ECSTR("kill-ring-save")		     },
	{ META|'X',	setvar,		ECSTR("eval-function")		     },
	{ META|'[',	matchlbra,	ECSTR("match-left-bracket")	     },
	{ META|']',	matchrbra,	ECSTR("match-right-bracket")	     },
	{ META|'{',	backparagraph,	ECSTR("backward-paragraph")	     },
	{ META|'}',	forwparagraph,	ECSTR("forward-paragraph")	     },
	{ META|'~',	notmodified,	ECSTR("not-modified")		     },
	{ META|0x7F,	delbword,	ECSTR("backward-kill-word")	     },
	{ CXDR|'$',	counterinsert,	ECSTR("counter-insert")		     },
	{ CXDR|'+',	counterincr,	ECSTR("counter-incr")		     },
	{ CXDR|'-',	counterdecr,	ECSTR("counter-decr")		     },
	{ CXDR|'S',	counterset,	ECSTR("counter-set")	     	     },
	{ CXDR|'F',	counterformat,	ECSTR("counter-format")    	     },
#if	defined( _DOSPATH )
	{ SPCL|';',  	describekey,	ECSTR("apropos")		     },
	{ SPCL|'<',  	forwsearch,	ECSTR("forward-search")		     },
	{ SPCL|'=',  	getdefinition,	ECSTR("get-definition")		     },
	{ SPCL|'>',  	indentline,	ECSTR("re-indent-line")		     },
	{ SPCL|'?',  	switchscreen,	ECSTR("switch-screen-size")	     },
	{ SPCL|'4',  	completeword,	ECSTR("complete-word")		     },
	{ SPCL|'@',  	again,		ECSTR("repeat-last-command")	     },
	{ SPCL|'A',  	nexterror,	ECSTR("next-error")		     },
	{ SPCL|'B',  	filesave,	ECSTR("write-current-file")	     },
	{ SPCL|'C',  	ctlxe,		ECSTR("execute-keyboard-macro")	     },
	{ SPCL|'G',  	gotobob,	ECSTR("beginning-of-buffer")	     },
	{ SPCL|'H',  	backline,	ECSTR("previous-line")		     },
	{ SPCL|'I',  	backpage,	ECSTR("scroll-down")		     },
	{ SPCL|'K',  	backchar,	ECSTR("backward-char")		     },
	{ SPCL|'M',  	forwchar,	ECSTR("next-character")		     },
	{ SPCL|'O',  	gotoeob,	ECSTR("end-of-buffer")		     },
	{ SPCL|'P',  	forwline,	ECSTR("next-line")		     },
	{ SPCL|'Q',  	forwpage,	ECSTR("scroll-up")		     },
	{ SPCL|'R',  	instoggle,	ECSTR("insert-toggle")		     },
	{ SPCL|'S',  	forwdel,	ECSTR("delete-next-character")	     },
	{ SPCL|'k',  	exitemacs,	ECSTR("exit-emacs")		     },
	{ SPCL|'s',  	backword,	ECSTR("previous-word")		     },
	{ SPCL|'t',  	forwword,	ECSTR("next-word")		     },
	{ SPCL|'u',  	mvdnwind,	ECSTR("move-down-window")	     },
	{ SPCL|'w',  	mvupwind,	ECSTR("move-up-window")		     },
	{ SPCL|0x81, 	setmark,	ECSTR("set-mark-command")	     },
	{ SPCL|0x88, 	filesave,	ECSTR("write-current-file")	     },
#endif
#if	defined( _UNIX ) || defined( _VMS )
	{ SPCL|'1',  	gotobob,	ECSTR("beginning-of-buffer")	     },
	{ SPCL|'2',  	instoggle,	ECSTR("toggle-insert")		     },
	{ SPCL|'3',  	forwdel,	ECSTR("delete-next-character")	     },
	{ SPCL|'4',  	gotoeob,	ECSTR("end-of-buffer")		     },
	{ SPCL|'5',  	backpage,	ECSTR("scroll-down")		     },
	{ SPCL|'6',  	forwpage,	ECSTR("scroll-up")		     },
	{ SPCL|'A',  	backline,	ECSTR("previous-line")		     },
	{ SPCL|'B',  	forwline,	ECSTR("next-line")		     },
	{ SPCL|'C',  	forwchar,	ECSTR("next-character")		     },
	{ SPCL|'D',  	backchar,	ECSTR("backward-character")	     },
	{ SPCL|'U',	forwpage,	ECSTR("scroll-up")		     },
	{ SPCL|'V',	backpage,	ECSTR("scroll-down")		     },
	{ SPCL|'F',	gotoeob,	ECSTR("end-of-buffer")		     },
	{ SPCL|'H',	gotobob,	ECSTR("beginning-of-buffer")	     },
	{ SPCL|'L',	instoggle,	ECSTR("overwrite-mode")		     },
	{ SPCL|'Y',	gotoeob,	ECSTR("end-of-buffer")		     },
#endif
	{ BACKDEL,	backdel,	ECSTR("delete-previous-character")   },
	{ MEVT,		findwind,	ECSTR("find-window")		     },

	/*
	 *	unbound functions (called with M-x)
	 */

	{ UNBOUND,	ansitooem,	ECSTR("ansi-to-oem")		     },
	{ UNBOUND,	binaryfile,	ECSTR("binary-file")		     },
	{ UNBOUND,	comparewindows,	ECSTR("compare-windows")	     },
	{ UNBOUND,	compile,	ECSTR("compile")		     },
	{ UNBOUND,	emacsversion,	ECSTR("emacs-version")		     },
	{ UNBOUND,	exitemacs,	ECSTR("exit-emacs")		     },
	{ UNBOUND,	fillregion,	ECSTR("fill-region")		     },
	{ UNBOUND,	grep,		ECSTR("grep")			     },
	{ UNBOUND,	help,		ECSTR("help")			     },
	{ UNBOUND,	switchsgml,	ECSTR("sgml-mode")		     },
	{ UNBOUND,	justifycurline,	ECSTR("justify-current-line")        },
	{ UNBOUND,	killemacs,	ECSTR("kill-emacs")		     },
	{ UNBOUND,	mactoansi,	ECSTR("mac-to-ansi")		     },
	{ UNBOUND,	mactooem,	ECSTR("mac-to-oem")		     },
	{ UNBOUND,	man,		ECSTR("man")			     },
	{ UNBOUND,	matchlcur,	ECSTR("match-left-curly-bracket")    },
	{ UNBOUND,	matchrcur,	ECSTR("match-right-curly-bracket")   },
	{ UNBOUND,	oemtoansi,	ECSTR("oem-to-ansi")		     },
	{ UNBOUND,	printbuffer,	ECSTR("print-buffer")		     },
	{ UNBOUND,	perl,		ECSTR("perl")			     },
	{ UNBOUND,	switchperl,	ECSTR("perl-mode")		     },
	{ UNBOUND,	redrawscreen,	ECSTR("redraw-screen")		     },
	{ UNBOUND,	sed,		ECSTR("sed")			     },
	{ UNBOUND,	setjustifyleft,	ECSTR("set-justification-left")	     },
	{ UNBOUND,	setjustifyfull,	ECSTR("set-justification-full")      },
	{ UNBOUND,	uncompile,	ECSTR("uncompile-macro")	     },
	{ UNBOUND,	unlinkfile,	ECSTR("unlink-file")		     },
	{ UNBOUND,	utf8encoding,	ECSTR("utf8-encoding")		     },
	{ UNBOUND,	utf16encoding,	ECSTR("utf16-encoding")		     },
	{ UNBOUND,	systemencoding, ECSTR("system-encoding")	     },
	{ UNBOUND,	enterdebug,	ECSTR("enter-debug")		     }
};

#define NKEYTAB		(sizeof(keytab)/sizeof(keytab[0]))

/*
 *	Custom variables
 */

int	append_process_buffer	= NIL;
int	auto_encoding_mode	= T;
int	auto_fill_mode		= NIL;
int	background_color	= 0;
int	backup_before_writing	= T;
int	binary_mode		= NIL;
int	black_on_white		= NIL;
int	bold_font		= NIL;
int	case_sensitivity	= T;
int	compile_in_buffer	= T;
int	confirm_unsaved_buffer	= T;
int	date_completion		= T;
int	display_43l		= NIL;
int	display_command		= NIL;
int	fast_redisplay		= T;
int	fill_column		= 70;
int	foreground_color	= 7;
int	gnu_compatible		= T;
int	sgml_mode		= NIL;
int	latex_mode		= NIL;
int	line_number_mode	= T;
int	mouse_avoidance_mode	= T;
int	mouse_avoidance_nudge	= 3;
int	mouse_flag		= T;
int	monochrome_monitor	= NIL;
int	print_eval_loop		= T;
int	pipe_process		= T;
int	process_pending		= NIL;
int	screen_height		= 25;
int	screen_width		= 80;
int	set_show_graphic	= NIL;
int	show_menu		= T;
int	tab_display		= 8;
int	tab_size		= 8;
int	java_indent		= 2;
int	replace_mode		= NIL;
int	system_colors		= T;

EMCHAR	library[ NPAT ];		/* Library Path		*/
EMCHAR	as_arg[ NPAT ];			/* Assembler arguments	*/
EMCHAR	as_name[ NCMDN ];		/* Assembler name	*/
EMCHAR	cc_arg[ NPAT ];			/* Compiler arguments	*/
EMCHAR	cc_name[ NCMDN ];		/* Compiler name	*/
EMCHAR	fill_prefix[ NPAT ];		/* Fill prefix string	*/
EMCHAR	java_comp_args[ NPAT ];		/* Java compiler name	*/
EMCHAR	java_comp_name[ NCMDN ];	/* Java compiler args.	*/
EMCHAR	java_exec_args[ NPAT ];		/* Java exec name	*/
EMCHAR	java_exec_name[ NCMDN ];	/* Java exec args.	*/
EMCHAR	make_arg[ NPAT ];		/* Make args		*/
EMCHAR	make_name[ NCMDN ];		/* Make name		*/
EMCHAR	search_buffer[ NPAT ];		/* Main pattern		*/
EMCHAR	unicode_buffer[ NPAT ];		/* Main pattern		*/
EMCHAR	helpfile1[ NPAT ];		/* help file 1 (F11)	*/
EMCHAR	helpfile2[ NPAT ];		/* help file 2 (F12)	*/
EMCHAR	helpfile3[ NPAT ];		/* help file 3 (Alt-F11)*/
EMCHAR	helpfile4[ NPAT ];		/* help file 4 (Alt-F12)*/

static	VARTAB	vartab[] = {
 { &append_process_buffer,	ECSTR("append-process-buffer"),       BOOLEAN },
 { &as_arg[ 0 ], 	  	ECSTR("assembler-arguments"),	      NPAT    },
 { &as_name[ 0 ], 	  	ECSTR("assembler-name"),	      NCMDN   },
 { &auto_encoding_mode,	  	ECSTR("auto-encoding-mode"),	      BOOLEAN },
 { &auto_fill_mode, 	  	ECSTR("auto-fill-mode"),	      BOOLEAN },
 { &background_color, 	  	ECSTR("background-color"),	      FIXVAL  },
 { &backup_before_writing,	ECSTR("backup-before-writing"),       BOOLEAN },
 { &binary_mode, 	  	ECSTR("binary-mode"),		      BOOLEAN },
 { &black_on_white, 	  	ECSTR("black-on-white"),	      BOOLEAN },
 { &bold_font, 		  	ECSTR("bold-font"),		      BOOLEAN },
 { &case_sensitivity, 	  	ECSTR("case-fold-search"),	      BOOLEAN },
 { &compile_in_buffer, 	  	ECSTR("compile-in-buffer"),	      BOOLEAN },
 { &confirm_unsaved_buffer,	ECSTR("confirm-unsaved-buffer"),      BOOLEAN },
 { &cc_arg[ 0 ],	  	ECSTR("compiler-arguments"),	      NPAT    },
 { &cc_name[ 0 ],	  	ECSTR("compiler-name"),	              NCMDN   },
 { &date_completion,	  	ECSTR("date-completion"),	      BOOLEAN },
 { &display_43l,	  	ECSTR("display-43-lines"),	      BOOLEAN },
 { &display_command,	  	ECSTR("display-command"),	      BOOLEAN },
 { &fast_redisplay,	  	ECSTR("fast-redisplay"),	      BOOLEAN },
 { &foreground_color,	  	ECSTR("foreground-color"),	      FIXVAL  },
 { &fill_column,	  	ECSTR("fill-column"),		      FIXVAL  },
 { &fill_prefix[ 0 ],	  	ECSTR("fill-prefix"),		      NPAT    },
 { &gnu_compatible,	  	ECSTR("gnu-compatible"),	      BOOLEAN },
 { &helpfile1[ 0 ],	  	ECSTR("help-file1"),		      NPAT    },
 { &helpfile2[ 0 ],	  	ECSTR("help-file2"),		      NPAT    },
 { &helpfile3[ 0 ],	  	ECSTR("help-file3"),		      NPAT    },
 { &helpfile4[ 0 ],	  	ECSTR("help-file4"),		      NPAT    },
 { &case_sensitivity,	  	ECSTR("isearch-toggle-case-fold"),    BOOLEAN },
 { &java_comp_args[ 0 ],  	ECSTR("java-compiler-arguments"),     NCMDN   },
 { &java_comp_name[ 0 ],  	ECSTR("java-compiler-name"),	      NCMDN   },
 { &java_exec_args[ 0 ],  	ECSTR("java-executable-arguments"),   NCMDN   },
 { &java_exec_name[ 0 ],  	ECSTR("java-executable-name"),        NCMDN   },
 { &java_indent,	  	ECSTR("java-indent"),		      FIXVAL  },
 { &line_number_mode,	  	ECSTR("line-number-mode"),	      BOOLEAN },
 { &latex_mode,		  	ECSTR("latex-mode"),		      BOOLEAN },
 { &make_arg[ 0 ],	  	ECSTR("make-arguments"),	      NPAT    },
 { &make_name[ 0 ],	  	ECSTR("make-name"),		      NCMDN   },
 { &monochrome_monitor,	  	ECSTR("monochrome-monitor"),	      BOOLEAN },
 { &mouse_flag,		  	ECSTR("mouse-flag"),		      BOOLEAN },
 { &mouse_avoidance_mode, 	ECSTR("mouse-avoidance-mode"),        BOOLEAN },
 { &mouse_avoidance_nudge,	ECSTR("mouse-avoidance-nudge-var"),   FIXVAL  },
 { &print_eval_loop,	  	ECSTR("print-eval-loop"),	      BOOLEAN },
 { &pipe_process,	  	ECSTR("pipe-process"),	      	      BOOLEAN },
 { &replace_mode,	  	ECSTR("replace-mode"),	              BOOLEAN },
 { &screen_height,	  	ECSTR("screen-height"),	              FIXVAL  },
 { &screen_width,	  	ECSTR("screen-width"),	              FIXVAL  },
 { &search_buffer[ 0 ],	  	ECSTR("search-buffer"),	              NPAT    },
 { &set_show_graphic,	  	ECSTR("set-show-graphic"),	      BOOLEAN },
 { &show_menu,		  	ECSTR("show-menu"),		      BOOLEAN },
 { &system_colors,	  	ECSTR("system-colors"),	              BOOLEAN },
 { &tab_display,	  	ECSTR("tabulation-display"),	      FIXVAL  },
 { &tab_size,		  	ECSTR("tabulation-size"),	      FIXVAL  }
};

#define	NVARTAB		(sizeof(vartab)/sizeof(vartab[0]))

/*
 *	Fill widgets with default behavior
 */

WIDGET	widget = {
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

EMPRINT	printer = {
	mllpprint
};

static	int	clast = 0;	/* last executed command	*/
static	int	nlast = 1;	/* last executed repeat count	*/
static	int	isoset;		/* ISO 8859-1 char set		*/

EXTERN_C int
emacsascii( int argc, char *argv[] )
{
	EMCHAR **argvu = (EMCHAR **)malloc(((size_t)argc+1) * sizeof(EMCHAR *));
	int	i;
	int	res;

	for( i = 0 ; i < argc ; ++i ) {
		argvu[ i ] = (EMCHAR *)emnewstring( argv[ i ] );
	}

	argvu[ i ] = NULL;

	aargv = (char **)argv;
	res   = emacs( argc, argvu );
	
	for( i = 0 ; i < argc ; ++i ) {
		free( argvu[ i ] );
	}

	free( argvu );
	return( res );
}

EXTERN_C int
emacs( int argc, EMCHAR *argv[] )
{
	int	curarg   = 1;
	int	lineinit = 1;
	int	i	 = 0;
	EMCHAR	bname[ NBUFN ];

	pterm	 = &term;
	eargc	 = argc;
	eargv	 = argv;
	lastflag = 0;
	editflag = T;

#if	defined( _WINDOWS_SOURCE ) || defined( UNIX )
	isoset	 = T;
#else
	isoset	 = NIL;
#endif

	if( eargc > curarg )
		if( linenump( eargv[ curarg ] ) ) {
			lineinit = emstrtoi( eargv[ curarg ] );
			curarg++;
		}

	if( !initflag ) {
		nkeytab	= NKEYTAB;
		nvartab	= NVARTAB;

		pkeytab	= (KEYTAB *)&keytab[ 0 ];
		pvartab	= (VARTAB *)&vartab[ 0 ];
		pmactab	= (MACTAB *)&mactab[ 0 ];

		(void)emstrcpy( bname,          BUF_SCRATCH	);
		(void)emstrcpy( cc_name,        ECSTR("cc")	);
		(void)emstrcpy( java_comp_name, ECSTR("javac")	);
		(void)emstrcpy( java_exec_name, ECSTR("java")	);
		(void)emstrcpy( as_name,        ECSTR("masm")	);
		(void)emstrcpy( make_name,      ECSTR("make")	);
		(void)emstrcpy( library,        PATH		);

		search_buffer[ 0 ]  = '\000';
		unicode_buffer[ 0 ] = '\000';

		(void)customize();

		background_color &= 0x07;
		foreground_color &= 0x07;

		TTYopen();

		kbdm[ 0 ] = -1;

		vtinit();
		edinit( bname );

		if( eargc > curarg ) {
			i++;
			update( T );
			(void)newfile( eargv[ curarg++ ] );
		}

		if( eargc > curarg ) {
			i++;
			(void)splitwind();
			(void)newfile( eargv[ curarg++ ] );
			(void)prevwind();
		}

		while( eargc > curarg ) {
			i++;
			(void)newfile( eargv[ curarg++ ] );
		}

		(void)connectwindow( curwp, curbp );

		if( i > 2 ) {
			(void)listbuffers();
			(void)prevwind();
		}

		/*
		 *  Search for a Lisp macro named 'emacs-init'
		 *  to execute at startup.
		 */

		for( i = 0 ; i < nmactab ; i++ )
			if( MACname(i) &&
			    emstrcmp(MACname(i),ECSTR("emacs-init"))==0 ) {
				(void)internaleval( i );
				break;
			}

		initflag  = 1;
	} else	{
		(void)customize();

		background_color &= 0x07;
		foreground_color &= 0x07;

		TTYopen();
		(void)redrawscreen();
		update( T );
		if( eargc > curarg )
			(void)newfile( eargv[ curarg ] );
	}

	if( lineinit > 1 ) {
		repeat = lineinit;
		(void)gotobob();
		(void)gotoline();
	}

#if	!defined( _WINGUI )
	if( eargc == 1 )
		emacsversion();
#endif
#if	defined( _OPENLISP )
#if	defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4312 )
#endif

	/*
	 *	Install an error handler and run editloop.
	 */

	OLENTERLISP;
	while( editflag == T ) {
		if( olcatchonstack("EmACT") == 0) {
			/*
			 *	Install handler to catch evaluation errors.
			 */
			olpush( olerror_t );
			oladjstk( -HEADER_ENV_SIZE );
			olxspmov( 0 ) = catchtag;
			olxspmov( 1 ) = olerror_t;
			olxspmov( 2 ) = olmakefix( 0 );
			olxspmov( 3 ) = olenv;
#if	(HEADER_ENV_SIZE > 4 )
			olxspmov( 4 ) = NULLPTR;
#endif
			editloop();
		} else	{
			WDGwrite(
				  ECSTR("OpenLisp internal error ID#%x : %s : '%s'."),
				  olerrtype,
				  olerrmsg[ olerrtype ],
				  olerrfun
				);
		}

		oladjstk( olenvsize + HEADER_ENV_SIZE + 1 );
	}
	OLLEAVELISP;

#if	defined( _MSC_VER )
#pragma warning( pop )
#endif
#else
	editloop();
#endif

	return( 0 );
}

/*
 *	Initialize all of the buffers and windows. The buffer name is
 *	passed down as an argument, because the main routine may have
 *	been told to read in a file by default.
 */

static	void
edinit( EMCHAR *bname )
{
	BUFFER *bp;
	WINSCR *wp;

	bp = bfind( bname, T, NIL, LISPMODE );	   /* First buffer	*/
	wp = (WINSCR *)malloc( sizeof( WINSCR ) ); /* First window	*/

	if( bp == NULL || wp == NULL )
		exit( 0 );

	wheadp		= wp;
	wp->w_wndp	= NULL;			   /* Initialize window	*/
	wp->w_toprow	= 0;
	wp->w_ntrows	= ((int)TTYnrow-1);
	wp->w_bufp	= (BUFFER *)NULL;
	wp->w_flag      = 0;
	wp->w_force     = 0;
	curwp		= wp;

	(void)connectwindow( wp, bp );
}

/*
 *	Infinite  loop  that  read a character and execute associated
 *	command.
 */

static void
editloop( void )
{
	while( editflag ) {
		int	mflag;
		int	c;
		int	n;

		update( T );

		if( (c = getkey()) == (Ctrl|'U') ) {
			mflag	= 0;
			n	= 4;
			WDGwrite( ECSTR("Arg: 4") );
			while(((c=getkey())>='0' && c<='9') || c==(Ctrl|'U') ) {
				if( c == (Ctrl|'U') )
					n *= 4;
				else	{
					if( !mflag ) {
						n = 0;
						mflag = 1;
					}
					n = 10*n + c - '0';
				}
				WDGwrite( ECSTR("Arg: %d"), n );
			}
		} else	n = 1;

		if( kbdmip != NULL ) {		/* Save macro strokes.	*/
			if( c != (CTLX|')') && kbdmip > &kbdm[NKBDM-6] ) {
				WDGwrite( ECSTR("Macro buffer is full.") );
				(void)ctrlg();
				continue;
			}
			if( n > 1 ) {
				*kbdmip++ = (Ctrl|'U');
				*kbdmip++ = n;
			}
			*kbdmip++ = c;
		}

		if( mpresf != NIL ) {
			mlerase();
			update( T );
		}

		if( c == (SPCL|'@') || c == (CTLX|METACH) ) {
			c = clast;
			n = nlast;
		} else	{
			clast = c;
			nlast = n;
		}

		(void)execute( c, n );
	}

}

/*
 *	This is the general command execution routine. It handles the
 *	fake binding of all the keys to "self-insert". It also clears
 *	out  the  "thisflag"  word,  and arranges to move it  to  the
 *	"lastflag",  so that the next command can look at it.  Return
 *	the status of command.
 */

int
execute( int c, int n )
{
	KEYTAB	*ktp;
	int	status;
	int	i;

	repeat  = n;
	frdflag = NIL;		/* check is made only once for each command */

	if( c > MAX_EMCHAR || c == BACKDEL ) {

		/* Look in macro table.	*/

		for( i = 0 ; i < nmactab ; i++ ) {
			if( MACcode( i ) == c ) {
				if( display_command == T )
					WDGwrite( ECSTR("%s"), MACname( i ) );
				thisflag = 0;
				status	 = internaleval( i );
				lastflag = thisflag;
				return( status );
			}
		}

		/* Look in key table.	*/

		for( ktp = pkeytab ; ktp <= (pkeytab+NKEYTAB-1) ; ktp++ )
			if( ktp->k_code == c ) {
				if( display_command == T )
					WDGwrite( ECSTR("%s"), ktp->k_name );
				if( c & SPCL )
					thisflag = CFFKEY;
				else	thisflag = 0;
				status	 = (*ktp->k_fp)();
				lastflag = thisflag;
				return( status );
			}

		if( c != (Ctrl|'Q') && c != (Ctrl|'S') )
			return( ctrlg() );

	}

	if( self_insert( c ) ) {
		if( n <= 0 ) {
			lastflag = 0;
			return( repeat < 0 ? NIL : T );
		}
		thisflag = 0;

#if	defined( _UNIX ) || defined( _VMS )
		/*
		 *	Check for F-KEY ^[[?~
		 */

		if( (c == '~') && (lastflag & CFFKEY) ) {
			lastflag = 0;
			return( T );
		}
#endif
		if( (c=='}')&&
	            (curwp->w_emode==CMODE      ||
	             curwp->w_emode==CPPMODE    ||
	             curwp->w_emode==CSHARPMODE ||
	             curwp->w_emode==PERLMODE   ||
		     curwp->w_emode==JAVAMODE)) {
			status	 = unindent( c, T );
			lastflag = thisflag;
			return( status );
		}

		if( c > 0x7F && (latex_mode==T || curwp->w_emode==SGMLMODE) )
			status	= latexinsert( repeat, c );
		else if( replace_mode == NIL )
			status	= linsert( repeat, c );
		else	status	= lreplace( repeat, c );

		lastflag = thisflag;

		if( !kbdmop && (c == ')' || c == '}' || c == ']') &&
		    curwp->w_emode!=FUNDAMENTAL)
			return( automatch( c, T ) );

		if( !kbdmop && c == '>' && curwp->w_emode == SGMLMODE )
			return( automatch( c, T ) );

		if( !kbdmop && auto_fill_mode == T )
			if( c==' ' && lposition(curwp->w_dotp)>fill_column ) {
				(void)setmark();
				(void)splitlinetofill();
				(void)swapmark();
				(void)gotoeol();
			}

		return( status );
	}
	lastflag = 0;
	return( NIL );
}

static CMD
again( void )
{
	return( execute( clast, nlast ) );
}

/*
 *	Read in a key. Do the standard keyboard preprocessing.
 */

int
getkey( void )
{
	int	c = TTYgetc();

	switch( c ) {

#if	defined( _UNIX ) || defined( _VMS )
	case METACH :
		WDGwrite( ECSTR("M-") );
		if( (c = getctl()) == (Ctrl|'G') )
			return( c );

		if( c == '[' || c == 'O' )
			return( SPCL | TTYgetc() );
		else	return( META | c );
	case CSICODE :
	case SS3CODE :
		return( SPCL | TTYgetc() );

#else
	case METACH :		/* M- is prefix		*/
		WDGwrite( ECSTR("M-") );
		if( (c = getctl()) == (Ctrl|'G') ) {
			return( c );
		} else	{
			return( META | c );
		}
#endif

	case CTLXCH :		/* ^X is a prefix	*/
		WDGwrite( ECSTR("C-x-") );
		if( (c = getctl()) == (Ctrl|'G') )
			return( c );

		if( c == '$' ) {
			WDGwrite( ECSTR("C-x-$-") );
			if( (c = getctl()) == (Ctrl|'G') )
				return( c );
			return( CXDR | c );
		}

		return( CTLX | c );

	case CTLCCH :		/* ^C is a prefix	*/
		WDGwrite( ECSTR("C-c-") );
		if( (c = getctl()) == (Ctrl|'G') )
			return( c );
		else	return( CTLC | c );

	default :		/* C0 control -> C-	*/
#if	defined( __MVS__ )
		if( c == 0 )
			c = Ctrl | '@';
		else if( c >= 1 && c <= 9 )
			c = Ctrl | (c+'A'-1);
		else if( c >= 10 && c <= 19 )
			c = Ctrl | (c+'J'-1);
		else if( c >= 20 && c <= 26 )
			c = Ctrl | (c+'T'-1);
#else
		if( c >= 0x00 && c <= 0x1F )
			c = Ctrl | (c+'@');
#endif
	}

	return( c );

}

CMD
emacsversion( void )
{
#if	defined( _UNICODE )
	WDGwrite(
		  ECSTR("%s (%s / UNICODE - Build: %d) of %s - C. Jullien."),
		  version,
		  SYSTEMNAME,
		  EMBUILD,
#if	defined( TEXT )
		  TEXT(__DATE__)
#else
		  ECSTR("unknown")
#endif
		);
#else
	WDGwrite(
		  ECSTR("%s (%s / ANSI - Build: %d) of %s - C. Jullien."),
		  version,
		  SYSTEMNAME,
		  EMBUILD,
		  __DATE__
		);
#endif
	return( T );
}

/*
 *	Get a key. Apply control modifications to the read key.
 */

static	int
getctl( void )
{
	int	c = TTYgetc();

#if	defined( __MVS__ )
	if( c == 0 )
		return( Ctrl | '@' );
	else if( c >= 1 && c <= 9 )
		return( Ctrl | (c+'A'-1) );
	else if( c >= 10 && c <= 19 )
		return( Ctrl | (c+'J'-10) );
	else if( c >= 20 && c <= 26 )
		retrun( Ctrl | (c+'S'-20) );
	else	return( c );
#else
	if( c == METACH ) {
		return( c );
	} else	if( c >= 0x00 && c <= 0x1F ) {
		return( Ctrl | (c+'@') );
	}
#endif

	if( isalpha( c ) && islower( c) )	/* Force to upper	*/
		c = toupper( c );
	return( c );
}

/*
 *      CAUTION !! Hard exit quit Emacs without saving and without warning
 *	Not bound.
 */

CMD
killemacs( void )
{
	vttidy();
	editflag = NIL;
	return( T );
}

/*
 *	Quit command.  If an argument, always quit. Otherwise confirm
 *	if  a buffer has been changed and not written  out.  Normally
 *	bound to "C-X C-C".
 */

CMD
exitemacs( void )
{
	static EMCHAR *msg = ECSTR("Modified buffers exist; exit anymawy? ");

	CMD	res = anycb( ANYCB_PROMPT );

	if( res == ABORT )
		return( NIL );

	if( confirm_unsaved_buffer == T ) {
		if( res == NIL || WDGyesno( msg ) == T )
			return( killemacs() );
	} else	if( res == NIL || WDGyn( msg ) == T )
			return( killemacs() );

	return( NIL );
}

/*
 *	Begin  keyboard  macro.   Error  if not at the top level in
 *	keyboard processing. Set up variables and return.
 */

CMD
ctlxlp( void )
{
	if( kbdmip != NULL || kbdmop != NULL ) {
		WDGmessage( ECSTR("Already defining kbd macro.") );
		return( NIL );
	}
	WDGplay( NIL );
	WDGmessage( ECSTR("Defining kbd macro...") );
	kbdmip = &kbdm[0];
	return( T );
}

/*
 *	End  keyboard  macro.  Check for the same limit conditions as
 *	the above routine. Set up the variables and return.
 */

CMD
ctlxrp( void )
{
	if( kbdmip == NULL ) {
		WDGmessage( ECSTR("Not defining a kbd macro.") );
		return( NIL );
	}
	WDGplay( T );
	WDGmessage( ECSTR("Keyboard macro defined.") );
	kbdmip = NULL;
	return( T );
}

/*
 *	Execute a macro.  The command argument is the number of times
 *	to loop.  Quit as soon as a command gets an error.  Returns T
 *	if all ok, else NIL.
 */

CMD
ctlxe( void )
{
	int	n = repeat;
	int	c;
	int	an;
	int	s;
	int	save;

	if( kbdm[ 0 ] == -1 ) {
		WDGmessage( ECSTR("No keyboard macro to execute.") );
		return( NIL );
	}

	if( kbdmip != NULL || kbdmop != NULL ) {
		WDGmessage(ECSTR("You can't call the keyboard macro while defining it."));
		return( NIL );
	}

	s = T;

	do	{
		save	= repeat;
		kbdmop	= &kbdm[0];
		do
			if( (c = *kbdmop++) == (Ctrl|'U') ) {
				an = *kbdmop++;
				c  = *kbdmop++;
			} else	an = 1;
		while( c != (CTLX|')') && (s=execute( c, an )) == T );
		kbdmop	= NULL;
		repeat	= save;
	} while( s == T && --n );

	return( s );
}

/*
 *	Abort,  beep the beeper. Kill off any keyboard macro, etc.,
 *	that is in progress.   Sometimes called as a routine, to do
 *	general aborting of stuff.
 */

CMD
ctrlg( void )
{
	TTYbeep();

	if( kbdmip != NULL ) {
		kbdm[0] = (CTLX|')');
		kbdmip	= NULL;
	}

	return( ABORT );
}

/*
 *	Insert unicode character from code
 */

CMD
insertunicode( void )
{
	int	c;
	EMCHAR	buf[ 2 ];

	if( WDGedit(ECSTR("Insert unicode from code: "),
		    unicode_buffer, NLINE) == ABORT )
		return( NIL );

	c = (EMCHAR)emstrtoi( unicode_buffer );

	if( c < 0 || c > MAX_EMCHAR ) {
		WDGerror(ECSTR("Code not in range [0, MAX_EMCHAR]"));
		return( NIL );
	}

	buf[ 0 ] = (EMCHAR)c;
	buf[ 1 ] = '\000';
	WDGwrite( ECSTR("Unicode='%s', code(%d, 0x%x)"), buf, c, c );
	linsert( repeat, c );
	return( T );
}

/*
 *	Switch between binary and text mode.
 */

CMD
binaryfile( void )
{
	int	newmode;

	if( curwp->w_binary == T )
		newmode = NIL;
	else	newmode = T;

	curbp->b_binary = newmode;

	lchange( WFEDIT );
	updatemodes();
	return( T );
}

/*
 *	Switch to utf8 encoding. (requires UNICODE version).
 */

CMD
utf8encoding( void )
{
#if	defined( _UNICODE )
	curbp->b_wide = EMUTF8;

	lchange( WFEDIT );
	updatemodes();

	return( T );
#else
	WDGwrite( ECSTR("This command requires an UNICODE enabled version.") );
	return( NIL );
#endif
}

/*
 *	Switch to utf16 encoding. (requires UNICODE version).
 */

CMD
utf16encoding( void )
{
#if	defined( _UNICODE )
	curbp->b_wide = EMUTF16;

	lchange( WFEDIT );
	updatemodes();
	return( T );
#else
	WDGwrite( ECSTR("This command requires an UNICODE enabled version.") );
	return( NIL );
#endif
}

/*
 *	Switch to system default encoding.
 */

CMD
systemencoding( void )
{
#if	defined( _UNICODE )
	curbp->b_wide = EMASCII;

	lchange( WFEDIT );
	updatemodes();
	return( T );
#else
	/*
	 * this is the default behavior for on-UNICODE version?
	 */
	return( T );
#endif
}

/*
 *	Switch back to fundamental mode.
 */

CMD
switchfund( void )
{
	curbp->b_emode = FUNDAMENTAL;
	updatemodes();
	return( T );
}

/*
 *	Switch to C mode. Active auto match-parentheses.
 */

CMD
switchcc( void )
{
	curbp->b_emode = CMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to C++ mode. Active auto match-parentheses.
 */

CMD
switchcpp( void )
{
	curbp->b_emode = CPPMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to Java mode. Active auto match-parentheses.
 */

CMD
switchjava( void )
{
	curbp->b_emode = JAVAMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to Fortran mode. Active auto match-parentheses.
 */

CMD
switchfortran( void )
{
	curbp->b_emode = FORTRANMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to Lisp mode. Active auto match-parentheses.
 */

CMD
switchlisp( void )
{
	curbp->b_emode = LISPMODE;
	curbp->b_binary = T;
	updatemodes();
	return( T );
}

/*
 *	Switch to Perl mode. Active auto match-parentheses.
 */

CMD
switchperl( void )
{
	curbp->b_emode  = PERLMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to SGML mode.
 */

CMD
switchsgml( void )
{
	curbp->b_emode = SGMLMODE;
	sgml_mode      = T;
	auto_fill_mode = T;
	latex_mode     = NIL;

	updatemodes();
	return( T );
}

/*
 *	Switch to Prolog mode. Active auto match-parentheses.
 */

CMD
switchprolog( void )
{
	curbp->b_emode = PROLOGMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to assembler mode.
 */

CMD
switchas( void )
{
	curbp->b_emode = ASMODE;
	updatemodes();
	return( T );
}

/*
 *	Switch to Shell mode. Active auto match-parentheses.
 */

CMD
switchshell( void )
{
	curbp->b_emode  = SHELLMODE;
	curbp->b_binary = T;
	updatemodes();
	return( T );
}

/*
 *	Returns 1 if c is a separator, 0 otherwise.
 */

int
separatorp( int c )
{
	return( (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') );
}

int
charp( int c )
{
	return( isalnum( c ) || (c == '-') || (c == '_') || (c == '+') );
}

static	int
linenump( EMCHAR *s )
{
	if( *s == '+' )
		s++;

	while( *s && isdigit( (int)*s ) )
		s++;

	return( !*s );
}

/*
 *	Display an internal error.
 */

void
emacserror( EMCHAR *msg, const char *file, int n )
{
	TTYbeep();
	if( file != NULL || n != 0 )
	     WDGwrite(ECSTR("Internal error '%s' file %s at line %d."),
		      msg, file, n);
	else WDGwrite(ECSTR("Internal error '%s'."), msg );
	TTYgetc();
}

/*
 *	Given a 'foreign' character, replace by its LaTeX equivalent.
 */

static CMD
latexinsert( int n, int c )
{
	int	i;

	static	struct	{
		int	oem;
		int	iso8859;
		EMCHAR	*latex;
		EMCHAR	*sgml;
	} convtab[] = {
	/*
	 *	  OEM   ISO   LaTeX       SGML
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

	for( i = 0 ; i < (int)(sizeof(convtab) / sizeof(convtab[ 0 ])) ; i++ )
		if( c == (isoset ? convtab[ i ].iso8859 : convtab[ i ].oem) ) {
			EMCHAR	*p;

			if( curwp->w_emode == SGMLMODE )
				while( n-- > 0 )
					for( p = convtab[ i ].sgml ; *p ; p++ )
						(void)linsert( 1, *p );
			else	while( n-- > 0 )
					for( p = convtab[ i ].latex ; *p ; p++ )
						(void)linsert( 1, *p );

			return( T );
		}

	return( linsert( n, c ) );

}
