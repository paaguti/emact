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

#if !defined(__EMACS_H)
#define __EMACS_H

/*
 * This  file  is  the  general header file for all parts of the
 * EMACS   display  editor.  It  contains  definitions  used  by
 * everyone,  and  it  contains  the  stuff  you have to edit to
 * create  a  version  of  the  editor  for a specific operating
 * system and terminal.
 */

#if defined(_MSC_VER)
#define _CRT_SECURE_NO_DEPRECATE        1
#define _CRT_NONSTDC_NO_DEPRECATE       1
#define _CRT_NON_CONFORMING_SWPRINTFS   1
#endif

#if defined(_WIDECHARS)
#if !defined(_UNICODE)
#define _UNICODE
#endif
#if !defined(UNICODE)
#define UNICODE
#endif
#endif

#if defined(UNICODE) && !defined(_WIDECHARS)
#define _WIDECHARS
#endif

#if defined(_WINDEBUG)
#if !defined(_DEBUG)
#define _DEBUG
#endif
#include <crtdbg.h>
#endif

/*
 * START OF INCLUDE SECTION
 */

#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <cstdarg>
#include <cstdint>
#include <ctime>
#include <cassert>
#include <cstring>
#include <climits>

#include <algorithm>
#include <vector>
#include <sstream>
#include <list>
#include <array>
#include <limits>
#include <memory>

#if defined(HAVE_CONFIG_H)
#include "./config.h"

/*
POSIX.1-1988  _POSIX_VERSION = 198808L  
POSIX.1-1990  _POSIX_VERSION = 199009L    ISO/IEC 9945-1:1990 
POSIX.2       _POSIX2_C_VERSION = 199209L ISO/IEC 9945-2:1993 
POSIX.1b-1993 _POSIX_VERSION = 199309L    IEEE 1003.1b-1993 
POSIX.1-1996  _POSIX_VERSION = 199506L    IEEE 1003.1-1996 
POSIX.1-2001  _POSIX_VERSION = 200112L    IEEE 1003.1-2001 
POSIX.1-2008  _POSIX_VERSION = 200809L    IEEE 1003.1-2008 
*/

#if !defined(_POSIX_C_SOURCE)
// #define _POSIX_C_SOURCE 199506L
// #define _POSIX_C_SOURCE 199808L
// #define _POSIX_C_SOURCE 200809L
#define _POSIX_C_SOURCE 200112L
// #define _POSIX_C_SOURCE 200809L
#endif

#if !defined(X_DISPLAY_MISSING)
#define _X11
#endif
#endif

#if defined(_POSIX_C_SOURCE)
#include <unistd.h>
#define _SPAWNED_PIPE
#endif  /* _POSIX_C_SOURCE */

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
using ENTRY = struct dirent;

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#include <windows.h>
#include <io.h>
#include <direct.h>

#define popen   _popen
#define pclose  _pclose
#define chdir   _chdir
#define getcwd  _getcwd
#define stat    _stat
#define S_IWUSR S_IWRITE
#define S_IRUSR S_IREAD

using mode_t = int;

#define _SPAWNED_PIPE
#define _DOSPATH
#endif  /* _WIN32 */

/*
 *      END OF INCLUDE SECTION
 */

#if !defined(S_ISDIR)
#define S_ISDIR(mode)         (((mode) & S_IFMT) == S_IFDIR)
#endif

#if !defined(S_ISREG)
#define S_ISREG(mode)         ((mode) & S_IFREG)
#endif

using EMSTAT = struct stat;

#include "./CharType.h"

/*
 * Get the system name
 */

#if defined(__APPLE__) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("macOS")
#endif

#if defined(_POSIX_C_SOURCE) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("Posix")
#endif

#if defined(unix) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("unix")
#endif

#if defined(_WIN64) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("win64")
#endif

#if defined(_WIN32) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("win32")
#endif

#if defined(_WINDOWS) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("Windows")
#endif

#if !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("unknown")
#endif

#if defined(_DOSPATH) && !defined(PATH)
#define PATH    ECSTR("c:/usr/lib/emacs")
#endif

#if !defined(PATH)
#define PATH    ECSTR("/usr/local/emact/lib")
#endif

/*
 * Constant definition :
 */

static constexpr auto NFILEN(FILENAME_MAX); // # of bytes, ISO file name
static constexpr auto NCMDN(16);            // # of bytes, command name
static constexpr auto NMAX(64);             // # of macros
static constexpr auto MAXLINE(0x8000);      // # of bytes, line in read
static constexpr auto NLINE(256);           // # of bytes, line (internal)
static constexpr auto NPAT(80);             // # of bytes, pattern

static constexpr auto METACH(0x1B);         // M- prefix, Control-[, ESC

#if defined(max)
#undef max
#endif

static constexpr int MAX_EMCHAR(std::numeric_limits<EMCHAR>::max());

#if defined(UNICODE)
#define _prefix(x)      ((x) << 16)
#else
#define _prefix(x)      ((x) << 8)
#endif

static constexpr auto Ctrl(_prefix(1 << 0));    // Control flag, or'ed in
static constexpr auto META(_prefix(1 << 1));    // Meta flag, or'ed in
static constexpr auto CTLX(_prefix(1 << 2));    // ^X flag, or'ed in
static constexpr auto SPCL(_prefix(1 << 3));    // Function Key (PC and Clones)
static constexpr auto CTLC(_prefix(1 << 4));    // ^C flag, or'ed in
static constexpr auto MEVT(_prefix(1 << 5));    // Mouse click
static constexpr auto CXDR(_prefix(1 << 6));    // ^X$ prefix
static constexpr auto UNBOUND(_prefix(1 << 7)); // Unbound

static constexpr auto FIOSUC(0x00);  // File I/O, success.
static constexpr auto FIOFNF(0x10);  // File I/O, file not found.
static constexpr auto FIOEOF(0x20);  // File I/O, end of file.
static constexpr auto FIOERR(0x30);  // File I/O, error.
static constexpr auto FIOWNL(0x40);  // File I/O, end without <NL>
static constexpr auto FIOENC(0x50);  // File I/O, encoding error

static constexpr auto CFUNSET(0);      // Last command was unknown
static constexpr auto CFCPCN(1 << 0);  // Last command was C-P, C-N
static constexpr auto CFKILL(1 << 1);  // Last command was a kill
static constexpr auto CFMMOV(1 << 2);  // Last command needs mouse move
static constexpr auto CFFSRC(1 << 3);  // Last command was C-S
static constexpr auto CFBSRC(1 << 4);  // Last command was C-R
static constexpr auto CFTAB(1  << 5);  // Last command was TAB
static constexpr auto CFCOMP(1 << 6);  // Last command was compare-win
static constexpr auto CFCPLT(1 << 7);  // Last command was complete
static constexpr auto CFFKEY(1 << 8);  // Last command was fn-key
static constexpr auto CFFAIL(1 << 9);  // Last command has faild

static constexpr auto DIREDMARK(2);   // Two characters to mark dired

/*
 * Flags for functions that deal with directories
 */

static constexpr auto SLASH(0x0000);    // Normalize with trailing /
static constexpr auto NOSLASH(0x0001);  // Normalize with no trailing /

/*
 * Flags for functions that deal with external commands
 */

static constexpr auto SYSCOMP_NOERROR(0x0000); // Call system and no error
static constexpr auto SYSCOMP_ERRORS(0x0001);  // Call system and check errors

/*
 * List of internal buffer names
 */

static constexpr auto BUF_LIST(ECSTR("*Buffer List*"));
static constexpr auto BUF_HELP(ECSTR("*Help*"));
static constexpr auto BUF_PROC(ECSTR("*Process*"));
static constexpr auto BUF_DIR(ECSTR("*Directory*"));
static constexpr auto BUF_SCRATCH(ECSTR("*scratch*"));

static constexpr auto BFCHG(0x01);         // Changed since last write

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

static inline bool
self_insert(int c) {
  return (((unsigned int)c & MAX_EMCHAR) >= 0x20 && c != 0x7F);
}

/*
 * Configurable variables:
 */

namespace opt {
extern EMCHAR as_name[NCMDN];         // Assembler name
extern EMCHAR as_arg[NPAT];           // Assembler argument
extern EMCHAR cc_arg[NPAT];           // Compiler argument
extern EMCHAR cc_name[NCMDN];         // Compiler name
extern EMCHAR fill_prefix[NPAT];      // Fill prefix string
extern EMCHAR java_comp_args[NPAT];   // Java compiler argument
extern EMCHAR java_comp_name[NCMDN];  // Java compiler name
extern EMCHAR java_exec_args[NPAT];   // Java executable argument
extern EMCHAR java_exec_name[NCMDN];  // Java extern name
extern EMCHAR make_arg[NPAT];         // Make argument
extern EMCHAR make_name[NCMDN];       // Make name
extern EMCHAR helpfile1[NPAT];        // help file 1
extern EMCHAR helpfile2[NPAT];        // help file 2
extern EMCHAR helpfile3[NPAT];        // help file 1
extern EMCHAR helpfile4[NPAT];        // help file 2

extern bool   append_process_buffer;  // Append in process buffer
extern bool   auto_fill_mode;         // Auto fill mode flag
extern bool   auto_encoding_mode;     // Auto encoding mode flag
extern int    background_color;       // Background color
extern bool   backup_before_writing;  // Save a .BAK file
extern bool   binary_mode;            // Save in binary mode
extern bool   black_on_white;         // Display attribute
extern bool   bold_font;              // Use bold font attribute
extern bool   case_sensitivity;       // Preserve case in search
extern bool   compile_in_buffer;      // Compile in buffer or not
extern bool   confirm_unsaved_buffer; // Prompt for unsaved buffer
extern bool   date_completion;        // Complete date __/__/__
extern bool   display_command;        // Display command on mode line
extern bool   fast_redisplay;         // Fast redisplay
extern int    fill_column;            // Column to start filling on
extern int    foreground_color;       // Foreground color
extern bool   gnu_compatible;         // GNU Emacs compatible
extern int    java_indent;            // Java indent
extern bool   latext_mode;            // LaTex mode flag
extern bool   line_number_mode;       // Display average on mode line
extern bool   mouse_flag;             // Activation mouse flag
extern bool   monochrome_monitor;     // Monochrome flag
extern bool   pipe_process;           // Use pipes with process
extern bool   replace_mode;           // Replace mode status
extern int    tab_display;            // Default display size
extern int    tab_size;               // Default tabulation size
extern int    screen_height;          // Screen height
extern int    screen_width;           // Screen width
extern bool   set_show_graphic;       // Display graphic char or not
extern bool   show_menu;              // Show menu on Windows
extern bool   system_colors;          // Display system colors
extern bool   mouse_avoidance_mode;   // Auto mouse move flag
extern int    mouse_avoidance_nudge;  // Auto mouse move nudge
} // namespace opt

class Buffer;
class EditWindow;
class Redisplay;
class Terminal;

class Line;

extern Buffer*     curbp;              // Current buffer
extern EditWindow* curwp;              // Current window
extern Redisplay*  redisplay;          // Object which redisplays things.
extern Terminal*   term;               // Emact terminal

#include "./defines.h"
#endif
