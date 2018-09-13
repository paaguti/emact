/*
 * static char rcsid[] = "$Id: emacs.h,v 1.66 2018/09/09 07:25:14 jullien Exp $";
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

#if (defined(UNICODE) || defined(_UNICODE)) && !defined(_WIDECHARS)
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

#include <vector>
#include <limits>
#include <memory>

#if defined(HAVE_CONFIG_H)
#include "config.h"
#if !defined(X_DISPLAY_MISSING)
#define _X11
#endif
#endif

#if defined(_POSIX_C_SOURCE)
#include <unistd.h>
#define _SPAWNED_PIPE
#endif  /* _POSIX_C_SOURCE */

#if defined(_DIRECTORY)
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
typedef struct dirent ENTRY;
#endif

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
#define _SPAWNED_PIPE
#define _DOSPATH
#endif  /* _WIN32 */

/*
 *      END OF INCLUDE SECTION
 */

#define internalerror(msg)    emacserror(msg, __FILE__, __LINE__)

#if !defined(_POSIX_C_SOURCE) && defined(S_IREAD)
#define S_IWUSR S_IWRITE
#define S_IRUSR S_IREAD
using mode_t = int;
#endif

#if !defined(S_ISDIR)
#define S_ISDIR(mode)         (((mode) & S_IFMT) == S_IFDIR)
#endif

#if !defined(S_ISREG)
#define S_ISREG(mode)         ((mode) & S_IFREG)
#endif

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

#if defined(_UNICODE)
#include <cwchar>
#include <cwctype>

using EMCHAR = wchar_t;

#if !defined(__linux__)
#define stat    _stat
#endif

typedef struct stat  EMSTAT;

#define ECSTR(x)        (EMCHAR *)(L ## x)
#define EMEOF           WEOF
#define EMBOM           ((EMCHAR)0xfeff)   // BOM = 0xFEFF

#define EMMB_LEN_MAX    4                  // as required by RFC-3629

#define emstrcat(s1,s2)           std::wcscat(s1, s2)
#define emstrcpy(s1,s2)           std::wcscpy(s1, s2)
#define emstrcmp(s1,s2)           std::wcscmp(s1, s2)
#define emstrncat(s1,s2,n)        std::wcsncat(s1, s2, n)
#define emstrncpy(s1,s2,n)        std::wcsncpy(s1, s2, n)
#define emstrncmp(s1,s2,n)        std::wcsncmp(s1, s2, n)
#define emstrpbrk(s1,s2)          std::wcspbrk(s1, s2)
#define emstrrchr(s1,c)           std::wcsrchr(s1, c)
#define emstrlwr(s)               wcslwr(s)

#define emfwide(fd,mode)          std::fwide(fd, mode)

#define emsprintf1(b,f,x)         std::swprintf(b, sizeof(b), f, x)
#define emsprintf2(b,f,x,y)       std::swprintf(b, sizeof(b), f, x, y)
#define emsprintf3(b,f,x,y,z)     std::swprintf(b, sizeof(b), f, x, y, z)
#define emsprintf4(b,f,x,y,z,u)   std::swprintf(b, sizeof(b), f, x, y, z, u)
#define emsprintf5(b,f,x,y,z,u,v) std::swprintf(b, sizeof(b), f, x, y, z, u, v)

#if defined(_WINDOWS_SOURCE)
#define emstrnicmp(s1,s2,n)       wcsnicmp(s1, s2, n)
#else
#define emstrnicmp(s1,s2,n)       wcsncmp(s1, s2, n)
#endif

#define emstrlen(s)               (int)std::wcslen(s)
#define emstrtoi(s)               (int)std::wcstol(s, nullptr, 0)
#else   /* _WIDECHARS */
using EMCHAR = char;
typedef struct stat     EMSTAT;

#define ECSTR(x)        (EMCHAR*)x
#define EMEOF           EOF
#define EMBOM           0
#define EMMB_LEN_MAX    4                  /* as required by RFC-3629 */

#define emstrcat(s1,s2)           std::strcat((char *)s1, (char *)s2)
#define emstrcpy(s1,s2)           std::strcpy((char *)s1, (char *)s2)
#define emstrcmp(s1,s2)           std::strcmp((char *)s1, (char *)s2)
#define emstrncat(s1,s2,n)        std::strncat((char *)s1, (char *)s2, n)
#define emstrncpy(s1,s2,n)        std::strncpy((char *)s1, (char *)s2, n)
#define emstrncmp(s1,s2,n)        std::strncmp((char *)s1, (char *)s2, n)
#define emstrpbrk(s1,s2)          std::strpbrk((char *)s1, (char *)s2)
#define emstrrchr(s1,c)           std::strrchr((char *)s1, c)
#define emstrlwr(s)               strlwr((char *)s)

#define emsprintf1(b,f,a)         std::sprintf(b, f, a)
#define emsprintf2(b,f,x,y)       std::sprintf(b, f, x, y)
#define emsprintf3(b,f,x,y,z)     std::sprintf(b, f, x, y, z)
#define emsprintf4(b,f,x,y,z,u)   std::sprintf(b, f, x, y, z, u)
#define emsprintf5(b,f,x,y,z,u,v) std::sprintf(b, f, x, y, z, u, v)

#if defined(_WINDOWS_SOURCE)
#define emstrnicmp(s1,s2,n)       strnicmp((char *)s1, (char *)s2, n)
#else
#define emstrnicmp(s1,s2,n)       std::strncmp((char *)s1, (char *)s2, n)
#endif
#define emstrlen(s)               (int)std::strlen((char *)s)
#define emstrtoi(s)               (int)std::strtol((char *)s, nullptr, 0)
#endif  /* _WIDECHARS */

/*
 * Get the system name
 */

#if defined(__APPLE__) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("macOS")
#endif

#if defined(_POSIX_C_SOURCE) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("Posix")
#endif

#if defined(_BSD) && !defined(SYSTEMNAME)
#define SYSTEMNAME      ECSTR("BSD")
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

#if defined(_WIN32)
#define CRSIZE  2
#else
#define CRSIZE  1
#endif

/*
 * Constant definition :
 */

static constexpr auto NFILEN(FILENAME_MAX); // # of bytes, ISO file name
static constexpr auto NCMDN(16);            // # of bytes, command name
static constexpr auto NMAX(64);             // # of macros
static constexpr auto MAXLINE(0x8000);      // # of bytes, line in read
static constexpr auto NLINE(256);           // # of bytes, line (internal)
static constexpr auto NKBDM(256);           // # of strokes, keyboard macro
static constexpr auto NPAT(80);             // # of bytes, pattern

static constexpr auto METACH(0x1B);         // M- prefix, Control-[, ESC

#if defined(max)
#undef max
#endif

static constexpr auto MAX_EMCHAR(std::numeric_limits<EMCHAR>::max());

#if defined(_UNICODE)
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

static constexpr auto CFCPCN(1 << 0);  // Last command was C-P, C-N
static constexpr auto CFKILL(1 << 1);  // Last command was a kill
static constexpr auto CFMMOV(1 << 2);  // Last command needs mouse move
static constexpr auto CFFSRC(1 << 3);  // Last command was C-S
static constexpr auto CFBSRC(1 << 4);  // Last command was C-R
static constexpr auto CFTAB( 1 << 5);  // Last command was TAB
static constexpr auto CFCOMP(1 << 6);  // Last command was compare-win
static constexpr auto CFCPLT(1 << 7);  // Last command was complete
static constexpr auto CFFKEY(1 << 8);  // Last command was fn-key
static constexpr auto CFFAIL(1 << 9);  // Last command has faild

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
  BUFFERMODE,
  /** SGML mode */
  SGMLMODE,
  /** Perl mode */
  PERLMODE,
  /** C# mode */
  CSHARPMODE,
  /** shell mode */
  SHELLMODE
};

static constexpr auto DIREDMARK(2);   // Two characters to mark dired

/*
 * Flags for functions that deal with directories
 */

static constexpr auto SLASH(0x0000);    // Normalize with trailing /
static constexpr auto NOSLASH(0x0001);  // Normalize with no trailing /

/*
 * Flags for functions that deal with external commands
 */

static constexpr auto SYSCOMP_NOERROR = 0x0000; // Call system and no error
static constexpr auto SYSCOMP_ERRORS  = 0x0001; // Call system and check errors

/**
 * Enum for anycb function
 */
enum class ANYCB {
  /** Prompt for any unsaved buf */
  PROMPT = 0x0000,
  /** Check only unsaved buffers */
  CHECK  = 0x0001
};

/*
 * List of internal buffer names
 */

static constexpr auto BUF_LIST(ECSTR("*Buffer List*"));
static constexpr auto BUF_HELP(ECSTR("*Help*"));
static constexpr auto BUF_PROC(ECSTR("*Process*"));
static constexpr auto BUF_DIR(ECSTR("*Directory*"));
static constexpr auto BUF_SCRATCH(ECSTR("*scratch*"));

class BUFFER;
class EDLINE;

/**
 * A Point is a position of a character in an EDLINE.
 */
class Point final {
 public:
  explicit Point(EDLINE* line = nullptr, int pos = 0)
    : _line(line),
      _pos(pos) {
  }
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
  set(EDLINE* line, int pos) {
    _line = line;
    _pos  = pos;
  }

  void
  setLine(EDLINE* line) {
    _line = line;
  }

  void
  setPos(int pos) {
    _pos  = pos;
  }

  EDLINE*
  line() const noexcept {
    return _line;
  }

  int
  pos() const noexcept {
    return _pos;
  }
  
 private:
  EDLINE* _line{nullptr};
  int     _pos{0};
};

/*
 * There  is  a  window  structure  allocated  for  every active
 * display  window.  The windows are kept in a big list,  in top
 * to bottom screen order,  with the listhead at "wheadp".  Each
 * window  contains  its  own  values of dot and mark.  The flag
 * field  contains  some  bits that are set by commands to guide
 * redisplay.
 */

class WINSCR {
 public:
  static constexpr uint32_t WFCLEAR = 0x00; // All flags cleared.
  static constexpr uint32_t WFFORCE = 0x01; // Window needs forced reframe
  static constexpr uint32_t WFMOVE  = 0x02; // Movement from line to line
  static constexpr uint32_t WFEDIT  = 0x04; // Editing within a line
  static constexpr uint32_t WFHARD  = 0x08; // Better to a full display
  static constexpr uint32_t WFMODE  = 0x10; // Update mode line.

  WINSCR();
  ~WINSCR() {}

  WINSCR*
  next() const noexcept {
    return _wndp;
  }

  BUFFER*
  buffer() const noexcept {
    return _bufp;
  }

  void
  current();

  void
  disconnect();

  CMD
  connect(BUFFER* bp);

  int
  toprow() const noexcept {
    return _toprow;
  }

  /**
   * @return # of rows of text in window
   */
  int
  rows() const noexcept {
    return _ntrows;
  }

  void
  setFlags(uint32_t flags) noexcept {
    if (flags == WFCLEAR) {
      _flags = 0;
      _force = 0;
    } else {
      _flags |= flags;
    }
  }

  uint32_t
  getFlags() const noexcept {
    return _flags;
  }

  int
  force() const noexcept {
    return _force;
  }

  Point
  getDot() const noexcept {
    return _dot;
  }

  void
  setDot(const Point& point) noexcept {
    _dot = point;
  }

  void
  setDot(EDLINE* line, int pos) noexcept {
    _dot.set(line, pos);
  }

  void
  setDotPos(int pos) noexcept {
    _dot.setPos(pos);
  }

  void
  moveDotPos(int delta) noexcept {
    _dot.setPos(_dot.pos() + delta);
  }

  void
  setDotLine(EDLINE* line) noexcept {
    _dot.setLine(line);
  }

  /**
   * helper. Equivalent to getDot().line()
   */
  EDLINE*
  line() const noexcept {
    return _dot.line();
  }

  /**
   * helper. Equivalent to getDot().pos()
   */
  int
  pos() const noexcept {
    return _dot.pos();
  }

  void
  setMark(EDLINE* line, int pos) noexcept {
    _mark.set(line, pos);
  }

  void
  setMark(const Point& p) noexcept {
    _mark = p;
  }

  Point
  getMark() const noexcept {
    return _mark;
  }

  EDLINE*
  topline() {
    return _toplinep;
  }

  void
  setTopline(EDLINE* line) {
    _toplinep = line;
  }

  /*
   * EmACT commands having access to private members.
   */
  friend CMD recenter();
  friend CMD reposition();
  friend CMD resize();
  friend CMD onlywind();
  friend CMD enlargewind();
  friend CMD shrinkwind();
  friend CMD delwind();
  friend CMD splitwind();

 private:
  EDLINE*  _toplinep{nullptr};  // Top line in the window
  Point    _dot;                // Line containing "."
  Point    _mark;               // Mark point.
  WINSCR*  _wndp{0};            // Next window
  BUFFER*  _bufp{0};            // Buffer displayed in window
  int      _toprow{0};          // Origin 0 top row of window
  int      _ntrows{0};          // # of rows of text in window
  uint32_t _flags{0};           // Flags.
  int      _force{0};           // If non-zero, forcing row.

  static WINSCR* wheadp;        // Head of list of windows

public:
  static WINSCR* head() { return wheadp; }

//private:
};

/*
 * Text  is kept in buffers.  A buffer header,  described below,
 * exists  for every buffer in the system.  The buffers are kept
 * in  a big list,  so that commands that search for a buffer by
 * name  can  find the buffer header.  There is a safe store for
 * the  dot  and  mark in the header,  but this is only valid if
 * the  buffer is not being displayed (that is,  if "b_count" is
 * 0).  The  text  for the buffer is kept in a circularly linked
 * list of lines, with a pointer to the header line in b_linep.
 */

static constexpr auto BFCHG(0x01);         // Changed since last write

class BUFFER {
 public:
  static constexpr size_t NBUFN{16}; // # of bytes, buffer name

  /*
   * Find  a  buffer,  by  name.  Return  a  pointer to the BUFFER
   * structure  associated with it.  If the named buffer is found,
   * but is a TEMP buffer (like the buffer list) conplain.  If the
   * buffer  is  not  found and the "cflag" is T,  create it.
   */
  static BUFFER*
  find(const EMCHAR* bname,
       bool cflag = true,
       EDITMODE mode = EDITMODE::FUNDAMENTAL);

  BUFFER*
  next() const noexcept {
    return _bufp;
  }

  void
  ontop() noexcept;

  WINSCR*
  show() noexcept;

  bool
  clear() noexcept;

  bool
  discard() noexcept;

  EDLINE*
  firstline();

  EDLINE*
  lastline() {
    return _linep;
  }

  bool
  usewindow() const noexcept;

  int
  count() const noexcept {
    return _count;
  }

  void
  incr() noexcept {
    ++_count;
  }
  
  void
  decr() noexcept {
    --_count;
  }

  ENCODING
  encoding() const noexcept {
    return _wide;
  }

  void
  setEncoding(ENCODING encoding) noexcept {
    _wide = encoding;
  }

  static void
  validitycheck();

  static void
  updatemodes() noexcept;

  static BUFFER* head() noexcept;

  Point
  getDot() const noexcept {
    return _dot;
  }

  void
  setDot(const Point& point) noexcept {
    _dot = point;
  }

  void
  setDot(EDLINE* line, int pos) noexcept {
    _dot.set(line, pos);
  }

  /**
   * helper. Equivalent to getDot().line()
   */
  EDLINE*
  line() const noexcept {
    return _dot.line();
  }

  /**
   * helper. Equivalent to getDot().pos()
   */
  int
  pos() const noexcept {
    return _dot.pos();
  }

  Point
  getMark() const noexcept {
    return _mark;
  }

  void
  setMark(EDLINE* line, int pos) noexcept {
    _mark.set(line, pos);
  }

  void
  setMark(const Point& p) noexcept {
    _mark = p;
  }

 private:
  BUFFER(const EMCHAR* bname,
         bool bflag = false,
         EDITMODE mode = EDITMODE::FUNDAMENTAL);

 public:
  ~BUFFER() {}

  void
  setBuffer(const EMCHAR* name) {
    size_t i;
    for (i = 0; name[i] != '\000' && i < NBUFN; ++i) {
      _bname[i] = name[i];
    }
    _bname[i] = '\000';
  }

  EMCHAR*
  filename() {
    return _fname;
  }

  EMCHAR*
  bufname() {
    return _bname;
  }

  bool
  unbound() const {
    return _fname[0] == (EMCHAR)0;
  }

  bool
  isChanged() {
    return _flag;
  }

  EDITMODE
  editMode() const noexcept {
    return _emode;
  }

  void
  setEditMode(EDITMODE mode) noexcept {
    _emode  = mode;
    _binary = (mode != EDITMODE::FUNDAMENTAL);
  }

  void
  setChanged(bool flag = true) {
    _flag = flag;
  }

  time_t
  time() const noexcept {
    return _time;
  }

  void
  setTime(time_t time) noexcept {
    _time = time;
  }

  bool
  binary() const noexcept {
    return _binary;
  }

  void
  setBinary(bool binary) noexcept {
    _binary = binary;
  }

  void
  setReadonly(bool readOnly) noexcept {
    _readonly = readOnly;
  }

  bool
  readonly() const noexcept {
    return _readonly;
  }

  void
  setPermissions(mode_t mode) noexcept {
    _mode = mode;
  }

  mode_t
  getPermissions() const noexcept {
    return _mode;
  }

 private:
  mode_t   _mode{0};                      // File permission mode.
  Point    _dot;                          // "." EDLINE and offset link.
  EDLINE*  _linep{nullptr};               // Link to the header EDLINE
  BUFFER*  _bufp{nullptr};                // Link to next BUFFER
  Point    _mark;                         // Mark in this buffer.
  ENCODING _wide{ENCODING::EMASCII};      // Wide flag
  EDITMODE _emode{EDITMODE::FUNDAMENTAL}; // Buffer electric mode
  bool     _flag{false};                  // Flags
  bool     _binary{false};                // Binary flag
  bool     _readonly{false};              // when true, buffer is read only
  EMCHAR   _fname[NFILEN];                // File name
  EMCHAR   _bname[NBUFN];                 // Buffer name
  time_t   _time{0};                      // Last modification time
  int      _count{0};                     // Count of windows on buffer

  static BUFFER* bheadp;                  // Head of list of buffers
};

/*
 * All  text  is  kept  in  circularly  linked  lists  of EDLINE
 * structures.  These  begin  at  the  header line (which is the
 * blank  line  beyond  the  end  of  the buffer).  This line is
 * pointed  to by the "BUFFER".  Each line contains a the number
 * of bytes in the line (the "used" size),  the size of the text
 * array,  and  the  text.  The  end  of line is not stored as a
 * byte;  it's  implied.  Future  additions  will include update
 * hints, and a list of marks into the line.
 */

class EDLINE {
 public:
  EDLINE() = delete;
  EDLINE(const EDLINE&) = delete;
  EDLINE& operator=(const EDLINE&) = delete;
  ~EDLINE() = delete;

  static EDLINE* alloc(int used = 0);
  static void    dispose(EDLINE*& lp);
  static void    free(EDLINE*& lp);

  int
  leftmargin() const noexcept;

  EMCHAR*
  text() const noexcept {
    return l_text;
  }

  /**
   * @return last valid character address in this line.
   */
  EMCHAR*
  last() const noexcept {
    return l_text + l_used;
  }

  /**
   * @return address of character at position n.
   */
  EMCHAR*
  address(int n) const noexcept {
    return l_text + n;
  }

  int
  size() const noexcept {
    return l_size;
  }

  EMCHAR
  get(int n) const noexcept {
    return l_text[n];
  }

  void
  put(int n, int c) noexcept {
    l_text[n] = static_cast<EMCHAR>(c);
  }

  /*
   * Return  the  position  of  last  character in the line 'line'
   * expanding tabs to the current tab_display value.
   */
  int
  position() const noexcept;

  int
  length() const noexcept {
    return l_used;
  }

  void
  setLength(int n) noexcept {
    l_used = n;
  }

  EDLINE*
  forw() const noexcept {
    return l_fp;
  }

  EDLINE*
  back() const noexcept {
    return l_bp;
  }

  void
  print(const char* file, size_t line, const char* msg) {
    auto f = [this](const char* what, const EDLINE* l) {
               char buf[128];
               (void)std::memset(buf, 0, sizeof(buf));
               (void)std::memcpy(buf, l->l_text, l_used);
               std::printf("%s: %d/%d %s\n", what, l->l_used, l->l_size, buf);
             };

    std::printf("%s(%d): '%s'\n", file, static_cast<int>(line), msg);
    f("prev", l_bp);
    f("this", this);
    f("next", l_fp);
  }

  /*
   *  Before:
   *        +------+
   *        | prev |--+
   *        +------+  |
   *    +-------------+
   *    |   +------+
   *    +-->| this |
   *        +------+
   *
   *    After:
   *        +------+
   *        | prev |--+
   *        +------+  |
   *    +-------------+
   *    |   +------+
   *    +-->| line |--+
   *        +------+  |
   *    +-------------+
   *    |   +------+
   *    +-->| this |
   *        +------+
   */
  EDLINE*
  insertBefore(int n, const char* file = nullptr, size_t lineNb = 0) {
    EDLINE* line;

    if (file) {
      print(file, lineNb, ">> insertBefore-1 this");
    }

    if ((line = alloc(n)) == nullptr) {
     return line;
    }

    auto prev = this->l_bp; /* Previous line */

    prev->l_fp   = line;       /* Link in */
    line->l_fp   = this;
    line->l_bp   = prev;
    this->l_bp   = line;

    if (file) {
      print(file, lineNb, "<< insertBefore-2 this");
    }

    return line;
  }

  EDLINE*
  realloc(int n, const char* file = nullptr, size_t lineNb = 0) {
    EDLINE* line;

    if (file) {
      (void)print(file, lineNb, "realloc");
    }

    if ((line = alloc(n)) == nullptr) {
      return line;
    }

    auto prev(this->l_bp);
    auto next(this->l_fp);

    line->l_fp = next;
    next->l_bp = line;

    line->l_bp = prev;
    prev->l_fp = line;

    (void)std::memcpy(line->l_text,
                      this->l_text,
                      this->l_used * sizeof(EMCHAR));

    if (file) {
      (void)print(file, lineNb, "<< realloc this");
      line->print(file, lineNb, "<< realloc line");
    }

    return line;
  }

  /**
   * remove and free line argument.
   * Current line becomes the parent of its next line.
   */
  void
  remove(EDLINE* line);

  /**
   * swap current line with line argument.
   */
  void
  swap(EDLINE* line);

 private:
  EMCHAR* l_text; // A bunch of characters.
  EDLINE* l_fp;   // Link to the next line
  EDLINE* l_bp;   // Link to the previous line
  int     l_size; // Allocated size
  int     l_used; // Used size
};

#define lgetdot()  curwp->line()->get(curwp->pos())
#define lputdot(c) curwp->line()->put(curwp->pos(), c)

/*
 * The  editor  communicates with the display using a high level
 * interface.  A  "TERM"  structure holds useful variables,  and
 * indirect pointers to routines that do useful operations.  The
 * low  level  get  and  put routines are here too.  This lets a
 * terminal,  in addition to having non standard commands,  have
 * funny  get  and put character code too.  Some implementations
 * using  a high level interface such as Windowing system define
 * graphic  widget  to select a file,  display error and ask the
 * user.  A  "WIDGET" structure holds indirect pointers to those
 * functionalities.
 */

class Terminal {
 public:
  /* Open terminal at the start. */
  Terminal() {}
  /* Close terminal at end. */
  virtual ~Terminal() {}
  /* Get character from keyboard. */
  virtual int
  get() {
    return 0;
  }
  /* Put character to display. */
  virtual void
  insert(int c) = 0;
  /* Put a string to display. */
  virtual void
  insert(const EMCHAR* s, int n) = 0;
  /* Flush output buffers. */
  virtual void
  flush() {}
  /* Move the cursor, origin 0. */
  virtual void
  move(int x, int y) = 0;
  /* Erase to end of line. */
  virtual void
  eeol() = 0;
  /* Erase to end of page. */
  virtual void
  eeop() = 0;
  /* Beep. */
  virtual void
  beep() {}
  /* Start inverse video */
  virtual void
  si() {}
  /* End   inverse video */
  virtual void
  ei() {}
  /* Show/Hide the cursor */
  virtual void
  cshow(bool flag) {
    (void)flag;
  }
  /* Check event */
  virtual bool
  check() {
    return 0;
  }
  /* Put in raw mode */
  virtual void
  rawmode() {
  }

 public:
  /* Number of rows. */
  int t_nrow{0};
  /* Number of columns. */
  int t_ncol{0};
  /* Scroll position. */
  int t_nscroll{0};
  /* Term initialized. */
  int t_init{false};
};

struct WIDGET {
  /*
   * Y/N Widget
   */
  CMD (*w_yn)(const EMCHAR* s);
  /*
   * YES/NO Widget
   */
  CMD (*w_yesno)(const EMCHAR* s);
  /*
   * CONFIRM Widge
   */
  CMD (*w_confirm)(const EMCHAR *s);
  /*
   * ERROR Widget
   */
  void (*w_error)(const EMCHAR *s);
  /*
   * TITLE Widget
   */
  EMCHAR* (*w_title)(EMCHAR *b, EMCHAR *f);
  /*
   * ASKER Widget
   */
  CMD (*w_asker)(const EMCHAR *s, EMCHAR *b, int n);
  /*
   * EDITOR Widget
   */
  CMD (*w_edit)(const EMCHAR* s, EMCHAR *b, int n);
  /*
   * CHANGE Widget
   */
  CMD (*w_change)(const EMCHAR* os,
                  const EMCHAR* ns,
                  EMCHAR* op,
                  EMCHAR* np,
                  int n);
  /*
   * PLAY Widget
   */
  void (*w_play)(int flag);
  /*
   * WAIT Widget
   */
  void (*w_wait)();
  /*
   * MSG Widget
   */
  void (*w_message)(const EMCHAR* str);
  /*
   * WRITE Widget
   */
  void (*w_write)(const EMCHAR *fmt, ...);
  /*
   * ADJUST Widget
   */
  void (*w_adjust)();
  /*
   * UPDATE Widget
   */
  void (*w_update)(const EMCHAR* p, EMCHAR* b);
  /*
   * CBCOPY Widget
   */
  void (*w_clipcopy)();
  /*
   * CBPAST Widget
   */
  void (*w_clippaste)();
};

struct EMPRINT {
  /*
   * print buffer
   */
  void (*p_print)();
};

extern Terminal* tt;

void TTYopen();

#define TTYncol               tt->t_ncol
#define TTYnrow               tt->t_nrow
#define TTYinit               tt->t_init
#define TTYbeep()             tt->beep()
#define TTYclose()            delete tt;
#define TTYmove(x, y)         tt->move(x, y)
#define TTYeol()              tt->eeol()
#define TTYeop()              tt->eeop()
#define TTYflush()            tt->flush()
#define TTYgetc()             tt->get()
#define TTYinverse()          tt->si()
#define TTYnormal()           tt->ei()
#define TTYputc(c)            tt->insert(c)
#define TTYputs(s, n)         tt->insert(s, n)
#define TTYcshow(f)           tt->cshow(f)
#define TTYcheck()            tt->check()
#define TTYrawmode()          tt->rawmode()

#define LPTprint()            (*printer.p_print)()

#define WDGyn(s)              (*widget.w_yn)(s)
#define WDGyesno(s)           (*widget.w_yesno)(s)
#define WDGconfirm(s)         (*widget.w_confirm)(s)
#define WDGerror(s)           (*widget.w_error)(s)
#define WDGtitle(b, f)        (*widget.w_title)(b, f)
#define WDGasker(p, b, n)     (*widget.w_asker)(p, b, n)
#define WDGedit( p, b, n)     (*widget.w_edit)( p, b, n)
#define WDGchange(o,n,s,r,l)  (*widget.w_change)(o, n, s, r, l)
#define WDGplay(f)            (*widget.w_play)(f)
#define WDGwait()             (*widget.w_wait)()
#define WDGmessage(s)         (*widget.w_message)(s)
#define WDGwrite              (*widget.w_write)
#define WDGadjust             (*widget.w_adjust)
#define WDGupdate(p,b)        (*widget.w_update)(p,b)
#define WDGclipcopy()         (*widget.w_clipcopy)()
#define WDGclippaste()        (*widget.w_clippaste)()

/*
 * Commands and variables table.
 */

class KEYTAB {
 private:
  typedef CMD (*CB)();
  int     k_code;                // Key code
  CMD     (*k_fp)();             // Routine to handle it
  const EMCHAR* k_name;          // Name of the command

 public:
  constexpr KEYTAB(int code, CB fp, const EMCHAR* name)
    : k_code{code},
      k_fp{fp},
      k_name{name} {
  }

  const EMCHAR*
  name() const noexcept {
    return k_name;
  }

  int
  code() const noexcept {
    return k_code;
  }

  void
  unset() {
    k_code = UNBOUND;
  }

  CMD
  execute() const noexcept {
    return k_fp();
  }

  CB
  callback() const noexcept {
    return k_fp;
  }

  static std::vector<KEYTAB> keytab;
};

enum EMVAR {
  BOOLVAL = 0x0000,          // T or NIL type
  FIXVAL  = 0x0001,          // Integer type
  STRING  = 0x0002           // String type
};

struct VARTAB {
 public:
  template<typename T>
  constexpr VARTAB(T& val, EMCHAR* name, EMVAR type)
  : f_val{&val},
    f_name{name},
    f_type{type} {
  }

  template<typename T>
  constexpr VARTAB(T& val, EMCHAR* name, int)
  : f_val{&val},
    f_name{name},
    f_type{STRING} {
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

  static std::vector<VARTAB> vartab;

 private:
  void*   f_val;                 // Flag address
  EMCHAR* f_name;                // Flag name
  EMVAR   f_type;                // Type/length of the variable
};

#define VARstring(i)  VARTAB::vartab[i].string()
#define VARintp(i)    VARTAB::vartab[i].intp()
#define VARboolp(i)   VARTAB::vartab[i].boolp()
#define VARname(i)    VARTAB::vartab[i].name()
#define VARtype(i)    VARTAB::vartab[i].type()

struct MACTAB {
  int*    m_exec;  // Code
  EMCHAR* m_name;  // Macro name
  int     m_code;  // Key bind
  int     m_size;  // total command length
};

#define MACcode(i)            ((pmactab+i)->m_code)
#define MACname(i)            ((pmactab+i)->m_name)
#define MACexec(i)            ((pmactab+i)->m_exec)
#define MACsize(i)            ((pmactab+i)->m_size)

/*
extern int           MACcode(int i);
extern const EMCHAR* MACname(int i);
extern int           MACsize(int i);
*/

/**
 * Handle completion.
 */
class Completion {
 private:
  using Callback = EMCHAR* (*)(const EMCHAR* prompt, EMCHAR* buf);

 public:
  enum class Status {
    COMPLETE_ONE,
    COMPLETE_AGAIN,
    COMPLETE_ABORT,
    COMPLETE_FAIL
  };

  const EMCHAR*
  operator()(const EMCHAR* prompt, EMCHAR* buf) {
    return _fn(prompt, buf);
  }

  Completion&
  operator=(Callback cb) {
    _fn = cb;
    return *this;
  }

  bool
  operator==(Callback cb) {
    return _fn == cb;
  }

  bool
  operator!=(Callback cb) {
    return _fn != cb;
  }

  Status
  status() const noexcept {
    return _status;
  }

  void
  setStatus(Status status) noexcept {
    _status = status;
  }

 private:
  Status _status;
  Callback _fn;
};

extern Completion complete;            // Automatic completion

extern WINSCR* curwp;                  // Current window
extern BUFFER* curbp;                  // Current buffer

extern WIDGET  widget;                 // Widgets tools
extern EMPRINT printer;                // Printer object

/*
 *	display.c
 */

class DISPLAY {
 public:
  enum class Mode {
    DELTA   = 0,
    MINIBUF = 0x0200,  // Minibuffer
    REFRESH = 0x0400   // Full refresh
  };
  enum class Sync {
    GARBAGE      = 0,
    SYNCHRONIZED = 1,
    EXPOSE       = 2
  };
  DISPLAY();
  ~DISPLAY();
  bool running() const noexcept;
  void tidy() const noexcept;
  const EMCHAR* text(int y) const noexcept;
  void update(DISPLAY::Mode mode = Mode::DELTA);
  void statputc(int n, int c);
  void modeline(WINSCR* wp);
  static void garbaged() { _sgarbf = Sync::GARBAGE; }
  static void synchronized() { _sgarbf = Sync::SYNCHRONIZED; }
  static void exposed() { _sgarbf = Sync::EXPOSE; }
  static int    _currow;   // Cursor row
  static int    _curcol;   // Cursor column
  static EMCHAR _curchar;  // Char at cursor
  static Sync   _sgarbf;   // screen is garbage
  static bool   _mouse;    // mouse flags

 private:
  void refresh(WINSCR* wp);
};

extern DISPLAY* display;

/*
 *	Header for mouse driver (if any).
 */

struct MEvent {
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

extern MEvent mevent;

class Emacs {
 public:
#if 0
  static int repeat;                 // Repeat count
  static int thisflag;               // Flags, this command
  static int lastflag;               // Flags, last command
  static int curgoal;                // Goal for C-P, C-N
#endif
};

extern Emacs* emact;

extern int      eargc;                 // Argc
extern EMCHAR** eargv;                 // Argv

extern int     kbdm[NKBDM];            // Holds keyboard macro data
extern int*    kbdmip;                 // Input pointer for above
extern int*    kbdmop;                 // Output pointer for above

extern EMCHAR search_buffer[NPAT];     // Search pattern

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

extern int repeat;                 // Repeat count
extern int thisflag;               // Flags, this command
extern int lastflag;               // Flags, last command
extern int curgoal;                // Goal for C-P, C-N

#include "defines.h"
#endif
