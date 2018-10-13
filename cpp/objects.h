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
  BUFFERMODE,
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

class BUFFER;
class EDLINE;
class Point;
class WINSCR;
class Terminal;
class EditorCommand;
class Completion;
class Editor;
class Kbdm;

/**
 * A Point is a position of a character in an EDLINE.
 */
class Point final {
 public:
  explicit Point(EDLINE* pointLine = nullptr, int pointPos = 0)
    : _line{pointLine},
      _pos{pointPos} {
  }

  ~Point() = default;
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
  set(EDLINE* newLine, int newPos) {
    _line = newLine;
    _pos  = newPos;
  }

  void
  setLine(EDLINE* newLine) {
    _line = newLine;
  }

  void
  setPos(int newPos) {
    _pos  = newPos;
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
 * There is a window structure allocated for every active display
 * window.  The windows are kept in a big list, in top to bottom
 * screen order.  Each window contains its own values of dot and mark.
 * The flag field contains some bits that are set by commands to guide
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

  /**
   * Default ctor.
   * Create a new WINSCR object and put its pointer on top of an internal list.
   * @param [in] bp buffer associated to this window.
   */
  explicit WINSCR(BUFFER* bp) noexcept;

  /**
   * destroy window and uncontitionally removes its pointer
   * from an internal list.
   */
  ~WINSCR();

  BUFFER*
  buffer() const noexcept {
    return _bufp;
  }

  /*
   * Make the window pointed by wp the current window.  It restack
   * the  buffer so that it becomes on top of the list for command
   * switch-buffer or kill-buffer.
   */
  void
  current() noexcept;

  /**
   * Disconnect  the  buffer  associated  to the window pointed by
   * this.  If the buffer display count equals 0 (meaning that the
   * buffer  is  no  more  displayed on the screen) then copy mark
   * and   dot  values  in  the  buffer.  This  function  is  used
   * internally.
   */
  void
  disconnect() noexcept;

  /**
   * Connect  the  buffer  "bp" to the window pointed by "wp".  If
   * another  window  point  to  the same buffer copy mark and dot
   * values in the buffer. This function is used internally.
   * @param [in] bp buffer to connect to thsi windows.
   * @param [in] check if true, do a sanity check on buffer count.
   * @return true on success.
   */
  bool
  connect(BUFFER* bp, bool check = true) noexcept;

  int
  toprow() const noexcept {
    return _toprow;
  }

  /**
   * Pick  a  window  for  a pop-up.  Split the screen if there is
   * only  one  window.
   * @return uppermost window that isn't the current window.
   */
  static WINSCR*
  popup() noexcept;

  /*
   * Redisplay  the  screen  after  a resize.  Useful on Windowing
   * system. This command is not bound to any key stroke.
   * @return true on success.
   */
  static bool
  resize() noexcept;

  /**
   * Return the window down this one in windows list or nullptr if it is
   * the ast window.
   * @return window down or nullptr.
   */
  WINSCR*
  down() const noexcept {
    auto it = std::find(WINSCR::list().begin(), WINSCR::list().end(), this);
    if (++it != WINSCR::list().end()) {
      return *it;
    } else {
      return nullptr;
    }
  }

  /**
   * Return the window above this one in windows list or nullptr if it is
   * the last window.
   * @return window up or nullptr.
   */
  WINSCR*
  up() const noexcept {
    auto it = std::find(WINSCR::list().rbegin(), WINSCR::list().rend(), this);
    if (++it != WINSCR::list().rend()) {
      return *it;
    } else {
      return nullptr;
    }
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
  setDot(EDLINE* dotLine, int dotPos) noexcept {
    _dot.set(dotLine, dotPos);
  }

  void
  setDotPos(int dotPos) noexcept {
    _dot.setPos(dotPos);
  }

  void
  moveDotPos(int delta) noexcept {
    _dot.setPos(_dot.pos() + delta);
  }

  void
  setDotLine(EDLINE* dotLine) noexcept {
    _dot.setLine(dotLine);
  }

  /**
   * helper. Equivalent to getDot().line()
   */
  EDLINE*
  line() const noexcept {
    return _dot.line();
  }

  inline EMCHAR
  getChar() const;

  inline void
  setChar(int c);

  /**
   * helper. Equivalent to getDot().pos()
   */
  int
  pos() const noexcept {
    return _dot.pos();
  }

  void
  setMark(EDLINE* markLine, int markPos) noexcept {
    _mark.set(markLine, markPos);
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
  setTopline(EDLINE* topLine) {
    _toplinep = topLine;
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

  static std::list<WINSCR*>& list() noexcept {
    return _wlist;
  }

 private:
  static std::list<WINSCR*> _wlist;
  EDLINE*  _toplinep{nullptr};  // Top line in the window
  Point    _dot;                // Line containing "."
  Point    _mark;               // Mark point.
  BUFFER*  _bufp{nullptr};      // Buffer displayed in window
  int      _toprow{0};          // Origin 0 top row of window
  int      _ntrows{0};          // # of rows of text in window
  uint32_t _flags{0};           // Flags.
  int      _force{0};           // If non-zero, forcing row.
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

class BUFFER {
 public:
  /**
   * Enum for anycb function
   */
  enum class ANYCB {
    /** Prompt for any unsaved buf */
    PROMPT = 0x0000,
    /** Check only unsaved buffers */
    CHECK  = 0x0001
  };

  static constexpr size_t NBUFN{16}; // # of bytes, buffer name

  /**
   * Find a buffer, by name.
   * @return a pointer to the BUFFER structure associated with it.  If
   * the named buffer is found, but is a TEMP buffer (like the buffer
   * list) conplain.
   * @param [in] bname buffer name
   * @param [in] cflag if true, create the buffer if not found.
   * @param [in] mode buffer edit mode (default EDITMODE::FUNDAMENTAL).
   */
  static BUFFER*
  find(const EMCHAR* bname,
       bool cflag = true,
       EDITMODE mode = EDITMODE::FUNDAMENTAL);

  void
  ontop() noexcept;

  /**
   * Show  an  internal  buffer  'bp'.  If  this  buffer in not on
   * screen,  split  the  current  window  and  display  it.
   * @return the window that display the buffer or nullptr if it fails.
   */
  WINSCR*
  show() noexcept;

  /**
   * Look through the list of buffers. Buffers that hold magic
   * internal stuff are not considered; who cares if the list of
   * buffer names is hacked.
   * @return true if there are any changed buffers, false otherwise.
   */
  static bool
  anycb(ANYCB flag);

  /*
   * This routine gets called when a character is changed in place in
   * the current buffer.  It updates all of the required flags in the
   * buffer and window system. if the buffer is being displayed in
   * more than 1 window we change EDIT to HARD.  Set MODE if the mode
   * line needs to be updated (the "*" has to be set).
   * @param [in] flag requested new flag.
   */
  static void
  change(int flag);

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
  setEncoding(ENCODING encodingMode) noexcept {
    _wide = encodingMode;
  }

  static void
  validitycheck(const char* msg);

  static void
  updatemodes() noexcept;

  static std::list<BUFFER*>& list() noexcept {
    return _blist;
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
  setDot(EDLINE* dotLine, int dotPos) noexcept {
    _dot.set(dotLine, dotPos);
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
  setMark(EDLINE* markLine, int markPos) noexcept {
    _mark.set(markLine, markPos);
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
  setTime(time_t timeVal) noexcept {
    _time = timeVal;
  }

  bool
  binary() const noexcept {
    return _binary;
  }

  void
  setBinary(bool newBinary) noexcept {
    _binary = newBinary;
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
  setPermissions(mode_t newMode) noexcept {
    _mode = newMode;
  }

  mode_t
  getPermissions() const noexcept {
    return _mode;
  }

 private:
  static std::list<BUFFER*> _blist;

  mode_t   _mode{0};                      // File permission mode.
  Point    _dot;                          // "." EDLINE and offset link.
  EDLINE*  _linep{nullptr};               // Link to the header EDLINE
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
   * This  routine,  given a pointer to a EDLINE, and the  current
   * cursor  goal column,  return the best choice for the  offset.
   * The offset is returned.  Used by "C-N" and "C-P".
   * @param [in] goal current goal (generally Editing::_curcol).
   */
  int
  getgoal(int goal) const noexcept;

  /**
   * Insert a newline into the buffer at the current location of dot in
   * the current window.  The funny ass-backwards way it does things is
   * not a botch; it just makes the last line in the file not a special
   * case.
   * @return true if everything works out and false on error
   * (memory allocation failure).
   */
  static bool
  newline() noexcept;

  /**
   * swap current line with line argument.
   */
  void
  swap(EDLINE* line);

  /**
   * Insert  "n"  copies  of  the  character  "c"  at  the current
   * location  of  dot.  In  the easy case all that happens is the
   * text  is stored in the line.  In the hard case,  the line has
   * to  be  reallocated.  When  the window list is updated,  take
   * special care; I screwed it up once.  You always update dot in
   * the  current  window.  You update mark,  and a dot in another
   * window,  if  it  is  greater than the place where you did the
   * insert.
   * @param [in] c character code to insert.
   * @param [in] n number of character to insert (default 1).
   * @return true if all is well, and false on errors.
   */
  static bool
  linsert(int c, int n = 1);

  /**
   * This function deletes "n" bytes, starting at dot.  It understands
   * how do deal with end of lines, etc.
   * @param [in] n number of character to delete.
   * @param [in] kflag if true, text should be put in the kill buffer.
   * @return true if  all of the characters were deleted, and false
   * if they were not (because dot ran into the end of the buffer).
   */
  static bool
  ldelete(int n, bool kflag = false);

  /**
   * Replace "n" copies of the character "c" at the current location
   * of dot.  In the easy case all that happens is the text is
   * replaced in the line. In the hard case, at the end of the line,
   * the routine EDLINE::linsert is call with n equal to the number of
   * characters alredy replaced.
   * @param [in] c character code to insert.
   * @param [in] n number of character to replace (default 1).
   * @return true if all is well, and false on errors.
   */
  static bool
  lreplace(int c, int n = 1);

  /**
   * Append this line to the buffer. Handcraft the EOL on the end.
   * @param [in] bp buffer.
   * @param [in] text points to a string to append.
   */
  static void
  append(BUFFER* bp, const EMCHAR* text);

 private:
  /*
   * Delete  a newline.  Join the current line with the next line.
   * If  the  next  line is the magic header line always return true;
   * merging  the last line with the header line can be thought of
   * as  always  being a successful operation,  even if nothing is
   * done,  and  this  makes  the  kill buffer work "right".  Easy
   * cases  can  be  done  by  shuffling  data around.  Hard cases
   * require  that  lines be moved about in memory.
   * Called by "ldelete" only.
   * @return false on error and true if all looks ok. 
   */
  static bool delnewline();

  EMCHAR* l_text; // A bunch of characters.
  EDLINE* l_fp;   // Link to the next line
  EDLINE* l_bp;   // Link to the previous line
  int     l_size; // Allocated size
  int     l_used; // Used size
};

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
  /* Create a concrete terminal at the start. */
  static Terminal* getInstance();
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

  /* Open terminal at the start. */
  Terminal() {}
};

class WIDGET {
 public:
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
  CMD (*w_change)(const EMCHAR* msg, EMCHAR* op, EMCHAR* np, int n);
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
  /*
   * print buffer
   */
  void (*w_print)();
};

/*
 * Commands and variables table.
 */

class EditorCommand final {
 private:
  using CB = CMD (*)();

 public:
  constexpr EditorCommand(int keyCode, CB fp, const EMCHAR* keyName)
    : k_code{keyCode},
      k_fp{fp},
      k_name{keyName} {
  }

  bool
  operator==(const EditorCommand& rhs) const noexcept {
    return k_code == rhs.k_code;
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
  operator()() const noexcept {
    return k_fp();
  }

  CB
  callback() const noexcept {
    return k_fp;
  }

 private:
  int           k_code;          // Key code
  CB            k_fp;            // Routine to handle it
  const EMCHAR* k_name;          // Name of the command
};

enum EMVAR {
  BOOLVAL = 0x0000,          // T or NIL type
  FIXVAL  = 0x0001,          // Integer type
  STRING  = 0x0002           // String type
};

class VARTAB {
 public:
  template<typename T>
  constexpr VARTAB(T& val, EMCHAR* varName, EMVAR varType)
  : f_val{&val},
    f_name{varName},
    f_type{varType} {
  }

  template<typename T>
  constexpr VARTAB(T& val, EMCHAR* varName, int size)
  : f_val{&val},
    f_name{varName},
    f_type{STRING},
    f_size{static_cast<size_t>(size)} {
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

  static std::vector<VARTAB> vartab;

 private:
  void*   f_val;                 // Flag address
  EMCHAR* f_name;                // Flag name
  EMVAR   f_type;                // Type of the variable
  size_t  f_size{0};             // Size of the variable
};

class MLisp;
class MACTAB {
  friend class MLisp;
 public:
  MACTAB() = default;

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
  setStatus(Status newStatus) noexcept {
    _status = newStatus;
  }

 private:
  Status _status;
  Callback _fn;
};

/**
 * Class that handles display.
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
  void statputc(int n, int c) const noexcept;
  void modeline(const WINSCR* wp) noexcept;
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
  static void computecursor();
  static void updateline(int row, EMCHAR* vline, EMCHAR* pline);
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

  void
  engine();

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
  static EMCHAR*
  searchBuffer() noexcept {
    return &_search[0];
  }

  /*
   * Read in a key. Do the standard keyboard preprocessing.
   */
  static int
  getkey();

  static std::vector<MACTAB>&
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
  static std::vector<MACTAB> _macros;
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

EMCHAR
WINSCR::getChar() const {
  return _dot.line()->get(_dot.pos());
}

void
WINSCR::setChar(int c) {
  _dot.line()->put(_dot.pos(), c);
}

#endif /* __OBJECTS_H */
