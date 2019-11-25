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
class Line;
class Point;
class EditWindow;
class Terminal;
class EditorCommand;
class Completion;
class Editor;
class Kbdm;
class Redisplay;

/**
 * A Point is a position of a character in an Line.
 */
class Point final {
 public:
  explicit Point(Line* pointLine = nullptr, int pointPos = 0)
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
  set(Line* newLine, int newPos) {
    _line = newLine;
    _pos  = newPos;
  }

  void
  setLine(Line* newLine) {
    _line = newLine;
  }

  void
  setPos(int newPos) {
    _pos  = newPos;
  }

  Line*
  line() const noexcept {
    return _line;
  }

  int
  pos() const noexcept {
    return _pos;
  }

 private:
  Line* _line{nullptr};
  int     _pos{0};
};

/*
 * There is a window structure allocated for every active display
 * window.  The windows are kept in a big list, in top to bottom
 * screen order.  Each window contains its own values of dot and mark.
 * The flag field contains some bits that are set by commands to guide
 * redisplay.
 */

class EditWindow {
 public:
  static constexpr uint32_t WFCLEAR = 0x00;  // All flags cleared.
  static constexpr uint32_t WFFORCE = 0x01;  // Window needs forced reframe
  static constexpr uint32_t WFMOVE  = 0x02;  // Movement from line to line
  static constexpr uint32_t WFEDIT  = 0x04;  // Editing within a line
  static constexpr uint32_t WFHARD  = 0x08;  // Better to a full display
  static constexpr uint32_t WFMODE  = 0x10;  // Update mode line.

  /**
   * Default ctor.
   * Create a new EditWindow object and put its pointer on top of an internal
   * list.
   * @param [in] bp buffer associated to this window.
   */
  explicit EditWindow(Buffer* bp) noexcept;

  /**
   * destroy window and uncontitionally removes its pointer
   * from an internal list.
   */
  ~EditWindow();

  Buffer*
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
  connect(Buffer* bp, bool check = true) noexcept;

  int
  toprow() const noexcept {
    return _toprow;
  }

  /**
   * Pick  a  window  for  a pop-up.  Split the screen if there is
   * only  one  window.
   * @return uppermost window that isn't the current window.
   */
  static EditWindow*
  popup() noexcept;

  /*
   * Redisplay  the  screen  after  a resize.  Useful on EditWindowing
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
  EditWindow*
  down() const noexcept {
    auto it = std::find(EditWindow::list().begin(),
                        EditWindow::list().end(),
                        this);
    if (++it != EditWindow::list().end()) {
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
  EditWindow*
  up() const noexcept {
    auto it = std::find(EditWindow::list().rbegin(),
                        EditWindow::list().rend(),
                        this);
    if (++it != EditWindow::list().rend()) {
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
  setDot(Line* dotLine, int dotPos) noexcept {
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
  setDotLine(Line* dotLine) noexcept {
    _dot.setLine(dotLine);
  }

  /**
   * helper. Equivalent to getDot().line()
   */
  Line*
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
  setMark(Line* markLine, int markPos) noexcept {
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

  Line*
  topline() {
    return _toplinep;
  }

  void
  setTopline(Line* topLine) {
    _toplinep = topLine;
  }

  /*
   * EmACT commands having access to private members.
   */
  friend CMD recenter();
  friend CMD resize();
  friend CMD onlywind();
  friend CMD delwind();

  static std::list<EditWindow*>&
  list() noexcept {
    return _wlist;
  }

  /*
   * Editor commands bound to key:
   */
  static CMD moveDown();
  static CMD moveUp();
  static CMD reposition();
  static CMD recenter();
  static CMD next();
  static CMD previous();
  static CMD onlywind();
  static CMD delwind();
  static CMD split();
  static CMD enlarge();
  static CMD shrink();
  static CMD find();
  static CMD adjust();

 private:
  static std::list<EditWindow*> _wlist;
  Line*    _toplinep{nullptr};  // Top line in the window
  Point    _dot;                // Line containing "."
  Point    _mark;               // Mark point.
  Buffer*  _bufp{nullptr};      // Buffer displayed in window
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

class Buffer {
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

  static constexpr size_t NBUFN{16};  // # of bytes, buffer name

  /**
   * Find a buffer, by name.
   * @return a pointer to the Buffer structure associated with it.  If
   * the named buffer is found, but is a TEMP buffer (like the buffer
   * list) conplain.
   * @param [in] bname buffer name
   * @param [in] cflag if true, create the buffer if not found.
   * @param [in] mode buffer edit mode (default EDITMODE::FUNDAMENTAL).
   */
  static Buffer*
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
  EditWindow*
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

  Line*
  firstline();

  Line*
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

  static std::list<Buffer*>& list() noexcept {
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
  setDot(Line* dotLine, int dotPos) noexcept {
    _dot.set(dotLine, dotPos);
  }

  /**
   * helper. Equivalent to getDot().line()
   */
  Line*
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
  setMark(Line* markLine, int markPos) noexcept {
    _mark.set(markLine, markPos);
  }

  void
  setMark(const Point& p) noexcept {
    _mark = p;
  }

 private:
  Buffer(const EMCHAR* bname,
         bool bflag = false,
         EDITMODE mode = EDITMODE::FUNDAMENTAL);

 public:
  ~Buffer() {}

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

  static CMD buffercmd(int c);

  /*
   * Editor commands bound to key:
   */
  static CMD usebuffer();
  static CMD kill();
  static CMD listbuffers();

 private:
  static std::list<Buffer*> _blist;

  mode_t   _mode{0};                       // File permission mode.
  Point    _dot;                           // "." Line and offset link.
  Line*    _linep{nullptr};                // Link to the header Line
  Point    _mark;                          // Mark in this buffer.
  ENCODING _wide{ENCODING::EMASCII};       // Wide flag
  EDITMODE _emode{EDITMODE::FUNDAMENTAL};  // Buffer electric mode
  bool     _flag{false};                   // Flags
  bool     _binary{false};                 // Binary flag
  bool     _readonly{false};               // when true, buffer is read only
  EMCHAR   _fname[NFILEN];                 // File name
  EMCHAR   _bname[NBUFN];                  // Buffer name
  time_t   _time{0};                       // Last modification time
  int      _count{0};                      // Count of windows on buffer
};

/*
 * All text is kept in circularly linked lists of Line structures.
 * These begin at the header line (which is the blank line beyond the
 * end of the buffer).  This line is pointed to by the "Buffer".  Each
 * line contains a the number of bytes in the line (the "used" size),
 * the size of the text array, and the text.  The end of line is not
 * stored as a byte; it's implied.  Future additions will include
 * update hints, and a list of marks into the line.
 */
class Line {
 public:
  Line() = delete;
  Line(const Line&) = delete;
  Line& operator=(const Line&) = delete;
  ~Line() = delete;

  static Line* alloc(int used = 0);
  static void  dispose(Line*& lp);
  static void  free(Line*& lp);

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

  Line*
  forw() const noexcept {
    return l_fp;
  }

  Line*
  back() const noexcept {
    return l_bp;
  }

  void
  print(const char* file, size_t line, const char* msg) {
    auto f = [this](const char* what, const Line* l) {
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
  Line*
  insertBefore(int n, const char* file = nullptr, size_t lineNb = 0) {
    Line* line;

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

  Line*
  realloc(int n, const char* file = nullptr, size_t lineNb = 0) {
    Line* line;

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
  remove(Line* line);

  /**
   * This  routine,  given a pointer to a Line, and the  current
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
  swap(Line* line);

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
  insert(int c, int n = 1);

  /**
   * This function deletes "n" bytes, starting at dot.  It understands
   * how do deal with end of lines, etc.
   * @param [in] n number of character to delete.
   * @param [in] kflag if true, text should be put in the kill buffer.
   * @return true if  all of the characters were deleted, and false
   * if they were not (because dot ran into the end of the buffer).
   */
  static bool
  remove(int n, bool kflag = false);

  /**
   * Replace "n" copies of the character "c" at the current location
   * of dot.  In the easy case all that happens is the text is
   * replaced in the line. In the hard case, at the end of the line,
   * the routine Line::insert is call with n equal to the number of
   * characters alredy replaced.
   * @param [in] c character code to insert.
   * @param [in] n number of character to replace (default 1).
   * @return true if all is well, and false on errors.
   */
  static bool
  replace(int c, int n = 1);

  /**
   * Append this line to the buffer. Handcraft the EOL on the end.
   * @param [in] bp buffer.
   * @param [in] text points to a string to append.
   */
  static void
  append(Buffer* bp, const EMCHAR* text);

  /*
   * Editor commands bound to key:
   */
  static CMD notmodified();
  static CMD twiddle();
  static CMD instoggle();

 private:
  /*
   * Delete  a newline.  Join the current line with the next line.
   * If  the  next  line is the magic header line always return true;
   * merging  the last line with the header line can be thought of
   * as  always  being a successful operation,  even if nothing is
   * done,  and  this  makes  the  kill buffer work "right".  Easy
   * cases  can  be  done  by  shuffling  data around.  Hard cases
   * require  that  lines be moved about in memory.
   * Called by "remove" only.
   * @return false on error and true if all looks ok. 
   */
  static bool delnewline();

  EMCHAR* l_text;  // A bunch of characters.
  Line*   l_fp;    // Link to the next line
  Line*   l_bp;    // Link to the previous line
  int     l_size;  // Allocated size
  int     l_used;  // Used size
};

/*
 * The editor communicates with the display using a high level
 * interface.  A "Terminal" class holds useful variables, and indirect
 * pointers to routines that do useful operations.  The low level get
 * and put routines are here too.  This lets a terminal, in addition
 * to having non standard commands, have funny get and put character
 * code too.  Some implementations using a high level interface such
 * as Windowing system define graphic widget to select a file, display
 * error and ask the user.  A "Widget" structure holds indirect
 * pointers to those functionalities.
 */

class Terminal {
 public:
  /* Create a concrete terminal at the start. */
  static Terminal* getInstance();
  /* Close terminal at end. */
  virtual ~Terminal() {
    t_init = false;
  }
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
  /* Number of rows. */
  int
  getNbRows() const noexcept {
    return t_nrow;
  }
  /* Number of columns. */
  int
  getNbCols() const noexcept {
    return t_ncol;
  }

 protected:
  /* Open terminal at the start. */
  Terminal() = default;

  void
  setNbRows(int rows) noexcept {
    if (rows <= 1) {
      t_nrow = 2;
    } else {
      t_nrow = rows;
    }
  }

  bool
  isInitialized() const noexcept {
    return t_init;
  }

  void
  setInitialized() noexcept {
    t_init = true;
  }

  void
  setNbCols(int cols) noexcept {
    t_ncol = cols;
  }

 private:
  /* Term initialized. */
  int t_init{false};
  /* Number of rows. */
  int t_nrow{0};
  /* Number of columns. */
  int t_ncol{0};
};

class Widget {
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
  CMD (*w_confirm)(const EMCHAR* s);
  /*
   * ERROR Widget
   */
  void (*w_error)(const EMCHAR* s);
  /*
   * TITLE Widget
   */
  EMCHAR* (*w_title)(EMCHAR* b, EMCHAR* f);
  /*
   * ASKER Widget
   */
  CMD (*w_asker)(const EMCHAR* s, EMCHAR* b, int n);
  /*
   * EDITOR Widget
   */
  CMD (*w_edit)(const EMCHAR* s, EMCHAR* b, int n);
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
  unset() noexcept {
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

  static CMD     diredCommand(int c);
  static EMCHAR* fileMatch(const EMCHAR* prompt, EMCHAR* file);
  static EMCHAR* fileAccept(const EMCHAR* prompt, EMCHAR* file);
  static bool    diredBuffer(const EMCHAR* fmatch);

  static CMD dired();

 private:
  Status _status;
  Callback _fn;
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

EMCHAR
EditWindow::getChar() const {
  return _dot.line()->get(_dot.pos());
}

void
EditWindow::setChar(int c) {
  _dot.line()->put(_dot.pos(), c);
}

class Options {
 public:
  /*
   *      options.cpp
   */

  static CMD describekey();
  static CMD help();
  static CMD setvar();
  static CMD uncompile();
  static CMD findtag();
  static CMD tagsloopcont();

  static int completeintag(int tagnext, const EMCHAR* tagname, EMCHAR* tagcomp);

  static int tagfound;
};

/**
 * Counter class is used to manage an integer counter that can be incremented,
 * decremented and inserted in current buffer. It is mainly used in a macro.
 */
class Counter {
 public:
  /**
   * Insert the current value of counter at dot before increment
   * it. Bound to C-X-$-$
   * @return T
   */
  static CMD
  insert() {
    EMCHAR buf[NPAT];

    (void)emsprintf(buf, &_fmt[0], _val);

    for (auto s = &buf[0]; *s; ++s) {
      (void)Line::insert(*s);
    }

    return T;
  }

  /**
   * Increment counter. Bound to C-X-$-'+'
   * @return T
   */
  static CMD
  incr() noexcept {
    _val += Editor::_repeat;

    return T;
  }

  /**
   * Decrement counter. Bound to C-X-$-'-'
   * @return T
   */
  static CMD
  decr() {
    _val -= Editor::_repeat;

    return T;
  }

  /**
   * Set the value of counter. Bound to C-X-$-S
   * @return T
   */
  static CMD
  set() {
    _val = Editor::_repeat;

    return T;
  }

  /**
   * Change the format of counter from default (%d). Bound to C-X-$-F
   * @return CMD
   */
  static CMD
  format();

 private:
  static int    _val;
  static EMCHAR _fmt[NPAT];
};

/**
 * This class searches for errors in compilation buffer.
 */
class Error {
 public:
  static void clear();
  static CMD  next();

 private:
  static bool get();

  static bool   _flag;
  static int    _linenum;
  static EMCHAR _fname[NFILEN];
};

class Search {
 public:
  static bool next();
  static void wait(int n);
  static void substitute(int length, const EMCHAR* newstr);
  static bool matchBackward(int patc, bool printflag = true);
  static bool matchForward(int patc, bool printflag = true);
  static bool autoMatch(int c, bool printflag = true);

  /*
   * Editor commands bound to key:
   */
  static CMD forward();
  static CMD backward();
  static CMD globalReplace();
  static CMD queryReplace();
  static CMD definition();
  static CMD rightParent();
  static CMD leftParent();
  static CMD rightCurly();
  static CMD leftCurly();
  static CMD rightBracket();
  static CMD leftBracket();
  static CMD complete();
  static CMD diffWindows();
  static CMD compareWindows();

 private:
  static bool prev();
};

class MiniBuf {
 public:
  static void erase();
  static int cursor();
  static CMD yn(const EMCHAR* prompt);
  static CMD yesno(const EMCHAR* prompt);
  static CMD confirm(const EMCHAR* prompt);
  static CMD edit(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
  static CMD reply(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
  static bool allowComplete(bool flag);
  static void write(const EMCHAR* fmt, ...);
  static void error(const EMCHAR* msg);
  static EMCHAR * title(EMCHAR* buffer,EMCHAR* fname);
  static CMD change(const EMCHAR* msg, EMCHAR* opat, EMCHAR* npat, int length);
  static void play(int flag);
  static void wait();
  static void message(const EMCHAR* msg);
  static void adjust();
  static void update(const EMCHAR* prompt, EMCHAR* buf);
  static void clipCopy();
  static void clipPaste();
  static void lpPrint();
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

/*
 *      spawn.cpp
 */

class Process {
 public:
  static bool syscompile(const EMCHAR* cmd, int flag);

  static CMD spawncli();
  static CMD spawn();
  static CMD makefile();
  static CMD man();
  static CMD grep();
  static CMD perl();
  static CMD sed();
  static CMD compile();
  static CMD compilecurrent();
  static CMD ccompile();
  static CMD javacompile();
  static CMD assemble();
  static CMD evalbuf();
  static CMD getcommand();
  static CMD changedir();
};
#endif /* __OBJECTS_H */
