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

#if !defined(__OBJECTS_H)
#define __OBJECTS_H

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
  /** shell mode */
  SHELLMODE
};

/**
 * Enum for anycb function
 */
enum class ANYCB {
  /** Prompt for any unsaved buf */
  PROMPT = 0x0000,
  /** Check only unsaved buffers */
  CHECK  = 0x0001
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
  constexpr EditorCommand(int code, CB fp, const EMCHAR* name)
    : k_code{code},
      k_fp{fp},
      k_name{name} {
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

class MACTAB {
 public:
  MACTAB() = default;
  void
  set(int code, EMCHAR* name, int index) {
    m_code  = code;
    m_name  = name;
    m_index = index;
  }
      
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
  setStatus(Status status) noexcept {
    _status = status;
  }

 private:
  Status _status;
  Callback _fn;
};

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

/*
 *	Header for mouse driver (if any).
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
    : _argc{argc},
      _argv{new EMCHAR*[argc + 1]} {
    auto cvt = [](const T* str) -> EMCHAR* {
                 size_t len = 0;
                 while (str[len] != 0) {
                   ++len;
                 }
                 auto res = new EMCHAR[len + 1];

                 for (int i = 0; i < (int)len; ++i) {
                   res[i] = (EMCHAR)str[i];
                 }
                 res[len] = '\000';

                 return res;
               };
    for (int i = 0; i < argc; ++i) {
      _argv[i] = cvt(argv[i]);
    }

    _argv[argc] = nullptr;
  }

  ~Editor() {
    for (int i = 0; i < _argc; ++i) {
      delete[] _argv[i];
    }
  }

  void
  engine();

  static const EMCHAR*
  getName() {
    return _name;
  };

 public:
  static EMCHAR*
  searchBuffer() noexcept {
    return &_search[0];
  }

  static std::array<MACTAB, NMAX>&
  getMacros() {
    return _mactab;
  }

  static std::vector<EditorCommand> _keytab;
  /* User macros table */
  static std::array<MACTAB, NMAX> _mactab;
  static std::vector<MACTAB> _macros;
  static int _nmactab;
  static int _thisflag;                   // Flags, this command
  static int _lastflag;                   // Flags, last command
  static int _repeat;                     // Repeat count
  static int _curgoal;                    // Goal for C-P, C-N
 private:
  int _argc{0};
  std::unique_ptr<EMCHAR*[]> _argv{nullptr};

  static const EMCHAR* _name;
  static EMCHAR _search[NPAT];

#if 0
  static int repeat;                 // Repeat count
  static int thisflag;               // Flags, this command
  static int lastflag;               // Flags, last command
#endif
};
#endif /* __OBJECTS_H */
