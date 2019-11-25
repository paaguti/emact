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

#if !defined(__BUFFER_H)
#define __BUFFER_H
/*
 * Text is kept in buffers.  A buffer class, described below, exists
 * for every buffer in the system.  The buffers are kept in a big
 * list, so that commands that search for a buffer by name can find
 * the buffer header.  There is a safe store for the dot and mark in
 * the header, but this is only valid if the buffer is not being
 * displayed (that is, if "_count" is 0).  The text for the buffer is
 * kept in a circularly linked list of lines, with a pointer to the
 * header line in _linep.
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
#endif /* __BUFFER_H */
