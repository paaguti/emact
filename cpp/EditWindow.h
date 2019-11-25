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

#if !defined(__EDIT_WINDOW_H)
#define __EDIT_WINDOW_H
#include "./emacs.h"

/**
 * There is a window class allocated for every active display
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

  EMCHAR
  getChar() const;

  void
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
#endif /* __EDIT_WINDOW_H */
