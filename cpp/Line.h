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

#if !defined(__LINE_H)
#define __LINE_H
#include "./emacs.h"

/**
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
#endif /* __LINE_H */
