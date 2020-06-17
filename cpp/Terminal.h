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

#if !defined(__TERMINAL_H)
#define __TERMINAL_H
#include "./emacs.h"
/**
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
#endif /* __OBJECTS_H */
