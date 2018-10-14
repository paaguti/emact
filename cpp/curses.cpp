#if     !defined(lint)
static auto rcsid("$Id: curses.cpp,v 1.22 2018/09/07 17:57:09 jullien Exp $");
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
 * The routines in this file provide support for CURSES package.
 */

#include "./emacs.h"

#if defined(_CURSES)

#if defined(_POSIX_C_SOURCE) || defined(HAVE_TERMIOS_H)
#include <termios.h>
#endif

#if defined(HAVE_NCURSESW_CURSES_H)
#  include <ncursesw/curses.h>
#elif defined(HAVE_NCURSESW_H)
#  include <ncursesw.h>
#elif defined(HAVE_NCURSES_CURSES_H)
#  include <ncurses/curses.h>
#elif defined(HAVE_NCURSES_H)
#  include <ncurses.h>
#elif defined(HAVE_CURSES_H)
#  include <curses.h>
#else
#  error "SysV or X/Open-compatible Curses header file required"
#endif

/*
 * not mandatory by POSIX
 */

#if !defined(_POSIX_VDISABLE)
#if defined(CDISABLE)
#define _POSIX_VDISABLE         CDISABLE
#else
#define _POSIX_VDISABLE         ((unsigned char)'\377')
#endif
#endif

#if     defined(_X11)
//#define term    cursesterm
#endif

#if defined(COLOR_PAIR)
static constexpr const decltype(COLOR_BLACK) _colortable[] = {
  COLOR_BLACK,
  COLOR_BLUE,
  COLOR_GREEN,
  COLOR_CYAN,
  COLOR_RED,
  COLOR_MAGENTA,
  COLOR_YELLOW,
  COLOR_WHITE
};
#endif

class CursesTerminal final : public Terminal {
 public:
  /* Open terminal at the start. */
  CursesTerminal();
  /* Close terminal at end. */
  ~CursesTerminal();
  /* Get character from keyboard. */
  int
  get() override;
  /* Put character to display. */
  void
  insert(int c) override;
  /* Put a string to display. */
  void
  insert(const EMCHAR* s, int n) override;
  /* Flush output buffers. */
  void
  flush() override;
  /* Move the cursor, origin 0. */
  void
  move(int x, int y) override;
  /* Erase to end of line. */
  void
  eeol() override;
  /* Erase to end of page. */
  void
  eeop() override;
  /* Beep. */
  void
  beep() override;
  /* Start inverse video */
  void
  si() override;
  /* End   inverse video */
  void
  ei() override;
  /* Show/Hide the cursor */
  void
  cshow(bool flag) override;
  /* Check event */
  bool
  check() override;
  /* Put in raw mode */
  void
  rawmode() override;

 private:
  void setmode();
  void getmode();

  int _x;        /* current X position (line) */
  int _y;        /* current Y position (row)  */
  int _color;    /* current color             */

#if defined(_POSIX_C_SOURCE) || defined(HAVE_TERMIOS_H)
  struct termios  _ostate;
  struct termios  _nstate;
#endif
};

/*
 * Curses raw mode is sometimes half-cooked.  That is, some control
 * caracters are still intercepted by OS (Ctrl-O Ctrl-V, Ctrl-K, ..
 * rawmode go further and changes terminal to send all characters
 * directly to EmACT.
 */

void
CursesTerminal::rawmode() {
#if defined(_POSIX_C_SOURCE) || defined(__QNX__)
  (void)::tcgetattr(1, &_nstate);
  _nstate.c_iflag &= ~(IGNBRK|INLCR|ICRNL|IXON|IXOFF);
  _nstate.c_iflag |= BRKINT;
  _nstate.c_oflag &= ~(OPOST);
  _nstate.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
  _nstate.c_lflag |= ISIG;

  /*
   * Disable  the  value of character that can confuse terminal
   * in non canonical mode.
   */

  for (int i = 0; i < NCCS; ++i) {
    switch (i) {
    case VQUIT:
      break;
    default:
      _nstate.c_cc[i] = _POSIX_VDISABLE;
    }
  }

  /*
   * Set intercharacter timer and accept at least 6 characters
   */

  _nstate.c_cc[VMIN]  = 1;      /* has been 6 for a while */
  _nstate.c_cc[VTIME] = 1;
  (void)::tcsetattr(1, TCSANOW, &_nstate);
#endif
}

void
CursesTerminal::getmode() {
#if defined(_POSIX_C_SOURCE)
  (void)::tcgetattr(1, &_ostate);
#endif
}

void
CursesTerminal::setmode() {
#if defined(_POSIX_C_SOURCE)
  (void)::tcsetattr(1, TCSANOW, &_ostate);
  std::printf("Exit ...\n");
  std::fgetc(stdin);
#endif
}

CursesTerminal::CursesTerminal() {
//  this->getmode();

  (void)::initscr();
//  (void)::cbreak();
  (void)::raw();
  (void)::noecho();
  /*
   * no implementation should need this
   * (void)::nonl();
   */
#if defined(KEY_HOME)
  (void)::keypad(stdscr, 1);
#endif

//  this->rawmode();
  this->setNbCols(COLS);
  this->setNbRows(LINES - 1);

#if defined(COLOR_PAIR)
  if (::start_color() == OK) {
    if (opt::monochrome_monitor) {
      (void)::init_pair(1, COLOR_WHITE, COLOR_BLACK);
      (void)::init_pair(2, COLOR_BLACK, COLOR_WHITE);
    } else {
      (void)::init_pair(1,
                        _colortable[opt::foreground_color],
                        _colortable[opt::background_color]);
      (void)::init_pair(2, COLOR_BLACK,  COLOR_WHITE);
    }
  } else {
    opt::monochrome_monitor = true;
  }
  _color = COLOR_PAIR(1);
#endif

  /*
   * Use package functions!!
   */

  this->move(0, 0);
  this->eeop();
  this->flush();
  this->setInitialized();
}

CursesTerminal::~CursesTerminal() {
  /*
   * Use native curses functions!!
   */

  _color = 0;
  (void)::move(0, 0);
  (void)::clear();
  (void)::move(0, 0);
  (void)::refresh();

  (void)::noraw();
  (void)::endwin();
}

int
CursesTerminal::get() {
  static constexpr auto CTLZCH(0x1A);         // Ctrl-Z char

  auto c = getch();

  switch (c) {
#if defined(KEY_HOME)
  case KEY_DOWN      : return (Ctrl|'N');
  case KEY_UP        : return (Ctrl|'P');
  case KEY_LEFT      : return (Ctrl|'B');
  case KEY_RIGHT     : return (Ctrl|'F');
  case KEY_HOME      : return (META|'<');
  case KEY_BACKSPACE : return (Ctrl|'H');
#if defined(__FreeBSD__)
  case KEY_DC        : return (Ctrl|'H');
#else
  case KEY_DC        : return (Ctrl|'D');
#endif
  case KEY_IC        : return (META|'I');
  case KEY_NPAGE     : return (Ctrl|'V');
  case KEY_PPAGE     : return (META|'V');
  case KEY_F(1)      : return (META|'?');
  case KEY_F(2)      : return (Ctrl|'S');
  case KEY_F(3)      : return (META|Ctrl|'F');
  case KEY_F(4)      : return (CTLX|Ctrl|'I');
  case KEY_F(6)      : return (CTLX|METACH);
  case KEY_F(7)      : return (CTLZCH);
  case KEY_F(8)      : return (CTLX|Ctrl|'S');
  case KEY_F(9)      : return (CTLX|'E');
  case KEY_F(11)     : return (META|'M');
  case KEY_ENTER     : return (0x0D);
  case KEY_LL        : return (META|'<');
#if defined(KEY_A1)
  case KEY_A1        : return (META|'>');
  case KEY_A3        : return (META|'V');
  case KEY_C1        : return (META|'<');
  case KEY_C3        : return (Ctrl|'V');
#endif
#if defined(KEY_END)
  case KEY_END       : return (META|'>');
#endif
#endif
  default            : return (c);
  }
}

void
CursesTerminal::insert(int c) {
#if defined(COLOR_PAIR)
  if (opt::monochrome_monitor) {
    (void)::addch((chtype)c);
  } else {
    (void)::addch((chtype)(c | _color));
  }
#else
  (void)::addch(c);
#endif
  ++_y;
}

void
CursesTerminal::insert(const EMCHAR* s, int n) {
  while (n-- > 0) {
    this->insert((int)*s++);
  }
}

void
CursesTerminal::flush() {
  (void)::refresh();
}

void
CursesTerminal::move(int x, int y) {
  _x = x;
  _y = y;

  (void)::move(x, y);
}

void
CursesTerminal::eeol() {
#if defined(_CURSES_CLEARS)
  (void)clrtoeol();
#else   /* _CURSES_CLEARS */
  for (auto i = _y; i < COLS; ++i) {
    (void)::move(_x, i);
    (void)::addch((chtype)(' ' | _color));
  }
#endif  /* _CURSES_CLEARS */
}

void
CursesTerminal::eeop() {
#if defined(_CURSES_CLEARS)
  (void)clear();
#else   /* _CURSES_CLEARS */
  for (int i = 0; i < LINES; ++i) {
    this->move(i, 0);
    this->eeol();
  }
#endif  /* _CURSES_CLEARS */
}

void
CursesTerminal::beep() {
#if defined(_BSD) || defined(sun) || defined(__linux__)
  (void)std::fputc(7, stdout);
#else
  (void)::beep();
#endif
}

void
CursesTerminal::si() {
#if defined(COLOR_PAIR)
  if (opt::monochrome_monitor) {
    (void)standout();
  } else {
    (void)attrset((chtype)(_color = COLOR_PAIR(2)));
  }
#else
  (void)standout();
#endif
}

void
CursesTerminal::ei() {
#if defined(COLOR_PAIR)
  if (opt::monochrome_monitor) {
    (void)standend();
  } else {
    (void)attrset((chtype)(_color = COLOR_PAIR(1)));
  }
#else
  (void)standend();
#endif
}

void
CursesTerminal::cshow(bool flag) {
  (void)flag;
  (void)::refresh();
}

bool
CursesTerminal::check() {
  return false;
}

Terminal*
makeCursesTerminal() {
  return new CursesTerminal;
}
#endif
