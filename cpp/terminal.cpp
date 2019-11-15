#if !defined(lint)
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
 * The routines in this file contains the generic terminal builder.
 */

#include "./emacs.h"

#if defined(_CURSES)
extern Terminal* makeCursesTerminal();
#else
static Terminal* makeCursesTerminal() { return nullptr; }
#endif

#if defined(_WIN32)
extern Terminal* makeXpTerminal();
#else
static Terminal* makeXpTerminal() { return nullptr; }
#endif

#if defined(_X11) && !defined(X_DISPLAY_MISSING)
extern Terminal* makeX11Terminal();
#else
static Terminal* makeX11Terminal() { return nullptr; }
#endif

Terminal*
Terminal::getInstance() {
  for (auto f : { makeX11Terminal, makeCursesTerminal, makeXpTerminal }) {
    try {
      auto terminal = f();
      if (terminal != nullptr) {
        return terminal;
      }
    } catch(...) {
    }
  }

  (void)std::fprintf(stderr, "Can't open display.\n");
  exit(0);
}

