#if     !defined(lint)
static  char rcsid[] = "$Id: termio.cpp,v 1.3 2018/09/02 17:48:50 jullien Exp $";
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
 * The  functions  in  this  file  negotiate  with the operating
 * system  for  characters,  and  write  characters  in a barely
 * buffered fashion on the display. All operating systems.
 */

#include "emacs.h"

#include <termios.h>

static struct termios ostate;
static struct termios nstate;

/*
 * This  function  is  called once to set up the terminal device streams.
 */

void
ttopen() {
  (void)tcgetattr(1, &ostate);
  (void)tcgetattr(1, &nstate);
  nstate.c_cc[VMIN]  = 6;
  nstate.c_cc[VTIME] = 1;
  nstate.c_iflag &= ~(IGNBRK|INLCR|ICRNL|IXON|IXOFF);
  nstate.c_iflag |= BRKINT;
  nstate.c_oflag &= ~(OPOST);
  nstate.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
  nstate.c_lflag |= ISIG;
  (void)tcsetattr(1, TCSANOW, &nstate);
}

/*
 * This  function gets called just before we go back home to the
 * command  interpreter.
 */

void
ttclose() {
  (void)tcsetattr(1, TCSANOW, &ostate);
}

/*
 * Write characters to the display.
 */

void
ttinsert(int c) {
  (void)std::fputc(c, stdout);
}

void
ttinsert(const EMCHAR* s, int n) {
  while (n--) {
    TTYputc((int)*s++);
  }
}

/*
 * Show or hide the logical cursor.  On machines with a hardware
 * cursor nothing is done.
 */

void
ttcshow(int flag) {
  ttflush();
}

/*
 * Flush  terminal  buffer.  Does  real  work where the terminal
 * output  is buffered up.  A no-operation on systems where byte
 * at a time terminal I/O is done.
 */

void
ttflush() {
  (void)std::fflush(stdout);
}

/*
 * Read  a  character  from the terminal,  performing no editing
 * and  doing  no  echo at all.
 */

int
ttgetc() {
  return std::fgetc(stdin) & MAX_EMCHAR;
}
