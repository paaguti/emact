#if !defined( lint ) && defined( _EMACSLIB )
static auto rcsid("$Id: llemacs.cpp,v 1.12 2018/09/04 05:13:08 jullien Exp $");
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

#if defined( _EMACSLIB )

#include "./emacs.h"

/*
 * The functions in this file implement commands that perform
 * interactions between Emacs and other applications.
 */

static EMCHAR* call[] = {
  "",
  "",
};

#define EVAL    0x100

static  EDLINE* curline;
static  int     evalflag;

#define EXIT            0
#define EVALBUFFER      1
#define EVALFUNCTION    2

/*
 * Call emacs form an application with an optional filename.
 */

int
llemacs(EMCHAR* file) {
  evalflag = EXIT;

  if (*file) {
    call[1] = file;
    (void)emacs(2, call);
  } else {
    (void)emacs(1, nullptr);
  }

  return evalflag;
}

/*
 * Get an entire line from emacs buffer in 'buf' then point to
 * the next line.
 */

int
llembol(const EMCHAR* buf) {
  int i;

  if (curline == curbp->lastline()) {
    return -1;
  }

  for (i = 0; i < curline->length(); i++) {
    buf[i] = curline->get(i);
  }

  curline = curline->forw();
  return i;
}

/*
 * Insert a line in emacs buffer.
 */

int
llemeol(const EMCHAR *buf, int n) {
  while (n-- && EDLINE::linsert(*buf++) == T) {
    continue;
  }

  (void)endline();
}

/*
 * Position eval flag before exit from emacs.
 */

CMD
lispevalbuffer() {
  evalflag = EVALBUFFER;
  curline  = curbp->firstline();
  tidy();
}

CMD
evalfunction() {
  const auto& dot(curwp->getDot());

  (void)blispexpr();

  evalflag = EVALBUFFER;
  curline  = curwp->line();
  curwp->setDot(dot);

  tidy();
}
#endif
