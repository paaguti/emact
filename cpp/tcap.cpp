#if     !defined( lint )
static auto rcsid("$Id: tcap.cpp,v 1.1 2018/07/29 13:16:38 jullien Exp $");
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
 * The routines in this file provide support for TERMCAP package.
 */

#include  "emacs.h"

#if     defined( _TERMCAP )

#if     defined( HAVE_NCURSES_H ) && !defined( _EMACS_CURSES_DEFINED )
#include <ncurses.h>
#define _EMACS_CURSES_DEFINED
#endif  /* HAVE_NCURSES_H */

#if     defined( HAVE_CURSES_H )  && !defined( _EMACS_CURSES_DEFINED )
#include <curses.h>
#define _EMACS_CURSES_DEFINED
#endif  /* HAVE_NCURSES_H */

#if     defined( HAVE_TERM_H )
#include <term.h>
#endif

static void    tcapmove(int row, int col);
static void    tcapeeol(void);
static void    tcapeeop(void);
static void    tcapputc(int);
static void    tcapsi(void);
static void    tcapei(void);
static void    tcapbeep(void);
static void    tcapopen(void);
static int     tcapcheck(void);
static void    tcaprawmode(void);

#if     !(defined( _EMACS_CURSES_DEFINED ) && defined( HAVE_TERM_H ))
extern void    tputs(char *str, int n, int (*pfn)( int c ));
extern char *  tgoto(char *str, int col, int row);
extern char *  tgetstr(char *id, char **aera);
extern int     tgetent(char *bp, char *name);
extern int     tgetnum(char *id);
#endif

#define TCAPSLEN        1024

#define putpad(str)   tputs(str, PC, (int (*)(int))tcapputc)

static  int     PC;     /*      Pad Count               */
static  char    *CM;    /*      Cursor motion           */
static  char    *CE;    /*      Clear end of line       */
static  char    *SO;    /*      Start overintense       */
static  char    *SE;    /*      End overintense         */
static  char    *CD;    /*      Clear end of page       */

TERM term = {
  0,
  0,
  0,
  NIL,
  tcapopen,
  ttclose,
  ttgetc,
  tcapputc,
  ttputs,
  ttflush,
  tcapmove,
  tcapeeol,
  tcapeeop,
  tcapbeep,
  tcapsi,
  tcapei,
  ttcshow,
  tcapcheck,
  tcaprawmode
};

static void
tcapputc(int c) {
  fputc(c, stdout);
}

static void
tcapopen(void) {
  char    *pc;
  char    tcbuf[TCAPSLEN];
  char    *terminal;
  char    *p;

  if ((terminal = getenv("TERM")) == NULL) {
    (void)puts("TERM variable not defined");
    exit(1);
  }

  if ((tgetent(tcbuf, terminal)) != 1) {
    (void)puts("Unknown terminal type");
    (void)puts(terminal);
    exit(1);
  } else {
    p = tcbuf;
  }

  if ((pc = tgetstr("pc", &p)) != NULL) {
    PC = *pc;
  } else {
    PC = 0;
  }

  CD = tgetstr("cd", &p);
  CM = tgetstr("cm", &p);
  CE = tgetstr("ce", &p);
  SO = tgetstr("so", &p);
  SE = tgetstr("se", &p);

  if (CD == NULL || CM == NULL || CE == NULL) {
    (void)puts("Incomplete termcap entry\n");
    exit(1);
  }

  if (p >= &tcbuf[TCAPSLEN]) {
    (void)puts("Terminal description too big!\n");
    exit(1);
  }

  TTYnrow = tgetnum("li") - 1;
  TTYncol = tgetnum("co");
  TTYinit = T;
  ttopen();
}

static void
tcapmove(int row, int col) {
  putpad(tgoto(CM, col, row));
}

static void
tcapeeol(void) {
  putpad(CE);
}

static void
tcapeeop(void) {
  putpad(CD);
}

static void
tcapsi(void) {
  putpad(SO);
}

static void
tcapei(void) {
  putpad(SE);
}

static void
tcapbeep(void) {
  ttputc(0x07);
}

static  int
tcapcheck(void) {
  return 0;
}

static void
tcaprawmode(void) {
}
#endif
