#if !defined(lint)
static auto rcsid("$Id: indent.cpp,v 1.20 2018/09/04 05:13:08 jullien Exp $");
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
 * This file manage the indentation rules of C & Lisp languages.
 */

#include "./emacs.h"

static EDLINE* nextcindent();
static bool    nexttab(int col);
static CMD     indent();
static bool    lispindent();
static bool    sgmlindent();
static bool    cindent();

int     indento{0};
EDLINE* indentp{nullptr};
int     commento{0};

/*
 * Insert spaces/tabulation up to a given position
 */

static bool
nexttab(int col) {
  int i;

  if (col < 0) {
    term->beep();
    WDGmessage(ECSTR("nexttab invalid index"));
    return false;
  }

  if (curbp->editMode() == EDITMODE::SGMLMODE   ||
      curbp->editMode() == EDITMODE::CSHARPMODE ||
      curbp->editMode() == EDITMODE::PYTHONMODE ||
      curbp->editMode() == EDITMODE::LISPMODE   ||
      curbp->editMode() == EDITMODE::JAVAMODE) {
    return EDLINE::linsert(' ', col);
  }

  if (((i = col/opt::tab_display) != 0 && EDLINE::linsert('\t', i) == false) ||
      ((i = col%opt::tab_display) != 0 && EDLINE::linsert(' ', i) == false)) {
    return false;
  } else {
    return true;
  }
}

/*
 * Next  C  indent.  Compute  the  next  level of indentation in
 * CMODE, CPPMODE, CSHARPMODE, PERLMODE PYTHONMODE or JAVAMODE.
 */

static EDLINE*
nextcindent() {
  int     c;

  auto llflag = (curwp->line() == curbp->lastline());

  (void)EDLINE::linsert('}');

  const auto& dot(curwp->getDot());
  auto oclp = dot.line();
  auto ocbo = dot.pos() - 1;
  auto res  = lmatchc('}', false);

  curwp->setDot(oclp, ocbo);

  auto clp = oclp;

  (void)forwdel();        /* delete '}' */

  if (res != true) {
    return clp;
  }

  commento = 0;

  auto llp = indentp;

  while (curwp->line() != llp) {
    auto i = lastc(curwp->line());

    if (i > 0 && !commento) {
      c = curwp->line()->get(i - 1);
    } else {
      curwp->setDotLine(curwp->line()->back());
      if (curwp->line() == llp) {
        break;
      } else {
        continue;
      }
    }

    /*
     * At this point,  curwp is the first non empty line,
     * i  is  the index of the last valid character and c
     * is value of this character.
     */

    clp = curwp->line();

    if (c == '}') {
      /*
       * The last statement is a closing block. unindent.
       */
      curwp->setDotPos(i - 1);
      if (lmatchc('}', false)) {
        if (curwp->line() == indentp) {
          curwp->setDotLine(curwp->line()->back());
        } else {
          while (curwp->line() != indentp) {
            curwp->setDotLine(curwp->line()->back());
            if (curwp->line() == llp) {
              WDGmessage(ECSTR("Indent error"));
              break;
            }
          }
        }
        clp = curwp->line();
        continue;
      } else {
        break;
      }
    }

    do {
      auto back(curwp->line()->back());
      curwp->setDotLine(back);
      if (curwp->line() == llp) {
        curwp->setDot(oclp, ocbo);
        return clp;
      }
      i = lastc(curwp->line());
    } while (i == 0);

    c = curwp->line()->get(i - 1);

    if (c != ')' && c != '{') {
      break;
    } else {
      clp = curwp->line();
    }
  }

  curwp->setDot(oclp, ocbo);

  if (llflag) {
    /*
     * Special at the end, delete the newline added by '}'
     */
    (void)forwdel();
  }

  return clp;
}

/*
 * Delete one level of indentation in CMODE,  CPPMODE,  PERLMODE
 * CSHARPMODE, PYTHONMODE or JAVAMODE buffer.
 */

bool
unindent(int c, bool f) {
  auto max(curwp->pos());

  if (max > 1 && curwp->line()->get(max - 1) == '\'') {
    return EDLINE::linsert(c);
  }

  if (EDLINE::linsert(c) != true || automatch(c, f) != true || backdel() != T) {
    return false;
  }

  if ((indentp == curwp->line())
      && (max > 1)
      && curwp->line()->get(max - 1) != '{') {
    return EDLINE::linsert(c);
  }

  while (curwp->pos() > 0) {
    if (!separatorp(curwp->line()->get(curwp->pos() - 1))) {
      if (!EDLINE::newline()) {
        return false;
      } else {
        break;
      }
    } else {
      if (backdel() == NIL) {
        return false;
      }
    }
  }

  if (!nexttab(indento)) {
    return false;
  }

  return EDLINE::linsert(c);
}

/*
 * Returns the last position of the C statement of a line.
 */

int
lastc(EDLINE* line) {
  auto buf = line->text();

  for (auto n(line->length() - 1); n >= 0; --n) {
    switch (buf[n]) {
    case ' ' :
    case '\t':
      break;
    default  :
      if (commento == 1) {
        int i;
        for (i = n; i > 0; i--) {
          if (buf[i] == '*' && buf[i-1] == '/') {
            commento = 0;
            break;
          }
        }

        if (i == 0) {
          return n + 1;
        } else {
          n = i - 1;
        }
      } else {
        return n + 1;
      }
    }
  }

  return 0;
}

/*
 * Try to indent according to C/C++/Java indentation rules.
 */

static bool
cindent() {
  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto i(lastc(clp));
  int nindent = 0;
  int ncol;
  int n;

  /*
   * Search the first non empty line above the current one.
   */

  while (clp != curbp->lastline()) {
    if ((i = lastc(clp)) > 0) {
      break;
    } else {
      clp = clp->back();
    }
  }

  switch (clp->get(i - 1)) {
  case '{' :
    n = clp->length() - 6;
    for (i = 0; i < n; ++i) {
      if (clp->get(i) == 's' && !emstrncmp(clp->text()+i, ECSTR("switch"), 6)) {
        break;
      }
    }
    if (i >= n) {
      nindent++;
    }
    break;
  case '}' :
    i = lmatchc('{', false);
    if (i) {
      ncol = indentp->leftmargin();
      return nexttab(ncol);
    }
    break;
  case '*' :
    n = clp->length();
    if (n >= (i - 2) && clp->get(i - 2) == '/') {
      /*
       * start of a C comment
       */
      ncol = clp->leftmargin();
      return nexttab(ncol+1);
    }
    break;
  case '/' :
    n = clp->length();
    if (n >= (i-2)) {
      EMCHAR c = clp->get(i - 2);

      if (c == '/') {
        /*
         * start of C++ comment
         */
        ncol = clp->leftmargin();

        return nexttab(ncol);
      }

      if (c == '*') {
        /*
         * end of C comment
         */

        i -= 2;
        while (i > 1) {
          if (clp->get(i) == '*' &&
              clp->get(i-1) == '/') {
            break;
          }
          i--;
        }

        if (i > 1) {
          break;
        }

        ncol = clp->leftmargin();
        if (ncol > 0) {
          return nexttab(ncol-1);
        } else {
          return nexttab(ncol);
        }
      }
    }
    break;
  case ')' :
  case ':' :
    if (separatorp(clp->get(0))) {
      nindent++;
    }
    break;
  default  :
    clp = nextcindent();
  }

  ncol = clp->leftmargin();

  if (nindent) {
    int ntab;
    if ((curbp->editMode() == EDITMODE::JAVAMODE)
        || (curbp->editMode() == EDITMODE::CSHARPMODE)) {
      ntab = opt::java_indent;
    } else {
      ntab = opt::tab_size;
    }
    ncol = ntab * ((ncol + ntab) / ntab);
  }

  return nexttab(ncol);
}

/*
 * Returns the last position of the lisp statement in buf.
 */

int
lastlisp(EDLINE* line) {
  auto buf = line->text();
  auto  n  = line->length();
  int     dblq = 0;
  int     i    = 0;
  int     j;

  commento = -1;

  while (i < n) {
    switch (buf[i]) {
    case '"' :
      /*
       * Is  character  '"' as a Lisp char (#\;) or
       * quoted in string ("..\"..") ?
       */
      if (i == 0 || buf[i-1] != '\\') {
        /*
         * No, increment double-quote count.
         */
        dblq++;
      }
      i++;
      break;
    case ';' :
      if (((dblq % 2) == 1) || (i > 0 && buf[i-1] == '\\')) {
        /*
         * In string or quoted, continue
         */
        i++;
        break;
      }
      commento = i;
      return i - 1;
    case ' ' :
    case '\t':
      j = i - 1;
      do {
        if (i++ >= n-1) {
           return j;
        }
      } while (separatorp(buf[i]));

      if ((dblq % 2) == 0 && buf[i] == ';') {
        /*
         * It's not in string or quoted.
         */
        commento = i;
        return j;
      }
      break;
    default  :
      i++;
    }
  }

  return n - 1;
}

/*
 * Try to indent according to the current Lisp indentation rules.
 */

#define lambdap(l, n) (emstrncmp((l->text() + n), ECSTR("lambda"), 6) == 0)

static bool
lispindent() {
  const auto& dot(curwp->getDot());
  int sexpr  = 0;
  int lambda = 0;
  int max;
  int c;

  /*
   *      Search the first non empty line above the current one.
   */

  indentp = dot.line();
  while (indentp != curbp->lastline()) {
    if (lastlisp(indentp) > 0) {
      break;
    } else {
      indentp = indentp->back();
    }
  }

  max = lastlisp(indentp);

  if (max < 0) {
    if (commento >= 0) {
      indento = commento;
    }
  } else if (indentp->get(max) == ')' && lmatchc(')')) {
    max = lastlisp(indentp);

    for (int i = 0; i <= max; ++i) {
      if ((c = indentp->get(i)) == '(') {
        if (indento == i) {
          /*
           * case:  "      (...)"
           * indent here ->_
           */
          break;
        }

        /*
         * get  the first unclosed parenthese
         * from right to left.
         */

        for (int j = max; j >= i; j--) {
          switch (indentp->get(j)) {
          case '(' : sexpr++; break;
          case ')' : sexpr--; break;
          }
          if (sexpr > 0) {
            i = j;
            break;
          }
        }

        if ((max - i) > 6 && lambdap(indentp, i+1)) {
          /*
           * case: "  (xxx (lambda"
           * indent here    ->_
           */
          ++lambda;
        }

        if (indento == i) {
          /*
           * case: "  (xxx (yyy"
           * indent here ->_
           */
          break;
        }

        if (sexpr > 0) {
          /*
           * unclosed   parenthese  was
           * found,    go    into   the
           * expression.
           */
          indento = ++i;
        }

        if ((indento > 0) && (indentp->get(indento) == '(')) {
          /*
           * case:  " ... ((...)"
           * indent here ->_
           */
          break;
        } else {
          /*
           * case:  "      (xxx yyy ... (...)"
           * indent here      ->_
           */

          if (lambda) {
            /*
             * case: "  (xxx (lambda"
             * indent here    ->_
             */
            indento = i + 2;
            break;
          }

          while (isalnum((int)indentp->get(i++))) {
            continue;
          }

          if (indento == 1) {
            indento = 3;    /* (def...  */
          } else {
            indento = i;    /* standard */
          }
        }
        break;
      }

      if (!separatorp(c)) {
        if (i == indento - 1 &&
            (indentp->get(i) == '\'' ||
             indentp->get(i) == ','  ||
             indentp->get(i) == '`')) {
          /*
           * case   " ... '(...)"
           * case   " ... `(...)"
           * case   " ... ,(...)"
           * indent here ->_
           */
          break;
        } else {
          /*
           * case:  "      xxx (...)"
           * indent here ->_
           */
          indento = i;
        }
        break;
      }
    }
  } else {
    for (int i = max, j = 0; i >= 0; i--) {
      if ((c = indentp->get(i)) == ')') {
        j++;
        continue;
      }
      if (c == '(' && --j < 0) {
        /*
         * case:  "      (xxx yy"
         * indent here      ->_
         */
        i++;
        while (isalnum((int)indentp->get(i)) && i <= max) {
          i++;
        }
        indento = i + 1;
        break;
      }
      if (!separatorp(c)) {
        /*
         * case:  "      xxx yy"
         * indent here ->_
         */
        indento = i;
      }
    }
  }

  int j = 0;
  for (int i = 0; i < indento; ++i) {
    if (indentp->get(i) == '\t') {
      do {
        ++j;
      } while (j % opt::tab_display);
    } else {
      ++j;
    }
  }

  indento = j;
  curwp->setDot(dot);
  curwp->setFlags(WINSCR::WFMOVE);

  return nexttab(indento);
}

/*
 * Try to indent according to the SGML indentation rules.
 */

static bool
sgmlindent() {
  auto clp = curwp->line();
  auto buf = clp->text();
  auto n  = clp->length();

  /*
   * find the first non-empty line above.
   */

  while (clp != curbp->lastline()) {
    if (lastc(clp) > 0) {
      break;
    } else {
      clp = clp->back();
    }
  }

  for (int i = 0; i < n; ++i) {
    switch (buf[i]) {
    case ' ' :
    case '\t':
      break;
    case '<' :
      /*
       * WAP tags only!
       */
      if (!emstrncmp(&buf[i+1], ECSTR("card"),     4) ||
          !emstrncmp(&buf[i+1], ECSTR("do"),       2) ||
          !emstrncmp(&buf[i+1], ECSTR("onevent"),  7) ||
          !emstrncmp(&buf[i+1], ECSTR("p"),        1) ||
          !emstrncmp(&buf[i+1], ECSTR("select"),   6) ||
          !emstrncmp(&buf[i+1], ECSTR("table"),    5) ||
          !emstrncmp(&buf[i+1], ECSTR("template"), 8) ||
          !emstrncmp(&buf[i+1], ECSTR("wml"),      3)) {
        return nexttab(i + 3);
      } else {
        return nexttab(i);
      }
    default:
      return nexttab(i);
    }
  }

  return nexttab(clp->leftmargin());
}

/*
 * Insert  enough  tabs  and  spaces  to respect the indentation
 * rules of the language in use (C,  C++,  Java or Lisp). Insert
 * a  newline  by  calling  the  standard  routine.  Insert  the
 * indentation  by  inserting  the  right  number  of  tabs  and
 * spaces.  Return  T  all  ok  or NIL if one of the subcommands
 * failed.
 */

static CMD
indent() {
  auto clp = curwp->line();
  int     nindent = 0;
  int     ncol;
  int     i;

  /*
   *      Try to indent.
   */

  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
  case EDITMODE::CSHARPMODE:
  case EDITMODE::JAVAMODE:
  case EDITMODE::PERLMODE:
  case EDITMODE::PYTHONMODE:
    return cindent() ? T : NIL;
  case EDITMODE::LISPMODE:
    return lispindent() ? T : NIL;
  case EDITMODE::SGMLMODE:
    return sgmlindent() ? T : NIL;
  case EDITMODE::PROLOGMODE:
  case EDITMODE::FORTRANMODE:
  case EDITMODE::SHELLMODE:
    i = 0;

    /*
     * find the first non-empty line above.
     */

    while (clp != curbp->lastline()) {
      if ((i = lastc(clp)) > 0) {
        break;
      } else {
        clp = clp->back();
      }
    }

    switch (clp->get(i - 1)) {
    case '-' :
      if (i >= 2 && clp->get(i - 2) == ':') {
        if (EDLINE::newline()) {
          return EDLINE::linsert('\t') ? T : NIL;
        } else {
          return NIL;
        }
      }
      break;
    case '.' :
      return EDLINE::newline() ? T : NIL;
    }
    break;
  default:
    break;
  }

  ncol = clp->leftmargin();

  if (nindent) {
    ncol = opt::tab_size * ((ncol + opt::tab_size) / opt::tab_size);
  }

  return nexttab(ncol) ? T : NIL;
}

/*
 * Reindent current line.
 */

CMD
tabindent() {
  (void)openline();
  (void)indent();
  (void)forwdel();

  const auto& dot(curwp->getDot());

  if (dot.pos() != dot.line()->length() && curwp->getChar() == '}') {
    (void)forwdel();
    (void)unindent('}', false);
    (void)backchar();
  }

  return T;
}

/*
 * This  function  indent  the  current  line  with the language
 * rules  in  use  (Lisp  or  C).  First,  cursor  moves  to the
 * beginning  of  the  line  and then execute a tab (actually an
 * indentation) and goes to the next line. Bound to C-X-I.
 */

CMD
indentline() {
  auto n    = Editor::_repeat;
  auto save = Editor::_repeat;

  Editor::_repeat = 1;

  if (curbp->editMode() == EDITMODE::FUNDAMENTAL ||
      curbp->editMode() == EDITMODE::DIRED       ||
      curwp->line()->length() == 0) {
    (void)forwline();
    Editor::_repeat = save;
    return NIL;
  }

  while (n-- && (gotobol() == T) && (tabindent() == T) && (forwline() == T)) {
    continue;
  }

  Editor::_repeat = save;

  return T;
}

/*
 * Break  the  current  line  in  two part.  It removes trailing
 * blanks/tabs  at the left part and indent the right.  Bound to
 * C-J.
 */

CMD
newlineindent() {
  (void)justonespace();
  (void)backchar();
  (void)openline();
  (void)forwline();
  (void)gotobol();
  (void)tabindent();

  return T;
}

/*
 * Move to the indentation point of the current line.
 * Bound to M-m.
 */

CMD
backtoindent() {
  (void)gotobol();
  const auto len(curwp->line()->length());
  for (int i{0}; i < len; ++i) {
    if (separatorp(curwp->getChar())) {
      (void)forwchar();
    } else {
      break;
    }
  }
  return T;
}

/*
 * This  function  search for the beginning of a LISP expression
 * and  set point to the first parenthese.  The beginning of the
 * is the first line with '(' at position 0. Bound to M-C-B.
 */

CMD
blispexpr() {
  EMCHAR c;

  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
  case EDITMODE::CSHARPMODE:
  case EDITMODE::JAVAMODE:
  case EDITMODE::PERLMODE:
  case EDITMODE::PYTHONMODE:
    c = '{';
    break;
  case EDITMODE::LISPMODE:
    c = '(';
    break;
  default:
    return NIL;
  }

  while (curwp->line()->get(0) != c && backline() == T) {
    continue;
  }

  curwp->setDotPos(0);

  return T;
}

/*
 * This  function  search  for  the end of a LISP expression and
 * set  point  to  the  last  parenthese.  First  seach  for the
 * beginning   of   the   expression,  then  match  the  current
 * parenthese. Bound to M-C-E.
 */

CMD
elispexpr() {
  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
  case EDITMODE::CSHARPMODE:
  case EDITMODE::PERLMODE:
  case EDITMODE::PYTHONMODE:
  case EDITMODE::JAVAMODE:
    if (blispexpr() == T && matchrcur() == T) {
      return T;
    } else {
      return NIL;
    }
  case EDITMODE::LISPMODE:
    if (blispexpr() == T && matchrpar() == T) {
      return T;
    } else {
      return NIL;
    }
  default:
    return NIL;
  }
}

/*
 * Insert only one blank around two words. Bound to M-SPACE
 */

CMD
justonespace() {
  EMCHAR c;

  if (curwp->pos() == curwp->line()->length()) {
    /*
     *      At the end, back one char.
     */
    (void)backchar();
    if ((c = curwp->getChar()) != ' ' && c != '\t') {
      (void)forwchar();
      (void)EDLINE::linsert(' ');
      return T;
    }
  }

  do {
    if (backchar() == NIL) {
      break;
    }
  } while (curwp->pos() >= 0 && ((c = curwp->getChar()) == ' ' || c == '\t'));

  if ((c = curwp->getChar()) != ' ' && c != '\t') {
    (void)forwchar();
  }

  (void)EDLINE::linsert(' ');

  while ((curwp->pos() < curwp->line()->length()) &&
         ((c = curwp->getChar()) == ' ' || c == '\t')) {
    (void)forwdel();
  }

  return T;
}
