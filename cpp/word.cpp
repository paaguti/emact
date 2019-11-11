#if !defined(lint)
static auto rcsid("$Id: word.cpp,v 1.9 2018/09/02 14:06:50 jullien Exp $");
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

#include "./emacs.h"

/*
 * Return  T  if  the  character  at  dot is a character that is
 * considered  to be part of a word.  The word character list is
 * hard coded. Should be setable.
 */

bool
Word::inword() {
  const auto& dot(curwp->getDot());

  if (dot.line() == curbp->lastline() || dot.pos() == dot.line()->length()) {
    return false;
  }

  int c = curwp->getChar();

  /*
   * Check for alphanumeric or '$' or '_'
   */

  if (isalnum(c) || (c == '$') || (c == '_')) {
    return true;
  }

  /*
   * Lisp mode add few extra valid characters
   */

  if (curbp->editMode() == EDITMODE::LISPMODE) {
    switch (c) {
    case '-' :
    case '+' :
    case '*' :
    case '%' :
      return true;
    }
  }

  if (c & 0x80) {
    return true;
  }

  return false;
}

/*
 * Returns word at cursor in buffer 'buf'. Used internally.
 */

bool
Word::atCursor(EMCHAR* buf, size_t len) {
  auto cbo = curwp->pos();

  /*
   *      At the end of the word ??
   */

  if (!inword() && curwp->pos() > 0) {
    (void)Editor::backchar();
  }

  if (!inword()) {
    curwp->setDotPos(cbo);
    return false;
  }

  if ((Word::forward() == T) && (Word::backward() == T)) {
    size_t i{0};
    while (inword() && i < (len - 1)) {
      buf[i++] = curwp->getChar();
      if (Editor::forwchar() == NIL) {
        break;
      }
    }

    buf[i] = '\000';
  }

  curwp->setDotPos(cbo);

  return true;
}

/*
 * Move the cursor backward by "n" words.  All of the details of
 * motion   are  performed  by  the  "backchar"  and  "forwchar"
 * routines. Error if you try to move beyond the buffers.
 */

CMD
Word::backward() {
  int n = Editor::_repeat;

  if (Editor::backchar() == NIL) {
    return NIL;
  }

  while (n--) {
    while (!inword()) {
      if (Editor::backchar() == NIL) {
        return NIL;
      }
    }

    while (inword()) {
      const auto& dot(curwp->getDot());
      if (dot.line() == curbp->firstline() && dot.pos() == 0) {
        return T; /* start of buffer */
      } else if (Editor::backchar() == NIL) {
        return NIL;
      }
    }
  }

  return Editor::forwchar();
}

/*
 * Move the cursor forward by the specified number of words. All
 * of  the  motion is done by "forwchar".  Error if you try  and
 * move beyond the buffer's end.
 */

CMD
Word::forward() {
  int n = Editor::_repeat;

  while (n--) {
    while (!inword()) {
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }

    while (inword()) {
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }
  }
  return T;
}

/*
 * Move the cursor forward by the specified number of words. As
 * you move, convert any characters to upper case. Error if you
 * try and move beyond the end of the buffer. Bound to "M-U".
 */

CMD
Word::upper() {
  if (freadonly()) {
    return NIL;
  }

  int n = Editor::_repeat;

  while (n--) {
    while (!inword()) {
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }

    while (inword()) {
      int c = curwp->getChar();
      if (std::isalpha(c) && std::islower(c)) {
        curwp->setChar(std::toupper(c));
        Buffer::change(Window::WFHARD);
      }
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }
  }

  return T;
}

/*
 * Move the cursor forward by the specified number of words. As
 * you move convert characters to lower case. Error if you  try
 * and move over the end of the buffer. Bound to "M-L".
 */

CMD
Word::lower() {
  if (freadonly()) {
    return NIL;
  }

  int n = Editor::_repeat;
  while (n--) {
    while (!inword()) {
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }

    while (inword()) {
      int c = curwp->getChar();
      if (std::isalpha(c) && std::isupper(c)) {
        curwp->setChar(std::tolower(c));
        Buffer::change(Window::WFHARD);
      }
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }
  }

  return T;
}

/*
 * Move the cursor forward by the specified number of words. As
 * you  move convert the first character of the word  to  upper
 * case, and subsequent characters to lower case.  Error if you
 * try and move past the end of the buffer. Bound to "M-C".
 */

CMD
Word::capitalize() {
  int n = Editor::_repeat;

  if (freadonly()) {
    return NIL;
  }

  while (n--) {
    while (!inword()) {
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
    }

    if (inword()) {
      int c = curwp->getChar();
      if (std::isalpha(c) && std::islower(c)) {
        curwp->setChar(std::toupper(c));
        Buffer::change(Window::WFHARD);
      }
      if (Editor::forwchar() == NIL) {
        return NIL;
      }
      while (inword()) {
        c = curwp->getChar();
        if (std::isalpha(c) && std::isupper(c)) {
          curwp->setChar(std::tolower(c));
          Buffer::change(Window::WFHARD);
        }
        if (Editor::forwchar() == NIL) {
          return NIL;
        }
      }
    }
  }

  return T;
}

/*
 * Kill forward by "n" words. Bound to "M-D".
 */

CMD
Word::delForward() {
  int n  = Editor::_repeat;
  int sv = Editor::_repeat;

  Editor::_repeat = 1;
  while (n--) {
    while (!inword()) {
      if (forwdel() == NIL) {
        return NIL;
      }
    }
    while (inword()) {
      if (forwdel() == NIL) {
        return NIL;
      }
    }
  }
  Editor::_repeat = sv;

  return T;
}

/*
 * Kill  backwards by "n" words.  Bound to  "M-Rubout"  and to
 * "M-Backspace".
 */

CMD
Word::delBackward() {
  auto n  = Editor::_repeat;
  auto sv = Editor::_repeat;

  Editor::_repeat = 1;
  while (n--) {
    while (Editor::backchar() == T && !inword()) {
      if (forwdel() == NIL) {
        return NIL;
      }
    }
    while (forwdel() == T && Editor::backchar() == T) {
      if (!inword()) {
        break;
      }
    }
  }

  (void)Editor::forwchar();
  Editor::_repeat = sv;

  return T;
}

/*
 * Swap the current and the next words. Text between the words
 * is not altered.  After  the  exchange,  point is positioned
 * after the two words.
 */

CMD
Word::twiddle() {
  int     i;
  EMCHAR  word1[NPAT];
  EMCHAR  word2[NPAT];

  if ((Word::forward() == NIL) || (Word::backward() == NIL)) {
    return NIL;
  }

  /*
   * xxxxx,   (yyyyy)
   *           ^
   *           @
   */

  const auto& dot(curwp->getDot());
  auto dotp = dot.line();
  auto doto = dot.pos();

  /*
   * delete right word first and store it in 'word1'
   */

  for (i = 0; inword() && i < NPAT - 1; ++i) {
    word1[i] = curwp->getChar();
    (void)forwdel();
  }
  word1[i] = '\0';
  doto += i;

  /*
   * go to previous word
   *
   * xxxxx,   ()
   * ^         @
   */

  (void)Word::backward();

  /*
   * then delete left word and store it in 'word2'
   */

  for (i = 0; inword() && i < NPAT - 1; ++i) {
    word2[i] = curwp->getChar();
    (void)forwdel();
  }

  word2[i] = '\0';
  doto -= i;

  /*
   * insert word stored in 'word1'
   *
   * yyyyy,   ()
   *      ^    @
   */

  for (auto j(0); word1[j]; ++j) {
    (void)Line::insert(word1[j]);
  }

  /*
   * go to saved position and insert word stored in 'word2'
   *
   * yyyyy,   (xxxxx)
   *           @    ^
   */

  while (curwp->line() != dotp) {
    if (Editor::forwline() == NIL) {
      return NIL;
    }
  }

  if (doto > 0 && doto < curwp->line()->length()) {
    curwp->setDotPos(doto);
  }

  for (auto j(0); word2[j]; ++j) {
    (void)Line::insert(word2[j]);
  }

  return T;
}
