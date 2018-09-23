#if !defined( lint )
static char rcsid[] = "$Id: line.cpp,v 1.25 2018/09/07 17:57:09 jullien Exp $";
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
 * The  functions  in  this  file  are  a  general  set  of line
 * management  utilities.  They are the only routines that touch
 * the  text.  They also touch the buffer and window structures,
 * to  make  sure  that the necessary updating gets done.  There
 * are  routines  in  this file that handle the kill buffer too.
 * It isn't here for any good reason.
 *
 * Note  that  this code only updates the dot and mark values in
 * the  window  list.  Since  all  the  code acts on the current
 * window,  the  buffer  that  we  are  editing  must  be  being
 * displayed,  which means that count() is non zero, which means
 * that  the  dot  and  mark  values  in  the buffer headers are
 * nonsense.
 */

#include <sstream>
#include "emacs.h"

/*
 * This  routine  allocates  a  block  of memory large enough to
 * hold  a  LINE  containing  "used"  characters.  The  block is
 * always  rounded up a bit.  Return a pointer to the new block,
 * or nullptr if there isn't any memory left.  Print a message in
 * the message line if no space.
 */
EDLINE*
EDLINE::alloc(int used) {
	static constexpr auto NBLOCK(16); // Line block chunk size
  size_t size = (size_t)((used + NBLOCK - 1) & ~(NBLOCK - 1));

  if (size == 0) {
    size = NBLOCK;  // Assume that an empty line is for type-in.
  }

  const auto buflen = size * sizeof(EMCHAR);

  auto raw = new std::uint8_t[sizeof(EDLINE) + buflen];
  auto lp  = reinterpret_cast<EDLINE*>(raw);

  if (lp == nullptr) {
    TTYbeep();
    WDGerror(ECSTR("No memory left, file may be corrupted."));
    return nullptr;
  }

  lp->l_text = reinterpret_cast<EMCHAR*>(raw + sizeof(EDLINE));
  lp->l_size = static_cast<int>(size);
  lp->l_used = used;
  lp->l_bp   = lp;
  lp->l_fp   = lp;

  return lp;
}

/*
 * This routine free memory allocated for this line,
 * line is then set to nullptr.
 */
void
EDLINE::dispose(EDLINE*& lp) {
  auto raw = reinterpret_cast<std::uint8_t*>(lp);
  delete[] raw;
  lp = nullptr;
}

/*
 * Delete  line  "lp".  Fix all of the links that might point at
 * it  (they are moved to offset 0 of the next line.  Unlink the
 * line  from  whatever  buffer  it  might  be  in.  Release the
 * memory.  The  buffers  are  updated too; the magic conditions
 * described in the above comments don't hold here.
 */

void
EDLINE::free(EDLINE*& lp) {
  for (auto wp : WINSCR::list()) {
    if (wp->topline() == lp) {
      wp->setTopline(lp->forw());
    }

    if (wp->line() == lp) {
      wp->setDot(lp->forw(), 0);
    }

    if (wp->getMark().line() == lp) {
      wp->setMark(lp->forw(), 0);
    }
  }

  for (auto bp : BUFFER::list()) {
    if (bp->count() == 0) {
      if (bp->line() == lp) {
        bp->setDot(lp->forw(), 0);
      }

      if (bp->getMark().line() == lp) {
        bp->setMark(lp->forw(), 0);
      }
    }
  }

  lp->l_bp->l_fp = lp->forw();
  lp->l_fp->l_bp = lp->back();

  dispose(lp);
}

void
EDLINE::swap(EDLINE* lp2) {
  if (this != curbp->lastline()) {
    lp2->l_fp        = this->l_fp;
    this->l_fp->l_bp = lp2;
    this->l_fp       = lp2;
  } else {
    lp2->l_fp = curbp->firstline();
  }

  this->l_bp      = lp2->back();
  lp2->l_bp->l_fp = this;
  lp2->l_bp       = this;
}

void
EDLINE::remove(EDLINE* line) {
  this->l_fp  = line->l_fp;
  line->l_fp->l_bp = this;
  EDLINE::dispose(line);
}

/*
 * Return  the  position  of  last  character in the line 'line'
 * expanding tabs to the current tab_display value.
 */

int
EDLINE::position() const noexcept {
  auto str  = this->text();
  auto lmax = this->length();
  int  col  = 0;

  for (int i = 0; i < lmax; i++) {
    if (*str++ == '\t') {
      do {
        ++col;
      } while (col % opt::tab_display);
    } else {
      ++col;
    }
  }

  return col;
}

/*
 * Returns  the  left  margin  of  a  given line up to the 'max'
 * position. Tabs are expanded to spaces.
 */

int
EDLINE::leftmargin() const noexcept {
  int  ncol = 0;
  auto max  = this->length();

  for (auto buf(this->text()); max-- > 0 && separatorp(*buf); ++buf) {
    if (*buf == '\t') {
      do {
        ++ncol;
      } while (ncol % opt::tab_display);
    } else {
      if (!self_insert(*buf)) {
        ++ncol;
      }
      ++ncol;
    }
  }
  return ncol;
}

/*
 * This   function  deletes  "n"  bytes,  starting  at  dot.  It
 * understands how do deal with end of lines,  etc. It returns T
 * if  all of the characters were deleted,  and NIL if they were
 * not (because dot ran into the end of the buffer.  The "kflag"
 * is true if the text should be put in the kill buffer.
 */

bool
EDLINE::ldelete(int n, bool kflag) {
  if (freadonly()) {
    return false;
  }

  while (n != 0) {
    const auto& dot(curwp->getDot());
    if (dot.line() == curbp->lastline()) {
      /*
       * Hit end of buffer.
       */
      return false;
    }

    auto chunk = dot.line()->length() - dot.pos();  /* Size of chunk.       */
    if (chunk > n) {
      chunk = n;
    }
    if (chunk == 0) {              /* End of line, merge.  */
      lchange(WINSCR::WFHARD);
      if (!EDLINE::delnewline() || (kflag && !kinsert('\n'))) {
        return false;
      }
      --n;
      continue;
    }
    lchange(WINSCR::WFEDIT);
    auto cp1 = dot.line()->address(dot.pos());     /* Scrunch text.        */
    auto cp2 = cp1 + chunk;
    if (kflag) {            /* Kill?                */
      while (cp1 - cp2) {
        if (!kinsert(*cp1)) {
          return false;
        }
        ++cp1;
      }
      cp1 = dot.line()->address(dot.pos());
    }

    while (cp2 - dot.line()->last()) {
      *cp1++ = *cp2++;        /* take care of address */
    }

    dot.line()->setLength(dot.line()->length() - chunk);

    for (auto wp : WINSCR::list()) {
      if (wp->line() == dot.line() && wp->pos() >= dot.pos()) {
        wp->moveDotPos(-chunk);
        if (wp->pos() < dot.pos()) {
          wp->setDotPos(dot.pos());
        }
      }       

      const auto& mark(wp->getMark());
      if (mark.line() == dot.line() && mark.pos() >= dot.pos()) {
        auto marko = mark.pos() - chunk;
        if (marko < dot.pos()) {
          marko = dot.pos();
        }
        wp->setMark(mark.line(), marko);
      }
    }
    n -= chunk;
  }

  if (kflag) {
    WDGclipcopy();
  }

  return true;
}

/*
 * Delete  a newline.  Join the current line with the next line.
 * If  the  next  line is the magic header line always return true;
 * merging  the last line with the header line can be thought of
 * as  always  being a successful operation,  even if nothing is
 * done,  and  this  makes  the  kill buffer work "right".  Easy
 * cases  can  be  done  by  shuffling  data around.  Hard cases
 * require  that  lines be moved about in memory.  Return false on
 * error and true if all looks ok. Called by "ldelete" only.
 */

bool
EDLINE::delnewline() {
  auto lp1 = curwp->line();
  auto lp2 = lp1->forw();

  if (lp2 == curbp->lastline()) {
    /*
     * At the buffer end.
     */
    if (lp1->length() == 0) {
      /* Blank line. */
      EDLINE::free(lp1);
    }
    return true;
  }

  /*
   * If  not  FUNDAMENTAL,  BUFFERMODE  or  DIRED,  delete left
   * blanks and tabs.
   */

  auto cp2 = lp2->text();
  auto chunk = 0;

  if (curbp->editMode() != EDITMODE::FUNDAMENTAL &&
      curbp->editMode() != EDITMODE::DIRED       &&
      curbp->editMode() != EDITMODE::BUFFERMODE  &&
      (Editor::_thisflag & CFKILL) == 0) {
    while (cp2 - lp2->last()) {
      if (*cp2 == ' ' || *cp2 == '\t') {
        cp2++;
        chunk++;
      } else {
        break;
      }
    }
  }

  EDLINE* del = nullptr;
  auto curLen = lp1->length();
  if (lp2->length() > (lp1->size() - curLen)) {
    del = lp1;
    lp1 = lp1->realloc(curLen + lp2->length() - chunk);
  } else {
    lp1->setLength(curLen + lp2->length() - chunk);
  }

  for (auto cp(lp1->address(curLen)); cp2 - lp2->last(); ++cp) {
    *cp = *cp2++;
  }

  for (auto wp : WINSCR::list()) {
    if (wp->topline() == del || wp->topline() == lp2) {
      wp->setTopline(lp1);
    }

    if (wp->line() == del) {
      wp->setDotLine(lp1);
    } else if (wp->line() == lp2) {
      wp->setDot(lp1, wp->pos() + lp1->length());
    }

    const auto& mark(wp->getMark());
    if (mark.line() == del) {
      wp->setMark(lp1, mark.pos());
    } else if (wp->getMark().line() == lp2) {
      wp->setMark(lp1, mark.pos() + lp1->length());
    }
  }

  lp1->remove(lp2);

  if (del != nullptr) {
    dispose(del);
  }

  return true;
}

/*
 * The argument "text" points to a string.  Append this line to the
 * buffer.  Handcraft the EOL on the end.
 */

void
EDLINE::append(BUFFER* bp, const EMCHAR* text) {
  auto ntext(emstrlen(text));
  auto cur(bp->lastline());
  auto line(cur->insertBefore(ntext));

  for (int i = 0; i < ntext; ++i) {
    line->put(i, text[i]);
  }

  if (bp->line() == bp->lastline()) {
    /*
     * If "." is at the end, move it to new line
     */
    bp->setDot(line, bp->pos());
  }
}

/*
 * Insert a newline into the buffer at the current location of dot in
 * the current window.  The funny ass-backwards way it does things is
 * not a botch; it just makes the last line in the file not a special
 * case.  Return true if everything works out and false on error
 * (memory allocation failure).  The update of dot and mark is a bit
 * easier then in the above case, because the split forces more
 * updating.
 */

bool
EDLINE::newline() noexcept {
  if (freadonly()) {
    return false;
  }

  lchange(WINSCR::WFHARD);

  /*
   * Get the address and offset of "."
   */

  const auto& dot(curwp->getDot());
  auto lp1  = dot.line();
  auto doto = dot.pos();

  /*
   * Alloc new first half line
   */

  auto lp2 = lp1->insertBefore(doto);

  /*
   * Shuffle text around
   */

  auto cp1 = lp1->text();
  auto cp2 = lp2->text();

  while (cp1 - lp1->address(doto)) {
    *cp2++ = *cp1++;
  }

  cp2 = lp1->text();
  while (cp1 - lp1->last()) {
    *cp2++ = *cp1++;
  }

  lp1->setLength(lp1->length() - doto);

  for (auto wp : WINSCR::list()) {
    if (wp->topline() == lp1) {
      wp->setTopline(lp2);
    }

    if (wp->line() == lp1) {
      if (wp->pos() < doto) {
        wp->setDotLine(lp2);
      } else {
        wp->moveDotPos(-doto);
      }
    }

    const auto& mark(wp->getMark());
    if (mark.line() == lp1) {
      if (mark.pos() < doto) {
        wp->setMark(lp2, mark.pos());
      } else {
        wp->setMark(mark.line(), mark.pos() - doto);
      }
    }
  }

  return true;
}

/*
 * This  routine  gets  called  when  a  character is changed in
 * place  in the current buffer.  It updates all of the required
 * flags  in  the  buffer  and  window system.  The flag used is
 * passed  as  an  argument; if the buffer is being displayed in
 * more  than  1 window we change EDIT to HARD.  Set MODE if the
 * mode line needs to be updated (the "*" has to be set).
 */

void
lchange(int flag) {
  if (curbp->count() != 1) {
    /* Ensure hard. */
    flag = WINSCR::WFHARD;
  }

  if (!curbp->isChanged()) {    /* First change, so     */
    flag |= WINSCR::WFMODE;     /* update mode lines.   */
    curbp->setChanged(true);
  }

  for (auto wp : WINSCR::list()) {
    if (wp->buffer() == curbp) {
      wp->setFlags(flag);
    }
  }
}

/*
 * Reverse  the  effects  of lchange for the current buffer.  It
 * updates  all  of  the required flags in the buffer and window
 * system.  If  the  buffer  is being displayed in more than one
 * window  we  change  EDIT  to HARD.  Set MODE if the mode line
 * needs to be updated (the "*" has to be removed).
 */

CMD
notmodified() {
  int flag{0};

  if (curbp->count() != 1) {
    /* Ensure hard. */
    flag = WINSCR::WFHARD;
  }

  curbp->setChanged(false);
  flag |= WINSCR::WFMODE;        /* update mode lines. */

  for (auto wp : WINSCR::list()) {
    if (wp->buffer() == curbp) {
      wp->setFlags(flag);
    }
  }

  return T;
}

/*
 * Insert  "n"  copies  of  the  character  "c"  at  the current
 * location  of  dot.  In  the easy case all that happens is the
 * text  is stored in the line.  In the hard case,  the line has
 * to  be  reallocated.  When  the window list is updated,  take
 * special care; I screwed it up once.  You always update dot in
 * the  current  window.  You update mark,  and a dot in another
 * window,  if  it  is  greater than the place where you did the
 * insert. Return true if all is well, and false on errors.
 */

bool
linsert(int c, int n) {
  extern Point found;

  if (curbp->readonly()) {
    if (curbp->editMode() == EDITMODE::BUFFERMODE) {
      return buffercmd(c) == T;
    }

    if (curbp->editMode() == EDITMODE::DIRED) {
      return diredcmd(c) == T;
    }
  }

  if (freadonly()) {
    return false;
  }

  lchange(WINSCR::WFEDIT);

  if (c > 0xFF && curbp->encoding() == ENCODING::EMASCII) {
    curbp->setEncoding(ENCODING::EMUTF16);
    BUFFER::updatemodes();
  }

  auto lp1 = curwp->line();
  EDLINE* lp2;

  if (lp1 == curbp->lastline()) {
    /*
     * At the end: special
     */

    if (curwp->pos() != 0) {
      internalerror(ECSTR("pos() != 0"));
      return false;
    }

    /*
     * Insert a line of n characters c above lp1.
     */
    lp2 = lp1->insertBefore(n);

    auto cp(lp2->text());

    for (int i = 0; i < n; ++i) {
      *cp++ = (EMCHAR)c;
    }

    curwp->setDot(lp2, n);
    return true;
  }

  auto kflag = false; // in case of realloc
  auto doto  = curwp->pos();

  if ((lp1->length() + n) > lp1->size()) {
    /*
     * Hard: reallocate, lp2 is a strict copy of lp1.
     * All links have been updated and lp1 must be deleted at the end.
     */
    lp2 = lp1->realloc(lp1->length() + n);

    /*
     *      Copy the rest of the line (space is left for insertion)
     */
    auto cp1 = lp1->text() + doto;
    auto cp2 = lp2->text() + doto + n;
    for (int i = doto; i < lp1->length(); ++i) {
      *cp2++ = *cp1++;
    }

    kflag = true; // lp1 must be deleted
  } else {
    /*
     *      Easy: in place
     */

    lp2 = lp1;                      /* Pretend new line     */

    lp2->setLength(lp2->length() + n);

    auto cp2 = lp1->last();
    auto cp1 = cp2 - n;
    while (cp1 - lp1->address(doto)) {
      *--cp2 = *--cp1;
    }
  }

  for (int i = 0; i < n; ++i) {
    lp2->put(doto + i, c);
  }

  /*
   * Update mark and found.
   */

  if (found.line() == lp1) {
    found.setLine(lp2);
    if (found.pos() > doto) {
      found.setPos(found.pos() + n);
    }
  }

  for (auto wp : WINSCR::list()) {
    if (wp->topline() == lp1) {
      wp->setTopline(lp2);
    }

    if (wp->line() == lp1) {
      wp->setDotLine(lp2);
      if ((wp == curwp) || (wp->pos() > doto)) {
        wp->moveDotPos(n);
      }
    }

    const auto& mark(wp->getMark());
    if (mark.line() == lp1) {
      if (mark.pos() > doto) {
        wp->setMark(lp2, mark.pos() + n);
      } else {
        wp->setMark(lp2, mark.pos());
      }
    }
  }

  if (kflag) {
    EDLINE::dispose(lp1); /* was a realloc */
  }

  return true;
}

/*
 * Replace "n"  copies  of  the character  "c"  at  the  current
 * location  of dot.  In the easy case all that happens  is  the
 * text is replaced in the line. In the hard case, at the end of
 * the line, the routine linsert is call  with n  equal  to  the
 * number of characters alredy replaced.
 */

bool
lreplace(int c, int n) {
  if (freadonly()) {
    return false;
  }

  const auto& dot(curwp->getDot());
  auto dotp(dot.line());
  auto doto(dot.pos());

  for (auto i(0); i < n; ++i) {
    if (doto == dot.line()->length()) {
      curwp->setDotPos(doto);
      return linsert(c, n - i);
    } else {
      dotp->put(doto++, c);
    }
  }

  curwp->setDot(dotp, doto);

  lchange(WINSCR::WFHARD);
  return true;
}

/*
 * This routine exchanges two lines in the current buffer at "."
 * position with the line above  if it exists.  First  and  last
 * line  can't  be exchanged by this routine  and, in that case,
 * returns NIL.
 */

CMD
ltwiddle() {
  if (freadonly()) {
    return NIL;
  }

  auto lp1 = curwp->line(); /* Current line         */
  auto lp2 = lp1->back();   /* Line above "." line  */

  if (lp1 == curbp->firstline() || lp1 == curbp->lastline()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  for (auto wp : WINSCR::list()) {
    if (wp->topline() == lp2) {
      wp->setTopline(lp1);
    }

    if (wp->line() == lp1) {
      wp->setDot(lp2, 0);
    }

    if (wp->getMark().line() == lp1) {
      wp->setMark(lp2, 0);
    }
  }

  for (auto bp : BUFFER::list()) {
    if (bp->count() == 0) {
      if (bp->line() == lp1) {
        bp->setDot(lp2, 0);
      }
      if (bp->getMark().line() == lp1) {
        bp->setMark(lp2, 0);
      }
    }
  }

  lp1->swap(lp2);

  (void)forwline();

  return T;
}

/*
 * Change mode between insert and replace. Normally bound to M-I.
 */

CMD
instoggle() {
  opt::replace_mode = !opt::replace_mode;

  for (auto wp : WINSCR::list()) {
    display->modeline(wp);
  }

  return T;
}
