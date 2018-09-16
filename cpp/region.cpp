#if     !defined(lint)
static char rcsid[] = "$Id: region.cpp,v 1.18 2018/09/04 05:13:09 jullien Exp $";
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
 * The  routines in this file deal with the region,  that  magic
 * space between "." and mark. Some functions are commands. Some 
 * functions are just for internal use.
 */

#include "emacs.h"

/*
 * The starting position of a region, and the size of the region 
 * in  characters,  is kept in a region structure.   Used by the 
 * region commands.
 */

class REGION {
 public:
  EDLINE* _linep{nullptr};  // Origin EDLINE address
  int     _offset{0};       // Origin EDLINE offset
  int     _size{0};         // Length in characters
  int     _lines{0};        // Number of lines

  bool get();
};

/*
 * This  routine  figures  out  the  bounds of the region in the
 * current  window,  and  fills  in  the  fields of the "REGION"
 * structure  pointed  to by "rp".  Because the dot and mark are
 * usually  very  close  together,  we  scan  outward  from  dot
 * looking  for mark.  This should save time.  Return a standard
 * code.  Callers  of  this routine should be prepared to get an
 * "ABORT"  status;  we  might  make this have the conform thing
 * later.
 */

bool
REGION::get() {
  static auto rtoobig = ECSTR("Region too big.");

  const auto& mark(curwp->getMark());

  if (mark.line() == nullptr) {
    WDGmessage(ECSTR("No mark set in this buffer!"));
    return false;
  }

  _lines = 0;
  _size = 0;

  const auto& dot(curwp->getDot());

  if (dot.line() == mark.line()) {
    _linep = dot.line();
    if (dot.pos() < mark.pos()) {
      _offset = dot.pos();
      _size   = mark.pos() - dot.pos();
    } else {
      _offset = mark.pos();
      _size   = dot.pos() - mark.pos();
    }
    return true;
  }

  auto blp   = dot.line();
  auto bsize = dot.pos();

  auto flp   = dot.line();
  auto fsize = flp->length() - dot.pos() + 1;

  while (flp != curbp->lastline() || blp->back() != curbp->lastline()) {
    static constexpr size_t MAX_REGION{1024 * 1024};  // MAX region size.
    _lines++;
    if (flp != curbp->lastline()) {
      flp = flp->forw();
      if (flp == curwp->getMark().line()) {
        _linep  = dot.line();
        _offset = dot.pos();
        _size   = fsize + curwp->getMark().pos();
        return true;
      }
      fsize += flp->length() + 1;
      if ((size_t)fsize > (MAX_REGION - NLINE)) {
        WDGerror(rtoobig);
        return false;
      }
    }
    if (blp->back() != curbp->lastline()) {
      blp = blp->back();
      bsize += blp->length() + 1;
      if ((size_t)bsize >= MAX_REGION) {
        WDGerror(rtoobig);
        return false;
      }
      if (blp == curwp->getMark().line()) {
        _linep  = blp;
        _offset = curwp->getMark().pos();
        _size   = bsize - curwp->getMark().pos();
        return true;
      }
    }
  }

  WDGmessage(ECSTR("Error lost mark"));
  return false;
}

/*
 * Kill the region.  Ask "get" to figure out the bounds of the
 * region.  Move "." to the start, and kill the characters.  Bound to
 * "C-W". Region as a maximum of MAX_REGION characters in size and
 * check is made prior any operation on a region.
 */

CMD
killregion() {
  if (freadonly()) {
    return NIL;
  }

  REGION region;

  if (!region.get()) {
    return NIL;
  }

  if ((Emacs::_lastflag & CFKILL) == 0) {          /* This is a kill type  */
    kdelete();
  }

  Emacs::_thisflag |= CFKILL;
  curwp->setDot(region._linep, region._offset);

  if (ldelete(region._size, true)) {
    WDGclipcopy();
    return T;
  } else {
    return NIL;
  }
}

/*
 * Copy  all of the characters in the region to the kill buffer.
 * Don't  move  dot at all.  This is a bit like  a  kill  region
 * followed by a yank. Bound to "M-W".
 */

CMD
copyregion() {
  REGION  region;

  if (!region.get()) {
    return NIL;
  }

  if ((Emacs::_lastflag & CFKILL) == 0) {
    /* Kill type command. */
    kdelete();
  }

  Emacs::_thisflag |= CFKILL;

  auto linep = region._linep;                 /* Current line.        */
  auto loffs = region._offset;                /* Current offset.      */
  while (region._size--) {
    if (loffs == linep->length()) {
      if (!kinsert('\n')) {
        return NIL;
      }
      linep = linep->forw();
      loffs = 0;
    } else {                        /* Middle of line.      */
      if (!kinsert(linep->get(loffs))) {
        return NIL;
      }
      ++loffs;
    }
  }

  WDGclipcopy();
  return T;
}

/*
 * Lower  case region.  Zap all of the upper case characters  in
 * the  region  to lower case.  Use the region code to  set  the
 * limits. Scan the buffer, doing the changes. Call "lchange" to
 * ensure  that redisplay is done in all buffers.  Bound to "C-X
 * C-L".
 */

CMD
lowerregion() {
  if (freadonly()) {
    return NIL;
  }

  REGION region;

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  auto linep = region._linep;
  auto loffs = region._offset;

  while (region._size--) {
    if (loffs == linep->length()) {
      linep = linep->forw();
      loffs = 0;
    } else {
      int c = linep->get(loffs);
      if (isalpha(c) && isupper(c)) {
        linep->put(loffs, tolower(c));
      }
      ++loffs;
    }
  }

  return T;
}

/*
 * Fill region.  Insert the fill-prefix string on each line that
 * does not currently start with this prefix. Unbound.
 */

CMD
fillregion() {
  if (freadonly()) {
    return NIL;
  }

  REGION  region;

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  curwp->setDot(region._linep, 0);

  size_t fillmax = (size_t)emstrlen(opt::fill_prefix);

  while (region._lines--) {
    auto dotline(curwp->line());

    if ((size_t)dotline->length() < fillmax ||
        emstrncmp(dotline->text(), opt::fill_prefix, fillmax) != 0) {
      for (int i{0}; opt::fill_prefix[i]; ++i) {
        (void)linsert(opt::fill_prefix[i]);
      }
    }
    (void)forwline();
  }

  return T;
}

/*
 * Upper  case  region.  Zap all of the lower case characters in
 * the  region  to  upper  case.  Use the region code to set the
 * limits.  Scan the buffer,  doing the changes.  Call "lchange"
 * to  ensure  that  redisplay is done in all buffers.  Bound to
 * "C-X C-L".
 */

CMD
upperregion() {
  if (freadonly()) {
    return NIL;
  }

  REGION region;

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  auto linep = region._linep;
  auto loffs = region._offset;

  while (region._size--) {
    if (loffs == linep->length()) {
      linep = linep->forw();
      loffs = 0;
    } else {
      int c = linep->get(loffs);
      if (isalpha(c) && islower(c)) {
        linep->put(loffs, toupper(c));
      }
      ++loffs;
    }
  }

  return T;
}

/*
 * Write  all  of the characters in the region into a disk file.
 * Bound to "M-W".
 */

CMD
writeregion() {
  REGION region;

  if (!region.get()) {
    return NIL;
  }

  complete = filematch;

  EMCHAR fname[NFILEN];

  if (mlreply(ECSTR("Write region to file: "), fname, NFILEN) != T) {
    return NIL;
  }

  auto fd = ffopen(fname, ECSTR("w"));
  if (fd == nullptr) {
    return NIL;
  }
        
  auto linep = region._linep;                 /* Current line.        */
  auto loffs = region._offset;                /* Current offset.      */

  while (region._size--) {
    if (loffs == linep->length()) {
      /* End of line.         */
      (void)std::fputc('\n', fd);
      linep = linep->forw();
      loffs = 0;
    } else {
      (void)std::fputc(linep->get(loffs++), fd);
    }
  }

  (void)fclose(fd);

  return T;
}

/*
 * Reindent  a  region  usign  indent-line  on every line in the
 * block. Bound to M-C-\
 */

CMD
indentregion() {
  REGION region;
  auto s = T;
        
  if (freadonly()) {
    return NIL;
  }

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  curwp->setDot(region._linep, 0);

  do {
    s = indentline();
  } while (s == T && --region._lines > 0);

  return T;
}

/*
 * Reindent  a  region,  adding  a TAB at the beginning of every
 * line in the block. Bound to CX->
 */

CMD
shiftright() {
  REGION region;
        
  if (freadonly()) {
    return NIL;
  }

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  curwp->setDot(region._linep, 0);

  auto s = true;
  do {
    s = linsert('\t');
    curwp->setDot(curwp->line()->forw(), 0);
  } while (s && (--region._lines > 0));

  return T;

}

/*
 * Unindent a region,  removing a the first TAB of every line in
 * the block. Bound to CX-<
 */

CMD
shiftleft() {
  REGION region;
        
  if (freadonly()) {
    return NIL;
  }

  if (!region.get()) {
    return NIL;
  }

  lchange(WINSCR::WFHARD);

  curwp->setDot(region._linep, 0);

  do {
    if (curwp->line()->get(0) == '\t' && !ldelete(1)) {
      return NIL;
    }
    curwp->setDot(curwp->line()->forw(), 0);
  } while (--region._lines > 0);

  return T;
}
