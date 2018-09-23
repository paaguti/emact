#if     !defined( lint )
static  char rcsid[] = "$Id: killbuf.cpp,v 1.3 2018/08/28 12:03:54 jullien Exp $";
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
 * The  functions  in  this  file  are  a used by the kill buufer.
 * They are the only routines that touch or return the kill buffer.
 */

#include "emacs.h"

static std::vector<EMCHAR> kvect;

/**
 * return current killbuffer as a pair.
 */
const std::pair<const EMCHAR*, size_t>
kget() {
  return std::pair<const EMCHAR*, size_t>(kvect.data(), kvect.size());
}

/*
 * Delete  all of the text saved in the kill buffer.  Called by
 * commands when a new kill context is being created. The  kill
 * buffer  array is released, just in case the buffer has grown
 * to immense size. No errors.
 */

void
kdelete() {
  kvect.resize(4096);
  kvect.clear();
}

/*
 * Insert a character to the kill buffer,  enlarging the  buffer
 * if there isn't any room.  Always grow the buffer in by a  50%
 * factor,  on the  assumption that if you put  something in the
 * kill  buffer you are going to put more stuff there too later.
 * Return T if all is well, and NIL on errors.
 */

bool
kinsert(int c) {
  kvect.emplace_back((EMCHAR)c);
  return true;
}

/*
 * This  function gets characters from the kill buffer.  If  the
 * character  index "n" is off the end,  it returns  "-1".  This
 * lets the caller just scan along until it gets a "-1" back.
 */

int
kremove(int n) {
  if (n >= (int)kvect.size()) {
    return -1;
  } else {
    return kvect[n];
  }
}
