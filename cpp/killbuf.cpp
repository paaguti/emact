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

static EMCHAR* kbufp{nullptr};      // Kill buffer data
static size_t  kused{0};            // # of bytes used in KB
static size_t  ksize{0};            // of bytes allocated in KB

/**
 * return current killbuffer as a pair.
 */
std::pair<const EMCHAR*, size_t>
kget() {
  return std::pair<const EMCHAR*, size_t>(kbufp, kused);
}

/*
 * Delete  all of the text saved in the kill buffer.  Called by
 * commands when a new kill context is being created. The  kill
 * buffer  array is released, just in case the buffer has grown
 * to immense size. No errors.
 */

void
kdelete() {
  delete[] kbufp;
  kbufp = nullptr;
  kused = 0;
  ksize = 0;
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
  if (kused == ksize) {
    static constexpr size_t KBLOCK{4096};       // Kill buffer block size
    static constexpr size_t KMAX{1024 * 1024};  // MAX kill buufer size.

    /*
     * set newsize to KBLOCK or add 50 % if the
     * the kill-buffer is not empty
     */

    size_t newsize;

    if (ksize == 0) {
      newsize = KBLOCK;
    } else {
      newsize = ksize + (ksize / 2);
    }

    if (newsize > KMAX) {
      WDGerror(ECSTR("Kill buffer full."));
      return false;
    }

    auto nbufp = new EMCHAR[newsize];
    (void)std::memcpy(nbufp, kbufp, ksize * sizeof(EMCHAR));
    delete[] kbufp;

    kbufp  = nbufp;
    ksize  = newsize;
  }

  kbufp[kused++] = (EMCHAR)c;
  return true;
}

/*
 * This  function gets characters from the kill buffer.  If  the
 * character  index "n" is off the end,  it returns  "-1".  This
 * lets the caller just scan along until it gets a "-1" back.
 */

int
kremove(int n) {
  if (kbufp == nullptr || n >= (int)kused) {
    return -1;
  } else {
    return kbufp[n];
  }
}
