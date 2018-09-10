/*
 * static char rcsid[] = "$Id: dirent.h,v 1.2 2018/08/28 12:03:53 jullien Exp $";
 */

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

#ifndef __DIRENT_H
#define __DIRENT_H

#if defined( __cplusplus )
extern "C" {
#endif

/*
 * Definitions for library routines operating on directories.
 */

#if !defined( __BORLANDC__ ) && !defined( __MINGW32__ )
typedef unsigned short ino_t;
#define _inode_t ino_t
#else
#define _inode_t unsigned int
#endif

#if defined( UNICODE )
typedef wchar_t _char_t;
#else
typedef char    _char_t;
#endif

struct dirent {
  /*
   * POSIX defined field
   */
  _inode_t d_ino;     /* inode number of entry */
  _char_t  d_name[1]; /* filename              */
};

struct _dirdesc;
typedef struct _dirdesc DIR;

extern DIR*            opendir(const _char_t *file);
extern struct dirent*  readdir(DIR *dirp);
extern void            rewinddir(DIR *dirp);
extern int             closedir(DIR *dirp);
#if defined( _POSIX_C_SOURCE )
extern long            telldir(DIR *dirp);
extern void            seekdir(DIR *dirp, long loc);
#endif

#if defined( __cplusplus )
}
#endif

#endif
