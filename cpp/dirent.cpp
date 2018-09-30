#if     !defined(lint)
static auto rcsid("$Id: dirent.cpp,v 1.4 2018/09/09 07:25:14 jullien Exp $");
#endif

#if defined(_WIN32) && !defined(_WINDOWS_SOURCE)
#define _WINDOWS_SOURCE
#endif

#if     defined(_WIDECHARS)
#if     !defined(_UNICODE)
#define _UNICODE
#endif
#if     !defined(UNICODE)
#define UNICODE
#endif
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
 *      NAME
 *              opendir, readdir, telldir, seekdir, rewinddir, closedir
 *              - directory operations.
 *
 *      SYNTAX
 *              #include <dirent.h>
 *
 *              DIR *opendir(filename)
 *              const char *filename;
 *
 *              struct direct *readdir(dirp)
 *              DIR     *dirp;
 *
 *              void rewinddir(dirp)
 *              DIR     *dirp;
 *
 *              int closedir(dirp)
 *              DIR     *dirp;
 *
 *      POSIX EXTENSIONS
 *              long telldir(dirp)
 *              DIR     *dirp;
 *
 *              void seekdir(dirp, loc)
 *              DIR     *dirp;
 *              long    loc;
 *
 *      DESCRIPTION
 *              Read directory entries with POSIX 1003.1 interface.
 *
 *      SEE ALSO
 *              see DIRECTORY(3) for more informations.
 *
 */

#if     defined(_MSC_VER)
#define _CRT_SECURE_NO_DEPRECATE        1
#define _CRT_NONSTDC_NO_DEPRECATE       1
#endif

#if     !defined(__EMACS_H) || defined(_DIRECTORY)

/* LINTLIBRARY */

#include        <stdio.h>
#include        <string.h>
#include        <stdlib.h>
#if     !defined(_WIN32_WCE)
#include        <io.h>
#endif  /* _WIN32_WCE */

#if     defined(_OS2) && !defined(FINDHANDLE)
#define I_STD   0x0000          /* Normal file - No restrictions */
#define I_RDO   0x0001          /* Read only file                */
#define I_HID   0x0002          /* Hidden file                   */
#define I_SYS   0x0004          /* System file                   */
#define I_LAB   0x0008          /* Volume ID file                */
#define I_DIR   0x0010          /* Subdirectory                  */
#define I_ARC   0x0020          /* Archive file                  */

#define FINDATTRIB      I_STD|I_RDO|I_HID|I_DIR

#define INCL_DOSMISC
#define INCL_DOSFILEMGR
#include        <os2.h>
#if     !defined(INCL_16)
typedef ULONG   UCOUNT;
#else
typedef USHORT  UCOUNT;
#endif
static  UCOUNT  count;

#define FINDHANDLE              unsigned int
#if     defined(_OS2V1)
#define FINDDATA                FILEFINDBUF
#if     !defined(FIL_STANDARD)
#define FIL_STANDARD            0L
#endif
#else
#define FINDDATA                FILEFINDBUF3
#endif
#define filename                achName
#define SysCloseDir(hdir)       DosFindClose((HDIR)hdir);
#endif  /* OS2 */

#if     defined(MSDOS) && !defined(FINDHANDLE)
#include        <dos.h>

#define I_STD   0x0000          /* Normal file - No restrictions */
#define I_RDO   0x0001          /* Read only file                */
#define I_HID   0x0002          /* Hidden file                   */
#define I_SYS   0x0004          /* System file                   */
#define I_LAB   0x0008          /* Volume ID file                */
#define I_DIR   0x0010          /* Subdirectory                  */
#define I_ARC   0x0020          /* Archive file                  */

#define FINDATTRIB      I_STD|I_RDO|I_HID|I_DIR

#if     defined(_NO_DOS_FIND)
#if     defined(__SC__)
#pragma pack(1)
#endif

struct _find_t {
  unsigned char    reserved[21];          /* DOS use (find next)    */
  unsigned char    attrib;                /* attribute found        */ 
  unsigned short   wr_time;               /* file's time            */
  unsigned short   wr_date;               /* file's last write      */
  long             size;                  /* size of current file   */
  char             name[13];              /* file name              */
};

typedef struct _find_t DOSFINDBUF;

#if     defined(__SC__)
#pragma pack()
#endif

static  int     _dos_findfirst(_char_t *buf, int attribute, struct _find_t *dp);
static  int     _dos_findnext(struct _find_t *dp);
static  void    SYSCALL(unsigned int syscall);
static  void    install(DIR *dp);
static  void    restore(void);
#endif

#define FINDHANDLE              unsigned int
#define FINDDATA                DOSFINDBUF
#define filename                name
#define SysCloseDir(hdir)
#endif  /* MSDOS */

#if     (defined(_WIN32) || defined(_WIN64)) && !defined(FINDHANDLE)
#include        <limits.h>
#include        <windows.h>
#if     !defined(_WINDOWS_SOURCE)
#define _WINDOWS_SOURCE
#endif
#define FINDHANDLE              HANDLE
#define FINDDATA                WIN32_FIND_DATA
#define filename                cFileName
#define SysCloseDir(hdir)       FindClose(hdir);
#endif  /* _WIN32 || _WIN64 */

#include        "dirent.h"

struct _dirdesc {
  /*
   *      DIR public (portable) interface
   */
  unsigned int    status;                 /* Status flag            */
  long            dd_loc;                 /* Current location       */
  _char_t *       dd_path;                /* Path name              */
  struct dirent * dd_direct;              /* Pointer to direct      */
  /*
   *      DIR private system interface
   */
  FINDHANDLE      dd_hfind;               /* Handle to find         */
  FINDDATA        dd_resbuf;              /* Find data              */
};

#if     defined(_UNICODE)
#define _pstrcpy        wcscpy
#define _pstrcat        wcscat
#define _pstrlen        wcslen
#define _STR(s)         (L ## s)
#else
#define _pstrcpy        strcpy
#define _pstrcat        strcat
#define _pstrlen        strlen
#define _STR(s)         (s)
#endif

#if     !defined(NAME_MAX)
#if     defined(FILENAME_MAX)
#define NAME_MAX        FILENAME_MAX
#else
#if     defined(MSDOS)
#define NAME_MAX        13
#else
#define NAME_MAX        255
#endif  /* MSDOS                */
#endif  /* FILENAME_MAX */
#endif  /* NAME_MAX     */

DIR*
opendir(const _char_t* dirname) {
  DIR* dp;
  size_t  len;

  dp = (DIR*)malloc(sizeof(DIR));

  if (dp == (DIR*)NULL) {
    return NULL;
  }

  len = sizeof(struct dirent) + (NAME_MAX * sizeof(_char_t));

  dp->dd_direct = (struct dirent *)malloc(len);
  dp->dd_hfind  = (FINDHANDLE)0;

  if (dp->dd_direct == (struct dirent *)NULL) {
    free(dp);
    return NULL;
  }

  len         = sizeof(_char_t) * ((size_t)_pstrlen(dirname)+1);
  dp->dd_path = (_char_t *)malloc(len);

  if (dp->dd_path == NULL) {
    free(dp->dd_direct);
    free(dp);
    return NULL;
  }

  (void)_pstrcpy(dp->dd_path, dirname);

  rewinddir(dp);

  if (dp->status) {
    free(dp->dd_path);
    free(dp->dd_direct);
    free(dp);
    return NULL;
  }

  return dp;
}

struct dirent*
readdir(DIR* dp) {
  struct dirent *dent = dp->dd_direct;

  if (dp->dd_loc != 0) {
#if defined(_OS2)
    count = 1;
    dp->status = DosFindNext(
      dp->dd_hfind,
      &dp->dd_resbuf,
      (ULONG)sizeof(dp->dd_resbuf),
      &count
      );
#endif
#if             defined(_WINDOWS_SOURCE)
    dp->status = (FindNextFile(dp->dd_hfind, &dp->dd_resbuf) != TRUE);
#endif
#if             defined(MSDOS)
    dp->status = _dos_findnext(&dp->dd_resbuf);
#endif
  }

  if (dp->status) {
    return (struct dirent *)NULL;
  }

#if     defined(_WINDOWS_SOURCE) && defined(_OEM_CONVERT)
  (void)CharToOem(dp->dd_resbuf.filename, dent->d_name);
#else
  (void)_pstrcpy(&dent->d_name[0], (_char_t *)&dp->dd_resbuf.filename[0]);
#endif

#if     !defined(_WINDOWS_SOURCE) || defined(_LOWERCASE)
  (void)strlwr(dent->d_name);
#endif

  dent->d_ino = (_inode_t)dp->dd_loc++;

  return dent;
}

void
rewinddir(DIR* dp) {
  _char_t buf[NAME_MAX + 8];
  _char_t c;

  (void)_pstrcpy(buf, dp->dd_path);

  if (((c = buf[(int)_pstrlen(buf)-1]) != '/') && c != '\\') {
    (void)_pstrcat(buf, _STR("\\*.*"));
  } else {
    (void)_pstrcat(buf, _STR("*.*"));
  }

  if (dp->dd_hfind) {
    SysCloseDir(dp->dd_hfind);
  }

#if     defined(_OS2)
  count        = 1;
  dp->dd_hfind = HDIR_CREATE;
  dp->status   = DosFindFirst(
    (_char_t *)&buf[0],
    (PHDIR)&dp->dd_hfind,
    (ULONG)FINDATTRIB,
    &dp->dd_resbuf,
    (ULONG)sizeof(dp->dd_resbuf),
    (PULONG)&count,
    (ULONG)FIL_STANDARD
    );

  if (_osmode == DOS_MODE) {
    dp->dd_hfind = HDIR_SYSTEM;     /* real mode */
  }
#endif  /* _OS2 */
#if     defined(_WINDOWS_SOURCE)
  dp->dd_hfind = FindFirstFile((LPTSTR)&buf[0], &dp->dd_resbuf);
  dp->status   = (dp->dd_hfind == INVALID_HANDLE_VALUE);
#endif  /* _WINDOWS_SOURCE */
#if     defined(MSDOS)
  dp->dd_hfind = 1;
  dp->status   = _dos_findfirst(buf, FINDATTRIB, &dp->dd_resbuf);
#endif  /* MSDOS */

  dp->dd_loc   = 0;
}

int
closedir(DIR* dp) {
  if (dp->dd_hfind) {
    SysCloseDir(dp->dd_hfind);
  }

  free(dp->dd_path);
  free(dp->dd_direct);
  free(dp);

  return 0;
}

#if defined(_POSIX_C_SOURCE)

long
telldir(DIR* dirp) {
  return dirp->dd_loc;
}

void
seekdir(DIR* dirp, long loc) {
  rewinddir(dirp);

  while ((dirp->dd_loc < loc-1) && !dirp->status) {
    (void)readdir(dirp);
  }
}

#endif

#if     defined(MSDOS) && defined(_NO_DOS_FIND)

/*
 *      Old interface with int86 routines (obsolete).
 */

#define GETDTA          0x2F00
#define SETDTA          0x1A00
#define FINDFIRST       0x4E00
#define FINDNEXT        0x4F00

#define AX              sys_regs.x.ax
#define BX              sys_regs.x.bx
#define CX              sys_regs.x.cx
#define DX              sys_regs.x.dx
#define DS              seg_regs.ds
#define ES              seg_regs.es

union   REGS            sys_regs;
static  unsigned  int   odp_off;

#if  defined(M_I86CM) || defined(M_I86LM) || defined(M_I86HM) \
  || defined(__COMPACT__) || defined(__MEDIUM__) || defined(__LARGE__)

static  unsigned  int   odp_seg;
struct  SREGS   seg_regs;
#define SYSTEM()                int86x(0x21, &sys_regs, &sys_regs, &seg_regs)
#define getseg(dta)             ((unsigned int)((unsigned long)dta >> 16))
#define getoffset(dta)          ((unsigned int)((unsigned long)dta))
#define setseg(seg, val)        (seg = val)

#else

#define SYSTEM()                int86(0x21, &sys_regs, &sys_regs);
#define getseg(dta)
#define getoffset(dta)          ((unsigned int)dta)
#define setseg(seg, val)

#endif

int
_dos_findfirst(_char_t* buf, int attribute, _find_t* dp) {
  install(dp);
  DX = getoffset(buf);
  DS = getseg(buf);
  CX = attribute;
  SYSCALL(FINDFIRST);
  restore();

  return AX & 0xFF;
}

int
dos_findnext(_find_t* dp) {
  install(dp);
  SYSCALL(FINDNEXT);
  restore();

  return AX & 0xFF;
}

static  void
SYSCALL(unsigned int syscall) {
  AX = syscall;
  SYSTEM();
}

static  void
install(dp)
  DIR     *dp; {
  SYSCALL(GETDTA);
  odp_off = BX;
  setseg(odp_seg, ES);
  DX = getoffset(dp);
  DS = getseg(dp);
  SYSCALL(SETDTA);
}

static void
restore() {
  DX = odp_off;
  setseg(DS, odp_seg);
  SYSCALL(SETDTA);
}

#endif
#endif

#if     defined(_TESTDIR)
#include        <stdio.h>
#include        <stdlib.h>
#include        <sys/types.h>
#include        <sys/stat.h>
#include        "dirent.h"

#if     !defined(S_ISDIR)
#define S_ISDIR(mode)           (((mode) & S_IFMT) == S_IFDIR)
#endif

#if     !defined(S_ISREG)
#define S_ISREG(mode)           ((mode) & S_IFREG)
#endif

#if     !defined(NFILEN)
#define NFILEN  256
#endif

void
list(_char_t* d) {
  DIR     *dirp;
  struct  dirent  *dp;
  _char_t buf[NFILEN];
  struct  stat stb;

  dirp = opendir(d);

  if (!dirp) {
    printf(_STR("No such directory: %s"), d);
    return;
  }

  while ((dp = readdir(dirp)) != NULL) {
    sprintf(buf, _STR("%s/%s"), d, dp->d_name);
    printf(_STR("%s\n"), buf);
    if (stat((_char_t *)buf, &stb) == 0 &&
        (S_ISREG(stb.st_mode) || S_ISDIR(stb.st_mode))) {
      int      isdir = S_ISDIR(stb.st_mode);
      _char_t* cdir  = isdir ? _STR("/") : _STR("");

      if (!strcmp(dp->d_name, _STR(".")) || !strcmp(dp->d_name, _STR(".."))) {
        continue;
      }

      if (isdir) {
        list(buf);
      }
    }
  }
}

int
main(int argc, _char_t* argv[]) {
  if (argc > 1) {
    list(argv[1]);
  }
}

#endif
