#if     !defined(lINT)
static auto rcsid("$Id: fileio.cpp,v 1.23 2018/09/09 07:21:10 jullien Exp $");
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
 * The  routines  in  this  file read and write ASCII or UNICODE
 * files  from  the  disk.  All of the knowledge about files are
 * here. A better message writing scheme should be used.
 */

#include "./emacs.h"

static  void testfilemode(const EMCHAR* fn, bool* binmode, ENCODING* utf8);

static FILE*    ffp;    /* File poINTer, all functions. */
static ENCODING ffunicode = ENCODING::EMASCII; /* UNICODE flag */

/*
 * Open a file for reading.
 */

#if defined(_WIN32) || defined(_UNICODE)
#define MAX_SENSE       256

/*
 * Given a file name, testfilemode checks if this file should be
 * opened  in  binary mode (i.e.  without \n -> \r\n conversion)
 * or  as  standard text mode.  The poINTer binmode is filled on
 * output  with the appropriate value.  This code is called only
 * on DOS boxes where conversions could apply. It also checks if
 * UTF-8 is specified in the MAX_SENSE first bytes.
 */

static ENCODING
emfindencoding(const char* mem, INT max) {
#if defined(_UNICODE)
  for (int i = 0; i < max; ++i) {
    if (!(mem[i] == 'u' || mem[i] == 'U')) {
      continue;
    }
    ++i;
    if (!(mem[i] == 't' || mem[i] == 'T')) {
      continue;
    }
    ++i;
    if (!(mem[i] == 'f' || mem[i] == 'F')) {
      continue;
    }
    ++i;
    if (mem[i] == '-') {
      ++i;
    }
    if (mem[i] == '8') {
      return ENCODING::EMUTF8;
    }
    if ((mem[i] == '1' || mem[i+1] == '6')) {
      return ENCODING::EMUTF16;
    }
  }
  return ENCODING::EMASCII;
#else
  (void)mem;
  (void)max;

  return ENCODING::EMASCII;
#endif
}

static void
testfilemode(const EMCHAR* fn, bool* binmode, ENCODING* utf8) {
  ENCODING widep = ENCODING::EMASCII;
  int      n;

  if (binmode == nullptr || utf8 == nullptr) {
    /*
     * Be paranoiac.
     */
    return;
  }

  *binmode = false;

  if (opt::auto_encoding_mode) {
    switch (getautomode(fn)) {
    case EDITMODE::JAVAMODE:
    case EDITMODE::CMODE:
    case EDITMODE::CPPMODE:
    case EDITMODE::CSHARPMODE:
    case EDITMODE::SHELLMODE:
      *utf8 = ENCODING::EMUTF8;
      break;
    default:
      *utf8 = ENCODING::EMASCII;
    }
  }

  /*
   * Open file in binary mode.
   */

  auto fd = ffopen(fn, ECSTR("rb"), &widep);

  if (fd != nullptr) {
    if (widep == ENCODING::EMUTF16) {
      *binmode = false;
      (void)std::fclose(fd);
      return;
    }

    /*
     * Read few bytes in a buffer and tries to find UTF-8/UTF-16 string
     */

    char buf[MAX_SENSE + 64];
    n = (int)std::fread(buf, sizeof(char), MAX_SENSE, fd);
    (void)std::fclose(fd);

    if (opt::auto_encoding_mode) {
      switch (emfindencoding(buf, n)) {
      case ENCODING::EMUTF8:
        *utf8     = ENCODING::EMUTF8;
        ffunicode = ENCODING::EMUTF8;
        break;
      case ENCODING::EMUTF16:
        ffunicode = ENCODING::EMUTF16;
        break;
      default:
        *utf8     = ENCODING::EMASCII;
        ffunicode = ENCODING::EMUTF8;
      }
    }

    if (buf[0] == '#' && buf[1] == '!') {
      *binmode = true;
      return;
    }

    /*
     * If   we  find  a  newline  and  no  preceding
     * carriage-return,  it's probably a binary file
     * (read a unix file).
     */

    for (int i = 0; i < n; ++i) {
      if (buf[i] == '\n') {
        if ((i > 0) && buf[i-1] == '\r') {
          *binmode = false;
        } else {
          *binmode = true;
        }
        break;
      }
    }
  }
}
#else
static void
testfilemode(const EMCHAR* fn, bool* binmode, ENCODING* utf8) {
  if (fn) {
    *binmode = false;
    *utf8    = ENCODING::EMASCII;
  }
}
#endif

int
ffropen(const EMCHAR* fn, bool* binmode, ENCODING* widep) {
  if (ffaccess(fn) != FIOSUC) {
    return FIOFNF;
  }

  testfilemode(fn, binmode, widep);

  if ((ffp = ffopen(fn, ECSTR("r"), widep)) == nullptr) {
    ffp = nullptr;
    return FIOFNF;
  }

  return FIOSUC;
}

/*
 * Open  a file for writing. Return T if all is well, and NIL
 * on error (cannot create).
 */

int
ffwopen(const EMCHAR* fn, int binmode, ENCODING widep) {
  EMCHAR wmode[3];

  if (binmode) {
    (void)emstrcpy(wmode, ECSTR("wb"));
  } else {
    (void)emstrcpy(wmode, ECSTR("w"));
  }

  if ((ffp = ffopen(fn, wmode, &widep)) == nullptr) {
    WDGerror(ECSTR("Cannot open file for writing"));
    return FIOERR;
  }

  return FIOSUC;
}

/*
 * Close a file. Should look at the status in all systems.
 */

int
ffclose() {
  if (std::fclose(ffp) != 0) {
    WDGerror(ECSTR("Error closing file"));
    return FIOERR;
  } else {
    ffp = nullptr;
    return FIOSUC;
  }
}

/*
 * Write a line to the already opened file.  The "buf" points to
 * the  buffer,  and the "nbuf" is its  length,  less  the  free
 * newline. Return the status.  Check only at the newline.
 */

int
ffputline(const EMCHAR* buf, int nbuf) {
#if defined(_UNICODE)
  char wbuf[EMMB_LEN_MAX];
  int  conv;
  int  c;

  clearerr(ffp);

  switch (ffunicode) {
  case ENCODING::EMUTF8:
    while (nbuf-- > 0) {
      c = *buf++;
      if ((conv = emwctomb(wbuf, (EMCHAR)c)) > 0) {
        (void)fwrite(wbuf, conv, 1, ffp);
      } else {
        (void)std::fputc('?', ffp);
      }
    }
    (void)std::fputc('\n', ffp);
    break;
  case ENCODING::EMUTF16:
    while (nbuf-- > 0) {
      (void)std::fputwc(*buf++, ffp);
    }
    (void)std::fputwc('\n', ffp);
    break;
  default:
    while (nbuf-- > 0) {
      (void)std::fputc(*buf++, ffp);
    }
    (void)std::fputc('\n', ffp);
  }
#else
  while (nbuf-- > 0) {
    (void)std::fputc(*buf++, ffp);
  }
  (void)std::fputc('\n', ffp);
#endif

  if (ferror(ffp) != 0) {
    WDGerror(ECSTR("Write I/O error"));
    return FIOERR;
  } else {
    return FIOSUC;
  }
}

/*
 * Read a line from a file,  and store the bytes in the supplied
 * buffer.  The  "nbuf"  is the length of the  buffer.  Complain
 * about long lines and lines at the end of the file that  don't
 * have  a  newline present.  Check for I/O errors  too.  Return
 * status and 'len' is set to the length of the line.
 */

int
ffgetline(EMCHAR* buf, int nbuf, int* len) {
  int     c;
  int     i = 0;
  int     status = FIOSUC;

  clearerr(ffp);
#if     defined(_UNICODE)
  switch (ffunicode) {
  case ENCODING::EMUTF8:
    while ((c = std::fgetc(ffp)) != EOF && c != '\n') {
      EMCHAR wc = 0;
      char   tmp[EMMB_LEN_MAX];
      int    nb;

      if (i >= nbuf) {
        WDGwrite(ECSTR("File has line longer than %d chars."), i);
        return FIOERR;
      }

      nb = emmbclen(c);

      if (nb == 1) {
        buf[i++] = (EMCHAR)c;
        continue;
      } else if (nb == 0) {
        WDGwrite(ECSTR("UTF-8 encoding error#1."));
        return FIOERR;
      }

      tmp[0] = (char)c;

      for (int j = 1; j < nb; ++j) {
        c = std::fgetc(ffp);
        if (c == EOF) {
          break;
        } else if ((c == 0) || ((c & 0x80) == 0)) {
          WDGwrite(ECSTR("UTF-8 encoding error#2."));
          return FIOERR;
        }
        tmp[j] = (char)c;
      }

      if (emmbtowc(&wc, tmp, nb) == -1) {
        WDGwrite(ECSTR("UTF-8 encoding error#3."));
        return FIOERR;
      }

      buf[i++] = (EMCHAR)wc;
    }
    break;
  case ENCODING::EMUTF16:
    while ((c = std::fgetwc(ffp)) != EMEOF && c != '\n') {
      if (c == '\r') {
        continue;
      }
      if (i >= nbuf) {
        WDGwrite(ECSTR("File has line longer than %d chars."), i);
        return FIOERR;
      } else {
        buf[i++] = (EMCHAR)c;
      }
    }
    break;
  default:
    while ((c = std::fgetc(ffp)) != EOF && c != '\n') {
      if (i >= nbuf) {
        WDGwrite(ECSTR("File has line longer than %d chars."), i);
        return FIOERR;
      } else {
        buf[i++] = (EMCHAR)c;
      }
    }
  }
#else
  while ((c = std::fgetc(ffp)) != EMEOF && c != '\n') {
    if (i >= nbuf) {
      WDGwrite(ECSTR("File has line longer than %d chars."), i);
      return FIOERR;
    } else {
      buf[i++] = (EMCHAR)c;
    }
  }
#endif
  buf[*len = i] = '\0';

  if (c == ((ffunicode == ENCODING::EMUTF16) ? EMEOF : EOF)) {
    if (ferror(ffp) != 0) {
      WDGerror(ECSTR("File read error"));
      return FIOERR;
    }
    if (i != 0) {
      WDGerror(ECSTR("File should end with newline."));
      return FIOWNL;
    }
    return FIOEOF;
  } else {
    return status;
  }
}

/*
 * Delete the file 'fn'. Returns FIOSUC on succes.
 */

int
ffremove(const EMCHAR* fn) {
  if (emremove(fn) == 0) {
    return FIOSUC;
  } else {
    return FIOERR;
  }
}

/*
 * Delete the file 'fn'. Returns FIOSUC on succes.
 */

int
ffrename(const EMCHAR* oldfn, const EMCHAR* newfn) {
  if (emrename(oldfn, newfn) == 0) {
    return FIOSUC;
  } else {
    return FIOERR;
  }
}

/*
 * Get a line form fd.
 */

EMCHAR*
ffgets(EMCHAR* buf, int n, FILE *fd) {
#if defined(_UNICODE)
  char data[MAXLINE];
  int  i;

  switch (ffunicode) {
  case ENCODING::EMUTF16:
    return std::fgetws(buf, n, fd);
  case ENCODING::EMUTF8:
  default:
    if (std::fgets(data, n, fd)) {
      for (i = 0; data[i]; ++i) {
        buf[i] = (EMCHAR)data[i];
      }
      buf[i] = '\000';
      return &buf[0];
    } else {
      return nullptr;
    }
  }
#else
  return std::fgets(buf, n, fd);
#endif
}

/*
 * Returns the complete path of 'fname' into 'rfname'.
 */

int
ffullname(EMCHAR* rname, const EMCHAR* fname) {
#if defined(_WIN32)
  LPTSTR s;
  EMCHAR tmp[NFILEN];

  if (GetFullPathName((LPCTSTR)fname, NFILEN, tmp, &s) == 0) {
    rname[0] = '\000';
    return FIOSUC;
  }

  if (GetLongPathName(tmp, rname, NFILEN) == 0) {
    s = tmp;
    emstrcpy(rname, tmp);
  } else {
    s = rname;
  }

  /*
   * force device to uppercase:
   */

  if (s && (emstrlen(s) >= 2) && s[1] == ':') {
    if (std::isalpha((int)s[0]) && std::islower((int)s[0])) {
      s[0] = (TCHAR)std::toupper((int)s[0]);
    }
  }

  (void)normalize(rname, NOSLASH);
#else
  EMCHAR c;

  if (fname[0] == '\\' || fname[0] == '/' || fname[1] == ':') {
    (void)emstrcpy(rname, fname);
    (void)normalize(rname, NOSLASH);
    return FIOSUC;
  }

  (void)ffgetcwd(rname, NFILEN-1);

  if ((c = rname[emstrlen(rname) - 1]) != '\\' && c != '/') {
    (void)emstrcat(rname, ECSTR("/"));
  }
  (void)emstrcat(rname, fname);
  (void)normalize(rname, NOSLASH);
#if defined(_WIN32)
  (void)emstrlwr(rname);
#endif
#endif
  return FIOSUC;
}

/*
 * Test  the  accessibility  of  file  'fn'.  Returns  FIOSUC on
 * succes.  If  the  operating  system does not support 'access'
 * return FIOSUC.
 */

#if !defined(F_OK)
#define F_OK 0x00
#endif

int
ffaccess(const EMCHAR* fn) {
  if (emaccess(fn, F_OK) == 0) {
    return FIOSUC;
  } else {
    return FIOERR;
  }
}

/*
 * Write Byte Order Mark for UNICODE
 */

void
ffputbom(ENCODING widep) {
  if (widep == ENCODING::EMUTF16) {
#if defined(_UNICODE)
    std::fputwc(EMBOM, ffp);
#else
    WDGwrite(ECSTR("BOM set without UNICODE"));
#endif
  }
}

/*
 * Open a file for reading. Wrapper for UNICODE.
 */

FILE*
ffopen(const EMCHAR* file, const EMCHAR* mode, ENCODING* widep) {
  FILE* fd;
#if defined(_UNICODE)
  int     c1 = 0;
  int     c2 = 0;
  auto w = ENCODING::EMASCII;

  if (widep == nullptr) {
    /*
     * widep may be passed as nullptr if not used.
     */
    widep     = &w;
    ffunicode = ENCODING::EMASCII;
  } else {
    ffunicode = *widep;
  }

  if (mode[0] == 'r') {
    fd = emfopen(file, mode);

    if (fd == nullptr) {
      return fd;
    }
    c1 = std::fgetc(fd);
    if (c1 != EOF) {
      c2 = std::fgetc(fd);
    }
    (void)std::fclose(fd);

    if (((c1 == 0xFF) && (c2 == 0xFE))
        || ((c1 == 0xFE) && (c2 == 0xFF)) ) {
      fd = emfopen(file, ECSTR("rb"));
      emfwide(fd, 1);
      ffunicode = ENCODING::EMUTF16;
      if (std::fgetwc(fd)) {
        *widep = ENCODING::EMUTF16;
      }
      return fd;
    }
  } else if (mode[0] == 'w' && *widep == ENCODING::EMASCII) {
    fd = emfopen(file, ECSTR("wb"));
    if (fd == nullptr) {
      return fd;
    }
    emfwide(fd, 1);
    if (*widep == ENCODING::EMUTF16) {
      std::fputwc(EMBOM, fd);
    }
    ffunicode = *widep;
    return fd;
  }
#endif /* _UNICODE */

  if (mode[0] == '\r') {
    *widep = ENCODING::EMASCII;
  }

  fd = emfopen(file, mode);
  return fd;
}

/*
 * stat wrapper for UNICODE.
 */

int
ffstat(const EMCHAR* file, EMSTAT* mode) {
  return emstat(file, mode);
}

/*
 * chmod wrapper for UNICODE.
 */

int
ffchmod(const EMCHAR* file, bool readonly) {
  return emchmod(file, readonly);
}

/*
 * chmod wrapper for UNICODE.
 */

int
ffchmod(const EMCHAR* file, mode_t mode) {
  return emchmod(file, mode);
}

/*
 * chdir wrapper for UNICODE.
 */

int
ffchdir(const EMCHAR* file) {
  return emchdir(file);
}

/*
 * getcwd wrapper for UNICODE.
 */

EMCHAR*
ffgetcwd(EMCHAR* file, int len) {
  (void)emgetcwd(file, len);
  return file;
}

/*
 * getenv wrapper for UNICODE.
 */

EMCHAR*
ffgetenv(const EMCHAR* var) {
  return emgetenv(var);
}

/*
 * system wrapper for UNICODE.
 */

int
ffsystem(const EMCHAR* cmd) {
  return emsystem(cmd);
}

/*
 * Returns true if the file has been modified since last read or last
 * write. On return, time variable contains last modification time.
 */

bool
ffchanged(const EMCHAR* fname, time_t* time) {
  *time = 0;
  EMSTAT mode;

  auto res = ffstat(fname, &mode);

  if (res != 0) {
    return false;
  }

  *time = mode.st_mtime;

  if (mode.st_mtime > curbp->time()) {
    if (WDGyn(ECSTR("File changed on disk, really save the buffer? ")) == T) {
      return false;
    } else {
      WDGmessage(ECSTR(""));
      return true;
    }
  } else {
    return false;
  }
}

/*
 * Set the mode and access time of fname.
 */

bool
ffsetaccess(const EMCHAR* fname, BUFFER* bp) {
  EMSTAT mode;

  bp->setReadonly(false);
  bp->setPermissions(0);
  bp->setTime(0);

  if ((ffaccess(fname) == FIOSUC) && (ffstat(fname, &mode) == 0)) {
    if (S_ISREG(mode.st_mode)) {
      bp->setReadonly((mode.st_mode & S_IWUSR) == 0);
      bp->setPermissions(mode.st_mode);
      bp->setTime(mode.st_mtime);

      {
#if 0
#if defined(_POSIX_C_SOURCE)
       static auto first(true);
       if (first) {
        remove("trace");
        first = false;
       }

       FILE* fd = fopen("trace", "wa");
       fprintf(fd, "perm: %x UW=%d UR=%d GW=%d GR=%d\n",
               bp->getPermissions(),
               (mode.st_mode & S_IWUSR) != 0,
               (mode.st_mode & S_IRUSR) != 0,
               (mode.st_mode & S_IWGRP) != 0,
               (mode.st_mode & S_IRGRP) != 0);
       fclose(fd);
#else
       printf("perm: %x UW=%d UR=%d\n",
               bp->getPermissions(),
               (mode.st_mode & S_IWUSR) != 0,
               (mode.st_mode & S_IRUSR) != 0);
#endif
#endif
      }
    } else {
      return false;
    }
  }

  return true;
}

/*
 * Returns T if the file can be opened, NIL otherwise.
 */

bool
ffilevalid(const EMCHAR* fname) {
  if (ffaccess(fname) == FIOSUC) {
    return true;
  }

  EMCHAR path[NFILEN];

  auto p = emstrcpy(path, fname);

  for (auto s(p); *s; ++s) {
    if (*s == '\\' || *s == '/') {
      p = s;
    }
  }

  if (p != (EMCHAR*)&path[0]) {
    *p = '\000';

    if (*(p - 1) == ':') {
      /* assumes root directory on MS-DOS */
      return true;
    }

    EMSTAT mode;
    if (ffstat(path, &mode) == 0) {
      if (!S_ISDIR(mode.st_mode)) {
        WDGwrite(ECSTR("Not a directory %s."), path);
        return false;
      }
    } else {
      WDGwrite(ECSTR("Directory %s not found."), path);
      return false;
    }
  }

  return true;
}

/*
 * Return true if fname is a directory and false otherwise. Name as
 * been normalized by the caller.
 */

bool
ffdiredp(const EMCHAR* fname) {
  EMSTAT mode;

  if ((fname[1] == ':') && !fname[2]) {
    /* root drive on MS-DOS */
    return true;
  }

  if ((ffaccess(fname) == FIOSUC) && (ffstat(fname, &mode) == 0)) {
    if (S_ISDIR(mode.st_mode)) {
      return true;
    }
  }

  return false;
}
