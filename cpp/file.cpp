#if !defined(lint)
static char rcsid[] = "$Id: file.cpp,v 1.37 2018/09/09 07:21:09 jullien Exp $";
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
 * The  routines  in this file handle the reading and writing of
 * files. Details of the I/O are in fileio.c
 */

#include "emacs.h"

static bool    frdflag{false}; // flag for freadonly
static void    savetime();
static EMCHAR* getbufdir();

/*
 * Resset freadonly flag made before a command is executed.
 */

void
resetfreadonly() {
  frdflag = false;
}

/*
 * Test  the  file  for readonly flag.  Returns true if the file is
 * marked as readonly, false otherwise.
 */

bool
freadonly() {
  if (frdflag) {
    return false;
  }

  frdflag = true;

  if (curbp->readonly()) {
    TTYbeep();
    WDGmessage(ECSTR("Buffer is readonly."));
    return true;
  }

  return false;
}

/*
 * Return the absolute path for the current buffer.  The current
 * directory  is  taken from file associated with current buffer
 * or  the  actual  current  working  directory  if  it does not
 * exist.  The  return  vaule is a pointer the start of a static
 * local buffer.
 */

static EMCHAR*
getbufdir() {
  static EMCHAR curd[NFILEN];
  EMCHAR lastch;

  if (curbp->unbound()) {
    (void)ffullname(curd, ECSTR("."));
    lastch = curd[emstrlen(curd) - 1];
    if (!(lastch == '\\' || lastch == '/')) {
      (void)emstrcat(curd, ECSTR("/"));
    }
  } else {
    (void)emstrcpy(curd, curbp->filename());
    (void)updir(curd, SLASH);
  }

  return (EMCHAR*)&curd[0];
}

/*
 * Look around to see if  you  can  find  the file  in  another
 * buffer; if you can find it just switch to the buffer. If you
 * cannot find the file, create a new buffer, read in the text,
 * and switch to the new buffer.
 */

bool
newfile(const EMCHAR* filename) {
  EMCHAR fname[NFILEN];

  (void)ffullname(fname, filename);

  if (!ffilevalid(fname)) {
    return false;
  }

  for (auto bp = BUFFER::head(); bp != nullptr; bp = bp->next()) {
    if (emstrcmp(bp->filename(), fname) == 0) {
      (void)curwp->connect(bp);
      auto lp = curwp->line();
      auto i  = curwp->rows() / 2;
      while (i-- && lp->back() != curbp->lastline()) {
        lp = lp->back();
      }
      curwp->setTopline(lp);

      EMCHAR buf[NLINE];
      (void)emstrcpy(buf, ECSTR("Discard changes made to buffer "));
      (void)emstrcat(buf, bp->bufname());
      (void)emstrcat(buf, ECSTR(" ? "));

      if (bp->isChanged() && (WDGyn(buf) == T)) {
        bp->setChanged(false);   /* Don't complain! */
        return revertbuffer() == T;
      }
      curwp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
      WDGmessage(ECSTR("Old buffer"));
      return true;
    }
  }

  if (ffdiredp(fname)) {
    return diredbuffer(fname);
  }

  EMCHAR bname[BUFFER::NBUFN];
  makename(bname, fname);

  {
    /*
     * find or create buffer if it does not exist.
     */
    auto bp = BUFFER::find(bname);

    if (bp == nullptr) {
      WDGerror(ECSTR("Cannot create buffer"));
      return false;
    }

    (void)curwp->connect(bp);
  }

  return readin(fname);
}

/*
 * Read  file "fname" into the current buffer,  blowing away any
 * text found there. Called by both the read and visit commands.
 * Return  the  final status of the read.  Also  called  by  the
 * mainline, to  read in a file specified on the command line as
 * an argument.
 */

bool
readin(const EMCHAR* fname) {
  int    f;
  int    nline;
  long   nchar;
  int    nbytes;
  EMCHAR line[MAXLINE];

  if (ffdiredp(fname)) {
    return diredbuffer(fname);
  }

  if (!ffsetaccess(fname, curbp)) {
    WDGwrite(ECSTR("Can't access file %s"), fname);
    return false;
  }

  if (!curbp->clear()) {
    return false;
  }

  curbp->setChanged(false);

  (void)ffullname(curbp->filename(), fname);

  auto widep   = ENCODING::EMASCII;
  auto binmode = false;
  switch (f = ffropen(fname, &binmode, &widep)) {
  case FIOFNF:   /* File not found.      */
    curbp->setReadonly(false);
    curbp->setPermissions(0);
    curbp->setTime(0);
    WDGwrite(ECSTR("(New file %s)"), fname);
    break;
  default:
    WDGmessage(ECSTR("Reading file ..."));
    nline = 0;
    nchar = 0L;

    while ((f = ffgetline(line, MAXLINE, &nbytes)) == FIOSUC || f == FIOWNL) {
      auto lp1(curbp->lastline()->insertBefore(nbytes));
      if (lp1 == nullptr) {
        f = FIOERR;     /* Keep message on the  */
        break;          /* display.             */
      }

      for (int i(0); i < nbytes; ++i) {
        lp1->put(i, line[i]);
      }

      nline += 1;
      nchar += nbytes + (binmode ? 1 : CRSIZE);
      if (f == FIOWNL) {
        break;
      }
    }
    (void)ffclose();
    if (f == FIOERR) {
      return false;
    }
    if (f == FIOEOF) {
      /* Don't zap message!   */
      WDGwrite(ECSTR("Read %d line(s), %ld byte(s)"),
               nline,
               (size_t)nchar * sizeof(EMCHAR));
    }

    (void)ffsetaccess(fname, curbp);
    curbp->setBinary(binmode);
    curbp->setEncoding(widep);
  }

  curbp->setEditMode(getautomode(fname));

  for (auto wp = WINSCR::head(); wp != nullptr; wp = wp->next()) {
    if (wp->buffer() == curbp) {
      wp->setTopline(curbp->firstline());
      wp->setDot(curbp->firstline(), 0);
      wp->setMark(nullptr, 0);
      // update mode line and ensure hard!
      wp->setFlags(WINSCR::WFMODE|WINSCR::WFHARD);
    }
  }

  return true;
}

/*
 * Normalize a pathname. When 'flag' is T, removes the trailing
 * '/..' and '/.'. On MS-DOS, standardize pathnames with '/'.
 *
 * struct  {
 * EMCHAR    *before;
 * EMCHAR    *after;
 * } test[] = {
 * { "/foo/bar/../gee",            "/foo/gee"      },
 * { "/foo/bar/../../gee",         "/gee"          },
 * { "/foo/bar/../../../gee",      "gee"           },
 * { "/foo/bar/.././gee",          "/foo/gee"      },
 * { "/foo/bar/./../gee",          "/foo/gee"      },
 * { "../gee",                     "../gee"        },
 * { "../../../gee",               "../../../gee"  },
 * { "gee/..",                     "."             },
 * { "gee/../foo/..",              "."             },
 * { "gee/../..",                  ".."            },
 * { "/bar/gee/..",                "/bar"          },
 * { "/foo/bar/./gee",             "/foo/bar/gee"  },
 * { "/foo/bar/.",                 "/foo/bar"      },
 * { "./foo/bar/./gee",            "foo/bar/gee"   },
 * { "./foo/bar/../gee",           "foo/gee"       },
 * { "foo/bar/./gee",              "foo/bar/gee"   },
 * { "foo/bar/../gee",             "foo/gee"       },
 * { "../..",                      "../.."         },
 * { "/",                          "/"             },
 * { nullptr,                      nullptr         }
 * };
 */

EMCHAR*
normalize(EMCHAR* fname, int flag) {
  /*
   * convert \\ to /
   */

  for (auto p = fname; *p != 0; ++p) {
    if (*p == '\\') {
      *p = '/';
    }
  }

  /*
   * skip initial './'
   */

  while (!emstrncmp(fname, ECSTR("./"), 2)) {
    auto p = fname + 2;
    auto s = fname;
    while (*p) {
      *s++ = *p++;
    }
    *s = '\000';
  }

  auto lastdir = fname;
  auto res     = fname;

  while (*fname) {
    if (*fname == '/') {
      if (!emstrncmp(fname, ECSTR("/.."), 3)) {
        auto s = lastdir;
        auto p = fname + 3;
        if (*lastdir != '.' && (flag || *(fname+3))) {
          while (*p) {
            *s++ = *++p;
          }
          *s = '\000';
          return normalize(res, flag);
        }
      } else if (!emstrncmp(fname, ECSTR("/."), 2)) {
        auto c = fname[2];
        if (c != '\000' && c != '/') {
          lastdir = fname + 2;
        } else {
          auto s = fname;
          auto p = fname + 2;
          if (flag || *(fname+2)) {
            while (*p) {
              *s++ = *p++;
            }
            *s = '\000';
            return normalize(res, flag);
          }
        }
      } else {
        lastdir = fname + 1;
      }

      if (*(fname+1)) {
        fname++;
      } else {
        if (flag && (fname != res)) {
          /*
           * Removes trailing '/'
           */
          *fname = '\000';
        }
        return res;
      }
    } else {
      fname++;
    }
  }

  if (*res == 0) {
    (void)emstrcpy(res, ECSTR("."));
  }

  return res;
}

/*
 * Returns  the  parent  directory  for  the  given  path.  When
 * 'slashflag' is NOSLASH, removes the trailing '/'.
 */

EMCHAR*
updir(EMCHAR* fname, int slashflag) {
  EMCHAR* p{nullptr};

  fname = normalize(fname, NOSLASH);

  for (auto s = fname; *s != 0; s++) {
    if (*s == ':') {
      p = s + 1;
    } else if ((*s == '/') && *(s+1)) {
      if (slashflag == NOSLASH) {
        p = s;
      } else {
        p = s + 1;
      }
    }
  }

  if (p != nullptr) {
    *p = '\000';
  } else {
    *fname = '\000';
  }

  return fname;
}

/*
 * Take a file name,  and from it fabricate a buffer name.  This
 * routine  knows  about the syntax of file names on the  target
 * system.  I  suppose that this information could be put in   a
 * better place than a line of code.
 */

void
makename(EMCHAR* bname, const EMCHAR* fname) {
  EMCHAR i = '2';

  /*
   *      Point to the last char of pathname.
   */

  auto cp1 = fname + emstrlen(fname) - 1;

  /*
   *      Find the file name component.
   */

#if defined(_WIN32)
  while (cp1 != fname && cp1[-1] != ':' && cp1[-1] != '\\' && cp1[-1] != '/') {
    --cp1;
  }
#else
  while (cp1 != fname && cp1[-1] != '/') {
    --cp1;
  }
#endif

  /*
   *      Copy the file name into buffer name.
   */

  auto cp2 = bname;
  while (cp2 != (bname + BUFFER::NBUFN - 1) && *cp1) {
    *cp2++ = *cp1++;
  }
  *cp2 = 0;

  /*
   * Check if buffer name already exists and, in this case,
   * add version number such as name<2>.
   */

  while (BUFFER::find(bname, false) != nullptr) {
    /*
     * Buffer name already exists, append <X> and try again
     */
    while (cp2 >= (bname + BUFFER::NBUFN - 4)) {
      /*
       * find the end of bname minus 4 chars for <?>
       */
      cp2--;
    }
    if (i > '9') {
      i = '@';
    }
    *cp2       = '<';
    *(cp2 + 1) = i++;
    *(cp2 + 2) = '>';
    *(cp2 + 3) = '\000';
  }
}

/*
 * This function performs the details of file writing.  Uses the
 * file  management  routines in  the  "fileio.c"  package.  The
 * number of lines written is displayed.  Sadly, it looks inside
 * a LINE;  provide a macro for this. Most of the grief is error
 * checking of some sort.
 */

bool
writeout(const EMCHAR* fname) {
  if (freadonly()) {
    return false;
  }

  time_t time;
  if (ffchanged(fname, &time)) {
    return false;
  }

  curbp->setTime(time);
  savetime();

  /*
   * Replace the last extension with .BAK
   */

  int    s;
  EMCHAR bak[NFILEN];
  EMCHAR* p = nullptr;

  (void)emstrcpy(&bak[0], &fname[0]);

  for (s = 0; bak[s]; s++) {
    EMCHAR c = bak[s];

    if (c == '/' || c == '\\') {
      /*
       * Path separator, reset p.
       */
      p = nullptr;
      continue;
    }

    if (c == '.') {
      p = (EMCHAR*)&bak[s];
      if (emstrcmp(p, ECSTR(".BAK")) == 0 ||
          emstrcmp(p, ECSTR(".bak")) == 0 ||
          emstrcmp(p, ECSTR(".Bak")) == 0) {
        /*
         * Don't backup .BAK files.
         */
        break;
      }
    }
  }

  if (p != nullptr) {
    *p = '\000';
  }

  (void)emstrcat(bak, ECSTR(".BAK"));

  FILE* fd;

  if (ffaccess(bak) == FIOSUC) {
    if ((fd = ffopen(bak, ECSTR("r"))) != nullptr) {
      (void)std::fclose(fd);
      (void)ffchmod(bak, curbp->readonly());
      (void)ffremove(bak);
    }
  }

  if (opt::backup_before_writing
      && (fd = ffopen(fname, ECSTR("r"))) != nullptr) {
    (void)std::fclose(fd);
    if (ffrename(fname, bak) != FIOSUC) {
      WDGwrite(ECSTR("Could not rename %s to backup file .BAK"), fname);
    }
  }

  if ((s = ffwopen(fname, curbp->binary(), curbp->encoding())) != FIOSUC) {
    return false;
  }

  auto nline = 0;
  auto nchar = 0;

  for (auto lp = curbp->firstline(); lp != curbp->lastline(); lp = lp->forw()) {
    if ((s = ffputline(lp->text(), lp->length())) != FIOSUC) {
      break;
    }
    nline += 1;
    nchar += lp->length() + (curbp->binary() ? 1 : CRSIZE);
  }

  if (s == FIOSUC) {
    s = ffclose();
    if (s == FIOSUC) {
      WDGwrite(ECSTR("Wrote %d line(s), %ld byte(s)"),
               nline,
               (size_t)nchar * sizeof(EMCHAR));
    }
  } else {
    s = ffclose();
  }

  (void)ffchmod(fname, curbp->getPermissions());
  (void)ffsetaccess(fname, curbp);
  curbp->setChanged(false);

  /* 
   *      Update mode lines.
   */

  BUFFER::updatemodes();

  return (s == FIOSUC);
}

#if     defined(_WIN32)
extern  CMD NTansitooem(EDLINE* lp);
extern  CMD NToemtoansi(EDLINE* lp);

CMD
ansitooem() {
  if (freadonly()) {
    return NIL;
  }

  curbp->setChanged(false);

  for (auto lp = curbp->firstline(); lp != curbp->lastline(); lp = lp->forw()) {
    NTansitooem(lp);
  }

  lchange(WINSCR::WFHARD);

  return T;
}

CMD
oemtoansi() {
  if (freadonly()) {
    return NIL;
  }

  curbp->setChanged(false);

  for (auto lp = curbp->firstline(); lp != curbp->lastline(); lp = lp->forw()) {
    NToemtoansi(lp);
  }

  lchange(WINSCR::WFHARD);

  return T;
}

static unsigned char MacToISOLatin1[] = {
  /* 00 */      0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  /* 08 */      0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
  /* 10 */      0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  /* 18 */      0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
  /* 20 */      0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  /* 28 */      0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
  /* 30 */      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  /* 38 */      0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
  /* 40 */      0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  /* 48 */      0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
  /* 50 */      0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  /* 58 */      0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
  /* 60 */      0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  /* 68 */      0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
  /* 70 */      0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  /* 78 */      0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
  /* 80 */      0xC4, 0xC5, 0xC7, 0xC9, 0xD1, 0xD6, 0xDC, 0xE1,
  /* 88 */      0xE0, 0xE2, 0xE4, 0xE3, 0xE5, 0xE7, 0xE9, 0xE8,
  /* 90 */      0xEA, 0xEB, 0xED, 0xEC, 0xEE, 0xEF, 0xF1, 0xF3,
  /* 98 */      0xF2, 0xF4, 0xF6, 0xF5, 0xFA, 0xF9, 0xFB, 0xFC,
  /* A0 */      0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
  /* A8 */      0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
  /* B0 */      0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
  /* B8 */      0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
  /* C0 */      0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
  /* C8 */      0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
  /* D0 */      0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
  /* D8 */      0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
  /* E0 */      0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xCA, 0xC1,
  /* E8 */      0xCB, 0xC8, 0xCD, 0xCE, 0xCF, 0xCC, 0xD3, 0xD4,
  /* F0 */      0xF0, 0xF1, 0xDA, 0xDB, 0xD9, 0xF5, 0xF6, 0xF7,
  /* F8 */      0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
};

CMD
mactoansi() {
  if (freadonly()) {
    return NIL;
  }

  curbp->setChanged(false);

  for (auto lp = curbp->firstline(); lp != curbp->lastline(); lp = lp->forw()) {
    auto p = lp->text();
    int     l  = lp->length();

    for (int i = 0; i < l; ++i) {
      p[i] = (EMCHAR)MacToISOLatin1[(p[i] & 0xff)];
    }
  }

  lchange(WINSCR::WFHARD);

  return T;
}

CMD
mactooem() {
  if (mactoansi() == T) {
    return ansitooem();
  } else {
    return NIL;
  }
}

#else
CMD
ansitooem() {
  return T;
}

CMD
oemtoansi() {
  return T;
}

CMD
mactoansi() {
  return T;
}

CMD
mactooem() {
  return T;
}
#endif

/*
 * Return the ptr in sp at which the character c last appears;
 * returns nullptr if none is found.
 */

EDITMODE
getautomode(const EMCHAR* sp) {
  static struct {
    const EMCHAR*  ext;
    EDITMODE mode;
  } modtable[] = {
    { ECSTR(".ll"),    EDITMODE::LISPMODE    },
    { ECSTR(".LL"),    EDITMODE::LISPMODE    },
    { ECSTR(".cl"),    EDITMODE::LISPMODE    },
    { ECSTR(".ml"),    EDITMODE::LISPMODE    },
    { ECSTR(".ML"),    EDITMODE::LISPMODE    },
    { ECSTR(".lap"),   EDITMODE::LISPMODE    },
    { ECSTR(".LAP"),   EDITMODE::LISPMODE    },
    { ECSTR(".lsp"),   EDITMODE::LISPMODE    },
    { ECSTR(".LSP"),   EDITMODE::LISPMODE    },
    { ECSTR(".lisp"),  EDITMODE::LISPMODE    },
    { ECSTR(".emacs"), EDITMODE::LISPMODE    },
    { ECSTR(".osp"),   EDITMODE::LISPMODE    },
    { ECSTR(".c"),     EDITMODE::CMODE       },
    { ECSTR(".h"),     EDITMODE::CMODE       },
    { ECSTR(".wmls"),  EDITMODE::CMODE       },
    { ECSTR(".C"),     EDITMODE::CPPMODE     },
    { ECSTR(".CC"),    EDITMODE::CPPMODE     },
    { ECSTR(".cc"),    EDITMODE::CPPMODE     },
    { ECSTR(".cpp"),   EDITMODE::CPPMODE     },
    { ECSTR(".cs"),    EDITMODE::CSHARPMODE  },
    { ECSTR(".cxx"),   EDITMODE::CPPMODE     },
    { ECSTR(".H"),     EDITMODE::CPPMODE     },
    { ECSTR(".hh"),    EDITMODE::CPPMODE     },
    { ECSTR(".hxx"),   EDITMODE::CPPMODE     },
    { ECSTR(".hpp"),   EDITMODE::CPPMODE     },
    { ECSTR(".idl"),   EDITMODE::CPPMODE     },
    { ECSTR(".IDL"),   EDITMODE::CPPMODE     },
    { ECSTR(".html"),  EDITMODE::SGMLMODE    },
    { ECSTR(".htm"),   EDITMODE::SGMLMODE    },
    { ECSTR(".sgml"),  EDITMODE::SGMLMODE    },
    { ECSTR(".sml"),   EDITMODE::SGMLMODE    },
    { ECSTR(".wml"),   EDITMODE::SGMLMODE    },
    { ECSTR(".xml"),   EDITMODE::SGMLMODE    },
    { ECSTR(".xsl"),   EDITMODE::SGMLMODE    },
    { ECSTR(".java"),  EDITMODE::JAVAMODE    },
    { ECSTR(".jav"),   EDITMODE::JAVAMODE    },
    { ECSTR(".js"),    EDITMODE::JAVAMODE    },
    { ECSTR(".xlg"),   EDITMODE::PROLOGMODE  },
    { ECSTR(".pro"),   EDITMODE::PROLOGMODE  },
    { ECSTR(".pl"),    EDITMODE::PERLMODE    },
    { ECSTR(".asm"),   EDITMODE::ASMODE      },
    { ECSTR(".s"),     EDITMODE::ASMODE      },
    { ECSTR(".pas"),   EDITMODE::PASCALMODE  },
    { ECSTR(".p"),     EDITMODE::PASCALMODE  },
    { ECSTR(".f"),     EDITMODE::FORTRANMODE },
    { ECSTR(".for"),   EDITMODE::FORTRANMODE },
    { ECSTR(".sh"),    EDITMODE::SHELLMODE   },
  };

  const EMCHAR* trail = nullptr;

  do {
    if (*sp == '.') {
      trail = sp;
    }
  } while (*sp++);

  if (trail) {
    int size = (int) (sizeof(modtable)/sizeof(modtable[0]));

    for (int i = 0; i < size; i++) {
      if ((emstrcmp(trail, modtable[i].ext) == 0)) {
        return modtable[i].mode;
      }
    }
  }

  return EDITMODE::FUNDAMENTAL;
}

#define slashp(c)             ((c) == '/')

static void
savetime() {
  if (opt::date_completion) {
    return;
  }

  EDLINE* lp;

  if ((lp = curbp->firstline()) == curbp->lastline() ||
      (lp = lp->forw())         == curbp->lastline()) {
    return;
  }

  auto maxl = lp->length();
  auto buf  = lp->text();

  for (int i = 0; i < (maxl - 7); i++) {
    if (std::isdigit(buf[i + 0]) &&
        std::isdigit(buf[i + 1]) &&
        slashp(buf[i + 2]) &&
        std::isdigit(buf[i + 3]) &&
        std::isdigit(buf[i + 4]) &&
        slashp(buf[i + 5]) &&
        std::isdigit(buf[i + 6]) &&
        std::isdigit(buf[i + 7])) {
      static constexpr size_t MAXDATE{32};  // maximum date length
      int    datelen;
      auto   atline = false;
      EMCHAR sdate[MAXDATE];
      std::tm*    nt;
      std::time_t tb;

      if (i < (maxl - 9)
          && std::isdigit(buf[i + 8])
          && std::isdigit(buf[i + 9])) {
        datelen = 10;
      } else {
        datelen =  8;
      }

      if (i > 2 && std::isdigit(buf[i - 1]) && std::isdigit(buf[i - 2])) {
        datelen += 2;
        i       -= 2;
      }

      const auto& dot(curwp->getDot());
      auto oldp = dot.line();
      auto oldo = dot.pos();

      if (oldp == lp) {
        atline = true;
      }

      (void)std::time(&tb);
      nt = std::localtime(&tb);

      (void)emsprintf3(sdate,
                       ECSTR("%04d/%02d/%02d"),
                       (nt->tm_year + 1900) % 10000,
                       (nt->tm_mon  + 1) % 100,
                       (nt->tm_mday) % 100);

      curwp->setDot(lp, i);
      (void)ldelete(datelen);

      for (auto j = 0; sdate[j]; ++j) {
        (void)linsert(sdate[j]);
      }

      if (!atline) {
        curwp->setDot(dot);
      } else {
        curwp->setDotPos(oldo);
      }

      curwp->setFlags(WINSCR::WFHARD);
      break;
    }
  }
}

/*
 * Toggle the write access for the current buffer only.  Bound to
 * C-XC-Q
 */

CMD
toggleread() {
  if (curbp->readonly()) {
    curbp->setReadonly(false);
    WDGmessage(ECSTR("Access right set to read/write"));
  } else {
    curbp->setReadonly(true);
    WDGmessage(ECSTR("Access right set to read"));
  }

  return T;
}

/*
 * The   command  allows  the user  to  modify  the   file  name
 * associated  with  the  current buffer.  It is  like  the  "f"
 * command in UNIX "ed".  The operation is simple;  just zap the
 * name in the BUFFER structure, and mark the windows as needing
 * an  update.  You can type a blank line at the prompt  if  you
 * wish.
 */

CMD
findfile() {
  auto ofname = getbufdir();
  CMD s;

  complete = filematch;

  if ((s = WDGedit(ECSTR("Find file: "), ofname, NFILEN)) != T && s != NIL) {
    return s;
  } else {
    return newfile(ofname) ? T : NIL;
  }
}

/*
 * Read a file into the current buffer. This is really easy; all
 * you  do it find the name of the file,  and call the  standard
 * "read a file into the current buffer" code.  Before returning
 * it sets the mode to Read-only. Bound to "C-X C-R".
 */

CMD
fileread() {
  auto ofname = getbufdir();
  CMD  s;

  complete = filematch;

  s = WDGedit(ECSTR("Find file read-only: "), ofname, NFILEN);

  if (s != T && s != NIL) {
    return s;
  }

  if (!newfile(ofname)) {
    return NIL;
  }

  curbp->setReadonly(true);

  BUFFER::updatemodes();
  return T;
}

/*
 * Select  a file for editing. Asks the user for a file name to
 * be read and call internal routine "readin". Bound to C-XC-V
 */

CMD
filealternate() {
  auto   ofname = getbufdir();
  EMCHAR bname[BUFFER::NBUFN];
  CMD s;

  complete = filematch;

  if ((s = WDGedit(ECSTR("Find alternate file: "), ofname, NFILEN)) != T) {
    return s;
  }

  if (!ffilevalid(ofname)) {
    return NIL;
  }

  if (!readin(ofname)) {
    return NIL;
  }

  makename((EMCHAR*)&bname[0], ofname);
  curbp->setBuffer((EMCHAR*)&bname[0]);

  (void)WDGtitle(curbp->bufname(), curbp->filename());

  /*
   *      Update mode lines.
   */

  BUFFER::updatemodes();
  return T;
}

/*
 * Insert  file "fname"  into the  current  buffer,  at  dot "."
 * position preserving any existent text in buffer. Return  the
 * final status of the read. In some EMACS this command is bound
 * to C-XC-I.
 */

CMD
fileinsert() {
  EDLINE* lp2;
  int     nline;
  int     nbytes;
  bool    binmode;

  EMCHAR   line[MAXLINE];

  if (freadonly()) {
    return NIL;
  }

  complete = filematch;

  auto ofname = getbufdir();

  {
    CMD s;
    if ((s = WDGedit(ECSTR("Insert file: "), ofname, NFILEN)) != T) {
      return s;
    }
  }

  if (!ffilevalid(ofname)) {
    return NIL;
  }

  if (ffdiredp(ofname)) {
    TTYbeep();
    WDGwrite(ECSTR("Can't insert directory!!"));
    return NIL;
  }

  auto bp = curbp; // Cheap.
  bp->setChanged(false);

  auto widep(ENCODING::EMASCII);
  auto res(ffropen(ofname, &binmode, &widep));

  if (res == FIOERR) {
    WDGwrite(ECSTR("Can't open %s"), ofname);
    return NIL;
  } else if (res == FIOFNF) {
    /*
     * File not found.
     */
    WDGwrite(ECSTR("%s not found."), ofname);
    return NIL;
  }

  WDGmessage(ECSTR("Inserting file ..."));

  const auto& dot(curwp->getDot());
  nline = 0;
  lp2   = nullptr;

  while ((res = ffgetline(line, MAXLINE, &nbytes)) == FIOSUC || res == FIOWNL) {
    if (nline++ == 0) {
      /*
       * It's   the   first  line.  Insert  at  the
       * current position.
       */

      for (int i = 0; i < nbytes; ++i) {
        (void)linsert(line[i]);
      }

      /*
       * break line only if not a the end of buffer.
       */

      if (curbp->lastline() != curwp->line()->forw()) {
        (void)openline();
      }

      lp2 = curwp->line();
      curwp->setMark(dot);
    } else {
        /*
       * A   previous   line   has   already   been
       * inserted. lp2 points to this line.
       */
 
      auto next = lp2->forw();
      auto lp1  = next->insertBefore(nbytes);

      if (lp1 == nullptr) {
        res = FIOERR;  // Keep message on the display
        break;
      }

      for (int i = 0; i < nbytes; ++i) {
        lp1->put(i, line[i]);
      }

      lp2 = lp1;

      if (res == FIOWNL) {
        break;
      }
    }
  }

  (void)ffclose();                        /* Ignore errors.       */
  if (res == FIOEOF) {                    /* Don't zap message!   */
    if (nline == 1) {
      WDGwrite(ECSTR("Insert 1 line"));
    } else {
      WDGwrite(ECSTR("Insert %d lines"), nline);
    }
  }

  curwp->setDot(dot);
  lchange(WINSCR::WFHARD);

  return (res == FIOERR) ? NIL : T;
}

/*
 * Ask  for a file name,  and write the contents of the  current
 * buffer  to that file.   Update the remembered file  name  and
 * clear the buffer changed flag. Bound to "C-XC-W".
 */

CMD
filewrite() {
  auto ofname = getbufdir();
  CMD s;

  if (curbp->unbound()) {
    /*
     * Must have a name.
     */
    complete = filematch;
    s = WDGedit(ECSTR("File to save in: "), ofname, NFILEN);
  } else {
    complete = fileaccept;
    (void)emstrcpy(ofname, curbp->filename());
    if ((s = WDGedit(ECSTR("Write file: "), ofname, NFILEN)) == NIL) {
      (void)filesave();
    }
  }

  if (s != T) {
    return s;
  }

  if (!ffsetaccess(ofname, curbp)) {
    WDGwrite(ECSTR("Can't write %s"), ofname);
    return NIL;
  }

  if (writeout(ofname)) {
    EMCHAR bname[BUFFER::NBUFN];

    makename((EMCHAR*)&bname[0], ofname);
    (void)emstrcpy(curbp->bufname(), bname);
    (void)emstrcpy(curbp->filename(), ofname);
    (void)ffullname(curbp->filename(), ofname);
    s = T;
  }

  return s;
}

/*
 * Save  the contents of the current buffer  in  its  associated
 * file.  No  nothing if nothing has changed (this may be a bug,
 * not a feature). Error if there is no remembered file name for
 * the buffer. Bound to "C-X C-S".
 */

CMD
filesave() {
  if (!curbp->isChanged()) {   /* Return, no changes.  */
    WDGmessage(ECSTR("(No changes need to be saved)"));
    return T;
  }

  if (curbp->unbound()) {
    return filewrite();
  }

  return writeout(curbp->filename()) ? T : NIL;
}

/*
 * Save all unsaved buffer.  Ask for confirmation.  Bound to "C-XC-S".
 */

CMD
savesomebuffers() {
  return anycb(ANYCB::PROMPT);
}

/*
 * Restore  a  buffer  to its orignial contents and try to go to
 * the  previous position (where the dot was before calling this
 * command). Not bound.
 */

CMD
revertbuffer() {
  if (curbp->unbound()) {
    WDGwrite(ECSTR("Buffer does not seem to be associated with any file"));
    return NIL;
  }

  /*
   * Compute the line number of containing dot.
   */

  const auto& dot(curwp->getDot());
  const auto opos(dot.pos());
  auto cln(0);

  for (auto clp = curbp->firstline(); clp != dot.line(); clp = clp->forw()) {
    ++cln;
  }

  /*
   * Reading buffer again.
   */

  (void)readin(curbp->filename());

  /*
   * Goto the line at dot (if any).
   */

  EDLINE* clp;

  for (clp = curbp->firstline(); clp != curbp->lastline(); clp = clp->forw()) {
    if (cln-- == 0) {
      break;
    }
  }

  if (clp->length() >= opos) {
    curwp->setDot(clp, opos);
  } else {
    curwp->setDot(clp, clp->length());
  }

  curwp->setFlags(WINSCR::WFMOVE);
  return T;
}

/*
 * Delete a file. Not bound.
 */

CMD
unlinkfile() {
  auto ofname = getbufdir();
  CMD s;

  complete = filematch;

  if ((s = WDGedit(ECSTR("Delete file: "), ofname, NFILEN)) != T) {
    return s;
  }

  return removefile(ofname, false) ? T : NIL;
}

bool
removefile(const EMCHAR* fname, bool flag) {
  if (flag == false) {
    EMCHAR buf[NFILEN + 16];

    (void)emstrcpy(buf, ECSTR("Delete "));
    (void)emstrcat(buf, fname);
    (void)emstrcat(buf, ECSTR(" ? "));

    if (WDGyn(buf) != T) {
      return false;
    }
  }

  if (ffremove(fname) == FIOSUC) {
    if (flag) {
      WDGmessage(ECSTR("File deleted."));
    }
    return true;
  } else {
    if (!flag) {
      WDGmessage(ECSTR("Can't delete file."));
    }
    return false;
  }
}

/*
 * Print the current buffer on the printer connected to the system.
 */

CMD
printbuffer() {
  WDGprint();
  return T;
}
