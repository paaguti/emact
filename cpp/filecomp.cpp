#if !defined(lint)
static auto rcsid("$Id: filecomp.cpp,v 1.19 2018/09/09 07:21:10 jullien Exp $");
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
 * Search  for  a  matching file in the current directory.  If a
 * given pattern match a file then the file is returned.
 */

#include "emacs.h"

static void    getdir(EMCHAR* fname, EMCHAR* dmatch, EMCHAR* fmatch);
static EMCHAR* caseconvert(EMCHAR* s);

EMCHAR*
fileaccept(const EMCHAR* prompt, EMCHAR* file) {
  return filematch(prompt, file);
}

#if     defined(_WIN32)
#define cmpname(x, y, n)      emstrnicmp(x, y, n)
#else
#define cmpname(x, y, n)      emstrncmp(x, y, n)
#endif

/*
 * Try  to convert filename on systems that inherit from DOS FAT
 * names  (mainly WINxx and OS/2).  When the name use a mixing
 * case  or  has no extension we leave it unchanged.  Otherwise,
 * we convert it into lower case letter only.
 */

static EMCHAR*
caseconvert(EMCHAR* s) {
#if defined(_WIN32)
  auto p = s;
  auto lowercase{false};
  auto uppercase{false};
  auto nodot{true};

  for (auto c = *p; *p != 0; ++p) {
    if (std::isalpha(c)) {
      if (std::isupper(c)) {
        uppercase = true;
      } else {
        lowercase = true;
      }
    } else if (c == '.') {
      nodot = false;
    }
  }

  if ((uppercase && lowercase) || nodot) {
    return s;
  } else {
    return emstrlwr(s);
  }
#else
  return s;
#endif
}

EMCHAR*
filematch(const EMCHAR* prompt, EMCHAR* file) {
  static  EMCHAR newchar[] = { '?', 0 };
  static  EMCHAR pmatch[NFILEN]; /* pmatch is returned form filematch */

  DIR*    dirp;
  ENTRY*  dp;
  EMSTAT  stb;
  int     len;
  int     len2;
  EMCHAR  fmatch[NFILEN];
  EMCHAR  dmatch[NFILEN];
  EMCHAR  line[NFILEN];

loop:
  (void)getdir(file, dmatch, fmatch);
  len  = ((emstrcmp(fmatch, ECSTR(".")) == 0) ? 0 : emstrlen(fmatch));
  len2 = emstrlen(dmatch) - 1;

  if (*dmatch && (dmatch[len2] != '/')) {
    dmatch[++len2] = '/';
    dmatch[len2]   = '\000';
  }

  dirp = emopendir(*dmatch ? dmatch : ECSTR("."));

  if (!dirp) {
    WDGwrite(ECSTR("No such directory: %s"), dmatch);
    return nullptr;
  }

  while ((dp = readdir(dirp)) != nullptr) {
    EMCHAR* name = emgetdirentry(dp);
    (void)caseconvert(name);
    (void)emstrcpy(pmatch, dmatch);
    (void)emstrcat(pmatch, name);
    if (ffstat(pmatch, &stb) == 0 &&
        (S_ISREG(stb.st_mode) || S_ISDIR(stb.st_mode)) &&
        (len == 0 || !cmpname(name, fmatch, (size_t)len))) {
      auto isdir = S_ISDIR(stb.st_mode);
      EMCHAR* p;
      EMCHAR* s;
      EMCHAR* fmt;

      if (!emstrcmp(name, ECSTR(".")) || !emstrcmp(name, ECSTR(".."))) {
        continue;
      }

      /* find extension */

      for (s = p = name; *p; p++) {
        if (*p == '.') {
          s = p;
        }
      }

      --p;

      /* ignore some extension */

      if (!emstrcmp(s, ECSTR(".o")) ||
          !emstrcmp(s, ECSTR(".obj")) ||
          !emstrcmp(s, ECSTR(".com")) ||
          !emstrcmp(s, ECSTR(".class")) ||
          !emstrcmp(s, ECSTR(".exe")) ||
          !emstrcmp(s, ECSTR(".dll")) ||
          !emstrcmp(s, ECSTR(".lap")) ||
          !emstrcmp(s, ECSTR(".a")) ||
          !emstrcmp(s, ECSTR(".old")) ||
          !emstrcmp(s, ECSTR(".BAK")) ||
          !emstrcmp(s, ECSTR(".bak"))) {
        continue;
      }

      if (*p == '~' || *p == '#') {
        continue;
      }

      fmt = (emunicode() ? ECSTR("%ls%ls") : ECSTR("%s%s"));

      (void)emsprintf2(line, fmt, pmatch, (isdir ? ECSTR("/") : ECSTR("")));

      WDGupdate(prompt, line);

      switch ((newchar[0] = (EMCHAR)TTYgetc())) {
      case 0x03: /* Ctrl-C */
        (void)closedir(dirp);
        complete.setStatus(Completion::Status::COMPLETE_AGAIN);
        *pmatch    = '\000';
        return pmatch;
      case 0x04: /* Ctrl-D */
        (void)closedir(dirp);
        (void)newfile(updir(pmatch, NOSLASH));
        complete.setStatus(Completion::Status::COMPLETE_ABORT);
        return nullptr;
      case 0x07: /* Ctrl-G */
        (void)closedir(dirp);
        TTYbeep();
        WDGmessage(ECSTR("Quit"));
        complete.setStatus(Completion::Status::COMPLETE_ABORT);
        return nullptr;
      case 0x0D: /* Ctrl-M */
      case 0x0A: /* Ctrl-J */
        (void)closedir(dirp);
        return pmatch;
      case 0x1B: /* ESC */
        (void)closedir(dirp);
        file = updir(pmatch, NOSLASH);
        len  = emstrlen(file);
        if (*file == 0 || (len == 2 && file[1] == ':')) {
          /*
           * Special case when we are at the root of the device.  Add
           * a / and restart match.
           */
          (void)emstrcat(file, ECSTR("/"));
          complete.setStatus(Completion::Status::COMPLETE_AGAIN);
          (void)emstrcpy(pmatch, file);
          return pmatch;
        }
        goto loop;
      case '\t':
      case ' ':
        break;
      default :
        if (isdir) {
          /*
           * Inspect a new directory.
           */
          (void)closedir(dirp);
          if (newchar[0] != 0x08) {
            (void)emstrcat(pmatch, ECSTR("/"));
            (void)emstrcat(pmatch, newchar);
          }
          complete.setStatus(Completion::Status::COMPLETE_AGAIN);
          return pmatch;
        }
        TTYbeep();
      }
    }
  }

  (void)closedir(dirp);

  complete.setStatus(Completion::Status::COMPLETE_AGAIN);
  TTYbeep();
  return file;
}

/*
 * Normalize  and  then split a path fname into two components :
 * directory 'dmatch' and filename 'fmatch'.
 */

static void
getdir(EMCHAR* fname, EMCHAR* dmatch, EMCHAR* fmatch) {
  fname = normalize(fname, SLASH);
  (void)emstrcpy(dmatch, fname);
  (void)emstrcpy(fmatch, fname);

  const EMCHAR* r{nullptr};
  int j{0};

  for (int i = 0; *fname; ++i, ++fname) {
    if ((*fname == '/') || (*fname == ':')) {
      r = fname + 1;
      j = i;
    }
  }

  if (r != nullptr) {
    (void)emstrcpy(fmatch, r);
    dmatch[++j] = '\000';
  } else {
    dmatch[0] = '\000';
  }
}

/*
 * This function put buffer un DIRED mode (RETURN on a line loads
 * the file of the current line).
 */

CMD
dired() {
  EMCHAR fname[NFILEN];

  complete = filematch;

  (void)emstrcpy(fname, curbp->filename());
  (void)ffullname(fname, ECSTR("file"));

  (void)updir(fname, SLASH);
  auto s = WDGedit(ECSTR("Dired (directory): "), fname, NFILEN);

  if (s == ABORT) {
    return s;
  }

  return newfile(fname) ? T : NIL;
}

bool
diredbuffer(const EMCHAR* fname) {
  EMCHAR  buf[NFILEN];
  EMCHAR  bname[BUFFER::NBUFN];

  fname = normalize(const_cast<EMCHAR*>(fname), NOSLASH);
  makename(&bname[0], fname);

  auto bp(BUFFER::find(bname, true, EDITMODE::DIRED));

  if (bp == nullptr) {
    return false;
  }

  (void)ffullname(bp->filename(), fname);

  bp->setChanged(false);  // Don't complain!
  bp->setReadonly(true);  // Can't modify buffer

  /* Blow old text away   */
  if (!bp->clear()) {
    return false;
  }

  const bool rootp{fname[emstrlen(fname) - 1] == '/'};

  DIR* dirp{emopendir(fname)};
  if (dirp == nullptr) {
    WDGwrite(ECSTR("Can't open directory: %s"), fname);
    return false;
  }

  EMCHAR mark[DIREDMARK + 1];

  for (int i = 0; i < DIREDMARK; ++i) {
    mark[i] = ' ';
  }
  mark[DIREDMARK] = '\000';

  int nfiles = 0;

  try {
    for (auto dp = readdir(dirp); dp != nullptr; dp = readdir(dirp)) {
      (void)emstrcpy(buf, mark);
      (void)emstrcat(buf, fname);
      if (!rootp) {
        (void)emstrcat(buf, ECSTR("/"));
      }
      (void)emstrcat(buf, caseconvert(emgetdirentry(dp)));

      EDLINE::append(bp, buf);
      ++nfiles;
    }
  } catch(...) {
    (void)closedir(dirp);
    return false;
  }

  (void)closedir(dirp);
  (void)curwp->connect(bp);
  (void)gotobob();

  WDGwrite(ECSTR(": %d file(s) found."), nfiles);

  return true;
}

/*
 * Process dired commands 'f', 'd', 'x', 'R' or TAB.
 */

CMD
diredcmd(int c) {
  EMCHAR  buf[NFILEN];
  EMCHAR  newname[NFILEN];
  EMCHAR* pfname = &buf[DIREDMARK];
  EMCHAR* p;
  EMCHAR* s;
  int     removed = 0;
  int     asked;

  auto lp = curwp->line();

  if (c != 'x' && c != 0x08) {
    if (lp == curbp->lastline() || lp->length() == 0) {
      return ctrlg();
    }
  }

  (void)emstrncpy(buf, lp->text(), (size_t)lp->length());
  buf[lp->length()] = '\000';

  switch (c) {
  case 'd':
    if (lp->length() == 0) {
      return ctrlg();
    }

    curbp->setReadonly(false);

    if (lp->get(0) == 'D') {
      lp->put(0, ' ');
    } else {
      lp->put(0, 'D');
    }

    BUFFER::change(WINSCR::WFEDIT);
    (void)forwline();

    curbp->setReadonly(true);
    curbp->setChanged(false);
    return T;
  case 'f':
    return newfile(pfname) ? T : NIL;
  case 'n':
    return forwline();
  case 'p':
    return backline();
  case 'v':
    if (!readin(pfname)) {
      return NIL;
    }
    if (!curbp->readonly()) {
      curbp->setReadonly(true);
    }
    return T;
  case 'x' :
    curbp->setReadonly(false);
    (void)gotobob();

    asked = 0;

    do {
      if (curwp->line()->get(0) == 'D') {
        asked++;

        (void)emstrncpy(buf,
                        curwp->line()->text(),
                        (size_t)curwp->line()->length());
        buf[curwp->line()->length()] = '\000';

        (void)ffchmod(pfname, false);

        if (removefile(pfname, true)) {
          removed++;
          (void)gotobol();
          (void)EDLINE::ldelete(curwp->line()->length() + 1);
          (void)backline();
        }
      }
    } while (curwp->line() != curbp->lastline() && forwline() == T);

    curbp->setReadonly(true);
    curbp->setChanged(false);

    (void)gotobob();

    switch (asked) {
    case 0 :
      curwp->setDot(lp, 0);
      WDGwrite(ECSTR("(No deletions requested)"));
      break;
    case 1 :
      WDGwrite(ECSTR("%d deletion done"), removed);
      break;
    default:
      WDGwrite(ECSTR("%d deletions done"), removed);
    }

    return T;
  case 'R' :
    p = s = &newname[0];
    (void)emstrcpy(newname, pfname);

    while (*s) {
      if (*s == '\\' || *s == '/') {
        p = s;
      }
      s++;
    }

    {
      EMCHAR prompt[60 + NFILEN];

      (void)emstrcpy(prompt, ECSTR("Rename "));
      (void)emstrcat(prompt, p + 1);
      (void)emstrcat(prompt, ECSTR(" to: "));

      if (*p == '\\' || *p == '/') {
        *++p = '\000';
      }

      complete = filematch;

      if (WDGedit(prompt, newname, NFILEN) != T) {
        return NIL;
      }

      if (ffrename(pfname, newname) != FIOSUC) {
        WDGwrite(ECSTR("Could not rename to %s"), newname);
        return NIL;
      }
      WDGwrite(ECSTR("Move: 1 file"));
    }

    (void)gotobob();

    (void)emstrncpy(buf,
                    curwp->line()->text(),
                    (size_t)curwp->line()->length());
    buf[curwp->line()->length()] = '\000';

    return diredbuffer(pfname) ? T : NIL;
  case 0x08 : /* ^H */
    if (backline() == T && gotobol() == T && lp->get(0) == 'D') {
      curbp->setReadonly(false);
      curwp->line()->put(0, ' ');
      BUFFER::change(WINSCR::WFEDIT);
      curbp->setReadonly(true);
      curbp->setChanged(false);
    }
    return T;
  default :
    return ctrlg();
  }
}
