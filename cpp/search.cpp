#if     !defined(lint)
static  char rcsid[] = "$Id: search.cpp,v 1.25 2018/09/04 05:13:09 jullien Exp $";
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
 * The functions in this file implement commands that search in the
 * forward and backward directions.  There are no special characters
 * in the search strings.
 */

#include "emacs.h"
#include <chrono>
#include <thread>

static EMCHAR* NOMATCH = ECSTR("No match.");
static int upline = 0;

extern int     indento;
extern EDLINE* indentp;
extern int     commento;

Point found;

static bool replace(bool prompt);
static bool quotep(const EDLINE* l, int i);
static bool instringp(const EDLINE* clp, int cbo);
static void mlmatch(const EDLINE* clp, int cbo);
static CMD  readpattern(const EMCHAR* prompt);
static bool bfindstring();
static void saveindent(EDLINE *clp, int cbo);

/*
 * Compare two characters.  The "bc" comes from the  buffer. It
 * has it's case folded out. The "pc" is from the pattern.
 */

static inline bool
cmpchars(int bc, int pc) {
  if (opt::case_sensitivity) {
    /*
     * character must match.
     */
    return bc == pc;
  } else {
    return tolower(bc) == tolower(pc);
  }
}

/*
 * Internal search forward. The string to be search is in 'pat'.
 * This function, set global variable found_p and found_o to the
 * the position of next match.
 */

bool
ffindstring() {
  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());

  EMCHAR c;
  while (clp != curbp->lastline()) {
    if (cbo == clp->length()) {
      clp = clp->forw();
      cbo = 0;
      c   = '\n';
    } else {
      c = clp->get(cbo++);
    }

    if (cmpchars(c, search_buffer[0])) {
      auto tlp = clp;
      auto tbo = cbo;
      EMCHAR* s;
      for (s = &search_buffer[1]; *s != 0; s++) {
        if (tlp == curbp->lastline()) {
          return false;
        }

        if (tbo == tlp->length()) {
          tlp = tlp->forw();
          tbo = 0;
          c   = '\n';
        } else {
          c = tlp->get(tbo++);
        }

        if (!cmpchars(c, *s)) {
          break;
        }
      }
      if (*s == 0) {
        found.set(tlp, tbo);
        return true;
      }
    }
  }

  return false;
}

/*
 * Internal search backward. The string to be search is in 'pat'.
 * This function, set global variable found_p and found_o to the
 * the position of next match.
 */

static bool
bfindstring() {
  EMCHAR  c;
  EMCHAR* epp;
  EMCHAR* pp;

  for (epp = &search_buffer[0]; epp[1] != 0; ++epp) {
    continue;
  }

  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());

  for (;;) {
    if (cbo == 0) {
      clp = clp->back();
      if (clp == curbp->lastline()) {
        return false;
      }
      cbo = clp->length() + 1;
    }

    if (--cbo == clp->length()) {
      c = '\n';
    } else {
      c = clp->get(cbo);
    }

    if (cmpchars(c, *epp)) {
      auto tlp = clp;
      auto tbo = cbo;

      for (pp = epp; pp != &search_buffer[0]; pp--) {
        if (tbo == 0) {
          tlp = tlp->back();
          if (tlp == curbp->lastline()) {
            return false;
          }
          tbo = tlp->length()+1;
        }

        if (--tbo == tlp->length()) {
          c = '\n';
        } else {
          c = tlp->get(tbo);
        }

        if (!cmpchars(c, *(pp - 1))) {
          break;
        }
      }

      if (pp == &search_buffer[0]) {
        found.set(tlp, tbo);
        return true;
      }
    }
  }
}

/*
 * Auxiliary function for 'global' and 'query'.
 */

static bool
replace(bool prompt) {
  int     replaced;
  int     c = 0;
  EMCHAR  opat[NPAT];
  EMCHAR  npat[NPAT];

  if (freadonly()) {
    return false;
  }
  
  /*
   * compute the line number of curwp->dotp. Since this pointer
   * may change in a substitution we can't save it as the usual
   * way.
   */

  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());

  int cln = 0;
  for (clp = curbp->firstline(); clp != dot.line(); clp = clp->forw()) {
    ++cln;
  }

  {
    const EMCHAR* msgo;
    const EMCHAR* msgn;

    if (prompt) {
      msgo = ECSTR("Query replace old string: ");
      msgn = ECSTR("Query replace new string: ");
    } else {
      msgo = ECSTR("Replace old string: ");
      msgn = ECSTR("Replace new string: ");
    }

    if (WDGchange(msgo, msgn, (EMCHAR*)opat, (EMCHAR*)npat, NPAT) != T) {
      return false;
    } else {
      (void)emstrcpy(search_buffer, opat);
    }
  }

  auto patl = emstrlen(opat);

  for (replaced = 0; ffindstring() && c != 'q' && c != '.';) {
    curwp->setDot(found);
    curwp->setFlags(WINSCR::WFHARD);

    if (!prompt) {
      subst(patl, (EMCHAR*)npat);
      replaced++;
    } else {
      WDGmessage(ECSTR("Query-Replace mode "));
      display->update();
      for (c = '?'; c == '?';) {
        switch (c = TTYgetc()) {
        case '!' :
          subst(patl, (EMCHAR*)npat);
          replaced++;
          display->update();
          prompt = false;
          break;
        case ',' :
          subst(patl, (EMCHAR*)npat);
          replaced++;
          display->update();
          prompt = false;
          break;
        case 'Y' :
        case 'y' :
        case ' ' :
        case '.' :
          subst(patl, (EMCHAR*)npat);
          replaced++;
          display->update();
          break;
        case 0x07 :     /* ABORT */
          WDGwrite(ECSTR("Quit"));
          c = 'q';
          break;
        case 'Q' :
        case 'q' :
        case 0x1B: /* ESC */
          c = 'q';
          break;
        case 'N'  :
        case 'n'  :
        case 0x08 :
          break;
        default  :
          WDGmessage(ECSTR("Help: ' ' yes, DEL no, '.'"
                           " change and exit, '!' all, ESC quit."));
          c = '?';
        }
      }
    }
  }

  for (clp = curbp->firstline(); cln--;) {
    clp = clp->forw();
  }

  curwp->setDot(clp, cbo);
  curwp->setFlags(WINSCR::WFHARD);

  if (c == 'q') {
    return false;
  }
  
  WDGwrite(ECSTR("Replaced %d occurence(s)"), replaced);
  return true;
}

/*
 * Substitute 'newstr'  string of 'length' characters at current
 * position in the buffer. Call lchange to ensure that redisplay
 * is done in.
 */

void
subst(int length, const EMCHAR* newstr) {
  auto obmode(curbp->editMode());

  curbp->setEditMode(EDITMODE::FUNDAMENTAL);

  for (int i = 0; i < length; ++i) {
    (void)backdel();
  }

  for (; *newstr; ++newstr) {
    if (*newstr == '\n') {
      (void)lnewline();
    } else {
      (void)linsert(*newstr);
    }
  }

  curbp->setEditMode(obmode);
  lchange(WINSCR::WFHARD);
}

/*
 * Read a pattern.  Stash it in the external variable "pat". The
 * "pat"  is not updated if the user types in an empty line.  If
 * the  user  typed an empty line,  and there is no old pattern,
 * it  is  an  error.  Display the old pattern,  in the style of
 * Jeff Lomicka. There is some do-it-yourself control expansion.
 */

static CMD
readpattern(const EMCHAR* prompt) {
  complete = nullptr;

  return WDGedit(prompt, search_buffer, NPAT);
}

/*
 * Auxiliary  function  which  returns  T if "." is not a quoted
 * character like 'c' in C mode or #/c in Lisp mode.
 */

static bool
quotep(const EDLINE* l, int i) {
  if (i < 1) {
    return false;
  }
  
  int c;

  if ((c = l->get(i - 1)) == '\'' &&
      (curbp->editMode() == EDITMODE::CMODE      ||
       curbp->editMode() == EDITMODE::CPPMODE    ||
       curbp->editMode() == EDITMODE::CSHARPMODE ||
       curbp->editMode() == EDITMODE::PERLMODE   ||
       curbp->editMode() == EDITMODE::SHELLMODE  ||
       curbp->editMode() == EDITMODE::JAVAMODE)) {
    if (i >= 3 && (c = l->get(i - 3)) == '\'') {
      return false;
    } else {
      return true;
    }
  } else {
    if (curbp->editMode() == EDITMODE::LISPMODE
        && (c == '/')
        && (i < 2)
        && (l->get(i - 2) == '#')) {
      return true;
    }
  }

  return false;
}

static bool
instringp(const EDLINE* clp, int cbo) {
  bool dblquote{false};

  for (int i = cbo; i >= 0; i--) {
    if (clp->get(i) == '"' && !quotep(clp, i)) {
      dblquote = !dblquote;
    }
  }

  return dblquote;
}

/*
 * If auto-match  should  change  page  display,   the  matched
 * character comes in mode-line at the bottom of the screen.
 */

static void
mlmatch(const EDLINE* clp, int cbo) {
  int     i = 0;
  int     j = 0;
  int     pos   = 0;
  int     count = 0;
  EMCHAR  c;
  EMCHAR  mlline[NLINE];

  auto s(clp->text());
  auto maxchar(clp->length());

  while ((c = *s++) != '\000' && (i < TTYncol - 1) && (i < maxchar + j)) {
    if (count++ == cbo) {
      pos = i;
    }
    if (c == '\t') {
      do {
        mlline[i++] = ' ';
        j++;
      } while (i % opt::tab_display);
      /*
       * j is only the count of extra character.  Since \t always
       * print at least one blank, we must take count this one and
       * decrement j.
       */
      --j;
    } else {
      mlline[i++] = c;
    }
  }

  mlline[i] = '\0';

  WDGwrite(ECSTR("%s"), mlline);

  if (widget.w_write == mlwrite) {
    TTYmove(TTYnrow, pos);
    TTYflush();
  }
}

bool
rmatchc(int patc, bool printflag) {
  int     nbmatch = 0;
  const auto lisp = (curbp->editMode() == EDITMODE::LISPMODE);
  EMCHAR  matchpat;
  int     max;
  int     c;

  switch (patc) {
  case '(' : matchpat = ')'; break;
  case '{' : matchpat = '}'; break;
  case '[' : matchpat = ']'; break;
  default  : return false;
  }

  if (lgetdot() == matchpat) {
    (void)forwchar();
  }
  
  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());

  if (lisp) {
    max = lastlisp(clp) + 1;
  } else {
    max = clp->length();
  }

  while (clp != curbp->lastline()) {
    if (cbo >= max) {
      clp = clp->forw();
      cbo = 0;
      if (lisp) {
        max = lastlisp(clp) + 1;
      } else {
        max = clp->length();
      }
      c   = '\n';
    } else {
      c = clp->get(cbo++);
    }

    if (quotep(clp, cbo-1)) {
      continue;
    }

    if (c == patc) {
      nbmatch++;
      continue;
    }

    if (c == matchpat && --nbmatch == 0) {
      curwp->setDot(clp, cbo - 1);
      curwp->setFlags(WINSCR::WFMOVE);
      return true;
    }
  }

  if (printflag) {
    WDGmessage(NOMATCH);
  }
  
  return false;
}

bool
lmatchc(int patc, bool printflag) {
  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());
  auto mode(curbp->editMode());
  auto nbmatch(0);
  auto strp(false);
  auto indx((cbo >= clp->length()) ? cbo - 1 : cbo);

  commento = 0;

  if (quotep(clp, indx) || instringp(clp, indx)) {
    return false;
  }
  
  EMCHAR matchpat;
  switch (patc) {
  case '>' : matchpat = '<'; break;
  case ')' : matchpat = '('; break;
  case '}' : matchpat = '{'; break;
  case ']' : matchpat = '['; break;
  default  : return false;
  }

  if (clp->get(indx) == matchpat) {
    (void)backchar();
  }

  if (mode == EDITMODE::LISPMODE && (cbo > lastlisp(clp) + 1)) {
    return false;
  }
  
  for (;;) {
    while (cbo < 0 ||
           ((mode == EDITMODE::CMODE      ||
             mode == EDITMODE::CPPMODE    ||
             mode == EDITMODE::CSHARPMODE ||
             mode == EDITMODE::PERLMODE   ||
             mode == EDITMODE::JAVAMODE) &&
            commento != 0)) {
      clp = clp->back();
      upline++;
      if (clp == curbp->lastline()) {
        if (printflag) {
          WDGmessage(NOMATCH);
        }
        return false;
      }
      switch (mode) {
      case EDITMODE::CMODE:
      case EDITMODE::CPPMODE:
      case EDITMODE::CSHARPMODE:
      case EDITMODE::PERLMODE:
      case EDITMODE::JAVAMODE:
      case EDITMODE::FORTRANMODE:
      case EDITMODE::PROLOGMODE:
      case EDITMODE::SHELLMODE:
        cbo = lastc(clp);
        break;
      case EDITMODE::LISPMODE:
        cbo = lastlisp(clp);
        break;
      default:
        cbo = clp->length();
      }
    }

    if ((cbo == clp->length()) || cbo < 0) {
      --cbo;
      continue;
    }

    auto c = clp->get(cbo--);

    if (quotep(clp, cbo+1) || (c == '"' && (strp = !strp) != false) || strp) {
      continue;
    }
    
    if (c == patc) {
      nbmatch++;
      continue;
    }

    if (c == matchpat && --nbmatch == 0) {
      saveindent(clp, ++cbo);
      curwp->setDot(clp, cbo);
      curwp->setFlags(WINSCR::WFMOVE);
      return true;
    }
  }
}

/*
 * Save the current indentation point in (indentp, indento).
 */

static void
saveindent(EDLINE *clp, int cbo) {
  indentp = clp;

  switch (curbp->editMode()) {
  case EDITMODE::LISPMODE	:
    indento = cbo;
    break;
  case EDITMODE::CMODE	:
	case EDITMODE::CPPMODE:
	case EDITMODE::CSHARPMODE:
	case EDITMODE::PERLMODE:
	case EDITMODE::JAVAMODE:
	case EDITMODE::FORTRANMODE:
	case EDITMODE::SHELLMODE:
	case EDITMODE::PROLOGMODE:
    indento = clp->leftmargin();
    break;
	default:
    indento = 0;
	}
}

/*
 * The function automatch search  backward in the  file to find
 * a special caracter (in general '(', '[' or '{') depending on
 * the mode set. The redisplay screen at cursor wait 1/3 second
 * and go  back to the  previous  position to the corresponding
 * ('}', ']' or ')').
 */

bool
automatch(int c, bool f) {
  const auto& dot(curwp->getDot());
  auto crow(DISPLAY::_currow - (int)curwp->toprow());
  auto res(false);
  EDLINE* mlp{nullptr};
  int     mbo{0};

  upline = 0;

  if (backchar() == T && lmatchc(c, f) && f) {
    res = true;;
    if (upline <= crow) {
      display->update();
      TTYcshow(true);
      waitmatch(1);
      TTYcshow(false);
    } else {
      /*
       * get moved dot.
       */
      mlp = curwp->line();
      mbo = curwp->pos();;
    }
  }

  curwp->setDot(dot); // reposition
  curwp->setFlags(WINSCR::WFMOVE);

  if (mlp != nullptr) {
    mlmatch(mlp, mbo);
    waitmatch(1);
  }

  return res;
}

/*
 * Wait 1/3 second for every match.
 */

void
waitmatch(int n) {
  static constexpr auto TEMPO(333);
  const std::chrono::milliseconds wait{n * TEMPO};
  const auto end = std::chrono::high_resolution_clock::now() + wait;

  while (std::chrono::high_resolution_clock::now() < end) {
    WDGwait();
  }
}

/*
 * Search  forward.  Get  a search string  from  the  user,  and
 * search, beginning at ".", for the string. If found, reset the
 * "." to be just after the match string,  and [perhaps] repaint
 * the display. Bound to "M-S".
 */

CMD
forwsearch() {
  thisflag |= CFFSRC;

  if (!(lastflag & (CFFSRC | CFBSRC))) {
    CMD s;

    if ((s = readpattern(ECSTR("Search: "))) != T) {
      thisflag &= ~CFFSRC;
      return s;
    }
  } else {
    WDGwrite(ECSTR("Search \"%s\": "), search_buffer);
  }

  if (lastflag & CFFAIL) {
    lastflag &= ~CFFAIL;
    (void)gotobob();
    return forwsearch();
  }

  if (ffindstring()) {
    curwp->setDot(found);
    curwp->setFlags(WINSCR::WFMOVE);
    return T;
  } else {
    TTYbeep();
    WDGwrite(ECSTR("Failing search: %s"), search_buffer);
    thisflag |= CFFAIL;
    return NIL;
  }
}

/*
 * Reverse  search.  Get  a  search string from  the user, and
 * search,  starting at "." and proceeding toward the front of
 * the  buffer.  If  found "." is left pointing  at the  first
 * character of  the pattern  [the  last  character  that  was
 * matched]. Bound to "M-R".
 */

CMD
backsearch() {
  thisflag |= CFBSRC;

  if (!(lastflag & (CFFSRC | CFBSRC))) {
    CMD s;

    if ((s = readpattern(ECSTR("Search backward: "))) != T) {
      thisflag &= ~CFBSRC;
      return s;
    }
  } else {
    WDGwrite(ECSTR("Search backward \"%s\": "), search_buffer);
  }

  if (lastflag & CFFAIL) {
    lastflag &= ~CFFAIL;
    (void)gotoeob();
    return backsearch();
  }

  if (bfindstring()) {
    curwp->setDot(found);
    curwp->setFlags(WINSCR::WFMOVE);
    return T;
  } else {
    TTYbeep();
    WDGwrite(ECSTR("Failing search backward: %s"), search_buffer);
    thisflag |= CFFAIL;
    return NIL;
  }
}

/*
 * Global replace.  Get  two strings (old and new) from the user
 * and search from the current position all occurences of  'old'
 * and replace them by 'new'. Bound to 'M-&'.
 */

CMD
global() {
  return replace(false) ? T : NIL;
}

/*
 * Query replace.  Get two strings  (old and new)  from the user
 * and search from the current position all occurences  of 'old'
 * and replace them by  'new'  according to  the response of the
 * user until completion or 'q' answer. Bound to 'M-%'.
 */

CMD
query() {
  return replace(true) ? T : NIL;
}

/*
 * Find definition.  This  function  search in whole  text for a
 * specific  definition.  The pattern is different for C or Lisp
 * mode.  For C mode, a definition is of the form  "pattern"  in
 * column 0 and of the form "(XXXX pattern" for Lisp mode.
 */

CMD
getdefinition() {
  EMCHAR  save[NPAT];

  if (curbp->editMode() == EDITMODE::FUNDAMENTAL) {
    return NIL;
  }
  
  (void)emstrcpy(save, search_buffer);
  *search_buffer = '\000';

  CMD s;

  if ((s = readpattern(ECSTR("Search-definition: "))) != T) {
    return s;
  }
  
  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());
  auto len(emstrlen(search_buffer));

  curwp->setDot(curbp->firstline(), 0);

  while (ffindstring()) {
    curwp->setDot(found);
    switch (curbp->editMode()) {
    case EDITMODE::ASMODE:
    case EDITMODE::CMODE:
    case EDITMODE::CPPMODE:
    case EDITMODE::CSHARPMODE:
    case EDITMODE::PERLMODE:
    case EDITMODE::JAVAMODE:
    case EDITMODE::FORTRANMODE:
    case EDITMODE::PROLOGMODE:
    case EDITMODE::SHELLMODE:
      if (found.pos() == len) {
        curwp->setFlags(WINSCR::WFMOVE);
        (void)emstrcpy(search_buffer, save);
        (void)backline();
        (void)reposition();
        return T;
      }
      break;
    case EDITMODE::LISPMODE:
      if (backword() == NIL ||
          backword() == NIL ||
          backchar() == NIL ||
          curwp->pos() != 0 || curwp->line()->get(0) != '(') {
        curwp->setDot(found);
      } else {
        curwp->setFlags(WINSCR::WFMOVE);
        (void)emstrcpy(search_buffer, save);
        (void)backline();
        (void)reposition();
        return T;
      }
      break;
    default:
      break;
    }
  }

  curwp->setDot(clp, cbo);
  (void)emstrcpy(search_buffer, save);

  WDGmessage(ECSTR("Not found"));
  return NIL;

}

/*
 * Complete the current word with previous token in the previous
 * text. If the word found is not correct, another ESC/ continue
 * the search. Bound to ESC-/.
 */

#define REJECT_HISTORY  32
#define REJECT_WORDSIZE 32

using DIRFNP = bool (*)();

static  struct  {
  EMCHAR  startw[REJECT_WORDSIZE];
  int     size;
} reject[REJECT_HISTORY];

CMD
completeword() {
  static int    rejectnb = 0;
  static int    lasto    = 0;
  static int    indx     = 0;
  static DIRFNP find     = bfindstring;

  EMCHAR  buf[NPAT];
  EMCHAR  save[NPAT];
  int     slen;
  int     i;
  int     k;
  CMD     s;
  bool    res;

  thisflag |= CFCPLT;

  if (lastflag & CFCPLT) {
    /*
     * Last command was ESC/, add match to reject history,
     * delete the word and continue.
     */

    if (rejectnb < REJECT_HISTORY) {
      for (i = lasto, k = 0;
           i < curwp->pos() && k < (REJECT_WORDSIZE - 1);
           ++i) {
        reject[rejectnb].startw[k++] = curwp->line()->get(i);
      }
      reject[rejectnb].startw[k] = '\000';
      reject[rejectnb].size      = k;
      rejectnb++;
    }

    while (curwp->pos() > lasto) {
      /*
       * Undo previous completion.
       */
      backdel();
    }
  } else {
    /*
     *      It's  a  new completion,  clear reject buffer
     *      indx and start with a backward search.
     */

    find     = bfindstring;
    rejectnb = 0;
    indx     = 0;
  }

  const auto& dot(curwp->getDot());
  auto clp(dot.line());
  auto cbo(dot.pos());
  
  lasto = cbo;

  /*
   *      Copy the start of match
   */

  (void)backword();

  for (slen = 0, i = curwp->pos(); i < cbo; ++i) {
    buf[slen++] = clp->get(i);
  }

  buf[slen] = 0;

  (void)emstrcpy(save, search_buffer);
  (void)emstrcpy(search_buffer, buf);

loop:
  if (lastflag & CFCPLT) {
    curwp->setDot(found);
  }

  while ((res = find()) == true) {
    /*
     * A  match is found,  point at the start of the
     * match  to  check  if the matching sequence is
     * at the start of a new word.
     */

    if (find == ffindstring) {
      /*
       * For  forward  search,  make the match
       * point  on  the left most character of
       * the match.
       */
      found.setPos(found.pos() - slen);
    }
    if (found.pos() > 0) {
      curwp->setDot(found.line(), found.pos() - 1);
      if (inword()) {
        /* match the middle of a word */
        if (find == ffindstring) {
          found.setPos(found.pos() + slen);
          curwp->setDotPos(found.pos());
        }
        continue;
      }
    }
    break;
  }

  s = (res ? T : NIL);

  if (res) {
    /*
     * Set  current  dot  to  point on the left most character of the match.
     */

    curwp->setDot(found.line(), found.pos() + slen);

    /*
     *      Copy new characters in a temporary buffer.
     */

    for (i = 0; inword(); ++i) {
      buf[i] = lgetdot();
      (void)forwchar();
    }

    buf[i] = '\000';

    /*
     *      Search  in  reject  buffers  to  see  if  the
     *      current match has not already rejected.
     */

    for (k = 0; k < rejectnb; k++)
      if ((i == reject[k].size) && emstrcmp(buf, reject[k].startw) == 0) {
        if (find == ffindstring) {
          found.setPos(found.pos() + slen);
          curwp->setDotPos(found.pos());
        } else {
          curwp->setDotPos(found.pos());
        }
        goto loop;
      }

    /*
     *      Reset current dot position and add completion
     */

    curwp->setDot(clp, cbo);

    for (i = 0; buf[i] != '\000'; i++) {
      if (!linsert(buf[i])) {
        s = NIL;
        break;
      }
    }
  } else {
    curwp->setDot(clp, cbo);

    if (find == bfindstring) {
      /*
       * Start forward search from dot.
       */

      found.set(clp, cbo);

      find    = ffindstring;
      goto loop;
    } else {
      EMCHAR tagbuf[NLINE];

      indx = completeintag(indx, search_buffer, tagbuf);

      if (indx != 0) {
        /*
         * Search in reject buffers to see if the current match has
         * not already rejected.
         */

        for (k = 0; k < rejectnb; ++k) {
          if (emstrcmp(tagbuf, reject[k].startw) == 0) {
            continue;
          }
        }

        /*
         * Reset current dot position and add completion
         */

        curwp->setDot(clp, cbo);

        for (i = slen; tagbuf[i] != '\000'; i++) {
          if (!linsert(tagbuf[i])) {
            break;
          }
        }
        
        (void)emstrcpy(search_buffer, save);
        return T;
      }

      thisflag &= ~CFCPLT;
      if (rejectnb == 0) {
        WDGwrite(ECSTR("No dynamic expansion for '%s' found"),
                 search_buffer);
      } else {
        WDGwrite(ECSTR("No further dynamic expansion for '%s' found"),
                 search_buffer);
      }
    }
  }

  (void)emstrcpy(search_buffer, save);
  return s;
}

CMD
diffwindows() {
  auto wp1 = curwp;
  auto wp2 = curwp->next();

  if (wp2 == nullptr) {
    wp2 = WINSCR::head();
    if (wp2 == wp1) {
      WDGmessage(ECSTR("no other window"));
      return NIL;
    }
  }

  wp1->setDot(wp1->buffer()->firstline(), 0);
  wp1->setFlags(WINSCR::WFHARD);

  wp2->setDot(wp2->buffer()->firstline(), 0);
  wp2->setFlags(WINSCR::WFHARD);

  return comparewindows();
}

/*
 * Compare the current window and the next one (if any)  starting
 * at current position of the two windows. Display the first line
 * which is different and change the position of the two windows.
 * This command is not bound.
 */

CMD
comparewindows() {
  auto wp1 = curwp;
  auto wp2 = curwp->next();

  if (wp2 == nullptr) {
    wp2 = WINSCR::head();
    if (wp2 == wp1) {
      WDGmessage(ECSTR("no other window"));
      return NIL;
    }
  }

  const auto& dot1(wp1->getDot());
  auto lp1 = dot1.line();
  auto lo1 = dot1.pos();
  auto bp1 = wp1->buffer();

  const auto& dot2(wp2->getDot());
  auto lp2 = dot2.line();
  auto lo2 = dot2.pos();
  auto bp2 = wp2->buffer();

  if (lastflag & CFCOMP) {
    /*
     *      continue on next line
     */

    if (lp1 != bp1->lastline()) {
      lp1 = lp1->forw();
    }

    if (lp2 != bp2->lastline()) {
      lp2 = lp2->forw();
    }
    lo1 = 0;
    lo2 = 0;
  }

  while ((lp1 != bp1->lastline()) && (lp2 != bp2->lastline())) {
    if ((lp1->length()-lo1) == (lp2->length()-lo2) &&
        ((lp1->length()-lo1) == 0 ||
         emstrncmp(lp1->text()+lo1,
                   lp2->text()+lo2,
                   (size_t)(lp1->length()-lo1)) == 0)) {
      lp1 = lp1->forw();
      lp2 = lp2->forw();
      lo1 = 0;
      lo2 = 0;
    } else {
      while ((lo1 < lp1->length()) &&
             (lo2 < lp2->length()) && // BUG?? lo1 or lo2??
             (lp1->get(lo1) == lp2->get(lo2))) {
        lo1++;
        lo2++;
      }

      wp2->setDot(lp2, lo2);
      wp2->setFlags(WINSCR::WFMOVE);

      wp1->setDot(lp1, lo1);
      wp1->setFlags(WINSCR::WFMOVE);

      thisflag    |= CFCOMP;
      return showcpos();
    }
  }

  auto flag = false;

  if (lp1 != bp1->lastline()) {
    WDGwrite(ECSTR("Line(s) added to %s"), bp1->filename());
    flag = true;
  }

  if (lp2 != bp2->lastline()) {
    WDGwrite(ECSTR("Line(s) added to %s"), bp2->filename());
    flag = true;
  }

  if (flag) {
    wp2->setDot(lp2, 0);
    wp2->setFlags(WINSCR::WFMOVE);

    wp1->setDot(lp1, 0);
    wp1->setFlags(WINSCR::WFMOVE);

    thisflag |= CFCOMP;
    return T;
  }

  if (lastflag & CFCOMP) {
    WDGmessage(ECSTR("no other change."));
  } else {
    WDGmessage(ECSTR("no change."));
  }
 
  return T;
}

/*
 * The  functions in this file implement commands that search in
 * the forward and backward directions for matching a character.
 */

CMD
matchrpar() {
  return rmatchc('(') ? T : NIL;
}

CMD
matchrcur() {
  return rmatchc('{') ? T : NIL;
}

CMD
matchrbra() {
  return rmatchc('[') ? T : NIL;
}

CMD
matchlpar() {
  return lmatchc(')') ? T : NIL;
}

CMD
matchlcur() {
  return lmatchc('}') ? T : NIL;
}

CMD
matchlbra() {
  return lmatchc(']') ? T : NIL;
}
