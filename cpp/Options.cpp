#if !defined(lint)
static auto rcsid("$Id: options.cpp,v 1.16 2018/09/09 07:21:10 jullien Exp $");
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
 * Display a small help screen. Bound to M-?.
 */

#include "./emacs.h"
#include "./Editor.h"
#include "./Buffer.h"
#include "./Completion.h"
#include "./EditWindow.h"
#include "./EditorVariable.h"
#include "./Kbdm.h"
#include "./Line.h"
#include "./MiniBuf.h"
#include "./Options.h"
#include "./Redisplay.h"
#include "./Terminal.h"
#include "./Word.h"
#include "./Widget.h"

extern Kbdm kbdm;
extern Completion complete;  // Automatic completion

static void    getkeyname(int key, EMCHAR *buf);
static EMCHAR* varmatch(const EMCHAR* prompt, EMCHAR* buf);
static void    printcmd(int c, Buffer* bp);
static void    printmacro(const EMCHAR* name, Buffer* bp);
static CMD     internalfindtag(int lineno);

int Options::tagfound = -1;

/*
 * Describe  the  next  key  or  the entire binding if RETURN is
 * pressed.
 */
CMD
Options::describekey() {
  int     c;
  EMCHAR  ch[2];
  EMCHAR  meta[10];

  WDGwrite(ECSTR("Apropos keyword: "));

  if ((c = Editor::getkey()) == (Ctrl|'M')) {
    return help();
  }

  getkeyname(c, meta);

  ch[0] = (EMCHAR)(c & MAX_EMCHAR);
  ch[1] = '\000';

  for (const auto& macro : Editor::getMacros()) {
    if (macro.code() == c) {
      WDGwrite(ECSTR("%s%s is bound to: %s"), meta, ch, macro.name());
      return T;
    }
  }

  for (const auto& ktp : Editor::_keytab) {
    if (ktp.code() == c) {
      WDGwrite(ECSTR("%s%s is bound to: %s"), meta, ch, ktp.name());
      return T;
    }
  }

  if (self_insert(c)) {
    WDGwrite(ECSTR("%s is bound to self-insert"), ch);
  } else {
    WDGwrite(ECSTR("%s%s is not bound!"), meta, ch);
  }

  return NIL;
}

/*
 * Get the printable keyname of a command.
 */

static void
getkeyname(int key, EMCHAR* buf) {
  buf[0] = '\0';

  if (key & CXDR) {
    (void)emstrcat(buf, ECSTR("C-X-$"));
  }
  if (key & CTLX) {
    (void)emstrcat(buf, ECSTR("C-X"));
  }
  if (key & CTLC) {
    (void)emstrcat(buf, ECSTR("C-C"));
  }
  if (key & META) {
    (void)emstrcat(buf, ECSTR("M-"));
  }
  if (key & Ctrl) {
    (void)emstrcat(buf, ECSTR("C-"));
  }
}

/*
 * Decribe all the current keymap binding on a help window.
 */

CMD
Options::help() {
  static constexpr int COLUMN_VALUE{30};
  int     i;
  EMCHAR  ch[2];
  EMCHAR  meta[10];
  EMCHAR  line[NLINE];
  EMCHAR  num[16];

  /*
   * find or create buffer if it does not exist.
   */
  auto bp = Buffer::find(BUF_HELP);
  bp->setChanged(false);

  /* Blow old text away */
  if (!bp->clear()) {
    return NIL;
  }

  Line::append(bp, ECSTR("===== Standard key definition ====="));

  for (const auto& ktp : Editor::_keytab) {
    auto c(ktp.code());
    if ((c & SPCL) && c != UNBOUND) {
      continue;
    }
    getkeyname(c, meta);
    (void)emstrcpy(line, ktp.name());
    for (i = emstrlen(line); i < COLUMN_VALUE; ++i) {
      line[i] = ' ';
    }
    line[i++] = '(';
    line[i]   = '\0';
    ch[0]     = (EMCHAR)(c & MAX_EMCHAR);
    ch[1]     = '\000';

    if (c == UNBOUND) {
      (void)emstrcat(line, ECSTR("unbound)"));
    } else {
      (void)emstrcat(line, meta);
      if ((c & MAX_EMCHAR) == 0x7F) {
        (void)emstrcat(line, ECSTR("DEL)"));
      } else {
        (void)emstrcat(line, ch);
        (void)emstrcat(line, ECSTR(")"));
      }
    }
    Line::append(bp, line);
  }

  Line::append(bp, ECSTR("====== User macro definition ======"));

  for (const auto& macro : Editor::getMacros()) {
    /* Look in macro table. */
    auto c(macro.code());
    if ((c & SPCL) && c != -1) {
      continue;
    }
    (void)emstrcpy(line, macro.name());
    for (i = emstrlen(line); i < COLUMN_VALUE; ++i) {
      line[i] = ' ';
    }
    line[i] = '\0';
    if (c != -1) {
      getkeyname(c, meta);
      ch[0] = (EMCHAR)(c & MAX_EMCHAR);
      ch[1] = '\000';
      (void)emstrcat(line, ECSTR("("));
      (void)emstrcat(line, meta);
      (void)emstrcat(line, ch);
      (void)emstrcat(line, ECSTR(")"));
    } else {
      (void)emstrcat(line, ECSTR("unbound"));
    }

    Line::append(bp, line);
  }

  Line::append(bp, ECSTR("===== Customer variable value ====="));

  for (const auto& vtp : EditorVariable::vartab) {
    (void)emstrcpy(line, vtp.name());
    for (i = emstrlen(line); i < COLUMN_VALUE; ++i) {
      line[i] = ' ';
    }
    line[i] = '\0';
    switch (vtp.type()) {
    case BOOLVAL :
      {
        auto p = vtp.boolp();
        (void)emstrcat(line, (*p ? ECSTR("t") : ECSTR("nil")));
      }
      break;
    case FIXVAL :
      {
        auto p = vtp.intp();
        (void)emsprintf(num, ECSTR("%d"), *p);
        (void)emstrcat(line, num);
      }
      break;
    default :
      (void)emstrcat(line, ECSTR("\""));
      (void)emstrcat(line, vtp.string());
      (void)emstrcat(line, ECSTR("\""));
    }

    Line::append(bp, line);
  }

  (void)bp->show();
  return T;
}

/*
 * Change  the  boolean  value  of  a global Emacs flag.  If the
 * value  was  T  change it to NIL and vice-versa.  This command
 * allows a dynamic control of Emacs options. Bound to C-XR
 */

#define INDEX_UNKNOWN           0
#define INDEX_FUNCTION          1
#define INDEX_VARIABLE          2

static int index_found;
static int index_type;

static EMCHAR*
varmatch(const EMCHAR* prompt, EMCHAR* buf) {
  int len(emstrlen(buf));

  /*
   * Search first for an exact match
   */

  index_found = 0;
  index_type  = INDEX_VARIABLE;
  for (const auto& var : EditorVariable::vartab) {
    if (emstrcmp(var.name(), buf) == 0) {
      return var.name();
    }
    ++index_found;
  }

  index_found = 0;
  index_type  = INDEX_FUNCTION;
  for (const auto& ktp : Editor::_keytab) {
    if (emstrcmp(ktp.name(), buf) == 0) {
      return const_cast<EMCHAR*>(ktp.name());
    }
    ++index_found;
  }

  /*
   * Try to match with the help of the user.
   */

  index_found = 0;
  index_type  = INDEX_VARIABLE;
  for (const auto& var : EditorVariable::vartab) {
    if (len == 0 || emstrncmp(var.name(), buf, len) == 0) {
      if (len != emstrlen(var.name())) {
        WDGupdate(prompt, var.name());
        switch (term->get()) {
        case 0x07:
          WDGwrite(ECSTR("Quit"));
          return nullptr;
        case 0x0D:
        case 0x0A:
        case 'y' :
        case 'Y' :
          return var.name();
        default:
          continue;
        }
      } else {
        return var.name();
      }
    }
    ++index_found;
  }

  index_found = 0;
  index_type  = INDEX_FUNCTION;
  for (const auto& ktp : Editor::_keytab) {
    if (len == 0 || emstrncmp(ktp.name(), buf, len) == 0) {
      if (len != emstrlen(ktp.name())) {
        WDGupdate(prompt, const_cast<EMCHAR*>(ktp.name()));
        switch (term->get()) {
        case 0x07:
          WDGwrite(ECSTR("Quit"));
          return nullptr;
        case 0x0D:
        case 0x0A:
        case 'y' :
        case 'Y' :
          break;
        default:
          continue;
        }
      }
      return const_cast<EMCHAR*>(ktp.name());
    }
    ++index_found;
  }

  index_found = -1;
  WDGwrite(ECSTR("Not found."));
  return nullptr;
}

/*
 * Change the value of an Emacs variable
 */

CMD
Options::setvar() {
  EMCHAR buf[NPAT];
  auto status = T;

  complete = varmatch;
  index_type = INDEX_UNKNOWN;

  if (MiniBuf::reply(ECSTR(": eval-function "), buf, NPAT) != T) {
    return NIL;
  }

  varmatch(ECSTR(": match "), buf);  /* just to compute the INDEX */

  switch (index_type) {
  case INDEX_FUNCTION:
    Editor::_thisflag = CFUNSET;
    status  = Editor::_keytab[index_found]();  // execute command
    Editor::_lastflag = Editor::_thisflag;
    return status;
  case INDEX_VARIABLE:
    {
      auto& var(EditorVariable::vartab[index_found]);

      switch (var.type()) {
      case BOOLVAL :
      {
        auto p = var.boolp();
        *p = !*p;
        WDGwrite(ECSTR("%s set to %s"),
                 var.name(),
                 (*p ? ECSTR("T") : ECSTR("NIL")));
      }
      break;
      case FIXVAL: {
        {
          EMCHAR newval[NPAT];
          auto p = var.intp();
          (void)emsprintf(buf, ECSTR("%s (%d) > "), var.name(), *p);
          (void)MiniBuf::reply(buf, newval, 16);
          *p = emstrtoi(newval);
        }
        break;
      }
      default:
        (void)emstrcpy(buf, var.name());
        (void)emstrcat(buf, ECSTR(" \""));
        (void)emstrcat(buf, var.string());
        (void)emstrcat(buf, ECSTR("\" > "));
        (void)MiniBuf::reply(buf, var.string(), static_cast<int>(var.size()));
        break;
      }
    }
  }

  for (auto wp : EditWindow::list()) {
    redisplay->modeline(wp);
  }

  return T;
}

/*
 * Code that deal with TAGS. Bound to M-.
 */

CMD
Options::findtag() {
  return internalfindtag(0);
}

/*
 * Repeat find-tag for the next entry. Bound to M-,
 */

CMD
Options::tagsloopcont() {
  return internalfindtag(tagfound);
}

/*
 * Internal find tag. Find a tag name from a TAGS file.
 */

static CMD
internalfindtag(int tagnext) {
  static EMCHAR tagname[NPAT] = { '\000' };
  EMCHAR tagline[NLINE];
  EMCHAR tagdir[NFILEN];
  EMCHAR tagfile[NFILEN];
  FILE*  tagfd;
  int    taglen;
  CMD    s;

  (void)emstrcpy(tagdir, curbp->filename());
  (void)updir(tagdir, SLASH);
  (void)emstrcpy(tagfile, tagdir);
  (void)emstrcat(tagfile, ECSTR("tags"));

  if ((tagfd = ffopen(tagfile, ECSTR("r"))) == nullptr) {
    WDGerror(ECSTR("No tags file."));
    return NIL;
  }

  if (tagnext != 0) {
    if (tagname[0] == '\000') {
      term->beep();
      WDGerror(ECSTR("No M-x find-tag in progress."));
      return NIL;
    }
  } else {
    tagname[0] = '\000';

    if (!Word::atCursor(tagname, NPAT)) {
      tagname[0] = '\000';
    }

    if ((s = WDGedit(ECSTR("Find tag: "), tagname, NPAT)) != T) {
      if (s != NIL || tagname[0] == '\000') {
        (void)std::fclose(tagfd);
        return s;
      }
    }
  }

  taglen = emstrlen(tagname);

  for (int tagno = 0; ffgets(tagline, NLINE, tagfd) != nullptr; ++tagno) {
    if (tagno <= tagnext) {
      continue;
    }
    if (emstrncmp(tagline, tagname, (size_t)taglen) == 0) {
      auto str = tagline;

      (void)std::fclose(tagfd);

      Options::tagfound = tagno;

      /*
       * skip tag name
       */

      while (*str++ != ' ') {
        continue;
      }

      /*
       * skip space
       */

      while (*str++ == ' ') {
        continue;
      }

      /*
       * read the line number.
       */

      auto line = str-1;
      while (*str++ != ' ') {
        continue;
      }

      *(str-1) = '\000';

      /*
       * read the file name.
       */

      auto file = str;
      while (*str && (*str != '\n')) {
        str++;
      }
      *str = '\000';

      /*
       * load the filename and goto tag.
       */

      (void)emstrcpy(tagfile, tagdir);
      (void)emstrcat(tagfile, file);
#if     defined(_WIN32)
      (void)emstrlwr(tagfile);
#endif

      (void)newfile(tagfile);
      {
        auto save(Editor::_repeat);
        Editor::_repeat = emstrtoi(line);
        (void)Editor::gotoline();
        Editor::_repeat = save;
      }
      return T;
    }
  }

  (void)std::fclose(tagfd);

  if (tagnext == 0) {
    WDGwrite(ECSTR("No tags containing %s"), tagname);
  } else {
    WDGwrite(ECSTR("No more tags containing %s"), tagname);
  }

  return NIL;
}

/*
 * Complete  a  given  word  'tagname'  into  'tagcomp' from the
 * current  tag file.  The variable tagnext is the line where to
 * start the search.  It returns the line where a match is found
 * that  could be used for the next search or 0 when no match is
 * found. This internal routine is used by complete-word.
 */

int
Options::completeintag(int tagnext, const EMCHAR* tagname, EMCHAR* tagcomp) {
  EMCHAR  tagline[NLINE];
  EMCHAR  tagdir[NFILEN];
  EMCHAR  tagfile[NFILEN];


  (void)emstrcpy(tagdir, curbp->filename());
  (void)updir(tagdir, SLASH);
  (void)emstrcpy(tagfile, tagdir);
  (void)emstrcat(tagfile, ECSTR("tags"));

  auto tagfd = ffopen(tagfile, ECSTR("r"));

  if (tagfd == nullptr) {
    return 0;
  }

  auto mode(curbp->editMode());

  for (int tagno = 0; ffgets(tagline, NLINE, tagfd) != nullptr; ++tagno) {
    if (tagno <= tagnext) {
      continue;
    }

    if (Editor::isEqual(tagline, tagname, mode == EDITMODE::LISPMODE)) {
      (void)std::fclose(tagfd);

      Options::tagfound = tagno;

      /*
       * skip tag name
       */

      for (auto s = tagline; *s != ' '; ++s) {
        *tagcomp++ = *s;
      }

      *tagcomp = '\000';

      return tagnext = tagno;
    }
  }

  (void)std::fclose(tagfd);
  return 0;
}

/*
 * Uncompile  the  current macro definition in Lisp or OpenLisp
 * statements.  The  command  popup  the help buffer and display
 * the definition. Bound to C-XU
 */

static constexpr auto NMACLINE(128);        // # of bytes in macro line

static  EMCHAR  macline[NMACLINE];
static  bool    instringp = false;
static  int     count     = 1;

/*
 *      Print the macro 'name'.
 */

static void
printmacro(const EMCHAR* name, Buffer* bp) {
  if (instringp) {
    if (count > 1) {
      (void)emstrcat(macline, ECSTR("\"))"));
    } else {
      (void)emstrcat(macline, ECSTR("\")"));
    }

    Line::append(bp, macline);
  }

  if (name) {
    if (count > 1) {
      (void)emstrcat(macline, ECSTR(" ("));
    } else {
      (void)emstrcpy(macline, ECSTR("   ("));
    }

    (void)emstrcat(macline, name);

    if (count > 1) {
      (void)emstrcat(macline, ECSTR("))"));
    } else {
      (void)emstrcat(macline, ECSTR(")"));
    }

    Line::append(bp, macline);
  }

  instringp = false;
}

/*
 * Print the current command of the keyboard macro.
 */

static void
printcmd(int c, Buffer* bp) {
  EMCHAR ch[2];

  ch[0] = (EMCHAR)(c & MAX_EMCHAR);
  ch[1] = '\000';

  if (count > 1) {
    printmacro(nullptr, bp);
    (void)emsprintf(macline, ECSTR("   (repeat %d"), count);
  }

  for (const auto& macro : Editor::getMacros()) {
    /* Look in macro table. */
    if (macro.code() == c) {
      printmacro(macro.name(), bp);
      return;
    }
  }

  for (const auto& ktp : Editor::_keytab) {
    if (ktp.code() == c) {
      printmacro(ktp.name(), bp);
      return;
    }
  }

  if (self_insert(c)) {
    if (!instringp) {
      if (count > 1) {
        (void)emstrcat(macline, ECSTR(" (insert-string \""));
      } else {
        (void)emstrcpy(macline, ECSTR("   (insert-string \""));
      }
      instringp = true;
    }

    if (c == '\\' || c == '"') {
      (void)emstrcat(macline, ECSTR("\\"));
    }
    (void)emstrcat(macline, ch);
    if (count > 1) {
      printmacro(nullptr, bp);
    }
  }
}

/*
 * Uncompile  the  current  keyboard macro in Lisp code into the
 * help buffer.
 */

CMD
Options::uncompile() {
  int c;

  if (kbdm.exist()) {
    WDGmessage(ECSTR("No keyboard macro to uncompile."));
    return NIL;
  }

  if (kbdm.isRecording() || kbdm.isPlaying()) {
    WDGmessage(ECSTR("You can't uncompile macro while defining it."));
    return NIL;
  }

  /*
   * find or create buffer if it does not exist.
   */
  auto bp = Buffer::find(BUF_HELP);
  bp->setChanged(false);

  /* Blow old text away */
  if (!bp->clear()) {
    return NIL;
  }

  Line::append(bp, ECSTR("(defun current-macro ()"));

  kbdm.startPlaying();
  while ((c = kbdm.play()) != (CTLX|')')) {
    if (c == (Ctrl|'U')) {
      count = kbdm.play();
      c     = kbdm.play();
    } else {
      count = 1;
    }
    printcmd(c, bp);
  }
  kbdm.stopPlaying();
  printmacro(nullptr, bp);

  Line::append(bp, ECSTR(")"));

  (void)bp->show();

  return T;
}
