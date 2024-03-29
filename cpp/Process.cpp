#if !defined(lint)
static auto rcsid("$Id: spawn.cpp,v 1.24 2018/09/09 07:21:10 jullien Exp $");
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
 * The routines in this file are called to create a subjob running a
 * command interpreter.
 */

#define _CRT_SECURE_NO_WARNINGS

#include <thread>
#include "./emacs.h"
#include "./Editor.h"
#include "./Buffer.h"
#include "./EditWindow.h"
#include "./Error.h"
#include "./Line.h"
#include "./MLisp.h"
#include "./MiniBuf.h"
#include "./Process.h"
#include "./Redisplay.h"
#include "./Terminal.h"
#include "./Widget.h"
#include "./Word.h"

extern Widget* widget;       // Widgets tools

static const EMCHAR* SEDPROG{ECSTR("sed")};
static const EMCHAR* GREPPROC{ECSTR("grep -n ")};

#if defined(UNICODE)
static const EMCHAR* SHELLFMT1{ECSTR("%ls %ls %ls > %ls")};
static const EMCHAR* SHELLFMT2{ECSTR("%ls %ls \"%ls\" %ls > %ls")};
static const EMCHAR* SEDFMT1{ECSTR("%ls %ls %ls > %ls")};
static const EMCHAR* SEDFMT2{ECSTR("%ls -e \"%ls\" %ls > %ls")};
static const EMCHAR* PERLFMT1{ECSTR("%ls %ls %ls")};
#else
static const EMCHAR* SHELLFMT1{ECSTR("%s %s %s > %s")};
static const EMCHAR* SHELLFMT2{ECSTR("%s %s \"%s\" %s > %s")};
static const EMCHAR* SEDFMT1{ECSTR("%s %s %s > %s")};
static const EMCHAR* SEDFMT2{ECSTR("%s -e \"%s\" %s > %s")};
static const EMCHAR* PERLFMT1{ECSTR("%s %s %s")};
#endif

static CMD javaevalbuffer();

/*
 * Send  a  system  command.  Output is redirected to a filename
 * and then loaded in the 'Process' window.
 */

#if defined(_WIN32)
bool
Process::syscompile(const EMCHAR* cmd, int flag) {
  auto owp = curwp;
  Buffer* bp;

  /*
   * find or create buffer it it does not exist.
   */
  if ((bp = Buffer::find(BUF_PROC)) == nullptr) {
    return false;
  }

  /* Blow old text away */
  if (!bp->clear()) {
    return false;
  }

  auto wp = bp->show();

  wp->current();

  if (ffsystem(cmd) != 0) {
    term->beep();
    WDGwrite(ECSTR("'%s' command fails !"), cmd);
    owp->current();
    return false;
  }

  (void)Line::notmodified();
  (void)Editor::gotobob();
  owp->current();

  WDGwrite(ECSTR("Done."));
  Error::clear();

  if (flag == SYSCOMP_ERRORS) {
    (void)Error::next();
  }

  return true;
}

#else
bool
Process::syscompile(const EMCHAR* cmd, int flag) {
  auto    owp(curwp);
  auto    status(false);
  int     out(-1);
  Buffer* bp;
  int     err;
  int     tmp1;

  /*
   * find or create buffer it it does not exist.
   */
  if ((bp = Buffer::find(BUF_PROC)) == nullptr) {
    return false;
  }

  /* Blow old text away */
  if (!bp->clear()) {
    return false;
  }

  auto wp = bp->show();

  wp->current();

  if (opt::pipe_process) {
#if 1
    EMCHAR line[NLINE];

    (void)emstrcpy(line, cmd);
    (void)emstrcat(line, ECSTR(" 2>&1"));

    auto fd = empopen(&line[0], ECSTR("r"));

    if (fd == nullptr) {
      return false;
    }

    for (auto c = std::fgetc(fd); c != EOF; c = std::fgetc(fd)) {
      if (term->check()) {
        term->beep();
        WDGmessage(ECSTR("Quit"));
        break;
      }
      switch (c) {
      case '\b' :
        (void)Editor::backdel();
        break;
      case '\r' :
        redisplay->update();
        break;
      case '\n':
        (void)Editor::newline();
        redisplay->update();
        break;
      default:
        (void)Line::insert(c);
      }
    }

    term->rawmode();
    status = (pclose(fd) == 0);
#else
    EMCHAR line[NLINE];
    (void)emstrcpy(line, cmd);
    (void)emstrcat(line, ECSTR(" 2>&1"));

    auto proc = [&line](Window* old) {
      auto fd2 = empopen(&line[0], ECSTR("r"));
      if (fd2 == nullptr) {
        return false;
      }

      int c;

      while ((c = std::fgetc(fd2)) != EOF) {
        Buffer* bproc;

        /*
         * find or create buffer it it does not exist.
         */
        if ((bproc = Buffer::find(BUF_PROC)) == nullptr) {
          return false;
        }

        auto wproc = bproc->show();

        wproc->current();

#if 0
        if (term->check()) {
          term->beep();
          WDGmessage(ECSTR("Quit"));
          break;
        }
#endif
        switch (c) {
        case '\b' :
          (void)backdel();
          break;
        case '\r' :
          redisplay->update();
          break;
        case '\n':
          (void)Editor::newline();
          redisplay->update();
          break;
        default:
          (void)Line::insert(c);
        }
        old->current();
      }
      term->rawmode();
      return true;
    };

    printf("Start..\n");
    std::thread th{proc, owp};
    printf("Joining..\n");
    th.detach();
    printf("Joined!\n");

    status = true;
#endif
  } else {
    if (opt::compile_in_buffer) {
      static const auto procname(ECSTR("process.tmp"));
      auto fd = ffopen(procname, ECSTR("w"));

      if (fd == nullptr) {
        return false;
      }

      tmp1 = fileno(fd);

      if ((err  = dup(fileno(stderr))) != -1 &&
          (out  = dup(fileno(stdout))) != -1 &&
          dup2(tmp1, fileno(stderr))   != -1 &&
          dup2(tmp1, fileno(stdout))   != -1) {
        term->rawmode(); /* open the duplicate out ^C */
        status = (ffsystem(cmd) == 0);
        (void)dup2(out, fileno(stdout));
        (void)dup2(err, fileno(stderr));
        term->rawmode(); /* close the duplicate out */
        redisplay->update(Redisplay::Mode::REFRESH);
      }

      if (tmp1 != -1) {
        (void)close(tmp1);
      }
      if (out != -1) {
        (void)close(out);
      }
      if (out != -1) {
        (void)close(err);
      }

      if (!bp->clear()) {
        return false;
      } else {
        (void)bp->show();
      }

      (void)readin(procname);
      (void)std::remove((char *)procname);
    } else {
      redisplay->tidy();
      status = (ffsystem(cmd) == 0);
      term = Terminal::getInstance();
      WDGwrite(ECSTR("Strike any key to continue .. "));
      term->get();
      (void)Editor::redrawscreen();
      return status;
    }
  }

  (void)Line::notmodified();
  (void)Editor::gotobob();
  owp->current();

  WDGwrite(ECSTR("Done."));
  Error::clear();

  if (flag == SYSCOMP_ERRORS) {
    (void)Error::next();
  }

  return status;
}
#endif

/*
 * Create  a  subjob  with  a copy of the command intrepreter in
 * it.  Mark  the  screen as garbage for full repaint.  Bound to
 * "C-C".
 */

CMD
Process::spawncli() {
#if defined(_WIN32)
  (void)ffsystem(ffgetenv(ECSTR("ComSpec")));
#else
  redisplay->tidy();

  if (emstrcmp(ffgetenv(ECSTR("TERM")), ECSTR("xterm")) == 0) {
    (void)ffsystem(ECSTR("xterm"));
  } else {
    auto cp = ffgetenv(ECSTR("SHELL"));
    (void)ffsystem((cp != nullptr) ? cp : ECSTR("sh"));
  }

  (void)Editor::redrawscreen();
  term = Terminal::getInstance();
#endif
  return T;
}

/*
 * Run a one-liner in a subjob.  When the command returns,  wait
 * for  a single character to be typed,  then mark the screen as
 * garbage so a full repaint is done. Bound to "C-X !".
 */

CMD
Process::spawn() {
  CMD s;
  EMCHAR  line[NLINE];

  if ((s = MiniBuf::reply(ECSTR("System command: "), line, NLINE)) != T) {
    return s;
  }

#if defined(_WIN32)
  (void)ffsystem(line);
#else
  redisplay->tidy();

  (void)ffsystem(line);

  (void)std::fputs("Strike any key to continue .. ", stdout);
  (void)std::fgetc(stdin);

  term = Terminal::getInstance();
  MiniBuf::erase();
  (void)Editor::redrawscreen();
#endif
  return T;
}

/*
 * Execute current makefile.
 */

CMD
Process::makefile() {
  EMCHAR  buf[NFILEN];
  CMD s;

  (void)emstrcpy(buf, opt::make_name);

  if (*opt::make_arg) {
    (void)emstrcat(buf, ECSTR(" "));
    (void)emstrcat(buf, opt::make_arg);
  }

  if ((s = Editor::saveSomeBuffers()) == ABORT) {
    return s;
  }

  WDGwrite(ECSTR("%s .."), buf);
  return syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
}

/*
 * Execute man with word at cursor
 */

CMD
Process::man() {
#if defined(_POSIX_C_SOURCE) || defined(_WIN32)
  EMCHAR  buf[NPAT];
  EMCHAR  cmd[NLINE];

  if (!Word::atCursor(buf, NPAT)) {
    buf[0] = '\000';
  }

  if (WDGedit(ECSTR("man : "), buf, NLINE) == ABORT) {
    return NIL;
  }

  (void)emstrcpy(cmd, ECSTR("man "));
  (void)emstrcat(cmd, buf);

  return syscompile(cmd, SYSCOMP_NOERROR) ? T : NIL;
#else
  WDGwrite(ECSTR("No man on this system"));
  return NIL;
#endif
}

/*
 * Use  external  command  from  emacs.  First,  change  to  the
 * directory  of  file  in  the  current  buffer,  then  run the
 * command  in  this  directory  and  go  back  to  the original
 * location (the current directory).
 */

static CMD
externalcommand(EMCHAR* cmdname, const EMCHAR* cmdline) {
  EMCHAR  buf[NLINE];
  EMCHAR  prompt[NLINE];
  EMCHAR  info[NLINE];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  CMD s;

  (void)ffgetcwd(cdir, NLINE-1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  (void)emstrcpy(info, ECSTR("Run "));
  (void)emstrcat(info, cmdname);
  (void)emstrcat(info, ECSTR(" (with arg): "));

  (void)emstrcpy(buf, cmdline);

  prompt[0] = '\000';

  if (WDGedit(info, prompt, NLINE) == ABORT) {
    (void)ffchdir(cdir);
    return NIL;
  }

  (void)emstrcat(buf, prompt);

  WDGwrite(ECSTR("%s .."), buf);
  s = Process::syscompile(buf, SYSCOMP_NOERROR) ? T : NIL;
  (void)ffchdir(cdir);

  return s;
}

/*
 * Use grep from emacs. This command is not bound.
 */

CMD
Process::grep() {
  return externalcommand(ECSTR("grep"), GREPPROC);
}

/*
 * Use shell command processor from emacs.  First, change to the
 * directory  of file in the current buffer,  then run the shell
 * in  this  directory  with  the  user  command and replace the
 * current buffer by the result of execution. Then we go back to
 * the  original location (the current directory).  This command
 * is not bound.
 */

#define SHELLTEMP       ECSTR("em-shell.tmp")
#define SHELLRESULT     ECSTR("em-shell.res")

CMD
shellbuffer(EMCHAR* prog, EMCHAR* def) {
  EMCHAR  buf[1024];
  EMCHAR  info[NLINE];
  EMCHAR  prompt[NLINE];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  EMCHAR  oldfname[NFILEN];
  int     option = 0;
  CMD     s;

  (void)ffgetcwd(cdir, NLINE - 1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  (void)emstrcpy(&oldfname[0], curbp->filename());

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }

  writeout(SHELLTEMP);

  (void)emstrcpy(info, ECSTR("Run "));
  (void)emstrcat(info, prog);
  (void)emstrcat(info, ECSTR(" (with arg): "));

  prompt[0] = '\000';

  if (WDGedit(info, prompt, NLINE) == ABORT) {
    (void)ffchdir(cdir);
    return NIL;
  }

  for (auto i(0); prompt[i]; ++i) {
    if (prompt[i] == ' ' || prompt[i] == '\t') {
      continue;
    }
    if (prompt[i] == '-') {
      option = 1;
    } else {
      option = 0;
    }
    break;
  }

  if (option) {
    (void)emsprintf(buf, SHELLFMT1, prog, prompt, SHELLTEMP, SHELLRESULT);
  } else {
    (void)emsprintf(buf, SHELLFMT2, prog, def, prompt, SHELLTEMP, SHELLRESULT);
  }

  if ((s = ((ffsystem(buf) == 0) ? T : NIL)) == T) {
    (void)readin(SHELLRESULT);
    (void)emstrcpy(curbp->filename(), &oldfname[0]);
    Buffer::change(EditWindow::WFEDIT);
  }

  term->rawmode();

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }
  if (ffaccess(SHELLRESULT) == FIOSUC) {
    (void)ffremove(SHELLRESULT);
  }

  (void)ffchdir(cdir);

  return s;
}

/*
 * Use  sed from emacs.  First,  change to the directory of file
 * in  the  current buffer,  then run sed in this directory with
 * the  user  command  and  replace  the  current  buffer by the
 * result  of  sed  execution.  The  we  go back to the original
 * location (the current directory). This command is not bound.
 */

CMD
Process::sed() {
  EMCHAR  buf[1024];
  EMCHAR  prompt[NLINE];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  EMCHAR  oldfname[NFILEN];
  int     option = 0;
  CMD     s;

  if (curbp->encoding() != ENCODING::EMASCII) {
    WDGwrite(ECSTR("Can't use sed on an UNICODE buffer."));
    return NIL;
  }

  (void)ffgetcwd(cdir, NLINE - 1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  (void)emstrcpy(&oldfname[0], curbp->filename());

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }

  writeout(SHELLTEMP);

  prompt[0] = '\000';

  if (WDGedit(ECSTR("Run sed (with arg): "), prompt, NLINE) == ABORT) {
    (void)ffchdir(cdir);
    return NIL;
  }

  for (auto i = 0; prompt[i]; ++i) {
    if (prompt[i] == ' ' || prompt[i] == '\t') {
      continue;
    }
    if (prompt[i] == '-') {
      option = 1;
    } else {
      option = 0;
    }
    break;
  }

  if (option) {
    (void)emsprintf(buf, SEDFMT1,
                     SEDPROG, prompt, SHELLTEMP, SHELLRESULT);
  } else {
    (void)emsprintf(buf, SEDFMT2,
                        SEDPROG, prompt, SHELLTEMP, SHELLRESULT);
  }

  if ((s = ((ffsystem(buf) == 0) ? T : NIL)) == T) {
    (void)readin(SHELLRESULT);
    (void)emstrcpy(curbp->filename(), &oldfname[0]);
    Buffer::change(EditWindow::WFEDIT);
  }

  term->rawmode();

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }

  if (ffaccess(SHELLRESULT) == FIOSUC) {
    (void)ffremove(SHELLRESULT);
  }

  (void)ffchdir(cdir);

  return s;
}

/*
 * Use perl from emacs.  First,  change to the directory of file
 * in  the current buffer,  then run perl in this directory with
 * the  user  command  and  replace  the  current  buffer by the
 * result  of  perl  execution.  The  we go back to the original
 * location (the current directory). This command is not bound.
 */

#define PERLPROG        ECSTR("perl")

CMD
Process::perl() {
  EMCHAR  buf[1024];
  EMCHAR  prompt[NLINE];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  EMCHAR  oldfname[NFILEN];
  CMD     s;

  (void)ffgetcwd(cdir, NLINE-1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  (void)emstrcpy(&oldfname[0], curbp->filename());

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }

  writeout(SHELLTEMP);

  prompt[0] = '\000';

  if (WDGedit(ECSTR("Run perl (with arg): "), prompt, NLINE) == ABORT) {
    (void)ffchdir(cdir);
    return NIL;
  }

  (void)emsprintf(buf, PERLFMT1, PERLPROG, prompt, SHELLTEMP);

  if ((s = ((ffsystem(buf) == 0) ? T : NIL)) == T) {
    (void)readin(SHELLTEMP);
    (void)emstrcpy(curbp->filename(), &oldfname[0]);
    Buffer::change(EditWindow::WFEDIT);
  }

  term->rawmode();

  if (ffaccess(SHELLTEMP) == FIOSUC) {
    (void)ffremove(SHELLTEMP);
  }

  if (ffaccess(SHELLRESULT) == FIOSUC) {
    (void)ffremove(SHELLRESULT);
  }

  (void)ffchdir(cdir);

  return s;
}

/*
 * Compile, interactively call make_name
 */

CMD
Process::compile() {
  static EMCHAR buf[NFILEN] = { '\000' };

  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  CMD     s;

  (void)ffgetcwd(cdir, NLINE-1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  if (buf[0] == '\000') {
    (void)emstrcpy(buf, opt::make_name);
    if (*opt::make_arg) {
      (void)emstrcat(buf, ECSTR(" "));
      (void)emstrcat(buf, opt::make_arg);
      (void)emstrcat(buf, ECSTR(" "));
    }
  }

  {
    auto prev(MiniBuf::allowComplete(true));
    s = WDGedit(ECSTR("Compile command: "), buf, NFILEN);
    (void)MiniBuf::allowComplete(prev);
  }

  if (s == ABORT) {
    return NIL;
  }

  if ((s = Editor::saveSomeBuffers()) == ABORT) {
    return s;
  }

  s = Process::syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
  (void)ffchdir(cdir);

  return s;
}

/*
 * Compile  current filename using the associated file compiler.
 * Bound to C-XC.
 */

CMD
Process::compilecurrent() {
  switch (curbp->editMode()) {
  case EDITMODE::CMODE:
  case EDITMODE::CPPMODE:
    return ccompile();
  case EDITMODE::JAVAMODE:
    return Process::javacompile();
  default:
    term->beep();
    WDGwrite(ECSTR("Don't know how to compile %s"), curbp->filename());
  }

  return NIL;
}

/*
 * Compile current filename if it's a C file.
 */

CMD
Process::ccompile() {
  EMCHAR  buf[NFILEN];

  (void)emstrcpy(buf, opt::cc_name);
  if ((curbp->editMode() == EDITMODE::CMODE)
      || (curbp->editMode() == EDITMODE::CPPMODE)) {
    (void)emstrcat(buf, ECSTR(" "));
    (void)emstrcat(buf, opt::cc_arg);
    (void)emstrcat(buf, ECSTR(" "));
    (void)emstrcat(buf, curbp->filename());
    WDGwrite(ECSTR("%s"), buf);
    return Process::syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
  } else {
    return NIL;
  }
}

/*
 * Compile current filename if it's a Java file.
 */

CMD
Process::javacompile() {
  EMCHAR  buf[NFILEN];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  EMCHAR* base;
  CMD     s;

  if (curbp->editMode() != EDITMODE::JAVAMODE) {
    term->beep();
    WDGwrite(ECSTR("not a java file !"));
    return NIL;
  }

  (void)ffgetcwd(cdir, NLINE-1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  base = curbp->filename();

  for (auto fname = base; *fname != 0; ++fname) {
    if (*fname == '/' || *fname == '\\') {
      base = fname + 1;
    }
  }

  (void)emstrcpy(buf, opt::java_comp_name);
  (void)emstrcat(buf, ECSTR(" "));

  if (opt::java_comp_args[0]) {
    (void)emstrcat(buf, opt::java_comp_args);
    (void)emstrcat(buf, ECSTR(" "));
  }

  (void)emstrcat(buf, base);

  if ((s = Editor::saveSomeBuffers()) == ABORT) {
    return s;
  }

  WDGwrite(ECSTR("%s"), buf);

  s = syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
  (void)ffchdir(cdir);

  return s;
}

/*
 * Assemble current filename if it's a assembler file.
 */

CMD
Process::assemble() {
  EMCHAR  buf[NFILEN];

  if (curbp->editMode() == EDITMODE::ASMODE) {
    (void)emstrcpy(buf, opt::as_name);
    (void)emstrcat(buf, ECSTR(" "));
    (void)emstrcat(buf, opt::as_arg);
    (void)emstrcat(buf, ECSTR(" "));
    (void)emstrcat(buf, curbp->filename());
    (void)emstrcat(buf, ECSTR(";"));
    WDGwrite(ECSTR("%s"), buf);
    return syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
  }
  return NIL;
}

/*
 * Evaluate current filename if it's a Java file.
 */

static CMD
javaevalbuffer() {
  EMCHAR  buf[NFILEN];
  EMCHAR  cdir[NLINE];
  EMCHAR  gdir[NLINE];
  EMCHAR* p;
  EMCHAR* d = nullptr;
  CMD     s;

  if (curbp->editMode() != EDITMODE::JAVAMODE) {
    term->beep();
    WDGwrite(ECSTR("not a java file !"));
    return NIL;
  }

  (void)ffgetcwd(cdir, NLINE-1);
  (void)emstrcpy(gdir, curbp->filename());
  (void)updir(gdir, NOSLASH);

  if (gdir[0]) {
    (void)ffchdir(gdir);
  }

  p = curbp->filename();

  for (auto fname = p; *fname; ++fname) {
    if (*fname == '/' || *fname == '\\') {
      p = fname + 1;
    }
  }

  (void)emstrcpy(buf, opt::java_exec_name);
  (void)emstrcat(buf, ECSTR(" "));
  if (opt::java_exec_args[0]) {
    (void)emstrcat(buf, opt::java_exec_args);
    (void)emstrcat(buf, ECSTR(" "));
  }
  (void)emstrcat(buf, p);

  /*
   * Remove extension.
   */

  for (p = buf; *p != 0; ++p) {
    if (*p == '.') {
      d = p;
    }
  }

  if (d != buf) {
    *d = '\000';
  }

  /*
   * Compile the file first, then execute
   */

  s = Process::javacompile();

  if (s == T) {
    WDGwrite(ECSTR("%s"), buf);

    s = Process::syscompile(buf, SYSCOMP_ERRORS) ? T : NIL;
    (void)ffchdir(cdir);
  }

  return s;
}

/*
 * Evaluate the content of current buffer depending on mode.
 */

CMD
Process::evalbuf() {
  switch (curbp->editMode()) {
  case EDITMODE::LISPMODE:
    return MLisp::evalBuffer();
  case EDITMODE::JAVAMODE:
    return javaevalbuffer();
  default:
    term->beep();
    WDGwrite(ECSTR("Cannot evaluate this buffer."));
  }

  return NIL;
}

/*
 * Get system command in process buffer.
 */

CMD
Process::getcommand() {
  CMD     s;
  EMCHAR  line[NLINE];

  if ((s = MiniBuf::reply(ECSTR(": get-system-command: "), line, NLINE)) != T) {
    return s;
  } else {
    return syscompile(line, SYSCOMP_NOERROR) ? T : NIL;
  }
}

/*
 * Change  the  working  directory  (usefull  on  UN*X operating
 * system)
 */

CMD
Process::changedir() {
  EMCHAR line[NLINE];
  auto s(MiniBuf::reply(ECSTR("Change default directory: "), line, NLINE));

  if (s != T) {
    return s;
  } else if (ffchdir(line) == 0) {
    return T;
  } else {
    WDGerror(ECSTR("Directory not found or access denied."));
    return NIL;
  }
}
