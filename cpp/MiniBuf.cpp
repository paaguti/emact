#if !defined(lint)
static auto rcsid("$Id: minibuf.cpp,v 1.20 2018/09/09 07:21:10 jullien Exp $");
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
 * The   functions  in  this  file  handle  the  minibuffer  the
 * one-line display at the bottom of the screen.
 */

#include "./emacs.h"
#include "./Line.h"
#include "./Completion.h"
#include "./MiniBuf.h"
#include "./Redisplay.h"
#include "./Search.h"

static  void    mlputs(const EMCHAR* s, int size);
static  void    mlputi(int i, int r);
static  void    mlclearentry(EMCHAR* buf, int cpos);

bool mpresf{false};  // true if message in last line.

Completion complete;

static bool allowcomplete{false};
static int mbcursor{0};

/*
 * Erase the user response up to the prompt.
 */

static void
mlclearentry(EMCHAR* buf, int cpos) {
  while (cpos > 0 && mbcursor > 0) {
    redisplay->statputc(--mbcursor, ' ');
    if (buf[--cpos] < (EMCHAR)0x20) {
      redisplay->statputc(--mbcursor, ' ');
    }
  }
}

int
MiniBuf::cursor() {
  return mbcursor;
}

void
MiniBuf::erase() {
  mbcursor = 0;

  while (mbcursor < term->getNbCols()) {
    redisplay->statputc(mbcursor++, ' ');
  }

#if defined(_WIN32)
  WDGmessage(ECSTR(" "));       /* clear the dialog (Widgets version) */
#endif

  mpresf = false;
}

/*
 * Ask  a y or n question in the message line.  Return either T,
 * NIL,  or  ABORT.  The  ABORT  status  is returned if the user
 * bumps out of the question with a ^G.
 */

CMD
MiniBuf::yn(const EMCHAR* prompt) {
  MiniBuf::write(ECSTR("%s"), prompt);

  for (;;) {
    switch (term->get()) {
    case 0x07:
      (void)Editor::ctrlg();
      WDGmessage(ECSTR("Quit"));
      return ABORT;
    case 'y' :
    case 'Y' :
      return T;
    case 'n' :
    case 'N' :
      return NIL;
    case 'q' :
    case 'Q' :
    case 0x1B:
      return ABORT;
    default  :
      (void)Editor::ctrlg();
    }
  }
}

/*
 * Ask  a yes or no question in the message line.  Return either
 * T,  NIL,  or ABORT.  The ABORT status is returned if the user
 * bumps out of the question with a ^G.
 */

CMD
MiniBuf::yesno(const EMCHAR* prompt) {
  EMCHAR  buf[NPAT];

  for (;;) {
    if (MiniBuf::reply(prompt, buf, NPAT) != T) {
      return ABORT;
    }
    if (emstrcmp(buf, ECSTR("Yes")) == 0 ||
        emstrcmp(buf, ECSTR("yes")) == 0 ||
        emstrcmp(buf, ECSTR("YES")) == 0) {
      return T;
    }
    if (emstrcmp(buf, ECSTR("No")) == 0 ||
        emstrcmp(buf, ECSTR("no")) == 0 ||
        emstrcmp(buf, ECSTR("NO")) == 0) {
      return NIL;
    }
    (void)Editor::ctrlg();
    MiniBuf::write(ECSTR("Please answer yes or no"));
    Search::wait(3);
  }
}

CMD
MiniBuf::confirm(const EMCHAR* prompt) {
  return MiniBuf::yesno(prompt);
}

/*
 * Routine to allow or not edit strokes to complete a defaut value.
 */

bool
MiniBuf::allowComplete(bool flag) {
  auto prev(allowcomplete);
  allowcomplete = flag;
  return prev;
}

/*
 * Write  a  prompt  into  the  message  line,  then read back a
 * response.  Keep track of the physical position of the cursor.
 * If  we  are  in  a keyboard macro throw the prompt away,  and
 * return the remembered response.  This lets macros run at full
 * speed. Handle erase, kill, quote and abort keys.
 */

CMD
MiniBuf::edit(const EMCHAR* prompt, EMCHAR* buf, int nbuf) {
  int     i;
  int     c;
  int     cpos;
  auto    editflg = NIL;

 loop:
  cpos = 0;
  complete.setStatus(Completion::Status::COMPLETE_ONE);

  if (kbdm.isPlaying()) {
    while ((c = kbdm.play()) != '\000') {
      buf[cpos++] = (EMCHAR)c;
    }
    buf[cpos] = '\000';
    complete = nullptr;
    return (buf[0] == 0) ? NIL : T;
  } else {
    cpos = emstrlen(buf);
  }

  MiniBuf::write(ECSTR("%s%s"), prompt, buf);

  for (;;) {
    redisplay->update(Redisplay::Mode::MINIBUF);

    term->cshow(true);
    c = term->get();
    term->cshow(false);

    switch (c) {
    case 0x03:      /* Ctrl-C */
      while (cpos > 0) {
        redisplay->statputc(--mbcursor, ' ');
        if ((unsigned int)buf[--cpos] < 0x20) {
          redisplay->statputc(--mbcursor, ' ');
        }
      }
      cpos = 0;
      break;
    case 0x07:      /* Abort                */
      complete = nullptr;
      complete.setStatus(Completion::Status::COMPLETE_ABORT);
      WDGmessage(ECSTR("Quit"));
      return Editor::ctrlg();
    case 0x0D:      /* Return               */
    case 0x0A:      /* LineFeed             */
    case 0x12:      /* C-R, Back Search     */
    case 0x13:      /* C-S, Search          */
      editflg = T;
      if ((c != 0x0D) && (c != 0x0A)) {
        if (!(Editor::_thisflag & (CFFSRC | CFBSRC))) {
          continue;
        }
      }

      complete = nullptr;
      buf[cpos++] = 0;

      if (kbdm.isRecording()) {
        try {
          for (i = 0; i < cpos; ++i) {
            kbdm.record(buf[i]);
          }
        } catch (const Kbdm::BufferFullException&) {
          (void)Editor::ctrlg();
        }
      }
      return (buf[0] == 0) ? NIL : editflg;
    case 0x7F:      /* Rubout, erase        */
    case 0x08:      /* Backspace, erase     */
    case Ctrl|'H':
      if (cpos != 0) {
        redisplay->statputc(--mbcursor, ' ');
        if ((unsigned int)buf[--cpos] < 0x20) {
          redisplay->statputc(--mbcursor, ' ');
        }
      }
      break;
    case METACH:
      if (complete == Completion::fileMatch) {
        buf[cpos] = '\000';
        (void)updir(buf, SLASH);
        cpos = emstrlen(buf);
        if ((cpos == 0) || ((cpos == 2) && (buf[1] == ':'))) {
          buf[cpos++] = '/';
          buf[cpos] = '\000';
        }
        MiniBuf::write(ECSTR("%s%s"), prompt, buf);
      }
      break;
    case 0x0B:      /* kill ^K              */
      mlclearentry(buf, cpos);
      cpos = 0;
      break;
    case 0x19:      /* yank ^Y              */
      mlclearentry(buf, cpos);
      cpos = 0;

      i = 0;
      while (cpos < nbuf - 1) {
        if ((c = KillBuf::remove(i++)) < 0) {
          goto doneyank;
        }

        buf[cpos++] = (EMCHAR)c;
        if (c < ' ') {
          redisplay->statputc(mbcursor++, '^');
          c ^= 0x40;
        }
        redisplay->statputc(mbcursor++, c);
      }

    doneyank:
      break;
    default:
      if (c == 0x11) { /* Quote next char */
        c = term->get();
      }

      if ((complete == Completion::fileMatch) && (cpos > 0) && (c == ':')) {
        /*
         * Check for device change
         */

        c = buf[cpos - 1];
        mlclearentry(buf, cpos);
        cpos = 0;
        buf[cpos++] = (EMCHAR)c;
        buf[cpos++] = ':';
        buf[cpos]   = '\000';
        MiniBuf::write(ECSTR("%s%s"), prompt, buf);
        editflg = T;
        continue;
      }

      if (allowcomplete) {
        editflg = T;
      }

      if (editflg == NIL) {
        if (complete == Completion::fileMatch && c != ' ' && c != '\t') {
          if (cpos > 0 && buf[cpos - 1] == '/') {
            if (c == '/' || c == '\\') {
              mlclearentry(buf, cpos);
              cpos = 0;
            }
            editflg = T;
          } else {
            buf[cpos] = '\000';
            (void)updir(buf, SLASH);
            cpos          = emstrlen(buf);
            buf[cpos++] = (EMCHAR)c;
            buf[cpos]   = '\000';
            MiniBuf::write(ECSTR("%s%s"), prompt, buf);
            editflg = T;
            continue;
          }
        } else if (complete != Completion::fileMatch) {
          mlclearentry(buf, cpos);
          cpos = 0;
        }
      }

      if ((c &= MAX_EMCHAR) == 0) {
        break;
      }

      if (((c == ' ') || (c == '\t')) && (complete != nullptr)) {
        buf[cpos] = '\000';
        complete.setStatus(Completion::Status::COMPLETE_ONE);
        auto s(complete(prompt, buf));
        if (s != nullptr) {
          (void)emstrcpy(buf, s);
          if (complete.status() == Completion::Status::COMPLETE_AGAIN) {
            goto loop;
          }

          complete = nullptr;
          return T;
        } else {
          complete = nullptr;
          if (complete.status() == Completion::Status::COMPLETE_ABORT) {
            return ABORT;
          }
          complete.setStatus(Completion::Status::COMPLETE_FAIL);

          return NIL;
        }
      }
      if (cpos < (nbuf - 1)) {
        buf[cpos++] = (EMCHAR)c;
        if (c < ' ') {
          redisplay->statputc(mbcursor++, '^');
          c ^= 0x40;
        }
        redisplay->statputc(mbcursor++, c);
      }
    }
    editflg = T;
  }
}

/*
 * Same as MiniBuf::edit, except that the previous buffer is empty.
 */

CMD
MiniBuf::reply(const EMCHAR* prompt, EMCHAR* buf, int nbuf) {
  buf[0] = '\000';

  return WDGedit(prompt, buf, nbuf);
}

/*
 * Sounds the beeper, then display an error message on status line
 */

void
MiniBuf::error(const EMCHAR* msg) {
  term->beep();
  MiniBuf::write(ECSTR("%s"), msg);
}

/*
 * Write  a  message  in  a  printf like format into the message
 * line. Keep track of the physical cursor position.
 */

/*VARARGS1*/

void
MiniBuf::write(const EMCHAR* fmt, ...) {
  EMCHAR  c;
  EMCHAR  *ap;
  va_list var;

  va_start(var, fmt);

  mbcursor = 0;

  while ((c = *fmt++) != 0) {
    if (c != '%') {
      redisplay->statputc(mbcursor++, c);
    } else {
      switch (c = *fmt++) {
      case 'd':
        mlputi(va_arg(var, int), 10);
        break;
      case 'l':
        mlputi((int)va_arg(var, long), 10);  // NOLINT(runtime/int)
        if (*fmt == 'd') {
          fmt++;
        }
        break;
      case 'x':
        mlputi(va_arg(var, int), 16);
        break;
      case 'L':
        {
          auto lp = (Line *)va_arg(var, Line *);
          if (lp->length() < (term->getNbCols() - 1)) {
            mlputs(lp->text(), lp->length());
          } else {
            mlputs(lp->text(), term->getNbCols() - 1);
          }
        }
        break;
      case 's':
        ap = va_arg(var, EMCHAR*);
        mlputs(ap, emstrlen(ap));
        break;
      default:
        redisplay->statputc(mbcursor++, c);
      }
    }
  }

  for (auto i(mbcursor); i < term->getNbCols(); ++i) {
    redisplay->statputc(i, ' ');
  }

  redisplay->update(Redisplay::Mode::MINIBUF);
  mpresf = true;

  va_end(var);
}

/*
 * Write out a string. Update the physical cursor position.
 */

static void
mlputs(const EMCHAR* s, int size) {
  while (size--) {
    if ((unsigned int)*s < (unsigned int)' ') {
      redisplay->statputc(mbcursor++, '^');
      redisplay->statputc(mbcursor++, '@' + *s++);
    } else {
      redisplay->statputc(mbcursor++, *s++);
    }
  }
}

/*
 * Write  out an integer,  in the specified  radix.  Update  the
 * physical cursor position.
 */

static void
mlputi(int i, int r) {
  int q;
  static EMCHAR hexdigits[] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  };

  if (i < 0) {
    i = -i;
    redisplay->statputc(mbcursor++, '-');
  }

  if ((q = i / r) != 0) {
    mlputi(q, r);
  }

  redisplay->statputc(mbcursor++, hexdigits[i % r]);
}

/*
 * Set the title of application
 */

EMCHAR*
MiniBuf::title(EMCHAR* s, EMCHAR* f) {
  /*
   * next lines is dummy code to remove warning on args not used.
   */

  if (s != f) {
    f = s;
  }

  return f;
}

/*
 * Change selection. If opat and npat exist, propose the same selection again.
 */

CMD
MiniBuf::change(const EMCHAR* msg, EMCHAR* opat, EMCHAR* npat, int len) {
  EMCHAR buf[NLINE + 1];

  if (*opat && *npat) {
    bool changed{false};
    /*
     * Try with previous choice
     */
    for (;;) {
      EMCHAR prev[NLINE + 1];
      (void)emstrncpy(prev, npat, NLINE);
      (void)emstrncpy(buf, msg, NLINE);
      (void)emstrncat(buf, ECSTR(" "), NLINE);
      (void)emstrncat(buf, opat, NLINE);
      (void)emstrncat(buf, ECSTR(" with: "), NLINE);
      auto s = MiniBuf::edit(buf, npat, len);
      switch (s) {
      case T:
        if (changed || emstrcmp(npat, prev) == 0) {
          return s;
        }
        (void)emstrncpy(opat, npat, NLINE);
        changed = true;
        break;
      case NIL:
      case ABORT:
        return s;
      }
    }
  } else {
    (void)emstrncpy(buf, msg, NLINE);
    (void)emstrncat(buf, ECSTR(": "), NLINE);

    if (MiniBuf::edit(buf, opat, len) == T) {
      /*
       * First time.
       */
      (void)emstrncpy(buf, msg, NLINE);
      (void)emstrncat(buf, ECSTR(" "), NLINE);
      (void)emstrncat(buf, opat, NLINE);
      (void)emstrncat(buf, ECSTR(" with: "), NLINE);
      return (MiniBuf::edit(buf, npat, len) != ABORT) ? T : NIL;
    } else {
      return NIL;
    }
  }
}

void
MiniBuf::play(int flag) {
  /*
   * The next two lines is dummy code to remove warning on args not
   * used.
   */

  if (flag) {
    MiniBuf::wait();
  }
}

void
MiniBuf::wait() {
}

void
MiniBuf::message(const EMCHAR* msg) {
  MiniBuf::write(ECSTR("%s"), msg);
}

void
MiniBuf::adjust() {
}

void
MiniBuf::clipCopy() {
}

void
MiniBuf::clipPaste() {
}

void
MiniBuf::update(const EMCHAR* prompt, EMCHAR* line) {
  MiniBuf::write(ECSTR("%s%s"), prompt, line);
}

void
MiniBuf::lpPrint() {
  MiniBuf::write(ECSTR("Can't print on this system"));
}
