#if !defined(lint)
static auto rcsid("$Id: nexterr.cpp,v 1.13 2018/09/02 14:37:58 jullien Exp $");
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
 * nexterr.c : Find the next error after a compilation.
 */

#include "./emacs.h"
#include "./Editor.h"
#include "./Buffer.h"
#include "./EditWindow.h"
#include "./Error.h"
#include "./Line.h"
#include "./Search.h"
#include "./Widget.h"

extern Widget* widget;       // Widgets tools

bool   Error::_flag{false};  // Error flag
int    Error::_linenum{0};
EMCHAR Error::_fname[NFILEN];

bool
Error::get() {
  int    j;
  int    c;
  int    start;
  int    stop;
  EMCHAR save[NPAT];

  (void)emstrcpy(save, Editor::searchBuffer());
  Editor::setSearchBuffer(ECSTR(":"));

  Line* line{nullptr};
  for (;;) {
    _linenum = 0;

    if (!Search::next() || Editor::_found.line() == line) {
      if (_flag) {
        WDGmessage(ECSTR("no more errors"));
      } else {
        WDGmessage(ECSTR("no errors"));
      }
      Editor::setSearchBuffer(save);
      return false;
    }

    line = Editor::_found.line();

    curwp->setDot(line, 0);

    /*
     * search for last 'xx.yy' (assumes a filename with extension)
     */

    const auto eol(line->length());
    auto i = -1; /* '.' position in line */

    for (j = 1; j < (eol - 1); ++j) {
      c = line->get(j);
      if (c != '.') {
        continue;
      } else if (i != -1 && (Editor::separatorp(c) || c == ':')) {
        /*
         * stop at the first separator when at least one '.'
         * is line.
         */
        break;
      } else if (Editor::charp(line->get(j - 1))
                 && Editor::charp(line->get(j + 1))) {
        /*
         * remember  the  postion  of  '.'  and try to
         * find another occurence.  Complete file path
         * may contain more than one '.' as in:
         * /foo/bar-2.4.15/gee.c
         */
        i = j;
      }
    }

    if (i == -1) {
      continue;  // found end of line
    }

    /*
     * find the start of the word.
     */

    for (start = i; start >= 0; --start) {
      c = line->get(start);

      if (c == '/' || c == '.') {
        continue;
      }

      if (Editor::separatorp(c) || c == '"' || c == '\'') {
        break;
      }
    }

    start++;

    /*
     * find the end of the word.
     */

    for (stop = i + 1; stop < eol; ++stop) {
      if (!Editor::charp(line->get(stop))) {
        break;
      }
    }

    /*
     * get the filename in _fname buffer, ext is the last position of '.'
     */

    auto ext = &_fname[0];
    for (j = 0; (start + j) < stop; ++j) {
      _fname[j] = line->get(start + j);
      if (_fname[j] == '.') {
        ext = &_fname[j];
      }
    }

    _fname[j] = '\0';

    /*
     * ignore some extensions
     */

    if (!emstrcmp(ext, ECSTR(".o")) ||
        !emstrcmp(ext, ECSTR(".obj")) ||
        !emstrcmp(ext, ECSTR(".com")) ||
        !emstrcmp(ext, ECSTR(".class")) ||
        !emstrcmp(ext, ECSTR(".exe")) ||
        !emstrcmp(ext, ECSTR(".dll")) ||
        !emstrcmp(ext, ECSTR(".a"))) {
      continue;
    }

    /*
     * search for a number (assumes line number).
     */

    i = curwp->pos();

    do {
      if (i == start) {
        i = stop;
      }
      c = line->get(++i);
    } while (!(c >= '0' && c <= '9') && i < eol);

    /*
     * get the line number in _linenum.
     */

    while (c >= '0' && c <= '9' && i < eol) {
      _linenum = _linenum * 10 + (c - '0');
      c = line->get(++i);
    }

    curwp->setDot(line, eol);

    if (_linenum == 0) {
      continue;
    }

    _flag = true;
    break;
  }

  Editor::setSearchBuffer(save);
  return _linenum != 0;
}

void
Error::clear() {
  _flag = false;
}

CMD
Error::next() {
  /*
   * find or create buffer if it does not exist.
   */
  auto bp(Buffer::find(BUF_PROC));

  if (bp == nullptr) {
    return NIL;
  }

  auto owp(curwp);
  auto wp(bp->show());

  wp->current();
  auto err(get());
  owp->current();

  if (err) {
    if (!newfile(_fname)) {
      return NIL;
    } else {
      auto save(Editor::_repeat);
      Editor::_repeat = _linenum;
      (void)Editor::gotoline();
      Editor::_repeat = save;
      WDGwrite(ECSTR("%L"), Editor::_found.line());
    }
  }

  return T;
}
