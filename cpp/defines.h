/*
 * static auto rcsid("$Id: defines.h,v 1.33 2018/09/04 05:13:08 jullien Exp $");
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

#ifndef __DEFINES_H
#define __DEFINES_H

/*
 *      charutil.cpp
 */

int     emwctomb(char* mbchar, EMCHAR wchar);
int     emmbtowc(EMCHAR* wchar, const char* mbchar, size_t count);
int     emmbstowcs(EMCHAR* wchar, const char* mbchar, size_t count);
size_t  emwcstombs(char* mbstr, EMCHAR* wcstr, size_t count);
int     emmbclen(int c);

/*
 *      file.cpp
 */

EDITMODE getautomode(const EMCHAR* sp);
void     resetfreadonly();
bool     freadonly();
bool     newfile(const EMCHAR* fname);
bool     readin(const EMCHAR* fname);
void     makename(EMCHAR* bname, const EMCHAR* fname);
bool     writeout(const EMCHAR* fn);
CMD      ansitooem();
CMD      oemtoansi();
CMD      mactoansi();
CMD      mactooem();
EMCHAR*  normalize(EMCHAR* fname, int flag);
EMCHAR*  updir(EMCHAR* fname, int slashflag);
CMD      toggleread();
CMD      fileread();
CMD      filealternate();
CMD      fileinsert();
CMD      filewrite();
CMD      filesave();
CMD      findfile();
CMD      revertbuffer();
CMD      savesomebuffers();
CMD      unlinkfile();
bool     removefile(const EMCHAR* fname, bool flag);
CMD      printbuffer();

/*
 *      filecomp.cpp
 */

EMCHAR* filematch(const EMCHAR* prompt, EMCHAR* file);
EMCHAR* fileaccept(const EMCHAR* prompt, EMCHAR* file);
CMD     dired();
bool    diredbuffer(const EMCHAR* fmatch);
CMD     diredcmd(int c);

/*
 *      fileio.cpp
 */

FILE*   ffopen(const EMCHAR* fn, const EMCHAR* mode, ENCODING* widep = nullptr);
int     ffropen(const EMCHAR* fn, bool* binmode, ENCODING* widep);
int     ffwopen(const EMCHAR* fn, int binmode, ENCODING widep);
int     ffclose();
int     ffputline(const EMCHAR* buf, int nbuf);
int     ffgetline(EMCHAR* buf, int nbuf, int* len);
EMCHAR* ffgets(EMCHAR* buf, int nbuf, FILE* fd);
int     ffstat(const EMCHAR* file, EMSTAT* mode);
int     ffchmod(const EMCHAR* file, bool readonly);
int     ffchmod(const EMCHAR* file, mode_t mode);
int     ffchdir(const EMCHAR* file);
int     ffsystem(const EMCHAR* cmd);
EMCHAR* ffgetcwd(EMCHAR* file, int n);
EMCHAR* ffgetenv(const EMCHAR* file);
int     ffremove(const EMCHAR* fn);
int     ffrename(const EMCHAR* oldfn, const EMCHAR* newfn);
int     ffaccess(const EMCHAR* fn);
bool    ffsetaccess(const EMCHAR* fname, Buffer* bp);
int     ffullname(EMCHAR* rname, const EMCHAR* fname);
bool    ffchanged(const EMCHAR* fname, time_t* time);
bool    ffilevalid(const EMCHAR* fname);
bool    ffdiredp(const EMCHAR* fname);
void    ffputbom(ENCODING widep);

/*
 *      indent.cpp
 */

bool    unindent(int c, bool f = true);
int     lastc(Line* line);
int     lastlisp(Line* line);
CMD     tabindent();
CMD     indentline();
CMD     newlineindent();
CMD     backtoindent();
CMD     blispexpr();
CMD     elispexpr();

/*
 *      llemacs.cpp
 */

#if     defined( _EMACSLIB )
int     llemacs(EMCHAR* file);
int     llembol(const EMCHAR* buf);
int     llemeol(const EMCHAR* buf,int n);
CMD     evalbuffer();
CMD     evalfunction();
#endif

/*
 *      mscterm.cpp
 */

CMD     switchscreen();

/*
 *      random.cpp
 */

//CMD     newline();
//CMD     showcpos();
//CMD     twiddle();
//CMD     quotechar();
//CMD     tab();
//CMD     tabexpand();
//CMD     openline();
//CMD     endline();
//CMD     deblank();
//CMD     forwdel();
//CMD     backdel();
//CMD     killtext();
//CMD     yank();

#if 0
CMD     Editor::appendNextKill();
CMD     Editor::setFillColumn();
CMD     Editor::setFillPrefix();
CMD     Editor::backParagraph();
CMD     Editor::forwParagraph();
CMD     Editor::markParagraph();
CMD     Editor::fillParagraph();
CMD     Editor::splitLineToFill();
CMD     Editor::justOneSpace();
CMD     Editor::justifyCurLine();
CMD     Editor::setJustifyLeft();
CMD     Editor::setJustifyFull();
CMD     Editor::justifyComment();
CMD     Editor::Editor::undo();
CMD     Editor::enterDebug();
#endif

/*
 *      termio.cpp
 */

void    ttopen();
void    ttclose();
void    ttputc(int c);
void    ttputs(EMCHAR* s, int n);
void    ttcshow(int f);
void    ttflush();
int     ttgetc();

/*
 *      unicode.cpp
 */

int     emunicode();
int     emremove(const EMCHAR* path);
int     emchdir(const EMCHAR* path);
int     emsystem(const EMCHAR* path);
int     emstat(const EMCHAR* path, EMSTAT *buf);
int     emaccess(const EMCHAR* path, int mode);
int     emchmod(const EMCHAR* path, bool readonly);
int     emchmod(const EMCHAR* path, mode_t mode);
int     emrename(const EMCHAR* pathold, const EMCHAR* pathnew);
FILE*   emfopen(const EMCHAR* path, const EMCHAR* mode);
FILE*   empopen(const EMCHAR* path, const EMCHAR* mode);
EMCHAR* emgetcwd(EMCHAR* buffer, int len);
EMCHAR* emgetenv(const EMCHAR* path);
DIR*    emopendir(const EMCHAR* path);
EMCHAR* emgetdirentry(ENTRY *entryp);
EMCHAR* ematou(const char *in, EMCHAR* out, int max);
char*   emutoa(const EMCHAR* in, char *out, int max);

#if     defined( _X11 )
int     X11emacs(int argc, char* argv[]);
#endif

#endif
