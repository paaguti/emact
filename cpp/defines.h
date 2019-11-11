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
 *      emacs.cpp
 */

int     getkey();
bool    separatorp(int c);
bool    charp(int c);

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
CMD     justonespace();

/*
 *      killbuf.cpp
 */

void    kdelete();
bool    kinsert(int c);
int     kremove(int n);
const std::pair<const EMCHAR*, size_t> kget();

/*
 *      llemacs.cpp
 */

#if     defined( _EMACSLIB )
int     llemacs(EMCHAR* file);
int     llembol(const EMCHAR* buf);
int     llemeol(const EMCHAR* buf,int n);
CMD     lispevalbuffer();
CMD     evalfunction();
#endif

/*
 *      minibuf.cpp
 */

void    mlerase();
int     mlcursor();
CMD     mlyn(const EMCHAR* prompt);
CMD     mlyesno(const EMCHAR* prompt);
CMD     mlconfirm(const EMCHAR* prompt);
CMD     mledit(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
CMD     mlreply(const EMCHAR* prompt, EMCHAR* buf, int nbuf);
bool    mlallowcomplete(bool flag);
void    mlwrite(const EMCHAR* fmt, ...);
void    mlerror(const EMCHAR* msg);
EMCHAR* mltitle(EMCHAR* buffer,EMCHAR* fname);
CMD     mlchange(const EMCHAR* msg, EMCHAR* opat, EMCHAR* npat, int length);
void    mlplay(int flag);
void    mlwait();
void    mlmessage(const EMCHAR* msg);
void    mladjust();
void    mlupdate(const EMCHAR* prompt, EMCHAR* buf);
void    mlclipcopy();
void    mlclippaste();
void    mllpprint();

/*
 *      mlisp.cpp
 */

bool    mlcustomize();
CMD     mlinternaleval(int expr);
CMD     getmacfile();
CMD     lispevalbuffer();
CMD     evalexpression();

/*
 *      mscterm.cpp
 */

CMD     switchscreen();

/*
 *      nexterr.cpp
 */

void    clearerr();
CMD     nexterror();

/*
 *      options.cpp
 */

CMD     describekey();
CMD     help();
CMD     setvar();
CMD     uncompile();
CMD     findtag();
CMD     tagsloopcont();
int     completeintag(int tagnext, const EMCHAR* tagname, EMCHAR* tagcomp);

/*
 *      random.cpp
 */

CMD     newline();
CMD     showcpos();
CMD     twiddle();
CMD     quotechar();
CMD     tab();
CMD     tabexpand();
CMD     openline();
CMD     endline();
CMD     deblank();
CMD     forwdel();
CMD     backdel();
CMD     killtext();
CMD     yank();
CMD     appendnextkill();
CMD     setfillcolumn();
CMD     setfillprefix();
CMD     backparagraph();
CMD     forwparagraph();
CMD     markparagraph();
CMD     fillparagraph();
CMD     splitlinetofill();
CMD     justifycurline();
CMD     setjustifyleft();
CMD     setjustifyfull();
CMD     justifycomment();
CMD     counterinsert();
CMD     counterincr();
CMD     counterdecr();
CMD     counterset();
CMD     counterformat();
CMD     undo();
CMD     enterdebug();

/*
 *      search.cpp
 */

bool    ffindstring();
bool    rmatchc(int patc, bool printflag = true);
bool    lmatchc(int patc, bool printflag = true);
bool    automatch(int c, bool f = true);
void    waitmatch(int n);
CMD     forwsearch();
CMD     backsearch();
CMD     global();
void    subst(int length, const EMCHAR* newstr);
CMD     query();
CMD     getdefinition();
CMD     matchrpar();
CMD     matchrcur();
CMD     matchrbra();
CMD     matchlpar();
CMD     matchlcur();
CMD     matchlbra();
CMD     completeword();
CMD     diffwindows();
CMD     comparewindows();

/*
 *      spawn.cpp
 */

bool    syscompile(const EMCHAR* cmd, int flag);
CMD     spawncli();
CMD     spawn();
CMD     makefile();
CMD     man();
CMD     grep();
CMD     perl();
CMD     sed();
CMD     compile();
CMD     compilecurrent();
CMD     ccompile();
CMD     javacompile();
CMD     assemble();
CMD     evalbuf();
CMD     getcommand();
CMD     changedir();

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

/*
 *      word.cpp
 */

bool    inword();
bool    wordatcursor(EMCHAR* buf, size_t len);
CMD     backword();
CMD     forwword();
CMD     upperword();
CMD     lowerword();
CMD     capword();
CMD     delfword();
CMD     delbword();
CMD     wtwiddle();

#if     defined( _X11 )
int     X11emacs(int argc, char* argv[]);
#endif

#endif
