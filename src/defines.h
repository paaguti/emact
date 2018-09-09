/*
 * static char rcsid[] = "$Id: defines.h,v 1.14 2012/10/21 19:07:18 jullien Exp $";
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

#ifndef	__DEFINES_H
#define	__DEFINES_H

#if	defined( __cplusplus )
extern	"C" {
#endif

/*
 *	basic.c
 */

CMD	_define(gotobol,(void));
CMD	_define(backchar,(void));
CMD	_define(gotoeol,(void));
CMD	_define(forwchar,(void));
CMD	_define(gotobob,(void));
CMD	_define(gotoeob,(void));
CMD	_define(gotoline,(void));
CMD	_define(forwline,(void));
CMD	_define(backline,(void));
CMD	_define(forwpage,(void));
CMD	_define(forwother,(void));
CMD	_define(backpage,(void));
CMD	_define(setmark,(void));
CMD	_define(markwholebuffer,(void));
CMD	_define(swapmark,(void));

/*
 *	buffer.c
 */

void	_define(bufferontop,(BUFFER *oldbp));
WINSCR*	_define(showbuffer,(BUFFER *bname));
CMD	_define(anycb,(int flag));
BUFFER *_define(bfind,(EMCHAR *bname, int cflag, int bflag,int mode));
int	_define(bclear,(BUFFER *bp));
CMD	_define(updatemodes,(void));
CMD	_define(disconnectwindow,(WINSCR *wp));
CMD	_define(connectwindow,(WINSCR *wp,BUFFER *bp));
CMD	_define(buffercmd,(int c));
CMD	_define(usebuffer,(void));
CMD	_define(killbuffer,(void));
CMD	_define(listbuffers,(void));

/*
 *	charutil.c
 */

int	_define(emwctomb,(char *mbchar, EMCHAR wchar));
int	_define(emmbtowc,(EMCHAR *wchar, const char *mbchar, size_t count));
int	_define(emmbstowcs,(EMCHAR *wchar, const char *mbchar, size_t count));
size_t	_define(emwcstombs,(char *mbstr, EMCHAR *wcstr, size_t count));
int	_define(emmbclen,(int c));

/*
 *	display.c
 */

void	_define(vtinit,(void));
void	_define(vtfree,(void));
CMD	_define(vtrunning,(void));
void	_define(vttidy,(void));
VIDEO** _define(getvscreen,(void));
void	_define(statputc,(int n,int c));
void	_define(update,(int mode));
void	_define(modeline,(WINSCR *wp));

/*
 *	emacs.c
 */

EXTERN_C int _define(emacsascii,(int argc, char *argv[]));
EXTERN_C int _define(emacs,(int argc, EMCHAR *argv[]));

/*
int	EMCDECL _define(main,(int argc, char *argv[]));
*/
int	_define(execute,(int c,int n));
int	_define(getkey,(void));
int	_define(separatorp,(int c));
int	_define(charp,(int c));
void	_define(emacserror,(EMCHAR *msg, const char *file,int line));
CMD	_define(emacsversion,(void));
CMD	_define(killemacs,(void));
CMD	_define(exitemacs,(void));
CMD	_define(ctlxlp,(void));
CMD	_define(ctlxrp,(void));
CMD	_define(ctlxe,(void));
CMD	_define(ctrlg,(void));
CMD	_define(insertunicode,(void));
CMD	_define(binaryfile,(void));
CMD	_define(utf8encoding,(void));
CMD	_define(utf16encoding,(void));
CMD	_define(systemencoding,(void));
CMD	_define(switchfund,(void));
CMD	_define(switchcc,(void));
CMD	_define(switchcpp,(void));
CMD	_define(switchsgml,(void));
CMD	_define(switchjava,(void));
CMD	_define(switchlisp,(void));
CMD	_define(switchshell,(void));
CMD	_define(switchperl,(void));
CMD	_define(switchprolog,(void));
CMD	_define(switchas,(void));
CMD	_define(switchfortran,(void));

/*
 *	file.c
 */

int	_define(getautomode,(EMCHAR *sp));
int	_define(freadonly,(void));
int	_define(newfile,(EMCHAR *fname));
int	_define(readin,(EMCHAR *fname));
void	_define(makename,(EMCHAR *bname,EMCHAR *fname));
int	_define(writeout,(EMCHAR *fn));
CMD	_define(ansitooem,(void));
CMD	_define(oemtoansi,(void));
CMD	_define(mactoansi,(void));
CMD	_define(mactooem,(void));
EMCHAR *_define(normalize,(EMCHAR *fname,int flag));
EMCHAR *_define(updir,(EMCHAR *fname,int slashflag));
CMD	_define(toggleread,(void));
CMD	_define(fileread,(void));
CMD	_define(filealternate,(void));
CMD	_define(fileinsert,(void));
CMD	_define(filewrite,(void));
CMD	_define(filesave,(void));
CMD	_define(findfile,(void));
CMD	_define(revertbuffer,(void));
CMD	_define(savesomebuffers,(void));
CMD	_define(unlinkfile,(void));
CMD	_define(removefile,(EMCHAR *fname,int flag));
CMD	_define(printbuffer,(void));

/*
 *	filecomp.c
 */

EMCHAR *_define(filematch,(EMCHAR *prompt,EMCHAR *file));
EMCHAR *_define(fileaccept,(EMCHAR *prompt,EMCHAR *file));
CMD	_define(dired,(void));
CMD	_define(diredbuffer,(EMCHAR *fmatch));
CMD	_define(diredcmd,(int c));

/*
 *	fileio.c
 */

FILE *	_define(ffopen,(EMCHAR *fn, EMCHAR* mode, int *widep));
int	_define(ffropen,(EMCHAR *fn,int* binmode, int *widep));
int	_define(ffwopen,(EMCHAR *fn,int binmode, int widep));
int	_define(ffclose,(void));
int	_define(ffputline,(EMCHAR *buf,int nbuf));
int	_define(ffgetline,(EMCHAR *buf, int nbuf,int *len));
EMCHAR *_define(ffgets,(EMCHAR *buf, int nbuf, FILE *fd));
int	_define(ffstat,(EMCHAR *file, EMSTAT *mode));
int	_define(ffchmod,(EMCHAR *file, int mode));
int	_define(ffchdir,(EMCHAR *file));
int	_define(ffsystem,(EMCHAR *cmd));
EMCHAR *_define(ffgetcwd,(EMCHAR *file, int n));
EMCHAR *_define(ffgetenv,(EMCHAR *file));
int	_define(ffremove,(EMCHAR *fn));
int	_define(ffrename,(EMCHAR *oldfn,EMCHAR *newfn));
int	_define(ffaccess,(EMCHAR *fn));
int	_define(ffsetaccess,(EMCHAR *fname));
int	_define(ffullname,(EMCHAR *rname,EMCHAR *fname));
int	_define(ffchanged,(EMCHAR *fname));
int	_define(ffilevalid,(EMCHAR *fname));
int	_define(ffdiredp,(EMCHAR *fname));
void	_define(ffputbom,(int widep));

/*
 *	indent.c
 */

int	_define(leftmargin,(EDLINE *lp));
int	_define(unindent,(int c, int f));
int	_define(lastc,(EDLINE *line));
int	_define(lastlisp,(EDLINE *line));
CMD	_define(tabindent,(void));
CMD	_define(indentline,(void));
CMD	_define(newlineindent,(void));
CMD	_define(backtoindent,(void));
CMD	_define(blispexpr,(void));
CMD	_define(elispexpr,(void));
CMD	_define(justonespace,(void));

/*
 *	line.c
 */

EDLINE *_define(lalloc,(int used));
void	_define(lfree,(EDLINE *lp));
void	_define(lchange,(int fla));
int	_define(linsert,(int n,int c));
int	_define(lreplace,(int n,int c));
int	_define(lnewline,(void));
int	_define(ldelete,(int n,int kflag));
int	_define(addline,(BUFFER *bp,EMCHAR *text));
int	_define(lposition,(EDLINE *line));
void	_define(kdelete,(void));
int	_define(kinsert,(int c));
int	_define(kremove,(int n));
CMD	_define(notmodified,(void));
CMD	_define(ltwiddle,(void));
CMD	_define(instoggle,(void));

/*
 *	llemacs.c
 */

#if	defined( _EMACSLIB )
int	_define(llemacs,(EMCHAR *file));
int	_define(llembol,(EMCHAR *buf));
int	_define(llemeol,(EMCHAR *buf,int n));
CMD	_define(lispevalbuffer,(void));
CMD	_define(evalfunction,(void));
#endif

/*
 *	minibuf.c
 */

void	_define(mlerase,(void));
int	_define(mlyn,(EMCHAR *prompt));
int	_define(mlyesno,(EMCHAR *prompt));
int	_define(mlconfirm,(EMCHAR *prompt));
int	_define(mledit,(EMCHAR *prompt, EMCHAR *buf,int nbuf));
int	_define(mlreply,(EMCHAR *prompt, EMCHAR *buf,int nbuf));
int	_define(mlallowcomplete,(int flag));
void	_define(mlwrite,(EMCHAR *fmt, ...));
void	_define(mlerror,(EMCHAR *msg));
EMCHAR *_define(mltitle,(EMCHAR *buffer,EMCHAR *fname));
int	_define(mlchange,(EMCHAR *msgo, EMCHAR *msgn, EMCHAR *opat, EMCHAR *npat,int length));
void	_define(mlplay,(int flag));
void	_define(mlwait,(void));
void	_define(mlmessage,(EMCHAR *msg));
void	_define(mladjust,(void));
void	_define(mlupdate,(EMCHAR *prompt,EMCHAR *buf));
void	_define(mlclipcopy,(void));
void	_define(mlclippaste,(void));
void	_define(mllpprint,(void));

/*
 *	mlisp.c
 */

#if	!defined( _OPENLISP )
CMD	_define(mlcustomize,(void));
CMD	_define(mlinternaleval,(int expr));
CMD	_define(getmacfile,(void));
CMD	_define(lispevalbuffer,(void));
CMD	_define(evalexpression,(void));
#endif

/*
 *	mouse.c
 */

#if	defined( _MOUSE )
int	_define(mouse_init,(void));
void	_define(mouse_stop,(void));
void	_define(mouse_show,(void));
void	_define(mouse_hide,(void));
int	_define(mouse_click,(void));
void	_define(mouse_move,(unsigned int x,unsigned int y));
#endif

/*
 *	mscterm.c
 */

CMD	_define(switchscreen,(void));

/*
 *	nexterr.c
 */

CMD	_define(nexterror,(void));

/*
 *	options.c
 */

CMD	_define(describekey,(void));
CMD	_define(help,(void));
CMD	_define(setvar,(void));
CMD	_define(uncompile,(void));
CMD	_define(findtag,(void));
CMD	_define(tagsloopcont,(void));
int	_define(completeintag,(int tagnext, EMCHAR *tagname, EMCHAR *tagcomp));

/*
 *	openlisp.c
 */

#if	defined( _OPENLISP )
void	_define(olttynextline,(void));
CMD	_define(olinternalexec,(int idx));
CMD	_define(olcustomize,(void));
CMD	_define(getmacfile,(void));
CMD	_define(lispevalbuffer,(void));
CMD	_define(evalexpression,(void));
#endif

/*
 *	random.c
 */

CMD	_define(newline,(void));
CMD	_define(showcpos,(void));
CMD	_define(twiddle,(void));
CMD	_define(quotechar,(void));
CMD	_define(tab,(void));
CMD	_define(tabexpand,(void));
CMD	_define(openline,(void));
CMD	_define(endline,(void));
CMD	_define(deblank,(void));
CMD	_define(forwdel,(void));
CMD	_define(backdel,(void));
CMD	_define(killtext,(void));
CMD	_define(yank,(void));
CMD	_define(appendnextkill,(void));
CMD	_define(setfillcolumn,(void));
CMD	_define(setfillprefix,(void));
CMD	_define(backparagraph,(void));
CMD	_define(forwparagraph,(void));
CMD	_define(markparagraph,(void));
CMD	_define(fillparagraph,(void));
CMD	_define(splitlinetofill,(void));
CMD	_define(justifycurline,(void));
CMD	_define(setjustifyleft,(void));
CMD	_define(setjustifyfull,(void));
CMD	_define(justifycomment,(void));
CMD	_define(counterinsert,(void));
CMD	_define(counterincr,(void));
CMD	_define(counterdecr,(void));
CMD	_define(counterset,(void));
CMD	_define(counterformat,(void));
CMD	_define(undo,(void));
CMD	_define(enterdebug,(void));

/*
 *	rawmode.c
 */

#if	defined( _DOS )
void	_define(setraw,(void));
void	_define(restraw,(void));
#endif

/*
 *	region.c
 */

CMD	_define(killregion,(void));
CMD	_define(copyregion,(void));
CMD	_define(lowerregion,(void));
CMD	_define(upperregion,(void));
CMD	_define(writeregion,(void));
CMD	_define(fillregion,(void));
CMD	_define(indentregion,(void));
CMD	_define(shiftright,(void));
CMD	_define(shiftleft,(void));

/*
 *	search.c
 */

int	_define(ffindstring,(void));
int	_define(rmatchc,(int patc,int printflag));
int	_define(lmatchc,(int patc,int printflag));
int	_define(automatch,(int c,int f));
void	_define(waitmatch,(int n));
CMD	_define(forwsearch,(void));
CMD	_define(backsearch,(void));
CMD	_define(global,(void));
void	_define(subst,(int length, EMCHAR *newstr));
CMD	_define(query,(void));
CMD	_define(getdefinition,(void));
CMD	_define(matchrpar,(void));
CMD	_define(matchrcur,(void));
CMD	_define(matchrbra,(void));
CMD	_define(matchlpar,(void));
CMD	_define(matchlcur,(void));
CMD	_define(matchlbra,(void));
CMD	_define(completeword,(void));
CMD	_define(diffwindows,(void));
CMD	_define(comparewindows,(void));

/*
 *	spawn.c
 */

int	_define(syscompile,(EMCHAR *cmd, int flag));
CMD	_define(spawncli,(void));
CMD	_define(spawn,(void));
CMD	_define(makefile,(void));
CMD	_define(man,(void));
CMD	_define(grep,(void));
CMD	_define(perl,(void));
CMD	_define(sed,(void));
CMD	_define(compile,(void));
CMD	_define(compilecurrent,(void));
CMD	_define(ccompile,(void));
CMD	_define(javacompile,(void));
CMD	_define(assemble,(void));
CMD	_define(evalbuf,(void));
CMD	_define(getcommand,(void));
CMD	_define(changedir,(void));

/*
 *	termio.c
 */

void	_define(ttopen,(void));
void	_define(ttclose,(void));
void	_define(ttputc,(int c));
void	_define(ttputs,(EMCHAR *s, int n));
void	_define(ttcshow,(int f));
void	_define(ttflush,(void));
int	_define(ttgetc,(void));

/*
 *	unicode.c
 */

int	_define(emunicode,(void));
int	_define(emremove,(const EMCHAR *path));
int	_define(emchdir,(const EMCHAR *path));
int	_define(emsystem,(const EMCHAR *path));
int	_define(emstat,(const EMCHAR *path, EMSTAT *buf));
int	_define(emaccess,(const EMCHAR *path, int mode));
int	_define(emchmod,(const EMCHAR *path, FMODE_T mode));
int	_define(emrename,(const EMCHAR *pathold, const EMCHAR *pathnew));
FILE *	_define(emfopen,(const EMCHAR *path, const EMCHAR *mode));
FILE *	_define(empopen,(const EMCHAR *path, const EMCHAR *mode));
EMCHAR* _define(emgetcwd,(EMCHAR *buffer, int len));
EMCHAR*	_define(emgetenv,(const EMCHAR *path));
DIR* 	_define(emopendir,(const EMCHAR *path));
EMCHAR* _define(emgetdirentry,(ENTRY *entryp));
EMCHAR *_define(emnewstring,(const char *str));
EMCHAR *_define(ematou,(const char *in, EMCHAR *out, int max));
char   *_define(emutoa,(const EMCHAR *in, char *out, int max));

/*
 *	window.c
 */

CMD	_define(reposition,(void));
CMD	_define(recenter,(void));
CMD	_define(redrawscreen,(void));
CMD	_define(resize,(void));
CMD	_define(setcurrentwindow,(WINSCR *wp));
CMD	_define(nextwind,(void));
CMD	_define(prevwind,(void));
CMD	_define(topwind,(void));
CMD	_define(mvdnwind,(void));
CMD	_define(mvupwind,(void));
CMD	_define(onlywind,(void));
CMD	_define(delwind,(void));
CMD	_define(splitwind,(void));
CMD	_define(enlargewind,(void));
CMD	_define(shrinkwind,(void));
CMD	_define(findwind,(void));
CMD	_define(adjust,(void));
WINSCR *_define(wpopup,(void));

/*
 *	word.c
 */

int	_define(inword,(void));
CMD	_define(wordatcursor,(EMCHAR *buf,int len));
CMD	_define(backword,(void));
CMD	_define(forwword,(void));
CMD	_define(upperword,(void));
CMD	_define(lowerword,(void));
CMD	_define(capword,(void));
CMD	_define(delfword,(void));
CMD	_define(delbword,(void));
CMD	_define(wtwiddle,(void));

#if	defined( _X11 )
int	_define(X11emacs,(int argc, char *argv[]));
#endif

#if	defined( __cplusplus )
}
#endif

#endif
