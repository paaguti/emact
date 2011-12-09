/*
 * static char rcsid[] = "$Id: emacs.h,v 1.20 2009/05/09 07:15:25 jullien Exp $";
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

#ifndef	__EMACS_H
#define	__EMACS_H

/*
 *	This  file  is  the  general header file for all parts of the
 *	EMACS   display  editor.  It  contains  definitions  used  by
 *	everyone,  and  it  contains  the  stuff  you have to edit to
 *	create  a  version  of  the  editor  for a specific operating
 *	system and terminal.
 */

#if	defined( _MSC_VER ) && (_MSC_VER >= 1400)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#define	_CRT_NON_CONFORMING_SWPRINTFS	1
#endif

#if	defined( _WIDECHARS )
#if	!defined( _UNICODE )
#define	_UNICODE
#endif
#if	!defined( UNICODE )
#define	UNICODE
#endif
#if	defined( __linux__ )
#define	_ISOC99_SOURCE
#define	_POSIX_C_SOURCE		2
#endif
#endif

#if	(defined( UNICODE ) || defined( _UNICODE )) && !defined( _WIDECHARS )
#define	_WIDECHARS
#endif

#if	defined( _WINDEBUG )
#if	!defined( _DEBUG )
#define	_DEBUG
#endif
#include	<crtdbg.h>
#endif

#if	defined( _RELEASE ) || !defined( __SC__ )
#if	!defined( NDEBUG )
#define	NDEBUG			/* define this variable in release mode */
#endif
#endif

#if	!defined( _ISO )
#define	_ISO
#endif

/*
 *	START OF INCLUDE SECTION
 */

#if	defined( HAVE_CONFIG_H )
#include	"config.h"

#if	!defined( X_DISPLAY_MISSING )
#define	_X11
#endif

#if	defined( HAVE_CURSES_H ) && defined( HAVE_LIBCURSES )
#define	_CURSES
#endif

#if	defined( HAVE_NCURSES_H ) && defined( HAVE_LIBNCURSES )
#if	!defined( _CURSES )
#define	_CURSES
#endif
#endif

#if	!defined( _CURSES )
#define	_TERMCAP
#endif

#endif

#if	defined( __linux__ ) && !defined( __USE_POSIX )
#define	__USE_POSIX
#endif

#include	<stdio.h>
#include	<ctype.h>
#include	<stdlib.h>
#include	<stdarg.h>

#if	!defined( _WIN32_WCE )
#include	<time.h>
#include	<assert.h>
#else
#include	"winport.h"
#define	clock()	_wceclock()
#endif

#if	defined( _BSD ) && !defined( COHERENT )
#include	<strings.h>
#include	<sys/timeb.h>	/* no clock in pure BSD */
#else
#include	<string.h>
#endif

#if	defined( _XMALLOC )
#include	"xmalloc.h"
#define	malloc( n )		xmalloc( n )
#define	free( n )		xfree( n )
#define	realloc( p, n )		xrealloc( p, n )
#define	calloc( p, n )		xcalloc( p, n )
#endif

#if	defined( _WIN32 ) || defined( _WIN64 )
#define	_WINDOWS_SOURCE
#define	EMCDECL	__cdecl
#else
#define	EMCDECL
#endif

#if	defined( _DOS )
#include	<dos.h>
#endif

#if	defined( _OS2 )
#include	<os2.h>
#if	!defined( _OS2V1 )
#define	_OS2V2
#endif
#endif

#if	(defined(_DOS) \
	|| defined(_OS2) \
	|| defined(__MINGW32__) \
	|| defined(__CYGWIN__) \
	|| defined(_WINDOWS_SOURCE)) \
	&& !defined(_WIN32_WCE)
#include	<io.h>
#if	defined( __TURBOC__ )
#include	<dir.h>
#endif
#if	((defined( _MSC_VER ) && (_MSC_VER >= 700)))
#include	<direct.h>
#endif
#if	defined( __SC__ )
#include	<direct.h>
#endif
#if	defined( _OS2 )
#include	<direct.h>
#if	!defined( __TURBOC__ )
#define	getcwd	_getcwd
#endif
#endif
#endif

#if	defined( _DIRECTORY )
#if	!defined( _WIN32_WCE )
#include	<sys/types.h>
#include	<sys/stat.h>
#endif
#if	defined( _BSD ) && !defined( COHERENT ) && !defined( HAVE_DIRENT_H )
#include	<sys/dir.h>
typedef	struct	direct	ENTRY;
#else
#include	<dirent.h>
typedef	struct	dirent	ENTRY;
#endif
#endif

#if	defined( _POSIX_SOURCE ) || defined( HAVE_UNISTD_H )
#include	<sys/types.h>
#include	<unistd.h>
#if	!defined(_POSIX_SOURCE)
#define	_POSIX_SOURCE
#endif	/* _POSIX_SOURCE */
#endif  /* _POSIX_SOURCE || HAVE_UNISTD_H */

#if	defined( _VMS )
#include	<types.h>
#include	<stat.h>
#include	<stsdef.h>
#include	<ssdef.h>
#include	<descrip.h>
#include	<iodef.h>
#include	<ttdef.h>
#endif

#if	defined( _WINDOWS_SOURCE )
#define	WIN32_LEAN_AND_MEAN
#define	WIN32_EXTRA_LEAN
#include	<windows.h>
#if	!defined( _WIN32_WCE )
#include	<io.h>
#if	defined( __TURBOC__ )
#include	<dir.h>
#endif	/* __TURBOC__ */
#if	defined( __SC__ ) || defined( _MSC_VER ) || defined( __LCC__ )
#include	<direct.h>
#endif	/* __SC__ || _MSC_VER || __LCC__ */
#endif	/* _WIN32_WCE	   */
#endif	/* _WINDOWS_SOURCE */

#if	defined(_DOSPATH)
#define	popen	_popen
#define	pclose	_pclose
#define	_SPAWNED_PIPE
#endif

#if	(defined( _OS2V2 ) && defined( __TURBOC__ )) || defined( __BORLANDC__ )
#define	popen	_popen
#define	pclose	_pclose
#define	_SPAWNED_PIPE
#endif

#if	defined(_WINDOWS_SOURCE) && defined(_MSC_VER) && !defined(_WIN32_WCE)
#define	popen	_popen
#define	pclose	_pclose
#define	_SPAWNED_PIPE
#endif

#if	defined( _UNIX )
#define	_SPAWNED_PIPE
#endif

#include	"mouse.h"

#if	defined( _OPENLISP )
#include	"openlisp.h"
#endif

/*
 *	END OF INCLUDE SECTION
 */

#if	defined( __cplusplus )
extern	"C" {
#endif

#if	!defined( EXTERN_C )
#if	defined( __cplusplus )
#define	EXTERN_C	extern "C"
#else
#define	EXTERN_C
#endif
#endif

#if	!defined( assert )
#define	assert( x )	((void)0)
#endif

#if	defined( __FILE__ ) && defined( __LINE__ )
#define	internalerror( msg )	emacserror( msg, __FILE__, __LINE__ )
#else
#define	internalerror( msg )	emacserror( msg, NULL, 0 )
#endif

#if	defined( _POSIX_SOURCE ) || defined( _POSIX_C_SOURCE )
#define	WMODE	(S_IRUSR|S_IWUSR)
#define	WACCESS	S_IWUSR
#define	FMODE_T	mode_t
#define	FTIME_T	time_t
#else
#if	defined( S_IREAD )
#define	WMODE	(S_IREAD|S_IWRITE)
#define	WACCESS	S_IWRITE
#define	FMODE_T	int
#define	FTIME_T	time_t
#endif
#endif

#if	!defined( WMODE ) && !defined( WACCESS )
#define	WMODE	(00600)
#define	WACCESS	(00200)
#define	FMODE_T	int
#define	FTIME_T	unsigned long
#endif

#if	!defined( S_ISDIR )
#if	!defined( S_IFMT )
#define	S_IFMT	0xF000
#endif
#define	S_ISDIR( mode )		(((mode) & S_IFMT) == S_IFDIR)
#endif

#if	!defined( S_ISREG )
#define	S_ISREG( mode )		((mode) & S_IFREG)
#endif

#if	!defined( F_OK )
#define	F_OK			0x00
#endif

#if	defined( _DOS ) && !defined( _WINDOWS_SOURCE ) && !defined( _ANSITERM )
#define	_PCTERM
#endif

typedef	int CMD;

#if	defined(_DOS) || defined(_OS2) || defined(_WINDOWS_SOURCE)
#define	_DOSPATH
#endif

#if	defined( _OS2V2 ) && defined( _MISSING_STAT )
#define	stat( pathname, buffer )	OS2stat( pathname, buffer )

#if	defined( access )
#undef	access
#endif

#define	access( x, y )	0

#endif

#if	defined( _UNIX ) && !defined( _POSIX_SOURCE )

#if	!defined( rename )
#define	rename( old, new )	(link( (old), (new) ) || unlink( old ))
#endif

#if	!defined( remove )
#define	remove( file )		unlink( file )
#endif

#endif

/*
 *	Macros to simplify prototype definitions.
 */

#if	!defined( PROTO )
#define	PROTO( x )		x
#endif

#if	!defined( _define )
#define	_define( fun, args )		fun PROTO( args )
#endif

/*
 *	Emacs pseudo-types.
 */

#if	defined( _UNICODE )
#if	!defined( _WIN32_WCE )
#include	<wchar.h>
#include	<wctype.h>
#endif	/* _WIN32_WCE */

#if	defined( _WINDOWS_SOURCE )
#include	<tchar.h>
typedef	TCHAR		EMCHAR;
#else
typedef	wchar_t		EMCHAR;
#endif

#if	!defined( _WIN32_WCE ) && !defined( __linux__ )
#define	stat	_stat
#endif

typedef struct stat	EMSTAT;

#define	ECSTR( x )	(EMCHAR *)(L ## x)
#define	EMEOF		WEOF
#define	EMBOM		((EMCHAR)0xfeff)   /* BOM = 0xFEFF */

#define	EMASCII		0
#define	EMUTF16		1
#define	EMUTF8		2

#define EMMB_LEN_MAX    4                  /* as required by RFC-3629 */

#if	defined( __linux__ )
#define	EMMAXSPRINT	1024,
#else
#define	EMMAXSPRINT
#endif

#define	emstrcat(s1,s2)		wcscat( s1, s2 )
#define emstrcpy(s1,s2)		wcscpy( s1, s2 )
#define emstrcmp(s1,s2)		wcscmp( s1, s2 )
#define	emstrncat(s1,s2,n)	wcsncat( s1, s2, n )
#define emstrncpy(s1,s2,n)	wcsncpy( s1, s2, n )
#define emstrncmp(s1,s2,n)	wcsncmp( s1, s2, n )
#define	emstrpbrk(s1,s2)	wcspbrk( s1, s2 )
#define	emstrrchr(s1,c)		wcsrchr( s1, c )
#define	emstrlwr(s)		wcslwr( s )

#if	defined( _WIN32_WCE )
#define	emfwide(fd,mode)
#else
#define	emfwide(fd,mode)	fwide( fd, mode )
#endif

#define	emsprintf1(b,f,a)	  swprintf( b, EMMAXSPRINT f, a )
#define	emsprintf2(b,f,x,y)	  swprintf( b, EMMAXSPRINT f, x, y )
#define	emsprintf3(b,f,x,y,z)	  swprintf( b, EMMAXSPRINT f, x, y, z )
#define	emsprintf4(b,f,x,y,z,u)	  swprintf( b, EMMAXSPRINT f, x, y, z, u )
#define	emsprintf5(b,f,x,y,z,u,v) swprintf( b, EMMAXSPRINT f, x, y, z, u, v )

#if	defined( _WINDOWS_SOURCE ) || defined( _DOS )
#define emstrnicmp(s1,s2,n)	wcsnicmp( s1, s2, n )
#else
#define emstrnicmp(s1,s2,n)	wcsncmp( s1, s2, n )
#endif

#define emstrlen(s)		(int)wcslen( s )
#define emstrtoi(s)		(int)wcstol( s, (EMCHAR **)NULL, 0 )
#else	/* _WIDECHARS */
typedef	char		EMCHAR;
typedef struct stat	EMSTAT;

#define	ECSTR( x )	(EMCHAR *)x
#define	EMEOF		EOF
#define	EMBOM		0
#define	EMASCII		0
#define	EMUTF16		0
#define	EMUTF8		0
#define EMMB_LEN_MAX    4                  /* as required by RFC-3629 */

#define	emstrcat(s1,s2)		strcat( (char *)s1, (char *)s2 )
#define emstrcpy(s1,s2)		strcpy( (char *)s1, (char *)s2 )
#define emstrcmp(s1,s2)		strcmp( (char *)s1, (char *)s2 )
#define	emstrncat(s1,s2,n)	strncat( (char *)s1, (char *)s2, n )
#define emstrncpy(s1,s2,n)	strncpy( (char *)s1, (char *)s2, n )
#define emstrncmp(s1,s2,n)	strncmp( (char *)s1, (char *)s2, n )
#define	emstrpbrk(s1,s2)	strpbrk( (char *)s1, (char *)s2 )
#define	emstrrchr(s1,c)		strrchr( (char *)s1, c )
#define	emstrlwr(s)		strlwr( (char *)s )

#define	emsprintf1(b,f,a)	  sprintf( b, f, a )
#define	emsprintf2(b,f,x,y)	  sprintf( b, f, x, y )
#define	emsprintf3(b,f,x,y,z)	  sprintf( b, f, x, y, z )
#define	emsprintf4(b,f,x,y,z,u)	  sprintf( b, f, x, y, z, u )
#define	emsprintf5(b,f,x,y,z,u,v) sprintf( b, f, x, y, z, u, v )

#if	defined( _WINDOWS_SOURCE ) || defined( _DOS )
#define emstrnicmp(s1,s2,n)	strnicmp( (char *)s1, (char *)s2, n )
#else
#define emstrnicmp(s1,s2,n)	strncmp( (char *)s1, (char *)s2, n )
#endif
#define emstrlen(s)		(int)strlen( (char *)s )
#define emstrtoi(s)		(int)strtol( (char *)s, (EMCHAR **)NULL, 0 )
#endif	/* _WIDECHARS */

#if	defined( _WIN32_WCE )
#define	_waccess(s,m)		_wceaccess(s,m)
#define	_wchdir(s)		_wcechdir(s)
#define	_wchmod(s,m)		_wcechmod(s,m)
#define	_wgetcwd(s,l)		_wcegetcwd(s,l)
#define	_wgetenv(e)		_wcegetenv(e)
#define	_wremove(s)		_wceremove(s)
#define	_wrename(s1,s2)		_wcerename(s1,s2)
#define	_wstat(s,m)		_wcestat(s,m)
#define	_wsystem(s)		_wcesystem(s)
#endif	/* _WIN32_WCE */

/*
 *	Get the system name
 */

#if	defined( _POSIX_SOURCE ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("Posix")
#endif

#if	defined( _BSD ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("BSD")
#endif

#if	defined( unix ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("unix")
#endif

#if	defined( _OS2 ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("OS/2")
#endif

#if	defined( _MAC ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("Mac")
#endif

#if	defined( _WINDOWS95 ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("Windows95")
#endif

#if	defined( _WIN32_WCE ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("PocketPC")
#endif

#if	defined( _WIN64 ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("win64")
#endif

#if	defined( _WIN32 ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("win32")
#endif

#if	defined( _WINDOWS ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("Windows")
#endif

#if	defined( _VMS ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("VMS")
#endif

#if	defined( _DOS ) && !defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("MS-DOS")
#endif

#if	!defined( SYSTEMNAME )
#define	SYSTEMNAME	ECSTR("unknown")
#endif

#if	defined( _VMS ) && !defined( PATH )
#define	PATH	"SYS$SYSROOT:[SYSLIB.EMACS]"
#endif

#if	defined( _DOSPATH ) && !defined( PATH )
#define	PATH	ECSTR("c:/usr/lib/emacs")
#endif

#if	!defined( PATH )
#define	PATH	ECSTR("/usr/local/emact/lib")
#endif

#if	defined( _DOSPATH )
#define	CRSIZE	2
#else
#define	CRSIZE	1
#endif

/*
 *	Constant definition :
 */

#define NFILEN		128		/* # of bytes, file name	*/

#if	defined( FILENAME_MAX ) && (NFILEN < FILENAME_MAX)
#undef	NFILEN
#define NFILEN		FILENAME_MAX	/* # of bytes, ISO file name	*/
#endif

#if	defined( _POSIX_PATH_MAX ) && (NFILEN < _POSIX_PATH_MAX)
#undef	NFILEN
#define NFILEN		_POSIX_PATH_MAX	/* # of bytes, POSIX file name	*/
#endif

#define	EMMAXINT	((int)((unsigned)(-1) >> 1)) /* Bigest integer	*/
#define NBUFN		16		/* # of bytes, buffer name	*/
#define NCMDN		16		/* # of bytes, command name	*/
#define	NMAX		64		/* # of macros			*/
#if	!defined( MAXLINE )
#if	defined( unix ) || defined( _WINDOWS_SOURCE ) || defined( __SC__ )
#define MAXLINE		0x8000		/* # of bytes, line in read	*/
#else
#define MAXLINE		2048		/* # of bytes, line in read	*/
#endif
#endif
#define NBLOCK		16		/* Line block chunk size	*/
#define KBLOCK		2048		/* Kill buffer block size	*/
#define NLINE		256		/* # of bytes, line (internal)	*/
#define NKBDM		256		/* # of strokes, keyboard macro */
#define NPAT		80		/* # of bytes, pattern		*/
#define NMACLINE	128		/* # of bytes in macro line	*/

#define	CTLXCH		0x18		/* Ctrl-X prefix		*/
#define	CTLCCH		0x03		/* Ctrl-C prefix		*/
#define	CTLZCH		0x1A		/* Ctrl-Z char			*/
#if	defined( __MVS__ )
#define METACH		0x1B		/* M- prefix,	Control-[, ESC	*/
#define	BACKDEL		0x07
#else
#define METACH		0x1B		/* M- prefix,	Control-[, ESC	*/
#define	BACKDEL		0x7F
#endif
#define	CSICODE		0x8F		/* 8 bits version of ESC [	*/
#define	SS3CODE		0x9B		/* Application mode of CSI	*/

#if	defined( _UNICODE )
#define	_prefix(x)	((x) << 16)
#define	MAX_EMCHAR	0xffff
#else
#define	_prefix(x)	((x) << 8)
#define	MAX_EMCHAR	0xff
#endif

#define Ctrl		_prefix(0x01)	/* Control flag, or'ed in	*/
#define META		_prefix(0x02)	/* Meta flag, or'ed in		*/
#define CTLX		_prefix(0x04)	/* ^X flag, or'ed in		*/
#define	SPCL		_prefix(0x08)	/* Function Key (PC and Clones)	*/
#define	CTLC		_prefix(0x10)	/* ^C flag, or'ed in		*/
#define	MEVT		_prefix(0x20)	/* Mouse click			*/
#define	CXDR		_prefix(0x40)	/* ^X$ prefix			*/
#define	UNBOUND		_prefix(0x80)	/* Unbound			*/

#define	JUSTLEFT	0x00		/* left justification (default)	*/
#define	JUSTFULL	0x10		/* full justification		*/

#if	!defined( NIL )
#define NIL		0x00		/* False, no, bad, etc.		*/
#endif

#if	!defined( T )
#define T		0x10		/* True, yes, good, etc.	*/
#endif

#if	!defined( ABORT )
#define ABORT		0x20		/* Death, ^G, abort, etc.	*/
#endif

#if	!defined( cfree )
#define	cfree( x )	(free( (void *)(x) ), ((x) = NULL))
#endif

#define FIOSUC		0x00		/* File I/O, success.		*/
#define FIOFNF		0x10		/* File I/O, file not found.	*/
#define FIOEOF		0x20		/* File I/O, end of file.	*/
#define FIOERR		0x30		/* File I/O, error.		*/
#define	FIOWNL		0x40		/* File I/O, end without <NL>	*/
#define	FIOENC		0x50		/* File I/O, encoding error	*/

#define CFCPCN		0x0001		/* Last command was C-P, C-N	*/
#define CFKILL		0x0002		/* Last command was a kill	*/
#define CFMMOV		0x0004		/* Last command needs mouse move*/
#define	CFFSRC		0x0008		/* Last command was C-S		*/
#define	CFBSRC		0x0010		/* Last command was C-R		*/
#define	CFTAB		0x0020		/* Last command was TAB		*/
#define	CFCOMP		0x0040		/* Last command was compare-win	*/
#define	CFCPLT		0x0080		/* Last command was complete	*/
#define	CFFKEY		0x0100		/* Last command was fn-key	*/
#define	CFFAIL		0x1000		/* Last command has faild	*/

#define	EXPOSE		0x0100		/* Expose event			*/
#define	MINIBUF		0x0200		/* Minibuffer			*/

#define	FUNDAMENTAL	0x00		/* Fundamental mode		*/
#define	CMODE		0x01		/* Electric C mode		*/
#define	LISPMODE	0x02		/* Lisp mode			*/
#define	PROLOGMODE	0x03		/* Prolog mode			*/
#define	ASMODE		0x04		/* Assembler mode		*/
#define	DIRED		0x05		/* Directory mode		*/
#define	PASCALMODE	0x06		/* Pascal mode			*/
#define	CPPMODE		0x07		/* Electric C++ mode		*/
#define	JAVAMODE	0x08		/* Java mode			*/
#define	FORTRANMODE	0x09		/* Fortran mode			*/
#define	BUFFERMODE	0x0A		/* Buffer mode			*/
#define	SGMLMODE	0x0B		/* SGML mode			*/
#define	PERLMODE	0x0C		/* Perl mode			*/
#define	CSHARPMODE	0x0D		/* C# mode			*/
#define	SHELLMODE	0x0E		/* shell mode			*/

#define	DIREDMARK	2		/* Two character to mark dired	*/
#define	BUFFERPOS	13		/* Buffer name is at pos 13	*/

/*
 *	Flags for functions that deal with directories
 */

#define	SLASH		0x0000		/* Normalize with trailing /	*/
#define	NOSLASH		0x0001		/* Normalize with no trailing /	*/

/*
 *	Flags for functions that deal with external commands
 */

#define	SYSCOMP_NOERROR	0x0000		/* Call system and no error	*/
#define	SYSCOMP_ERRORS	0x0001		/* Call system and check errors	*/

/*
 *	Flags for anycb function
 */

#define	ANYCB_PROMPT	0x0000		/* Prompt for any unsaved buf	*/
#define	ANYCB_CHECK	0x0001		/* Check only unsaved buffers	*/

/*
 *	List of internal buffer names
 */

#define	BUF_LIST	ECSTR("*Buffer List*")	/* Buffer for buffers list */
#define	BUF_HELP	ECSTR("*Help*")		/* Help buffer		   */
#define	BUF_PROC	ECSTR("*Process*")	/* Process buffer	   */
#define	BUF_DIR		ECSTR("*Directory*")	/* Directory buffer	   */
#define	BUF_SCRATCH	ECSTR("*scratch*")	/* Scratch buffer	   */

/*
 *	There  is  a  window  structure  allocated  for  every active
 *	display  window.  The windows are kept in a big list,  in top
 *	to bottom screen order,  with the listhead at "wheadp".  Each
 *	window  contains  its  own  values of dot and mark.  The flag
 *	field  contains  some  bits that are set by commands to guide
 *	redisplay.
 */

typedef struct	WINSCR	{
	struct	WINSCR	*w_wndp;	/* Next window			*/
	struct	BUFFER	*w_bufp;	/* Buffer displayed in window	*/
	struct	EDLINE	*w_linep;	/* Top line in the window	*/
	struct	EDLINE	*w_dotp;	/* Line containing "."		*/
	struct	EDLINE	*w_markp;	/* Line containing "mark"	*/
	int		w_doto;		/* Byte offset for "."		*/
	int		w_marko;	/* Byte offset for "mark"	*/
	int		w_toprow;	/* Origin 0 top row of window	*/
	int		w_ntrows;	/* # of rows of text in window	*/
	int		w_force;	/* If NZ, forcing row.		*/
	int		w_flag;		/* Flags.			*/
	int		w_binary;	/* Binary flag			*/
	int		w_wide;		/* Wide flag			*/
	int		w_emode;	/* Edit Mode (C, Lisp, Normal)	*/
}	WINSCR;

#define	firstline( bp )	lforw( (bp)->b_linep )
#define	lastline( bp )	((bp)->b_linep)

#define WFFORCE 	((int)0x01)	/* Window needs forced reframe	*/
#define WFMOVE		((int)0x02)	/* Movement from line to line	*/
#define WFEDIT		((int)0x04)	/* Editing within a line	*/
#define WFHARD		((int)0x08)	/* Better to a full display	*/
#define WFMODE		((int)0x10)	/* Update mode line.		*/

#define	self_insert(c)	(((unsigned int)c & MAX_EMCHAR) >= 0x20 && c != BACKDEL)

/*
 *	Text  is kept in buffers.  A buffer header,  described below,
 *	exists  for every buffer in the system.  The buffers are kept
 *	in  a big list,  so that commands that search for a buffer by
 *	name  can  find the buffer header.  There is a safe store for
 *	the  dot  and  mark in the header,  but this is only valid if
 *	the  buffer is not being displayed (that is,  if "b_count" is
 *	0).  The  text  for the buffer is kept in a circularly linked
 *	list of lines, with a pointer to the header line in b_linep.
 */

typedef struct	BUFFER {
	struct	BUFFER	*b_bufp;	/* Link to next BUFFER		*/
	struct	EDLINE	*b_dotp;	/* Link to "." EDLINE structure	*/
	struct	EDLINE	*b_markp;	/* The same as the above two,	*/
	struct	EDLINE	*b_linep;	/* Link to the header EDLINE	*/
	FMODE_T		b_fmode;	/* File mode			*/
	FTIME_T		b_time;		/* Last modification time	*/
	int		b_doto;		/* Offset of . in above EDLINE	*/
	int		b_marko;	/* but for the "mark"		*/
	int		b_binary;	/* Binary flag			*/
	int		b_wide;		/* Wide flag			*/
	int		b_emode;	/* Buffer electric mode		*/
	int		b_count;	/* Count of windows on buffer	*/
	int		b_flag;		/* Flags			*/
	EMCHAR		b_fname[NFILEN];/* File name			*/
	EMCHAR		b_bname[NBUFN];	/* Buffer name			*/
}	BUFFER;

#define BFCHG		((int)0x01)	/* Changed since last write	*/

/*
 *	All  text  is  kept  in  circularly  linked  lists  of EDLINE
 *	structures.  These  begin  at  the  header line (which is the
 *	blank  line  beyond  the  end  of  the buffer).  This line is
 *	pointed  to by the "BUFFER".  Each line contains a the number
 *	of bytes in the line (the "used" size),  the size of the text
 *	array,  and  the  text.  The  end  of line is not stored as a
 *	byte;  it's  implied.  Future  additions  will include update
 *	hints, and a list of marks into the line.
 */

typedef struct	EDLINE {
	EMCHAR		*l_text;	/* A bunch of characters.	*/
	struct	EDLINE	*l_fp;		/* Link to the next line	*/
	struct	EDLINE	*l_bp;		/* Link to the previous line	*/
	int		l_size;		/* Allocated size		*/
	int		l_used;		/* Used size			*/
}	EDLINE;

#define lforw( lp )		((lp)->l_fp)
#define lback( lp )		((lp)->l_bp)
#define lgetc( lp, n )		((lp)->l_text[(n)])
#define lputc( lp, n, c ) 	((lp)->l_text[(n)]=((EMCHAR)c))
#define llength( lp )		((lp)->l_used)
#define lsize( lp )		((lp)->l_size)
#define ltext( lp )		((lp)->l_text)
#define	lchadr( lp, n )		((lp)->l_text + (n))

#define	lgetdot()		lgetc( curwp->w_dotp, curwp->w_doto )
#define	lputdot( c )		lputc( curwp->w_dotp, curwp->w_doto, c )

/*
 *	VIDEO structure used by redisplay.
 */

typedef struct	{
	short	flag;		/* Flags			*/
	EMCHAR	*text;		/* Screen data.			*/
}	VIDEO;

/*
 *	The  editor  communicates with the display using a high level
 *	interface.  A  "TERM"  structure holds useful variables,  and
 *	indirect pointers to routines that do useful operations.  The
 *	low  level  get  and  put routines are here too.  This lets a
 *	terminal,  in addition to having non standard commands,  have
 *	funny  get  and put character code too.  Some implementations
 *	using  a high level interface such as Windowing system define
 *	graphic  widget  to select a file,  display error and ask the
 *	user.  A  "WIDGET" structure holds indirect pointers to those
 *	functionalities.
 */  

typedef struct	{
	int	t_nrow;			/* Number of rows.		*/
	int	t_ncol;			/* Number of columns.		*/
	int	t_nscroll;		/* Scroll position.		*/
	int	t_init;			/* Term initialized.		*/
	void	(*t_open)( void );	/* Open terminal at the start.	*/
	void	(*t_close)( void );	/* Close terminal at end.	*/
	int	(*t_getc)( void );	/* Get character from keyboard. */
	void	(*t_putc)( int c );	/* Put character to display.	*/
	void	(*t_puts)(EMCHAR *s,int n);/* Put a string to display.	*/
	void	(*t_flush)( void );	/* Flush output buffers.	*/
	void	(*t_move)(int x,int y);	/* Move the cursor, origin 0.	*/
	void	(*t_eeol)( void );	/* Erase to end of line.	*/
	void	(*t_eeop)( void );	/* Erase to end of page.	*/
	void	(*t_beep)( void );	/* Beep.			*/
	void	(*t_si)( void );	/* Start inverse video		*/
	void	(*t_ei)( void );	/* End   inverse video		*/
	void	(*t_cshow)( int flag );	/* Show/Hide the cursor		*/
	int	(*t_check)( void );	/* Check event			*/
	void	(*t_rawmode)( void );	/* Put in raw mode		*/
}	TERM;

typedef	struct	{
	int	(*w_yn)( EMCHAR *s );			/* Y/N    Widget */
	int	(*w_yesno)( EMCHAR *s );		/* YES/NO Widget */
	int	(*w_confirm)( EMCHAR *s );		/* CONFIRM Widget*/
	void	(*w_error)( EMCHAR *s );		/* ERROR  Widget */
	EMCHAR *(*w_title)( EMCHAR *b, EMCHAR *f );	/* TITLE  Widget */
	int	(*w_asker)(EMCHAR *s,EMCHAR *b,int n);	/* ASKER  Widget */
	int	(*w_edit)( EMCHAR *s,EMCHAR *b,int n);	/* EDITOR Widget */
	int	(*w_change)( EMCHAR *os,
			     EMCHAR *ns,
			     EMCHAR *op,
			     EMCHAR *np,
			     int n );			/* CHANGE  Widget */
	void	(*w_play)( int flag );			/* PLAY   Widget */
	void	(*w_wait)( void );			/* WAIT   Widget */
	void	(*w_message)( EMCHAR *str );		/* MSG    Widget */
	void	(*w_write)( EMCHAR *fmt, ... );		/* WRITE  Widget */
	void	(*w_adjust)( void );			/* ADJUST Widget */
	void	(*w_update)( EMCHAR *p, EMCHAR *b );	/* UPDATE Widget */
	void	(*w_clipcopy)( void );			/* CBCOPY Widget */
	void	(*w_clippaste)( void );			/* CBPAST Widget */
}	WIDGET;

typedef	struct	{
	void	(*p_print)( void );			/* print buffer	  */
}	EMPRINT;

#define	TTYncol			(*pterm).t_ncol
#define	TTYnrow			(*pterm).t_nrow
#define	TTYnscroll		(*pterm).t_nscroll
#define	TTYinit			(*pterm).t_init
#define	TTYbeep()		(*(*pterm).t_beep)()
#define	TTYclose()		(*(*pterm).t_close)()
#define	TTYmove( x, y )		(*(*pterm).t_move)( x, y )
#define	TTYeol()		(*(*pterm).t_eeol)()
#define	TTYeop()		(*(*pterm).t_eeop)()
#define	TTYflush()		(*(*pterm).t_flush)()
#define	TTYgetc()		(*(*pterm).t_getc)()
#define	TTYinverse()		(*(*pterm).t_si)()
#define	TTYnormal()		(*(*pterm).t_ei)()
#define	TTYopen()		(*(*pterm).t_open)()
#define	TTYputc(c)		(*(*pterm).t_putc)(c)
#define	TTYputs( s, n )		(*(*pterm).t_puts)( s, n )
#define	TTYcshow( f )		(*(*pterm).t_cshow)( f )
#define	TTYcheck()		(*(*pterm).t_check)()
#define	TTYrawmode()		(*(*pterm).t_rawmode)()
#if	defined( _UNICODE )
#define	TTYmoveusing( x, y, l )	(*(*pterm).t_moveusing)( x, y, l )
#endif

#define	LPTprint()		(*printer.p_print)()

#define	WDGyn( s )		(*widget.w_yn)( s )
#define	WDGyesno( s )		(*widget.w_yesno)( s )
#define	WDGconfirm( s )		(*widget.w_confirm)( s )
#define	WDGerror( s )		(*widget.w_error)( s )
#define	WDGtitle( b, f )	(*widget.w_title)( b, f )
#define	WDGasker( p, b, n )	(*widget.w_asker)( p, b, n )
#define	WDGedit(  p, b, n )	(*widget.w_edit)(  p, b, n )
#define	WDGchange( o,n,s,r,l )	(*widget.w_change)( o, n, s, r, l )
#define	WDGplay( f )		(*widget.w_play)( f )
#define	WDGwait()		(*widget.w_wait)()
#define	WDGmessage(s)		(*widget.w_message)(s)
#define	WDGwrite		(*widget.w_write)
#define	WDGadjust		(*widget.w_adjust)
#define	WDGupdate(p,b)		(*widget.w_update)(p,b)
#define	WDGclipcopy()		(*widget.w_clipcopy)()
#define	WDGclippaste()		(*widget.w_clippaste)()

/*
 *	Commands and variables table.
 */

typedef struct	{
	int	k_code;			/* Key code			*/
	CMD	(*k_fp)( void );	/* Routine to handle it		*/
	EMCHAR	*k_name;		/* Name of the command		*/
}	KEYTAB;

typedef	struct	{
	void	*f_val;			/* Flag address			*/
	EMCHAR	*f_name;		/* Flag name			*/
	int	f_type;			/* Type/length of the variable	*/
}	VARTAB;

#define	VARval(  i )	((pvartab+i)->f_val)
#define	VARname( i )	((pvartab+i)->f_name)
#define	VARtype( i )	((pvartab+i)->f_type)

#if	defined( _OPENLISP )
#define	internaleval( i )	olinternalexec( i )
#define	customize()		olcustomize()
#else
#define	internaleval( i )	mlinternaleval( i )
#define	customize()		mlcustomize()
#define	POINTER			int *
#endif

typedef	struct	{
	POINTER	m_exec;			/* Code				*/
	EMCHAR	*m_name;		/* Macro name			*/
	int	m_code;			/* Key bind			*/
}	MACTAB;

#define	MACcode( i )		((pmactab+i)->m_code)
#define	MACname( i )		((pmactab+i)->m_name)
#define	MACexec( i )		((pmactab+i)->m_exec)

#define	BOOLEAN		0x0000		/* T or NIL type		*/
#define	FIXVAL		0x0001		/* Integer type			*/
#define	STRING		0x0002		/* String type			*/

typedef	union	{
	EMCHAR	*((*fn)( EMCHAR *prompt, EMCHAR *buf ));
	long	flag;
}	COMPLETE;

extern	COMPLETE complete;		/* Automatic completion		*/
extern	int	 completion;		/* Next action for completion	*/

#define	COMPLETE_ONE		0x00
#define	COMPLETE_AGAIN		0x01
#define	COMPLETE_ABORT		0x02
#define	COMPLETE_FAIL		0x04

extern	WINSCR	*curwp;			/* Current window		*/
extern	WINSCR	*wheadp;		/* Head of list of windows	*/

extern	BUFFER	*curbp;			/* Current buffer		*/
extern	BUFFER	*bheadp;		/* Head of list of buffers	*/

extern	TERM	term;			/* Terminal information.	*/
extern	TERM	*pterm;			/* Terminal information.	*/
extern	WIDGET	widget;			/* Widgets tools		*/
extern	EMPRINT	printer;		/* Printer object		*/

#if	defined(_WINDOWS_SOURCE) || ((defined(_MSC_VER) && (_MSC_VER >= 700)))

/*
 *	for really strict compilers
 */

#define	_NPAT	NPAT
#define	_NBUFN	NBUFN
#define	_NCMDN	NCMDN
#define	_NKBDM	NKBDM
#else
#define	_NPAT
#define	_NBUFN
#define	_NCMDN
#define	_NKBDM
#endif

extern	char	**aargv;		/* ASCII Argv			*/
extern	EMCHAR	**eargv;		/* Argv				*/
extern	EMCHAR	library[ _NPAT ];	/* Library path			*/
extern	EMCHAR	as_name[ _NCMDN ];	/* Assembler name		*/
extern	EMCHAR	as_arg[ _NPAT ];	/* Assembler argument		*/
extern	EMCHAR	cc_arg[ _NPAT ];	/* Compiler argument		*/
extern	EMCHAR	cc_name[ _NCMDN ];	/* Compiler name		*/
extern	EMCHAR	fill_prefix[_NPAT];	/* Fill prefix string		*/
extern	EMCHAR	java_comp_args[ _NPAT ];/* Java compiler argument	*/
extern	EMCHAR	java_comp_name[_NCMDN];	/* Java compiler name		*/
extern	EMCHAR	java_exec_args[ _NPAT ];/* Java executable argument	*/
extern	EMCHAR	java_exec_name[_NCMDN];	/* Java extern name		*/
extern	EMCHAR	make_arg[ _NPAT ];	/* Make argument		*/
extern	EMCHAR	make_name[ _NCMDN ];	/* Make name			*/
extern	EMCHAR	curchar;		/* Char at cursor		*/
extern	EMCHAR	search_buffer[ _NPAT ];	/* Search pattern		*/
extern	EMCHAR	helpfile1[ _NPAT ];	/* help file 1			*/
extern	EMCHAR	helpfile2[ _NPAT ];	/* help file 2			*/
extern	EMCHAR	helpfile3[ _NPAT ];	/* help file 1			*/
extern	EMCHAR	helpfile4[ _NPAT ];	/* help file 2			*/
extern	EMCHAR	*version;		/* Current version		*/
extern	EMCHAR	*licence;		/* Current licence		*/

extern	int	kbdm[ _NKBDM ];		/* Holds keyboard macro data	*/
extern	int	*kbdmip;		/* Input pointer for above	*/
extern	int	*kbdmop;		/* Output pointer for above	*/

extern	long	maxlen;			/* Number of lines		*/

extern	EDLINE	*found_p;		/* Point to a line that match	*/
extern	int	found_o;		/* Offset of the match		*/

extern	EMCHAR	*kbufp;			/* Kill buffer data		*/
extern	size_t	kused;			/* # of bytes used in KB	*/
extern	int	eargc;			/* Argc				*/
extern	int	append_process_buffer;	/* Append in process buffer	*/
extern	int	auto_fill_mode;		/* Auto fill mode flag		*/
extern	int	auto_encoding_mode;	/* Auto encoding mode flag	*/
extern	int	background_color;	/* Background color		*/
extern	int	backup_before_writing;	/* Save a .BAK file		*/
extern	int	binary_mode;		/* Save in binary mode		*/
extern	int	black_on_white;		/* Display attribute		*/
extern	int	bold_font;		/* Use bold font attribute	*/
extern	int	case_sensitivity;	/* Preserve case in search 	*/
extern	int	compile_in_buffer;	/* Compile in buffer or not	*/
extern	int	confirm_unsaved_buffer;	/* Prompt for unsaved buffer	*/
extern	int	date_completion;	/* Complete date __/__/__	*/
extern	int	display_43l;		/* Display 43 lines		*/
extern	int	display_command;	/* Display command on mode line	*/
extern	int	fast_redisplay;		/* Fast redisplay		*/
extern	int	fill_column;		/* Column to start filling on	*/
extern	int	foreground_color;	/* Foreground color		*/
extern	int	gnu_compatible;		/* GNU Emacs compatible		*/
extern	int	java_indent;		/* Java indent			*/
extern	int	latext_mode;		/* LaTex mode flag		*/
extern	int	line_number_mode;	/* Display average on mode line	*/
extern	int	justmode;		/* Justification mode		*/
extern	int	mouse_flag;		/* Activation mouse flag	*/
extern	int	monochrome_monitor;	/* Monochrome flag		*/
extern	int	pipe_process;		/* Use pipes with process	*/
extern	int	process_pending;	/* Flag a process pending	*/
extern	int	print_eval_loop;	/* Print lisp evaluations	*/
extern	int	replace_mode;		/* Replace mode status		*/
extern	int	tab_display;		/* Default display size		*/
extern	int	tab_size;		/* Default tabulation size	*/
extern	int	screen_height;		/* Screen height		*/
extern	int	screen_width;		/* Screen width			*/
extern	int	set_show_graphic;	/* Display graphic char or not	*/
extern	int	show_menu;		/* Show menu on Windows		*/
extern	int	system_colors;		/* Display system colors	*/
extern	int	repeat;			/* Repeat count			*/
extern	int	curflag;		/* Working cursor flag		*/
extern	int	frdflag;		/* flag for freadonly		*/
extern	int	currow;			/* Cursor row			*/
extern	int	curcol;			/* Cursor column		*/
extern	int	mbcursor;		/* Minibuffer cursor		*/
extern	int	thisflag;		/* Flags, this command		*/
extern	int	lastflag;		/* Flags, last command		*/
extern	int	editflag;		/* Flags, edit command		*/
extern	int	initflag;		/* Flags, for initialisation	*/
extern	int	mouse_avoidance_mode;	/* Auto mouse move flag		*/
extern	int	mouse_avoidance_nudge;	/* Auto mouse move nudge	*/
extern	int	mouseflag;		/* Flags, for mouse interface	*/
extern	int	errflag;		/* Flags, an error		*/
extern	int	curgoal;		/* Goal for C-P, C-N		*/
extern	int	mpresf;			/* Stuff in message line	*/
extern	int	sgarbf;			/* State of screen unknown	*/
extern	int	average;		/* Model Line average		*/

#include	"defines.h"

#if	defined( __cplusplus )
}
#endif

#endif
