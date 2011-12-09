#if	!defined( lint )
static	char rcsid[] = "$Id: main.c,v 1.5 2008/11/29 08:57:37 jullien Exp $";
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
 *	main.c  :  just  a wrapper to emacs entry point.  This is the
 *	only  file  that  can  be  compiled with a C++ compiler (in a
 *	mixed  C/C++  environment,  main  must  be  compiled by a C++
 *	compiler).
 */

#include "emacs.h"

#if	defined( _DOS ) || defined( _WINDOWS_SOURCE )

/*
 *	Expands arguments with *?
 */

static	int	glob( int *pargc, EMCHAR **pargv[] );
static	int 	insert( EMCHAR *name );
static	int	expand( EMCHAR *name );

typedef	struct	NODE {
	EMCHAR		*n_argv;	/* argument string		*/
	struct	NODE	*n_next;	/* pointer to next node		*/
}	NODE;

static NODE	*phead;			/* pointer to list head		*/
static NODE	*ptail;			/* pointer to list tail		*/
static int	newc;			/* new number of argument	*/

/*
 *	A simple glob function. Directories are not expanded.
 */

static	int
glob( int *argc, EMCHAR **argv[] )
{
	NODE	*pcurr;
	NODE	*pnext;
	EMCHAR	**pnew;
	int	i;

	newc  = 0;
	phead = (NODE *)NULL;

	for( i = 0 ; i < *argc ; i++ )
		/*
		 * expand all arguments
		 */
		if( expand( (*argv)[i] ) == 0 )
			return( 0 );

	if((pnew = (EMCHAR **)malloc((size_t)newc * sizeof(EMCHAR *))) == NULL)
		return( 0 );

	for( i = 0, pcurr = phead ; i < newc ; i++ ) {
		/*
		 * write over to new argv
		 */
		pnew[ i ] = pcurr->n_argv;
		pnext	  = pcurr->n_next;
		free( pcurr );
		pcurr	  = pnext;
	}

	*argv = pnew;
	*argc = newc;

	return( 1 );
}

#if	defined( _WINDOWS_SOURCE )
static	int
expand( EMCHAR *argv )
{
	WIN32_FIND_DATA	findbuf;
	HANDLE		hFindFile;

	if( emstrpbrk( argv, ECSTR("*?") ) == NULL )
		/*
		 * no wildcard, insert
		 */
		return( insert( argv ) );

	if( (hFindFile=FindFirstFile( argv, &findbuf ))==INVALID_HANDLE_VALUE )
		/* must be a "no match", go back with error */
		return( 0 );

	/*
	 * loop until no more matches
	 */

	do
		if( insert( findbuf.cFileName ) == 0 )
			return( 0 );
	while( FindNextFile( hFindFile, &findbuf ) != 0 );

	FindClose( hFindFile );

	return( 1 );
}
#else
static	int
expand( argv )
EMCHAR	*argv;
{
	struct find_t findbuf;

	if( emstrpbrk( argv, ECSTR("*?") ) == NULL )
		/*
		 * no wildcard, insert
		 */
		return( insert( argv ) );

	if( _dos_findfirst( (char *)argv, _A_NORMAL, &findbuf ) != 0 )
		/* must be a "no match", go back with error */
		return( 0 );

	/*
	 * loop until no more matches
	 */

	do
		if( insert( (EMCHAR *)emstrlwr( findbuf.name ) ) == 0 )
			return( 0 );
	while( _dos_findnext( &findbuf ) == 0 );

	return( 1 );
}
#endif

static	int
insert( EMCHAR *str )
{
	EMCHAR	*s    = (EMCHAR *)malloc( sizeof(EMCHAR) * (size_t)(emstrlen(str)+1) );
	NODE	*nptr = (NODE *)malloc( sizeof( NODE ) );

	if( (s == NULL) || (nptr == NULL) )
		return( 0 );
	else	newc++;

	(void)emstrcpy( s, str );

#if	!defined(_UNICODE) && defined(_WINCONSOLE)
	CharToOemBuff( s, s, emstrlen( s ) );
#endif

	if( phead == NULL )
		phead	      = nptr;
	else	ptail->n_next = nptr;

	ptail	      = nptr;
	ptail->n_next = NULL;
	ptail->n_argv = s;

	return( 1 );
}

#endif

int EMCDECL
main( int argc, char *argv[] )
{
	int	res;

#if	defined( _WINDEBUG )
        res = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
        res |= _CRTDBG_DELAY_FREE_MEM_DF;
        res |= _CRTDBG_LEAK_CHECK_DF;
        _CrtSetDbgFlag( res );

	/*
	 * Send all reports to stdout.
	 */

	_CrtSetReportMode( _CRT_WARN,   _CRTDBG_MODE_FILE   );
	_CrtSetReportFile( _CRT_WARN,   _CRTDBG_FILE_STDOUT );
	_CrtSetReportMode( _CRT_ERROR,  _CRTDBG_MODE_FILE   );
	_CrtSetReportFile( _CRT_ERROR,  _CRTDBG_FILE_STDOUT );
	_CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_FILE   );
	_CrtSetReportFile( _CRT_ASSERT, _CRTDBG_FILE_STDOUT );
#endif

#if	defined( _X11 )
	res = X11emacs( argc, argv );
	return( res );
#else

#if	(defined(_DOS) || defined(_WINDOWS_SOURCE))
#if	!defined(_WIDECHARS)
	glob( &argc, &argv );
#else
	if( argc == -1) {
		/*
		 * never called but pretends glob is actually used to
		 * prevent compiler warning.
		 */
		glob( &argc, &((EMCHAR **)argv) );
	}
#endif
#endif

#if	defined( _WIDECHARS )
	res = emacsascii( argc, argv );
#else
	res = emacs( argc, argv );
#endif

#if	defined( _WINDEBUG )
	_CrtCheckMemory();
	_CrtDumpMemoryLeaks();
#endif

	return( res );
#endif
}
