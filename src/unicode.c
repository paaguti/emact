#if	!defined( lint )
static	char rcsid[] = "$Id: unicode.c,v 1.10 2015/08/22 14:27:13 jullien Exp $";
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
 *	ASCII / UNICODE stuff.
 */

#include	"emacs.h"

/*
 *	Convert 8bit string to UNICODE string.
 */

#if	defined( _WIDECHARS ) && defined( _WINDOWS_SOURCE )
#define	_WINDOWS_UNICODE
#endif

#if	!defined( _WINDOWS_UNICODE )
#define	EMMAXCHARCONV	1024

static	char	abuf[EMMAXCHARCONV + 1];
static	EMCHAR	ubuf[EMMAXCHARCONV + 1];
#endif

/*
 *	Returns 1 if emacs is compiled for UNICODE.
 */
int
emunicode( void )
{
#if	defined( _WIDECHARS )
	return( 1 );
#else
	return( 0 );
#endif
}

int
emremove(const EMCHAR *path)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wremove( (EMCHAR *)path ) );
#else
	return( remove( emutoa( path, abuf, EMMAXCHARCONV ) ) );
#endif
}

DIR *
emopendir(const EMCHAR *path)
{
#if	defined( _WINDOWS_SOURCE )
	/*
	 *	opendir is UNICODE aware on Windows.
	 */
	return( opendir( path ) );
#else
	return( opendir( emutoa( path, abuf, EMMAXCHARCONV ) ) );
#endif
}

/*
 *	Returns filename associated to current ENTRY as returned by readdir.
 */

EMCHAR *
emgetdirentry( ENTRY *entryp )
{
#if	defined( _WINDOWS_SOURCE )
	/*
	 *	opendir is UNICODE aware on Windows.
	 */
	return( entryp->d_name );
#else
	return( ematou( entryp->d_name, ubuf, EMMAXCHARCONV ) );
#endif
}

int
emsystem(const EMCHAR *path)
{
#if	defined( _WINDOWS_UNICODE )
#if	defined( _WIN32_WCE )
	return( 0 );
#else
	return( _wsystem( path ) );
#endif
#else
	return( system( emutoa( path, abuf, EMMAXCHARCONV ) ) );
#endif
}

int
emchdir(const EMCHAR *path)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wchdir( (EMCHAR *)path ) );
#else
	return( chdir( emutoa( path, abuf, EMMAXCHARCONV ) ) );
#endif
}

int
emstat(const EMCHAR *path, EMSTAT *buf)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wstat( (EMCHAR *)path, buf ) );
#else
	return( stat( emutoa( path, abuf, EMMAXCHARCONV ), buf ) );
#endif
}

int
emaccess(const EMCHAR *path, int mode)
{
#if	defined( _WINDOWS_UNICODE )
	return( _waccess( (EMCHAR *)path, mode ) );
#else
	return( access( emutoa( path, abuf, EMMAXCHARCONV ), mode ) );
#endif
}

int
emchmod(const EMCHAR *path, FMODE_T mode)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wchmod( (EMCHAR *)path, mode ) );
#else
	return( chmod( emutoa( path, abuf, EMMAXCHARCONV ), mode ) );
#endif
}

int
emrename(const EMCHAR *pathold, const EMCHAR *pathnew)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wrename( (EMCHAR *)pathold, (EMCHAR *)pathnew ) );
#else
	char	anew[EMMAXCHARCONV];
	emutoa( pathnew, anew, EMMAXCHARCONV );
	emutoa( pathold, abuf, EMMAXCHARCONV );
	return( rename( abuf, anew ) );
#endif
}

FILE *
emfopen(const EMCHAR *path, const EMCHAR *mode)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wfopen( path, mode ) );
#else
	char	amode[8];
	emutoa( path, abuf,  EMMAXCHARCONV );
	emutoa( mode, amode, 8 );
	return( fopen( abuf, amode ) );
#endif
}

#if	defined( _SPAWNED_PIPE )

FILE *
empopen(const EMCHAR *path, const EMCHAR *mode)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wpopen( path, mode ) );
#else
	char	amode[8];
	emutoa( path, abuf,  EMMAXCHARCONV );
	emutoa( mode, amode, 8 );
	return( popen( abuf, amode ) );
#endif
}
#endif

EMCHAR *
emgetenv(const EMCHAR *path)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wgetenv( (EMCHAR *)path ) );
#else
	char *res = getenv( emutoa( path, abuf, EMMAXCHARCONV ) );

	return( ematou( res, ubuf, EMMAXCHARCONV ) );
#endif
}

EMCHAR	*
emgetcwd(EMCHAR *buffer, int len)
{
#if	defined( _WINDOWS_UNICODE )
	return( _wgetcwd( buffer, len ) );
#else
	(void)getcwd( abuf, EMMAXCHARCONV );
	ematou( abuf, buffer, len );

	return( buffer );
#endif
}

EMCHAR *
emnewstring(const char *str)
{
	size_t	len = strlen( str );
	EMCHAR *res = (EMCHAR *)malloc( (len + 1) * sizeof( EMCHAR ) );
	int	i;

	if( res == (EMCHAR *)NULL ) {
		return( res );
	}

	for( i = 0 ; i < (int)len ; ++i )
		res[ i ] = (EMCHAR)str[ i ];
	res[ i ] = '\000';

	return( res );
}

EMCHAR *
ematou(const char *in, EMCHAR *out, int max)
{
	int	i;

	if( in == NULL )
		return( NULL );

	for( i = 0 ; in[ i ] && i < max ; ++i )
		out[ i ] = (EMCHAR)in[ i ];
	out[ i ] = '\000';

	return( out );
}

/*
 *	Convert UNICODE string to 8bit string.
 */

char *
emutoa(const EMCHAR *in, char *out, int max)
{
	int	i;

	if( in == NULL )
		return( NULL );

	for( i = 0 ; in[ i ] && i < max ; ++i )
		out[ i ] = (char)in[ i ];
	out[ i ] = '\000';

	return( out );
}
