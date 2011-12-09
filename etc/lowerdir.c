#if	!defined( lint )
static	char *sccsid = "@(#)lowerdir.c	(c) C. Jullien 2009/05/02";
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
 *	lowerdir.c :	
 */

/*
 *	Convert  upper  case  only filenames into lower case with the
 *	following rules :
 *
 *	Files  that  have  already  at lease one lowercase letter are
 *	not converted.
 *
 *	Files that have a special name are converted (list may grow)
 *
 *		README		->	README
 *		READ.ME		->	READ.ME
 *		MAKEFILE	->	Makefile
 *
 *	Other  upper  case  and  number  only  files are converted in
 *	lower case and number
 */

#if	defined(_WIN32) || defined(_WIN64)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<dirent.h>

#if	!defined( S_ISDIR )
#if	!defined( S_IFMT )
#define	S_IFMT	0xF000
#endif
#define	S_ISDIR( mode )		(((mode) & S_IFMT) == S_IFDIR)
#endif

#if	!defined( S_ISREG )
#define	S_ISREG( mode )		((mode) & S_IFREG)
#endif

extern	int	main( int, char *argv[] );
static	void	usage( void );
static	void	makefilename( char *buf, char *path, char *fname );
static	int	specialname( char *s );
static	void	smartconvert( char *s );
static	int	lowerpath( char *path );
static	char *	realname( char *path );

static	int	recursedir = 0;
static	int	verbose	   = 0;
static	int	forceall   = 0;
static	int	testonly   = 0;

static	struct {
	char	*upper;
	char	*special;
} specialtab[] = {
	{ "MAKEFILE",		"Makefile"	},
	{ "COPYING",		"COPYING"	},
	{ "LICENSE",		"LICENSE"	},
	{ "READ.ME",		"READ.ME"	},
	{ "README",		"README"	},
	{ "README.1ST",		"README.1ST"	},
	{ "LISEZMOI",		"LISEZMOI"	},
	{ "LISEZ.MOI",		"LISEZ.MOI"	}
};

enum { SPECIAL_NOTFOUND, SPECIAL_FOUND, SPECIAL_SAMEFILE };

static	int
specialname( char *s )
{
	int	i;

	for( i = 0 ; i < (int)(sizeof(specialtab)/sizeof(specialtab[0])) ; i++ )
		if( strcmp( s, specialtab[ i ].upper ) == 0 ) {
			if( strcmp( s, specialtab[ i ].special ) == 0 )
				return( SPECIAL_SAMEFILE );
			else	{
				(void)strcpy( s, specialtab[ i ].special );
				return( SPECIAL_FOUND );
			}
		}

	return( SPECIAL_NOTFOUND );
}

static	void
makefilename( char *buf, char *path, char *fname )
{
	(void)strcpy( buf, path  );
	(void)strcat( buf, "\\"  );
	(void)strcat( buf, fname );
}

static	void
smartconvert( char *s )
{
	char	*p;
	char	*fname;
	int	c;

	p = strrchr( s, (int)'\\' );

	if( p )
		p++;
	else	p = s;

	fname = p;

	if( forceall == 0 ) {
		while( (c = (int)*p++) != 0 )
			if( (isalpha( c ) && islower( c )) || ((c&0x80) != 0) )
				/*
				 * lower char and extended ASCII
				 */
				return;

		switch( specialname( fname ) ) {
		case SPECIAL_NOTFOUND :
			break;
		case SPECIAL_FOUND    :
			if( verbose != 0 )
				(void)printf( "  convert to %s\n", s );
			if( testonly == 0)
				(void)rename( s, s );
			return;
		case SPECIAL_SAMEFILE :
			return;
		}
	}
	
	(void)strlwr( fname );
	if( !testonly )
		(void)rename( s, s );

	if( verbose )
		(void)printf( "  convert to %s\n", s );
}

static	int
lowerpath( char *path )
{
	DIR	*dirp;
	struct	dirent	*dp;
	struct	stat	stb;
	char	buf[ FILENAME_MAX ];

	smartconvert( path );

	if( !recursedir )
		return( 0 );

	if( stat( path, &stb ) != 0 )
		return( -1 );

	if( S_ISDIR( stb.st_mode ) == 0 )
		return( 0 );

	if( verbose )
		(void)printf( "recurse into %s\n", path );

	dirp = opendir( path );

	if( !dirp ) {
		(void)fprintf( stderr, "Can't open path %s.\n", path );
		return( -1 );
	}

	while( (dp = readdir( dirp )) != NULL ) {
		if( strcmp( dp->d_name, "."  ) == 0 ||
		    strcmp( dp->d_name, ".." ) == 0 )
			continue;

		makefilename( (char *)&buf[0], path, dp->d_name );

		if( stat( (char*)&buf[0], &stb ) == 0 ) {
			if( S_ISREG( stb.st_mode ) || S_ISDIR( stb.st_mode ) ) {
				smartconvert( buf );

				if( S_ISDIR( stb.st_mode ) && recursedir )
					(void)lowerpath( buf );
			}
		}
	}

	(void)closedir( dirp );

	return( 0 );
}

static	void
usage()
{
	(void)fprintf(stderr, "lowerdir (c) 1996-2001 by C. Jullien\n");
	(void)fprintf(stderr, "Usage:\n");
	(void)fprintf(stderr, "lowerdir [-r] [-v] [-f] [-t] path1 .. pathN\n");
	(void)fprintf(stderr, "  -r recurse into directory.\n");
	(void)fprintf(stderr, "  -v verbose mode.\n");
	(void)fprintf(stderr, "  -f force every file to lower only.\n");
	(void)fprintf(stderr, "  -t test only (set -v also).\n");
}

static char *
realname( char *fname )
{
	LPTSTR		s;
	WIN32_FIND_DATA	data;
	HANDLE		hFind;
	char		rname[ FILENAME_MAX ];

	if( GetFullPathName( (LPCTSTR)fname, FILENAME_MAX, rname, &s ) == 0 )
		return( fname );

	if( (hFind = FindFirstFile( rname, &data )) != INVALID_HANDLE_VALUE ) {
		(void)strcpy( fname, data.cFileName );
		FindClose( hFind );
	}

	return( fname );
}

int
main( argc, argv )
int	argc;
char	*argv[];
{
	if( argc == 1 ) {
		usage();
		return( -1 );
	}

	while( --argc > 0 && *(*++argv) == '-' )
		while( *++(*argv) != 0 )
			switch( **argv ) {
			case 'r' :
				recursedir++;
				break;
			case 'f' :
				forceall++;
				break;
			case 'v' :
				verbose++;
				break;
			case 't' :
				testonly++;
				verbose++;
				break;
			default:
				usage();
				return( -1 );
		}

	if( argc <= 0 ) {
		usage();
		return( -1 );
	}

	while( argc-- > 0 )
		(void)lowerpath( realname( *argv++ ) );

	return( 0 );
}
