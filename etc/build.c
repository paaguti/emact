#if	!defined( lint )
static	char *sccsid = "@(#)build.c	(c) C. Jullien 2009/05/02";
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
 *	build.c :	
 */

#if	defined(_WIN32) || defined(_WIN64)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern	int	main( int, char *argv[] );

int
main( argc, argv )
int	argc;
char	*argv[];
{
	FILE	  *fd;
	struct tm *nt;
	time_t	  tb;
	char	  date[ 32 ];
	char	  *buildcnt;
	char	  *buildh;
	int	  build;

	if( argc != 3 ) {
		fprintf( stderr, "build: build.cnt build.h\n" );
		return( 1 );
	}

	buildcnt = argv[ 1 ];
	buildh   = argv[ 2 ];

	fd = fopen( buildcnt, "r" );

	if( fd == NULL ) {
		fprintf( stderr, "build: file not found: %s.\n", buildcnt );
		return( 1 );
	}

	fscanf(fd, "%d", &build);
	fclose( fd );

	fd = fopen( buildcnt, "w" );
	fprintf( fd, "%d\n", ++build );
	fclose( fd );

	(void)time( &tb );
	nt = localtime( &tb );

	fd = fopen( buildh, "w" );

	if( fd == NULL ) {
		fprintf( stderr, "build: can't open file %s.\n", buildh );
		return( 1 );
	}

	(void)sprintf( date,
		       "%04d/%02d/%02d",
		       nt->tm_year + 1900,
		       nt->tm_mon  + 1,
		       nt->tm_mday
		     );

	fprintf( fd, "/*\n" );
	fprintf( fd, " static char *sccsid = " );
	fprintf( fd, "\"@(#)build.h  (c) C. Jullien %s\";\n", date );
	fprintf( fd, " */\n" );
	fprintf( fd, "\n" );
	fprintf( fd, "#if	!defined( __EMACS_BUILD_H )\n" );
	fprintf( fd, "#define	__EMACS_BUILD_H\n" );
	fprintf( fd, "\n" );
	fprintf( fd, "/*\n" );
	fprintf( fd, " *	build.h :\n" );
	fprintf( fd, " */\n" );
	fprintf( fd, "\n" );
	fprintf( fd, "#if	!defined( EMBUILD )\n" );
	fprintf( fd, "#define	EMBUILD		%d\n", build );
	fprintf( fd, "#endif\n" );
	fprintf( fd, "\n" );
	fprintf( fd, "#endif\n" );
	fclose( fd );

	return( 0 );
}

