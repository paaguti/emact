#if	!defined( lint )
static	char *sccsid = "@(#)diffh.c	(c) C. Jullien 2009/05/02";
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
 *	diffh.c :	
 */

#if	defined(_WIN32) || defined(_WIN64)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#if	defined( __STDC__ ) || defined( _ISO )
#include	<stdlib.h>

extern	int	main( int, char *argv[] );
extern	char *	getl( int f, long n );
extern	void	clrl( int f, long n );
extern	void	movstr( char *s, char *t );
extern	int	easysynch( void );
extern	int	output( int a, int b );
extern	void	change( long a, int b, long c, int d, char *s );
extern	void	range( long a, int b );
extern	int	cmp( char *s, char *t );
extern	FILE *	dopen( char *f1, char *f2 );
extern	void	progerr( char *s );
extern	void	error( char *s, char *t );
extern	int	hardsynch( void );

#else
extern	char	*malloc();

extern	int	main();
extern	char *	getl();
extern	void	clrl();
extern	void	movstr();
extern	int	easysynch();
extern	int	output();
extern	void	change();
extern	void	range();
extern	int	cmp();
extern	struct _iobuf *dopen();
extern	void	progerr();
extern	void	error();
extern	int	hardsynch();

#endif

#if	!defined( S_IFMT )
#define S_IFMT	0170000	/* type of file			*/
#endif

#define C	3
#define RANGE	30
#define LEN	255
#define INF	16384

char *	text[2][RANGE];
long	lineno[2] = {1, 1};	/* no. of 1st stored line in each file	*/
int	ntext[2];		/* number of stored lines in each	*/
long	n0;
long	n1;			/* scan pointer in each			*/
int	bflag;
int	debug = 0;
FILE *	file[2];

#if	defined( _ISO ) || defined( __STDC__ )
char	_sobuf[ BUFSIZ ];
#else
extern	char _sobuf[];
#endif


/* return pointer to line n of file f*/

char	*
getl( f, n )
int	f;
long	n;
{
	register char *t;
	register int delta;
	register int nt;
again:
	delta = (int)(n - lineno[f]);
	nt = ntext[f];
	if( delta < 0 )
		progerr( "1" );
	if( delta < nt )
		return(text[f][delta]);
	if( delta > nt )
		progerr( "2" );
	if( nt >= RANGE )
		progerr( "3" );
	if( feof( file[f] ) )
		return( NULL );

	t = text[f][nt];

	if( t == 0 ) {
		t = text[f][nt] = (char *)malloc( LEN+1 );
		if( t == NULL ) {
			if( hardsynch() )
				goto again;
			else	progerr( "5" );
		}
	}

	t = fgets( t, LEN, file[f] );

	if( t != NULL )
		ntext[ f ]++;

	return( t );
}

/*
 * remove thru line n of file f from storage
 */

void
clrl(f,n)
int	f;
long	n;
{
	register int i;
	register int j;

	j = (int)(n-lineno[f]+1);

	for( i = 0 ; i+j<ntext[f] ; i++ )
		movstr( text[f][i+j], text[f][i] );

	lineno[f] = n+1;
	ntext[f] -= j;
}

void
movstr( s, t )
register char *s;
register char *t;
{
	while( (*t++ = *s++) != 0 )
		;
}

int
main( argc, argv )
int	argc;
char	**argv;
{
	char	*s0;
	char	*s1;

	setbuf( stdout, _sobuf );
	while( *argv[1] == '-' ) {
		argc--;
		argv++;
		while( *++argv[0] )
			if( *argv[0] == 'b' )
				bflag++;
	}
	if( argc != 3 )
		error( "must have 2 file arguments", "" );
	file[0] = dopen( argv[1], argv[2] );
	file[1] = dopen( argv[2], argv[1] );

	for( ;; ) {
		s0 = getl( 0, ++n0 );
		s1 = getl( 1, ++n1 );
		if( s0 == NULL || s1 == NULL )
			break;
		if( cmp( s0, s1 ) != 0 ) {
			if( !easysynch() && !hardsynch() )
				progerr( "5" );
		} else	{
			clrl( 0, n0 );
			clrl( 1, n1 );
		}
	}

	if( s0 == NULL && s1 == NULL )
		return( 0 );
	if( s0 == NULL )
		output( -1, INF );
	if( s1 == NULL )
		output( INF, -1 );

	fflush( stdout );
	return( 0 );
}

/* synch on C successive matches*/

int
easysynch()
{
	int	i,j;
	int	k,m;
	char	*s0,*s1;

	for( i=j=1 ; i < RANGE && j < RANGE ; i++, j++ ) {
		s0 = getl( 0, n0+i );
		if( s0 == NULL )
			return( output( INF, INF ) );
		for( k = C-1 ; k < j ; k++ ) {
			for( m = 0 ; m < C ; m++ )
				if( cmp( getl( 0, n0+i-m ),
					getl( 1, n1+k-m )) != 0 )
					goto cont1;
			return( output( i-C, k-C ) );
cont1:
			;
		}
		s1 = getl( 1, n1+j );
		if( s1 == NULL )
			return( output( INF, INF ) );
		for( k = C-1 ; k <= i ; k++ ) {
			for( m = 0 ; m < C ; m++ )
				if( cmp( getl( 0, n0+k-m ),
					getl( 1, n1 + j-m )) != 0 )
					goto cont2;
			return( output( k-C, j-C) );
cont2:
			;
		}
	}

	return( 0 );
}

int
output( a, b )
int	a;
int	b;
{
	register int i;
	char	*s;

	if( a < 0 )
		change( n0-1, 0, n1, b, "a" );
	else	if( b < 0 )
		change( n0, a, n1-1, 0, "d" );
	else	change( n0, a, n1,   b, "c" );

	for( i = 0 ; i <= a ; i++ ) {
		s = getl( 0, n0+i );
		if( s== NULL )
			break;
		(void)printf( "< %s", s );
		clrl( 0, n0+i );
	}
	n0 += i-1;

	if( a>=0 && b>=0 )
		(void)printf( "---\n" );
	for( i = 0 ; i <= b ; i++ ) {
		s = getl( 1, n1+i );
		if( s == NULL )
			break;
		(void)printf( "> %s", s );
		clrl( 1, n1+i );
	}
	n1 += i-1;
	return( 1 );
}

void
change( a, b, c, d, s )
long	a;
int	b;
long	c;
int	d;
char	*s;
{
	range( a, b );
	(void)printf( "%s", s );
	range( c, d );
	(void)printf( "\n" );
}

void
range( a, b )
long	a;
int	b;
{
	if( b == INF )
		(void)printf( "%ld,$", a );
	else	if( b == 0 )
		(void)printf( "%ld", a );
	else	(void)printf( "%ld,%ld", a, a+b );
}

int
cmp( s, t )
char	*s;
char	*t;
{
	if( debug )
		(void)printf( "%s:%s\n", s, t );
	for( ;; ) {
		if( bflag && isspace( *s ) && isspace( *t ) ) {
			while( isspace( *++s ) )
				;
			while( isspace( *++t ) )
				;
		}
		if( *s!=*t || *s==0 )
			break;
		s++;
		t++;
	}
	return( *s - *t );
}

FILE *
dopen( f1, f2 )
char	*f1;
char	*f2;
{
	FILE	*f;
	char	b[ 256 ];
	char	*bptr;
	char	*eptr;
	struct stat statbuf;

	if( cmp( f1, "-" ) == 0 ) {
		if( cmp( f2, "-" ) == 0 )
			error( "can't do - -", "" );
		else	return( stdin );
	}

	if( stat( f1, &statbuf ) == -1 )
		error( "can't access ", f1 );

	if( (statbuf.st_mode&S_IFMT)==S_IFDIR ) {
		for( bptr = (char *)&b[0] ; (*bptr = *f1++) != 0 ; bptr++ )
			;
		*bptr++ = '/';
		for( eptr = f2 ; *eptr; eptr++ )
			if( *eptr=='/' && eptr[1]!=0 &&eptr[1]!='/' )
				f2 = eptr+1;
		while( (*bptr++ = *f2++) != 0 )
			;
		f1 = b;
	}

	f = fopen( f1, "r" );

	if( f == NULL )
		error( "can't open", f1 );

	return( f );
}

void
progerr( s )
char	*s;
{
	error( "program error ", s );
}

void
error( s, t )
char	*s;
char	*t;
{
	(void)fprintf( stderr, "diffh: %s%s\n", s, t );
	exit( 1 );
}

/*
 * stub for resychronization beyond limits of text buf
 */

int
hardsynch()
{
	change( n0, INF, n1, INF, "c" );
	(void)printf( "---change record omitted\n" );
	error( "can't resynchronize", "" );
	return( 0 );
}
