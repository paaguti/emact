#if	!defined( lint )
static char *sccsid = "@(#)grep.c	(c) Christian Jullien 2009/05/02";
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
 * grep -- print lines matching (or not matching) a pattern
 */

/*
 * Global Regular Expression Parser.
 */

#if	defined(_WIN32) || defined(_WIN64)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

#include	<stdio.h>
#include	<ctype.h>

#if	defined( _RGREP )
#include	<sys/stat.h>
#include	<dirent.h>
#include	"fnmatch.h"

#if	!defined( S_ISDIR )
#define	S_ISDIR( mode )		(((mode) & S_IFMT) == S_IFDIR)
#endif

#if	!defined( S_ISREG )
#define	S_ISREG( mode )		((mode) & S_IFREG)
#endif
#endif

#include	<stdlib.h>
#include	<string.h>

extern	int	main( int argc, char *argv[] );
static	void	compile( char *astr );
static	int	same( int a, int b );
static	void	execute( char *file );
static	void	rexecute( char *pattern, char *d );
static	int	advance( char *alp, char *aep );
static	int	cclass( char *aset, int ac, int af );
static	void	succeed( char *f );
static	char	*unquote( char *file );
#if	defined( LETTERP )
static	int	letterp( int c );
#endif
static	void	usage( void );

#if	defined( LETTERP )
static	int
letterp( int c )
{
	if( (c >= 'a') && (c <= 'z') )
		return( c );
	if( (c >= 'A') && (c <= 'Z') )
		return( c + 'a' - 'A' );
	return( 0 );
}
#else
#define	letterp( c )	(isalpha( c ) || (c) == '_')
#endif

#define	digitp( c )	isdigit( (int)c )
#define	uletterp( c )	(letterp(c) || (c) == '_')

#define	CCHR		2
#define	CDOT		4
#define	CCL		6
#define	NCCL		8
#define	CDOL		10
#define	CEOF		11

#define	CBRC		14
#define	CLET		15
#define	STAR		1

#define	BUFFERLEN	(4*BUFSIZ)

#define	LBSIZE		BUFFERLEN
#define	ESIZE		256

/*
 *	Flags
 */

static	int	cflag;		/* print matching files	*/
static	int	hflag;		/* print only lines	*/
static	int	iflag;		/* ignore case		*/
static	int	lflag;		/* print matching files	*/
static	int	nflag;		/* print line number	*/
static	int	qflag;		/* quiet mode		*/
#if	defined( _RGREP )
static	int	rflag;		/* recurse directory	*/
#endif
static	int	sflag;		/* silent mode (no err)	*/
static	int	vflag;		/* print unmatched lines*/
static	int	wflag;		/* match complete word	*/

static	char	expbuf[ ESIZE+1 ];
static	char	linebuf[ LBSIZE+1 ];
static	long	lnum;
static	int	nfile;		/* more than one file	*/

static	int	nsucc;
static	int	circf;
static	char	ibuf[ BUFFERLEN ];
static	long	tln;

#define	VERSION	"grep version 2.3, Copyright (c) 1993-2008 C. Jullien\n"

int
main( int argc, char *argv[] )
{
	char	obuf[ BUFFERLEN ];

	setbuf( stdout, obuf );

	while( --argc > 0 && (++argv)[0][0]=='-' ) {
		char *cp = argv[0] + 1;

		while( *cp ) switch( *cp++ ) {

		case 'V':
			(void)printf( VERSION );
			return( 0 );

#if	defined( _RGREP )
		case 'R':
		case 'r':
			rflag++;
			continue;
#endif

		case 'v':
			vflag++;
			continue;

		case 'h':
			hflag++;
			continue;

		case 'i':
			iflag++;
			continue;

		case 'l':
			lflag++;
			cflag++;
			continue;

		case 'c':
			cflag++;
			continue;

		case 'w':
			wflag++;
			continue;

		case 's':
			sflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'q':
			qflag++;
			continue;

		case 'e':
			--argc;
			++argv;
			goto out;

		default:
			usage();
			return( -1 );
		}
	}
out:

	if( argc <= 0 )
		return( 2 );

	compile( *argv );

	nfile = --argc;

	if( argc <= 0 ) {
		if( lflag )
			return( 1 );
		execute( NULL );
	} else	while( --argc >= 0 ) {
			argv++;
#if	defined( _RGREP )
			if( rflag ) {
				nfile = 100;
				rexecute( unquote( *argv ), "." );
			} else	execute( *argv );
#else
			execute( *argv );
#endif
	}

	fflush( stdout );

	return( nsucc == 0 );
}

static	void
compile( char *astr )
{
	int	c;
	int	cclcnt;
	char	*ep;
	char	*sp;
	char	*lastep;

	ep	= expbuf;
	sp	= astr;
	lastep	= NULL;

	if( *sp == '^' ) {
		circf++;
		sp++;
	}

	if( wflag )
		*ep++ = CBRC;

	for( ;; ) {
		if( ep >= &expbuf[ ESIZE ] )
			goto cerror;

		if( (c = *sp++) != '*' )
			lastep = ep;

		switch( c ) {

		case '\0':
			if( wflag )
				*ep++ = CLET;
			*ep = CEOF;
			return;

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if( lastep == 0 )
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if( *sp != '\0' )
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			*ep++  = CCL;
			*ep++  = 0;
			cclcnt = 1;
			if( (c = *sp++) == '^' ) {
				c = *sp++;
				ep[-2] = NCCL;
			}
			do	{
				*ep++ = (char)c;
				cclcnt++;
				if( c=='\0' || ep >= &expbuf[ ESIZE ] )
					goto cerror;
			} while( (c = *sp++) != ']' );
			lastep[1] = (char)cclcnt;
			continue;

		case '\\':
			if( (c = *sp++) == '\0' )
				goto cerror;
			if( c == '<' ) {
				*ep++ = CBRC;
				continue;
			}
			if( c == '>' ) {
				*ep++ = CLET;
				continue;
			}
defchar:
		default:
			*ep++ = CCHR;
			*ep++ = (char)c;
		}
	}
cerror:
	(void)fprintf( stderr, "RE error\n" );
}

static	int
same( int a, int b )
{
     return( (a == b) || (iflag && (a ^ b) == ' ' && letterp(a)==letterp(b)) );
}

#if	defined( _RGREP )

#if	!defined( NFILEN )
#define	NFILEN	256
#endif

static void
rexecute( char *pattern, char *d )
{
	DIR	*dirp;
	struct	dirent	*dp;
	char	buf[ NFILEN ];
	struct	stat stb;

	dirp = opendir( d );

	if( !dirp ) {
		(void)printf( "No such directory: %s", d );
		return;
	}

	while( (dp = readdir( dirp )) != NULL ) {
		(void)sprintf( buf, "%s/%s", d, dp->d_name );

		if( stat( (char *)buf, &stb ) != 0 ) {
			(void)perror( buf );
		} else if( S_ISREG( stb.st_mode ) ) {
			if( fnmatch( pattern, dp->d_name, 0 ) == 0 ) {
				execute( buf );
			}
		} else if( S_ISDIR( stb.st_mode ) ) {
			if( !strcmp(dp->d_name,".")||!strcmp(dp->d_name,"..") )
				continue;

			rexecute( pattern, buf );
		}
	}

	closedir( dirp );
}
#endif

static	void
execute( char *file )
{
	char	 *p1;
	char	 *p2;
	char	 *ebp;
	char	 *cbp;
	FILE	 *fd;
	int	 c;

	if( file != NULL ) {
#if	defined( MSDOS )
		file = strlwr( file );
#endif
		if( (fd = fopen( file, "r" ) ) == NULL ) {
			if( !sflag )
				(void)perror( file );
			return;
		}
	} else	fd = stdin;

	ebp   = ibuf;
	cbp   = ibuf;
	lnum  = 0;
	tln   = 0;

	for( ;; ) {
		lnum++;
		if( (lnum&0377) == 0 )
			fflush( stdout );
		p1 = linebuf;
		p2 = cbp;
		for( ;; ) {
		     if( p2 >= ebp ) {
		     	 if( (c = (int)fread( ibuf, 1, BUFFERLEN, fd )) <= 0 ) {
			     fclose( fd );
			     if( cflag ) {
			     	 if( lflag ) {
				     if( tln )
				     	 (void)printf( "%s\n", file );
				 } else	{
				 	if( nfile > 1 && hflag == 0 )
						(void)printf( "%s:", file );
					(void)printf( "%ld\n", tln );
				 }
			     }
			     return;
			 }
			 p2  = ibuf;
			 ebp = ibuf+c;
		     }

		     if( (c = *p2++) == '\n' )
		     	 break;

		     if( c && (p1 < &linebuf[LBSIZE-1]) )
		     	 *p1++ = (char)c;
		}

		*p1   = 0;
		cbp   = p2;
		p1    = linebuf;
		p2    = expbuf;

		if( circf ) {
			if( advance( p1, p2 ) )
				goto found;
			goto nfound;
		}

		/*
		 * fast check for first character
		 */

		if( *p2 == CCHR ) {
			c = p2[1];
			do	{
				if( (*p1 != c) && (!iflag || (c ^ *p1) != ' '
				   || letterp((int)c) != letterp((int)*p1)))
					continue;
				if( advance( p1, p2 ) )
					goto found;
			} while( *p1++ );
			goto nfound;
		}

		/*
		 * regular algorithm
		 */

		do	{
			if( advance( p1, p2 ) )
				goto found;
		} while( *p1++ );
nfound:
		if( vflag )
			succeed( file );
		continue;
found:
		if( vflag == 0 )
			succeed( file );
	}
}

static	int
advance( char *alp, char *aep )
{
	char *lp;
	char *ep;
	char *curlp;

	lp = alp;
	ep = aep;

	for( ;; )
		switch( *ep++ ) {

		case CCHR:
			if( !same( *ep, *lp ) )
				return( 0 );
			ep++;
			lp++;
			continue;

		case CDOT:
			if( *lp++ )
				continue;
			return( 0 );

		case CDOL:
			if( *lp == 0 )
				continue;
			return( 0 );

		case CEOF:
			return( 1 );

		case CCL:
			if( cclass( ep, *lp++, 1 ) ) {
				ep += *ep;
				continue;
			}
			return( 0 );

		case NCCL:
			if( cclass( ep, *lp++, 0 ) ) {
				ep += *ep;
				continue;
			}
			return( 0 );

		case CDOT|STAR:
			curlp = lp;
			while( *lp++ )
				;
			goto star;

		case CCHR|STAR:
			curlp = lp;
			while( same( *lp, *ep ) )
				lp++;
			lp++;
			ep++;
			goto star;

		case CCL|STAR:
		case NCCL|STAR:
			curlp = lp;
			while( cclass(ep, *lp++, ep[-1]==(CCL|STAR)) )
				;
			ep += *ep;
			goto star;

		case CBRC:
			if( lp == expbuf )
				continue;
			if(   (uletterp( (int)*lp ) || digitp( (int)*lp ))
			   && !uletterp( (int)lp[-1] )
			   && !digitp( (int)lp[-1]) )
			   	continue;
			return( 0 );

		case CLET:
			if( !uletterp( (int)*lp ) && !digitp( (int)*lp ) )
				continue;
			return( 0 );

		default:
			(void)fprintf( stderr, "RE botch\n" );
			return( 1 );
		}

star:
	do	{
		lp--;
		if( advance( lp, ep ) )
			return( 1 );
	} while( lp > curlp );

	return( 0 );

}

static	int
cclass( char *aset, int ac, int af )
{
	char *	set;
	char	c;
	int	n;

	if( (c = (char)ac) == 0 )
		return( 0 );

	set = aset;
	n   = *set++;

	while( --n != 0 )
		if( n > 2 && set[1] == '-' ) {
			if( c >= set[0] && c <= set[2] )
				return( af );
			set += 3;
			n   -= 2;
		} else	if( *set++ == c )
				return( af );
	return( !af );
}

static	void
succeed( char *f )
{
	nsucc = 1;

	if( sflag || qflag )
		return;

	if( cflag ) {
		tln++;
		return;
	}

	if( nfile > 1 && hflag == 0 )
		(void)printf( "%s:", f );
	if( nflag )
		(void)printf( "%ld:", lnum );
	(void)printf( "%s\n", linebuf );
}

static char *
unquote( char *fname )
{
	if( fname[0] == '\'' && fname[strlen(fname) - 1] == '\'') {
		fname[strlen(fname) - 1] = '\000';
		return( &fname[1] );
	} else	{
		return fname;
	}
}

static void
usage( void )
{
	char	*help[] = {
#if	defined( _RGREP )
		"Usage: grep [-cehilnqrRsVvw] PATTERN file ..",
#else
		"Usage: grep [-cehilnqsVvw] PATTERN file ..",
#endif
		"  -c       only print a count of matching lines per FILE",
		"  -e       use PATTERN as a regular expression",
		"  -h       suppress the prefixing filename on output",
		"  -i       ignore case distinctions",
		"  -l       only print FILE names containing matches",
		"  -n       print line number with output lines",
		"  -q       suppress all normal output",
#if	defined( _RGREP )
		"  -r, -R,  recurse directories",
#endif
		"  -s       suppress error messages",
		"  -V       print version information and exit",
		"  -v       select non-matching lines",
		"  -w       force PATTERN to match only whole words",
		NULL
	};
	int	i;

	for( i = 0 ; help[ i ] ; ++ i )
		fprintf( stderr, "%s\n", help[ i ] );

}
