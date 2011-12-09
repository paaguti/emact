#if	!defined( lint )
static	char rcsid[] = "$Id: nexterr.c,v 1.4 2008/06/19 12:13:31 jullien Exp $";
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
 *	nexterr.c :	Find the next error after a compilation.
 */

#include	"emacs.h"

static  int	_define(geterror,(void));

static  int	errlinenum = 0;
static  EMCHAR	errfname[ NFILEN ];

static	int
geterror( void )
{
	int	eol;
	int	i;
	int	j;
	int	c;
	int	start;
	int	stop;
	EMCHAR	save[ NPAT ];

	(void)emstrcpy( save, search_buffer );
	(void)emstrcpy( search_buffer, ECSTR(":") );

	for( ;; ) {
		errlinenum = 0;

		if( ffindstring() != T ) {
			if( errflag == T )
				WDGmessage( ECSTR("no more errors") );
			else	WDGmessage( ECSTR("no errors") );
			(void)emstrcpy( search_buffer, save );
			return( NIL );
		}

		curwp->w_dotp = found_p;
		curwp->w_doto = 0;
		eol	      = llength( found_p );

		/*
		 *	search for last 'xx.yy'
		 *	(assumes a filename with extension)
		 */

		i = -1; /* '.' position in line */

		for( j = 1 ; j < (eol-1) ; j++ ) {
		   c = lgetc( found_p, j );
		   if( c != '.' )
		       continue;
		   if( i != -1 && (separatorp( c ) || c == ':') ) {
		       /*
			* stop at the first separator when at least one '.'
			* is found.
			*/
		       break;
		   }
		   if( charp(lgetc(found_p,j-1)) && charp(lgetc(found_p,j+1) ) )
		       /*
			* remember  the  postion  of  '.'  and try to
			* find another occurence.  Complete file path
			* may contain more than one '.' as in:
			* /foo/bar-2.4.15/gee.c
			*/
		       i = j;
		}

		if( i == -1 )
			continue;	/* found end of line */

		/*
		 *	find the start of the word.
		 */

		for( start = i ; start >= 0 ; start-- ) {
			c = lgetc( found_p, start );

			if( c == '/' || c == '.' )
				continue;

			if( separatorp( c ) || c == '"' || c == '\'' )
				break;
		}

		start++;

		/*
		 *	find the end of the word.
		 */

		for( stop = i+1 ; stop < eol ; stop++ )
			if( !charp( lgetc( found_p, stop ) ) )
				break;

		/*
		 *	get the filename in errfname buffer
		 */

		for( j = 0 ; (start + j) < stop ; j++ )
			errfname[ j ] = (EMCHAR)lgetc( found_p, (start+j) );

		errfname[ j ] = '\0';

		/*
		 *	search for a number (assumes line number).
		 */

		i = curwp->w_doto;

		do	{
			if( i == start )
				i = stop;
			c = lgetc( found_p, ++i );
		} while( !(c >= '0' && c <= '9') && i < eol );

		/*
		 *	get the line number in errlinenum.
		 */

		while( c >= '0' && c <= '9' && i < eol ) {
			errlinenum = errlinenum * 10 + (c - '0');
			c = lgetc( found_p, ++i );
		}

		curwp->w_dotp = found_p;
		curwp->w_doto = eol;

		if( errlinenum == 0 )
			continue;

		errflag = T;
		break;
	}

	(void)emstrcpy( search_buffer, save );
	return( errlinenum ? T : NIL );
}

CMD
nexterror( void )
{
	WINSCR	*owp = curwp;
	WINSCR	*wp;
	BUFFER	*bp;
	int	n;
	int	save;

	if( (bp = bfind( BUF_PROC, T, NIL, FUNDAMENTAL )) == NULL )
		return( NIL );
	else	wp = showbuffer( bp );

	(void)setcurrentwindow( wp );
	n = geterror();
	(void)setcurrentwindow( owp );

	if( n == T ) {
		if( newfile( errfname ) == NIL )
			return( NIL );
		save	= repeat;
		repeat	= errlinenum;
		(void)gotoline();
		repeat	= save;
		WDGwrite( ECSTR("%L"), found_p );
	}

	return( T );
}
