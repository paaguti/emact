#if	!defined( lint ) && defined( _EMACSLIB )
static	char rcsid[] = "$Id: llemacs.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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

#if	defined( _EMACSLIB )

#include	"emacs.h"

/*
 *	The  functions  in  this file implement commands that perform
 *	interactions between Emacs and other applications.
 */

static	EMCHAR	*call[] = {
	"",
	"",
};

#define	EVAL	0x100

static	EDLINE	*curline;
static	int	evalflag;

#define	EXIT		0
#define	EVALBUFFER	1
#define	EVALFUNCTION	2

/*
 *	Call emacs form an application with an optional filename.
 */

int
llemacs( EMCHAR *file )
{

	evalflag = EXIT;

	if( *file ) {
		call[ 1 ] = file;
		(void)emacs( 2, call );
	} else	(void)emacs( 1, (EMCHAR **)NULL );	

	return( evalflag );

}

/*
 *	Get an entire line from emacs buffer in 'buf' then point to
 *	the next line.
 */

int
llembol( EMCHAR *buf )
{
	int	i;

	if( curline == lastline( curbp ) )
		return( -1 );

	for( i = 0 ; i < llength( curline ) ; i++ )
		buf[ i ] = lgetc( curline, i );

	curline = lforw( curline );
	return( i );

}

/*
 *	Insert a line in emacs buffer.
 */

int
llemeol( EMCHAR *buf, int n )
{

	while( n-- && linsert( 1, *buf++ ) == T )
		;

	(void)endline();

}

/*
 *	Position eval flag before exit from emacs.
 */

CMD
lispevalbuffer()
{
	evalflag = EVALBUFFER;
	editflag = NIL;
	curline  = firstline( curbp );
	vttidy();
}

CMD
evalfunction()
{
	EDLINE	*clp = curwp->w_dotp;
	int	cbo  = curwp->w_doto;

	(void)blispexpr();

	evalflag	= EVALBUFFER;
	editflag	= NIL;
	curline		= curwp->w_dotp;
	curwp->w_dotp	= clp;
	curwp->w_doto	= cbo;

	vttidy();
}

#endif
