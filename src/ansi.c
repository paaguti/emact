#if	!defined( lint ) && defined( _ANSITERM )
static	char rcsid[] = "$Id: ansi.c,v 1.3 2008/11/29 08:57:37 jullien Exp $";
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
 *	The  routines  in  this  file  provide support for ANSI style
 *	terminals  over  a  serial  line  or on any system compatible
 *	with with ANSI Escape Sequences.
 */

#include	"emacs.h"

#if	defined( _ANSITERM )

#if	defined( _DOS )
#define	NROW	25
#else
#define NROW	24
#endif

#define NCOL	80
#define BEL	0x07
#define ESC	0x1B

static	void	_define(ansimove,(int row, int col));
static	void	_define(ansieeol,(void));
static	void	_define(ansieeop,(void));
static	void	_define(ansibeep,(void));
static	void	_define(ansisi,(void));
static	void	_define(ansiei,(void));
static	void	_define(ansiopen,(void));
static	void	_define(ansiclose,(void));
static	int	_define(ansicheck,(void));
static	int	_define(ansirawmode,(void));

/*
 *	Standard  terminal  interface  dispatch  table.  Some  fields
 *	point into "termio" code.
 */

TERM	term	= {
	NROW-1,
	NCOL,
	0,
	NIL,
	ansiopen,
	ansiclose,
	ttgetc,
	ttputc,
	ttputs,
	ttflush,
	ansimove,
	ansieeol,
	ansieeop,
	ansibeep,
	ansisi,
	ansiei,
	ttcshow,
	ansicheck,
	ansirawmode
};

static	void
ESCstrg( EMCHAR *s )
{
	ttputc( ESC );
	while( *s )
		ttputc( *s++ );
}

static	void
ansiparm( int n )
{
	int	q = n/10;

	if( q )
		ansiparm(q);
	ttputc((n%10) + '0');
}

static	void
ansimove( int row, int col )
{
	ESCstrg( "[" );
	ansiparm(row+1);
	ttputc(';');
	ansiparm(col+1);
	ttputc('H');
}

static	void
ansieeol( void )
{
	ESCstrg( "[K" );
}

static	void
ansieeop( void )
{
	ESCstrg( "[J" );
}

static	void
ansibeep( void )
{
	ttputc( BEL );
	ttflush();
}

static	void
ansisi( void )
{
	ESCstrg( "[7m" );
	ttflush();
}

static	void
ansiei( void )
{
	ESCstrg( "[0m" );
	ttflush();
}

static	void
ansiopen( void )
{
#if	defined( _DOS )
	static char outbuf[ BUFSIZ ];
	setbuf( stdout, outbuf );
	setraw();
#endif
#if	defined( _UNIX )
	char	*cp;

	if( (cp = getenv( "TERM" )) == NULL ) {
		(void)puts( "Shell variable TERM not defined!" );
		exit(1);
	}
	if( strcmp( cp, "vt100" ) ) {
		(void)puts( "Terminal type not 'vt100'!" );
		exit(1);
	}
#endif
	ttopen();
	TTYinit = T;
}

static	void
ansiclose( void )
{
	ttclose();
	restraw();
	TTYinit = NIL;
}

static	int
ansicheck( void )
{
	return( 0 );
}

static	void
ansirawmode( void )
{
}

#endif
