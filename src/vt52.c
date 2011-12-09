#if	!defined( lint )
static	char rcsid[] = "$Id: vt52.c,v 1.3 2008/11/29 08:57:38 jullien Exp $";
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
 *	Routines for VT52 terminals.
 */

#include	"emacs.h"

#if	defined( _VT52 )

static	void	vt52move(int row, int col);
static	void	vt52eeol(void);
static	void	vt52eeop(void);
static	void	vt52si(void);
static	void	vt52ei(void);
static	void	vt52beep(void);
static	int	vt52check(void);
static	void	vt52rawmode(void);

TERM	term	= {
	24-1,
	80,
	0,
	NIL,
	ttopen();
	ttclose,
	ttgetc,
	ttputc,
	ttputs,
	ttflush,
	vt52move,
	vt52eeol,
	vt52eeop,
	vt52beep,
	vt52si,
	vt52ei,
	ttcshow,
	vt52check,
	vt52rawmode
};

static	void
vt52move( int row, int col )
{
	ttputc( 0x1B );
	ttputc( 'Y' );
	ttputc( row+' ' );
	ttputc( col+' ' );
}

static	void
vt52eeol( void )
{
	ttputc( 0x1B );
	ttputc( 'K' );
}

static	void
vt52eeop( void )
{
	ttputc( 0x1B );
	ttputc( 'J' );
}

static	void
vt52beep( void )
{
	ttputc( 0x07 );
	ttflush();
}

static	void
vt52si( void )
{
	ttputc( 0x1B );
	ttputc( 'p' );
}

static	void
vt52ei( void )
{
	ttputc( 0x1B );
	ttputc( 'q' );
}

static	int
vt52check( void )
{
	return( 0 );
}

static	void
vt52rawmode( void )
{
}

#endif
