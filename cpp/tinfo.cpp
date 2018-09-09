#if	!defined( lint )
static	char rcsid[] = "$Id: tinfo.cpp,v 1.3 2018/09/02 17:48:50 jullien Exp $";
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
 *	The  routines  in  this  file  provide  support  for TERMINFO
 *	package.
 */

#include	"emacs.h"

#if	defined( _TERMINFO )

#include	<curses.h>
#include	<term.h>

static	void	tinfomove(int row, int col);
static	void 	tinfoeeol(void);
static	void 	tinfoeeop(void);
static	void	tinfoputs(EMCHAR *s, int n);
static	void	tinfobeep(void);
static	void	tinfoopen(void);
static	void	tinfoclose(void);
static	void	tinfosi(void);
static	void	tinfoei(void);
static	void	tinfocshow(int flag);
static	int	tinfocheck(void);
static	void	tinforawmode(void);

TERM	term = {
	0,
	0,
	NIL,
	tinfoopen,
	tinfoclose,
	ttgetc,
	ttputc,
	ttputs,
	ttflush,
	tinfomove,
	tinfoeeol,
	tinfoeeop,
	tinfobeep,
	tinfosi,
	tinfoei,
	tinfocshow,
	tinfocheck,
	tinforawmode
};

static	void
tinfoopen()
{
	static int fts;
	int	status;

	if( fts ) {
		(void)putp( enter_ca_mode );
#if	defined( COHERENT )
		(void)fixterm();
#else
		(void)reset_prog_mode();
#endif
		return;
	} else	fts++;

	setupterm( NULL, fileno( stdout ), &status );

	if( status != 1 ) {
		(void)puts( "Terminfo setup failed\n" );
		exit( 1 );
	}

	term.t_ncol = columns;
	term.t_nrow = lines - 1;

	(void)putp( enter_ca_mode );

	if( cursor_address == NULL ) {
		(void)puts( "Cursor address is not set in terminfo\n" );
		exit( 1 );
	}

	if( clear_screen == NULL ) {
		(void)puts( "Clear screen is not set in terminfo\n" );
		exit( 1 );
	}

	if( clr_eol == NULL ) {
		(void)puts( "Clear to end of line is not set in terminfo\n" );
		exit( 1 );
	}

	ttopen();
	TTYinit = NIL;
}

static	void
tinfoclose()
{
#if	defined( COHERENT )
	(void)resetterm();
#else
	(void)reset_shell_mode();
#endif
	(void)putp( exit_ca_mode );
	TTYinit = NIL;
}

static	void
tinfomove( int row, int col )
{
	(void)putp( tparm( cursor_address, row, col ) );
}

static	void
tinfoeeol()
{
	(void)putp( clr_eol );
}

/*
 *	Clear screen.
 */

static	void
tinfoeeop()
{
	(void)putp( clear_screen );
}

static	void
tinfobeep()
{
	(void)putp( bell );
}

static	void
tinfosi()
{
	(void)putp( enter_standout_mode );
}

static	void
tinfoei()
{
	(void)putp( exit_standout_mode );
}

static	void
tinfoputs( EMCHAR *s, int n )
{
	while (n-- > 0)
		(void)putchar( *s++ );
}

static	void
tinfocshow( flag )
int	flag;
{
	(void)ttflush();
}

static	int
tinfocheck()
{
	return( 0 );
}

static	void
tinforawmode()
{
}

#endif
