#if	!defined( lint )
static	char rcsid[] = "$Id: newterm.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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
 *	The  routines  in  this  file  provide  support  for  NEWTERM
 *	package.  More  seriously,  this  file  should  be  seen as a
 *	template to port EmACT on a brand new system.  Before hacking
 *	around you  MUST  rename  this file to a maximum 8.3 filename
 *	that  corespond  to  the  system/terminal you are running on.
 *	The  same way you MUST change the prefix NEW to another short
 *	prefix  that better match yor system.  For example,  on a new
 *	system named BUGIX,  this file could be named 'bugterm.c' and
 *	all 'NEWxxx' functions/variables changed to 'BUGxxx'.
 *
 *	Rename  also  makefile.new to makefile.bug to compile on this
 *	system.  Good  luke  and  please  don't forget to send me the
 *	modifications you have made.
 */

#if	defined( _NEWTERM )

#include	"emacs.h"

static	void	_define(NEWopen,(void));
static	void	_define(NEWclose,(void));
static	int	_define(NEWgetc,(void));
static	void	_define(NEWputc,(int c));
static	void	_define(NEWputs,(EMCHAR *s, int n));
static	void	_define(NEWmove,(int row, int col));
static	void	_define(NEWeeol,(void));
static	void	_define(NEWeeop,(void));
static	void	_define(NEWflush,(void));
static	void	_define(NEWbeep,(void));
static	void	_define(NEWei,(void));
static	void	_define(NEWsi,(void));
static	void	_define(NEWcshow,(int flag));
static	int	_define(NEWcheck,(void));
static	void	_define(NEWrawmode,(void));

TERM	term = {
	0,
	0,
	0,
	NIL,
	NEWopen,
	NEWclose,
	NEWgetc,
	NEWputc,
	NEWputs,
	NEWflush,
	NEWmove,
	NEWeeol,
	NEWeeop,
	NEWbeep,
	NEWsi,
	NEWei,
	NEWcshow,
	NEWcheck,
	NEWrawmode
};

static	int	NEWrow;		/* current cursor row position		*/
static	int	NEWcol;		/* current cursor col position		*/
static	EMCHAR*	NEWblank;	/* blank line (used by eol and eop)	*/

static	void
NEWopen( void )
{
	/*
	 *	MANDATORY.
	 *
	 *	Prepare  the  terminal  to  display  information.  On
	 *	graphic   systems,  it's  used  to  create  the  main
	 *	window,  load  font,  compute  the  the real terminal
	 *	size  in character units.  On text based systemd,  it
	 *	puts    the    terminal   in   rawmode.   The   basic
	 *	implementation  also  allocate  a blank line that can
	 *	be  used  with  the early port of NEWeeol and NEWeeop
	 *	functions.
	 */

	NEWrawmode();		/* put terminal in raw mode if needed	*/
	TTYncol = 80;		/* total number of columns		*/
	TTYnrow = 25-1;		/* total number of rows minus one	*/
	TTYinit = T;		/* terminal is initialized		*/

	/*
	 *	Allocate  and  initialize  a  blank  line  for  early
	 *	NEWeeol and NEWeeop functions.
	 */

	NEWblank = (EMCHAR *)malloc( TTYncol * sizeof( EMCHAR ) );

	for( i = 0 ; i < TTYncol ; i++ )
		NEWblank[ i ] = ' ';

	NEWrow	= 0;		/* initial cursor position is on row 0	*/
	NEWcol	= 0;		/* initial cursor position is on col 0	*/

}

static	void
NEWclose( void )
{
	/*
	 *	Free resources created by XXXopen().  It can be empty
	 *	for the first steps of the port.
	 */

	free( NEWblank );
	NEWblank = NULL;

	TTYinit  = NIL;		/* terminal is not initialized		*/
}

static	int
NEWgetc( void )
{
	/*
	 *	MANDATORY.
	 *
	 *	Wait for a valid character and,  eventually,  realize
	 *	the  mapping  beetwen  function  and  keypad keys and
	 *	EmACT keys.  You can see curses.c,  x11.c or ntterm.c
	 *	for  examples  of  such translation.  Char is read in
	 *	raw mode,  so : Ctrl-C,  Ctrl-Z,  Ctrl-Break,  ESC ..
	 *	are not interpreted but read as single characters.
	 */

	return( (int)fgetc( stdin ) );
}

static	void
NEWputc( int c )
{
	/*
	 *	MANDATORY.
	 *
	 *	Display  the  character  with  ASCII  code 'c' at the
	 *	current position (NEWcol, NEWrow).
	 */

	(void)fputc( c, stdout );
}

static	void
NEWputs( EMCHAR *s, int n )
{
	/*
	 *	Display  the  first  'n' characters of the string 's'
	 *	at  the  current  position.  It  could be improved by
	 *	system  functions that directly display a string of a
	 *	given length (a kind of DrawString ..).
	 */

	while( n-- > 0 )
		NEWputc( (int)*s++ );
}

static	void
NEWflush( void )
{
	/*
	 *	Flush  the  terminal to synchronize the display after
	 *	a change. It's often not needed by graphic systems.
	 */

	(void)fflush( stdout );
}

static	void
NEWmove( int col, int row )
{
	/*
	 *	MANDATORY.
	 *
	 * 	Move  the  cursor  to  a  given  position.  An actual
	 * 	cursor  move  is  sometimes needed here specialy when
	 * 	an  hardware  cursor  is  used.  In general,  graphic
	 * 	systems  simply  actualize  cursor  position  and the
	 * 	NEWputc(),  NEWputs() functions use this position and
	 * 	NEWcshow()  function  is  responsible  for  the  real
	 * 	cursor display and/or blink.
	 */

	NEWcol = col;
	NEWrow = row;

}

static	void
NEWeeol( void )
{
	/*
	 *	Clear  from  the  current  cursor  position  (NEWcol,
	 *	NEWrow) to the end of line.  The following basic code
	 *	works  well  in  early  stage  of  the  port  but can
	 *	(please  read  must !!)  be  improved  in  the  final
	 *	version.
	 */

	TTYputs( NEWblank, (TTYncol - X11col) );

}

static	void
NEWeeop( void )
{
	/*
	 *	Clear  from  the  current  cursor  position  (NEWcol,
	 *	NEWrow)  to  the  end of screen.  The following basic
	 *	code  works  well  in early stage of the port but can
	 *	(please  read  must !!)  be  improved  in  the  final
	 *	version.
	 */

	int	i;

	for( i = 0 ; i < TTYnrow ; i++ ) {
		TTYmove( i, 0 );
		TTYputs( NEWblank, TTYncol );
	}
}

static	void
NEWbeep( void )
{
	/*
	 *	Ring  the  system bell in the most 'standad way' when
	 *	an  error  occurs.  It  can  be ignored for the first
	 *	step of the port.
	 */

	(void)fputc( 7, stdout );
}

static	void
NEWsi( void )
{
	/*
	 *	Start  Inverse.  In  fact,  change  the color used to
	 *	display  the  text.  This  function is only called to
	 *	display  modeline information or to display a logical
	 *	cursor.  It  can be ignored for the first step of the
	 *	port.
	 */
}

static	void
NEWei( void )
{
	/*
	 *	End  Inverse.  In  fact,  restore  the standard color
	 *	used  to display the text.  It can be ignored for the
	 *	first step of the port.
	 */
}

static	void
NEWcshow( int flag )
{
	/*
	 *	show  (flag  = T) or hide (flag = NIL) the new cursor
	 *	at  the  current  cursor postition (NEWcol,  NEWrow).
	 *	The  easiest  way  to  implement  this function is to
	 *	display  in inverse the character at current position
	 *	which is in 'curchar' EmACT global vairiable.
	 */

	if( flag == T ) {
		/*
		 *	Show the cursor (inverse curchar)
		 */

		TTYsi();
		TTYputc( curchar );
		TTYei();
	} else	{
		/*
		 *	Hide the cursor (restore curchar)
		 */

		TTYei();
		TTYputc( curchar );
		TTYsi();
	}
}

static	int
NEWcheck( void )
{
	/*
	 *	Check whether or not a character is present.  Used by
	 *	some ports.  In general, it a big nothing operation.
	 */

	return( 0 );
}

void
NEWrawmode( void )
{
	/*
	 *	Sometimes  used by some ports to restore terminal raw
	 *	mode  after  spawning  a  program  from  EmACT  (by a
	 *	system() command for example).
	 */
}

#endif
