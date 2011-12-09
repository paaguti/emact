#if	!defined( lint )
static	char rcsid[] = "$Id: curses.c,v 1.4 2008/06/20 09:25:11 jullien Exp $";
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
 *	The routines in this file provide support for CURSES package.
 */

#include	"emacs.h"

#if	!defined( _X11_ONLY ) && defined( _CURSES )

#if	defined(_POSIX_SOURCE) || defined(HAVE_TERMIOS_H)
#include	<termios.h>

static struct termios	ostate;
static struct termios	nstate;
#endif

#if	defined( HAVE_NCURSES_H ) && !defined( _EMACS_CURSES_DEFINED )
#include	<ncurses.h>
#define	_EMACS_CURSES_DEFINED
#endif	/* HAVE_NCURSES_H */

#if	defined( HAVE_CURSES_H )  && !defined( _EMACS_CURSES_DEFINED )
#include	<curses.h>
#define	_EMACS_CURSES_DEFINED
#endif	/* HAVE_NCURSES_H */

#if	!defined( _EMACS_CURSES_DEFINED )
#if	defined( __linux__ ) || defined( __FreeBSD__ ) || defined( __QNX__ )
#include	<ncurses.h>
#else
#include	<curses.h>
#endif	/* __linux__ || __FreeBSD__ || __QNX__ */
#define	_EMACS_CURSES_DEFINED
#endif	/* _EMACS_CURSES_DEFINED */

/*
 * not mandatory by POSIX
 */

#if	!defined( _POSIX_VDISABLE )
#if	defined( CDISABLE )
#define	_POSIX_VDISABLE		CDISABLE
#else
#define	_POSIX_VDISABLE		((unsigned char)'\377')
#endif
#endif

static	void	_define(cursesopen,(void));
static	void	_define(cursesclose,(void));
static	int	_define(cursesgetc,(void));
static	void	_define(cursesputc,(int c));
static	void	_define(cursesputs,(EMCHAR *s, int n));
static	void	_define(cursesmove,(int row, int col));
static	void	_define(curseseeol,(void));
static	void	_define(curseseeop,(void));
static	void	_define(cursesflush,(void));
static	void	_define(cursesbeep,(void));
static	void	_define(cursesei,(void));
static	void	_define(cursessi,(void));
static	void	_define(cursescshow,(int flag));
static	int	_define(cursescheck,(void));
static	void	_define(cursesrawmode,(void));
static	void	_define(cursesgetmode,(void));
static	void	_define(cursesresetmode,(void));

static	int	cursesx;	/* current X position (line) */
static	int	cursesy;	/* current Y position (row)  */
static	int	cursescolor;	/* current color	     */

#if	defined( _X11 )
#define	term	cursesterm
#endif

TERM	term = {
	0,
	0,
	0,
	NIL,
	cursesopen,
	cursesclose,
	cursesgetc,
	cursesputc,
	cursesputs,
	cursesflush,
	cursesmove,
	curseseeol,
	curseseeop,
	cursesbeep,
	cursessi,
	cursesei,
	cursescshow,
	cursescheck,
	cursesrawmode
};

#if	defined( COLOR_PAIR )
static	short	colortable[] = {
	COLOR_BLACK,
	COLOR_BLUE,
	COLOR_GREEN,
	COLOR_CYAN,
	COLOR_RED,
	COLOR_MAGENTA,
	COLOR_YELLOW,
	COLOR_WHITE
};
#endif

/*
 *	Curses  raw  mode  is  sometimes half-cooked.  That is,  some
 *	control   caracters  are  still  intercepted  by  OS  (Ctrl-O
 *	Ctrl-V,  Ctrl-K,  ..  cursesrawmode  go  further  and changes
 *	terminal to send all characters directly to EmACT.
 */

static void
cursesrawmode( void )
{
#if	defined( _POSIX_SOURCE ) || defined( __QNX__ )
	int	i;

	(void)tcgetattr( 1, &nstate );
	nstate.c_iflag &= ~(IGNBRK|INLCR|ICRNL|IXON|IXOFF);
	nstate.c_iflag |= BRKINT;
	nstate.c_oflag &= ~(OPOST);
	nstate.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
	nstate.c_lflag |= ISIG;

	/*
	 * Disable  the  value of character that can confuse terminal
	 * in non canonical mode.
	 */

	for( i = 0 ; i < NCCS ; i++ )
		switch( i ) {
		case VQUIT  :
			break;
		default     :
			nstate.c_cc[ i ] = _POSIX_VDISABLE;
		}

	/*
	 * Set intercharacter timer and accept at least 6 characters
	 */

	nstate.c_cc[ VMIN   ] = 1;	/* has been 6 for a while */
	nstate.c_cc[ VTIME  ] = 1;

	(void)tcsetattr( 1, TCSANOW, &nstate );
#endif
}

static void
cursesgetmode( void )
{
#if	defined( _POSIX_SOURCE )
	(void)tcgetattr( 1, &ostate );
#endif
}

static void
cursesresetmode( void )
{
#if	defined( _POSIX_SOURCE )
	(void)tcsetattr( 1, TCSANOW, &ostate );
#endif
}

static	void
cursesopen( void )
{
	cursesgetmode();

	(void)initscr();
	(void)raw();
#if	!defined( __linux__ )
	/*
	 * no implementation should need this
	 */
	(void)nonl();
#endif
	(void)noecho();
#if	defined( KEY_HOME )
	(void)keypad( stdscr, T );
#endif
	cursesrawmode();
	TTYncol = COLS;
	TTYnrow = LINES-1;

#if	defined( COLOR_PAIR )
	if( start_color() == OK ) {
		if( monochrome_monitor == T ) {
			(void)init_pair( 1, COLOR_WHITE, COLOR_BLACK );
			(void)init_pair( 2, COLOR_BLACK, COLOR_WHITE );
		} else	{
			(void)init_pair(
					 1,
					 colortable[ foreground_color ],
					 colortable[ background_color ]
				       );
			(void)init_pair( 2, COLOR_BLACK,  COLOR_WHITE );
		}
	} else	monochrome_monitor = T;
	cursescolor = COLOR_PAIR( 1 );
#endif

	/*
	 * Use package functions!!
	 */

	cursesmove( 0, 0 );
	curseseeop();
	cursesflush();
	TTYinit = T;
}

static	void
cursesclose( void )
{
	/*
	 * Use native curses functions!!
	 */

	cursescolor = 0;
	(void)move( 0, 0 );
 	(void)clear();
	(void)move( 0, 0 );
	(void)refresh();
	(void)echo();
	(void)noraw();
	(void)endwin();

	cursesresetmode();
	TTYinit = NIL;
}

#if	defined( __MVS__ )
static	int cursesctrl[] = {
              /* 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F */
/* 00 */	 0,  1,  2,  3,  0,  9,  0,  8,  0,  0,  0, 11, 12,  0, 14, 15,
/* 10 */	16, 17, 18, 19,  0, 13,  8,  0, 24, 25,  0,  0,  0,  0,  0,  0,
/* 20 */	 0, 13,  0,  0, 24,  0, 23, 27,  0,  0,  0,  0,  0,  5,  6,  7,
/* 30 */	 0,  0, 22,  0,  0,  0,  0,  4, 23,  0,  0,  0, 20, 21,  0, 26,
/* 40 */	 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};
#endif

static	int
cursesgetc( void )
{
	int	c = getch();

#if	defined( __MVS__ )
	if ( c < 0x40 ) {
		printf( "code %x -> %d\r\n", c, cursesctrl[c] );
		c = cursesctrl[c];
	};
#endif

	switch( c ) {
#if	defined( KEY_HOME )
	case KEY_DOWN	   :	return( Ctrl|'N' );
	case KEY_UP	   :	return( Ctrl|'P' );
	case KEY_LEFT	   :	return( Ctrl|'B' );
	case KEY_RIGHT	   :	return( Ctrl|'F' );
	case KEY_HOME	   :	return( META|'<' );
	case KEY_BACKSPACE :	return( Ctrl|'H' );
#if	defined( __FreeBSD__ )
	case KEY_DC	   :	return( Ctrl|'H' );
#else
	case KEY_DC	   :	return( Ctrl|'D' );
#endif
	case KEY_IC	   :	return( META|'I' );
	case KEY_NPAGE	   :	return( Ctrl|'V' );
	case KEY_PPAGE	   :	return( META|'V' );
	case KEY_F(1)	   :	return( META|'?' );
	case KEY_F(2)	   :	return( Ctrl|'S' );
	case KEY_F(3)	   :	return( META|Ctrl|'F' );
	case KEY_F(4)	   :	return( CTLX|Ctrl|'I' );
	case KEY_F(6)	   :	return( SPCL|'@' );
	case KEY_F(7)	   :	return( CTLZCH   );
	case KEY_F(8)	   :	return( CTLX|Ctrl|'S' );
	case KEY_F(9)	   :	return( CTLX|'E' );
	case KEY_F(11)	   :	return( META|'M' );
	case KEY_ENTER	   :	return( 0x0D     );
	case KEY_LL	   :	return( META|'<' );
#if	defined( KEY_A1 )
	case KEY_A1	   :	return( META|'>' );
	case KEY_A3	   :	return( META|'V' );
	case KEY_C1	   :	return( META|'<' );
	case KEY_C3	   :	return( Ctrl|'V' );
#endif
#if	defined( KEY_END )
	case KEY_END	   :	return( META|'>' );
#endif
#endif
	default		   :	return( c );
	}
}

static	void
cursesputc( int c )
{
#if	defined( COLOR_PAIR )
	if( monochrome_monitor == T )
		(void)addch( (chtype)c );
	else	(void)addch( (chtype)(c | cursescolor) );
#else
	(void)addch( c );
#endif
	++cursesy;
}

static	void
cursesputs( EMCHAR *s, int n )
{
	while( n-- > 0 )
		cursesputc( (int)*s++ );
}

static	void
cursesflush( void )
{
	(void)refresh();
}

static	void
cursesmove( int x, int y )
{
	cursesx = x;
	cursesy = y;

	(void)move( x, y );
}

static	void
curseseeol( void )
{
	int	i;

#if	defined( _CURSES_CLEARS )
 	(void)clrtoeol();
#else	/* _CURSES_CLEARS */
	for( i = cursesy ; i < COLS ; ++i ) {
		(void)move( cursesx, i );
		(void)addch( (chtype)(' ' | cursescolor) );
	}
#endif	/* _CURSES_CLEARS */
}

static	void
curseseeop( void )
{
	int	i;

#if	defined( _CURSES_CLEARS )
	(void)clear();
#else	/* _CURSES_CLEARS */
	for( i = 0 ; i < LINES ; ++i ) {
		cursesmove( i, 0 );
		curseseeol();
	}
#endif	/* _CURSES_CLEARS */
}

static	void
cursesbeep( void )
{
#if	defined( _BSD ) || defined( sun ) || defined( __linux__ )
	(void)fputc( 7, stdout );
#else
	(void)beep();
#endif
}

static	void
cursessi( void )
{
#if	defined( COLOR_PAIR )
	if( monochrome_monitor == T )
		(void)standout();
	else	(void)attrset( (attr_t)(cursescolor = COLOR_PAIR( 2 )) );
#else
	(void)standout();
#endif
}

static	void
cursesei( void )
{
#if	defined( COLOR_PAIR )
	if( monochrome_monitor == T )
		(void)standend();
	else	(void)attrset( (attr_t)(cursescolor = COLOR_PAIR( 1 )) );
#else
	(void)standend();
#endif
}

static	void
cursescshow( int flag )
{
	curflag = flag;
	(void)refresh();
}

static	int
cursescheck( void )
{
	return( 0 );
}

#endif /* _X11_ONLY */
