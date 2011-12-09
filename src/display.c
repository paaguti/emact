#if     !defined( lint )
static	char rcsid[] = "$Id: display.c,v 1.10 2009/05/01 07:36:11 jullien Exp $";
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
 *	The  functions in this file handle redisplay.  There are  two
 *	halves,  the ones that update the virtual display screen, and
 *	the  ones  that make the physical display screen the same  as
 *	the  virtual display screen.  These functions use hints  that
 *	are left in the windows by the commands.
 */

#include	"emacs.h"

static	void	_define(vtputc,(int c));
static	void	_define(vteeol,(void));
static	void	_define(vtmove,(int row, int col));
static	void	_define(updatewin,(WINSCR *wp));
static	void	_define(computecursor,(void));
static	void	_define(updateline,(int row, EMCHAR *vline, EMCHAR *pline));
static	void	_define(modeputs,(EMCHAR *s, int *n));

#if	defined( _PCTERM )
#define	OFFSET		1
#define	GRIPCHAR	((unsigned char)254)
#define	FOOTCHAR	((unsigned char)255)
#else
#define	OFFSET		2

#define	GRIPCHAR	'#'
#define	FOOTCHAR	'-'
#endif

#define	VERSION_LENGTH	6	/* the six letters  'E' 'm' 'A' 'C' 'T' ':' */

#define vgetc( vp, n )		((vp)->text[(n)])
#define vputc( vp, n, c )	((vp)->text[(n)] = (EMCHAR)(c))
#define vflag( vp, f )		((vp)->flag |= (f))

#define VFCHG	0x0001			/* Changed.			*/

int	sgarbf	= T;			/* T if screen is garbage	*/
int	mpresf	= NIL;			/* T if message in last line 	*/
long	current_row;			/* Current row text position	*/
long	current_col;			/* Current col text position	*/

static	int	vtok	  = 0;		/* VT ready to run		*/
static	int	vtrow	  = 0;		/* Row location of SW cursor	*/
static	int	vtcol	  = 0;		/* Column location of SW cursor */
static	VIDEO	**vscreen = NULL;	/* Virtual screen.		*/
static	VIDEO	**pscreen = NULL;	/* Physical screen.		*/

/*
 *	Initialize the data structures used by the display code.  The
 *	edge  vectors  used to access the screens  are  set  up.  The
 *	operating  system's terminal I/O channel is set up.  All  the
 *	other  things get initialized at compile time.   The original
 *	window  has  "WFCHG"  set,  so that it  will  get  completely
 *	redrawn on the first call to "update".
 */

void
vtinit( void )
{
	VIDEO	*vp;
	size_t	size;
	int	i;
	int	j;

	size = (size_t)(TTYnrow + 1) * sizeof( VIDEO * );
	if( (vscreen = (VIDEO **)malloc( size )) == NULL ||
	    (pscreen = (VIDEO **)malloc( size )) == NULL )
		exit( 1 );

	size = ((size_t)(TTYncol + 1) * sizeof( EMCHAR )) + sizeof( VIDEO );
	for( i = 0 ; i <= TTYnrow ; ++i ) {
		if( (vp = (VIDEO *)calloc( size, 1 )) == NULL )
			exit( 1 );
		vp->text = (EMCHAR *)((char *)vp + sizeof( VIDEO ));
		for( j = 0 ; j < TTYncol ; ++j )
			vp->text[ j ] = ' ';
		vp->flag = 0;
		vscreen[ i ] = vp;

		if( (vp = (VIDEO *)calloc( size, 1 )) == NULL )
			exit( 1 );
		vp->text = (EMCHAR *)((char *)vp + sizeof( VIDEO ));
		for( j = 0 ; j < TTYncol ; ++j )
			vp->text[ j ] = ' ';
		vp->flag = 0;
		pscreen[ i ] = vp;
	}

	vtok = T;

	mlerase();
}

/*
 *	Return vscreen (may be need by some terminal)
 */

VIDEO **
getvscreen( void )
{
	return( vscreen );
}

void
vtfree( void )
{
	int	i;

	if( vscreen == NULL || pscreen == NULL )
		return;

	for( i = 0 ; i <= TTYnrow ; i++ ) {
		cfree( vscreen[ i ] );
		cfree( pscreen[ i ] );
	}

	cfree( vscreen );
	cfree( pscreen );

	vtok = NIL;
}

/*
 *	Check  if  vt  has  been  initialized.  Return T is so or NIL
 *	otherwise.
 */

CMD
vtrunning( void )
{
	return( vtok ? T : NIL );
}

/*
 *	Clean up the virtual terminal system,  in anticipation for  a
 *	return  to the operating system.  Move up  to the first  line
 *	and  clear it out (the next system prompt will be written  in
 *	the line). Shut down the channel to the terminal.
 */

void
vttidy( void )
{
	TTYmove( 0, 0 );
	TTYeop();
	TTYflush();
	TTYclose();
}

/*
 *	Set the virtual cursor to the specified row and column on the
 *	virtual  screen.  There  is no checking for nonsense  values;
 *	this might be a good idea during the early stages.
 */

static	void
vtmove( int row, int col )
{
	vtrow = row;
	vtcol = col;
}

/*
 *	Write a character to the virtual screen.  The virtual row and
 *	column are updated.  If the line is too long put a "\" in the
 *	last column.  This routine only puts printing characters into
 *	the  virtual  terminal  buffers.  Only  column  overflow   is
 *	checked.
 */

static	void
vtputc( int c )
{
	VIDEO	*vp = vscreen[ vtrow ];

	if( vtcol >= TTYncol ) {
		vputc( vp, TTYncol - 1, '\\' );
	} else	if( c == '\t' ) {
		do
			vtputc( (int)' ' );
		while( (vtcol % tab_display) && vtcol < TTYncol );
	} else	if( !self_insert( c ) ) {
		if( set_show_graphic || (mouseflag && (c==24||c==25)) )
			vputc( vp, vtcol++, (EMCHAR)c );
		else	{
			vtputc( (int)'^' );
			vtputc( (int)c ^ 0x40 );
		}
	}  else	{
		vputc( vp, vtcol++, (EMCHAR)c );
	}
}

void
statputc( int n, int c )
{
	if( n < TTYncol ) {
		vputc( vscreen[ TTYnrow ], n, (EMCHAR)c );
		vflag( vscreen[ TTYnrow ], VFCHG );
	}
}

/*
 *	Erase  from the end of the software cursor to the end of  the
 *	line on which the software cursor is located.
 */

static	void
vteeol( void )
{
	VIDEO	*vp = vscreen[ vtrow ];

	while( vtcol < TTYncol )
		vputc( vp, vtcol++, ' ' );
}

static	void
computecursor( void )
{
	EDLINE	*lp;
	int	i;
	int	c;

	lp	= curwp->w_linep;
	currow	= curwp->w_toprow;

	while( lp != curwp->w_dotp ) {
		++currow;
		lp = lforw( lp );
	}

	curcol = 0;

	for( i = 0 ; i < curwp->w_doto ; i++ ) {
		c = lgetc( lp, i );
		if( c == '\t' )
			do
				++curcol;
			while( curcol % tab_display );
		else	{
			if( set_show_graphic == NIL && !self_insert( c ) )
				++curcol;
			++curcol;
		}
	}

	if( curcol >= TTYncol )
		curcol = TTYncol-1;

	curchar = vgetc( vscreen[ currow ], curcol );

}

/*
 *	Make  sure that the display is right.  This is a  three  part
 *	process.  First,  scan through all of the windows looking for
 *	dirty  ones.  Check  the  framing,  and refresh  the  screen.
 *	Second,  make sure that "currow" and "curcol" are correct for
 *	the  current window.  Third,  make the virtual  and  physical
 *	screens the same.
 */

static	void
updatewin( WINSCR *wp )
{
	EDLINE	*lp;
	int	out = NIL;
	int	i;
	int	j;

	/*
	 *	If not force reframe, check the framing.
	 */

	if( (wp->w_flag & WFFORCE) == 0 ) {
		lp = wp->w_linep;
		for( i = 0 ; i < wp->w_ntrows ; ++i ) {
			if( lp == wp->w_dotp ) {
				out = T; /* line inside window */
				break;
			}
			if( lp == lastline( wp->w_bufp ) )
				break;
			lp = lforw( lp );
		}
	}

	/*
	 *	Not acceptable, better compute a new  value
	 *	for the line at the top of the window. Then
	 *	set the "WFHARD" flag to force full redraw.
	 */

	if( out == NIL ) {
		if( (i = wp->w_force) > 0 ) {
			if( --i >= wp->w_ntrows )
				i = wp->w_ntrows-1;
		} else	if( i < 0 && (i += wp->w_ntrows) < 0 )
				i = 0;
		else	i = wp->w_ntrows/2;

		lp = wp->w_dotp;
		while( i-- && lback( lp ) != lastline( wp->w_bufp ) )
			lp = lback( lp );

		wp->w_linep = lp;
		wp->w_flag |= WFHARD;	/* Force full.	*/
	}

	/*
	 *	Try to use reduced update.  Mode line update
	 *	has its own special flag. The fast update is
	 *	used if the only thing to  do is within  the
	 *	line editing.
	 */

	lp = wp->w_linep;
	i  = (int)wp->w_toprow;
	if( (wp->w_flag & ~WFMODE) == WFEDIT ) {
		j = wp->w_ntrows + i;
		while( (lp != wp->w_dotp) && (j > i) ) {
			++i;
			lp = lforw( lp );
		}
		if( j > i ) {
			vscreen[i]->flag |= VFCHG;
			vtmove( i, 0 );
			for( j = 0 ; j < llength( lp ) ; ++j )
				vtputc( (int)lgetc( lp, j ) );
			vteeol();
		}
	} else	if( (wp->w_flag & (WFEDIT|WFHARD)) != 0 )
			while( i < ((int)wp->w_toprow + (int)wp->w_ntrows) ) {
				vscreen[i]->flag |= VFCHG;
				vtmove( i, 0 );
				if( lp != lastline( wp->w_bufp ) ) {
					for( j = 0 ; j < llength( lp ) ; ++j )
						vtputc( (int)lgetc( lp, j ) );
					lp = lforw( lp );
				}
				vteeol();
				++i;
			}

	modeline( wp );
	wp->w_flag  = 0;
	wp->w_force = 0;
}

void
update( int mode )
{
	static	 BUFFER	*oldbp = (BUFFER *)NULL;

	WINSCR	*wp;
	VIDEO	*vp1;
	VIDEO	*vp2;
	int	i;
	int	j;

	if( vtok == NIL )
		return;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		/*
		 *	Look at any window with update flags set on.
		 */
		if( wp->w_flag != 0 )
			updatewin( wp );

	}

	/*
	 *	Always  recompute  the  row  and column number of the
	 *	hardware  cursor.  This is the only update for simple
	 *	moves.
	 */

	if( mode == MINIBUF ) {
		curcol  = mbcursor;
		currow  = TTYnrow;
		curchar = ' ';
	} else	computecursor();

	/*
	 *	Special  hacking if the screen is garbage.  Clear the
	 *	hardware  screen,  and update your copy to agree with
	 *	it.  Set all the virtual screen change bits, to force
	 *	a full update.
	 */

	if( sgarbf != NIL ) {
		for( i = 0 ; i <= TTYnrow ; ++i ) {
			vscreen[i]->flag |= VFCHG;
			vp1 = pscreen[i];
			for( j = 0 ; j < TTYncol ; ++j )
				vputc( vp1, j, ' ' );
		}
		if( sgarbf != EXPOSE ) {
			TTYmove( 0, 0 );
			TTYeop();
		}
		sgarbf = NIL;	/* Erase-page clears	*/
	}

	/*
	 *	Make  sure  that  the  physical  and virtual displays
	 *	agree.  Unlike  before,  the  updateline code is only
	 *	called with a line that has been updated for sure.
	 */

	for( i = 0 ; i <= TTYnrow ; ++i ) {
		vp1 = vscreen[ i ];
		if( (vp1->flag & VFCHG) != 0 ) {
			vp1->flag &= ~VFCHG;
			vp2 = pscreen[ i ];
			updateline( i, vp1->text, vp2->text );
		}
	}

	/*
	 *	Update the current buffer name when needed
	 */

	if( curbp != oldbp ) {
		(void)WDGtitle(
				(EMCHAR *)&curbp->b_bname[ 0 ],
				(EMCHAR *)&curbp->b_fname[ 0 ]
			      );
		oldbp = curbp;
	}

	/*
	 *	Finally,  update the hardware cursor,  char at cursor
	 *	and flush out buffers
	 */

	TTYmove( currow, curcol );
	TTYflush();
}

/*
 *	Update a single line. This does not know how to use insert or
 *	delete  character sequences.  Update  the  physical  row  and
 *	column variables.
 */

#if	defined( _UNIX )
#define	MAX_OUTPUT	1024
#endif

static	void
updateline( int row, EMCHAR *nline, EMCHAR *pline )
{
#if	defined( _UNICODE )
	int	stflag;
#if	defined( _UNIX )
	char outbuf[ MAX_OUTPUT ];
	emwcstombs( outbuf, nline, MAX_OUTPUT );
#else
	EMCHAR	*outbuf = nline;
#endif

	(void)pline;

	stflag = (emstrncmp( nline + OFFSET, version, VERSION_LENGTH ) == 0);
	TTYmove( row, 0 );


	if( stflag ) {
		TTYinverse();
		TTYputs( outbuf, TTYncol );
		TTYnormal();
	} else	{
		TTYputs( outbuf, TTYncol );
	}
#else
	EMCHAR	*cp1 = nline;
	EMCHAR	*cp2 = pline;
	EMCHAR	*cp3;
	EMCHAR	*cp4;
	EMCHAR	*cp5;
	int	count;
	int	nbflag;
	int	stflag;

	stflag = (emstrncmp( cp1+OFFSET, version, VERSION_LENGTH ) == 0);

	/*
	 *	Compute the left match.
	 */

	while( cp1 != (nline + TTYncol) && cp1[0] == cp2[0] ) {
		++cp1;
		++cp2;
	}

	if( cp1 == (nline + TTYncol) )
		/*
		 * Easy  an update is made outside the visible bounds
		 * of screen.  This can still happen,  even though we
		 * only  call  this routine on changed lines.  A hard
		 * update  is  always  done  when  a  line splits,  a
		 * massive  change is done,  or a buffer is displayed
		 * twice.  This  optimizes  out  most  of  the excess
		 * updating.  A  lot of computes are used,  but these
		 * tend  to  be  hard  operations  that  do  a lot of
		 * update. Nothing to do.
		 */
		return;

	/*
	 *	Compute right match and flag non blank changes
	 */

	nbflag	= NIL;
	cp3	= nline + TTYncol;
	cp4	= pline + TTYncol;

	while( cp3[-1] == cp4[-1] ) {
		--cp3;
		--cp4;
		if( cp3[0] != ' ' )		/* Note if any nonblank */
			nbflag = T;		/* in right match.	*/
	}

	cp5 = cp3;

	if( nbflag == NIL ) {
		/*
		 * Can we perform an erase to EOL ?
		 */

		while( (cp5 - cp1) && (cp5[-1] == ' ') )
			--cp5;

		/*
		 * Usefull only if erase is fewer characters.
		 */

		if( (cp3 - cp5) <= 3 )
			cp5 = cp3;
	}

	/*
	 *	Go to start of line.
	 */

	TTYmove( row, (int)(cp1 - nline) );

	if( stflag )
		TTYinverse();

	if( (count = (int)(cp5 - cp1)) > 0 ) {
		/*
		 * Display changes and update old line.
		 */
		TTYputs( cp1, count );

		while( count-- )
			*cp2++ = *cp1++;
	}

	if( cp5 != cp3 ) {
		/*
		 * Erase and update old line.
		 */
		TTYeol();
		while( cp1 - cp3 )
			*cp2++ = *cp1++;
	}

	if( stflag ) {
		TTYmove( row + 1, 0 );
		TTYnormal();
	}
#endif
}

/*
 *	Redisplay  the  mode line for the window pointed  to  by  the
 *	"wp".   This is the only routine that has any idea of how the
 *	modeline is formatted.  You can change the modeline format by
 *	hacking at this routine. Called by "update" any time there is
 *	a dirty window.
 */

#define	modeputc( c )		(vtputc( (int)c ), n++)

static	void
modeputs( EMCHAR *s, int *n )
{
	int	i;

	for( i = *n ; *s ; i++ )
		vtputc( (int)*s++ );

	*n = i;

}

void
modeline( WINSCR *wp )
{
	BUFFER	*bp;
	EDLINE	*lp;
	EMCHAR	buf[ 8 ];
	long	curlen;
	int	n;
	int	i;

	n = (int)wp->w_toprow + (int)wp->w_ntrows; /* Location.		 */
	vscreen[n]->flag |= VFCHG;		   /* Redraw next time.	 */
	vtmove( n, 0 );				   /* Seek to right line.*/
	n  = 0;					   /* Number of chars	 */
	bp = wp->w_bufp;

	modeputc( FOOTCHAR );
#if	!defined( _PCTERM )
	modeputc( ' ' );
#endif
	modeputs( version, &n );

	switch( wp->w_emode ) {
	case ASMODE	 : modeputs( ECSTR(" (Assembler"),   &n ); break;
	case BUFFERMODE	 : modeputs( ECSTR(" (Buffer Menu"), &n ); break;
	case CMODE	 : modeputs( ECSTR(" (C"),           &n ); break;
	case CPPMODE	 : modeputs( ECSTR(" (C++"),         &n ); break;
	case CSHARPMODE	 : modeputs( ECSTR(" (C#"),          &n ); break;
	case DIRED	 : modeputs( ECSTR(" (Dired"),       &n ); break;
	case SGMLMODE	 : modeputs( ECSTR(" (SGML"),        &n ); break;
	case FORTRANMODE : modeputs( ECSTR(" (Fortran"),     &n ); break;
	case JAVAMODE	 : modeputs( ECSTR(" (Java"),        &n ); break;
	case LISPMODE	 : modeputs( ECSTR(" (Lisp"),        &n ); break;
	case PASCALMODE	 : modeputs( ECSTR(" (Pascal"),      &n ); break;
	case PROLOGMODE	 : modeputs( ECSTR(" (Prolog"),      &n ); break;
	case PERLMODE	 : modeputs( ECSTR(" (Perl"),        &n ); break;
	case SHELLMODE	 : modeputs( ECSTR(" (Shell"),       &n ); break;
	default		 : modeputs( ECSTR(" (Fundamental"), &n ); break;
	}

	if( replace_mode == T )
		modeputs( ECSTR(" Ovwrt"), &n );

	if( auto_fill_mode == T )
		modeputs( ECSTR(" Fill"), &n );

#if	defined( _UNICODE )
	switch( wp->w_wide ) {
	case EMUTF8:
		modeputs( ECSTR(" UTF-8"), &n );
		break;
	case EMUTF16:
		modeputs( ECSTR(" UTF-16"), &n );
		break;
	}
#endif

	if( wp->w_binary == T && !wp->w_wide )
		modeputs( ECSTR(" Bin"), &n );

	modeputs( ECSTR(") "), &n );

	if( (bp->b_fmode & WACCESS) == 0 )
		/*
		 * "%" if readonly.
		 */
		modeputc( '%' );

	if( bp->b_flag & BFCHG  )
		/*
		 * "*" if changed.
		 */
		modeputc( '*' );

	modeputs( &bp->b_bname[0], &n );

	if( line_number_mode == T ) {
		EMCHAR	num[10];

		maxlen  = 1L;
		curlen	= 0L;

		lp  = firstline( curbp );

		while( lp != lastline( curbp ) ) {
			if( lp == curwp->w_dotp )
				curlen = maxlen;
			maxlen++;
			lp  = lforw( lp );
		}

		modeputs( ECSTR(" - L"), &n );

		if( curlen != 0 )
			(void)emsprintf1( &num[0], ECSTR("%ld "), curlen );
		else	(void)emsprintf1( &num[0], ECSTR("%ld "), maxlen );

		/*
		 * take care of UNICODE
		 */

		for( i = 0 ; num[i] != '\000' ; ++i )
			buf[i] = (EMCHAR)num[i];

		buf[i] = '\000';

		modeputs( buf, &n );

		if( curlen == 0L ) {
			current_row = maxlen;
			average = 100;
			modeputs( ECSTR("(100%)"), &n );
		} else	{
			current_row = curlen;
			average	= (int)((curlen*100)/maxlen);
			modeputc( '(' );
			modeputc( (EMCHAR)(average / 10 + (int)'0') );
			modeputc( (EMCHAR)(average % 10 + (int)'0') );
			modeputc( '%' );
			modeputc( ')' );
		}
	}

#if	defined( _DISPLAY_FILENAME )
	if( bp->b_fname[ 0 ] ) {
		/*
		 *	Complete file name with PATH.
		 */

		modeputs( ECSTR(" - "), &n );
		modeputs( &bp->b_fname[ 0 ], &n );
	}

	modeputc( ' ' );
#endif

	if( mouseflag ) {
		while( n < (TTYncol - 8) )
			modeputc( ' ' );

		vtcol = n = TTYncol-8;

		modeputs( ECSTR(" Up Dn "), &n );
		vtputc( (int)GRIPCHAR );
	} else	{
		while( n < (TTYncol - 1) )
			modeputc( ' ' );

		vtcol = TTYncol-1;

		vtputc( (int)FOOTCHAR );
	}
}
