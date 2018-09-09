#if	!defined( lint )
static	char rcsid[] = "$Id: window.c,v 1.6 2011/09/22 05:55:23 jullien Exp $";
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
 *	Window  management.  Some of the functions are internal,  and
 *	some are attached to keys that the user actually types.
 */

#include	"emacs.h"

/*
 *	Reposition dot in the current window. Bound to "M-!"
 */

CMD
reposition( void )
{
	curwp->w_force = repeat;
	curwp->w_flag |= WFFORCE;
	return( T );
}

/*
 *	Reposition  dot  in  the  middle  of  the  current window and
 *	refresh the entire screen. Bound to "C-L".
 */

CMD
recenter( void )
{
	curwp->w_force = (curwp->w_ntrows/2);
	curwp->w_flag |= WFFORCE;
	sgarbf         = T;

	return( T );
}

/*
 *	Refresh the entire screen. Not bound.
 */

CMD
redrawscreen( void )
{
	sgarbf = T;

	return( T );
}

/*
 *	Redisplay  the  screen  after  a resize.  Useful on Windowing
 *	system. This command is not bound to any key stroke.
 */

CMD
resize( void )
{
	WINSCR	*wp;
	BUFFER	*bp;

	bp = (BUFFER *)NULL;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp != curwp ) {
			bp = wp->w_bufp;
			break;
		}

	(void)onlywind();

	if( wheadp == NULL )
		return( NIL );

	wheadp->w_ntrows = (TTYnrow - 1);
	modeline( curwp );

	(void)WDGtitle((EMCHAR*)&curbp->b_bname[0],(EMCHAR*)&curbp->b_fname[0]);

	if( bp && (TTYnrow >= 4) ) {
		if( (wp = wpopup()) == NULL )
			return( NIL );
		else	return( connectwindow( wp, bp ) );
	} else	return( T );

}

/*
 *	Make the window pointed by wp the current window.  It restack
 *	the  buffer so that it becomes on top of the list for command
 *	switch-buffer or kill-buffer. Return always T.
 */

CMD
setcurrentwindow( WINSCR *wp )
{
	curwp = wp;
	bufferontop( wp->w_bufp );
	return( T );
}

/*
 *	The  command  make the next window the current window.  There
 *	are  no  real  errors,  although  the command does nothing if
 *	there is only 1 window on the screen. Bound to "C-XC-N".
 */

CMD
nextwind( void )
{
	WINSCR *wp;

	if( (wp = curwp->w_wndp) == NULL )
		wp = wheadp;

	return( setcurrentwindow( wp ) );
}

/*
 *	This  command  makes  the previous window the current window.
 *	There  arn't  any errors,  although the command does not do a
 *	lot if there is 1 window. Bound to "C-XT".
 */

CMD
prevwind( void )
{
	WINSCR *wp1 = wheadp;
	WINSCR *wp2 = curwp;

	if( wp1 == wp2 )
		wp2 = NULL;
	while( wp1->w_wndp != wp2 )
		wp1 = wp1->w_wndp;

	return( setcurrentwindow( wp1 ) );
}

/*
 *	This  command makes the top window the current window.  There
 *	arn't  any errors,  although the command does not do a lot if
 *	there is 1 window.
 */

CMD
topwind( void )
{
	WINSCR *wp;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp->w_toprow == 0 )
			return( setcurrentwindow( wp ) );

	return( NIL );
}

/*
 *	This  command  moves  the current window down by "arg" lines.
 *	Recompute  the  top line in the window.  Most of the work has
 *	to  do with reframing the window,  and picking a new dot.  We
 *	share  the code by having "move down" just be an interface to
 *	"move up". Bound to "C-XC-N".
 */

CMD
mvdnwind( void )
{
	int	save = repeat;

	repeat = -repeat;
	(void)mvupwind();
	repeat = save;

	return( T );
}

/*
 *	Move the current window up by "arg" lines.  Recompute the new
 *	top  line of the window.  Look to see if "." is still on  the
 *	screen.  If it is, you win.  If it isn't,  then move  "."  to
 *	center it in the new framing of the window (this command does
 *	not really move "."; it moves the frame). Bound to "C-XC-P".
 */

CMD
mvupwind( void )
{
	EDLINE	*lp = curwp->w_linep;
	int	n = repeat;
	int	i;

	if( n < 0 )
		while( n++ && lp != lastline( curbp ) )
			lp = lforw( lp );
	else	while( n-- && lback( lp ) != lastline( curbp ) )
			lp = lback( lp );

	curwp->w_linep	 = lp;
	curwp->w_flag	|= WFHARD;

	for( i = 0 ; i < curwp->w_ntrows ; ++i ) {
		if( lp == curwp->w_dotp )
			return( T );
		if( lp == lastline( curbp ) )
			break;
		lp = lforw( lp );
	}
	lp = curwp->w_linep;
	i  = curwp->w_ntrows/2;
	while( i-- && lp != lastline( curbp ) )
		lp = lforw( lp );
	curwp->w_dotp  = lp;
	curwp->w_doto  = 0;
	return( T );
}

/*
 *	This  command makes the current window the only window on the
 *	screen.  Bound to "C-X1".  Try to set the framing so that "."
 *	does  not  have  to move on the display.  Some care has to be
 *	taken  to  keep  the  values  of  dot  and mark in the buffer
 *	structures  right  if  the  distruction  of  a window makes a
 *	buffer become undisplayed.
 */

CMD
onlywind( void )
{
	WINSCR *wp;
	EDLINE	*lp;
	int	i;

	while( wheadp != curwp ) {
		wp	= wheadp;
		wheadp	= wp->w_wndp;
		(void)disconnectwindow( wp );
		free( wp );
	}
	while( curwp->w_wndp != NULL ) {
		wp	      = curwp->w_wndp;
		curwp->w_wndp = wp->w_wndp;
		(void)disconnectwindow( wp );
		free( wp );
	}

	lp = curwp->w_linep;

	for( i = (int)curwp->w_toprow; i!=0 && lback(lp)!=lastline(curbp); i-- )
		lp = lback( lp );

	curwp->w_toprow = 0;
	curwp->w_ntrows = (TTYnrow-1);
	curwp->w_linep	= lp;
	curwp->w_flag  |= WFMODE|WFHARD;
	return( T );
}

/*
 *	Delete  current  window.  Do  nothing  if  there  is only one
 *	window. Bound to C-XD or C-X0 in GNU compatible mode.
 */

CMD
delwind( void )
{
	WINSCR *wp;

	if( wheadp->w_wndp == NULL ) {
		WDGmessage( ECSTR("Only one window") );
		return( NIL );
	}

	if( curwp == wheadp ) {
		wheadp	     = curwp->w_wndp;
		wp	     = curwp->w_wndp;
		wp->w_toprow = 0;
	} else	{
		for( wp = wheadp ; wp->w_wndp != curwp ; wp = wp->w_wndp )
			;

		wp->w_wndp = curwp->w_wndp;
	}

	wp->w_ntrows += (curwp->w_ntrows + 1);
	wp->w_flag   |= WFMODE|WFHARD;

	(void)disconnectwindow( curwp );
	cfree( curwp );

	return( setcurrentwindow( wp ) );
}

/*
 *	Split  the  current  window.  A  window  smaller than 3 lines
 *	cannot  be split.  The only other error that is possible is a
 *	"malloc"   failure  allocating  the  structure  for  the  new
 *	window. Bound to "C-X2".
 */

CMD
splitwind( void )
{
	WINSCR	*wp;
	WINSCR	*wp1;
	WINSCR	*wp2;
	EDLINE	*lp;
	int	ntru;
	int	ntrl;
	int	ntrd;

	if( curwp->w_ntrows < 3 ) {
	  WDGmessage(ECSTR("You can't have windows smaller than 2 lines high"));
	  return( NIL );
	}

	if( (wp = (WINSCR *)malloc( sizeof( WINSCR ) )) == NULL ) {
		WDGmessage( ECSTR("Cannot allocate WINDOW block") );
		return( NIL );
	}

	wp->w_bufp	= curbp;
	wp->w_dotp	= curwp->w_dotp;
	wp->w_doto	= curwp->w_doto;
	wp->w_markp	= curwp->w_markp;
	wp->w_marko	= curwp->w_marko;
	wp->w_emode	= curwp->w_emode;
	wp->w_binary	= curwp->w_binary;
	wp->w_wide	= curwp->w_wide;
	wp->w_flag	= 0;
	wp->w_force	= 0;

	/* Displayed twice.	*/

	curbp->b_count  = curbp->b_count + 1;

	ntru = (curwp->w_ntrows-1) / 2;		/* Upper size		*/
	ntrl = (curwp->w_ntrows-1) - ntru;	/* Lower size		*/

	for( lp	= curwp->w_linep, ntrd = 0 ; lp != curwp->w_dotp ; ++ntrd ) {
		lp = lforw( lp );
	}

	lp = curwp->w_linep;
	if( ntrd <= ntru ) {			/* Old is upper window. */
		if( ntrd == ntru )		/* Hit mode line.	*/
			lp = lforw( lp );
		curwp->w_ntrows = ntru;
		wp->w_wndp	= curwp->w_wndp;
		curwp->w_wndp	= wp;
		wp->w_toprow	= curwp->w_toprow+ntru+1;
		wp->w_ntrows	= ntrl;
	} else	{				/* Old is lower window	*/
		wp1 = NULL;
		wp2 = wheadp;
		while( wp2 != curwp ) {
			wp1 = wp2;
			wp2 = wp2->w_wndp;
		}
		if( wp1 == NULL )
			wheadp = wp;
		else	wp1->w_wndp = wp;
		wp->w_wndp	 = curwp;
		wp->w_toprow	 = curwp->w_toprow;
		wp->w_ntrows	 = ntru;
		++ntru;				/* Mode line.		*/
		curwp->w_toprow += ntru;
		curwp->w_ntrows	 = ntrl;
		while( ntru-- )
			lp = lforw( lp );
	}
	curwp->w_linep	 = lp;		/* Adjust the top lines */
	curwp->w_flag	|= WFMODE|WFHARD;
	wp->w_linep	 = lp;
	wp->w_flag	|= WFMODE|WFHARD;
	return( T );
}

/*
 *	Enlarge  the  current  window.  Find  the  window  that loses
 *	space.  Make  sure it is big enough.  If so,  hack the window
 *	descriptions,  and ask redisplay to do all the hard work. You
 *	don't just set "force reframe" because dot would move.  Bound
 *	to "C-XZ".
 */

CMD
enlargewind( void )
{
	WINSCR	*adjwp;
	EDLINE	*lp;
	int	i;

	if( wheadp->w_wndp == NULL ) {
		WDGmessage( ECSTR("Only one window") );
		return( NIL );
	}
	if( (adjwp = curwp->w_wndp) == NULL ) {
		adjwp = wheadp;
		while( adjwp->w_wndp != curwp )
			adjwp = adjwp->w_wndp;
	}
	if( adjwp->w_ntrows <= repeat ) {
		WDGmessage( ECSTR("Can't change window size") );
		return( NIL );
	}
	if( curwp->w_wndp == adjwp ) {		/* Shrink below.	*/
		lp = adjwp->w_linep;
		for( i = 0 ; i < repeat && lp != lastline(adjwp->w_bufp) ; ++i )
			lp = lforw( lp );
		adjwp->w_linep	= lp;
		adjwp->w_toprow += repeat;
	} else	{			/* Shrink above.	*/
		lp = curwp->w_linep;
		for( i = 0 ; i < repeat && lback(lp) != lastline(curbp) ; ++i )
			lp = lback( lp );
		curwp->w_linep	 = lp;
		curwp->w_toprow -= repeat;
	}
	curwp->w_ntrows += repeat;
	curwp->w_flag	|= WFMODE|WFHARD;
	adjwp->w_ntrows -= repeat;
	adjwp->w_flag	|= WFMODE|WFHARD;
	return( T );
}

/*
 *	Shrink the current window.  Find the window that gains space.
 *	Hack at the window descriptions.  Ask the redisplay to do all
 *	the hard work. Bound to "C-XC-Z".
 */

CMD
shrinkwind( void )
{
	WINSCR	*adjwp;
	EDLINE	*lp;
	int	i;

	if( wheadp->w_wndp == NULL ) {
		WDGmessage( ECSTR("Only one window") );
		return( NIL );
	}
	if( (adjwp = curwp->w_wndp) == NULL ) {
		adjwp = wheadp;
		while( adjwp->w_wndp != curwp )
			adjwp = adjwp->w_wndp;
	}
	if( curwp->w_ntrows <= repeat ) {
		WDGmessage( ECSTR("Can't change window size") );
		return( NIL );
	}
	if( curwp->w_wndp == adjwp ) {		/* Grow below.		*/
		lp = adjwp->w_linep;
		for( i=0; i<repeat && lback(lp)!=lastline(adjwp->w_bufp); ++i )
			lp = lback( lp );
		adjwp->w_linep	 = lp;
		adjwp->w_toprow -= repeat;
	} else	{			/* Grow above.		*/
		lp = curwp->w_linep;
		for( i = 0 ; i < repeat && lp != lastline( curbp ) ; ++i )
			lp = lforw( lp );
		curwp->w_linep	 = lp;
		curwp->w_toprow += repeat;
	}
	curwp->w_ntrows	-= repeat;
	curwp->w_flag	|= WFMODE|WFHARD;
	adjwp->w_ntrows += repeat;
	adjwp->w_flag	|= WFMODE|WFHARD;
	return( T );
}

/*
 *	Pick  a  window  for  a pop-up.  Split the screen if there is
 *	only  one  window.  Pick  the uppermost window that isn't the
 *	current window.
 */

WINSCR	*
wpopup( void )
{
	WINSCR *wp;

	if( wheadp->w_wndp == NULL && splitwind() == NIL )
		return( NULL );

	for( wp = wheadp ; wp != NULL && wp == curwp ; wp = wp->w_wndp )
		;

	return( wp );
}

/*
 *	This portion of code deal with the mouse click.
 */

CMD
findwind( void )
{
	WINSCR	*wp;
	EDLINE	*lp;
	int	wx = mevent.x;
	int	wy = mevent.y;
	int	l  = wy;
	int	i;
	int	top;
	int	nrow;
	int	resizep = NIL;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		top  = (int)wp->w_toprow;
		nrow = (int)wp->w_ntrows;
		if( top <= l && top + nrow >= l ) {
			(void)setcurrentwindow( wp );
			lp = wp->w_linep;
			if( (top + nrow) == l ) {
				if( wx==(TTYncol-4) || wx==(TTYncol-3) )
				     (void)forwpage();
				else if( wx==(TTYncol-7) || wx==(TTYncol-6) )
				     (void)backpage();
				else switch( mevent.button ) {
				     case MButton1 :
					  resizep = shrinkwind();
					  break;
				     case MButton2 :
					  resizep = enlargewind();
					  break;
				     default :
					  WDGmessage(ECSTR("Not such window!"));
					  (void)ctrlg();
					  return( NIL );
				}
					
				if( resizep == NIL )
					waitmatch( 1 );

				return( T );
			}

			for( l = top; l < wy && lp != wp->w_dotp ; l++ )
				lp = lforw( lp );

			if( l < wy )
				while( l++ < wy )
					(void)forwline();
			else	while( lp != wp->w_dotp )
					(void)backline();
			
			wp->w_doto = 0;
			l	   = 0;
			i	   = 0;

			while( l < wx && i < llength( wp->w_dotp ) ) {
				if( lgetc( wp->w_dotp, i++ ) == '\t' )
					do
						++l;
					while( l % tab_display );
				else	++l;
				(void)forwchar();
			}

			if( curbp->b_emode == BUFFERMODE )
				return( buffercmd( 'f' ) );

			if( curbp->b_emode == DIRED )
				return( diredcmd( 'f' ) );

			switch( mevent.button ) {
			case MButton1:
				/* mouse-track */
				(void)setmark();
				break;
			case MButton2:
			case MButton3:
				/* x-set-point-and-insert-selection */
				WDGclippaste();
				(void)yank();
				update( T );
				break;
			case (MButton1|CTRLBUTTON):
				/* mouse-track insert */
				(void)copyregion();
				(void)swapmark();
				WDGclipcopy();
				break;
			case (MButton2|CTRLBUTTON):
				/* x-mouse-kill */
				(void)killregion();
				WDGclipcopy();
				break;
			case (MButton1|SHIFTBUTTON):
				/* mouse-track-adjust */
				(void)copyregion();
				WDGclipcopy();
				break;
			}

			thisflag = CFCPCN;
			lastflag = 0;

			return( T );
		}
	}

	return( T );
}

CMD
adjust( void )
{
	WDGadjust();
	return( T );
}

