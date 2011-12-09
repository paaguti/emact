#if	!defined( lint )
static	char rcsid[] = "$Id: basic.c,v 1.3 2008/06/19 12:13:29 jullien Exp $";
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
 *	The  routines  in  this file move the cursor  around  on  the
 *	screen.  They compute a new value for the cursor, then adjust
 *	".".  The display code always updates the cursor location, so
 *	only  moves between lines,  or functions that adjust the  top
 *	line in the window and invalidate the framing, are hard.
 */

#include	"emacs.h"

static	int	_define(getgoal,(EDLINE *dlp));

/*
 *	Move the cursor to the beginning of the current line.
 */

CMD
gotobol( void )
{
	curwp->w_doto  = 0;
	return( T );
}

/*
 *	Move  the cursor backwards by  "n" characters.   Compute  the
 *	new cursor location.  Error  if  you try and  move out of the
 *	buffer. Set the flag if the line pointer for dot changes.
 */

CMD
backchar( void )
{
	EDLINE	*lp;
	int	n = repeat;

	while( n-- )
		if( curwp->w_doto == 0 ) {
			if( (lp = lback(curwp->w_dotp)) == lastline(curbp) )
				return( NIL );
			curwp->w_dotp	= lp;
			curwp->w_doto	= llength( lp );
			curwp->w_flag  |= WFMOVE;
		} else	curwp->w_doto--;

	return( T );
}

/*
 *	Move the cursor to the end of the current line.
 */

CMD
gotoeol( void )
{
	curwp->w_doto = llength( curwp->w_dotp );
	return( T );
}

/*
 *	Move  the cursor forwards by "n" characters.  Compute the new
 *	cursor location,  and move ".". Error if you try and move off
 *	the  end of the buffer.  Set the flag if the line pointer for
 *	dot changes.
 */

CMD
forwchar( void )
{
	int n = repeat;

	while( n-- )
		if( curwp->w_doto == llength( curwp->w_dotp ) ) {
			if( curwp->w_dotp == lastline( curbp ) )
				return( NIL );
			curwp->w_dotp	= lforw( curwp->w_dotp );
			curwp->w_doto	= 0;
			curwp->w_flag  |= WFMOVE;
		} else	curwp->w_doto++;

	return( T );
}

/*
 *	Goto the beginning of the buffer.  Massive adjustment of dot.
 *	This is considered to be hard motion;  it really isn't if the
 *	original  value  of dot is the same as the new value of  dot.
 *	Normally bound to "M-<".
 */

CMD
gotobob( void )
{
	curwp->w_dotp  = firstline( curbp );
	curwp->w_doto  = 0;
	curwp->w_flag |= WFHARD;
	return( T );
}

/*
 *	Move to the end of the buffer.   Dot is always put at the end
 *	of the file.  The standard screen code does most of  the hard
 *	parts of update. Bound to "M->".
 */

CMD
gotoeob( void )
{
	curwp->w_dotp  = lastline( curbp );
	curwp->w_doto  = 0;
	curwp->w_flag |= WFHARD;
	return( T );
}

/*
 *	Prompt for a line number and jump to  that position.  If line
 *	is out buffer display error message and exit.
 *	Bound to "M-G" and "M-N".
 */

CMD
gotoline( void )
{
	EDLINE	*clp;
	int	n = repeat;
	EMCHAR	buf[ 20 ];
	
	if( n <= 1 ) {
		if( mlreply( ECSTR("Goto line: "), buf, 20 ) == ABORT )
			return( ABORT );
		n = emstrtoi( buf );
	}

	(void)gotobob();
	clp = curwp->w_dotp;
	while( --n && clp != lastline( curbp ) )
		clp = lforw( clp );

	curwp->w_dotp  = clp;
	curwp->w_doto  = 0;
	curwp->w_flag |= WFMOVE;

	if( n <= 0 )
		return( T );

	WDGmessage( ECSTR("No such line.") );
	return( NIL );
}

/*
 *	Move forward by full lines. The last command controls how the
 *	goal column is set. Bound to "C-N". No errors are possible.
 */

CMD
forwline( void )
{
	EDLINE	*dlp;
	int	n = repeat;

	if( (lastflag & CFCPCN) == 0 )		/* Reset goal if last	*/
		curgoal = curcol;		/* not C-P or C-N	*/
	thisflag |= CFCPCN;
	if( (dlp = curwp->w_dotp) == lastline( curbp ) )
		return( NIL );
	while( n-- && dlp != lastline( curbp ) )
		dlp = lforw( dlp );

	curwp->w_dotp  = dlp;
	curwp->w_doto  = getgoal( dlp );
	curwp->w_flag |= WFMOVE;

	return( T );
}

/*
 *	This  function is like "forwline",  but goes  backwards.  The
 *	scheme is  exactly the same. Figure out the new line and call
 *	"movedot" to perform the motion.  No errors are possible.
 *	Bound to "C-P".
 */

CMD
backline( void )
{
	EDLINE	*dlp;
	int	n = repeat;

	if( (lastflag&CFCPCN) == 0 )		/* Reset goal if the	*/
		curgoal = curcol;		/* last isn't C-P, C-N	*/
	thisflag |= CFCPCN;
	dlp  = curwp->w_dotp;
	if( lback( dlp ) == lastline( curbp ) ) {
		TTYbeep();
		return( NIL );
	}
	while( n-- && lback( dlp ) != lastline( curbp ) )
		dlp = lback( dlp );

	curwp->w_dotp  = dlp;
	curwp->w_doto  = getgoal( dlp );
	curwp->w_flag |= WFMOVE;

	return( T );
}

/*
 *	This  routine,  given a pointer to a EDLINE, and the  current
 *	cursor  goal column,  return the best choice for the  offset.
 *	The offset is returned.  Used by "C-N" and "C-P".
 */

static	int
getgoal( EDLINE	*dlp )
{
	int	c;
	int	newcol;
	int	col = 0;
	int	dbo = 0;

	while( dbo != llength( dlp ) ) {
		c      = lgetc( dlp, dbo );
		newcol = col;
		if( c == '\t' )
			do
				++newcol;
			while( newcol % tab_display );
		else	{
			if( !self_insert( c ) )
				++newcol;
			++newcol;
		}

		if( newcol > curgoal )
			break;
		col = newcol;
		++dbo;
	}

	return( dbo );
}

/*
 *	Scroll  forward by a specified number of lines,  or by a full
 *	page  if  no  argument.   Bound to "C-V".   The  "2"  in  the
 *	arithmetic  on the window size is the overlap;  this value is
 *	the  default overlap value in ITS EMACS.   Because this  zaps
 *	the  top  line in the display window,  we have to do  a  hard
 *	update.
 */

CMD
forwpage( void )
{
	EDLINE	*lp;
	int	l;

	if( (l = curwp->w_ntrows - 2) <= 0 )	/* Forget the overlap	*/
		l = 1;				/* if tiny window.	*/

	lp = curwp->w_linep;
	while( l-- && lp != lastline( curbp ) )
		lp = lforw( lp );

	curwp->w_linep = lp;
	curwp->w_dotp  = lp;
	curwp->w_doto  = 0;
	curwp->w_flag |= WFHARD;

	return( T );
}

/*
 *	This command is like "forwpage", but on the other window. The
 *	value is from the GNU EMACS manual. Bound to "M-C-V.
 */

CMD
forwother( void )
{
	if( nextwind() == T && forwpage() == T )
		return( prevwind() );
	else	return( NIL );
}

/*
 *	This command is like "forwpage",  but it goes backwards.  The
 *	"2",  like above, is the overlap between the two windows. The
 *	value is from the ITS EMACS manual.  Bound to "M-V".  We do a
 *	hard update for exactly the same reason.
 */

CMD
backpage( void )
{
	EDLINE	*lp;
	int	l;

	if( (l = curwp->w_ntrows - 2) <= 0 )	/* Don't blow up if the */
		l = 1;				/* window is tiny.	*/

	lp = curwp->w_linep;
	while( l-- && (lback( lp ) != lastline( curbp )) )
		lp = lback( lp );

	curwp->w_linep	= lp;
	curwp->w_dotp	= lp;
	curwp->w_doto	= 0;
	curwp->w_flag  |= WFHARD;

	return( T );
}

/*
 *	Set  the  mark  in  the current window to the value of "." in
 *	the window. No errors are possible. Bound to "C-SPC".
 */

CMD
setmark( void )
{
	curwp->w_markp = curwp->w_dotp;
	curwp->w_marko = curwp->w_doto;
	WDGmessage( ECSTR("Mark set.") );
	return( T );
}

/*
 *	Set the mark in the current window to the entire buffer. After
 *	this command,  "." is set at the end of the buffer.  No errors
 *	are possible. Bound to "C-XH".
 */

CMD
markwholebuffer( void )
{
	/*
	 *	Set mark at the end of the buffer.
	 */

	curwp->w_markp = lastline( curbp );
	curwp->w_marko = 0;

	/*
	 *	Go to the beginning of the buffer.
	 */

	(void)gotobob();

	WDGmessage( ECSTR("Mark set.") );
	return( T );
}

/*
 *	Swap the values of "." and "mark" in the current window. This
 *	is pretty easy, bacause all of the hard work gets done by the
 *	standard routine that moves the mark about. The only possible
 *	error is "no mark". Bound to "C-XC-X".
 */

CMD
swapmark( void )
{
	EDLINE	*odotp;
	int	odoto;

	if( curwp->w_markp == NULL ) {
		WDGmessage( ECSTR("No mark set in this buffer!") );
		return( NIL );
	}

	odotp		= curwp->w_dotp;
	odoto		= curwp->w_doto;
	curwp->w_dotp	= curwp->w_markp;
	curwp->w_doto	= curwp->w_marko;
	curwp->w_markp	= odotp;
	curwp->w_marko	= odoto;
	curwp->w_flag  |= WFMOVE;

	return( T );
}
