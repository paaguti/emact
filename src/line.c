#if	!defined( lint )
static	char rcsid[] = "$Id: line.c,v 1.8 2009/05/01 07:36:12 jullien Exp $";
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
 *	The  functions  in  this  file  are  a  general  set  of line
 *	management  utilities.  They are the only routines that touch
 *	the  text.  They also touch the buffer and window structures,
 *	to  make  sure  that the necessary updating gets done.  There
 *	are  routines  in  this file that handle the kill buffer too.
 *	It isn't here for any good reason.
 *
 *	Note  that  this code only updates the dot and mark values in
 *	the  window  list.  Since  all  the  code acts on the current
 *	window,  the  buffer  that  we  are  editing  must  be  being
 *	displayed,  which means that b_count is non zero, which means
 *	that  the  dot  and  mark  values  in  the buffer headers are
 *	nonsense.
 */

#include	"emacs.h"

EMCHAR	*kbufp	 = (EMCHAR *)NULL; /* Kill buffer data		 */
size_t	kused	 = 0;		   /* # of bytes used in KB	 */

static	size_t ksize = 0;	   /* # of bytes allocated in KB */

static	int	_define(ldelnewline,(void));

/*
 *	This  routine  allocates  a  block  of memory large enough to
 *	hold  a  LINE  containing  "used"  characters.  The  block is
 *	always  rounded up a bit.  Return a pointer to the new block,
 *	or  NULL  if there isn't any memory left.  Print a message in
 *	the message line if no space.
 */

EDLINE	*
lalloc( int used )
{
	EDLINE	*lp;
	size_t	size = (size_t)((used+NBLOCK-1) & ~(NBLOCK-1));
	size_t	buflen;

	if( size == 0 )			/* Assume that an empty */
		size = NBLOCK;		/* line is for type-in. */

	buflen = size * sizeof( EMCHAR );

	lp = (EDLINE *)malloc( (unsigned)(sizeof(EDLINE) + buflen) );

	if( lp == (EDLINE *)NULL ) {
		TTYbeep();
		WDGerror( ECSTR("No memory left, file may be corrupted.") );
		return( NULL );
	}

	ltext( lp )   = (EMCHAR *)((char *)lp + sizeof( EDLINE ));
	lsize( lp )   = (short)size;
	llength( lp ) = (short)used;

	return( lp );
}

/*
 *	Delete  line  "lp".  Fix all of the links that might point at
 *	it  (they are moved to offset 0 of the next line.  Unlink the
 *	line  from  whatever  buffer  it  might  be  in.  Release the
 *	memory.  The  buffers  are  updated too; the magic conditions
 *	described in the above comments don't hold here.
 */

void
lfree( EDLINE *lp )
{
	BUFFER *bp;
	WINSCR *wp;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		if( wp->w_linep == lp )
			wp->w_linep = lforw( lp );
		if( wp->w_dotp	== lp ) {
			wp->w_dotp  = lforw( lp );
			wp->w_doto  = 0;
		}
		if( wp->w_markp == lp ) {
			wp->w_markp = lforw( lp );
			wp->w_marko = 0;
		}
	}

	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp )
		if( bp->b_count == 0 ) {
			if( bp->b_dotp	== lp ) {
				bp->b_dotp = lforw( lp );
				bp->b_doto = 0;
			}
			if( bp->b_markp == lp ) {
				bp->b_markp = lforw( lp );
				bp->b_marko = 0;
			}
		}

	lforw( lback( lp ) ) = lforw( lp );
	lback( lforw( lp ) ) = lback( lp );
	free( lp );
}

/*
 *	This  routine  gets  called  when  a  character is changed in
 *	place  in the current buffer.  It updates all of the required
 *	flags  in  the  buffer  and  window system.  The flag used is
 *	passed  as  an  argument; if the buffer is being displayed in
 *	more  than  1 window we change EDIT to HARD.  Set MODE if the
 *	mode line needs to be updated (the "*" has to be set).
 */

void
lchange( int flag )
{
	WINSCR	*wp;

	if( curbp->b_count != 1 )		/* Ensure hard.		*/
		flag = WFHARD;

	if( (curbp->b_flag & BFCHG) == 0 ) {	/* First change, so 	*/
		flag          |= WFMODE;	/* update mode lines.	*/
		curbp->b_flag |= BFCHG;
	}

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp->w_bufp == curbp )
			wp->w_flag |= flag;

}

/*
 *	Reverse  the  effects  of lchange for the current buffer.  It
 *	updates  all  of  the required flags in the buffer and window
 *	system.  If  the  buffer  is being displayed in more than one
 *	window  we  change  EDIT  to HARD.  Set MODE if the mode line
 *	needs to be updated (the "*" has to be removed).
 */

CMD
notmodified( void )
{
	WINSCR	*wp;
	int	flag = 0;

	if( curbp->b_count != 1 )	/* Ensure hard.		*/
		flag = WFHARD;

	curbp->b_flag &= ~BFCHG;
	flag	      |= WFMODE;	/* update mode lines.	*/

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp->w_bufp == curbp )
			wp->w_flag |= flag;

	return( T );
}

/*
 *	Insert  "n"  copies  of  the  character  "c"  at  the current
 *	location  of  dot.  In  the easy case all that happens is the
 *	text  is stored in the line.  In the hard case,  the line has
 *	to  be  reallocated.  When  the window list is updated,  take
 *	special care; I screwed it up once.  You always update dot in
 *	the  current  window.  You update mark,  and a dot in another
 *	window,  if  it  is  greater than the place where you did the
 *	insert. Return T if all is well, and NIL on errors.
 */

int
linsert( int n, int c )
{
	EDLINE	*lp1;
	EDLINE	*lp2;
	EDLINE	*lp3;
	WINSCR	*wp;
	EMCHAR	*cp1;
	EMCHAR	*cp2;
	int	doto;
	int	i;
	int	j;
	int	kflag;

	if( ((curbp->b_fmode & WACCESS) == 0) ) {
		if( curbp->b_emode == BUFFERMODE )
			return( buffercmd( c ) );

		if( curbp->b_emode == DIRED )
			return( diredcmd( c ) );
	}

	if( freadonly() )
		return( NIL );

	lchange( WFEDIT );

#if	defined( _UNICODE )
	if( c > 0xFF && curbp->b_wide == 0 ) {
		curbp->b_wide = EMUTF16;
		updatemodes();
	}
#endif

	lp1 = curwp->w_dotp;

	if( lp1 == lastline( curbp ) ) {
		/*
		 * At the end: special
		 */

		if( curwp->w_doto != 0 ) {
			internalerror( ECSTR("w_doto != 0") );
			return( NIL );
		}

		if( (lp2 = lalloc( n )) == NULL )
			return( NIL );

		lp3		= lback( lp1 );	/* Previous line	*/
		lforw( lp3 )	= lp2;		/* Link in		*/
		lforw( lp2 )	= lp1;
		lback( lp1 )	= lp2;
		lback( lp2 )	= lp3;

		for( i = 0 ; i < n ; ++i )
			lputc( lp2, i, c );

		curwp->w_dotp = lp2;
		curwp->w_doto = n;
		return( T );
	}

 	kflag = NIL;	/* in case of realloc */
	doto  = curwp->w_doto;

	if( (llength( lp1 ) + n) > lsize( lp1 ) ) {

		/*
		 *	Hard: reallocate
		 */

		if( (lp2 = lalloc( llength( lp1 ) + n )) == NULL )
			return( NIL );
		cp1 = ltext( lp1 );
		cp2 = ltext( lp2 );

		/*
		 *	Copy up doto
		 */
		for( i = 0 ; i < doto ; i++ )
			*cp2++ = *cp1++;

		/*
		 *	Copy the rest of the line (space is left for insertion)
		 */
		cp2 += n;
		j    = llength( lp1 );
		while( i++ < j )
			*cp2++ = *cp1++;

		/*
		 *	Update chainning.
		 */

		lforw( lback( lp1 ) )	= lp2;
		lforw( lp2 )		= lforw( lp1 );
		lback( lforw( lp1 ) )	= lp2;
		lback( lp2 )		= lback( lp1 );

		kflag = T;			/* lp1 must be deleted  */
	} else	{

		/*
		 *	Easy: in place
		 */

		lp2 = lp1;			/* Pretend new line	*/
		llength( lp2 ) += (short)n;
		cp2 = lchadr( lp1, llength( lp1 ) );
		cp1 = cp2-n;
		while( cp1 - lchadr( lp1, doto ) )
			*--cp2 = *--cp1;
	}

	for( i = 0 ; i < n ; ++i )		/* Add the characters	*/
		lputc( lp2, doto+i, c );

	/*
	 * Update w_dotp, w_markp and found_p.
	 */

	if( found_p == lp1 ) {
		found_p = lp2;
		if( found_o > doto )
			found_o += n;
	}

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		if( wp->w_linep == lp1 )
			wp->w_linep = lp2;
		if( wp->w_dotp == lp1 ) {
			wp->w_dotp = lp2;
			if( (wp == curwp) || (wp->w_doto > doto) )
				wp->w_doto += n;
		}
		if( wp->w_markp == lp1 ) {
			wp->w_markp = lp2;
			if( wp->w_marko > doto )
				wp->w_marko += n;
		}
		
	}

	if( kflag == T )
		free( lp1 );	/* was a realloc */
		
	return( T );
}

/*
 *	Replace "n"  copies  of  the character  "c"  at  the  current
 *	location  of dot.  In the easy case all that happens  is  the
 *	text is replaced in the line. In the hard case, at the end of
 *	the line, the routine linsert is call  with n  equal  to  the
 *	number of characters alredy replaced.
 */

int
lreplace( int n, int c )
{
	if( freadonly() )
		return( NIL );

	while( n )
		if( curwp->w_doto == llength( curwp->w_dotp ) )
			return( linsert( n, c ) );
		else	{
			lputc( curwp->w_dotp, curwp->w_doto++, c );
			n--;
		}

	lchange( WFHARD );
	return( T );

}

/*
 *	Insert  a  newline into the buffer at the current location of
 *	dot  in  the  current window.  The funny ass-backwards way it
 *	does  things  is  not a botch; it just makes the last line in
 *	the  file  not  a special case.  Return T if everything works
 *	out and NIL on error (memory allocation failure).  The update
 *	of  dot  and  mark  is  a  bit easier then in the above case,
 *	because the split forces more updating.
 */

int
lnewline( void )
{
	EMCHAR	*cp1;
	EMCHAR	*cp2;
	EDLINE	*lp1;
	EDLINE	*lp2;
	int	doto;
	WINSCR	*wp;

	if( freadonly() )
		return( NIL );

	lchange( WFHARD );

	/*
	 * Get the address and offset of "."
	 */

	lp1  = curwp->w_dotp;
	doto = curwp->w_doto;

	/*
	 * Alloc new first half line
	 */

	if( (lp2 = lalloc( doto )) == NULL )
		return( NIL );

	/*
	 * Shuffle text around
	 */

	cp1 = ltext( lp1 );
	cp2 = ltext( lp2 );
	while( cp1 - lchadr( lp1, doto ) )
		*cp2++ = *cp1++;

	cp2 = ltext( lp1 );
	while( cp1 - lchadr( lp1, llength( lp1 )) )
		*cp2++ = *cp1++;

	llength( lp1 )	       -= (short)doto;
	lback( lp2 )		= lback( lp1 );
	lback( lp1 )		= lp2;
	lforw( lback( lp2 ) )	= lp2;
	lforw( lp2 )		= lp1;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		if( wp->w_linep == lp1 )
			wp->w_linep = lp2;
		if( wp->w_dotp == lp1 ) {
			if( wp->w_doto < doto )
				wp->w_dotp  = lp2;
			else	wp->w_doto -= doto;
		}
		if( wp->w_markp == lp1 ) {
			if( wp->w_marko < doto)
				wp->w_markp  = lp2;
			else	wp->w_marko -= doto;
		}
	}

	return( T );
}

/*
 *	This   function  deletes  "n"  bytes,  starting  at  dot.  It
 *	understands how do deal with end of lines,  etc. It returns T
 *	if  all of the characters were deleted,  and NIL if they were
 *	not (because dot ran into the end of the buffer.  The "kflag"
 *	is T if the text should be put in the kill buffer.
 */

int
ldelete( int n, int kflag )
{
	EMCHAR	*cp1;
	EMCHAR	*cp2;
	EDLINE	*dotp;
	int	doto;
	int	chunk;
	WINSCR	*wp;

	if( freadonly() )
		return( NIL );

	while( n != 0 ) {
		dotp = curwp->w_dotp;
		doto = curwp->w_doto;
		if( dotp == lastline( curbp ) )	/* Hit end of buffer.	*/
			return( NIL );
		chunk = llength( dotp ) - doto;	/* Size of chunk.	*/
		if( chunk > n )
			chunk = n;
		if( chunk == 0 ) {		/* End of line, merge.	*/
			lchange( WFHARD );
			if( ldelnewline() == NIL
			|| (kflag != NIL && kinsert( '\n' ) == NIL) )
				return( NIL );
			--n;
			continue;
		}
		lchange( WFEDIT );
		cp1 = lchadr( dotp, doto );	/* Scrunch text.	*/
		cp2 = cp1 + chunk;
		if( kflag != NIL ) {		/* Kill?		*/
			while( cp1 - cp2 ) {
				if( kinsert( *cp1 ) == NIL )
					return( NIL );
				++cp1;
			}
			cp1 = lchadr( dotp, doto );
		}
		while( cp2 - lchadr( dotp, llength( dotp )) )
			*cp1++ = *cp2++;	/* take care of address	*/
		llength( dotp ) -= (short)chunk;

		for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
			if( wp->w_dotp==dotp && wp->w_doto>=doto ) {
				wp->w_doto -= chunk;
				if( wp->w_doto < doto )
					wp->w_doto = doto;
			}	
			if( wp->w_markp==dotp && wp->w_marko>=doto ) {
				wp->w_marko -= chunk;
				if( wp->w_marko < doto )
					wp->w_marko = doto;
			}
		}
		n -= chunk;
	}

	if( kflag )
		WDGclipcopy();

	return( T );
}

/*
 *	Delete  a newline.  Join the current line with the next line.
 *	If  the  next  line is the magic header line always return T;
 *	merging  the last line with the header line can be thought of
 *	as  always  being a successful operation,  even if nothing is
 *	done,  and  this  makes  the  kill buffer work "right".  Easy
 *	cases  can  be  done  by  shuffling  data around.  Hard cases
 *	require  that  lines be moved about in memory.  Return NIL on
 *	error and T if all looks ok. Called by "ldelete" only.
 */

static int
ldelnewline( void )
{
	EMCHAR	*cp1;
	EMCHAR	*cp2;
	EDLINE	*lp1;
	EDLINE	*lp2;
	EDLINE	*lp3;
	WINSCR	*wp;
	short	chunk = 0;

	lp1 = curwp->w_dotp;
	lp2 = lforw( lp1 );

	if( lp2 == lastline( curbp ) ) {
		/*
		 *	At the buffer end.
		 */
		if( llength( lp1 ) == 0 )	/* Blank line.		*/
			lfree( lp1 );
		return( T );
	}

	/*
	 * If  not  FUNDAMENTAL,  BUFFERMODE  or  DIRED,  delete left
	 * blanks and tabs.
	 */

	cp2 = ltext( lp2 );
	if( curwp->w_emode != FUNDAMENTAL &&
	    curwp->w_emode != DIRED       &&
	    curwp->w_emode != BUFFERMODE  &&
	    (thisflag & CFKILL) == 0 ) {
		while( cp2 - lchadr( lp2, llength( lp2 )) )
			if( *cp2 == ' ' || *cp2 == '\t' ) {
				cp2++;
				chunk++;
			} else	break;
	}

	if( llength( lp2 ) <= lsize( lp1 ) - llength( lp1 ) ) {
		cp1 = lchadr( lp1, llength( lp1 ) );
		while( cp2 - lchadr( lp2, llength( lp2 )) )
			*cp1++ = *cp2++;

		for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
			if( wp->w_linep == lp2)
				wp->w_linep = lp1;
			if( wp->w_dotp == lp2 ) {
				wp->w_dotp  = lp1;
				wp->w_doto += llength( lp1 );
			}
			if( wp->w_markp == lp2 ) {
				wp->w_markp  = lp1;
				wp->w_marko += llength( lp1 );
			}
		}		

		llength( lp1 ) += (short)(llength( lp2 ) - chunk);
		lforw( lp1 )	= lforw( lp2 );
		lback( lforw( lp2 ) ) = lp1;
		free( lp2 );
		return( T );
	}
	if( (lp3 = lalloc( llength( lp1 ) + llength( lp2 ) - chunk )) == NULL )
		return( NIL );
	cp1 = ltext( lp1 );
	cp2 = ltext( lp3 );
	while( cp1 - lchadr( lp1, llength( lp1 )) )
		*cp2++ = *cp1++;
	cp1 = ltext( lp2 ) + chunk;
	while( cp1 - lchadr( lp2, llength( lp2 )) )
		*cp2++ = *cp1++;
	lforw( lback( lp1 ) )	= lp3;
	lforw( lp3 )		= lforw( lp2 );
	lback( lforw( lp2 ) )	= lp3;
	lback( lp3 )		= lback( lp1 );

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		if( wp->w_linep == lp1 || wp->w_linep == lp2 )
			wp->w_linep = lp3;
		if( wp->w_dotp == lp1)
			wp->w_dotp  = lp3;
		else if( wp->w_dotp == lp2) {
			wp->w_dotp  = lp3;
			wp->w_doto += llength( lp1 );
		}
		if( wp->w_markp == lp1 )
			wp->w_markp  = lp3;
		else if( wp->w_markp == lp2 ) {
			wp->w_markp  = lp3;
			wp->w_marko += llength( lp1 );
		}
	}

	free( lp1 );
	free( lp2 );
	return( T );
}

/*
 *	The  argument "text" points to a string.  Append this line to
 *	the  buffer.  Handcraft  the  EOL on the end.  Return T if it
 *	worked and NIL if you ran out of room.
 */

int
addline( BUFFER *bp, EMCHAR *text )
{
	EDLINE	*lp;
	int	i;
	int	ntext;

	ntext = emstrlen( text );

	if( (lp = lalloc( ntext )) == NULL )
		return( NIL );

	for( i = 0 ; i < ntext ; ++i )
		lputc( lp, i, text[ i ] );

	lforw(lback(bp->b_linep)) = lp;	/* Hook onto the end	*/
	lback( lp )		  = lback( bp->b_linep );
	lback( bp->b_linep )	  = lp;
	lforw( lp )		  = bp->b_linep;

	if( bp->b_dotp == lastline( bp ) ) /* If "." is at the end */
		bp->b_dotp = lp;	   /* move it to new line	*/

	return( T );
}

/*
 *	Return  the  position  of  last  character in the line 'line'
 *	expanding tabs to the current tab_display value.
 */

int
lposition( EDLINE *line )
{
	EMCHAR	*str = ltext( line );
	int	lmax = llength( line );
	int	col  = 0;
	int	i;

	for( i = 0 ; i < lmax ; i++ ) {
		if( *str++ == '\t' )
			do
				++col;
			while( col % tab_display );
		else	++col;
	}

	return( col );
}

/*
 *	Delete  all of the text saved in the kill buffer.  Called by
 *	commands when a new kill context is being created. The  kill
 *	buffer  array is released, just in case the buffer has grown
 *	to immense size. No errors.
 */

void
kdelete( void )
{
	if( kbufp != NULL ) {
		cfree( kbufp );
		kused = 0;
		ksize = 0;
	}
}

/*
 *	Insert a character to the kill buffer,  enlarging the  buffer
 *	if there isn't any room.  Always grow the buffer in by a  50%
 *	factor,  on the  assumption that if you put  something in the
 *	kill  buffer you are going to put more stuff there too later.
 *	Return T if all is well, and NIL on errors.
 */

int
kinsert( int c )
{
	EMCHAR	*nbufp;
	size_t	newsize;
	int	i;

	if( kused == ksize ) {
		/*
		 * set newsize to KBLOCK or add 50 % if the
		 * the kill-buffer is not empty
		 */

		if( ksize == 0 )
			newsize = KBLOCK;
		else	newsize = ksize + (ksize / 2);

		if( (unsigned)newsize > EMMAXINT ) {
			WDGerror( ECSTR("Kill buffer full.") );
			return( NIL );
		}

		if( (nbufp=(EMCHAR *)malloc(newsize*sizeof(EMCHAR))) == NULL )
			return( NIL );
		for( i = 0 ; i < (int)ksize ; i++ )
			nbufp[ i ] = kbufp[ i ];
		if( kbufp != NULL )
			cfree( kbufp );
		kbufp  = nbufp;
		ksize  = newsize;
	}
	kbufp[kused++] = (EMCHAR)c;
	return( T );
}

/*
 *	This  function gets characters from the kill buffer.  If  the
 *	character  index "n" is off the end,  it returns  "-1".  This
 *	lets the caller just scan along until it gets a "-1" back.
 */

int
kremove( int n )
{
  if( n >= (int)kused )
		return( -1 );
	else	return( kbufp[ n ] );
}

/*
 *	This routine exchanges two lines in the current buffer at "."
 *	position with the line above  if it exists.  First  and  last
 *	line  can't  be exchanged by this routine  and, in that case,
 *	returns NIL.
 */

CMD
ltwiddle( void )
{
	EDLINE	*lp1;		/* Current line		*/
	EDLINE	*lp2;		/* Line above "." line	*/
	BUFFER	*bp;
	WINSCR	*wp;

	if( freadonly() )
		return( NIL );

	lp1 = curwp->w_dotp;
	lp2 = lback( lp1 );

	if( lp1 == firstline( curbp ) || lp1 == lastline( curbp ) )
		return( NIL );

	lchange( WFHARD );

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp ) {
		if( wp->w_linep == lp2 )
			wp->w_linep = lp1;
		if( wp->w_dotp	== lp1 ) {
			wp->w_dotp  = lp2;
			wp->w_doto  = 0;
		}
		if( wp->w_markp == lp1 ) {
			wp->w_markp = lp2;
			wp->w_marko = 0;
		}
	}

	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp )
		if( bp->b_count == 0 ) {
			if( bp->b_dotp	== lp1 ) {
				bp->b_dotp = lp2;
				bp->b_doto = 0;
			}
			if( bp->b_markp == lp1 ) {
				bp->b_markp = lp2;
				bp->b_marko = 0;
			}
		}

	if( lp1 != lastline( curbp ) ) {
		lforw( lp2 )		= lforw( lp1 );
		lback( lforw( lp1 ) )	= lp2;
		lforw( lp1 )		= lp2;
	} else	lforw( lp2 )		= firstline( curbp );

	lback( lp1 )		= lback( lp2 );
	lforw( lback( lp2 ) )	= lp1;
	lback( lp2 )		= lp1;

	(void)forwline();

	return( T );
}

/*
 *	Change mode between insert and replace. Normally bound to M-I.
 */

CMD
instoggle( void )
{
	WINSCR	*wp;

	if( replace_mode == T )
		replace_mode = NIL;
	else	replace_mode = T;

	for( wp = wheadp ; wp ; wp = wp->w_wndp )
		modeline( wp );

	return( T );
}
