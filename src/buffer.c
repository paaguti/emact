#if	!defined( lint )
static	char rcsid[] = "$Id: buffer.c,v 1.11 2009/05/02 12:25:43 jullien Exp $";
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
 *	Buffer  management.  Some of the functions are internal,  and
 *	some are actually attached to user keys.  Like everyone else,
 *	they set hints for the display system.
 */

#include	"emacs.h"

static	EMCHAR *_define(bufmatch,(EMCHAR *prompt, EMCHAR *buffer));
static	BUFFER *_define(getbpcmd,(EMCHAR *buf));
static  void	_define(longtostrtr,(EMCHAR *buf, int width, size_t num));
static	int	_define(makelist,(BUFFER *blp));
static	CMD	_define(savebname,(EMCHAR *bname));
static	CMD	_define(usewindow,(BUFFER *bp));
static	CMD	_define(discardbuffer,(BUFFER *bp));

#define	BUFFER_DEBUG	1

#if	defined( BUFFER_DEBUG )
static	void	_define(validitycheck,(void));

/*
 *	This  function is called by [dis]connectwindow to insure that
 *	buffers   b_count   are   always  correct.  Valid  only  when
 *	BUFFER_DEBUG has been defined.
 */

static	void
validitycheck( void )
{
	WINSCR	*wp;
	BUFFER	*bp;

	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp ) {
		int	count = 0;
		
		for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
			if( wp->w_bufp == bp )
				count++;

		if( count != bp->b_count )
			internalerror( ECSTR("wrong buffer count") );
	}
}

#else
#define	validitycheck( void )
#endif

/*
 *	Disconnect  the  buffer  associated  to the window pointed by
 *	"wp".  If the buffer display count equals 0 (meaning that the
 *	buffer  is  no  more  displayed on the screen) then copy mark
 *	and   dot  values  in  the  buffer.  This  function  is  used
 *	internally.
 */

CMD
disconnectwindow( WINSCR *wp )
{
	BUFFER	*bp = wp->w_bufp;

	if( bp != NULL ) {
		if( bp->b_count == 1 ) {
			bp->b_count	= 0;
			bp->b_dotp	= wp->w_dotp;
			bp->b_doto	= wp->w_doto;
			bp->b_markp	= wp->w_markp;
			bp->b_marko	= wp->w_marko;
			bp->b_emode	= wp->w_emode;
			bp->b_binary	= wp->w_binary;
			bp->b_wide	= wp->w_wide;
		} else	{
			/* temporary test, should be removed
			if( bp != curbp )
				internalerror( ECSTR("bp != curbp") );
			*/
			bp->b_count     = bp->b_count - 1;
		}
	}

	wp->w_bufp = NULL;
	validitycheck();

	return( T );
}

/*
 *	Connect  the  buffer  "bp" to the window pointed by "wp".  If
 *	another  window  point  to  the same buffer copy mark and dot
 *	values in the buffer. This function is used internally.
 */

CMD
connectwindow( WINSCR *cwp, BUFFER *bp )
{
	WINSCR	*wp;

	(void)disconnectwindow( cwp );

	if( cwp == curwp )
		bufferontop( bp );

	cwp->w_bufp   = bp;
	cwp->w_linep  = bp->b_linep;		/* For macros, ignored. */
	cwp->w_flag  |= WFMODE|WFFORCE|WFHARD;	/* Quite nasty.		*/
	cwp->w_emode  = bp->b_emode;		/* Get old mode		*/
	cwp->w_binary = bp->b_binary;		/* Get old binary flag	*/
	cwp->w_wide   = bp->b_wide;		/* Get old wide flag	*/

	if( bp->b_count++ == 0 ) {
		/*
		 * First use, get the values from the buffer.
		 */
		cwp->w_dotp	= bp->b_dotp;
		cwp->w_doto	= bp->b_doto;
		cwp->w_markp	= bp->b_markp;
		cwp->w_marko	= bp->b_marko;
	} else	{

		/*
		 * get  the  current  values of the window already on
		 * screen.
		 */

		for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
			if( wp != curwp && wp->w_bufp == bp ) {
				cwp->w_dotp  = wp->w_dotp;
				cwp->w_doto  = wp->w_doto;
				cwp->w_markp = wp->w_markp;
				cwp->w_marko = wp->w_marko;
				break;
		}
	}

	validitycheck();
	return( T );
}

/*
 *	Make  newbp the current buffer and reorder the buffer list so
 *	that  kill-buffer and switch-buffer can select default buffer
 *	to kill or to use.
 */

void
bufferontop( BUFFER *newbp )
{
	BUFFER	*bp;

	/*
	 *	restack buffers (add the new buffer on top)
	 */

	if( newbp != bheadp ) {
		for( bp = bheadp ; bp != NULL ; bp=bp->b_bufp )
			if( bp->b_bufp == newbp ) {
				bp->b_bufp = newbp->b_bufp;
				break;
			}
		newbp->b_bufp	= bheadp;
		bheadp		= newbp;
	}

	curbp = newbp;
}

/*
 *	Show  an  internal  buffer  'bp'.  If  this  buffer in not on
 *	screen,  split  the  current  window  and  display  it.  This
 *	function  returns  the window that display the buffer or NULL
 *	if it fails.
 */

WINSCR	*
showbuffer( BUFFER *bp )
{
	WINSCR *wp;

	if( bp->b_count == 0 )	{ /* Not on screen yet.	*/
		if( (wp = wpopup()) == NULL )
			return( NULL );
	} else	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
			if( wp->w_bufp == bp )
				break;

	(void)connectwindow( wp, bp );

	return( wp );

}

/*
 *	Update all windows when a buffer change it's edit mode.
 */

CMD
updatemodes( void )
{
	WINSCR	*wp;
	int	flag = 0;

	if( curbp->b_count != 1 )	/* Ensure hard.		*/
		flag |= WFHARD;

	flag |= WFMODE;			/* update mode lines.	*/

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp->w_bufp == curbp ) {
			wp->w_flag  |= flag;
			wp->w_binary = curbp->b_binary;
			wp->w_wide   = curbp->b_wide;
			wp->w_emode  = curbp->b_emode;
		}

	return( T );
}

/*
 *	Long integer to ascii conversion (right justified).
 */

static	void
longtostrtr( EMCHAR *buf, int width, size_t num )
{
	buf[width] = 0;				/* End of string.	*/
	while( num >= 10 && width >= 2 ) {
		buf[--width] = (EMCHAR)((num%10) + '0');
		num /= 10;
	}
	buf[--width] = (EMCHAR)(num + '0');
	while( width )
		buf[--width] = ' ';
}

/*
 *	This  routine  rebuilds the text in the special secret buffer
 *	that holds the buffer list.  It is called by the list buffers
 *	command.  Return  T if everything works.  Return NIL if there
 *	is an error (if there is no memory).
 */

static	int
makelist( BUFFER *blp )
{
	EMCHAR	*cp1;
	EMCHAR	*cp2;
	BUFFER	*bp;
	EDLINE	*lp;
	long	nbytes;
	int	c;
	int	s;
	EMCHAR	len[ 6+1 ];
	EMCHAR	line[ NLINE ];

	blp->b_flag &= ~BFCHG;		/* Don't complain!	*/
	blp->b_fmode = 0;		/* Can't modify buffer	*/

	if( (s = bclear( blp )) != T )	/* Blow old text away	*/
		return( s );

	(void)emstrcpy( blp->b_fname, ECSTR("") );

	if(addline(blp,ECSTR(" MRBE   Size Buffer           Edit-Mode   File"))==NIL
	|| addline(blp,ECSTR(" ----   ---- ------           ---------   ----"))==NIL )
	  	return( NIL );

	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp ) {
		cp1 = &line[0];			/* Start at left edge	*/
		if( bp == curbp )
			*cp1++ = '.';
		else	*cp1++ = ' ';
		if( bp->b_flag & BFCHG )	/* "*" if changed	*/
			*cp1++ = '*';
		else	*cp1++ = ' ';
		if((bp->b_fmode & WACCESS)==0)	/* "%" if read only	*/
			*cp1++ = '%';
		else	*cp1++ = ' ';
		if( bp->b_binary )		/* "b" if binary mode	*/
			*cp1++ = 'b';		/* "b" if binary mode   */
		else	*cp1++ = ' ';
#if defined( _UNICODE )
		if( bp->b_wide == EMUTF16)	/* "w" if UTF-16 mode	*/
			*cp1++ = 'w';
		else if( bp->b_wide == EMUTF8)	/* "u" if UTF-8 mode	*/
			*cp1++ = 'u';
		else	*cp1++ = 'a';		/* "a" if ascii mode    */
#else
		*cp1++ = ' ';
#endif
		*cp1++ = ' ';			/* Gap.			*/
		nbytes = 0;			/* Count bytes in buf.	*/
		lp = firstline( bp );
		while( lp != lastline( bp ) ) {
			nbytes += llength( lp ) + 1;
			lp = lforw(lp);
		}
		/* 6 digit buffer size. */
		longtostrtr( len, 6, (size_t)nbytes * sizeof( EMCHAR ) );
		cp2 = &len[0];
		while( (c = *cp2++) != 0 )
			*cp1++ = (EMCHAR)c;
		*cp1++ = ' ';			/* Gap.			*/

		if( cp1 != (&line[0] + BUFFERPOS) ) {
			TTYbeep();
			WDGwrite( ECSTR("#<BUFFERPOS: invalid constant>") );
			TTYgetc();
		}

		cp2 = &bp->b_bname[0];		/* Buffer name		*/
		while( (c = *cp2++) != 0 )
			*cp1++ = (EMCHAR)c;

		while( cp1 - &line[5+1+6+1+NBUFN+1] )
			*cp1++ = ' ';		

		switch( bp->b_emode ) {
		case ASMODE	 : cp2 = ECSTR("Assembler   "); break;
		case BUFFERMODE	 : cp2 = ECSTR("Buffer Menu "); break;
		case CMODE	 : cp2 = ECSTR("C           "); break;
		case CPPMODE	 : cp2 = ECSTR("C++         "); break;
		case CSHARPMODE	 : cp2 = ECSTR("C#          "); break;
		case DIRED	 : cp2 = ECSTR("Dired       "); break;
		case FORTRANMODE : cp2 = ECSTR("Fortran     "); break;
		case JAVAMODE	 : cp2 = ECSTR("Java        "); break;
		case SGMLMODE	 : cp2 = ECSTR("SGML        "); break;
		case LISPMODE	 : cp2 = ECSTR("Lisp        "); break;
		case PASCALMODE	 : cp2 = ECSTR("Pascal      "); break;
		case PROLOGMODE	 : cp2 = ECSTR("Prolog      "); break;
		case PERLMODE	 : cp2 = ECSTR("Perl        "); break;
		case SHELLMODE	 : cp2 = ECSTR("Shell       "); break;
		default		 : cp2 = ECSTR("Fundamental ");
		}

		while( *cp2 )
			*cp1++ = *cp2++;

		cp2 = &bp->b_fname[ 0 ];	/* File name		*/

		if( *cp2 )
			while( (c = *cp2++) != 0 )
				if( cp1 < &line[ NLINE - 1 ] )
					*cp1++ = (EMCHAR)c;

		*cp1 = '\0';
		if( addline( blp, line ) == NIL )
			return( NIL );
	}
	return( T );
}

/*
 *	Look through the list of buffers.  Returns T if there are any
 *	changed  buffers.  Buffers that hold magic internal stuff are
 *	not  considered;  who  cares  if  the list of buffer names is
 *	hacked. Return NIL if no buffers have been changed.
 */

static CMD
savebname( EMCHAR *bname )
{
	BUFFER	*newbp;
	BUFFER	*oldbp	= curbp;
	int	res	= T;

	if( (newbp = bfind( bname, NIL, 0, 0 )) == NULL )
		return( NIL );

	curbp = newbp;
	if( filesave() != T )
		res = NIL;
	curbp = oldbp;

	return( res );
}

#define ANYHLP ECSTR("y = save, n = skip, ! = save all, . = save and exit, q = exit")

CMD
anycb( int flag )
{
	BUFFER	*bp;
	int	res     = NIL;
	int	alert   = NIL;
	int	saveall = NIL;

	for( bp = bheadp ; bp ; bp = bp->b_bufp ) {
		if( (bp->b_flag & BFCHG) == 0 )
			continue;

#if	defined( _IGNORE_SCRATCH )
		if( emstrcmp( bp->b_bname, BUF_SCRATCH ) == 0 )
			continue;
#endif

		if( flag == ANYCB_CHECK )
			return( T );

		if( (flag == ANYCB_PROMPT) || (confirm_unsaved_buffer == T) ) {
			EMCHAR	buf[ NFILEN ];
			int	valid = NIL;

			if( saveall == T ) {
				savebname( bp->b_bname );
				continue;
			}

			if( alert == NIL ) {
				alert = T; /* one beep only the first time */
				TTYbeep();
			}

			while( valid == NIL ) {
				(void)emstrcpy(buf,ECSTR("Save File "));
				(void)emstrcat(buf,bp->b_fname);
				(void)emstrcat(buf,ECSTR(" ? (y, n, !, ., q)"));

				WDGwrite( ECSTR("%s"), buf );
				switch( TTYgetc() ) {
				case 0x07:
					(void)ctrlg();
					WDGmessage( ECSTR("Quit") );
					return( ABORT );
				case ' ' :
				case 'y' :
				case 'Y' :
					valid   = T;
					(void)savebname( bp->b_bname );
					break;
				case 'n' :
				case 'N' :
					valid   = T;
					res     = T;
					break;
				case '!' :
					valid   = T;
					(void)savebname( bp->b_bname );
					saveall = T;
					break;
				case '.' :
					(void)savebname( bp->b_bname );
					return( res );
				case 'q' :
				case 'Q' :
				case 0x1B:
					return( res );
				default  :
					(void)ctrlg();
					WDGwrite( ANYHLP );
					waitmatch( 5 );
				}
			}
		} else	return( T );
	}

	return( res );
}

/*
 *	Find  a  buffer,  by  name.  Return  a  pointer to the BUFFER
 *	structure  associated with it.  If the named buffer is found,
 *	but is a TEMP buffer (like the buffer list) conplain.  If the
 *	buffer  is  not  found and the "cflag" is T,  create it.  The
 *	"bflag" is the settings for the flags in buffer.
 */

BUFFER	*
bfind( EMCHAR *bname, int cflag, int bflag, int mode )
{
	BUFFER *bp;
	EDLINE *lp;

	for( bp = bheadp ; bp ; bp = bp->b_bufp )
		if( emstrcmp( bname, bp->b_bname ) == 0 )
			return( bp );

	if( cflag != NIL ) {
		if( (bp = (BUFFER *)malloc( sizeof( BUFFER ) )) == NULL ) {
			WDGwrite( ECSTR("Can't create buffer %s"), bname );
			return( NULL );
		}

		if( (lp = lalloc( 0 )) == NULL ) {
			free( bp );
			return( NULL );
		}

		bp->b_bufp   = bheadp;
		bheadp	     = bp;
		bp->b_dotp   = lp;
		bp->b_doto   = 0;
		bp->b_markp  = NULL;
		bp->b_marko  = 0;
		bp->b_flag   = bflag;
		bp->b_count  = 0;
		bp->b_linep  = lp;
		bp->b_fmode  = WMODE;
		bp->b_emode  = (mode ? mode : FUNDAMENTAL);
		bp->b_binary = binary_mode;
		bp->b_wide   = 0;
		bp->b_time   = 0;
		(void)emstrcpy( bp->b_fname, ECSTR("") );
		(void)emstrcpy( bp->b_bname, bname );
		lforw( lp ) = lp;
		lback( lp ) = lp;
		return( bp );
	} else	return( NULL );

}

/*
 *	This  routine blows away all of the text in a buffer.  If the
 *	buffer  is  marked as changed then we ask if it is ok to blow
 *	it  away;  this is to save the user the grief of losing text.
 *	The  window chain is nearly always wrong if this gets called;
 *	the  caller  must  arrange for the updates that are required.
 *	Return T if everything looks good.
 */

int
bclear( BUFFER *bp )
{
	EDLINE	*lp;

	if( (bp->b_flag & BFCHG)!=0 && WDGyn(ECSTR("Discard changes? "))!=T )
		return( NIL );

	bp->b_flag  &= ~BFCHG;			/* Not changed		*/

	while( (lp = firstline( bp )) != lastline( bp ) )
		lfree( lp );

	bp->b_dotp  = bp->b_linep;		/* Fix "."		*/
	bp->b_doto  = 0;
	bp->b_markp = NULL;			/* Invalidate "mark"	*/
	bp->b_marko = 0;

	return( T );
}

/*
 *	Function to select a buffer from minibuffer
 */

static	EMCHAR	*
bufmatch( EMCHAR *prompt, EMCHAR *buffer )
{
	BUFFER *bp;
	size_t	len = (size_t)emstrlen( buffer );

	bp = bheadp;

	while( bp != NULL ) {
		if( len == 0 || !emstrncmp( bp->b_bname, buffer, len ) ) {
			WDGupdate( prompt, bp->b_bname );
			switch( TTYgetc() ) {
			case 0x07:
				completion = COMPLETE_ABORT;
				WDGwrite( ECSTR("Quit") );
				return( NULL );
			case 0x0D:
			case 0x0A:
			case 'y' :
			case 'Y' :
				return( bp->b_bname );
			}
		}
		bp = bp->b_bufp;
	}

	completion = COMPLETE_AGAIN;
	TTYbeep();
	return( buffer );
}

/*
 *	Attach a buffer to a window.  The values of dot and mark come
 *	from the buffer if the use count is 0.  Otherwise,  they come
 *	from some other window.
 */

CMD
usebuffer( void )
{
	BUFFER	*bp;
	BUFFER	*bp1;
	int	s;
	EMCHAR	bufn[ NBUFN ];
	EMCHAR	prompt[ NLINE ];

	complete.fn = bufmatch;

	bp1 = bheadp;
	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp )
		if( bp->b_count == 0 ) {
			/* Not on screen yet.	*/
			bp1 = bp;
			break;
		}

	(void)emstrcpy( prompt, ECSTR("Switch to buffer: (default ") );
	(void)emstrcat( prompt, bp1->b_bname );
	(void)emstrcat( prompt, ECSTR(") ") );

	bufn[ 0 ] = '\000';

	if( (s = WDGedit( prompt, bufn, NBUFN )) == ABORT )
		return( s );

	if( s == NIL )
		bp = bp1;
	else	if( (bp = bfind( bufn, NIL, 0, 0 )) == NULL )
			return( NIL );

	if( bp->b_count > 0 ) {
		/*
		 * buffer is already on screen
		 */
		if( usewindow( bp ) == T )
			return( T );
	}

	return( connectwindow( curwp, bp ) );

}

/*
 *	Process buffer commands 'd', 'f', 'k, 's', 'u', 'x' or TAB.
 */

static	BUFFER *
getbpcmd( EMCHAR *buf )
{
	/*
	 *	get buffer pointer, internally used by buffercmd
	 */

	int	j = BUFFERPOS;
	int	i;
	EMCHAR	c;

	if( llength( curwp->w_dotp ) < BUFFERPOS )
		return( NULL );

	for( i = 0 ; (c = lgetc( curwp->w_dotp, j++ )) != ' ' ; i++ )
		buf[ i ] = c;

	buf[ i ] = '\000';

	return( bfind( buf, NIL, 0, 0 ) );

}

CMD
buffercmd( int cmd )
{
	EMCHAR	buf[ NPAT ];
	EDLINE	*lp1;
	BUFFER	*bp;
	int	asked = 0;

	if( cmd != 'x' && cmd != 'n' && cmd != 'p' && cmd != 0x08 ) {
		lp1 = curwp->w_dotp;
		if( lp1 == lastline( curbp )  ||	/* At the end	    */
		    lp1 == firstline( curbp ) ||	/* At the beginning */
		    lp1 == lforw( firstline( curbp ) )	/* At 2nd line	    */
		  )
		    	return( ctrlg() );
	}

	switch( cmd ) {
	case 'f' :
		if( (bp = getbpcmd( buf )) == NULL )
			return( NIL );

		if( bp->b_count > 0 ) {
			/*
			 * buffer is already on screen
			 */
			if( usewindow( bp ) == T )
				return( T );
		}

		return( connectwindow( curwp, bp ) );
	case 'd':
	case 'k':
		if( llength( curwp->w_dotp ) == 0 )
			return( ctrlg() );

		curbp->b_fmode |= WACCESS;
		if( lgetc( curwp->w_dotp, 0 ) == 'D' )
			lputc( curwp->w_dotp, 0, ' ' );
		else	lputc( curwp->w_dotp, 0, 'D' );

		lchange( WFEDIT );
		(void)forwline();

		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;
		return( T );

	case 'n':
		return( forwline() );

	case 'p':
		return( backline() );

	case 's':
		if( llength( curwp->w_dotp ) == 0 )
			return( ctrlg() );

		curbp->b_fmode |= WACCESS;
		if( lgetc( curwp->w_dotp, 1 ) == 'S' )
			lputc( curwp->w_dotp, 1, ' ' );
		else	lputc( curwp->w_dotp, 1, 'S' );

		lchange( WFEDIT );
		(void)forwline();

		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;
		return( T );

	case 'u':
		if( (bp = getbpcmd( buf )) == NULL )
			return( NIL );

		curbp->b_fmode |= WACCESS;
		if( lgetc( curwp->w_dotp, 0 ) == 'D' )
			lputc( curwp->w_dotp, 0, ' ' );

		if( lgetc( curwp->w_dotp, 1 ) == 'S')
			lputc( curwp->w_dotp, 1, ' ' );

		if( (bp->b_fmode & WACCESS) == 0 )
			lputc( curwp->w_dotp, 2, '%' );
		else	lputc( curwp->w_dotp, 2, ' ' );

		lchange( WFEDIT );

		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;
		return( T );

	case '%':
		if( (bp = getbpcmd( buf )) == NULL )
			return( NIL );

		curbp->b_fmode |= WACCESS;

		if( lgetc( curwp->w_dotp, 2 ) == '%' )
			lputc( curwp->w_dotp, 2, ' ' );
		else	lputc( curwp->w_dotp, 2, '%' );

		lchange( WFEDIT );
		(void)forwline();
		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;

		return( T );

	case 'x':
		curbp->b_fmode |= WACCESS;
		(void)gotobob();
		(void)forwline();
		(void)forwline();

		do {
			if( (bp = getbpcmd( buf )) == NULL )
				continue;

			if( lgetc( curwp->w_dotp, 1 ) == 'S' ) {
				BUFFER	*oldbp = curbp;
				curbp = bp;
				if( filesave() == T ) {
					lputc( curwp->w_dotp, 1, ' ' );
					lchange( WFEDIT );
				}
				curbp = oldbp;
			}

			if( lgetc( curwp->w_dotp, 2 ) == '%' )
				bp->b_fmode &= ~WACCESS;
			else	bp->b_fmode |= WACCESS;

			/*
			 *	case delete should be the last one of the do
			 *	loop since it calls discardbuffer that makes
			 *	bp an invalid pointer.
			 */

			if( lgetc( curwp->w_dotp, 0 ) == 'D' ) {
				asked++;
				if( discardbuffer( bp ) == T ) {
				    (void)gotobol();
				    (void)ldelete(llength(curwp->w_dotp)+1,NIL);
				    lchange( WFEDIT );
				    (void)backline();
				    (void)gotobol();
				}
			}

		}  while( curwp->w_dotp != lastline(curbp) && forwline() == T );

		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;

		if( asked == 0 )
			WDGwrite( ECSTR("(No deletions requested)") );

		return( T );

	case 0x08 : /* ^H */
		if( backline() == T && gotobol() == T ) {
			if( (bp = getbpcmd( buf )) == NULL )
				return( NIL );

			curbp->b_fmode |= WACCESS;
			if( lgetc( curwp->w_dotp, 0 ) == 'D' ) {
				if( bp == curbp )
					lputc( curwp->w_dotp, 0, '.' );
				else	lputc( curwp->w_dotp, 0, ' ' );
				lchange( WFEDIT );
			}
			if( lgetc( curwp->w_dotp, 1 ) == 'S') {
				if( bp->b_flag & BFCHG )
					lputc( curwp->w_dotp, 1, '*' );
				else	lputc( curwp->w_dotp, 1, ' ' );
				lchange( WFEDIT );
			}
			if( lgetc( curwp->w_dotp, 2 ) == '%') {
				if( (bp->b_fmode & WACCESS) == 0 )
					lputc( curwp->w_dotp, 2, '%' );
				else	lputc( curwp->w_dotp, 2, ' ' );
				lchange( WFEDIT );
			}
			curbp->b_fmode &= ~WACCESS;
			curbp->b_flag  &= ~BFCHG;
		}
		return( T );

	default :
		return( ctrlg() );
	}
}

/*
 *	This  command  makes  the window with buffer "bp" the current
 *	window. Used internally by other functions.
 */

static CMD
usewindow( BUFFER *bp )
{
	WINSCR	*wp;

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
	 	if( wp->w_bufp == bp )
			return( setcurrentwindow( wp ) );

	return( NIL );
}

/*
 *	Dispose of a buffer,  by name.   Ask for the name. Look it up
 *	(don't  get too upset if it isn't there at all!).  Get  quite
 *	upset if the buffer is being displayed. Clear the buffer (ask
 *	if  the buffer has been changed).  Then free the header  line
 *	and the buffer header. Bound to "C-X K".
 */

CMD
killbuffer( void )
{
	BUFFER	*bp;
	int	s;
	EMCHAR	bufn[ NBUFN ];
	EMCHAR	prompt[ NLINE ];

	complete.fn = bufmatch;

	(void)emstrcpy( prompt, ECSTR("Kill buffer: (default ") );
	(void)emstrcat( prompt, curbp->b_bname );
	(void)emstrcat( prompt, ECSTR(") ") );

	bufn[ 0 ] = '\000';

	if( (s = WDGedit( prompt, bufn, NBUFN )) == ABORT )
		return( s );

	if( (s == NIL) )
		bp = curbp;
	else	if( (bp = bfind( bufn, NIL, 0, 0 )) == NULL )
			return( NIL );

	return( discardbuffer( bp ) );
}

/*
 *	Routine  that  really  implement  the kill-buffer,  'bp' is a
 *	valid pointer to a buffer.
 */

static CMD
discardbuffer( BUFFER *bp )
{
	BUFFER	*bp1;
	BUFFER	*bp2;
	WINSCR	*wp;
	EMCHAR	buf[ NLINE ];
	int	s;

	(void)emstrcpy( buf, ECSTR("Discard changes made to buffer ") );
	(void)emstrcat( buf, bp->b_bname );
	(void)emstrcat( buf, ECSTR(" ? ") );

	if( (bp->b_flag & BFCHG ) != 0 ) {
		if( WDGyn( buf ) != T )
			return( NIL );
		else	bp->b_flag &= ~BFCHG;	/* Don't complain! */
	}

	/*
	 * Try  to  find in bp1 the first buffer not displayed or use
	 * the first buffer in the list which is not bp.
	 */

	bp1 = bp;

	for( bp2 = bheadp ; bp2 != NULL ; bp2 = bp2->b_bufp ) {
		if( bp2 == bp )
			continue;
		if( bp2->b_count == 0 ) {
			bp1 = bp2; /* not displayed best guest */
			break;
		}
		if( bp1 == bp )
			bp1 = bp2; /* bp2 is a better guest    */
	}

	if( bp1 == bp ) {
		/*
		 * Only one buffer
		 */
		TTYbeep();
		WDGmessage( ECSTR("Only one buffer") );
		return( NIL );
	}

	/*
	 * Replace   "bp"  with  "bp1"  for  windows  all  on  screen
	 * containing buffer "bp".
	 */

	for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
		if( wp->w_bufp == bp )
			(void)connectwindow( wp, bp1 );

	if( (s = bclear( bp )) != T )
		return( s );

	cfree( bp->b_linep );			/* Release header line. */

	bp1 = NULL;				/* Find the header.	*/
	bp2 = bheadp;
	while( bp2 != bp ) {
		bp1 = bp2;
		bp2 = bp2->b_bufp;
	}

	bp2 = bp2->b_bufp;			/* Next one in chain.	*/
	if( bp1 == NULL )			/* Unlink it.		*/
		bheadp = bp2;
	else	bp1->b_bufp = bp2;

	free( bp );				/* Release buffer block */
	return( T );
}

/*
 *	List  all  of  the  active buffers.  First update the special
 *	buffer that holds the list.  Next make sure at least 1 window
 *	is  displaying the buffer list,  splitting the screen if this
 *	is  what  it takes.  Lastly,  repaint all of the windows that
 *	are displaying the list. Bound to "C-XC-B".
 */

CMD
listbuffers( void )
{
	BUFFER	*bp;
	int	s;

	if( (bp = bfind( BUF_LIST, T, NIL, BUFFERMODE )) == NULL )
		return( NIL );

	if( (s = makelist( bp )) != T )
		return( s );

	return( (showbuffer( bp ) == NULL) ? T : NIL );

}
