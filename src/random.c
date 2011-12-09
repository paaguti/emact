#if	!defined( lint )
static	char rcsid[] = "$Id: random.c,v 1.8 2008/06/20 09:25:13 jullien Exp $";
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
 *	This  file  contains  the  command  processing functions for a
 *	number  of  random  commands.  There is no functional grouping
 *	here, for sure.
 */

#include	"emacs.h"

static	int	_define(getccol,(void));
static	CMD	_define(addprefix,(void));
static	CMD	_define(prefixlinep,(EDLINE *line, int len));

/*
 *	Display  the current position of the cursor,  in origin 1 X-Y
 *	coordinates,  the  character  that  is  under  the cursor (in
 *	octal),  and  the  fraction  of  the  text that is before the
 *	cursor.  The displayed column is not the current column,  but
 *	the  column  that would be used on an infinite width display.
 *	Normally this is bound to "C-X =".
 */

CMD
showcpos( void )
{
	EDLINE	 *clp	= firstline( curbp );
	long	 nch	= 0;
	int	 cbo	= 0;
	int	 cac	= MAX_EMCHAR;
	int	 nbl	= 1;
	int	 col	= getccol();
	long	 nbc	= 0L;

	for( ;; ) {
		if( clp == curwp->w_dotp && cbo == curwp->w_doto ) {
			nbc = nch;
			if( cbo == llength( clp ) )
				cac = (int)'\n';
			else	cac = (int)((unsigned int)lgetc( clp, cbo ));
		}
		if( cbo == llength( clp ) ) {
			if( cac == MAX_EMCHAR )
				++nbl;
			if( clp == lastline( curbp ) )
				break;
			clp = lforw( clp );
			cbo = 0;
		} else	++cbo;
		++nch;
	}

	cac &= MAX_EMCHAR;

	if( cac < 32 ) {
	   WDGwrite(ECSTR("at line %d: col=%d row=%d code=(%d, %x) (%d%%)"),
		    nbl, col, currow, cac, cac,
		    (nch==0)?0:(int)((100*nbc)/nch));
	} else {
	   EMCHAR buf[2];
	   buf[0] = (EMCHAR)cac;
	   buf[1] = '\000';
	   WDGwrite(ECSTR("at line %d: col=%d row=%d char='%s' code=(%d, 0x%x) (%d%%)"),
		    nbl, col, currow, buf, cac, cac,
		    (nch==0)?0:(int)((100*nbc)/nch));
	}

	return( T );
}

/*
 *	Return current column.
 */

static	int
getccol( void )
{
	int c;
	int i;
	int col = 0;

	for( i = 0 ; i < curwp->w_doto ; ++i ) {
		c = lgetc( curwp->w_dotp, i );
		if( c == '\t')
			do
				++col;
			while( col % tab_display );
		else	{
			if( c < 0x20 || c == 0x7F )
				++col;
			++col;
		}
	}

	return( col );
}

/*
 *	Twiddle  the two characters on either side of dot.  If dot is
 *	at  the end of the line twiddle the two characters before it.
 *	Return  with  an error if dot is at the beginning of line; it
 *	seems to be a bit pointless to make this work.  This fixes up
 *	a  very  common typo with a single stroke.  Normally bound to
 *	"C-T".  This always works within a line,  so "WFEDIT" is good
 *	enough.
 */

CMD
twiddle( void )
{
	EDLINE	*dotp	= curwp->w_dotp;
	int	doto	= curwp->w_doto;
	int	cl;
	int	cd;

	if( freadonly() )
		return( NIL );

	/*
	 * if dot is at the end, backchar
	 */

	if( doto == llength( dotp ) )
		doto--;

	/*
	 * if dot is at the beginning, nothing to do.
	 */

	if( doto <= 0 )
		return( NIL );

	cd = lgetc( dotp, doto   );
	cl = lgetc( dotp, doto-1 );
	lputc( dotp, doto-1, cd );
	lputc( dotp, doto,   cl );

	if( curwp->w_doto < llength( dotp ) )
		(void)forwchar();

	lchange( WFEDIT );

	return( T );
}

/*
 *	Quote the next character,  and insert it into the buffer. All
 *	the  characters  are  taken literally,  with the exception of
 *	the  newline,  which  always  has its line splitting meaning.
 *	The  character  is  always  read,  even  if  it is inserted 0
 *	times,  for regularity. Bound to "C-Q" (for Rich, and only on
 *	systems that don't need XON-XOFF).
 */

CMD
quotechar( void )
{
	int	n = repeat;
	int	s;
	int	c;

	if( (c = TTYgetc()) == '\n' ) {
		while( (s = lnewline()) == T && --n )
			;
		return( s );
	}
	return( linsert( n, c ) );
}

/*
 *	Set  tab  size  if  given  non-default  argument  (n  <>  1).
 *	Otherwise,  insert a tab into file.  If given argument, n, of
 *	zero,  change to true tabs. If n > 1, simulate tab stop every
 *	n-characters  using  spaces.  This  has  to  be  done in this
 *	slightly  funny  way  because  the  tab  (in  ASCII) has been
 *	turned into "C-I" (in 10 bit code) already. Bound to "C-I".
 */

CMD
tab( void )
{
	int	s;

	if( repeat == 0 ) {
		tab_size = 8;	/* restore to default */
		return( T );
	}

	if( repeat != 1 ) {
		tab_size = repeat;
		return( T );
	}

	if( lastflag & CFTAB ) {
		/*
		 * Last indent fails, force a tab.
		 */
		thisflag &= ~CFTAB;
		return( tabexpand() );
	}

	if( (curwp->w_emode != FUNDAMENTAL) && (curwp->w_doto == 0) ) {
		s = tabindent();
		if( curwp->w_doto == 0 )
			/*
			 * Intdentation is still at 0 after trying to indent
			 */
			thisflag |= CFTAB;
		return( s );
     	}

	return( tabexpand() );
}

CMD
tabexpand( void )
{
	if( tab_size==8 &&
	    curwp->w_emode!=JAVAMODE &&
	    curwp->w_emode!=CSHARPMODE )
		return( linsert( 1, '\t' ) );
	else	return( linsert( tab_size - (getccol() % tab_size), ' ' ) );
}

/*
 *	Open  up  some blank space.  The basic plan is to insert a bunch of
 *	newlines  and  then  back  up over them.  Everything is done by the
 *	subcommand  processors.  They  even  handle the looping.  When this
 *	command  is  executed at the beginning of the a line it also insert
 *	the fill-prefix when it this one is defined. Normally this is bound
 *	to "C-O".
 */

CMD
openline( void )
{
	int	i;
	int	j;

	for( i = 0 ; i < repeat ; i++ ) {
		if( curwp->w_doto == 0 && fill_prefix[ 0 ] )
			for( j = 0 ; fill_prefix[ j ] ; j++ )
				if( linsert( 1, fill_prefix[ j ] ) == NIL )
					return( NIL );
		if( lnewline() != T )
			return( NIL );
	}

	if( backline() != T )
		return( NIL );

	return( gotoeol() );
}

/*
 *	Insert a newline.  Bound to "C-M".  Simply insert new line or
 *	indent  depending  of  selected  mode (Fundamental,  C/C++ or
 *	Lisp).
 */

CMD
endline( void )
{
	EMCHAR	buf[ NFILEN ];

	switch( curwp->w_emode ) {
	case CMODE	:
	case CPPMODE	:
	case CSHARPMODE	:
	case SGMLMODE   :
	case JAVAMODE	:
	case FORTRANMODE:
	case PASCALMODE :
	case PROLOGMODE	:
	case PERLMODE   :
	case SHELLMODE  :
	case LISPMODE	: return( newlineindent());
	case DIRED	: if( curwp->w_dotp == lastline( curbp ) ||
			      llength( curwp->w_dotp ) == 0 )
				return( ctrlg() );
			  (void)emstrncpy(
					 buf,
					 ltext( curwp->w_dotp ),
					 (size_t)llength( curwp->w_dotp )
				       );
			  buf[ llength( curwp->w_dotp ) ] = '\000';
			  return( newfile( buf + DIREDMARK ) );
	}
	return( newline() );
}

/*
 *	Insert  a newline.  Bound to "C-M".  If you are at the end of
 *	the  line  and the next line is a blank line,  just move into
 *	the  blank line.  This makes "C-O" and "C-X C-O" work nicely,
 *	and  reduces  the  ammount  of  screen  update that has to be
 *	done.  This  would not be as critical if screen update were a
 *	lot more efficient.
 */

CMD
newline( void )
{
	EDLINE	*lp;
	int	s;
	int	n = repeat;

	while( n-- ) {
		lp = curwp->w_dotp;
		if( llength(lp)==(short)curwp->w_doto &&
		    lp!=lastline( curbp ) && !llength(lforw(lp)) ) {
			if( (s = forwchar()) != T )
				return( s );
		} else	if( (s = lnewline()) != T )
			return( s );
	}
	return( T );
}

/*
 *	Delete  blank  lines  around  dot.  What  this  command  does
 *	depends if dot is sitting on a blank line.  If dot is sitting
 *	on  a  blank  line,  this command deletes all the blank lines
 *	above  and below the current line.  If it is sitting on a non
 *	blank  line  then it deletes all of the blank lines after the
 *	line.  Normally  this  command  is  bound  to "C-X C-O".  Any
 *	argument is ignored.
 */

CMD
deblank( void )
{
	EDLINE	*lp1;
	EDLINE	*lp2;
	int	nld = 0;

	lp1 = curwp->w_dotp;

	while( llength( lp1 ) == 0 && (lp2=lback( lp1 )) != lastline( curbp ) )
		lp1 = lp2;

	for( lp2 = lp1 ; (lp2=lforw(lp2))!=lastline(curbp)&&llength(lp2)==0 ; )
		++nld;

	if( nld == 0 )
		return( T );

	curwp->w_dotp = lforw(lp1);
	curwp->w_doto = 0;

	return( ldelete( nld, NIL ) );
}

/*
 *	Delete forward.  This is real easy,  because the basic delete
 *	routine does all of the work.  If any argument is present, it
 *	kills  rather than deletes,  to prevent loss of text if typed
 *	with a big argument. Normally bound to "C-D".
 */

CMD
forwdel( void )
{
	return( ldelete( repeat, NIL ) );
}

/*
 *	Delete  backwards.  This is quite easy too,  because it's all
 *	done  with  other functions.  Just move the cursor back,  and
 *	delete  forwards.  Like delete forward,  this actually does a
 *	kill  if  presented with an argument.  Bound to both "RUBOUT"
 *	and "C-H".
 */

CMD
backdel( void )
{
	int	s;

	if( ((curbp->b_fmode & WACCESS) == 0) ) {
		if( curbp->b_emode == BUFFERMODE )
			return( buffercmd( 0x08 ) );

		if( curbp->b_emode == DIRED )
			return( diredcmd( 0x08 ) );
	}

	if( curbp->b_emode!=FUNDAMENTAL && repeat==1 && curwp->w_doto>0 ) {
		/*
		 * Try to delete past a tab, expand tab before deleting.
		 */
		if( lgetc( curwp->w_dotp, curwp->w_doto-1 ) == '\t' ) {
			int pos;

			/*
			 *	Delete 'tab'
			 */

			if( backchar() == T )
				(void)ldelete( 1, NIL );

			pos = getccol();

			/*
			 *	Replace by blanks
			 */

			do
				(void)linsert( 1, ' ' );
			while( ++pos % tab_display );
		}
	}

	if( (s = backchar()) == T )
		s = ldelete( repeat, NIL );

	return( s );
}

/*
 *	Kill text.  If called without an argument,  it kills from dot
 *	to the end of the line,  unless it is at the end of the line,
 *	when  it kills the newline.  If called with an argument of 0,
 *	it kills from the start of the line to dot.  If called with a
 *	positive  argument,  it  kills  from  dot  forward  over that
 *	number of newlines.
 */

CMD
killtext( void )
{
	int	chunk;

	if( (lastflag & CFKILL) == 0)		/* Clear kill buffer if */
		kdelete();			/* last wasn't a kill.	*/

	thisflag |= CFKILL;

	chunk = llength( curwp->w_dotp ) - curwp->w_doto;

	if( chunk == 0 )
		chunk = 1;

	return( ldelete( chunk, T ) );
}

/*
 *	Yank  text  back  from the kill buffer.  This is really easy.
 *	All of the work is done by the standard insert routines.  All
 *	you  do  is  run  the  loop,  and check for errors.  Bound to
 *	"C-Y".  The blank lines are inserted with a call to "newline"
 *	instead  of a call to "lnewline" so that the magic stuff that
 *	happens  when  you type a carriage return also happens when a
 *	carriage  return  is  yanked back from the kill buffer.  As a
 *	special  case  we check if yank is done at the end of buffer.
 *	If so,  insert push a new line at the end of buffer that must
 *	be removed.
 */

CMD
yank( void )
{
	int	n	   = repeat;
	int	save	   = repeat;
	int	lastlflag  = (curwp->w_dotp == lastline( curbp ));
	int	firstlflag = (curwp->w_dotp == firstline( curbp ));

	repeat = 1;

	WDGclippaste();

	while( n-- ) {
		int	c;
		int	i;

		for( i = 0 ; (c = kremove( i )) >= 0 ; i++ )
			if( c == '\n' ) {
				if( lnewline() == NIL )
					return( NIL );
			} else	if( linsert( 1, c ) == NIL )
					return( NIL );
	}

	repeat = save;

	if( lastlflag ) {
		/*
		 * yank  was done at the end of the buffer,  kill the
		 * last char
		 */

		(void)forwdel();

		if( firstlflag ) {
			WINSCR	*wp;

			/*
			 * yank was done on an empty buffer, force redisplay
			 * for all active buffer.
			 */

			for( wp = wheadp ; wp != NULL ; wp = wp->w_wndp )
				if( wp->w_bufp == curbp )
					wp->w_flag |= WFFORCE;
		}
	}

	return( T );
}

/*
 *	Position  the CFKILL flag for the variable "thisflag" so that
 *	the  next  command can append text to the kill buffer.  Bound
 *	to M-C-W.
 */

CMD
appendnextkill( void )
{
	WDGmessage( ECSTR("If the next command is a kill, it will append") );
	thisflag |= CFKILL;
	return( T );
}

/*
 *	Internal  function that returns T if the line passed as first
 *	argument  match  the  prefix  string  of length 'len' and NIL
 *	otherwise.
 */

static	CMD
prefixlinep( EDLINE *line, int len )
{
	int l = llength( line );

	if( l == 0 || l < len )
		return( NIL );
	
	if( emstrncmp( ltext( line ), fill_prefix, (size_t)len ) == 0 )
		return(  T  );
	else	return( NIL );
}

/*
 *	Set  the  fill-column at the value of cursor current location
 *	or to the value set by the repeat command. Bound to C-X-F.
 */

CMD
setfillcolumn( void )
{
	int newfill = ((repeat == 1 ) ? curwp->w_doto : repeat);

	if( newfill < 3 ) {
		WDGwrite( ECSTR("fill-column can't be less than 3") );
		return( NIL );
	}

	fill_column = newfill;

	if( kbdmop == NULL )
		WDGwrite( ECSTR("fill-column set to %d"), fill_column );

	return( T );
}

/*
 *	Set  the  fill-prefix from the text beginning the line and up
 *	to the value of cursor current location. Bound to C-X-.
 */

CMD
setfillprefix( void )
{
	int	i;

	if( curwp->w_doto == 0 ) {
		if( kbdmop == NULL )
			WDGwrite( ECSTR("fill-prefix cancelled.") );
		
		fill_prefix[ 0 ] = '\0';
		return( T );
	}

	for( i = 0 ; i < curwp->w_doto ; i++ )
		fill_prefix[ i ] = lgetc( curwp->w_dotp, i );

	fill_prefix[ i ] = '\0';

	if( kbdmop == NULL )
		WDGwrite( ECSTR("fill-prefix: \"%s\""), fill_prefix );

	return( T );
}

/*
 *	Add  the  fill-prefix  string  to the current line and remove
 *	spaces at the beginning of the line.
 */

static CMD
addprefix( void )
{
	int	i;
	int	len	= emstrlen( fill_prefix );

	curwp->w_doto = 0;

	if( prefixlinep( curwp->w_dotp, len ) == NIL )
	    	for( i = 0 ; fill_prefix[ i ] ; i++ )
			if( linsert( 1, fill_prefix[ i ] ) != T )
				return( NIL );

	curwp->w_doto = len;

	while( (curwp->w_doto < llength( curwp->w_dotp )) && lgetdot() == ' ' )
		(void)ldelete( 1, NIL );

	return( T );
}

/*
 *	Fill  paragraph.  Insert  the fill-prefix string on each line
 *	of  the  current paragraph that does not currently start with
 *	this  prefix  and  limit  the  line  length  to  the value of
 *	fill-column variable. Bound to M-Q.
 */

CMD
fillparagraph( void )
{
	int	len;
	int	nbspace;
	int	oldmode;

	if( freadonly() )
		return( NIL );

	len		= emstrlen( fill_prefix );
 	oldmode		= curwp->w_emode;
	curwp->w_emode	= FUNDAMENTAL;

	(void)backparagraph();

	curwp->w_doto = 0;
	if( prefixlinep( curwp->w_dotp, len ) == T )
		(void)ldelete( len, NIL );

	/*
	 *	Convert  paragraph in only one line removing previous
	 *	prefix.
	 */

	while( llength( lforw( curwp->w_dotp ) ) > len &&
	       prefixlinep( lforw( curwp->w_dotp ), len ) ) {

		(void)gotoeol();
		(void)ldelete( 1, NIL );
		(void)linsert( 1, ' ' );
		(void)ldelete( len, NIL );
	}

	/*
	 *	Remove duplicated spaces
	 */

	curwp->w_doto = 0;
	while( curwp->w_doto < llength( curwp->w_dotp ) ) {
		if( lgetdot() == ' ' )
			nbspace = 1;
		else	nbspace = 0;

		(void)forwchar();

		if( nbspace )
			while( curwp->w_doto < llength( curwp->w_dotp ) ) {
				if( lgetdot() != ' ' )
					break;
				(void)ldelete( 1, NIL );
			}
	}

	/*
	 *	Split   this   line   within   the   fill_column  and
	 *	fill_prefix.
	 */

	(void)addprefix();

	curwp->w_doto = 0;
	while( lposition(curwp->w_dotp)>=fill_column )
		if( splitlinetofill() != T )
			break;

	(void)gotoeol();

	/*
	 *	Reset  the  curgoal  to  the  current  position  (any
	 *	better solution ?)
	 */

 	curcol = curgoal = getccol();

	curwp->w_emode = oldmode;

	return( T );
}

/*
 *	Internal   function   used  to  split  the  current  line  to
 *	fill-column.  Go  back  to the first non-space character less
 *	than  the  value  of  fill-column and break the current line.
 *	Then the fill-prefix is added on the new line.
 */

CMD
splitlinetofill( void )
{
	int	lmax = llength( curwp->w_dotp ); /* max position	*/
	int	bpos = 0;			 /* break position	*/
	int	dpos = 0;			 /* display position	*/
	int	fpos = 0;			 /* forced break	*/
	int	fflg = 1;			 /* fill prefix flag	*/
	int	i;

	/*
	 *	Compute the break position in 'bpos'.
	 */
		
	for( i = 0 ; i < lmax ; i++ ) {
		EMCHAR	c = lgetc( curwp->w_dotp, i );

		if( c == '\t' )
			do
				++dpos;
			while( dpos % tab_display );
		else	++dpos;

		/*
		 *	Break pos should be after the fill prefix
		 */

		if( fflg )
			fflg = (c == fill_prefix[i]);

		if( c == ' ' && fflg == 0 ) {
			if( dpos < fill_column )
				bpos = i;
			else	if( fpos == 0 )
					fpos = i;
		}
	}

	if( bpos == 0 && fpos == 0 )
		fpos = lmax - 1;

	if( dpos >= fill_column ) {
		if( (fpos - bpos) > fill_column )
			/*
			 * Event  a break at this point will not make
			 * the next line fit in fill_column.  Force a
			 * break at fpos.
			 */
			curwp->w_doto = fpos;
		else	curwp->w_doto = bpos;

		(void)endline();
		if( justmode != JUSTLEFT ) {
			/*
			 * assumes full-justify
			 */
			(void)backline();
			if( curwp->w_emode == SGMLMODE )
				(void)justifycomment();
			else	(void)justifycurline();
			(void)forwline();
		}
		(void)addprefix();
	}

	return( T );
}

/*
 *	Justify   current   line  by  adding  space  to  fill  up  to
 *	fill-column.  It  assumes  that  the  current  line length is
 *	already less than the value of fill-column.
 */

CMD
justifycurline( void )
{
	int	jutifyed;
	int	maxspace;
	int	fillmax;
	int	len;

	if( lposition( curwp->w_dotp ) == fill_column )
		return( T );

	len	= emstrlen( fill_prefix );
	fillmax = fill_column - 1;

	curwp->w_doto = 0;

	if( prefixlinep( curwp->w_dotp, len ) )
		/*
		 * Current  line  have  prefix,  move  to  the first character
		 * after the prefix string.
		 */
		curwp->w_doto = len;

	/*
	 *	Try to add spaces after '.' and ',' first.
	 */

	while( curwp->w_doto < llength( curwp->w_dotp ) ) {
		EMCHAR	c = lgetdot();

		if( c == '.' || c == ',' ) {
			(void)forwchar();
			if( curwp->w_doto < llength( curwp->w_dotp ) - 1 &&
			    lgetdot() == ' ' )
				(void)linsert( 1, ' ' );
		} else	(void)forwchar();

		if( lposition( curwp->w_dotp ) >= fillmax )
			return( T );
	}

	/*
	 *	Add spaces up to fill_column.
	 */

	jutifyed = T;
	maxspace = 1;

	while( lposition( curwp->w_dotp ) < fillmax && jutifyed == T ) {
		jutifyed = NIL;

		maxspace++;

		curwp->w_doto = 0;

		if( prefixlinep( curwp->w_dotp, len ) )
			/*
			 *	Current  line  have  prefix,  move to
			 *	the  first character after the prefix
			 *	string.
			 */
			 curwp->w_doto = len;

		while( curwp->w_doto < llength( curwp->w_dotp ) ) {
			if( lgetdot() == ' ' ) {
				int	nbspace = 0;

				do	{
					if( lgetdot() != ' ' )
						break;
					(void)forwchar();
					nbspace++;
				} while(curwp->w_doto<llength(curwp->w_dotp));

				if( nbspace < maxspace ) {
					(void)linsert( 1, ' ' );
					jutifyed = T;
				}
			} else	(void)forwchar();

			if( lposition( curwp->w_dotp ) >= fillmax )
				return( T );
		}
	}

	return( T );
}

/*
 *	Move to the beginning of paragraph. Bound to M-{
 */

CMD
backparagraph( void )
{
	int	len = emstrlen( fill_prefix );

	while(
	       lback( curwp->w_dotp ) != lastline( curbp ) &&
	       backline() == T &&
	       llength( curwp->w_dotp ) > len &&
	       prefixlinep( curwp->w_dotp, len ) == T
	     )
	       /* empty loop*/
	       ;

	if( curwp->w_dotp != firstline( curbp ) )
		(void)forwline();

	if( llength( curwp->w_dotp ) > len )
		curwp->w_doto = len;
	else	curwp->w_doto = 0;

	return( T );
}

/*
 *	Move to the end of paragraph. Bound to M-}
 */

CMD
forwparagraph( void )
{
	int	len = emstrlen( fill_prefix );

	while( curwp->w_dotp != firstline( curbp ) &&
	       forwline() == T &&
	       llength( curwp->w_dotp ) > len &&
	       prefixlinep( curwp->w_dotp, len ) == T
	     )
	       /* empty loop*/
	       ;

	if( curwp->w_dotp != lastline( curbp ) )
		(void)backline();

	(void)gotoeol();

	return( T );
}

/*
 *	Mark the paragraph. Bound to M-h
 */

CMD
markparagraph( void )
{
	(void)backparagraph();
	(void)setmark();
	(void)forwparagraph();

	return( T );
}

/*
 *	Set  the  justification mode to 'left' and adjust the current
 *	paragraph using this mode. Unbound.
 */

CMD
setjustifyleft( void )
{
	justmode = JUSTLEFT;

	return( fillparagraph() );
}

/*
 *	Set  the  justification mode to 'full' and adjust the current
 *	paragraph using this mode. Unbound.
 */

CMD
setjustifyfull( void )
{
	justmode = JUSTFULL;

	return( fillparagraph() );
}

/*
 *	Justify a comment (for programs like C/C++ ..).  Start at the
 *	beginning  of  the  line  and find the first character of the
 *	comment.  At this point,  set the fill prefix and justify the
 *	current paragraph. Bound to to M-;
 */

CMD
justifycomment( void )
{
	static	EMCHAR	skip[] = { ' ', '\t', '#', '*', '/', '-', ';', 0 };

	int	i;

	curwp->w_doto = 0;

	while( curwp->w_doto < llength( curwp->w_dotp ) ) {
		EMCHAR	c = lgetdot();

		for( i = 0 ; skip[ i ] ; i++ )
			if( c == skip[ i ] )
				break;

		if( skip[ i ] == 0 )
			break;

		(void)forwchar();

	}

	if( curwp->w_doto >= fill_column )
		/*
		 *	Too hard, fill-prefix > fill-column
		 */
		return( NIL );

	for( i = 0 ; i < curwp->w_doto ; i++ )
		fill_prefix[ i ] = lgetc( curwp->w_dotp, i );

	fill_prefix[ i ] = '\0';
	(void)fillparagraph();
	fill_prefix[ 0 ] = '\0';

	return( T );
}

/*
 *	Insert  the  current  value  of counter at dot before incrent
 *	it. Bound to C-X-$-$
 */

static	int	cntval		= 0;
static	EMCHAR	cntfmt[ NPAT ]	= { '%', 'd', 0 };

CMD
counterinsert( void )
{
	EMCHAR	buf[ NPAT ];
	EMCHAR	*s;

	(void)emsprintf1( buf, &cntfmt[0], cntval );

	for( s = (EMCHAR *)&buf[ 0 ] ; *s ; s++ )
		(void)linsert( 1, *s );

	return( T );
}

/*
 *	Increment counter. Bound to C-X-$-'+'
 */

CMD
counterincr( void )
{
	cntval += repeat;

	return( T );
}

/*
 *	Decrement counter. Bound to C-X-$-'-'
 */

CMD
counterdecr( void )
{
	cntval -= repeat;

	return( T );
}

/*
 *	Set the value of counter. Bound to C-X-$-S
 */

CMD
counterset( void )
{
	cntval = repeat;

	return( T );
}

/*
 *	Change the format of counter from default (%d). Bound to C-X-$-F
 */

CMD
counterformat( void )
{
	int	s;

	if( (s = WDGedit( ECSTR("Counter format: "), cntfmt, NPAT )) != T )
		return( s );

	return( T );
}

/*
 *	No undo yet !
 */

CMD
undo( void )
{
	TTYbeep();
	WDGwrite( ECSTR("'undo' not yet implemented ! Sorry.") );
	return( NIL );
}

/*
 *	Force an error to go to debugger.
 */

CMD
enterdebug( void )
{
	int	*p;
	int	*bug = NULL;

	if( repeat == 666 ) {
		p  = bug;
		*p = 1;
	}

	WDGwrite( ECSTR("enter-debug needs to be called with 666.") );
	return( NIL );
}
