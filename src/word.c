#if	!defined( lint )
static	char rcsid[] = "$Id: word.c,v 1.3 2008/06/19 12:13:33 jullien Exp $";
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
 *	The  routines  in this file implement commands that work word
 *	at a time. There are all sorts of word mode commands.
 */

#include	"emacs.h"

/*
 *	Return  T  if  the  character  at  dot is a character that is
 *	considered  to be part of a word.  The word character list is
 *	hard coded. Should be setable.
 */

int
inword( void )
{
	int	c;

	if( curwp->w_dotp == lastline( curbp ) ||
	    curwp->w_doto == llength( curwp->w_dotp )
	  )
		return( NIL );

	c = lgetdot();

	/*
	 * Check for alphanumeric or '$' or '_'
	 */

	if( isalnum( c ) || (c == '$') || (c == '_') )
		return( T );

	/*
	 * Lisp mode add few extra valid characters
	 */

	if( curwp->w_emode == LISPMODE )
		switch( c ) {
		case '-' :
		case '+' :
		case '*' :
		case '%' :
			return( T );
		}

#if	!defined( __MVS__ )
	if( c & 0x80 )
		return( T );
#endif

	return( NIL );
}

/*
 *	Returns word at cursor in buffer 'buf'. Used internally.
 */

CMD
wordatcursor( EMCHAR *buf, int len )
{
	int	cbo = curwp->w_doto;
	int	i   = 0;

	/*
	 *	At the end of the word ??
	 */

	if( inword() == NIL && curwp->w_doto > 0 )
		(void)backchar();

	if( inword() == NIL ) {
		curwp->w_doto    = cbo;
		return( NIL );
	}

	if( (forwword() == T) && (backword() == T) ) {
		while( inword() == T && i < (len-1) ) {
			buf[ i++ ] = lgetdot();
			if( forwchar() == NIL )
				break;
		}

		buf[ i ] = '\000';
		curwp->w_doto    = cbo;
		return( T );
	}

	curwp->w_doto    = cbo;
	return( T );

}

/*
 *	Move the cursor backward by "n" words.  All of the details of
 *	motion   are  performed  by  the  "backchar"  and  "forwchar"
 *	routines. Error if you try to move beyond the buffers.
 */

CMD
backword( void )
{
	int n = repeat;

	if( backchar() == NIL )
		return( NIL );

	while( n-- ) {
		while( inword() == NIL )
			if( backchar() == NIL )
				return( NIL );

		while( inword() != NIL )
			if(curwp->w_dotp==firstline(curbp) && curwp->w_doto==0)
				return( T ); /* start of buffer */
			else	if( backchar() == NIL )
				return( NIL );
	}

	return( forwchar() );

}

/*
 *	Move the cursor forward by the specified number of words. All
 *	of  the  motion is done by "forwchar".  Error if you try  and
 *	move beyond the buffer's end.
 */

CMD
forwword( void )
{
	int	n = repeat;

	while( n-- ) {
		while( inword() == NIL )
			if( forwchar() == NIL )
				return( NIL );

		while( inword() != NIL )
			if( forwchar() == NIL )
				return( NIL );

	}
	return( T );
}

/*
 *	Move the cursor forward by the specified number of words. As
 *	you move, convert any characters to upper case. Error if you
 *	try and move beyond the end of the buffer. Bound to "M-U".
 */

CMD
upperword( void )
{
	int	n = repeat;
	int	c;

	if( freadonly() )
		return( NIL );

	while( n-- ) {
		while( inword() == NIL ) {
			if( forwchar() == NIL )
				return( NIL );
		}
		while( inword() != NIL ) {
			c = lgetdot();
			if( isalpha( c ) && islower( c ) ) {
				lputdot( toupper( c ) );
				lchange( WFHARD );
			}
			if( forwchar() == NIL )
				return( NIL );
		}
	}
	return( T );
}

/*
 *	Move the cursor forward by the specified number of words. As
 *	you move convert characters to lower case. Error if you  try
 *	and move over the end of the buffer. Bound to "M-L".
 */

CMD
lowerword( void )
{
	int	n = repeat;
	int	c;

	if( freadonly() )
		return( NIL );

	while( n-- ) {
		while( inword() == NIL )
			if( forwchar() == NIL )
				return( NIL );

		while( inword() != NIL ) {
			c = lgetdot();
			if( isalpha( c ) && isupper( c ) ) {
				lputdot( tolower( c ) );
				lchange( WFHARD );
			}
			if( forwchar() == NIL )
				return( NIL );
		}
	}

	return( T );
}

/*
 *	Move the cursor forward by the specified number of words. As
 *	you  move convert the first character of the word  to  upper
 *	case, and subsequent characters to lower case.  Error if you
 *	try and move past the end of the buffer. Bound to "M-C".
 */

CMD
capword( void )
{
	int	n = repeat;
	int	c;

	if( freadonly() )
		return( NIL );

	while( n-- ) {
		while( inword() == NIL )
			if( forwchar() == NIL )
				return( NIL );

		if( inword() != NIL ) {
			c = lgetdot();
			if( isalpha( c ) && islower( c ) ) {
				lputdot( toupper( c ) );
				lchange( WFHARD );
			}
			if( forwchar() == NIL )
				return( NIL );
			while( inword() != NIL ) {
				c = lgetdot();
				if( isalpha( c ) && isupper( c ) ) {
					lputdot( tolower( c ) );
					lchange( WFHARD );
				}
				if( forwchar() == NIL )
					return( NIL );
			}
		}
	}
	return( T );
}

/*
 *	Kill forward by "n" words. Bound to "M-D".
 */

CMD
delfword( void )
{
	int n  = repeat;
	int sv = repeat;

	repeat = 1;
	while( n-- ) {
		while( inword() == NIL )
			if( forwdel() == NIL )
				return( NIL );
		while( inword() != NIL )
			if( forwdel() == NIL )
				return( NIL );
	}
	repeat = sv;

	return( T );

}

/*
 *	Kill  backwards by "n" words.  Bound to  "M-Rubout"  and to
 *	"M-Backspace".
 */

CMD
delbword( void )
{
	int n	= repeat;
	int sv = repeat;

	repeat = 1;
	while( n-- ) {
		while( backchar() == T && inword() == NIL )
			if( forwdel() == NIL )
				return( NIL );
		while( forwdel() == T && backchar() == T )
			if( inword() == NIL )
				break;
	}
	(void)forwchar();
	repeat = sv;

	return( T );
}

/*
 *	Swap the current and the next words. Text between the words
 *	is not altered.  After  the  exchange,  point is positioned
 *	after the two words.
 */

CMD
wtwiddle( void )
{

	EDLINE	*dotp;
	int	doto;
	int	i;
	int	j;
	EMCHAR	word1[ NPAT ];
	EMCHAR	word2[ NPAT ];

	if( (forwword() == NIL) || (backword() == NIL) )
		return( NIL );

	/*
	 *	xxxxx,   (yyyyy)
	 *	          ^
	 *		  @
	 */

	doto = curwp->w_doto;
	dotp = curwp->w_dotp;

	/*
	 *	delete right word first and store it in 'word1'
	 */

	for( i = 0 ; inword() == T && i < NPAT-1 ; i++ ) {
		word1[ i ] = lgetdot();
		(void)forwdel();
	}
	word1[ i ] = '\0';
	doto += i;

	/*
	 *	go to previous word
	 *
	 *	xxxxx,   ()
	 *	^         @
	 */

	(void)backword();

	/*
	 *	then delete left word and store it in 'word2'
	 */

	for( i = 0 ; inword() == T && i < NPAT-1 ; i++ ) {
		word2[ i ] = lgetdot();
		(void)forwdel();
	}
	word2[ i ] = '\0';
	doto -= i;

	/*
	 *	insert word stored in 'word1'
	 *
	 *	yyyyy,   ()
	 *	     ^    @
	 */

	for( j = 0 ; word1[ j ] ; j++ )
		(void)linsert( 1, word1[ j ] );

	/*
	 *	go to saved position and insert word stored in 'word2'
	 *
	 *	yyyyy,   (xxxxx)
	 *	          @    ^
	 */

	while( curwp->w_dotp != dotp )
		if( forwline() == NIL )
			return( NIL );

	if( doto > 0 && doto < llength( curwp->w_dotp ) )
		curwp->w_doto = doto;

	for( j = 0 ; word2[ j ] ; j++ )
		(void)linsert( 1, word2[ j ] );

	return( T );
}
