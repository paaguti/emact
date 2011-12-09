#if	!defined( lint )
static	char rcsid[] = "$Id: region.c,v 1.7 2008/06/20 09:25:13 jullien Exp $";
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
 *	The  routines in this file deal with the region,  that  magic
 *	space between "." and mark. Some functions are commands. Some 
 *	functions are just for internal use.
 */

#include "emacs.h"

/*
 *	The starting position of a region, and the size of the region 
 *	in  characters,  is kept in a region structure.   Used by the 
 *	region commands.
 */

typedef struct	REGION {
	EDLINE *r_linep;	/* Origin EDLINE address	*/
	int	r_offset;	/* Origin EDLINE offset		*/
	int	r_size;		/* Length in characters		*/
	int	r_lines;	/* Number of lines		*/
}	REGION;

static	int _define(getregion,(REGION *rp));

/*
 *	This  routine  figures  out  the  bounds of the region in the
 *	current  window,  and  fills  in  the  fields of the "REGION"
 *	structure  pointed  to by "rp".  Because the dot and mark are
 *	usually  very  close  together,  we  scan  outward  from  dot
 *	looking  for mark.  This should save time.  Return a standard
 *	code.  Callers  of  this routine should be prepared to get an
 *	"ABORT"  status;  we  might  make this have the conform thing
 *	later.
 */

static	int
getregion( REGION *rp )
{
	static	EMCHAR	*rtoobig = ECSTR("Region too big.");

	EDLINE	*flp;
	EDLINE	*blp;
	int	fsize;
	int	bsize;

	if( curwp->w_markp == NULL ) {
		WDGmessage( ECSTR("No mark set in this buffer!") );
		return( NIL );
	}

	rp->r_lines = 0;

	if( curwp->w_dotp == curwp->w_markp ) {
		rp->r_linep = curwp->w_dotp;
		if( curwp->w_doto < curwp->w_marko ) {
			rp->r_offset = curwp->w_doto;
			rp->r_size   = curwp->w_marko-curwp->w_doto;
		} else {
			rp->r_offset = curwp->w_marko;
			rp->r_size   = curwp->w_doto-curwp->w_marko;
		}
		return( T );
	}
	blp	= curwp->w_dotp;
	bsize	= curwp->w_doto;
	flp	= curwp->w_dotp;
	fsize	= llength(flp)-curwp->w_doto+1;
	while( flp != lastline( curbp ) || lback( blp ) != lastline( curbp ) ) {
		rp->r_lines++;
		if( flp != lastline( curbp ) ) {
			flp = lforw(flp);
			if( flp == curwp->w_markp ) {
				rp->r_linep  = curwp->w_dotp;
				rp->r_offset = curwp->w_doto;
				rp->r_size   = fsize+curwp->w_marko;
				return( T );
			}
			if( (fsize += llength(flp)+1) > (EMMAXINT - NLINE) ) {
				WDGerror( rtoobig );
				return( NIL );
			}
		}
		if( lback( blp ) != lastline( curbp ) ) {
			blp = lback( blp );
			if( (bsize += llength( blp ) + 1) >= EMMAXINT ) {
				WDGerror( rtoobig );
				return( NIL );
			}
			if( blp == curwp->w_markp ) {
				rp->r_linep	= blp;
				rp->r_offset	= curwp->w_marko;
				rp->r_size	= bsize - curwp->w_marko;
				return( T );
			}
		}
	}

	WDGmessage( ECSTR("Error lost mark") );
	return( NIL );
}

/*
 *	Kill the region.  Ask "getregion" to figure out the bounds of
 *	the region.  Move "." to the start,  and kill the characters.
 *	Bound to "C-W". Region as a maximum of EMMAXINT characters in
 *	size and check is made prior any operation on a region.
 */

CMD
killregion( void )
{
	int	s;
	REGION	region;

	if( freadonly() == T )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	if( (lastflag & CFKILL) == 0 )		/* This is a kill type	*/
		kdelete();

	thisflag |= CFKILL;
	curwp->w_dotp = region.r_linep;
	curwp->w_doto = region.r_offset;
	if( ldelete( region.r_size, T ) == T ) {
		WDGclipcopy();
		return( T );
	} else	return( NIL );
}

/*
 *	Copy  all of the characters in the region to the kill buffer.
 *	Don't  move  dot at all.  This is a bit like  a  kill  region
 *	followed by a yank. Bound to "M-W".
 */

CMD
copyregion( void )
{
	EDLINE	*linep;
	REGION	region;
	int	loffs;
	int	s;

	if( (s = getregion( &region )) != T )
		return( s );

	if( (lastflag & CFKILL) == 0 )		/* Kill type command.	*/
		kdelete();

	thisflag |= CFKILL;
	linep = region.r_linep;			/* Current line.	*/
	loffs = region.r_offset;		/* Current offset.	*/
	while( region.r_size-- )
		if( loffs == llength( linep )) {
			if( (s = kinsert( '\n' )) != T )
				return( s );
			linep = lforw( linep );
			loffs = 0;
		} else {			/* Middle of line.	*/
			if( (s = kinsert( lgetc( linep, loffs )) ) != T )
				return( s );
			++loffs;
		}

	WDGclipcopy();
	return( T );
}

/*
 *	Lower  case region.  Zap all of the upper case characters  in
 *	the  region  to lower case.  Use the region code to  set  the
 *	limits. Scan the buffer, doing the changes. Call "lchange" to
 *	ensure  that redisplay is done in all buffers.  Bound to "C-X
 *	C-L".
 */

CMD
lowerregion( void )
{
	EDLINE	*linep;
	REGION	region;
	int	loffs;
	int	c;
	int	s;

	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	linep = region.r_linep;
	loffs = region.r_offset;

	while( region.r_size-- )
		if( loffs == llength( linep ) ) {
			linep = lforw( linep );
			loffs = 0;
		} else	{
			c = lgetc( linep, loffs );
			if( isalpha( c ) && isupper( c ) )
				lputc( linep, loffs, tolower( c ) );
			++loffs;
		}

	return( T );
}

/*
 *	Fill region.  Insert the fill-prefix string on each line that
 *	does not currently start with this prefix. Unbound.
 */

CMD
fillregion( void )
{
	REGION	region;
	size_t	fillmax;
	int	i;
	int	s;

	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	curwp->w_dotp = region.r_linep;
	curwp->w_doto = 0;
	fillmax	      = (size_t)emstrlen( fill_prefix );

	while( region.r_lines-- ) {
		EMCHAR	*p = ltext( curwp->w_dotp );

		if( (size_t)llength( curwp->w_dotp ) < fillmax ||
		    emstrncmp( p, fill_prefix, fillmax ) != 0 ) {
		    	for( i = 0 ; fill_prefix[ i ] ; i++ )
				(void)linsert( 1, fill_prefix[ i ] );
		    }
		(void)forwline();
	}

	return( T );
}

/*
 *	Upper  case  region.  Zap all of the lower case characters in
 *	the  region  to  upper  case.  Use the region code to set the
 *	limits.  Scan the buffer,  doing the changes.  Call "lchange"
 *	to  ensure  that  redisplay is done in all buffers.  Bound to
 *	"C-X C-L".
 */

CMD
upperregion( void )
{
	EDLINE	*linep;
	REGION	region;
	int	loffs;
	int	c;
	int	s;

	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	linep = region.r_linep;
	loffs = region.r_offset;

	while( region.r_size-- )
		if( loffs == llength( linep ) ) {
			linep = lforw( linep );
			loffs = 0;
		} else	{
			c = lgetc( linep, loffs );
			if( isalpha( c ) && islower( c ) )
				lputc( linep, loffs, toupper( c ) );
			++loffs;
		}

	return( T );
}

/*
 *	Write  all  of the characters in the region into a disk file.
 *	Bound to "M-W".
 */

CMD
writeregion( void )
{
	EDLINE	*linep;
	REGION	region;
	EMCHAR	fname[NFILEN];
	int	loffs;
	int	s;
	FILE	*fd;

	if( (s = getregion( &region )) != T )
		return( s );

	complete.fn = filematch;

	if( (s = mlreply(ECSTR("Write region to file: "), fname, NFILEN)) != T )
		return( NIL );

	if( (fd = ffopen( fname, ECSTR("w"), NULL )) == NULL )
		return( NIL );
	
	linep = region.r_linep;			/* Current line.	*/
	loffs = region.r_offset;		/* Current offset.	*/

	while( region.r_size-- )
		if( loffs == llength( linep )) {
			/* End of line.		*/
			(void)fputc( '\n', fd );
			linep = lforw( linep );
			loffs = 0;
		} else	(void)fputc( lgetc( linep, loffs++ ), fd );

	(void)fclose( fd );

	return( T );
}

/*
 *	Reindent  a  region  usign  indent-line  on every line in the
 *	block. Bound to M-C-\
 */

CMD
indentregion( void )
{
	REGION	region;
	int	s;
	
	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	curwp->w_dotp = region.r_linep;
	curwp->w_doto = 0;

	do
		s = indentline();
	while( s == T && --region.r_lines > 0 );

	return( T );

}

/*
 *	Reindent  a  region,  adding  a TAB at the beginning of every
 *	line in the block. Bound to CX->
 */

CMD
shiftright( void )
{
	REGION	region;
	int	s;
	
	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	curwp->w_dotp = region.r_linep;
	curwp->w_doto = 0;

	do	{
		s = linsert( 1, '\t' );
		curwp->w_dotp = lforw( curwp->w_dotp );
		curwp->w_doto = 0;
	}	while( s == T && --region.r_lines > 0 );

	return( T );

}

/*
 *	Unindent a region,  removing a the first TAB of every line in
 *	the block. Bound to CX-<
 */

CMD
shiftleft( void )
{
	REGION	region;
	int	s;
	
	if( freadonly() )
		return( NIL );

	if( (s = getregion( &region )) != T )
		return( s );

	lchange( WFHARD );

	curwp->w_dotp = region.r_linep;
	curwp->w_doto = 0;

	do	{
		if( lgetc(curwp->w_dotp,0) == '\t' && ldelete(1,NIL) == NIL )
			return( NIL );
		curwp->w_dotp = lforw( curwp->w_dotp );
		curwp->w_doto = 0;
	} while( --region.r_lines > 0 );

	return( T );

}
