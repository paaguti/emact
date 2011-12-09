#if	!defined( lint )
static	char rcsid[] = "$Id: options.c,v 1.8 2008/06/20 09:25:13 jullien Exp $";
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
 *	Display a small help screen. Bound to M-?.
 */

#include	"emacs.h"

static  void	_define(getkeyname,(int key, EMCHAR *buf));
static  EMCHAR *_define(varmatch,(EMCHAR *prompt, EMCHAR *buf));
static	void	_define(printcmd,(int c, BUFFER *bp));
static	void	_define(printmacro,(EMCHAR *name, BUFFER *bp));
static	CMD	_define(internalfindtag,(int lineno));

/*
 *	Describe  the  next  key  or  the entire binding if RETURN is
 *	pressed.
 */

extern	KEYTAB	*pkeytab;
extern	int	nkeytab;
extern	VARTAB	*pvartab;
extern	int	nvartab;
extern	MACTAB	*pmactab;
extern	int	nmactab;

static	int	tagfound = -1;

#define	INDEX_FUNCTION		1
#define	INDEX_VARIABLE		2
#define	COLUMN_VALUE		30

CMD
describekey( void )
{
	KEYTAB *ktp;
	int	c;
	int	i;
	EMCHAR	ch[2];
	EMCHAR	meta[ 10 ];

	WDGwrite( ECSTR("Apropos keyword: ") );

	if( (c = getkey()) == (Ctrl|'M') )
		return( help() );

	getkeyname( c, meta );

	ch[0] = (EMCHAR)(c & MAX_EMCHAR);
	ch[1] = '\000';

	for( i = 0 ; i < nmactab ; i++ )	/* Look in macro table.	*/
		if( MACcode( i ) == c ) {
		    WDGwrite(ECSTR("%s%s is bound to: %s"),meta,ch,MACname(i));
		    return( T );
		}

	for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ )
		if( ktp->k_code == c ) {
		    WDGwrite(ECSTR("%s%s is bound to: %s"),meta,ch,ktp->k_name);
		    return( T );
		}

	if( self_insert( c ) )
		WDGwrite( ECSTR("%s is bound to self-insert"), ch );
	else	WDGwrite( ECSTR("%s%s is not bound!"), meta, ch );

	return( NIL );
	
}	

/*
 *	Get the printable keyname of a command.
 */

static	void
getkeyname( int key, EMCHAR *buf )
{

	buf[ 0 ] = '\0';

	if( key & CXDR )
		(void)emstrcat( buf, ECSTR("C-X-$") );
	if( key & CTLX )
		(void)emstrcat( buf, ECSTR("C-X") );
	if( key & CTLC )
		(void)emstrcat( buf, ECSTR("C-C") );
	if( key & META )
		(void)emstrcat( buf, ECSTR("M-") );
	if( key & Ctrl )
		(void)emstrcat( buf, ECSTR("C-") );

}

/*
 *	Decribe all the current keymap binding on a help window.
 */

CMD
help( void )
{
	KEYTAB	*ktp;
	VARTAB	*vtp;
	BUFFER	*bp;
	int	c;
	int	i;
	int	j;
	EMCHAR	ch[2];
	EMCHAR	meta[10];
	EMCHAR	line[ NLINE ];

	bp = bfind( BUF_HELP, T, NIL, FUNDAMENTAL );
	bp->b_flag &= ~BFCHG;

	if( bclear( bp ) != T )
		return( NIL );

	if( addline( bp, ECSTR("===== Standard key definition =====") ) == NIL )
		return( NIL );

	for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ ) {
		if( ((c = ktp->k_code) & SPCL) && c != UNBOUND )
			continue;
		getkeyname( c, meta );
		(void)emstrcpy( line, ktp->k_name );
		for( i = emstrlen( line ) ; i < COLUMN_VALUE ; i++ )
			line[ i ] = ' ';
		line[ i++ ]	= '(';
		line[ i ]	= '\0';
		ch[ 0 ]		= (EMCHAR)(c & MAX_EMCHAR);
		ch[ 1 ] 	= '\000';

		if( c == UNBOUND ) {
			(void)emstrcat( line, ECSTR("unbound)") );
		} else	{
			(void)emstrcat( line, meta );
			if( (c & MAX_EMCHAR) == 0x7F )
				(void)emstrcat( line, ECSTR("DEL)") );
			else	{
				(void)emstrcat( line, ch );
				(void)emstrcat( line, ECSTR(")") );
			}
		}
		if( addline( bp, line ) == NIL )
			return( NIL );
	}

	if( addline( bp, ECSTR("====== User macro definition ======") ) == NIL )
		return( NIL );

	for( j = 0 ; j < nmactab ; j++ ) {
		if( ((c = MACcode( j )) & SPCL) && c != -1 )
			continue;
		(void)emstrcpy( line, MACname( j ) );
		for( i = emstrlen( line ) ; i < COLUMN_VALUE ; i++ )
			line[ i ] = ' ';
		line[ i ] = '\0';
		if( c != -1 ) {
			getkeyname( c, meta );
			ch[ 0 ] = (EMCHAR)(c & MAX_EMCHAR);
			ch[ 1 ] = '\000';
			(void)emstrcat( line, ECSTR("(") );
			(void)emstrcat( line, meta );
			(void)emstrcat( line, ch );
			(void)emstrcat( line, ECSTR(")") );
		} else	(void)emstrcat( line, ECSTR("unbound") );

		if( addline( bp, line ) == NIL )
			return( NIL );
	}

	if( addline( bp, ECSTR("===== Customer variable value =====") ) == NIL )
		return( NIL );

	for( vtp = pvartab ; vtp < (pvartab+nvartab) ; vtp++ ) {
		int *intp;
		(void)emstrcpy( line, vtp->f_name );
		for( i = emstrlen( line ) ; i < COLUMN_VALUE ; i++ )
			line[ i ] = ' ';
		line[ i ] = '\0';
		switch( vtp->f_type ) {
		case BOOLEAN :
			intp = (int *)vtp->f_val;
			(void)emstrcat(
					line,
					(*intp ? ECSTR("t") : ECSTR("nil"))
				      );
			break;
		case FIXVAL :
			intp = (int *)vtp->f_val;
			(void)emsprintf1(
			                  (line + emstrlen( line )),
				          ECSTR("%d"),
					  *intp
				        );
			break;
		default :
			(void)emstrcat( line, ECSTR("\"") );
			(void)emstrcat( line, (EMCHAR *)vtp->f_val );
			(void)emstrcat( line, ECSTR("\"") );
		}

		if( addline( bp, line ) == NIL )
			return( NIL );

	}

	(void)showbuffer( bp );
	return( T );

}

/*
 *	Change  the  boolean  value  of  a global Emacs flag.  If the
 *	value  was  T  change it to NIL and vice-versa.  This command
 *	allows a dynamic control of Emacs options. Bound to C-XR
 */

static	int	VARindex;
static	int	index_type;

static	EMCHAR	*
varmatch( EMCHAR *prompt, EMCHAR *buf )
{
	KEYTAB	*ktp;
	int	i;
	size_t	len = (size_t)emstrlen( buf );

	/*
	 *	Search first for an exact match
	 */

	for( i = 0 ; i < nvartab ; i++ )
		if( emstrcmp( VARname( i ), buf ) == 0 ) {
			VARindex   = i;
			index_type = INDEX_VARIABLE;
			return( VARname( i ) );
		}

	for( ktp = pkeytab, i = 0 ; ktp < (pkeytab+nkeytab) ; ktp++, i++ )
		if( emstrcmp( ktp->k_name, buf ) == 0 ) {
			VARindex   = i;
			index_type = INDEX_FUNCTION;
			return( ktp->k_name );
		}

	/*
	 *	Try to match with the help of the user.
	 */

	for( i = 0 ; i < nvartab ; i++ )
		if( len == 0 || emstrncmp( VARname( i ), buf, len ) == 0 ) {
			if( len != (size_t)emstrlen( VARname( i ) ) ) {
				WDGupdate( prompt, VARname( i ) );
				switch( TTYgetc() ) {
				case 0x07:
					WDGwrite( ECSTR("Quit") );
					return( NIL );
				case 0x0D:
				case 0x0A:
				case 'y' :
				case 'Y' :
					VARindex   = i;
					index_type = INDEX_VARIABLE;
					return( VARname( i ) );
				default:
					continue;
				}
			} else	{
				VARindex   = i;
				index_type = INDEX_VARIABLE;
				return( VARname( i ) );
			}
		}

	for( ktp = pkeytab, i = 0 ; ktp < (pkeytab+nkeytab) ; ktp++, i++ )
		if( len == 0 || emstrncmp( ktp->k_name, buf, len ) == 0 ) {
			if( len != (size_t)emstrlen( ktp->k_name ) ) {
				WDGupdate( prompt, ktp->k_name );
				switch( TTYgetc() ) {
				case 0x07:
					WDGwrite( ECSTR("Quit") );
					return( NIL );
				case 0x0D:
				case 0x0A:
				case 'y' :
				case 'Y' :
					break;
				default:
					continue;
				}
			}
			VARindex   = i;
			index_type = INDEX_FUNCTION;
			return( ktp->k_name );
		}

	WDGwrite( ECSTR("Not found.") );
	return( NIL );
}

/*
 *	Change the value of an Emacs variable
 */

CMD
setvar( void )
{
	WINSCR	*wp;
	EMCHAR	buf[ NPAT ];
	int	status;
	int	*intp;

	complete.fn = varmatch;
	index_type  = 0;

	if( mlreply( ECSTR(": eval-function "), buf, NPAT ) != T )
		return( NIL );

	varmatch( ECSTR(": match "), buf ); /* just to return the INDEX */

	switch( index_type ) {
	case INDEX_FUNCTION :
		thisflag = 0;
		status	 = (*(pkeytab+VARindex)->k_fp)();
		lastflag = thisflag;
		return( status );
	case INDEX_VARIABLE :
		switch( VARtype( VARindex ) ) {
		case BOOLEAN :
			intp = (int *)VARval( VARindex );
			if( *intp == NIL )
				*intp = T;
			else	*intp = NIL;
			WDGwrite( ECSTR("%s set to %s"),
				  VARname(VARindex),
				  ((*intp == NIL) ? ECSTR("NIL") : ECSTR("T"))
				);
			break;
		case FIXVAL : {
			EMCHAR newval[ NPAT ];
			intp = (int *)VARval( VARindex );
			(void)emsprintf2(
				          buf,
				          ECSTR("%s (%d) > "),
					  VARname( VARindex ),
					  *intp
				        );
			(void)mlreply( buf, newval, 16 );
			*intp = emstrtoi( newval );
			break;
			}
		default :
			(void)emstrcpy( buf, VARname( VARindex ) );
			(void)emstrcat( buf, ECSTR(" \"") );
			(void)emstrcat( buf, (EMCHAR *)VARval( VARindex ) );
			(void)emstrcat( buf, ECSTR("\" > ") );
			(void)mlreply(
				       buf,
				       (EMCHAR *)VARval(VARindex),
				       VARtype(VARindex)
				     );
		}
		break;
	}

	for( wp = wheadp ; wp ; wp = wp->w_wndp )
		modeline( wp );

	return( T );

}

/*
 *	Code that deal with TAGS. Bound to M-.
 */

CMD
findtag( void )
{
	return( internalfindtag( 0 ) );
}

/*
 *	Repeat find-tag for the next entry. Bound to M-,
 */

CMD
tagsloopcont( void )
{
	return( internalfindtag( tagfound ) );
}

/*
 *	Internal find tag. Find a tag name from a TAGS file.
 */

static CMD
internalfindtag( int tagnext )
{
	static	EMCHAR	tagname[ NPAT   ] = { '\000' };
	EMCHAR	tagline[ NLINE  ];
	EMCHAR	tagdir[  NFILEN ];
	EMCHAR	tagfile[ NFILEN ];
	FILE *	tagfd;
	int	taglen;
	int	tagno;
	int	s;

	(void)emstrcpy( tagdir,  curbp->b_fname );
	(void)updir(  tagdir,  SLASH  );
	(void)emstrcpy( tagfile, tagdir );
	(void)emstrcat( tagfile, ECSTR("tags") );

	if( (tagfd  = ffopen( tagfile, ECSTR("r"), NULL )) == NULL ) {
		WDGerror( ECSTR("No tags file.") );
		return( NIL );
	}

	if( tagnext != 0 ) {
		if( tagname[ 0 ] == '\000' ) {
			TTYbeep();
			WDGerror( ECSTR("No M-x find-tag in progress.") );
			return( NIL );
		}
	} else	{
		tagname[ 0 ] = '\000';

		if( wordatcursor( tagname, NPAT ) != T )
			tagname[ 0 ] = '\000';

		if( (s = WDGedit( ECSTR("Find tag: "), tagname, NPAT )) != T ) {
			if( s != NIL || tagname[ 0 ] == '\000' ) {
				(void)fclose( tagfd );
				return( s );
			}
		}
	}

	taglen = emstrlen( tagname );

	for( tagno=0 ; ffgets(tagline,NLINE,tagfd) != NULL ; tagno++ ) {
		if( tagno <= tagnext )
			continue;
		if( emstrncmp( tagline, tagname, (size_t)taglen ) == 0 ) {
			EMCHAR	*str = tagline;
			EMCHAR	*line;
			EMCHAR	*file;
			int	n;

			(void)fclose( tagfd );			

			tagfound = tagno;

			/*
			 * skip tag name
			 */

			while( *str++ != ' ' )
				;

			/*
			 * skip space
			 */

			while( *str++ == ' ' )
				;

			/*
			 * read the line number.
			 */

			line = str-1;
			while( *str++ != ' ' )
				;

			*(str-1) = '\000';

			/*
			 * read the file name.
			 */

			file = str;
			while( *str && (*str != '\n') )
				str++;
			*str = '\000';

			/*
			 * load the filename and goto tag.
			 */

			(void)emstrcpy( tagfile, tagdir );
			(void)emstrcat( tagfile, file   );
#if	defined( _DOSPATH )
			(void)emstrlwr( tagfile );
#endif

			(void)newfile( tagfile );
			n	= repeat;
			repeat  = emstrtoi( line );
			(void)gotoline();
			repeat	= n;
			return( T );
		}
	}

	(void)fclose( tagfd );
	if( tagnext == 0 )
		WDGwrite( ECSTR("No tags containing %s"), tagname );
	else	WDGwrite( ECSTR("No more tags containing %s"), tagname );
	return( NIL );

}

/*
 *	Complete  a  given  word  'tagname'  into  'tagcomp' from the
 *	current  tag file.  The variable tagnext is the line where to
 *	start the search.  It returns the line where a match is found
 *	that  could be used for the next search or 0 when no match is
 *	found. This internal routine is used by complete-word.
 */

int
completeintag( int tagnext, EMCHAR *tagname, EMCHAR *tagcomp )
{
	EMCHAR	tagline[ NLINE  ];
	EMCHAR	tagdir[  NFILEN ];
	EMCHAR	tagfile[ NFILEN ];
	FILE *	tagfd;
	int	mode;
	int	tagno;
	size_t	taglen;

	mode = curwp->w_emode;

	(void)emstrcpy( tagdir,  curbp->b_fname );
	(void)updir(  tagdir,  SLASH  );
	(void)emstrcpy( tagfile, tagdir );
	(void)emstrcat( tagfile, ECSTR("tags") );

	if( (tagfd = ffopen( tagfile, ECSTR("r"), NULL )) == NULL )
		return( 0 );

	taglen = (size_t)emstrlen( tagname );

	for( tagno=0 ; ffgets(tagline,NLINE,tagfd) != NULL ; tagno++ ) {
		if( tagno <= tagnext )
			continue;
		if( (mode == LISPMODE) ?
		          (emstrnicmp( tagline, tagname, taglen ) == 0)
		      :   (emstrncmp(  tagline, tagname, taglen ) == 0) ) {
			EMCHAR	*s;

			(void)fclose( tagfd );			

			tagfound = tagno;

			/*
			 * skip tag name
			 */

			for( s = tagline ; *s != ' ' ; s++ )
				*tagcomp++ = *s;

			*tagcomp = '\000';

			return( tagnext = tagno );
		}
	}

	(void)fclose( tagfd );
	return( 0 );
}

/*
 *	Uncompile  the  current macro definition in MLisp or OpenLisp
 *	statements.  The  command  popup  the help buffer and display
 *	the definition. Bound to C-XU
 */

static	EMCHAR	macline[ NMACLINE ];
static	int	instringp = NIL;
static	int	count	  = 1;

/*
 *	Print the macro 'name'.
 */

static	void
printmacro( EMCHAR *name, BUFFER *bp )
{
	if( instringp == T ) {
		if( count > 1 )
			(void)emstrcat( macline, ECSTR("\"))") );
		else	(void)emstrcat( macline, ECSTR("\")") );

		(void)addline( bp, macline );
	}

	if( name ) {
		if( count > 1 )
			(void)emstrcat( macline, ECSTR(" (") );
		else	(void)emstrcpy( macline, ECSTR("   (") );

		(void)emstrcat( macline, name );

		if( count > 1 )
			(void)emstrcat( macline, ECSTR("))") );
		else	(void)emstrcat( macline, ECSTR(")") );

		(void)addline( bp, macline );
	}

	instringp = NIL;
}

/*
 *	Print the current command of the keyboard macro.
 */

static	void
printcmd( int c, BUFFER *bp )
{
	KEYTAB	*ktp;
	int	i;
	EMCHAR	ch[ 2 ];

	ch[ 0 ] = (EMCHAR)(c & MAX_EMCHAR);
	ch[ 1 ] = '\000';

	if( count > 1 ) {
		printmacro( NULL, bp );
		(void)emsprintf1( macline, ECSTR("   (repeat %d"), count );
	}

	for( i = 0 ; i < nmactab ; i++ )	/* Look in macro table.	*/
		if( MACcode( i ) == c ) {
			printmacro( MACname( i ), bp );
			return;
		}

	for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ )
		if( ktp->k_code == c ) {
			printmacro( ktp->k_name, bp );
			return;
		}

	if( self_insert( c ) ) {

		if( instringp == NIL ) {
			if( count > 1 )
			     (void)emstrcat( macline,
					     ECSTR(" (insert-string \"") );
			else (void)emstrcpy( macline,
					     ECSTR("   (insert-string \"") );
			instringp = T;
		}

		if( c == (int)'\\' || c == (int)'"' )
			(void)emstrcat( macline, ECSTR("\\") );
		(void)emstrcat( macline, ch );
		if( count > 1 )
			printmacro( NULL, bp );
	}
}	

/*
 *	Uncompile  the  current  keyboard macro in Lisp code into the
 *	help buffer.
 */

CMD
uncompile( void )
{
	BUFFER	*bp;
	int	c;

	if( kbdm[ 0 ] == -1 ) {
		WDGmessage( ECSTR("No keyboard macro to uncompile.") );
		return( NIL );
	}

	if( kbdmip != NULL || kbdmop != NULL ) {
		WDGmessage( ECSTR("You can't uncompile macro while defining it.") );
		return( NIL );
	}

	bp = bfind( BUF_HELP, T, NIL, FUNDAMENTAL );
	bp->b_flag &= ~BFCHG;

	if( bclear( bp ) != T )
		return( NIL );

	if( addline( bp, ECSTR("(defun current-macro ()") ) == NIL )
		return( NIL );

	for( kbdmop = &kbdm[0] ; (c = *kbdmop++) != (CTLX|')') ; ) {
		if( c == (Ctrl|'U') ) {
			count = *kbdmop++;
			c     = *kbdmop++;
		} else	count = 1;
		printcmd( c, bp );
	}

	printmacro( NULL, bp );

	kbdmop	= NULL;

	if( addline( bp, ECSTR(")") ) == NIL )
		return( NIL );

	(void)showbuffer( bp );

	return( T );
}
