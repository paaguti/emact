#if	!defined( lint )
static	char rcsid[] = "$Id: minibuf.c,v 1.5 2008/06/19 12:13:31 jullien Exp $";
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
 *	The   functions  in  this  file  handle  the  minibuffer  the
 *	one-line display at the bottom of the screen.
 */

#include	"emacs.h"

static  void	_define(mlputs,(EMCHAR *s, int size));
static  void	_define(mlputl,(long i, int r));
static	void	_define(mlclearentry,(EMCHAR *buf, int cpos));

int	 mbcursor;
COMPLETE complete;
int	 completion;

static	int	allowcomplete = NIL;

/*
 *	Erase the user response up to the prompt.
 */

static void
mlclearentry( EMCHAR *buf, int cpos )
{
	while( cpos > 0 && mbcursor > 0) {
		statputc( --mbcursor, (int)' ' );
		if( buf[--cpos] < (EMCHAR)0x20 )
			statputc( --mbcursor, (int)' ' );
	}
}

void
mlerase( void )
{
	mbcursor = 0;

	while( mbcursor < TTYncol )
		statputc( mbcursor++, (int)' ' );

#if	defined( _WINDOWS_SOURCE ) && !defined( _WIN32_WCE )
	WDGmessage( ECSTR(" ") );	/* clear the dialog (Widgets version) */
#endif

	mpresf = NIL;
}

/*
 *	Ask  a y or n question in the message line.  Return either T,
 *	NIL,  or  ABORT.  The  ABORT  status  is returned if the user
 *	bumps out of the question with a ^G.
 */

int
mlyn( EMCHAR *prompt )
{
	mlwrite( ECSTR("%s"), prompt );

	for( ;; )
		switch( TTYgetc() ) {
		case 0x07:
			(void)ctrlg();
			WDGmessage( ECSTR("Quit") );
			return( ABORT );
		case 'y' :
		case 'Y' :
			return( T );
		case 'n' :
		case 'N' :
			return( NIL );
		case 'q' :
		case 'Q' :
		case 0x1B:
			return( ABORT );
		default  :
			(void)ctrlg();
		}
}

/*
 *	Ask  a yes or no question in the message line.  Return either
 *	T,  NIL,  or ABORT.  The ABORT status is returned if the user
 *	bumps out of the question with a ^G.
 */

int
mlyesno( EMCHAR *prompt )
{
	EMCHAR	buf[ NPAT ];

	for( ;; ) {
		if( mlreply( prompt, buf, NPAT ) != T )
			return( ABORT );
		if( emstrcmp( buf, ECSTR("Yes") ) == 0 ||
		    emstrcmp( buf, ECSTR("yes") ) == 0 ||
		    emstrcmp( buf, ECSTR("YES") ) == 0 )
		    	return( T );
		if( emstrcmp( buf, ECSTR("No") ) == 0 ||
		    emstrcmp( buf, ECSTR("no") ) == 0 ||
		    emstrcmp( buf, ECSTR("NO") ) == 0 )
		    	return( NIL );
		(void)ctrlg();
		mlwrite( ECSTR("Please answer yes or no") );
		waitmatch( 3 );
	}
}

int
mlconfirm( EMCHAR *prompt )
{
	return( mlyesno( prompt ) );
}

/*
 *	Routine to allow or not edit strokes to complete a defaut value.
 */

int
mlallowcomplete( int flag )
{
	int	old = allowcomplete;

	allowcomplete = flag;

	return( old );
}

/*
 *	Write  a  prompt  into  the  message  line,  then read back a
 *	response.  Keep track of the physical position of the cursor.
 *	If  we  are  in  a keyboard macro throw the prompt away,  and
 *	return the remembered response.  This lets macros run at full
 *	speed. Handle erase, kill, quote and abort keys.
 */

int
mledit( EMCHAR *prompt, EMCHAR *buf, int nbuf )
{
	int	i;
	int	c;
	EMCHAR	*s;
	int	cpos;
	int	editflg;

	editflg   = NIL;

loop:
	cpos	   = 0;
	completion = COMPLETE_ONE;

	if( kbdmop != NULL ) {
		while( (c = *kbdmop++) != '\000' )
			buf[cpos++] = (EMCHAR)c;
		buf[ cpos ]   = '\000';
		complete.flag = 0L;
		return( (buf[ 0 ] == 0) ? NIL : T );
	} else	cpos = emstrlen( buf );

	mlwrite( ECSTR("%s%s"), prompt, buf );

	for( ;; ) {
		update( MINIBUF );

		TTYcshow( T );
		c = TTYgetc();
		TTYcshow( NIL );
		
		switch( c ) {
		case 0x03:	/* Ctrl-C */
			while( cpos > 0 ) {
				statputc( --mbcursor, (int)' ' );
				if( (unsigned int)buf[--cpos] < 0x20 )
					statputc( --mbcursor,(int)' ' );
			}
			cpos = 0;
			break;
		case 0x07:	/* Abort		*/
			complete.flag = 0L;
			completion    = COMPLETE_ABORT;
			WDGmessage( ECSTR("Quit") );
			return( ctrlg() );

		case 0x0D:	/* Return		*/
		case 0x0A:	/* LineFeed		*/
 		case 0x12:	/* C-R, Back Search	*/
 		case 0x13:	/* C-S, Search		*/
			if( (c != 0x0D) && (c != 0x0A) ) {
				editflg = T;
				if( !(thisflag & (CFFSRC | CFBSRC)) )
					continue;
			}
			complete.flag = 0L;
			buf[ cpos++ ] = 0;
			if( kbdmip != NULL ) {
				if( kbdmip+cpos > &kbdm[NKBDM-3] )
					return( ctrlg() );
				for( i = 0 ; i < cpos ; ++i )
					*kbdmip++ = buf[i];
			}

			return( (buf[ 0 ] == 0) ? NIL : editflg );

#if	!defined( __MVS__ )
		case 0x7F:	/* Rubout, erase	*/
#endif
		case 0x08:	/* Backspace, erase	*/
		case Ctrl|'H':
			if( cpos != 0 ) {
				statputc( --mbcursor, (int)' ' );
				if( (unsigned int)buf[--cpos] < 0x20 )
					statputc( --mbcursor, (int)' ' );
			}
			break;

		case METACH:
			if( complete.fn == filematch ) {
				buf[ cpos ] = '\000';
				(void)updir( buf, SLASH );
				cpos = emstrlen( buf );
				if( cpos==0 || (cpos==2 && buf[1] == ':') ) {
					buf[ cpos++ ] = '/';
					buf[ cpos   ] = '\000';
				}
				mlwrite( ECSTR("%s%s"), prompt, buf );
			}
			break;

		case 0x0B:	/* kill ^K		*/
		 	mlclearentry( buf, cpos );
			cpos = 0;
			break;

		case 0x19:	/* yank ^Y		*/
		 	mlclearentry( buf, cpos );
			cpos = 0;

			i = 0;
			while( cpos < nbuf-1 ) {
				if( (c = kremove( i++ )) < 0 )
					goto doneyank;

				buf[cpos++] = (EMCHAR)c;
				if( c < ' ' ) {
					statputc( mbcursor++, (int)'^' );
					c ^= 0x40;
				}
				statputc( mbcursor++, c );
			}

doneyank:
			break;

		case 0x11:	/* Quote next char	*/
			c = TTYgetc();
			/*
			 *  FALL THRU : default clause MUST follow !!!
			 */

		default:
			if( complete.fn==filematch && cpos > 0 && c == ':' ) {
				/*
				 * Check for device change
				 */

				c    = buf[ cpos-1 ];
				mlclearentry( buf, cpos );
				cpos = 0;
				buf[ cpos++ ] = (EMCHAR)c;
				buf[ cpos++ ] = ':';
				buf[ cpos ]   = '\000';
				mlwrite( ECSTR("%s%s"), prompt, buf );
				editflg = T;
				continue;
			}

			if( allowcomplete == T )
				editflg = T;

			if( editflg == NIL ) {
			    if( complete.fn==filematch && c!=' ' && c!='\t' ) {
				if( cpos > 0 && buf[ cpos-1] == '/' ) {
					if( c == '/' || c == '\\' ) {
						mlclearentry( buf, cpos );
						cpos = 0;
					}
					editflg = T;
				 } else	{
				 	buf[ cpos ] = '\000';
					(void)updir( buf, SLASH );
					cpos          = emstrlen( buf );
					buf[ cpos++ ] = (EMCHAR)c;
					buf[ cpos ]   = '\000';
					mlwrite( ECSTR("%s%s"), prompt, buf );
					editflg = T;
					continue;
				 }
			    } else if( complete.fn != filematch ) {
				 	mlclearentry( buf, cpos );
					cpos = 0;
			    	   }
			}

			if( (c &= MAX_EMCHAR) == 0 )
				break;

			if( ((c==' ') || (c=='\t')) && (complete.flag != 0L) ) {
				buf[ cpos ] = '\000';
				completion  = COMPLETE_ONE;
				if( (s = (*complete.fn)(prompt,buf)) != NULL ) {
					(void)emstrcpy( buf, s );
					if( completion == COMPLETE_AGAIN )
						goto loop;
						
					complete.flag = 0L;
					return( T );
				} else	{
					complete.flag = 0L;
					if( completion == COMPLETE_ABORT )
						return( ABORT );
					completion = COMPLETE_FAIL;

					return( NIL );
				}
			}
			if( cpos < nbuf-1 ) {
				buf[cpos++] = (EMCHAR)c;
				if( c < ' ' ) {
					statputc( mbcursor++, (int)'^' );
					c ^= 0x40;
				}
				statputc( mbcursor++, c );
			}
		}
		editflg = T;
	}
}

/*
 *	Same as mledit, except that the previous buffer is empty.
 */

int
mlreply( EMCHAR *prompt, EMCHAR *buf, int nbuf )
{
	buf[ 0 ] = '\000';

	return( WDGedit( prompt, buf, nbuf ) );
}

/*
 *	Sounds the beeper, then display an error message on status line
 */

void
mlerror( EMCHAR *msg )
{
	TTYbeep();
	mlwrite( ECSTR("%s"), msg );
}

/*
 *	Write  a  message  in  a  printf like format into the message
 *	line. Keep track of the physical cursor position.
 */

/*VARARGS1*/

void
mlwrite( EMCHAR *fmt, ... )
{
	EMCHAR	c;
	int	i;
	EDLINE	*lp;
	EMCHAR	*ap;
	va_list	var;

	va_start( var, fmt );

	mbcursor = 0;

	while( (c = *fmt++) != 0 )
		if( c != '%' )
			statputc( mbcursor++, (int)c );
		else	switch( c = *fmt++ ) {
			case 'd':
				mlputl( (long)va_arg( var, int ), 10 );
				break;

			case 'l':
				mlputl( va_arg( var, long ), 10 );
				if( *fmt == 'd' )
					fmt++;
				break;

			case 'x':
				mlputl( (long)va_arg( var, int ), 16 );
				break;

 			case 'L':
				lp = (EDLINE *)va_arg( var, EDLINE * );
				if( llength( lp ) < (short)(TTYncol-1) )
					mlputs( ltext( lp ), llength( lp ) );
				else	mlputs( ltext( lp ), TTYncol-1 );
				break;

			case 's':
				ap = va_arg( var, EMCHAR * );
				mlputs( ap, emstrlen( ap ) );
				break;

			default:
				statputc( mbcursor++, (int)c );
			}
	
	for( i = mbcursor ; i < TTYncol ; i++ )
		statputc( i, (int)' ' );

	update( MINIBUF );
	mpresf = T;

	va_end( var );
}

/*
 *	Write out a string. Update the physical cursor position.
 */

static	void
mlputs( EMCHAR *s, int size )
{
	while( size-- )
		if( (unsigned int)*s < (unsigned int)' ' ) {
			statputc( mbcursor++, (int)'^' );
			statputc( mbcursor++, (int)'@' + (int)*s++ );
		} else	statputc( mbcursor++, (int)*s++ );

}

/*
 *	Write  out an integer,  in the specified  radix.  Update  the
 *	physical cursor position.
 */

static	void
mlputl( long i, int r )
{
	long	q;
	static	 EMCHAR	hexdigits[] = {
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
	};

	if( i < 0 ) {
		i = -i;
		statputc( mbcursor++, (int)'-' );
	}

	if( (q = i / r) != 0 )
		mlputl( q, r );

	statputc( mbcursor++, (int)hexdigits[ (int)(i % r) ] );
}

/*
 *	Set the title of application
 */

EMCHAR	*
mltitle( EMCHAR *s, EMCHAR *f )
{
	/*
	 * The  next  two  lines  is  dummy code to remove warning on
	 * args not used.
	 */

	if( s != f )
		f = s;

	return( f );
}

/*
 *	Change selection
 */

int
mlchange( EMCHAR *msgo, EMCHAR *msgn, EMCHAR *opat, EMCHAR *npat, int len )
{
	if( (mlreply( msgo, opat, len ) == T ) )
		return( (mlreply( msgn, npat, len ) != ABORT) ? T : NIL );
	else	return( NIL );
}

void
mlplay( int flag )
{
	/*
	 * The  next  two  lines  is  dummy code to remove warning on
	 * args not used.
	 */

	if( flag )
		mlwait();
}

void
mlwait( void )
{
}

void
mlmessage( EMCHAR *msg )
{
	mlwrite( ECSTR("%s"), msg );
}

void
mladjust( void )
{
}

void
mlclipcopy( void )
{
}

void
mlclippaste( void )
{
}

void
mlupdate( EMCHAR *prompt, EMCHAR *line )
{
	mlwrite( ECSTR("%s%s"), prompt, line );
}

void
mllpprint( void )
{
	mlwrite( ECSTR("Can't print on this system") );
}
