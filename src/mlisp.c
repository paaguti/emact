#if	!defined( lint )
static	char rcsid[] = "$Id: mlisp.c,v 1.8 2008/06/20 09:25:13 jullien Exp $";
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
 *	This  file implements the macro definition of Emacs.  A macro
 *	file  is  a  text  file with macro definition in Lisp syntax.
 *	The body is defined by a set of command name.
 */

#if	defined( _MLISP ) && !defined( _OPENLISP )

#include	"emacs.h"
#include	<setjmp.h>

static  int	_define(getfun,(void));
static  void	_define(fillmacro,(int key));
static  void	_define(fillcommand,(int key));
static  int	_define(nextchar,(void));
static  EMCHAR *_define(getword,(void));
static  int	_define(decrypt,(EMCHAR *string));
static  EMCHAR *_define(duplicate,(EMCHAR *s));
static  int	_define(getcode,(EMCHAR *s, int *indx));
static  int	_define(eval1,(int expr, int depth));
static	int	_define(loadmacro,(EMCHAR *macfile));
#if	defined( __GNUC__ )
static  void	_define(readerror,(EMCHAR *msg, EMCHAR *arg)) __attribute__((noreturn));
#else
static  void	_define(readerror,(EMCHAR *msg, EMCHAR *arg));
#endif

#define	NOFUNCTION	0
#define	FUNCTION	-1
#define	READFIRST	-2
#define	READSECOND	-3
#define	READTHIRD	-4
#define	LOADMACRO	-5
#define	WRITEMINIBUF	-6
#define	FINDNEXT	-7
#define	REPEAT		-8
#define	SETQ		-9
#define	INSERTNAME	-10
#define	INSERTBASENAME	-11
#define	UPDATE		-12
#define	INSERTCOMMAND	-13
/*
#define	DEFUN		-14
*/
#define	BINDTOKEY	-15
#define	INSERTSTRING	-16
#define	INPACKAGE	-17
/*
 * last two entries
 */
#define	NOTFOUND	-18
#define	FREE		-19

#define	MAXKBD		1024
#define	ARGLEN		64
#define	DEPTH		8

extern	KEYTAB	*pkeytab;
extern	int	nkeytab;
extern	VARTAB	*pvartab;
extern	int	nvartab;

static	int	*macrotab;
static	int	msize;

static	jmp_buf	save;

static	EMCHAR	*ARG1 = ECSTR("arg1");
static	EMCHAR	*ARG2 = ECSTR("arg2");
static	EMCHAR	*ARG3 = ECSTR("arg3");

static	EMCHAR	arg1[ ARGLEN ];
static	EMCHAR	arg2[ ARGLEN ];
static	EMCHAR	arg3[ ARGLEN ];

static	FILE	*mfile = NULL;

extern	MACTAB	*pmactab;
extern	int	nmactab;

/*
 *	Read  a new function from the current input file and return T
 *	if the definition has been read without error.
 */

static	int
getfun( void )
{
	EMCHAR	*word;
	EMCHAR	*name = NULL;
	int	pmain = 0;
	int	lpar  = 0;
	int	code  = 0;
	int	*buf;
	int	indx;
	int	c;
	int	i;

	if( nmactab == NMAX )
		readerror( ECSTR("Macro workspace is full."), (EMCHAR *)NULL );

	msize	= 0;

	while( (c = fgetc( mfile )) != EOF && c != '(' )
		;
	if( c == EOF )
		return( NIL );

	word = getword();

	if( !emstrcmp( word, ECSTR("defun") ) ) {
		code = FUNCTION;
		name = duplicate( getword() );
		while( nextchar() != '(' )
			;
		while( nextchar() != ')' )
			;
	} else	if( !emstrcmp( word, ECSTR("in-package") ) ) {
		while( nextchar() != ')' )
			;
		return( T );
	} else	if( !emstrcmp( word, ECSTR("main") ) ||
	            !emstrcmp( word, ECSTR("progn") ) )
		pmain++;
	else	readerror( ECSTR("Error in macro description file: "), word );

	if( code != FUNCTION )
		for( indx = 0 ; indx < nmactab ; indx++ ) {
			if( MACcode( indx ) == FREE )
				break;
			if( MACcode( indx ) == code ) {
				free( (EMCHAR *)MACname( indx ) );
				free( (EMCHAR *)MACexec( indx ) );
				break;
			}
		}
	else	for( indx = 0 ; indx < nmactab ; indx++ ) {
			if( MACcode( indx ) == FREE )
				break;
			if( name && !emstrcmp( MACname( indx ), name ) ) {
				/*
				 * Delete previous definition
				 */
				free( (EMCHAR *)MACexec( indx ) );
				break;
			}
		}

	if( indx == nmactab )
		/*
		 *	It's a new macro.
		 */
		nmactab++;

	MACcode( indx ) = code;
	MACname( indx ) = name;

	i = 0;

	while( (c = nextchar()) != EOF ) {
		if( c == ')' ) {
			if( lpar-- )
				continue;
			else	break;
		}
		if( c == '(' ) {
			word = getword();
			if( !emstrcmp( word, ECSTR("insert-string") ) )
				fillcommand( INSERTSTRING );
			else	if( !emstrcmp( word, ECSTR("load-macro") ) )
				fillcommand( LOADMACRO );
			else	if( !emstrcmp( word, ECSTR("write-minibuffer") ) )
				fillcommand( WRITEMINIBUF );
			else	if( !emstrcmp( word, ECSTR("find-next-pattern") ) )
				fillmacro( FINDNEXT );
			else	if( !emstrcmp( word, ECSTR("read-first-argument") ) )
				fillcommand( READFIRST );
			else	if( !emstrcmp( word, ECSTR("read-second-argument") ) )
				fillcommand( READSECOND );
			else	if( !emstrcmp( word, ECSTR("read-third-argument") ) )
				fillcommand( READTHIRD );
			else	if( !emstrcmp( word, ECSTR("setq") ) )
				fillcommand( SETQ );
			else	if( !emstrcmp( word, ECSTR("bind-to-key") ) )
				fillcommand( BINDTOKEY );
			else	if( !emstrcmp( word, ECSTR("insert-buffer-name") ) )
				fillmacro( INSERTNAME );
			else	if( !emstrcmp( word, ECSTR("insert-base-name") ) )
				fillmacro( INSERTBASENAME );
			else	if( !emstrcmp( word, ECSTR("in-package") ) )
				fillmacro( INPACKAGE );
			else	if( !emstrcmp( word, ECSTR("update-screen") ) )
				fillmacro( UPDATE );
			else	if( !emstrcmp( word, ECSTR("insert-system-command") ) )
				fillcommand( INSERTCOMMAND );
			else	if( !emstrcmp( word, ECSTR("repeat") ) ) {
					fillmacro( REPEAT );
					word = getword();
					while( *word )
						fillmacro((int)(*word++ - '0'));
					fillmacro( -1 );
					lpar++;
					continue;
				}
		 	else	if( (c = getcode( word, &i )) == NOTFOUND )
					readerror(
						  ECSTR("Invalid macro name: "),
						  word
						 );
				else	if( c == FUNCTION ) {
						fillmacro( FUNCTION );
						fillmacro( i );
				} else	fillmacro( c );

			while( nextchar() != ')' )
				;
		}
		if( c == ';' )
			while( nextchar() != '\n' )
				;
	}

	buf = (int *)malloc( ((size_t)(msize + 1) * sizeof( int )) );
	for( c = 0 ; c < msize ; c++ )
		buf[ c ] = macrotab[ c ];
	buf[ c ] = 0;

	MACexec( indx ) = buf;

 	if( pmain ) {
		(void)mlinternaleval( indx );
		MACcode( indx ) = FREE;
		free( (EMCHAR *)MACexec( indx ) );
	}

	return( T );
}

/*
 *	Fill  the  current  macro definition with 'key' and check for
 *	macro overflow.
 */

static	void
fillmacro( int key )
{
	if( msize < MAXKBD )
		macrotab[ msize++ ] = key;
	else	readerror(
			   ECSTR("Macro bigger than 1024 keys."),
			   (EMCHAR *)NULL
			 );
}

/*
 *	Fill the current macro with a specific command.
 */

static	void
fillcommand( int key )
{
	EMCHAR	*word = getword();
	int	i;

	fillmacro( key );

	if( key == SETQ  ) {
		for( i = 0 ; i < nvartab ; i++ )
			if( VARname( i ) && !emstrcmp( VARname( i ), word ) )
				break;

		if( i >= nvartab )
			readerror( ECSTR("Unknown variable. "), word );

		fillmacro( i );
		word = getword();
		if( VARtype( i ) == BOOLEAN ) {
			if( !emstrcmp( word, ECSTR("nil") ) )
				fillmacro( NIL );
			else	fillmacro( T );
			fillmacro( 0 );
			return;
		}
		if( VARtype( i ) == FIXVAL ) {
			fillmacro( atoi( (char *)word ) & 0xff );
			fillmacro( 0 );
			return;
		}
	}

	if( key == BINDTOKEY ) {
		for( i = 0 ; i < nmactab ; i++ )
			if( MACname( i ) && !emstrcmp( MACname(i), word ) )
				break;

		if( i >= nmactab )
			readerror( ECSTR("Unknown function. "), word );

		fillmacro( i );
		word = getword();
		while( *word )
			fillmacro( *word++ );
		fillmacro( 0 );
		return;
	}

	if( key == INPACKAGE ) {
		while( *word++ )
			;
		return;
	}

	if( word == ARG1 ) {
		fillmacro( 0xff );
		fillmacro( 1 );
	} else	if( word == ARG2 ) {
		fillmacro( 0xff );
		fillmacro( 2 );
	} else	if( word == ARG3 ) {
		fillmacro( 0xff );
		fillmacro( 3 );
	} else	do
			fillmacro( *word & 0xff );
		while( *word++ );
}

/*
 *	Read and return the next char available from macro file.
 */

static	int
nextchar( void )
{
	int	c;
	
	if( (c = fgetc( mfile )) == EOF )
	    readerror(
		       ECSTR("Error EOF encounter in macro file."),
		       (EMCHAR *)NULL
		     );

	return( c );

}

/*
 *	Read and return the next word avalaible from the macro file.
 */

static	EMCHAR	*
getword( void )
{
	static	EMCHAR	workbuf[ NPAT ];
	EMCHAR	*word;
	int	c;
	
	while( (c=nextchar()) != 0 && (separatorp(c) || c == '\'') && c != '"' )
		continue;
	
	word = &workbuf[ 0 ];

	if( c == '"' ) {
		while( (c = nextchar()) != '"' )
			if( c == '\\' ) {
				c = (EMCHAR)nextchar();
				if( c == '\\' )
					c = (EMCHAR)nextchar();
				switch( c ) {
				case 'n'  : *word++ = '\n'; break;
				case 't'  : *word++ = '\t'; break;
				default   : *word++ = (EMCHAR)c;
				}
			} else	*word++ = (EMCHAR)c;
		*word = '\0';
		return( workbuf );
	}

	(void)ungetc( c, mfile );
	
	while( ((c = nextchar()) != 0) && charp( c ) )
		*word++ = (EMCHAR)c;
	
	*word = '\0' ;

	(void)ungetc( c, mfile );
	
	if( !emstrcmp( ARG1, workbuf ) )
		return( ARG1 );
	if( !emstrcmp( ARG2, workbuf ) )
		return( ARG2 );
	if( !emstrcmp( ARG3, workbuf ) )
		return( ARG3 );
	else	return( workbuf );

}

/*
 *	Given a command string, return the associated code.
 */

static	int
decrypt( EMCHAR *string )
{
	int	meta	= 0;
	int	ctrl	= 0;
	int	ctrlx	= 0;
	int	ctrlz	= 0;
	int	spcl	= 0;

	switch( *string++ ) {
	case 'M' :
		if( *string++ == '-' )
			meta = META;
		else	readerror(
				   ECSTR("Bad command for a macro: "),
				   string-2
				 );
		break;
	case 'C' :
		if( string[ 0 ] == '-' ) {
			if( string[ 1 ] == 'X' ) {
				ctrlx = CTLX;
				string += 2;
			} else	if( string[ 1 ] == 'C' ) {
				ctrlz = CTLC;
				string += 2;
			}
		} else	{
			ctrl = Ctrl;
			string++;
		}
		break;
	case 'F' :
		if( *string++ == '-' )
			spcl = SPCL;
	}

	if( string[ 0 ] == 'C' && string[ 1 ] == '-' ) {
		string += 2;
		ctrl = Ctrl;
	}

	return( *string | meta | ctrlx | ctrlz | spcl | ctrl );

}

/*
 *	Return a copy of a pre-allocated string.
 */

static	EMCHAR	*
duplicate( EMCHAR *s )
{
	s=(EMCHAR *)emstrcpy(
			      malloc(sizeof(EMCHAR)*((unsigned)emstrlen(s)+1)),
			      s
			    );
	return( s );

}

/*
 *	Return the code of a command name.
 */

static	int
getcode( EMCHAR *s, int *indx )
{
	KEYTAB *ktp;
	int	i;

	/*
	 *	Look in macro table.
	 */

	for( i = 0 ; i < nmactab ; i++ )
		if( MACname( i ) && !emstrcmp( s, MACname( i )) ) {
			*indx = i;
			return( MACcode( i ) );
		}

	/*
	 *	Look in key table.
	 */

	for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ )
		if( !emstrcmp( s, ktp->k_name ) )
			return( ktp->k_code );

	return( NOTFOUND );
}

/*
 *	Evaluate a macro definition.
 */

static	int
eval1( int expr, int depth )
{
	KEYTAB	*ktp;
	int	*bufcmd;
	EMCHAR	*strcmd = (EMCHAR*)NULL;
	EMCHAR	string[ NPAT ];
	int	c;
	int	i;
	int	n = 1;
	int	s = T;
	int	sv;
	int	code;

	if( depth > DEPTH ) {
		WDGmessage( ECSTR("Too many level of call in macro.") );
		return( NIL );
	}

	bufcmd = MACexec( expr );
	while( (c = *bufcmd) != 0 && s==T ) {
		if( n <= 0 )
			n = 1;

		switch( c ) {

		case READFIRST  :
		case READSECOND :
		case READTHIRD  :
			bufcmd++;
			for( i = 0 ; *bufcmd ; ++i )
				string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
			string[ i ] = '\0';
			switch( c ) {
			case READFIRST  :
				if( mlreply( string, arg1, ARGLEN ) != T )
					return( NIL );
				break;
			case READSECOND :
				if( mlreply( string, arg2, ARGLEN ) != T )
					return( NIL );
				break;
			case READTHIRD  :
				if( mlreply( string, arg3, ARGLEN ) != T )
					return( NIL );
				break;
			}
			break;

		case INSERTSTRING :
			bufcmd++;
			if( *bufcmd == 0xff ) {
				switch( *++bufcmd ) {
				case 1 :
					strcmd = arg1;
					break;
				case 2 :
					strcmd = arg2;
					break;
				case 3 :
					strcmd = arg3;
					break;
				}

				while( n-- > 0 && s ) {
					EMCHAR	*str = strcmd;

					while( *str && s ) {
						switch( *str ) {
						case '\t': s=tabexpand();break;
						case '\n': s=endline();  break;
						default  : s=linsert(1,*str);
						}
						str++;
					}
				}

			} else	{
				int	*cbuf = bufcmd;

				while( n-- > 0 && s ) {
					bufcmd = cbuf;
					while( *bufcmd && s ) {
						switch( *bufcmd ) {
						case '\t': s=tabexpand();break;
						case '\n': s=endline();  break;
						case '\\':
							if(*(bufcmd+1)!='n')
							   s=linsert(1,*bufcmd);
							else {
							   bufcmd++;
							   s = endline();
							}
							break;
						default  : s=linsert(1,*bufcmd);
						}
						bufcmd++;
					}
				}
				}

			break;

		case LOADMACRO :
			strcmd = (EMCHAR *)emstrcpy( string, library );
			bufcmd++;
			for( i = emstrlen( strcmd ) ; *bufcmd ; ++i )
				strcmd[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
			strcmd[ i ] = '\0';
			WDGwrite( ECSTR("%s"), strcmd );
			s = loadmacro( strcmd );
			break;

		case WRITEMINIBUF :
			bufcmd++;
			for( i = 0 ; *bufcmd ; ++i )
				string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
			string[ i ] = '\0';
			WDGwrite( ECSTR("%s"), string );
			break;

		case FINDNEXT :
			while( n-- && s )
				if( (s = ffindstring()) == T ) {
					curwp->w_dotp  = found_p;
					curwp->w_doto  = found_o;
					curwp->w_flag |= WFMOVE;
				}
			break;

		case REPEAT :
			n = 0;
			bufcmd++;
			do
				n = n * 10 + *bufcmd++;
			while( *bufcmd != -1 );
			++bufcmd;
			continue;

		case SETQ :
			bufcmd++;
			c	= *bufcmd++;
			strcmd	= (EMCHAR *)VARval( c );
			s	= VARtype( c );
			i	= 0;

			if( s == BOOLEAN || s == FIXVAL ) {
				int *intp = VARval( c );
				*intp = *bufcmd++;
			} else	{
				while( *bufcmd && ++i < s )
				  *strcmd++ = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
				*strcmd = '\0';
			}
			s = T;
			break;

		case BINDTOKEY :
			bufcmd++;
			c = *bufcmd++;
			for( i = 0 ; *bufcmd ; ++i )
				string[ i ] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
			string[ i ] = '\0';
			code = decrypt( string );

			/*
			 *	Delete previous binding of standard key.
			 */

			for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ )
				if( ktp->k_code == code ) {
					ktp->k_code = UNBOUND;
					break;
				}

			MACcode( c ) = code;
			break;

		case FUNCTION :
			if( *++bufcmd == expr ) {
				bufcmd = MACexec( expr );
				continue;
			} else	while( n-- && s )
					s = eval1( *bufcmd, depth+1 );
			break;

		case INSERTNAME :
			for( strcmd = curbp->b_bname ; *strcmd && s ; strcmd++ )
				s = linsert( 1, *strcmd );
			break;

		case INSERTBASENAME :
			for( strcmd = curbp->b_bname ; *strcmd && s ; strcmd++ )
				if( *strcmd == '.' )
					break;
				else	s = linsert( 1, *strcmd );
			break;

		case UPDATE :
			update( T );
			break;

		case INSERTCOMMAND :
			bufcmd++;
			for( i = 0 ; *bufcmd ; ++i )
				string[i] = (EMCHAR)(*bufcmd++ & MAX_EMCHAR);
			string[ i ] = '\0';
			WDGwrite( ECSTR(": execute '%s'"), string );
			s = syscompile( string, SYSCOMP_NOERROR );
			break;

		case INPACKAGE :
			bufcmd++;
			break;

		case NOFUNCTION :
			break;

		default :
			sv	= repeat;
			s	= execute( c, n );
			repeat	= sv;
		}
		n = 1;
		bufcmd++;
	}

	return( *bufcmd ? NIL : T );
}

/*
 *	An error occurs while reading a macro definition.
 */

static	void
readerror( EMCHAR *msg, EMCHAR *arg )
{
	EMCHAR	buf[ NPAT ];

	(void)emstrcpy( buf, msg  );
	(void)emstrcat( buf, arg  );
	(void)WDGmessage( buf );
	longjmp( save, -1 );
}

/*
 *	Load interactively a macro definition file.
 */

static int
loadmacro( EMCHAR *macfile )
{
	if( setjmp( save ) != 0 ) {
		(void)fclose( mfile );
		mfile = (FILE *)NULL;
		return( NIL );
	}

	if( ffaccess( macfile ) != FIOSUC )
		return( FIOFNF );

	if( (mfile = ffopen( macfile, ECSTR("r"), NULL )) != 0 ) {
		while( getfun() == T )
			;
		(void)fclose( mfile );
		mfile = (FILE *)NULL;
		return( T );
	} else	return( FIOFNF );
}

/*
 *	Load the default macro definition.
 */

int
mlcustomize( void )
{
	int	s;
	EMCHAR	*p;
	EMCHAR	init[ NPAT ];
	EMCHAR	base[ NPAT ];

	if( (p = ffgetenv( ECSTR("EMACSLIB") )) == NULL )
		(void)emstrcpy( library, PATH );
	else	(void)emstrcpy( library, p );

#if	!defined( _VMS )
	(void)emstrcat( library, ECSTR("/") );
#endif

	(void)emstrcpy( init, library );
	(void)emstrcat( init, ECSTR("emacs.lsp") );

	/*
	 * Try to find load emacs.lsp
	 */

	if( (mfile = ffopen( init, ECSTR("r"), NULL )) != 0 ) {
		/*
		 * The PATH is at the standard place.
		 */
		(void)fclose( mfile );
		mfile = (FILE *)NULL;
	} else	{
		/*
		 * Set the library PATH where the binary is found.
		 */
		(void)emstrcpy( library, eargv[0] );
		(void)updir( library, SLASH );
#if	defined( _DOSPATH )
		(void)emstrlwr( library );
#endif
	}

	macrotab = (int *)malloc( (unsigned)MAXKBD * sizeof( int ) );

	makename( (EMCHAR *)&base[0], eargv[ 0 ] );

	for( p = base ; *p ; p++ )
		if( *p == '.' ) {
			*p = '\000';
			break;
		}
	(void)emstrcat( base, ECSTR(".lsp") );

	(void)emstrcpy( init, library );
	(void)emstrcat( init, base );

	if( (s = loadmacro(base)) == FIOFNF &&
	    (s = loadmacro(init)) == FIOFNF )
		return( T );
	else	return( s );
}

/*
 *	Evaluate a macro definition n times.
 */

CMD
mlinternaleval( int expr )
{
	int n = repeat;

	while( n-- > 0 )
		if( eval1( expr, 1 ) != T )
			return( NIL );

	return( T );
}

/*
 *	Read  a  macro  file into the current macro buffer.  Find the
 *	name of the file,  and call the standard loadmacro code.  The
 *	file   is   search   in   current  directory  first  then  in
 *	/usr/local/emact/lib  or  EMACSLIB  directory  if  not found.
 *	/Bound to "M-C-M".
 */

CMD
getmacfile( void )
{
	int	s;
	EMCHAR	fname[ NFILEN ];

	complete.fn = filematch;

	s = mlreply( ECSTR(": macro-file "), fname, NFILEN );

	return( ((s == T) && loadmacro( fname )) ? T : NIL );

}

CMD
lispevalbuffer( void )
{
	WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

CMD
evalexpression( void )
{
	WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

#endif

#if	!defined( _MLISP ) && !defined( _OPENLISP )

/*
 *	Empty definitions if no lisp is used.
 */

#include "emacs.h"

int
mlcustomize( void )
{
	return( NIL );
}

CMD
getmacfile( void )
{
	WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

CMD
mlinternaleval( int expr )
{
	if( expr >= 0 )
		WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

CMD
lispevalbuffer( void )
{
	WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

CMD
evalexpression( void )
{
	WDGerror( ECSTR("Not implemented !") );
	return( NIL );
}

#endif
