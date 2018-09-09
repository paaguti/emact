#if	!defined( lint )
static	char rcsid[] = "$Id: openlisp.c,v 1.16 2012/10/21 12:23:34 jullien Exp $";
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
 *	openlisp.c :
 */


#include	"emacs.h"

#if	defined( _OPENLISP )

#if	defined( _MSC_VER )
#pragma warning( disable : 4311 4312 )
#endif

#define	EMPACKAGE			LCSTR("emacs")
#define	_VARIABLES_IN_EMACS_PACKAGE

extern	MACTAB	*pmactab;
extern	int	nmactab;
extern	VARTAB	*pvartab;
extern	int	nvartab;

/*
 *	Standard  zones are defined for the lowest architecture (i.e.
 *	16  bits  machines),  so  WORDSIZE  adjust  memory  for other
 *	processors.
 */

#define	REALZONE	(1  * WORDSIZE)
#define	STRGZONE	(2  * WORDSIZE)
#define	VECTZONE	(2  * WORDSIZE)
#define	SYMBZONE	(8  * WORDSIZE)
#define	CONSZONE	(20 * WORDSIZE)
#define	HEAPZONE	(30 * WORDSIZE)

#define	DEFAUTLVALUE	olmakefix( 0x8000 )
#define defaultp( o )	eq( cval( o ), DEFAUTLVALUE )

extern	KEYTAB	*pkeytab;
extern	int	nkeytab;

EXTERN_C int	_define(evalloop,(void));

static	int	_define(initopenlisp,(void));
static	void	_define(fillreadbuffer,(void));
static	void	_define(olemttynextline,(void));
static	void	_define(olemttyputs,(LCHAR *str, size_t len, FILE *fd));
static	int	_define(decrypt,(EMCHAR *str));
static	POINTER	_define(insertstring,(POINTER s));
static	POINTER	_define(insertchar,(POINTER c));
static	POINTER	_define(ldmacro,(POINTER s));
static	POINTER	_define(readchar,(void));
static	POINTER	_define(readstring,(POINTER prompt));
static	POINTER	_define(writeminibuf,(POINTER s));
static	POINTER	_define(insertname,(void));
static	POINTER	_define(insertbasename,(void));
static	POINTER	_define(updatescreen,(void));
static	POINTER	_define(bindtokey,(POINTER symb, POINTER key));
static	LCHAR*	_define(emstrtolstr,(EMCHAR *s));
static	EMCHAR*	_define(lstrtoemstr,(LCHAR *s));


/*
 *	Initialisation
 */

static	FTAB	ftab[] = {
	{ (PFUN)bindtokey,		SUBR2,	LCSTR("bind-to-key")	    },
	{ (PFUN)insertchar,		SUBR1,	LCSTR("insert-char")	    },
	{ (PFUN)insertstring,		SUBR1,	LCSTR("insert-string")	    },
	{ (PFUN)readchar,		SUBR0,	LCSTR("read-character")	    },
	{ (PFUN)readstring,		SUBR1,	LCSTR("read-string")	    },
	{ (PFUN)ldmacro,		SUBR1,	LCSTR("load-macro")	    },
	{ (PFUN)writeminibuf,		SUBR1,	LCSTR("write-minibuffer")   },
	{ (PFUN)insertname,		SUBR0,	LCSTR("insert-buffer-name") },
	{ (PFUN)updatescreen,		SUBR0,	LCSTR("update-screen")	    },
	{ (PFUN)insertbasename,		SUBR0,	LCSTR("insert-base-name")   }
};

#define	EVALBUFFERSIZE	1024

static	BUFFER	*evalbp	  = (BUFFER *)NULL;
static	EDLINE	*curline  = (EDLINE *)NULL;
static	LCHAR	curevalbuf[ EVALBUFFERSIZE ];

/*
 *	Convert a EMCHAR * to LCHAR *.
 */

static LCHAR *
emstrtolstr( EMCHAR *s )
{
	static LCHAR	tab[ OLMAXSTRCONV ];
	int		i;

	if( s == NULL )
		return( NULL );

	for( i = 0 ; i < (OLMAXSTRCONV-1) && *s ; ++i )
		tab[ i ] = (LCHAR)*s++;

	tab[ i ] = '\000';

	return( (LCHAR *)&tab[0] );
}

/*
 *	Convert a LCHAR * to EMCHAR *.
 */

static EMCHAR *
lstrtoemstr( LCHAR *s )
{
	static EMCHAR	tab[ OLMAXSTRCONV ];
	int		i;

	if( s == NULL )
		return( NULL );

	for( i = 0 ; i < (OLMAXSTRCONV-1) && *s ; ++i )
		tab[ i ] = (EMCHAR)*s++;

	tab[ i ] = '\000';

	return( (EMCHAR *)&tab[0] );
}

/*
 *	Initialize  OpenLisp.  This  routine is should be called only
 *	once by olcusomize.
 */

static	int
initopenlisp( void )
{
	int	res;
	int	realzone  = REALZONE;	/* in page (4ko/page)	*/
	int	strgzone  = STRGZONE;	/* in page (4ko/page)	*/
	int	vectzone  = VECTZONE;	/* in page (4ko/page)	*/
	int	symbzone  = SYMBZONE;	/* in page (4ko/page)	*/
	int	conszone  = CONSZONE;	/* in page (4ko/page)	*/
	int	heapzone  = HEAPZONE;	/* in Ko		*/
	LCHAR	*initname = NULL;	/* initname		*/
	LCHAR	*corename = NULL;	/* corename		*/

	/*
	 *	Variables not used in this samples :
	 */

	(void)initname;
	(void)corename;

	/*
	 *	OpenLisp  initialization  must occurs just after main
	 *	procedure  to  insure  correctness  of allocation for
	 *	LISP tagged types.
	 */

	olregisterhook( OLCB_NEXTLINE, (PFUN)olemttynextline );
	olregisterhook( OLCB_PUTS,     (PFUN)olemttyputs     );

	res = olinit(
		      eargc,
		      (char **)eargv,
		      (char *)NULL,
		      realzone,
		      strgzone,
		      vectzone,
		      symbzone,
		      conszone,
		      heapzone
		    );

	if( res != 0 )
		/*
	         * fail to initialize OpenLisp
		 */
		return( NIL );

	return( T );
}

/*
 *	Customize  EmACT with Lisp files.  At this point,  if a valid
 *	OpenLisp is not running we must launch it.  Then global EmACT
 *	variables  are  also  defined for OpenLisp.  The last part of
 *	initialization   is  to  load  OpenLisp  startup  files  and,
 *	finally, EmACT custom files.
 */

int
olcustomize( void )
{
	static	int	emacsenv   = NIL;
	static	int	emacsfiles = NIL;
	static	int	lispfiles  = NIL;

	POINTER	VOLATILE empkgname;
	POINTER	VOLATILE empkg;
	POINTER symb;
	KEYTAB	*ktp;
	VARTAB	*vtp;
	POINTER	obj;
	POINTER	opkg;
	FILE	*mfile;
	EMCHAR	init[ NPAT ];
	EMCHAR	base[ NPAT ];
	LCHAR	*lp;
	EMCHAR	*p;
	int	res = T;
	int	s;

	olregisterhook( OLCB_NEXTLINE, (PFUN)olemttynextline );
	olregisterhook( OLCB_PUTS,     (PFUN)olemttyputs     );

	if( olrunning == NIL ) {
		/*
		 *	Lisp is not already running.
		 */
		if( initopenlisp() != T ) {
			return( NIL );
		}
	} else	{
		lispfiles = T;
	}

	OLENTERLISP;
	empkgname = olmakeinternalsymbol( olsymbopenlisp, EMPACKAGE );
	empkg     = olcreatepackage3( empkgname, nil, olpkgopenlisp );
	opkg	  = olinpackage( empkg );

	if( emacsenv == NIL ) {
		for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ ) {
			(void) olfentry( (PFUN)ktp->k_fp,
					 SUBR0,
					 (LCHAR *)ktp->k_name
				       );
		}

		olinitexternalmodulenames( ftab );

		/*
		 *	Create global EmACT variables
		 */

		for( vtp = pvartab ; vtp < (pvartab + nvartab) ; vtp++ ) {
		     symb         = olusersymbol( (LCHAR*)vtp->f_name );
		     cval( symb ) = DEFAUTLVALUE;
		}

		emacsenv = T;
	}


	if( (p = ffgetenv( ECSTR("EMACSLIB") )) == NULL )
		(void)emstrcpy( library, PATH );
	else	(void)emstrcpy( library, p    );

#if	!defined( _VMS )
	(void)emstrcat( library, ECSTR("/") );
#endif

	if( lispfiles == NIL ) {
		(void)emstrcpy( init, library		   );
		(void)emstrcat( init, ECSTR("startup.lsp") );

		obj = olnewstring( (LCHAR *)emstrtolstr( init ) );

		if( not( null( olprobefile( obj ) ) ) ) {
			(void)olload1( obj, dynval( olsymbdefencoding ) );
		} else	{
			res = NIL;
			goto exitinit;
		}

		lispfiles = NIL;
	}

	if( emacsfiles == NIL ) {
		(void)emstrcpy( init, library		 );
		(void)emstrcat( init, ECSTR("emacs.lsp") );

		/*
		 * Try to find emacs.lsp
		 */

		if( (mfile = ffopen( init, ECSTR("r"), NULL )) != 0 )
			/*
			 * The PATH is at the standard place.
			 */
			 (void)fclose( mfile );
		else	{
			/*
			 * Set the library PATH where the binary is found.
			 */
			(void)emstrcpy( library, eargv[ 0 ] );
			(void)updir( library, SLASH );
#if	defined( _DOSPATH )
			(void)emstrlwr( library );
#endif
		}

		makename( base, eargv[ 0 ] );

		for( p = base ; *p ; p++ )
			if( *p == '.' ) {
				*p = '\000';
				break;
			}

		(void)emstrcat( base, ECSTR(".lsp") );
		(void)emstrcpy( init, library	    );
		(void)emstrcat( init, base	    );

		obj = olnewstring( (LCHAR *)emstrtolstr( init ) );

		if( not( null( olprobefile( obj ) ) ) ) {
			(void)olload1( obj, dynval( olsymbdefencoding ) );
		} else	{
			res = NIL;
			goto exitinit;
		}

		emacsfiles = T;
	}

	/*
	 *	Set  global  EmACT variables from OpenLisp equivalent
	 *	symbols.   This  section  is  always  executed  since
	 *	OpenLisp  may  have change those values independently
	 *	form EmACT.
	 */

	for( vtp = pvartab ; vtp < (pvartab+nvartab) ; vtp++ ) {
		POINTER	o;
		int *intp;

#if	defined( _VARIABLES_IN_EMACS_PACKAGE_ )
		olstrlcpy( (LCHAR *)&name[0], EMPACKAGE, OLMAXPLEN );
		olstrlcat( (LCHAR *)&name[0], LCSTR(":"), OLMAXPLEN );
		olstrlcat( (LCHAR *)&name[0], (LCHAR *)vtp->f_name, OLMAXPLEN );

		o = olusersymbol( (LCHAR *)&name[0] );
#else
		o = olusersymbol( (LCHAR *)vtp->f_name );
#endif

		switch( vtp->f_type ) {
		case BOOLEAN :
			intp = (int *)vtp->f_val;
			if( !defaultp( o ) )
			     *intp = (null( cval(o) ) ? NIL : T);
			else cval( o ) = (*intp == NIL) ? nil : t;
			break;
		case FIXVAL :
			intp = (int *)vtp->f_val;
			if( !defaultp( o ) )
				*intp = (int)olfix( cval(o) );
			else	cval( o ) = olmakefix( *intp );
			break;
		default:
			if( !defaultp( o ) ) {
				lp = olstrval(olgetstring(LCSTR("setq"),cval(o)));
				p  = lstrtoemstr( lp );
				(void)emstrcpy((EMCHAR *)vtp->f_val,(EMCHAR *)p );
			} else	{
				if( *((EMCHAR *)vtp->f_val) == 0 ) {
					cval( o ) = nil;
					break;
				}
				s	= emstrlen( (EMCHAR *)vtp->f_val );
				cval(o) = olallocstring( olmakefix(s), NULLPTR );
				(void)olstrlcpy(
					        olstrval(cval(o)),
						emstrtolstr((EMCHAR*)vtp->f_val),
						s
					      );
			}
			break;
		}
	}

exitinit:
	olinpackage( opkg );
	OLLEAVELISP;

	return( res );
}

/*
 *	Evaluate  a  lisp  expression  taken  form EmACT mini-buffer.
 *	Bound to M-M.
 */

CMD
evalexpression( void )
{
	EMCHAR	cmd[ NFILEN ];
	int	s;

	cmd[ 0 ] = '\000';

	if( (s = WDGedit( ECSTR("Eval: "), cmd, NFILEN ))!=T && s!=NIL )
		return( s );

	if( *cmd ) {
		POINTER	res;

		olreadbuf = emstrtolstr( cmd );
		res	  = olreadexpr( NULLPTR, t, t );
		olreadbuf = (LCHAR *)NULL;
		(void)olevalenv( res, OLNULLENV );
	}

	return( T );
}

CMD
olinternalexec( int i )
{
	int n = repeat;

	repeat = 1;

	olpush( symbfn( MACexec( i ) ) );
	while( n-- > 0 )
		(void)olfuncall( 1 );
	oladjstk( 1 );

	return( T );
}

/*
 *	Read  a  macro  file into the current macro buffer.  Find the
 *	name of the file,  and call the standard loadmacro code.  The
 *	file   is   search   in   current  directory  first  then  in
 *	/usr/lib/emacs if not found. Bound to "M-C-M".
 */

CMD
getmacfile( void )
{
	int	s;
	EMCHAR	fname[NFILEN];

	complete.fn = filematch;

	s = mlreply( ECSTR("Load macro file: "), fname, NFILEN );

	if( s == T ) {
		POINTER	o;
		int	len = emstrlen(fname);

		o = olallocstring(olmakefix(len), NULLPTR);
		(void)olstrlcpy( olstrval( o ), emstrtolstr( fname ), len );
		(void)olload1( o, dynval( olsymbdefencoding ) );
	}
		
	return( T );

}

/*
 *	Load a macro-file.
 */

static	POINTER
ldmacro( POINTER s )
{
	LCHAR	buf[ 128 ];
	LCHAR	*p;
	int	len;

	p   = olstrval( olgetstring( LCSTR("load-macro"), s ) );

	(void)olstrlcpy( buf, emstrtolstr( library ), 128 );
	(void)olstrlcat( buf, p, 128 );

	len = (int)olstrlen( buf );
	s   = olallocstring( olmakefix( len ), NULLPTR );
	(void)olstrlcpy( olstrval( s ), buf, len );
	(void)olload1( s, dynval( olsymbdefencoding ) );

	return( t );
}

/*
 *	Insert  the Lisp string 's' at the current location of active
 *	buffer.
 */

static	POINTER
insertstring( POINTER s )
{
	LCHAR	*str = olstrval(olgetstring(LCSTR("insert-string"),s));
	int	f    = T;

	while( *str && f == T ) {
		switch( *str ) {
		case '\t': f = tabexpand(); break;
		case '\n': f = endline();   break;
		case '\\': if( *(str+1) == 'n' ) {
				str++;
				f = endline();
			   }
			   break;
		default  : f = linsert( 1, (EMCHAR)*str );
		}
		str++;
	}

	return( (f == T) ? t : nil );
}

/*
 *	Insert  the  Lisp  character  'c'  at the current location of
 *	active buffer.
 */

static	POINTER
insertchar( POINTER c )
{
	int	f;
	int	code;

	if( not( characterp( c ) ) )
		return(oldomainerror(SN_SETMACROCHARACTER, c, olcharacter_t));

	switch( (code = olcharval( c )) ) {
	case '\t': f = tabexpand(); break;
	case '\n': f = endline();   break;
	default  : f = linsert( 1, code );
	}

	return( (f == T) ? t : nil );
}

static	POINTER
readchar( void )
{
	return( olmakechar( (LCHAR)lgetdot() ) );
}

/*
 *	Read  a string from minibuffer and convert the buffer into an
 *	OpenLisp string.
 */

static	POINTER
readstring( POINTER prompt )
{
	EMCHAR	buf[ NLINE ];
	POINTER	res;
	int	len;

	prompt = olgetstring( LCSTR("read-string"), prompt );
	buf[0] = '\000';

	if( WDGedit( lstrtoemstr( olstrval( prompt ) ), buf, NLINE ) != T )
		return( nil );

	len = (int)emstrlen( buf );
	res = olallocstring( olmakefix( len ), NULLPTR );
	(void)olstrlcpy( olstrval( res ), emstrtolstr( buf ), len );

	return( res );
}

/*
 *	Write the OpenLisp string 's' on minibuffer.
 */

static	POINTER
writeminibuf( POINTER s )
{
	LCHAR	*str = olstrval(olgetstring(LCSTR("insert-string"),s));
	WDGwrite( ECSTR("%s"), lstrtoemstr( str ) );

	return( t );
}

/*
 *	Insert the filename at the current location of active buffer.
 */

static	POINTER
insertname( void )
{
	EMCHAR	*str;
	int	s = T;

	for( str = curbp->b_bname ; *str && (s == T) ; str++ )
		s = linsert( 1, *str );

	return( (s == T) ? t : nil );
}

/*
 *	Insert the basename at the current location of active buffer.
 */

static	POINTER
insertbasename( void )
{
	EMCHAR	*str;
	int	s = T;

	for( str = curbp->b_bname ; *str && (s == T) ; str++ ) {
		if( *str == '.' )
			break;
		s = linsert( 1, *str );
	}

	return( (s == T) ? t : nil );
}

/*
 *	Force a full update.
 */

static	POINTER
updatescreen( void )
{
	update( T );
	return( t );
}

/*
 *	Decrypt a command binding.
 */

static	int
decrypt( EMCHAR *strg )
{
	int	meta	= 0;
	int	ctrl	= 0;
	int	ctrlx	= 0;
	int	ctrlz	= 0;
	int	spcl	= 0;

	switch( *strg++ ) {
	case 'M' :
		if( *strg++ == '-' )
			meta = META;
		else	(void)oldomainerror(
					     LCSTR("bind-to-key"),
					     NULLPTR,
					     olcharacter_t
					   );
		break;
	case 'C' :
		if( strg[ 0 ] == '-' )
			if( strg[ 1 ] == 'X' ) {
				ctrlx = CTLX;
				strg += 2;
			} else	if( strg[ 1 ] == 'C' ) {
				ctrlz = CTLC;
				strg += 2;
			}
		else	{
			ctrl = Ctrl;
			strg++;
		}
		break;
	case 'F' :
		if( *strg++ == '-' )
			spcl = SPCL;
	}

	if( strg[ 0 ] == 'C' && strg[ 1 ] == '-' ) {
		strg += 2;
		ctrl = Ctrl;
	}

	return( *strg | meta | ctrlx | ctrlz | spcl | ctrl );

}

/*
 *	Bind a lisp function to a given key.
 */

static	POINTER
bindtokey( POINTER fun, POINTER key )
{
	KEYTAB	*ktp;
	EMCHAR	*s;
	int	code;
	int	len;
	int	index;

	key = olgetstring( LCSTR("bind-to-key"), key );

	if( not( symbolp( fun ) ) )
		return(oldomainerror( LCSTR("bind-to-key"), fun, olsymbol_t ));

	s = lstrtoemstr( olstrval( olgetstring( LCSTR("bind-to-key"), key) ) );

	code = decrypt( s );

	for( index = 0 ; index < nmactab ; index++ )
		if( MACcode( index ) == code )
			/*
			 *	It's a macro redefinition
			 */
			break;

	if( index == NMAX )
		(void)olcannotcreatestring( LCSTR("bind-to-key") );

	/*
	 *	Delete previous binding in case of rebind.
	 */

	for( ktp = pkeytab ; ktp < (pkeytab+nkeytab) ; ktp++ )
		if( ktp->k_code == code ) {
			/*
			 *	It's a predefinied key rebind
			 */
			ktp->k_code = UNBOUND;
			break;
		}

	if( index == nmactab ) {
		/*
		 *	It's a new macro.
		 */
		nmactab++;
	} else	{
		free( MACname( index ) );
	}

	len = (int)olstrlen( olstrval( pname( fun ) ) ) + 1;
	s   = (EMCHAR *)malloc( len * sizeof( EMCHAR ) );

	if( s != NULL ) {
		emstrcpy( s, lstrtoemstr( olstrval( pname( fun ) ) ) );
	}

	MACcode( index ) = code;
	MACname( index ) = s; // lstrtoemstr( olstrval( pname( fun ) ) );
	MACexec( index ) = fun;

	return( t );
}

/*
 *	Fill OpenLisp readbuffer with the content of current line.
 */

void
fillreadbuffer( void )
{
	int	lmax = llength( curline );

	if( lmax > (EVALBUFFERSIZE - 2) ) {
		olreadbuf = (LCHAR *)NULL;
		WDGyn( ECSTR("Line too long.") );
		return;
	}

	(void)olstrlcpy( curevalbuf, emstrtolstr( ltext( curline ) ), lmax );

	curevalbuf[ lmax     ] = '\n';
	curevalbuf[ lmax + 1 ] = '\0';

	olreadbuf = (LCHAR *)&curevalbuf[ 0 ];
}

/*
 *	Fill OpenLisp readbuffer with the content of next line.
 */

static void
olemttynextline( void )
{
	if( evalbp == (BUFFER *)NULL || curline == lastline( evalbp ) ) {
		olreadbuf = (LCHAR *)NULL;
		evalbp    = (BUFFER *)NULL;
		return;
	}

	curline = lforw( curline );

	fillreadbuffer();
}

static void
olemttyputs( LCHAR *str, size_t len, FILE *fd )
{
	int	f = T;

	(void)len;

	if( editflag == NIL ) {
		(void)fputs( (char *)str, fd );
		return;
	}

	if( initflag == 0 )
		return;

	while( *str && f == T ) {
		switch( *str ) {
		case '\t': f = tabexpand(); break;
		case '\n': f = endline();   break;
		default  : f = linsert( 1, (EMCHAR)*str );
		}
		str++;
	}
}

/*
 *	Internal   eval-loop  to  evaluate  the  content  of  current
 *	buffer.  Install an error handler. Returns 0 on succes or the
 *	error code cautch by OpenLisp.
 */

EXTERN_C int
evalloop( void )
{
	POINTER	res;
	int	err;

	olpush( dynval( olsymbpackage ) );
	dynval(olsymbpackage) = olpkguser;

	olpushenv();

	if( olcatchonstack( "EmACT" ) == 0 ) {
		/*
		 *	Install handler to catch evaluation errors.
		 */
		olpush( olerror_t );
		oladjstk( -HEADER_ENV_SIZE );
		olxspmov( 0 ) = catchtag;
		olxspmov( 1 ) = olerror_t;
		olxspmov( 2 ) = olmakefix( 0 );
		olxspmov( 3 ) = olenv;
#if	(HEADER_ENV_SIZE > 4 )
		olxspmov( 4 ) = NULLPTR;
#endif

		for( ;; ) {
			res = olreadexpr( NULLPTR, nil, olsymbundef );

			if( res == olsymbundef )
				break;

			res = olevalenv( res, OLNULLENV );

			if( print_eval_loop == T ) {
				(void)olprobj( STREAMOUT, res );
				(void)olformatfreshline( STREAMOUT );
			}
		}
		err = 0;
	} else	{
		err = 1;
	}

	oladjstk( olenvsize + HEADER_ENV_SIZE + 1 );

	olpopenv();

	olpop( dynval( olsymbpackage ) );

	/*
	 *	When  error occurs,  olcatch returns a non-zero value
	 *	and olerrtype contains the error code.
	 */

	return( (err == 0) ? 0 : olerrtype );
}

/*
 *	Evaluate  the  content of current buffer.  The buffer must be
 *	in Lisp mode.  Error handlers are set to take control in case
 *	of  evaluation errors.  The result of evaluation is displayed
 *	in process buffer.
 */

CMD
lispevalbuffer( void )
{
	WINSCR	*owp = curwp;
	BUFFER	*bp;
	int	s;

	if( curwp->w_emode != LISPMODE ) {
		TTYbeep();
		WDGwrite( ECSTR("Not a Lisp buffer.") );
		return( NIL );
	}

	evalbp  = curbp;
	curline = firstline( evalbp );

	fillreadbuffer();

	if( (bp = bfind( BUF_PROC, T, NIL, FUNDAMENTAL )) == NULL )
		return( NIL );

	(void)setcurrentwindow( showbuffer( bp ) );
	(void)notmodified();

	if( (s = bclear( bp )) != T ) /* Blow old text away	*/
		return( s );

	/*
	 *	read and evaluate all expressions in the buffer.
	 */

	if( (s = evalloop()) != 0 ) {
		/*
		 * Print  the function name,  error message and wrong
		 * argument
		 */

		olprstr( STREAMOUT, LCSTR("** ") );
		olprstr( STREAMOUT, olerrfun );
		olprstr( STREAMOUT, LCSTR(" : ") );
		olprstr( STREAMOUT, olerrmsg[ olerrtype ] );

		if( olerrobj != NULLPTR ) {
			olprstr( STREAMOUT, LCSTR(" : ") );
			olprobj( STREAMOUT, olerrobj );
		}

		(void)olformatfreshline( STREAMOUT );
		WDGwrite( ECSTR("Evaluation error.") );
	} else	WDGwrite( ECSTR("Done.") );

	(void)notmodified();
	(void)gotobob();
	(void)setcurrentwindow( owp );

	if( s != 0 ) {
		/*
		 * an error has been detected at 'curline'
		 */
		curwp->w_dotp = curline;
		curwp->w_doto = 0;
	}

	errflag   = NIL;
	olreadbuf = (LCHAR *)NULL;
	evalbp    = (BUFFER *)NULL;

	return( T );
}
#endif
