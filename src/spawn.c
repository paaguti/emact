#if	!defined( lint )
static	char rcsid[] = "$Id: spawn.c,v 1.10 2008/06/19 12:13:32 jullien Exp $";
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
 *	The  routines  in  this  file  are  called to create a subjob
 *	running a command interpreter.
 */

#include	"emacs.h"

#if	defined( _UNIX )
#define	SEDPROG		ECSTR("sed")
#define	GREPPROC	ECSTR("grep -n ")
#else
#define	SEDPROG		ECSTR("emsed")
#define	GREPPROC	ECSTR("emgrep -n ")
#endif

#if	defined( _WIDECHARS )
#define	SHELLFMT1	ECSTR("%ls %ls %ls > %ls")
#define	SHELLFMT2	ECSTR("%ls %ls \"%ls\" %ls > %ls")
#define	SEDFMT1		ECSTR("%ls %ls %ls > %ls")
#define	SEDFMT2		ECSTR("%ls -e \"%ls\" %ls > %ls")
#define	PERLFMT1	ECSTR("%ls %ls %ls")
#else
#define	SHELLFMT1	ECSTR("%s %s %s > %s")
#define	SHELLFMT2	ECSTR("%s %s \"%s\" %s > %s")
#define	SEDFMT1		ECSTR("%s %s %s > %s")
#define	SEDFMT2		ECSTR("%s -e \"%s\" %s > %s")
#define	PERLFMT1	ECSTR("%s %s %s")
#endif

static	CMD	_define(javaevalbuffer,(void));

/*
 *	Send  a  system  command.  Output is redirected to a filename
 *	and then loaded in the 'Process' window.
 */

#if	defined( _WINDOWS_SOURCE )
int
syscompile( EMCHAR *cmd, int flag )
{
	WINSCR	*owp = curwp;
	WINSCR	*wp;
	BUFFER	*bp;
	int	s;

	if( (bp = bfind( BUF_PROC, T, NIL, FUNDAMENTAL )) == NULL )
		return( NIL );

	if( (s = bclear( bp )) != T ) /* Blow old text away	*/
		return( s );
	else	wp = showbuffer( bp );

	(void)setcurrentwindow( wp );

	if( ffsystem( cmd ) != 0 ) {
		TTYbeep();
		WDGwrite( ECSTR("'%s' command fails !"), cmd );
		(void)setcurrentwindow( owp );
		return( NIL );
	}

	(void)notmodified();
	(void)gotobob();
	(void)setcurrentwindow( owp );

	WDGwrite( ECSTR("Done.") );
	errflag	= NIL;
	if( flag == SYSCOMP_ERRORS )
		(void)nexterror();
	return( T );
}

#else

static	EMCHAR	*procname = ECSTR("process.tmp");

int
syscompile( EMCHAR *cmd, int flag )
{
#if	defined( _WIN32_WCE )
	return( NIL );
#else
	WINSCR	*owp	= curwp;
	WINSCR	*wp;
	BUFFER	*bp;
	FILE	*fd;
	int	status	= NIL;
	int	out	= -1;
	int	s;
	int	err;
	int	tmp1;

	if( (bp = bfind( BUF_PROC, T, NIL, FUNDAMENTAL )) == NULL )
		return( NIL );

	if( (s = bclear( bp )) != T )  /* Blow old text away	*/
		return( s );
	else	wp = showbuffer( bp );

	(void)setcurrentwindow( wp );

#if	defined( _SPAWNED_PIPE )

	if( pipe_process == T ) {
		EMCHAR	line[ NLINE ];
		int	c;

		(void)emstrcpy( line, cmd );
		(void)emstrcat( line, ECSTR(" 2>&1") );

		if( (fd = empopen( &line[0], ECSTR("r") )) == NULL )
			return( NIL );

		while( (c = fgetc( fd )) != EOF ) {
			if( TTYcheck() == T ) {
				TTYbeep();
				WDGmessage( ECSTR("Quit") );
				break;
			}
			switch( c ) {
			case '\b' :
				(void)backdel();
				break;
			case '\r' :
				update( T );
				break;
			case '\n':
				(void)newline();
				update( T );
				break;
			default:
				(void)linsert( 1, c );
			}
		}

		TTYrawmode();
		status = ((pclose( fd ) == 0) ? T : NIL);
	} else	{
#endif
		if( compile_in_buffer == T ) {
			if( (fd = ffopen(procname,ECSTR("w"),NULL)) == NULL )
				return( NIL );

			tmp1 = fileno( fd );

			if( (err  = dup( fileno( stderr ) )) != -1 &&
			    (out  = dup( fileno( stdout ) )) != -1 &&
			    dup2( tmp1, fileno( stderr ) )   != -1 &&
			    dup2( tmp1, fileno( stdout ) )   != -1 ) {
				TTYrawmode(); /* open the duplicate out ^C */
			    	status = ((ffsystem( cmd )==0) ? T : NIL);
				(void)dup2( out, fileno( stdout ) );
				(void)dup2( err, fileno( stderr ) );
				TTYrawmode(); /* close the duplicate out */
				(void)redrawscreen();
				update( T );
			    }

			if( tmp1 != -1 )
				(void)close( tmp1 );
			if( out != -1 )
				(void)close( out );
			if( out != -1 )
				(void)close( err );

			if( (s = bclear( bp )) != T )
				return( s );
			else	(void)showbuffer( bp );

			(void)readin( procname );
			(void)unlink( (char *)procname );
		} else	{
			vttidy();
		    	status = ((ffsystem( cmd ) == 0) ? T : NIL);
			TTYopen();
			WDGwrite( ECSTR("Strike any key to continue .. ") );
			TTYgetc();
			sgarbf = T;
			return( status );
		}
#if	defined( _SPAWNED_PIPE )
	}
#endif

	(void)notmodified();
	(void)gotobob();
	(void)setcurrentwindow( owp );

	WDGwrite( ECSTR("Done.") );
	errflag	= NIL;
	if( flag == SYSCOMP_ERRORS )
		(void)nexterror();
	return( status );
#endif	/* _WIN32_WCE */
}
#endif

/*
 *	Create  a  subjob  with  a copy of the command intrepreter in
 *	it.  Mark  the  screen as garbage for full repaint.  Bound to
 *	"C-C".
 */

CMD
spawncli( void )
{
#if	defined( _DOSPATH )
	(void)ffsystem( ffgetenv( ECSTR("ComSpec") ) );
#else
	vttidy();

	if( emstrcmp( ffgetenv( ECSTR("TERM") ), ECSTR("xterm") ) == 0 )
		(void)ffsystem( ECSTR("xterm") );
	else	{
		EMCHAR	*cp = ffgetenv( ECSTR("SHELL") );
		(void)ffsystem( (cp!=NULL) ? cp : ECSTR("sh") );
	}

	sgarbf = T;
	TTYopen();
#endif
	return( T );
}

/*
 *	Run a one-liner in a subjob.  When the command returns,  wait
 *	for  a single character to be typed,  then mark the screen as
 *	garbage so a full repaint is done. Bound to "C-X !".
 */

CMD
spawn( void )
{
	int	s;
	EMCHAR	line[ NLINE ];

	if( (s = mlreply( ECSTR("System command: "), line, NLINE )) != T )
		return( s );

#if	defined( _WINDOWS_SOURCE )
	(void)ffsystem( line );
#else
	vttidy();

	(void)ffsystem( line );

	(void)fputs( "Strike any key to continue .. ", stdout );
	(void)fgetc( stdin );

	TTYopen();
	mlerase();
	sgarbf = T;
#endif
	return( T );
}

/*
 *	Execute current makefile.
 */

CMD
makefile( void )
{
	EMCHAR	buf[ NFILEN ];
	int	s;

	(void)emstrcpy( buf, make_name );

	if( *make_arg ) {
		(void)emstrcat( buf, ECSTR(" ") );
		(void)emstrcat( buf, make_arg );
	}

 	if( (s = savesomebuffers()) == ABORT )
		return( s );

	WDGwrite( ECSTR("%s .."), buf );
	return( syscompile( buf, SYSCOMP_ERRORS ) );
}

/*
 *	Execute man with word at cursor
 */

CMD
man( void )
{
#if	defined( _UNIX ) || defined( _WINDOWS_SOURCE )
	EMCHAR	buf[ NPAT ];
	EMCHAR	cmd[ NLINE ];

	if( wordatcursor( buf, NPAT ) != T )
		buf[ 0 ] = '\000';

	if( WDGedit( ECSTR("man : "), buf, NLINE ) == ABORT )
		return( NIL );

	(void)emstrcpy( cmd, ECSTR("man ") );
	(void)emstrcat( cmd, buf );

	return( syscompile( cmd, SYSCOMP_NOERROR ) );
#else
	WDGwrite( ECSTR("No man on this system") );
	return( NIL );
#endif
}

/*
 *	Use  external  command  from  emacs.  First,  change  to  the
 *	directory  of  file  in  the  current  buffer,  then  run the
 *	command  in  this  directory  and  go  back  to  the original
 *	location (the current directory).
 */

static CMD	_define(externalcommand,(EMCHAR* cmdname, EMCHAR* cmdline));

static CMD
externalcommand( EMCHAR *cmdname, EMCHAR *cmdline )
{
	EMCHAR	buf[ NLINE ];
	EMCHAR	prompt[ NLINE ];
	EMCHAR	info[ NLINE ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	int	s;

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir( gdir, NOSLASH );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	(void)emstrcpy( info, ECSTR("Run ") );
	(void)emstrcat( info, cmdname );
	(void)emstrcat( info, ECSTR(" (with arg): ") );

	(void)emstrcpy( buf, cmdline );

	prompt[ 0 ] = '\000';

	if( WDGedit( info, prompt, NLINE ) == ABORT ) {
		(void)ffchdir( cdir );
		return( NIL );
	}

	(void)emstrcat( buf, prompt );

	WDGwrite( ECSTR("%s .."), buf );
	s = syscompile( buf, SYSCOMP_NOERROR );
	(void)ffchdir( cdir );

	return( s );

}

/*
 *	Use grep from emacs. This command is not bound.
 */

CMD
grep( void )
{
	return( externalcommand( ECSTR("grep"), GREPPROC ) );
}

/*
 *	Use shell command processor from emacs.  First, change to the
 *	directory  of file in the current buffer,  then run the shell
 *	in  this  directory  with  the  user  command and replace the
 *	current buffer by the result of execution.  The we go back to
 *	the  original location (the current directory).  This command
 *	is not bound.
 */

#define	SHELLTEMP	ECSTR("em-shell.tmp")
#define	SHELLRESULT	ECSTR("em-shell.res")

CMD	_define(shellbuffer,(EMCHAR* prog, EMCHAR* defopt));

CMD
shellbuffer( EMCHAR *prog, EMCHAR *defopt )
{
	EMCHAR	buf[ NLINE ];
	EMCHAR	info[ NLINE ];
	EMCHAR	prompt[ NLINE ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	EMCHAR	oldfname[ NFILEN ];
	int	option= 0;
	int	s;
	int	i;

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir(  gdir, NOSLASH );

	(void)emstrcpy( &oldfname[ 0 ], &curbp->b_fname[ 0 ] );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );

	writeout( (EMCHAR *)SHELLTEMP );

	(void)emstrcpy( info, ECSTR("Run ") );
	(void)emstrcat( info, prog );
	(void)emstrcat( info, ECSTR(" (with arg): ") );

	prompt[ 0 ] = '\000';

	if( WDGedit( info, prompt, NLINE ) == ABORT ) {
		(void)ffchdir( cdir );
		return( NIL );
	}

	for( i = 0 ; prompt[ i ] ; i++ ) {
		if( prompt[ i ] == ' ' || prompt[ i ] == '\t' )
			continue;
		if( prompt[ i ] == '-' )
			option = 1;
		else	option = 0;
		break;
	}

	if( option )
	     (void)emsprintf4( buf, SHELLFMT1,
			       prog, prompt, SHELLTEMP, SHELLRESULT );
	else (void)emsprintf5( buf, SHELLFMT2,
			       prog, defopt, prompt, SHELLTEMP, SHELLRESULT );

	if( (s = ((ffsystem( buf ) == 0) ? T : NIL )) != 0 ) {
		(void)readin( (EMCHAR *)SHELLRESULT );
		(void)emstrcpy( &curbp->b_fname[ 0 ], &oldfname[ 0 ] );
		lchange( WFEDIT );
	}

	TTYrawmode();

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );
	if( ffaccess( (EMCHAR *)SHELLRESULT ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLRESULT );

	(void)ffchdir( cdir );

	return( s );
}

/*
 *	Use  sed from emacs.  First,  change to the directory of file
 *	in  the  current buffer,  then run sed in this directory with
 *	the  user  command  and  replace  the  current  buffer by the
 *	result  of  sed  execution.  The  we  go back to the original
 *	location (the current directory). This command is not bound.
 */

CMD
sed( void )
{
	EMCHAR	buf[ NLINE ];
	EMCHAR	prompt[ NLINE ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	EMCHAR	oldfname[ NFILEN ];
	int	option= 0;
	int	s;
	int	i;

	if( curbp->b_wide ) {
		WDGwrite( ECSTR("Can't use sed on an UNICODE buffer.") );
		return( NIL );
	}

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname );
	(void)updir( gdir, NOSLASH );

	(void)emstrcpy( &oldfname[ 0 ], &curbp->b_fname[ 0 ] );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );

	writeout( (EMCHAR *)SHELLTEMP );

	prompt[ 0 ] = '\000';

	if( WDGedit( ECSTR("Run sed (with arg): "), prompt, NLINE ) == ABORT ) {
		(void)ffchdir( cdir );
		return( NIL );
	}

	for( i = 0 ; prompt[ i ] ; i++ ) {
		if( prompt[ i ] == ' ' || prompt[ i ] == '\t' )
			continue;
		if( prompt[ i ] == '-' )
			option = 1;
		else	option = 0;
		break;
	}

	if( option )
	     (void)emsprintf4( buf, SEDFMT1,
			       SEDPROG, prompt, SHELLTEMP, SHELLRESULT );
	else (void)emsprintf4( buf, SEDFMT2,
			       SEDPROG, prompt, SHELLTEMP, SHELLRESULT );

	if( (s = ((ffsystem( buf ) == 0) ? T : NIL )) != 0 ) {
		(void)readin( SHELLRESULT );
		(void)emstrcpy( &curbp->b_fname[ 0 ], &oldfname[ 0 ] );
		lchange( WFEDIT );
	}

	TTYrawmode();

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );
	if( ffaccess( (EMCHAR*)SHELLRESULT ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLRESULT );

	(void)ffchdir( cdir );

	return( s );
}

/*
 *	Use perl from emacs.  First,  change to the directory of file
 *	in  the current buffer,  then run perl in this directory with
 *	the  user  command  and  replace  the  current  buffer by the
 *	result  of  perl  execution.  The  we go back to the original
 *	location (the current directory). This command is not bound.
 */

#define	PERLPROG	ECSTR("perl")

CMD
perl( void )
{
	EMCHAR	buf[ NLINE ];
	EMCHAR	prompt[ NLINE ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	EMCHAR	oldfname[ NFILEN ];
	int	s;

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir( gdir, NOSLASH );

	(void)emstrcpy( &oldfname[ 0 ], &curbp->b_fname[ 0 ] );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );

	writeout( (EMCHAR *)SHELLTEMP );

	prompt[ 0 ] = '\000';

	if( WDGedit( ECSTR("Run perl (with arg): "), prompt, NLINE ) == ABORT ) {
		(void)ffchdir( cdir );
		return( NIL );
	}

	(void)emsprintf3( buf, PERLFMT1, PERLPROG, prompt, SHELLTEMP );

	if( (s = ((ffsystem( buf ) == 0) ? T : NIL )) != 0 ) {
		(void)readin( (EMCHAR *)SHELLTEMP );
		(void)emstrcpy( &curbp->b_fname[ 0 ], &oldfname[ 0 ] );
		lchange( WFEDIT );
	}

	TTYrawmode();

	if( ffaccess( (EMCHAR *)SHELLTEMP ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLTEMP );
	if( ffaccess( (EMCHAR *)SHELLRESULT ) == FIOSUC )
		(void)ffremove( (EMCHAR *)SHELLRESULT );

	(void)ffchdir( cdir );

	return( s );
}

/*
 *	Compile, interactively call make_name
 */

CMD
compile( void )
{
	static	EMCHAR	buf[ NFILEN ] = { '\000' };

	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	int	s;

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir( gdir, NOSLASH );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	if( buf[ 0 ] == '\000' ) {

		(void)emstrcpy( buf, make_name );

		if( *make_arg ) {
			(void)emstrcat( buf, ECSTR(" ") );
			(void)emstrcat( buf, make_arg );
			(void)emstrcat( buf, ECSTR(" ") );
		}
	}

	(void)mlallowcomplete( T );
	s = WDGedit( ECSTR("Compile command: "), buf, NFILEN );
	(void)mlallowcomplete( NIL );

	if( s == ABORT )
		return( NIL );

 	if( (s = savesomebuffers()) == ABORT )
		return( s );

	s = syscompile( buf, SYSCOMP_ERRORS );
	(void)ffchdir( cdir );

	return( s );

}

/*
 *	Compile  current filename using the associated file compiler.
 *	Bound to C-XC.
 */

CMD
compilecurrent( void )
{
	switch( curwp->w_emode ) {
	case CMODE:
	case CPPMODE:
		return( ccompile() );
	case JAVAMODE:
		return( javacompile() );
	default:
		TTYbeep();
		WDGwrite( ECSTR("Don't know how to compile %s"),curbp->b_fname);
	}

	return( NIL );
}

/*
 *	Compile current filename if it's a C file.
 */

CMD
ccompile( void )
{
	EMCHAR	buf[ NFILEN ];

	(void)emstrcpy( buf, cc_name );
	if( (curwp->w_emode == CMODE) || (curwp->w_emode == CPPMODE) ) {
		(void)emstrcat( buf, ECSTR(" ") );
		(void)emstrcat( buf, cc_arg );
		(void)emstrcat( buf, ECSTR(" ") );
		(void)emstrcat( buf, curbp->b_fname );
		WDGwrite( ECSTR("%s"), buf );
		return( syscompile( buf, SYSCOMP_ERRORS ) );
	} else	return( NIL );
}

/*
 *	Compile current filename if it's a Java file.
 */

CMD
javacompile( void )
{
	EMCHAR	buf[ NFILEN ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	EMCHAR	*base;
	EMCHAR	*fname;
	int	s;

	if( curwp->w_emode != JAVAMODE ) {
		TTYbeep();
		WDGwrite( ECSTR("not a java file !") );
		return( NIL );
	}

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir( gdir, NOSLASH );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	base = curbp->b_fname;

	for( fname = curbp->b_fname ; *fname ; fname++ )
		if( *fname == '/' || *fname == '\\' )
			base = fname + 1;

	(void)emstrcpy( buf, java_comp_name );
	(void)emstrcat( buf, ECSTR(" ")     );
	if( java_comp_args[ 0 ] ) {
		(void)emstrcat( buf, java_comp_args );
		(void)emstrcat( buf, ECSTR(" ")     );
	}
	(void)emstrcat( buf, base );

 	if( (s = savesomebuffers()) == ABORT )
		return( s );

	WDGwrite( ECSTR("%s"), buf );

	s = syscompile( buf, SYSCOMP_ERRORS );
	(void)ffchdir( cdir );

	return( s );
}

/*
 *	Assemble current filename if it's a assembler file.
 */

CMD
assemble( void )
{
	EMCHAR	buf[ NFILEN ];

	if( curwp->w_emode == ASMODE ) {
		(void)emstrcpy( buf, as_name );
		(void)emstrcat( buf, ECSTR(" ") );
		(void)emstrcat( buf, as_arg );
		(void)emstrcat( buf, ECSTR(" ") );
		(void)emstrcat( buf, curbp->b_fname );
		(void)emstrcat( buf, ECSTR(";") );
		WDGwrite( ECSTR("%s"), buf );
		return( syscompile( buf, SYSCOMP_ERRORS ) );
	}
	return( NIL );
}

/*
 *	Evaluate current filename if it's a Java file.
 */

static CMD
javaevalbuffer( void )
{
	EMCHAR	buf[ NFILEN ];
	EMCHAR	cdir[ NLINE ];
	EMCHAR	gdir[ NLINE ];
	EMCHAR	*fname;
	EMCHAR	*p;
	EMCHAR	*d = (EMCHAR *)NULL;
	int	s;

	if( curwp->w_emode != JAVAMODE ) {
		TTYbeep();
		WDGwrite( ECSTR("not a java file !") );
		return( NIL );
	}

	(void)ffgetcwd( cdir, NLINE-1 );
	(void)emstrcpy( gdir, curbp->b_fname  );
	(void)updir( gdir, NOSLASH );

	if( gdir[ 0 ] )
		(void)ffchdir( gdir );

	p = curbp->b_fname;

	for( fname = curbp->b_fname ; *fname ; fname++ )
		if( *fname == '/' || *fname == '\\' )
			p = fname + 1;

	(void)emstrcpy( buf, java_exec_name );
	(void)emstrcat( buf, ECSTR(" ")     );
	if( java_exec_args[ 0 ] ) {
		(void)emstrcat( buf, java_exec_args );
		(void)emstrcat( buf, ECSTR(" ")     );
	}
	(void)emstrcat( buf, p );

	/*
	 *	Remove extension.
	 */

	for( p = buf ; *p ; p++ )
		if( *p == '.' )
			d = p;

	if( d != buf )
		*d = '\000';

	/*
	 *	Compile the file first, then execute
	 */

	s = javacompile();

	if( s == T ) {
		WDGwrite( ECSTR("%s"), buf );

		s = syscompile( buf, SYSCOMP_ERRORS );
		(void)ffchdir( cdir );
	}

	return( s );
}

/*
 *	Evaluate the content of current buffer depending on mode.
 */

CMD
evalbuf( void )
{
	switch( curwp->w_emode ) {
	case LISPMODE :
		return( lispevalbuffer() );
	case JAVAMODE :
		return( javaevalbuffer() );
	default       :
		TTYbeep();
		WDGwrite( ECSTR("Cannot evaluate this buffer.") );
	}

	return( NIL );
}

/*
 *	Get system command in process buffer.
 */

CMD
getcommand( void )
{
	int	s;
	EMCHAR	line[ NLINE ];

	if( ( s=mlreply( ECSTR(": get-system-command: "), line, NLINE )) != T )
		return( s );
	else	return( syscompile( line, SYSCOMP_NOERROR ) );

}

/*
 *	Change  the  working  directory  (usefull  on  UN*X operating
 *	system)
 */

CMD
changedir( void )
{
	int	s;
	EMCHAR	line[ NLINE ];

	if( (s=mlreply(ECSTR("Change default directory: "), line, NLINE)) != T )
		return( s );

	if( ffchdir( line ) == 0 )
		return( T );
	else	{
		WDGerror( ECSTR("Directory not found or access denied.") );
		return( NIL );
	}
}
