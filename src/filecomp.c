#if	!defined( lint )
static	char rcsid[] = "$Id: filecomp.c,v 1.8 2008/06/20 09:25:12 jullien Exp $";
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
 *	Search  for  a  matching file in the current directory.  If a
 *	given pattern match a file then the file is returned.
 */

#include	"emacs.h"

static	void	_define(getdir,(EMCHAR *fname, EMCHAR *dmatch, EMCHAR *fmatch));
static	EMCHAR *_define(caseconvert,(EMCHAR *s));

EMCHAR	*
fileaccept( EMCHAR *prompt, EMCHAR *file )
{
	return( filematch( prompt, file ) );
}

#if	defined( _DIRECTORY )

#if	defined( _DOSPATH )
#define	cmpname( x, y, n )	emstrnicmp( x, y, n )
#else
#define	cmpname( x, y, n )	emstrncmp( x, y, n )
#endif

/*
 *	Try  to convert filename on systems that inherit from DOS FAT
 *	names  (mainly WINxx and OS/2).  When the name use a mixing
 *	case  or  has no extension we leave it unchanged.  Otherwise,
 *	we convert it into lower case letter only.
 */

static	EMCHAR	*
caseconvert( EMCHAR *s )
{
#if	defined( _WINDOWS_SOURCE ) || defined( _OS2 )
	EMCHAR	*p	  = s;
	int	lowercase = 0;
	int	uppercase = 0;
	int	nodot	  = 1;
	int	c;

	while( (c = (int)*p++) != 0 ) {
		if( isalpha( c ) )
			if( isupper( c ) )
				uppercase = 1;
			else	lowercase = 1;
		else	if( c == (int)'.' )
			nodot = 0;
	}

	if( (uppercase && lowercase) || nodot )
		return( s );
	else	return( emstrlwr( s ) );
#else
	return( s );
#endif
}

EMCHAR	*
filematch( EMCHAR *prompt, EMCHAR *file )
{
	static	EMCHAR newchar[] = { '?', 0 };
	static	EMCHAR pmatch[ NFILEN ]; /* pmatch is returned form filematch */

	DIR	*dirp;
	ENTRY	*dp;
	EMSTAT	stb;
	int	len;
	int	len2;
	EMCHAR	fmatch[ NFILEN ];
	EMCHAR	dmatch[ NFILEN ];
	EMCHAR	line[ NFILEN ];

loop:
	(void)getdir( file, dmatch, fmatch );
	len  = ((emstrcmp( fmatch, ECSTR(".") ) == 0) ? 0 : emstrlen( fmatch ));
	len2 = emstrlen( dmatch ) - 1;

	if( *dmatch && (dmatch[ len2 ] != '/') ) {
		dmatch[ ++len2 ] = '/';
		dmatch[ len2 ]   = '\000';
	}

	dirp = emopendir( *dmatch ? dmatch : ECSTR(".") );

	if( !dirp ) {
		WDGwrite( ECSTR("No such directory: %s"), (EMCHAR *)dmatch );
		return( NULL );
	}

	while( (dp = readdir( dirp )) != NULL ) {
		EMCHAR *name = emgetdirentry( dp );
		(void)caseconvert( name );
		(void)emstrcpy( pmatch, dmatch );
		(void)emstrcat( pmatch, name );
		if( ffstat( pmatch, &stb ) == 0 &&
		    (S_ISREG( stb.st_mode ) || S_ISDIR( stb.st_mode )) &&
		    (len == 0 || !cmpname(name,fmatch,(size_t)len)) ) {
		    	int	isdir = S_ISDIR( stb.st_mode );
			EMCHAR	*p;
			EMCHAR	*s;
                        EMCHAR  *fmt;

			if( !emstrcmp(name,ECSTR(".")) ||
			    !emstrcmp(name,ECSTR("..")) )
				continue;

			/* find extension */

			for( s = p = name ; *p ; p++ )
				if( *p == '.' )
					s = p;

			p--;

			/* ignore some extension */

			if( !emstrcmp( s, ECSTR(".o")     ) ||
			    !emstrcmp( s, ECSTR(".obj")   ) ||
			    !emstrcmp( s, ECSTR(".com")   ) ||
			    !emstrcmp( s, ECSTR(".class") ) ||
			    !emstrcmp( s, ECSTR(".exe")   ) ||
			    !emstrcmp( s, ECSTR(".dll")   ) ||
			    !emstrcmp( s, ECSTR(".lap")   ) ||
			    !emstrcmp( s, ECSTR(".a")     ) ||
			    !emstrcmp( s, ECSTR(".old")   ) ||
			    !emstrcmp( s, ECSTR(".BAK")   ) ||
			    !emstrcmp( s, ECSTR(".bak")   ) )
			    	continue;

			if( *p == '~' || *p == '#' )
				continue;

			fmt = (emunicode() ? ECSTR("%ls%ls") : ECSTR("%s%s"));

			(void)emsprintf2(
					  line,
					  fmt,
					  pmatch,
					  (isdir ? ECSTR("/") : ECSTR(""))
				        );

			WDGupdate( prompt, line );

			switch( (newchar[ 0 ] = (EMCHAR)TTYgetc()) ) {
			case 0x03: /* Ctrl-C */
				(void)closedir( dirp );
				completion = COMPLETE_AGAIN;
				*pmatch    = '\000';
				return( pmatch );
			case 0x04: /* Ctrl-D */
				(void)closedir( dirp );
				(void)newfile( updir( pmatch, NOSLASH ) );
				completion = COMPLETE_ABORT;
				return( NULL );
			case 0x07: /* Ctrl-G */
				(void)closedir( dirp );
				TTYbeep();
				WDGmessage( ECSTR("Quit") );
				completion = COMPLETE_ABORT;
				return( NULL );
			case 0x0D: /* Ctrl-M */
			case 0x0A: /* Ctrl-J */
				(void)closedir( dirp );
				return( pmatch );
			case 0x1B: /* ESC */
				(void)closedir( dirp );
				file = updir( pmatch, NOSLASH );
				len  = emstrlen( file );
				if( *file==0 || (len==2 && file[1]==':') ) {
					/*
					 * Special  case  when we are
					 * at the root of the device.
					 * Add a / and restart match.
					 */
					(void)emstrcat( file, ECSTR("/") );
					completion = COMPLETE_AGAIN;
					(void)emstrcpy( pmatch, file );
					return( pmatch );
				}
				goto loop;
			case '\t':
			case ' ':
				break;
			default :
				if( isdir ) {
					/*
					 * Inspect a new directory.
					 */
					(void)closedir( dirp );
					if( newchar[0] != 0x08 ) {
					   (void)emstrcat( pmatch, ECSTR("/") );
					   (void)emstrcat( pmatch, newchar );
					}
					completion = COMPLETE_AGAIN;
					return( pmatch );
				}
				TTYbeep();
			}
		}
	}

	(void)closedir( dirp );

	completion = COMPLETE_AGAIN;
	TTYbeep();
	return( file );
}

/*
 *	Normalize  and  then split a path fname into two components :
 *	directory 'dmatch' and filename 'fmatch'.
 */

static	void
getdir( EMCHAR *fname, EMCHAR *dmatch, EMCHAR *fmatch )
{
	EMCHAR	*r = NULL;
	int	i;
	int	j;

	fname = normalize( fname, SLASH );
 	(void)emstrcpy( dmatch, fname );
	(void)emstrcpy( fmatch, fname );

	for( i = 0, j = 0 ; *fname ; i++, fname++ )
		if( (*fname == '/') || (*fname == ':') ) {
			r = fname + 1;
			j = i;
		}

	if( r != NULL ) {
		(void)emstrcpy( fmatch, r );
		dmatch[ ++j ] = '\0';
	} else	dmatch[ 0 ]   = '\0';

}

/*
 *	This function put buffer un DIRED mode (RETURN on a line loads
 *	the file of the current line).
 */

CMD
dired( void )
{
	EMCHAR	fname[ NFILEN ];
	int	s;

	complete.fn = filematch;

	(void)emstrcpy( fname, curbp->b_fname );
	(void)ffullname( fname, ECSTR("file") );

	(void)updir( fname, SLASH );
	s = WDGedit( ECSTR("Dired (directory): "), fname, NFILEN );

	if( s == ABORT )
		return( s );

	return( newfile( fname ) );
}

CMD
diredbuffer( EMCHAR *fname )
{
	DIR	*dirp;
	ENTRY	*dp;
	BUFFER	*bp;
	EMCHAR	buf[ NFILEN ];
	EMCHAR	mark[ DIREDMARK + 1 ];
	EMCHAR	bname[ NBUFN ];
	int	nfiles = 0;
	int	rootp  = 0;
	int	s;

	fname = normalize( fname, NOSLASH );
	makename( (EMCHAR *)&bname[0], fname );

	if( (bp = bfind( bname, T, NIL, DIRED )) == NULL )
		return( NIL );

	(void)ffullname( bp->b_fname, fname );

	bp->b_flag &= ~BFCHG;			/* Don't complain!	*/
	bp->b_fmode = 0;			/* Can't modify buffer	*/

	if( (s = bclear( bp )) != T )		/* Blow old text away	*/
		return( s );

	if( fname[ emstrlen( fname ) - 1 ] == '/' )
		rootp = 1;

	if( (dirp = emopendir( fname )) == NULL ) {
		WDGwrite( ECSTR("Can't open directory: %s"), fname );
		return( NIL );
	}

	for( s = 0 ; s < DIREDMARK ; s++ )
		mark[ s ] = ' ';
	mark[ s ] = '\000';

	while( (dp = readdir( dirp )) != NULL ) {
		(void)emstrcpy( buf, mark );
		(void)emstrcat( buf, fname );
		if( rootp == 0 )
			(void)emstrcat( buf, ECSTR("/") );
		(void)emstrcat( buf, caseconvert( emgetdirentry( dp ) ) );

		if( addline( bp, buf ) == NIL ) {
			(void)closedir( dirp );
			return( NIL );
		}

		++nfiles;
	}

	(void)closedir( dirp );
	(void)connectwindow( curwp, bp );
	(void)gotobob();

	WDGwrite( ECSTR(": %d file(s) found."), nfiles );

	return( T );
}

/*
 *	Process dired commands 'f', 'd', 'x', 'R' or TAB.
 */

CMD
diredcmd( int c )
{
	EDLINE	*lp = curwp->w_dotp;
	EMCHAR	buf[ NFILEN ];
	EMCHAR	newname[ NFILEN ];
	EMCHAR	prompt[ 60 + NFILEN ];
	EMCHAR	*pfname = &buf[ DIREDMARK ];
	EMCHAR	*p;
	EMCHAR	*s;
	int	removed = 0;
	int	asked   = 0;

	if( c != 'x' && c != 0x08 )
		if( curwp->w_dotp==lastline(curbp)||llength(curwp->w_dotp)==0 )
			return( ctrlg() );

	(void)emstrncpy(buf,ltext(curwp->w_dotp),(size_t)llength(curwp->w_dotp));
	buf[ llength( curwp->w_dotp ) ] = '\000';

	switch( c ) {
	case 'd':
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

	case 'f':
		return( newfile( pfname ) );

	case 'n':
		return( forwline() );

	case 'p':
		return( backline() );

	case 'v':
		if( readin( pfname ) != T )
			return( NIL );
		if( (curbp->b_fmode & WACCESS) )
			curbp->b_fmode &= ~WACCESS;
		return( T );

	case 'x' :
		curbp->b_fmode |= WACCESS;
		(void)gotobob();
		(void)gotobol();

		do	{
			if( lgetc( curwp->w_dotp, 0 ) == 'D' ) {
				asked++;

				(void)emstrncpy(
					         buf,
					         ltext(curwp->w_dotp),
					         (size_t)llength(curwp->w_dotp)
					       );
				buf[ llength( curwp->w_dotp ) ] = '\000';

				(void)ffchmod( pfname, WMODE );

				if( removefile( pfname, T ) == T ) {
				    removed++;
				    (void)gotobol();
				    (void)ldelete(llength(curwp->w_dotp)+1,NIL);
				    (void)backline();
				}

			}
		} while( curwp->w_dotp != lastline(curbp) && forwline() == T );

		curbp->b_fmode &= ~WACCESS;
		curbp->b_flag  &= ~BFCHG;

		(void)gotobob();
		(void)gotobol();

		switch( asked ) {
		case 0 :
			curwp->w_dotp = lp;
			curwp->w_doto = 0;
			WDGwrite( ECSTR("(No deletions requested)") );
			break;
		case 1 :
			WDGwrite( ECSTR("%d deletion done"), removed );
			break;
		default:
			WDGwrite( ECSTR("%d deletions done"), removed );
		}

		return( T );

	case 'R' :
		p = s= &newname[ 0 ];
		(void)emstrcpy( newname, pfname );

		while( *s ) {
			if( *s == '\\' || *s == '/' )
				p = s;
			s++;
		}

		(void)emstrcpy( prompt, ECSTR("Rename ") );
		(void)emstrcat( prompt, p + 1 );
		(void)emstrcat( prompt, ECSTR(" to: ") );

		if( *p == '\\' || *p == '/' )
			*++p = '\000';

		complete.fn = filematch;

		if( WDGedit( prompt, newname, NFILEN ) != T )
			return( NIL );
		if( ffrename( pfname, newname ) != FIOSUC ) {
			WDGwrite( ECSTR("Could not rename to %s"), newname );
			return( NIL );
		}
		WDGwrite( ECSTR("Move: 1 file") );

		(void)gotobob();
		(void)gotobol();

		(void)emstrncpy(buf,
				ltext(curwp->w_dotp),
				(size_t)llength(curwp->w_dotp));
		buf[ llength( curwp->w_dotp ) ] = '\000';

		return( diredbuffer( pfname ) );

	case 0x08 : /* ^H */
		if(backline()==T&&gotobol()==T&&lgetc(curwp->w_dotp,0)=='D') {
			curbp->b_fmode |= WACCESS;
			lputc( curwp->w_dotp, 0, ' ' );
			lchange( WFEDIT );
			curbp->b_fmode &= ~WACCESS;
			curbp->b_flag  &= ~BFCHG;
		}
		return( T );

	default :
		return( ctrlg() );
	}
}

#else

EMCHAR	*
filematch( prompt, file )
EMCHAR	*prompt;
EMCHAR	*file;
{
	WDGerror( ECSTR("No file match in this version.") );
	return( (EMCHAR *)NULL );
}

CMD
diredbuffer( fname )
EMCHAR	*fname;
{
	WDGerror( ECSTR("No DIRED in this version.") );
	return( NIL );
}

CMD
dired(void)
{
	return( ctrlg() );
}

CMD
diredcmd( c )
int	c;
{
	return( ctrlg() );
}

#endif
