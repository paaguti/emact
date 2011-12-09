#if     !defined( lint )
static	char rcsid[] = "$Id: file.c,v 1.11 2009/05/01 07:36:11 jullien Exp $";
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
 *      The  routines  in this file handle the reading and writing of
 *      files. Details of the I/O are in fileio.c
 */

#include        "emacs.h"

static  void    _define(savetime,(void));
static	EMCHAR *_define(getbufdir,(void));

/*
 *      Test  the  file  for readonly flag.  Returns T if the file is
 *      marked as readonly, NIL otherwise.
 */

int
freadonly( void )
{
	if( frdflag == T )
		return( NIL );


	frdflag = T;

	if( (curbp->b_fmode & WACCESS) == 0 ) {
		TTYbeep();
		WDGmessage( ECSTR("Buffer is readonly.") );
		return( T );
	}

	return( NIL );
}

/*
 *	Return the absolute path for the current buffer.  The current
 *	directory  is  taken from file associated with current buffer
 *	or  the  actual  current  working  directory  if  it does not
 *	exist.  The  return  vaule is a pointer the start of a static
 *	local buffer.
 */

static	EMCHAR *
getbufdir( void )
{
	static	EMCHAR	curd[ NFILEN ];
	EMCHAR		lastch;

	if( curbp->b_fname[ 0 ] == '\000' ) {
		(void)ffullname( curd, ECSTR(".") );
		lastch = curd[ emstrlen( curd ) - 1 ];
		if( !(lastch == '\\' || lastch == '/') )
			(void)emstrcat( curd, ECSTR("/") );
	} else	{
		(void)emstrcpy( curd, curbp->b_fname );
		(void)updir( curd, SLASH );
	}

	return( (EMCHAR *)&curd[ 0 ] );
}

/*
 *      Look around to see if  you  can  find  the file  in  another
 *      buffer; if you can find it just switch to the buffer. If you
 *      cannot find the file, create a new buffer, read in the text,
 *      and switch to the new buffer.
 */

int
newfile( EMCHAR *filename )
{
	BUFFER  *bp;
	EDLINE  *lp;
	EMCHAR  bname[ NBUFN ];
	EMCHAR	fname[ NFILEN ];
	EMCHAR	buf[ NLINE ];
	EMCHAR	*discard = ECSTR("Discard changes made to buffer ");
	int     i;

	(void)ffullname( fname, filename );

	if( ffilevalid( fname ) != T )
		return( NIL );

	for( bp = bheadp ; bp != NULL ; bp = bp->b_bufp ) {
		if( emstrcmp( bp->b_fname, fname ) == 0 ) {
			(void)connectwindow( curwp, bp );
			lp      = curwp->w_dotp;
			i       = curwp->w_ntrows/2;
			while( i-- && lback( lp ) != lastline( curbp ) )
				lp = lback( lp );
			curwp->w_linep  = lp;

			(void)emstrcpy( buf, discard      );
			(void)emstrcat( buf, bp->b_bname  );
			(void)emstrcat( buf, ECSTR(" ? ") );

			if( (bp->b_flag & BFCHG) && (WDGyn( buf ) == T) ) {
				bp->b_flag &= ~BFCHG;	/* Don't complain! */
				return( revertbuffer() );
			}
			curwp->w_flag  |= WFMODE|WFHARD;
			WDGmessage( ECSTR("Old buffer") );
			return( T );
		}
	}

	if( ffdiredp( fname ) )
		return( diredbuffer( fname ) );

	makename( bname, fname );

	if( (bp = bfind( bname, T, 0, 0 )) == NULL ) {
		WDGerror( ECSTR("Cannot create buffer") );
		return( NIL );
	}

	(void)connectwindow( curwp, bp );

	return( readin( fname ) );
}

/*
 *      Read  file "fname" into the current buffer,  blowing away any
 *      text found there. Called by both the read and visit commands.
 *      Return  the  final status of the read.  Also  called  by  the
 *      mainline, to  read in a file specified on the command line as
 *      an argument.
 */

int
readin( EMCHAR *fname )
{
	EDLINE  *lp1;
	EDLINE  *lp2;
	WINSCR  *wp;
	int     i;
	int     f;
	int	widep;
	int     nline;
	long    nchar;
	int     nbytes;
	int	binmode;
	EMCHAR  line[MAXLINE];

	if( ffdiredp( fname ) )
		return( diredbuffer( fname ) );

	if( ffsetaccess( fname ) != T ) {
		WDGwrite( ECSTR("Can't access file %s"), fname );
		return( NIL );;
	}

	if( (f = bclear( curbp )) != T )
		return( f );

	curbp->b_flag &= ~BFCHG;

	(void)ffullname( curbp->b_fname, fname );

	widep = 0;
	switch( f = ffropen( fname, &binmode, &widep ) ) {
	case FIOFNF :   /* File not found.      */
		curbp->b_fmode = WMODE;
		curbp->b_time  = 0;
		WDGwrite( ECSTR("(New file %s)"), fname );
		break;
	default     :
		WDGmessage( ECSTR("Reading file ...") );
		nline = 0;
		nchar = 0L;

		while((f=ffgetline(line,MAXLINE,&nbytes))==FIOSUC||f==FIOWNL) {
			if( (lp1 = lalloc( nbytes )) == NULL ) {
				f = FIOERR;     /* Keep message on the  */
				break;          /* display.             */
			}
			lp2                        = lback( lastline( curbp ) );
			lforw( lp2 )               = lp1;
			lforw( lp1 )               = lastline( curbp );
			lback( lp1 )               = lp2;
			lback( lastline( curbp ) ) = lp1;

			for( i = 0 ; i < nbytes ; ++i )
				lputc( lp1, i, line[i] );

			nline += 1;
			nchar += nbytes + (binmode ? 1 : CRSIZE);
			if( f == FIOWNL )
				break;
		}
		(void)ffclose();
		if( f == FIOERR )
			return( NIL );
		if( f == FIOEOF ) {
		    /* Don't zap message!   */
		    WDGwrite(ECSTR("Read %d line(s), %ld byte(s)"),
			     nline,
			     (size_t)nchar * sizeof( EMCHAR ));
		}

#if	defined( _ADD_TO_RECENT_DOCS )
		NTaddrecent( curbp->b_fname );
#endif
		curbp->b_binary = binmode;
		curbp->b_wide   = widep;
	}

	for( wp = wheadp ; wp != NULL ; wp=wp->w_wndp ) {
		if( wp->w_bufp == curbp ) {
			curbp->b_emode = getautomode( fname );
			switch( curbp->b_emode ) {
			case LISPMODE:
			case JAVAMODE:
			case CMODE:
			case CPPMODE:
			case SHELLMODE:
				curbp->b_binary = T;
				break;
			}
			wp->w_linep    = firstline( curbp );
			wp->w_dotp     = firstline( curbp );
			wp->w_doto     = 0;
			wp->w_markp    = NULL;
			wp->w_marko    = 0;
			wp->w_flag    |= WFMODE|WFHARD;
			wp->w_emode    = curbp->b_emode;
			wp->w_binary   = curbp->b_binary;
			wp->w_wide     = curbp->b_wide;
			updatemodes();
		}
	}

	return( T );

}

/*
 *      Normalize a pathname. When 'flag' is T, removes the trailing
 *      '/..' and '/.'. On MS-DOS, standardize pathnames with '/'.
 *
 *      struct  {
 *              EMCHAR    *before;
 *              EMCHAR    *after;
 *      } test[] = {
 *              { "/foo/bar/../gee",            "/foo/gee"      },
 *              { "/foo/bar/../../gee",         "/gee"          },
 *              { "/foo/bar/../../../gee",      "gee"           },
 *              { "/foo/bar/.././gee",          "/foo/gee"      },
 *              { "/foo/bar/./../gee",          "/foo/gee"      },
 *              { "../gee",                     "../gee"        },
 *              { "../../../gee",               "../../../gee"  },
 *              { "gee/..",                     "."             },
 *              { "gee/../foo/..",              "."             },
 *              { "gee/../..",                  ".."            },
 *              { "/bar/gee/..",                "/bar"          },
 *              { "/foo/bar/./gee",             "/foo/bar/gee"  },
 *              { "/foo/bar/.",                 "/foo/bar"      },
 *              { "./foo/bar/./gee",            "foo/bar/gee"   },
 *              { "./foo/bar/../gee",           "foo/gee"       },
 *              { "foo/bar/./gee",              "foo/bar/gee"   },
 *              { "foo/bar/../gee",             "foo/gee"       },
 *              { "../..",                      "../.."         },
 *              { "/",                          "/"             },
 *              { NULL,                         NULL            }
 *      };
 *
 */

EMCHAR    *
normalize( EMCHAR *fname, int flag )
{
	EMCHAR    *lastdir = fname;
	EMCHAR    *res     = fname;
	EMCHAR    *s;
	EMCHAR    *p;
	EMCHAR    c;

	/*
	 *      convert \\ to /
	 */

	for( p = fname ; *p ; p++ )
		if( *p == '\\' )
			*p = '/';

	/*
	 *      skip initial './'
	 */

	while( !emstrncmp( fname, ECSTR("./"), 2) ) {
		p = fname+2;
		s = fname;
		while( *p )
			*s++ = *p++;
		*s = '\000';
	}

	while( *fname )
		if( *fname == '/' ) {
			if( !emstrncmp( fname, ECSTR("/.."), 3 ) ) {
				s = lastdir;
				p = fname+3;
				if( *lastdir != '.' && (flag || *(fname+3)) ) {
					while( *p )
						*s++ = *++p;
					*s = '\000';
					return( normalize(res, flag) );
				}
			} else  if( !emstrncmp( fname, ECSTR("/."), 2) ) {
				c = fname[ 2 ];
				if( c != '\000' && c != '/' )
					lastdir = fname+2;
				else    {
					s = fname;
					p = fname+2;
					if( flag || *(fname+2) ) {
						while( *p )
							*s++ = *p++;
						*s = '\000';
						return( normalize(res, flag) );
					}
				}
			} else  lastdir = fname+1;

			if( *(fname+1) )
				fname++;
			else    {
				if( flag && (fname != res) )
					/*
					 * Removes trailing '/'
					 */
					*fname = '\000';
				return( res );
			}
			
		} else  fname++;

	if( *res == 0 )
		(void)emstrcpy( res, ECSTR(".") );

	return( res );
}

/*
 *      Returns  the  parent  directory  for  the  given  path.  When
 *      'slashflag' is NOSLASH, removes the trailing '/'.
 */

EMCHAR    *
updir( EMCHAR *fname, int slashflag )
{
	EMCHAR    *p = NULL;
	EMCHAR    *s;

	fname = normalize( fname, NOSLASH );

	for( s = fname ; *s ; s++ )
		if( *s == ':' )
			p = s + 1;
		else	if( (*s == '/') && *(s+1) ) {
				if( slashflag == NOSLASH )
					p = s;
				else    p = s + 1;
			}

	if( p )
		*p      = '\000';
	else    *fname  = '\000';

	return( fname );
}

/*
 *      Take a file name,  and from it fabricate a buffer name.  This
 *      routine  knows  about the syntax of file names on the  target
 *      system.  I  suppose that this information could be put in   a
 *      better place than a line of code.
 */

void
makename( EMCHAR *bname, EMCHAR *fname )
{
	EMCHAR    *cp1;
	EMCHAR    *cp2;
	EMCHAR    i = '2';

	/*
	 *	Point to the last char of pathname.
	 */

	cp1 = fname + emstrlen( fname ) - 1;

	/*
	 *	Find the file name component.
	 */

#if     defined( _DOSPATH )
	while( cp1!=fname && cp1[-1]!=':' && cp1[-1]!='\\' && cp1[-1]!='/' )
		--cp1;
#else
	while( cp1 != fname && cp1[-1] != '/' )
		--cp1;
#endif

	/*
	 *	Copy the file name into buffer name.
	 */

	cp2 = bname;
	while( cp2 != (bname+ NBUFN - 1) && *cp1 )
		*cp2++ = *cp1++;
	*cp2 = 0;

	/*
	 *      Check if buffer name already exists and, in this case,
	 *      add version number such as name<2>.
	 */

	while( bfind( bname, NIL, 0, 0 ) != NULL ) {
		/*
		 *	Buffer  name  already exists,  append <X> and
		 *	try again
		 */
		while( cp2 >= (bname + NBUFN - 4) )
			/*
			 * find the end of bname minus 4 chars for <?>
			 */
			cp2--;
		if( i > '9' )
			i = '@';
		*cp2       = '<';
		*(cp2 + 1) = i++;
		*(cp2 + 2) = '>';
		*(cp2 + 3) = '\000';
	}
}

/*
 *      This function performs the details of file writing.  Uses the
 *      file  management  routines in  the  "fileio.c"  package.  The
 *      number of lines written is displayed.  Sadly, it looks inside
 *      a LINE;  provide a macro for this. Most of the grief is error
 *      checking of some sort.
 *
 */

int
writeout( EMCHAR *fname )
{
	EDLINE  *lp;
	FILE    *fd;
	int     s;
	int     nline;
	long    nchar;
	EMCHAR  bak[ NFILEN ];
	EMCHAR	*p;

	if( freadonly() == T )
		return( NIL );

	if( ffchanged( fname ) == T )
		return( NIL );

	savetime();

	/*
	 *	Replace the last extension with .BAK
	 */

	p = (EMCHAR *)NULL;

	(void)emstrcpy( &bak[0], &fname[0] );

	for( s = 0 ; bak[ s ] ; s++ ) {
		EMCHAR c = bak[ s ];

		if( c == '/' || c == '\\' ) {
			/*
			 * Path separator, reset p.
			 */
			p = (EMCHAR *)NULL;
			continue;
		}

		if( c == '.' ) {
			p = (EMCHAR *)&bak[ s ];
			if( emstrcmp( p, ECSTR(".BAK") ) == 0 ||
			    emstrcmp( p, ECSTR(".bak") ) == 0 ||
			    emstrcmp( p, ECSTR(".Bak") ) == 0 )
				/*
				 * Don't backup .BAK files.
				 */
			    	break;
		}
	}

	if( p != NULL )
		*p = '\000';

	(void)emstrcat( bak, ECSTR(".BAK") );

	if( ffaccess( bak ) == FIOSUC ) {
		if( (fd = ffopen( bak, ECSTR("r"), NULL )) != NULL ) {
			(void)fclose( fd );
			(void)ffchmod( bak, (int)curbp->b_fmode );
			(void)ffremove( bak );
		}
	}

	if( (backup_before_writing == T)
	    && (fd = ffopen( fname, ECSTR("r"), NULL )) != NULL) {
	    (void)fclose( fd );
	    if( ffrename( fname, bak ) != FIOSUC )
	      WDGwrite(ECSTR("Could not rename %s to backup file .BAK"), fname);
	}

	if( (s = ffwopen( fname, curbp->b_binary, curbp->b_wide )) != FIOSUC )
		return( NIL );

	nline = 0;
	nchar = 0;
	
	for( lp = firstline(curbp) ; lp != lastline(curbp) ; lp = lforw(lp) ) {
		if( (s = ffputline(ltext( lp ), llength(lp))) != FIOSUC )
			break;
		nline += 1;
		nchar += llength( lp ) + (curbp->b_binary ? 1 : CRSIZE);
	}

	if( s == FIOSUC ) {
	       s = ffclose();
	       if( s == FIOSUC )
		   WDGwrite(ECSTR("Wrote %d line(s), %ld byte(s)"),
			    nline,
			    (size_t)nchar * sizeof( EMCHAR ));
	} else s = ffclose();

	(void)ffchmod( fname, (int)curbp->b_fmode );
	(void)ffsetaccess( fname );

	curbp->b_flag &= ~BFCHG;

	/* 
	 *	Update mode lines.
	 */
	
	(void)updatemodes();

	return( (s != FIOSUC) ? NIL : T );
}

#if	defined(_WINDOWS_SOURCE) && !defined(_X11_ONLY) && !defined(HAVE_CONFIG_H)
extern 	CMD NTansitooem( EDLINE *lp );
extern 	CMD NToemtoansi( EDLINE *lp );

CMD
ansitooem( void )
{
#if	defined( _WIN32_WCE )
	return( NIL );
#else
	EDLINE	*lp;

	if( freadonly() == T )
		return( NIL );

	curbp->b_flag &= ~BFCHG;

	for( lp = firstline(curbp) ; lp != lastline(curbp) ; lp = lforw(lp) )
		NTansitooem( lp );

	lchange( WFHARD );

	return( T );
#endif
}

CMD
oemtoansi( void )
{
#if	defined( _WIN32_WCE )
	return( NIL );
#else
	EDLINE	*lp;

	if( freadonly() == T )
		return( NIL );

	curbp->b_flag &= ~BFCHG;

	for( lp = firstline(curbp) ; lp != lastline(curbp) ; lp = lforw(lp) )
		NToemtoansi( lp );

	lchange( WFHARD );

	return( T );
#endif
}

static unsigned char MacToISOLatin1[] = {
  /* 00 */	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  /* 08 */	0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
  /* 10 */	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  /* 18 */	0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
  /* 20 */	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  /* 28 */	0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
  /* 30 */	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  /* 38 */	0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
  /* 40 */	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  /* 48 */	0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
  /* 50 */	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  /* 58 */	0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
  /* 60 */	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  /* 68 */	0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
  /* 70 */	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  /* 78 */	0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
  /* 80 */	0xC4, 0xC5, 0xC7, 0xC9, 0xD1, 0xD6, 0xDC, 0xE1,
  /* 88 */	0xE0, 0xE2, 0xE4, 0xE3, 0xE5, 0xE7, 0xE9, 0xE8,
  /* 90 */	0xEA, 0xEB, 0xED, 0xEC, 0xEE, 0xEF, 0xF1, 0xF3,
  /* 98 */	0xF2, 0xF4, 0xF6, 0xF5, 0xFA, 0xF9, 0xFB, 0xFC,
  /* A0 */	0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
  /* A8 */	0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
  /* B0 */	0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
  /* B8 */	0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
  /* C0 */	0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
  /* C8 */	0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
  /* D0 */	0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
  /* D8 */	0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
  /* E0 */	0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xCA, 0xC1,
  /* E8 */	0xCB, 0xC8, 0xCD, 0xCE, 0xCF, 0xCC, 0xD3, 0xD4,
  /* F0 */	0xF0, 0xF1, 0xDA, 0xDB, 0xD9, 0xF5, 0xF6, 0xF7,
  /* F8 */	0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
};

CMD
mactoansi( void )
{
	EDLINE	*lp;

	if( freadonly() == T )
		return( NIL );

	curbp->b_flag &= ~BFCHG;

	for( lp = firstline(curbp) ; lp != lastline(curbp) ; lp = lforw(lp) ) {
		EMCHAR	*p = ltext( lp );
		int	l  = llength( lp );
		int	i;

		for( i = 0 ; i < l ; ++i )
			p[i] = (EMCHAR)MacToISOLatin1[ (p[i] & 0xff) ];
	}

	lchange( WFHARD );

	return( T );
}

CMD
mactooem( void )
{
	if( mactoansi() == T )
		return( ansitooem() );
	else	return( NIL );
}

#else
CMD
ansitooem( void )
{
	return( T );
}

CMD
oemtoansi( void )
{
	return( T );
}

CMD
mactoansi( void )
{
	return( T );
}

CMD
mactooem( void )
{
	return( T );
}
#endif

/*
 *      Return the ptr in sp at which the character c last appears;
 *      returns NULL if none is found.
 */

int
getautomode( EMCHAR *sp )
{
	static  struct  {
		EMCHAR  *ext;
		int     mode;
	} modtable[] = {
			{ ECSTR(".ll"),    LISPMODE    },
			{ ECSTR(".LL"),    LISPMODE    },
			{ ECSTR(".cl"),    LISPMODE    },
			{ ECSTR(".ml"),    LISPMODE    },
			{ ECSTR(".ML"),    LISPMODE    },
			{ ECSTR(".lap"),   LISPMODE    },
			{ ECSTR(".LAP"),   LISPMODE    },
			{ ECSTR(".lsp"),   LISPMODE    },
			{ ECSTR(".LSP"),   LISPMODE    },
			{ ECSTR(".lisp"),  LISPMODE    },
			{ ECSTR(".emacs"), LISPMODE    },
			{ ECSTR(".osp"),   LISPMODE    },
			{ ECSTR(".c"),     CMODE       },
			{ ECSTR(".h"),     CMODE       },
			{ ECSTR(".wmls"),  CMODE       },
			{ ECSTR(".C"),     CPPMODE     },
			{ ECSTR(".CC"),    CPPMODE     },
			{ ECSTR(".cc"),    CPPMODE     },
			{ ECSTR(".cpp"),   CPPMODE     },
			{ ECSTR(".cs"),    CSHARPMODE  },
			{ ECSTR(".cxx"),   CPPMODE     },
			{ ECSTR(".H"),     CPPMODE     },
			{ ECSTR(".hh"),    CPPMODE     },
			{ ECSTR(".hxx"),   CPPMODE     },
			{ ECSTR(".hpp"),   CPPMODE     },
			{ ECSTR(".idl"),   CPPMODE     },
			{ ECSTR(".IDL"),   CPPMODE     },
			{ ECSTR(".html"),  SGMLMODE    },
			{ ECSTR(".htm"),   SGMLMODE    },
			{ ECSTR(".sgml"),  SGMLMODE    },
			{ ECSTR(".sml"),   SGMLMODE    },
			{ ECSTR(".wml"),   SGMLMODE    },
			{ ECSTR(".xml"),   SGMLMODE    },
			{ ECSTR(".xsl"),   SGMLMODE    },
			{ ECSTR(".java"),  JAVAMODE    },
			{ ECSTR(".jav"),   JAVAMODE    },
			{ ECSTR(".js"),    JAVAMODE    },
			{ ECSTR(".xlg"),   PROLOGMODE  },
			{ ECSTR(".pro"),   PROLOGMODE  },
			{ ECSTR(".pl"),    PERLMODE    },
			{ ECSTR(".asm"),   ASMODE      },
			{ ECSTR(".s"),     ASMODE      },
			{ ECSTR(".pas"),   PASCALMODE  },
			{ ECSTR(".p"),     PASCALMODE  },
			{ ECSTR(".f"),     FORTRANMODE },
			{ ECSTR(".for"),   FORTRANMODE },
			{ ECSTR(".sh"),    SHELLMODE   },
	};

	EMCHAR	*trail = NULL;
	int     i;

	do if( *sp == '.' )
		trail = sp;
	while( *sp++ );

	if( trail ) {
		int size = (int) (sizeof(modtable)/sizeof(modtable[0]));

		for( i = 0 ; i < size ; i++ )
			if( (emstrcmp( trail, modtable[i].ext  ) == 0 ) )
				return( modtable[i].mode );
	}

	return( FUNDAMENTAL );
}

#define digitp( c )             ((c) >= '0' && (c) <= '9')
#define slashp( c )             ((c) == '/')

#define MAXDATE 16              /* maximum date length  */

static  void
savetime( void )
{
#if	!defined( _WIN32_WCE )
	EDLINE  *lp;
	EDLINE  *oldp;
	int     oldo;
	EMCHAR    *buf;
	int     i;
	int     maxl;

	if( date_completion != T )
		return;

	curwp->w_flag |= WFHARD;

	if( (lp = firstline( curbp )) == lastline( curbp ) ||
	    (lp = lforw( lp ))        == lastline( curbp ) )
		return;

	maxl = llength( lp );
	buf  = ltext( lp );

	for( i = 0 ; i < (maxl - 7) ; i++ ) {
		if( digitp( buf[ i + 0 ] ) &&
		    digitp( buf[ i + 1 ] ) &&
		    slashp( buf[ i + 2 ] ) &&
		    digitp( buf[ i + 3 ] ) &&
		    digitp( buf[ i + 4 ] ) &&
		    slashp( buf[ i + 5 ] ) &&
		    digitp( buf[ i + 6 ] ) &&
		    digitp( buf[ i + 7 ] ) ) {
			int       datelen;
			int       atline = NIL;
			EMCHAR      sdate[ MAXDATE ];
			struct tm *nt;
			time_t    tb;

			if( i < (maxl - 9) &&
			    digitp( buf[ i + 8 ] ) && digitp( buf[ i + 9 ] ) )
				datelen = 10;
			else    datelen =  8;

			if( i > 2 &&
			    digitp( buf[ i - 1 ] ) &&
			    digitp( buf[ i - 2 ] ) ) {
			    	datelen += 2;
				i	-= 2;
			}

			oldp = curwp->w_dotp;
			oldo = curwp->w_doto;

			if( oldp == lp )
				atline = T;

			(void)time( &tb );
			nt = localtime( &tb );

			(void)emsprintf3(
					  sdate,
					  ECSTR("%04d/%02d/%02d"),
					  nt->tm_year + 1900,
					  nt->tm_mon  + 1,
					  nt->tm_mday
				        );

			curwp->w_dotp = lp;

			curwp->w_doto = i;
			(void)ldelete( datelen, NIL );

			for( i = 0 ; sdate[ i ] ; i++ )
				(void)linsert( 1, sdate[ i ] );

			if( atline == NIL )
				curwp->w_dotp = oldp;

			curwp->w_doto = oldo;
			break;
		}
	}
#endif
}

/*
 *      Toggle  the  write access for the current buffer only.  Bound
 *      to C-XC-Q
 */

CMD
toggleread( void )
{
	if( (curbp->b_fmode & WACCESS) ) {
		curbp->b_fmode &= ~WACCESS;
		WDGmessage( ECSTR("Access right set to read") );
	} else  {
		curbp->b_fmode |= WACCESS;
		WDGmessage( ECSTR("Access right set to read/write") );
	}

	return( T );
}

/*
 *      The   command  allows  the user  to  modify  the   file  name
 *      associated  with  the  current buffer.  It is  like  the  "f"
 *      command in UNIX "ed".  The operation is simple;  just zap the
 *      name in the BUFFER structure, and mark the windows as needing
 *      an  update.  You can type a blank line at the prompt  if  you
 *      wish.
 */

CMD
findfile( void )
{
	EMCHAR	*ofname = getbufdir();
	int     s;

	complete.fn = filematch;

	if( (s=WDGedit(ECSTR("Find file: "), ofname, NFILEN)) != T && s != NIL )
		return( s );
	else    return( newfile( ofname ) );
}

/*
 *      Read a file into the current buffer. This is really easy; all
 *      you  do it find the name of the file,  and call the  standard
 *      "read a file into the current buffer" code.  Before returning
 *      it sets the mode to Read-only. Bound to "C-X C-R".
 */

CMD
fileread( void )
{
	EMCHAR	*ofname = getbufdir();
	int     s;

	complete.fn = filematch;

	s = WDGedit( ECSTR("Find file read-only: "), ofname, NFILEN );

	if( s!=T && s!=NIL )
		return( s );

	if( (s = newfile( ofname )) != T )
		return( s );

	if( (curbp->b_fmode & WACCESS) )
		curbp->b_fmode &= ~WACCESS;

	return( updatemodes() );
}

/*
 *      Select  a file for editing. Asks the user for a file name to
 *      be read and call internal routine "readin". Bound to C-XC-V
 */

CMD
filealternate( void )
{
	EMCHAR	*ofname = getbufdir();
	EMCHAR    bname[ NBUFN ];
	int     s;

	complete.fn = filematch;

	if( (s = WDGedit(ECSTR("Find alternate file: "), ofname, NFILEN)) != T )
		return( s );

	if( ffilevalid( ofname ) != T )
		return( NIL );

	if( readin( ofname ) != T )
		return( NIL );

	*curbp->b_bname = '\000';
	makename( (EMCHAR *)&bname[0], ofname );
	(void)emstrcpy( curbp->b_bname, (EMCHAR *)&bname[0] );

	(void)WDGtitle(
			(EMCHAR *)&curbp->b_bname[0],
			(EMCHAR *)&curbp->b_fname[0]
		      );

	/*
	 *      Update mode lines.
	 */

	return( updatemodes() );
}

/*
 *      Insert  file "fname"  into the  current  buffer,  at  dot "."
 *      position preserving any existent text in buffer. Return  the
 *      final status of the read. In some EMACS this command is bound
 *      to C-XC-I.
 */

CMD
fileinsert( void )
{
	BUFFER   *bp;
	EDLINE   *lp1;
	EDLINE   *lp2;
	EDLINE   *cbp;
	EMCHAR	 *ofname;
	int      i;
	int      cbo;
	int      s;
	int      nline;
	int      nbytes;
	int	 binmode;
	int	 widep = 0;

	EMCHAR   line[ MAXLINE ];

	if( freadonly() )
		return( NIL );

	complete.fn = filematch;

	ofname = getbufdir();

	if( (s = WDGedit( ECSTR("Insert file: "), ofname, NFILEN )) != T )
		return( s );

	if( ffilevalid( ofname ) != T )
		return( NIL );

	if( ffdiredp( ofname ) ) {
		TTYbeep();
		WDGwrite( ECSTR("Can't insert directory!!") );
		return( NIL );
	}

	bp = curbp;                             /* Cheap.               */
	bp->b_flag &= ~BFCHG;

	if( (s = ffropen( ofname, &binmode, &widep )) == FIOERR ) {
		WDGwrite( ECSTR("Can't open %s"), ofname );
		return( NIL );
	}

	if( s == FIOFNF ) {
		/*
		 * File not found.
		 */
		WDGwrite( ECSTR("%s not found."), ofname );
		return( NIL );
	}

	WDGmessage( ECSTR("Inserting file ...") );

	nline   = 0;
	cbo     = curwp->w_doto;
	cbp     = curwp->w_dotp;
	lp2	= (EDLINE *)NULL;

	while( (s=ffgetline( line, MAXLINE, &nbytes ))==FIOSUC || s==FIOWNL )
		if( nline++ == 0 ) {
			/*
			 * It's   the   first  line.  Insert  at  the
			 * current position.
			 */

			for( i = 0 ; i < nbytes ; ++i )
				(void)linsert( 1, line[ i ] );

			/*
			 * break line only if not a the end of buffer.
			 */

			if( lastline( curbp ) != lforw( curwp->w_dotp ) )
				(void)openline();

			lp2             = curwp->w_dotp;
			cbp             = curwp->w_dotp;
			curwp->w_markp  = cbp;
			curwp->w_marko  = cbo;
		} else  {
			/*
			 * A   previous   line   has   already   been
			 * inserted. lp2 points to this line.
			 */

			if( (lp1 = lalloc( nbytes )) == NULL ) {
				s = FIOERR;     /* Keep message on the  */
				break;          /* display.             */
			}
			lback( lforw( lp2 ) )   = lp1;
			lforw( lp1 )            = lforw( lp2 );
			lforw( lp2 )            = lp1;
			lback( lp1 )            = lp2;
			for( i = 0 ; i < nbytes ; ++i )
				lputc( lp1, i, line[ i ] );
			lp2 = lp1;
			if( s == FIOWNL )
				break;
		}

	(void)ffclose();                        /* Ignore errors.       */
	if( s == FIOEOF ) {                     /* Don't zap message!   */
		if( nline == 1 )
			WDGwrite( ECSTR("Insert 1 line") );
		else    WDGwrite( ECSTR("Insert %d lines"), nline );
	}

	curwp->w_doto = cbo;
	curwp->w_dotp = cbp;
	lchange( WFHARD );

	return( (s == FIOERR) ? NIL : T );

}

/*
 *      Ask  for a file name,  and write the contents of the  current
 *      buffer  to that file.   Update the remembered file  name  and
 *      clear the buffer changed flag. Bound to "C-XC-W".
 */

CMD
filewrite( void )
{
	EMCHAR	*ofname = getbufdir();
	int     s;

	if( curbp->b_fname[ 0 ] == 0 ) {
		/*
		 * Must have a name.
		 */
		complete.fn = filematch;
		s = WDGedit( ECSTR("File to save in: "), ofname, NFILEN );
	} else  {
		complete.fn = fileaccept;
		(void)emstrcpy( ofname, curbp->b_fname );
		if( (s=WDGedit(ECSTR("Write file: "), ofname, NFILEN)) == NIL )
			(void)filesave();
	}

	if( s != T )
		return( s );

	if( ffsetaccess( ofname ) != T ) {
		WDGwrite( ECSTR("Can't write %s"), ofname );
		return( NIL );
	}

	if( (s = writeout( ofname )) == T ) {
		EMCHAR    bname[ NBUFN ];

		makename( (EMCHAR *)&bname[0], ofname );
		(void)emstrcpy( curbp->b_bname, bname );
		(void)emstrcpy( curbp->b_fname, ofname );
		(void)ffullname( curbp->b_fname, ofname );
	}

	return( s );
}

/*
 *      Save  the contents of the current buffer  in  its  associated
 *      file.  No  nothing if nothing has changed (this may be a bug,
 *      not a feature). Error if there is no remembered file name for
 *      the buffer. Bound to "C-X C-S".
 */

CMD
filesave( void )
{
	if( (curbp->b_flag & BFCHG ) == 0 ) {   /* Return, no changes.  */
		WDGmessage( ECSTR("(No changes need to be saved)") );
		return( T );
	}

	if( curbp->b_fname[ 0 ] == 0 )
		return( filewrite() );

	return( writeout( curbp->b_fname ) );

}

/*
 *      Save all unsaved buffer.  Ask for confirmation.  Bound  to
 *      C-XC-S.
 */

CMD
savesomebuffers( void )
{
	return( anycb( ANYCB_PROMPT ) );
}

/*
 *      Restore  a  buffer  to its orignial contents and try to go to
 *      the  previous position (where the dot was before calling this
 *      command). Not bound.
 */

CMD
revertbuffer( void )
{
	EDLINE  *clp;
	int     cln  = 0;
	int     opos = curwp->w_doto;

	if( curbp->b_fname[0] == '\000' ) {
	 WDGwrite(ECSTR("Buffer does not seem to be associated with any file"));
	 return( NIL );
 	}

	/*
	 *      Compute the line number of containing dot.
	 */

	for( clp = firstline(curbp) ; clp != curwp->w_dotp ; clp = lforw(clp) )
		cln++;

	/*
	 *      Reading buffer again.
	 */

	(void)readin( curbp->b_fname );

	/*
	 *      Goto the line at dot (if any).
	 */

	for(clp = firstline(curbp) ; clp != lastline(curbp) ; clp = lforw(clp))
		if( cln-- == 0 )
			break;

	curwp->w_dotp  = clp;
	curwp->w_flag |= WFMOVE;

	if( llength( curwp->w_dotp ) >= opos )
		curwp->w_doto = opos;

	return( T );
}

/*
 *      Delete a file. Not bound.
 */

CMD
unlinkfile( void )
{
	EMCHAR	*ofname = getbufdir();
	int     s;

	complete.fn = filematch;

	if( (s = WDGedit( ECSTR("Delete file: "), ofname, NFILEN )) != T )
		return( s );

	return( removefile( ofname, NIL ) );
}

CMD
removefile( EMCHAR *fname, int flag )
{
	if( flag == NIL ) {
		EMCHAR    buf[ NFILEN + 16 ];

		(void)emstrcpy( buf, ECSTR("Delete ") );
		(void)emstrcat( buf, fname     );
		(void)emstrcat( buf, ECSTR(" ? ")     );

		if( WDGyn( buf ) != T )
			return( NIL );
	}

	if( ffremove( fname ) == FIOSUC ) {
		if( flag == NIL )
			WDGmessage( ECSTR("File deleted.") );
		return( T );
	} else  {
		if( flag == NIL )
			WDGmessage( ECSTR("Can't delete file.") );
		return( NIL );
	}
}

/*
 *	Print the current buffer on the printer connected to the system.
 */

CMD
printbuffer( void )
{
	LPTprint();
	return( T );
}
