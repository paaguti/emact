#if	!defined( lint )
static	char rcsid[] = "$Id: fileio.c,v 1.19 2011/09/22 05:55:22 jullien Exp $";
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
 *	The  routines  in  this  file read and write ASCII or UNICODE
 *	files  from  the  disk.  All of the knowledge about files are
 *	here. A better message writing scheme should be used.
 */

#include	"emacs.h"

static	void	_define(testfilemode,(EMCHAR *fn, int *binmode, int *utf8));

#if	defined( _WINDOWS_SOURCE ) && !defined( _WIN32_WCE )
static	void	_define(NTfullname,(EMCHAR *rname, EMCHAR *fname));
#endif

static	FILE	*ffp;			/* File pointer, all functions. */
static	int	ffunicode = 0;		/* UNICODE flag			*/

/*
 *	Open a file for reading.
 */

#if	defined( _DOSPATH ) || defined( _UNICODE )
#define	MAX_SENSE	256

/*
 *	Given a file name, testfilemode checks if this file should be
 *	opened  in  binary mode (i.e.  without \n -> \r\n conversion)
 *	or  as  standard text mode.  The pointer binmode is filled on
 *	output  with the appropriate value.  This code is called only
 *	on DOS boxes where conversions could apply. It also checks if
 *	UTF-8 is specified in the MAX_SENSE first bytes.
 */

#if	defined( _UNICODE )
static int	_define(emfindencoding,(const char *mem, int max ));

static int
emfindencoding(const char *mem, int max )
{
	int i;

	for( i = 0 ; i < max ; ++i ) {
		if( !(mem[i] == 'u' || mem[i] == 'U') )
			continue;
		++i;
		if( !(mem[i] == 't' || mem[i] == 'T') )
			continue;
		++i;
		if( !(mem[i] == 'f' || mem[i] == 'F') )
			continue;
		++i;
		if( mem[i] == '-' )
			++i;
		if( mem[i] == '8' ) {
			return( EMUTF8 );
		}
		if( (mem[i] == '1' || mem[i+1] == '6') )
			return( EMUTF16 );
	}

	return( EMASCII );
}
#endif

static	void
testfilemode( EMCHAR *fn, int *binmode, int *utf8 )
{
	FILE	*fd;
	char	buf[MAX_SENSE + 64];
	int	widep;
	int	n;
	int	i;

	if( binmode == NULL || utf8 == NULL ) {
		/*
		 *	Be paranoiac.
		 */
		return;
	}

	*binmode = NIL;

	if( auto_encoding_mode == T ) {
		switch( getautomode( fn ) ) {
		case JAVAMODE:
		case CMODE:
		case CPPMODE:
		case CSHARPMODE:
		case SHELLMODE:
			*utf8 = EMUTF8;
			break;
		default:
			*utf8 = EMASCII;
		}
	}

	/*
	 *	Open file in binary mode.
	 */

	if( (fd = ffopen( fn, ECSTR("rb"), &widep )) != NULL ) {
		if( widep == EMUTF16 ) {
			*binmode = NIL;
			(void)fclose( fd );
			return;
		}

		/*
		 *	Read few bytes in a buffer
		 */

		n = (int)fread( buf, sizeof( char ), MAX_SENSE, fd );
		(void)fclose( fd );

#if	defined( _UNICODE )
		if( auto_encoding_mode == T ) {
			switch( emfindencoding( buf, n ) ) {
			case EMUTF8:
				*utf8     = EMUTF8;
				ffunicode = EMUTF8;
				break;
			case EMUTF16:
				ffunicode = EMUTF16;
				break;
			}
		}
#endif

		if( buf[0] == '#' && buf[1] == '!' ) {
			*binmode = T;
			return;
		}

		/*
		 *	If   we  find  a  newline  and  no  preceding
		 *	carriage-return,  it's probably a binary file
		 *	(read a unix file).
		 */

		for( i = 0 ; i < n ; ++i ) {
			if( buf[i] == '\n' ) {
				if( (i > 0) && buf[i-1] == '\r' )
					*binmode = NIL;
				else	*binmode = T;
				break;
			}
		}
	}
}
#else
static	void
testfilemode( EMCHAR *fn, int *binmode, int *utf8 )
{
	if( fn ) {
		*binmode = NIL;
		*utf8    = EMASCII;
	}
}
#endif

int
ffropen( EMCHAR	*fn, int *binmode, int *widep )
{
	if( ffaccess( fn ) != FIOSUC )
		return( FIOFNF );

	testfilemode( fn, binmode, widep );

	if( (ffp = ffopen( fn, ECSTR("r"), widep )) == NULL ) {
		return( FIOFNF );
	}

	return( FIOSUC );
}

/*
 *	Open  a file for writing. Return T if all is well, and NIL
 *	on error (cannot create).
 */

int
ffwopen( EMCHAR *fn, int binmode, int widep )
{
	EMCHAR	wmode[ 3 ];

	if( binmode == T )
		(void)emstrcpy( wmode, ECSTR("wb") );
	else	(void)emstrcpy( wmode, ECSTR("w")  );

	if( (ffp = ffopen( fn, wmode, &widep )) == NULL ) {
		WDGerror( ECSTR("Cannot open file for writing") );
		return( FIOERR );
	}

	return( FIOSUC );
}

/*
 *	Close a file. Should look at the status in all systems.
 */

int
ffclose( void )
{
	if( fclose( ffp ) != 0 ) {
		WDGerror( ECSTR("Error closing file") );
		return( FIOERR );
	} else 	{
		ffp = (FILE *)NULL;
		return( FIOSUC );
	}

}

/*
 *	Write a line to the already opened file.  The "buf" points to
 *	the  buffer,  and the "nbuf" is its  length,  less  the  free
 *	newline. Return the status.  Check only at the newline.
 */

int
ffputline( EMCHAR *buf, int nbuf )
{
#if	defined( _UNICODE )
	char	wbuf[ EMMB_LEN_MAX ];
	int	conv;
	int	c;

	switch( ffunicode ) {
	case EMUTF8:
		while( nbuf-- > 0 ) {
			c = *buf++;
			if( (conv = emwctomb( wbuf, (EMCHAR)c )) > 0) {
				(void)fwrite( wbuf, conv, 1, ffp );
			} else	{
				(void)fputc( '?', ffp );
			}
		}
		(void)fputc( '\n', ffp );
		break;
	case EMUTF16:
		while( nbuf-- > 0 )
			(void)fputwc( *buf++, ffp );
		(void)fputwc( '\n', ffp );
		break;
	default:
		while( nbuf-- > 0 )
			(void)fputc( *buf++, ffp );
		(void)fputc( '\n', ffp );
	}
#else
	while( nbuf-- > 0 )
		(void)fputc( *buf++, ffp );
	(void)fputc( '\n', ffp );
#endif

	if( ferror( ffp ) != NIL ) {
		WDGerror( ECSTR("Write I/O error") );
		return( FIOERR );
	} else	return( FIOSUC );
}

/*
 *	Read a line from a file,  and store the bytes in the supplied
 *	buffer.  The  "nbuf"  is the length of the  buffer.  Complain
 *	about long lines and lines at the end of the file that  don't
 *	have  a  newline present.  Check for I/O errors  too.  Return
 *	status and 'len' is set to the length of the line.
 */

int
ffgetline( EMCHAR *buf, int nbuf, int *len )
{
	int	c;
	int	i = 0;
	int	status = FIOSUC;

#if	defined( _UNICODE )
	switch( ffunicode ) {
	case EMUTF8:
		while( (c = fgetc( ffp )) != EOF && c != '\n' ) {
		   EMCHAR wc = 0;
		   char	  tmp[ EMMB_LEN_MAX ];
		   int	  j;
		   int	  nb;

		   if( i >= nbuf ) {
		       WDGwrite(ECSTR("File has line longer than %d chars."),i);
		       return( FIOERR );
		   }

		   nb = emmbclen( c );

		   if( nb == 1 ) {
			   buf[ i++ ] = (EMCHAR)c;
			   continue;
		   } else  if( nb == 0 ) {
			   WDGwrite(ECSTR("UTF-8 encoding error#1."));
			   return( FIOERR );
		   }

		   tmp[ 0 ] = (char)c;

		   for( j = 1 ; j < nb ; ++j ) {
			   c = fgetc( ffp );
			   if( c == EOF ) {
				   break;
			   } else  if( (c == 0) || ((c & 0x80) == 0) ) {
				   WDGwrite(ECSTR("UTF-8 encoding error#2."));
				   return( FIOERR );
			   }
			   tmp[ j ] = (char)c;
		   }

		   if( emmbtowc( &wc, tmp, nb ) == -1 ) {
			   WDGwrite(ECSTR("UTF-8 encoding error#3."));
			   return( FIOERR );
		   }

		   buf[ i++ ] = (EMCHAR)wc;
		}
		break;
	case EMUTF16:
		while( (c = fgetwc( ffp )) != EMEOF && c != '\n' ) {
		   if( c == '\r' )
		       continue;
		   if( i >= nbuf ) {
		       WDGwrite(ECSTR("File has line longer than %d chars."),i);
		       return( FIOERR );
		   } else buf[ i++ ] = (EMCHAR)c;
		}
		break;
	default:
		while( (c = fgetc( ffp )) != EOF && c != '\n' ) {
		   if( i >= nbuf ) {
		       WDGwrite(ECSTR("File has line longer than %d chars."),i);
		       return( FIOERR );
		   } else buf[ i++ ] = (EMCHAR)c;
		}
	}
#else
	while( (c = fgetc( ffp )) != EMEOF && c != '\n' )
		if( i >= nbuf ) {
		       WDGwrite(ECSTR("File has line longer than %d chars."),i);
		       return( FIOERR );
		} else buf[ i++ ] = (EMCHAR)c;
#endif
	buf[ *len = i ] = '\0';

	if( c == ((ffunicode == EMUTF16) ? EMEOF : EOF) ) {
		if( ferror( ffp ) != NIL ) {
			WDGerror( ECSTR("File read error") );
			return( FIOERR );
		}
		if( i != 0 ) {
			WDGerror( ECSTR("File should end with newline.") );
			return( FIOWNL );
		}
		return( FIOEOF );
	} else	{
		return( status );
	}
}

/*
 *	Delete the file 'fn'. Returns FIOSUC on succes.
 */

int
ffremove( EMCHAR *fn )
{
	if( emremove( fn ) == 0 )
		return( FIOSUC );
	else	return( FIOERR );
}

/*
 *	Delete the file 'fn'. Returns FIOSUC on succes.
 */

int
ffrename( EMCHAR *oldfn, EMCHAR *newfn )
{
	if( emrename( oldfn, newfn ) == 0 )
		return( FIOSUC );
	else	return( FIOERR );
}

/*
 *	Get a line form fd.
 */

EMCHAR *
ffgets( EMCHAR *buf, int n, FILE *fd )
{
#if	defined( _UNICODE )
	char	data[ MAXLINE ];
	int	i;

	switch( ffunicode ) {
	case EMUTF16:
		return( fgetws( buf, n, fd ) );
	case EMUTF8:
	default:
		if( fgets( data, n, fd ) ) {
			for( i = 0 ; data[ i ] ; ++i )
				buf[ i ] = (EMCHAR)data[ i ];
			buf[ i ] = '\000';
			return( &buf[0] );
		} else	{
			return( (EMCHAR *)NULL );
		}
	}
#else
	return( fgets( buf, n, fd ) );
#endif
}

/*
 *	Returns the complete path of 'fname' into 'rfname'.
 */

int
ffullname( EMCHAR *rname, EMCHAR *fname )
{
#if	defined( _WINDOWS_SOURCE ) && !defined( _WIN32_WCE )
	NTfullname( rname, fname );
	(void)normalize( rname, NOSLASH );
#else
	EMCHAR	c;

	if( fname[0] == '\\' || fname[0] == '/' || fname[1] == ':' ) {
		(void)emstrcpy( rname, fname );
		(void)normalize( rname, NOSLASH );
		return( FIOSUC );
	}

	(void)ffgetcwd( rname, NFILEN-1 );

	if( (c = rname[ emstrlen( rname ) - 1 ]) != '\\' && c != '/' )
		(void)emstrcat( rname, ECSTR("/") );
	(void)emstrcat( rname, fname );
	(void)normalize( rname, NOSLASH );
#if	defined( _DOSPATH )
	(void)emstrlwr( rname );
#endif
#endif
	return( FIOSUC );
}

/*
 *	Test  the  accessibility  of  file  'fn'.  Returns  FIOSUC on
 *	succes.  If  the  operating  system does not support 'access'
 *	return FIOSUC.
 */

int
ffaccess( EMCHAR *fn )
{
#if	defined( _DIRECTORY )

	if( emaccess( fn, F_OK ) == 0 )
		return( FIOSUC );
	else	return( FIOERR );
#else
	return( FIOSUC );
#endif
}

/*
 *	Write Byte Order Mark for UNICODE
 */

void
ffputbom( int widep )
{
	if( widep == EMUTF16 )
#if	defined( _UNICODE )
		fputwc( EMBOM, ffp );
#else
		WDGwrite(ECSTR("BOM set without UNICODE"));
#endif
}

/*
 *	Open a file for reading. Wrapper for UNICODE.
 */

FILE *
ffopen( EMCHAR *file, EMCHAR *mode, int *widep )
{
	FILE	*fd;
#if	defined( _UNICODE )
	int	c1 = 0;
	int	c2 = 0;
	int	w  = 0;

	if( widep == NULL ) {
		/*
		 * widep may be passed as NULL if no used.
		 */
		widep     = &w;
		ffunicode = 0;
	} else	{
		ffunicode = *widep;
	}

	if( mode[0] == 'r' ) {
		fd = emfopen( file, mode );

		if( fd == NULL )
			return( fd );
		c1 = fgetc( fd );
		if( c1 != EOF)
			c2 = fgetc( fd );
		fclose( fd );

		if( ((c1 == 0xFF) && (c2 == 0xFE))
		    || ((c1 == 0xFE) && (c2 == 0xFF))  ) {
			fd = emfopen( file, ECSTR("rb") );
#if	!defined( _M_IA64 )
			emfwide( fd, 1 );
#endif
			ffunicode = EMUTF16;
			if( fgetwc( fd ) )
				*widep = EMUTF16;
			return( fd );
		}
	} else	if( mode[0] == 'w' && *widep ) {
		fd = emfopen( file, ECSTR("wb") );
		if( fd == NULL )
			return( fd );
#if	!defined( _M_IA64 )
		emfwide( fd, 1 );
#endif
		if( *widep == EMUTF16 ) {
			fputwc( EMBOM, fd );
		}
		ffunicode = *widep;
		return( fd );
	}

#endif

	if( mode[0] == '\r' ) {
		*widep = 0;
	}

	fd = emfopen( file, mode );
	return( fd );
}

/*
 *	stat wrapper for UNICODE.
 */

int
ffstat( EMCHAR *file, EMSTAT *mode )
{
	return( emstat( file, mode ) );
}

/*
 *	chmod wrapper for UNICODE.
 */

int
ffchmod( EMCHAR *file, int mode )
{
#if	defined(linux)
	return( emchmod( file, (mode_t)mode ) );
#else
	return( emchmod( file, mode ) );
#endif
}

/*
 *	chdir wrapper for UNICODE.
 */

#if	defined( _OS2V1 )
int
chdir( s )
EMCHAR	*s;
{
	EMCHAR	buf[ NLINE ];

	(void)emstrcpy( buf, "cd " );
	(void)emstrcat( buf, s );
	(void)ffsystem( s );
	return( 0 );
}
#endif

int
ffchdir( EMCHAR *file )
{
	return( emchdir( file ) );
}

/*
 *	getcwd wrapper for UNICODE.
 */

EMCHAR *
ffgetcwd( EMCHAR *file, int len )
{
	(void)emgetcwd( file, len );
	return( file );
}

/*
 *	getenv wrapper for UNICODE.
 */

EMCHAR *
ffgetenv( EMCHAR *var )
{
	return( emgetenv( var ) );
}

/*
 *	system wrapper for UNICODE.
 */

int
ffsystem( EMCHAR *cmd )
{
	return( emsystem( cmd ) );
}

/*
 *      Returns  T  if  the file has been modified since last read or
 *      last write.
 */

int
ffchanged( EMCHAR *fname )
{
#if     defined( _DIRECTORY )
	EMSTAT	mode;

	if( ffstat( fname, &mode ) != 0 )
		return( NIL );

	if( (long)(curbp->b_time - mode.st_mtime) < 0 ) {
		EMCHAR	*msg;
		msg = ECSTR("File changed on disk, really save the buffer? ");

		if( WDGyn(msg) == T ) {
			curbp->b_time = mode.st_mtime;
			return( NIL );
		} else	{
			WDGmessage( ECSTR("") );
			return( T );
		}
	} else return( NIL );

#else
	return( NIL );
#endif
}

/*
 *	Set the mode and access time of fname.
 */

int
ffsetaccess( EMCHAR *fname )
{
#if     defined( _DIRECTORY )
	EMSTAT	mode;

	if( (ffaccess( fname )==FIOSUC) && (ffstat( fname, &mode )==0) ) {
		if( S_ISREG( mode.st_mode ) ) {
			curbp->b_fmode = (FMODE_T)(mode.st_mode & 0777);
			curbp->b_time  = mode.st_mtime;
		} else	return( NIL );
	} else	{
		curbp->b_fmode = WMODE;
		curbp->b_time  = 0;
	}
#else
	curbp->b_fmode = WMODE;
	curbp->b_time  = 0;
#endif  /* _DIRECTORY */

	return( T );
}

/*
 *      Returns T if the file can be opened, NIL otherwise.
 */

int
ffilevalid( EMCHAR *fname )
{
#if     defined( _DIRECTORY )
	EMSTAT	mode;
	EMCHAR  path[ NFILEN ];
	EMCHAR  *p;
	EMCHAR  *s;

	if( ffaccess( fname ) == FIOSUC )
		return( T );

	s = p = emstrcpy( path, fname );

	while( *s ) {
		if( *s == '\\' || *s == '/' )
			p = s;
		s++;
	}

	if( p != (EMCHAR *)&path[0] ) {
		*p = '\000';

		if( *(p - 1 ) == ':' )
			/* assumes root directory on MS-DOS */
			return( T );

		if( ffstat( path, &mode ) == 0 ) {
			if( !S_ISDIR( mode.st_mode ) ) {
				WDGwrite( ECSTR("Not a directory %s."), path );
				return( NIL );
			}
		} else  {
			WDGwrite( ECSTR("Directory %s not found."), path );
			return( NIL );
		}
	}

#endif  /* _DIRECTORY */

	return( T );

}

/*
 *      Return T if fname is a directory and NIL otherwise. Name as
 *      been normalized by the caller.
 */

int
ffdiredp( EMCHAR *fname )
{
#if     defined( _DIRECTORY )
	EMSTAT	mode;

	if( (fname[1] == ':') && !fname[2] )
		/* root drive on MS-DOS */
		return( T );

	if( (ffaccess( fname ) == FIOSUC ) && (ffstat( fname, &mode ) == 0) ) {
		if( S_ISDIR( mode.st_mode ) )
			return( T );
	}
#endif  /* _DIRECTORY */

	return( NIL );

}

#if	defined( _WINDOWS_SOURCE ) && !defined( _WIN32_WCE )
static	void
NTfullname( EMCHAR *rname, EMCHAR *fname )
{
#if	defined( _MSC_VER ) && ( _MSC_VER >= 1200 )
	LPTSTR	s;
	EMCHAR	tmp[ NFILEN ];

	if( GetFullPathName( (LPCTSTR)fname, NFILEN, tmp, &s ) == 0 ) {
		rname[ 0 ] = '\000';
		return;
	}

	if( GetLongPathName( tmp, rname, NFILEN ) == 0 ) {
		s = tmp;
		emstrcpy( rname, tmp );
	} else	{
		s = rname;
	}

	/*
	 * force device to uppercase:
	 */

	if( s && (emstrlen( s ) >= 2) && s[1] == ':' ) {
		if( isalpha( (int)s[0] ) && islower( (int)s[0] ) )
			s[0] = (TCHAR)toupper( (int)s[0] );
	}
#else
	WIN32_FIND_DATA	data;
	HANDLE		hFind;
	LPTSTR		s;
	EMCHAR		tmp[ NFILEN ];

	if( GetFullPathName( (LPCTSTR)fname, NFILEN, tmp, &s ) == 0 ) {
		rname[ 0 ] = '\000';
		return;
	}

	/*
	 *	fill name with correct case.
	 */

	if( (hFind = FindFirstFile( tmp, &data )) != INVALID_HANDLE_VALUE ) {
		(void)emstrcpy( rname, tmp );
		(void)emstrcpy( s, data.cFileName );
		FindClose( hFind );
	}
#endif
}
#endif

#if     defined( _OS2V2 ) && defined( _MISSING_STAT )
#define INCL_DOSFILEMGR
#define INCL_DOSMISC
#define INCL_ERRORS

static  time_t          GetTime( FDATE *os2date, FTIME *os2time );
static  unsigned short  GetAttrib( EMCHAR *Name, USHORT Attr );
static  int             GetDev( EMCHAR *PathName, dev_t *dev_num );

extern  int             OS2stat( EMCHAR *pathname, EMSTAT *buffer );

#define FIRST_YEAR      80

static  time_t
GetTime( FDATE *osdate, FTIME *ostime )
{
	static  struct  tm      tm_buf;

	tm_buf.tm_sec   = ostime->twosecs;
	tm_buf.tm_min   = ostime->minutes;
	tm_buf.tm_hour  = ostime->hours;
	tm_buf.tm_mday  = osdate->day;
	tm_buf.tm_mon   = osdate->month - 1;
	tm_buf.tm_year  = osdate->year + FIRST_YEAR;

	return( mktime( &tm_buf ) );
}

/*
 *      Translate file modes from an OS2 syntax into an UNIX syntax.
 *
 *      Possible modes in OS2:
 *              FILE_NORMAL             normal file;
 *              FILE_READONLY           read only file;
 *              FILE_HIDDEN             hidden file;
 *              FILE_SYSTEM             system file;
 *              FILE_DIRECTORY          directory;
 *              FILE_ARCHIVED           archived file;
 *
 *      Possible modes in UNIX:
 *              S_IFDIR                 directory;
 *              S_IFCHR                 character special file;
 *              S_IFREG                 regular file;
 *              S_IREAD                 read permission, owner;
 *              S_IWRITE                write permission, owner;
 *              S_IEXEC                 execute/serach permission, owner;
 *
 */

static  unsigned short
GetAttrib( EMCHAR *name, USHORT attr )
{
	int             pos     = emstrlen( name ) - 4;
	unsigned short  mode    = S_IFREG       |
				  S_IREAD       | S_IWRITE      | /* owner */
				 (S_IREAD >> 3) |(S_IWRITE >> 3)| /* group */
				 (S_IREAD >> 6) |(S_IWRITE >> 6); /* other */

	if( attr & FILE_READONLY )
		mode &= (~S_IWRITE & ~(S_IWRITE  >> 3) & ~(S_IWRITE  >> 6));
	if( attr & FILE_DIRECTORY ) {
		mode |= S_IFDIR;
		mode &= ~S_IFREG;
		mode |= (S_IEXEC | (S_IEXEC >> 3) | (S_IEXEC >> 6));
	}
	if( !emstrcmp( &name[pos], ECSTR(".cmd") ) ||
	    !emstrcmp( &name[pos], ECSTR(".com") ) ||
	    !emstrcmp( &name[pos], ECSTR(".exe") ) )
		mode |= (S_IEXEC | (S_IEXEC >> 3) | (S_IEXEC >> 6));

	return( mode );
}

/*
 *      Get the device number (0:A, 1:B, 2:C, ...).
 */

static  int
GetDev( EMCHAR *fname, dev_t *dev_num )
{
	if( fname[1] == ':' )
		*dev_num = *fname - (EMCHAR)(isupper( *fname ) ? 'A' : 'a');
	else    {
		ULONG   Drive;
		ULONG   Map;

		if( DosQueryCurrentDisk( &Drive, &Map ) )
			return( -1 );
		*dev_num = (dev_t)(Drive - 1);
	}

	return( 0 );
}

int
OS2stat( EMCHAR *fname, EMSTAT *mode )
{
	FILESTATUS buf;

	if( DosQueryPathInfo( fname, FIL_STANDARD, &buf, sizeof( buf ) ) )
		return( -1 );

	if( GetDev( fname, &mode->st_dev ) )
		return( -1 );

	mode->st_ino   = 0;
	mode->st_mode  = GetAttrib( fname, buf.attrFile );
	mode->st_nlink = 1;
	mode->st_uid   = 0;
	mode->st_gid   = 0;
	mode->st_rdev  = mode->st_dev;
	mode->st_size  = ((mode->st_mode&S_IFMT) == S_IFDIR) ? 10 : buf.cbFile;
	mode->st_atime = GetTime( &buf.fdateLastAccess, &buf.ftimeLastAccess );
	mode->st_mtime = GetTime( &buf.fdateLastWrite,  &buf.ftimeLastWrite  );
	mode->st_ctime = GetTime( &buf.fdateCreation,   &buf.ftimeCreation   );

	return( 0 );
}

#endif

