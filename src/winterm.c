#if	!defined( lint )
static	char rcsid[] = "$Id: winterm.c,v 1.5 2008/04/10 05:29:26 jullien Exp $";
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
 *	winterm.c : The routines in this file provide support for WINDOWS.
 */

#define	NOCOMM		/* No COMM driver routines (for Warnings) */
#define	STRICT		/* Strict type checking for Windows & NT  */

#define	NO_TRACKING_2

#define	WIN32S_FLAG	0x80000000L	/* hight bit set if TRUE */

#if	defined( _WIN32 ) || defined( _WIN64 )
#define	_WINDOWS_SOURCE
#endif

#include	<windows.h>
#include	<commdlg.h>
#include	<shellapi.h>
#if	!defined( _WINDOWS_SOURCE )
#include	<print.h>
#endif
#include	<string.h>
#include	<stdarg.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	"emacs.h"
#include	"winterm.h"
#if	defined( _CTRL3D )
#include	"ctl3d.h"
#endif

#if	!defined( WM_MOUSEWHEEL )
#define WM_MOUSEWHEEL                   0x020A
#endif

#if	defined( _WIN64 )
#define	HELPBUF_TYPE	ULONG_PTR
#else
#define	HELPBUF_TYPE	DWORD
#endif

static	int	use_widgets;		/* Use Graphic Widgets flag	*/

static	void	winopen( void );
static	void	winclose( void );
static	int	wingetc( void );
static	void	winputc( int c );
static	void	winputs( char *szText, int nSize );
static	void	winmove( int nRow, int nCol );
static	void	wineeol( void );
static	void	wineeop( void );
static	void	winflush( void );
static	void	winbeep( void );
static	void	winei( void );
static	void	winsi( void );
static	void	wincshow( int f );
static	int	wincheck( void );
static	void	winrawmode( void );
static	void	winmousebottom( void );

/*
 *	Standard terminal interface dispatch table.
 */

TERM	term = {
	0,
	0,
	0,
	NIL,
	winopen,
	winclose,
	wingetc,
	winputc,
	winputs,
	winflush,
	winmove,
	wineeol,
	wineeop,
	winbeep,
	winsi,
	winei,
	wincshow,
	wincheck,
	winrawmode
};

static	HWND	wintoolbar( HWND hBar, char *szLabel, LONG nActivate );
static	void	wincharsize( void );
static	int	winversion( void );
static	void	winnomem( void );
static	void	wincutcopy( WPARAM wParam );
static	void	winclippaste( void );
static	void	winclipcopy( void );
static	CMD	winyesno( char *s );
static	CMD	winconfirm( char *s );
static	void	winerror( char *s );
static	char *	wintitle( char *buffer, char *fname );
static	CMD	winasker( char *prompt, char *buf, int nbuf );
static	CMD	winedit(  char *prompt, char *buf, int nbuf );
static	int	winchange( char *os, char *ns, char *op, char *np, int n );
static	void	winplay( int flag );
static	void	winwait( void );
static	void	winmessage( char *msg );
static	void	winwrite( char *fmt, ... );
static	void	winparams( void );
static	void	winbuttonbar( void );
static	void	winsize( int fCharSize );
static	void	winadjust( void );
static	void	winupdate( char *prompt, char *line );
static	CMD 	winwarning( char *s );

static	void	wingetparm( void );
static	void	winsetparm( void );
static	void	windrawbox( HDC hDC, RECT *rcRect );
static	void	winshowrow( int	f );
static	void	winshowcol( int f );
static	void	winshowstate( WORD state, char *key, RECT *rcRect );

extern	int	system( const char *s );

#if	defined( NO_TRACKING )
static	void	wintrack( void );
#endif

#if	defined( _WINDOWS_SOURCE )
extern	int	system32( const char *s );
#endif

extern	long	current_row;
extern	long	current_col;

static	struct	{
	int	x;
	int	y;
	int	w;
	int	h;
} IniPos = { 0, 0, 0, 0 };

#if	defined( _WINDOWS_SOURCE )
static	short	nWin32 = 1;
#define	EditBufOffset(w) (GetClassLong( w, GCL_CBWNDEXTRA ) -  4)
#define	EditPosOffset(w) (GetClassLong( w, GCL_CBWNDEXTRA ) -  8)
#define	EditQuoOffset(w) (GetClassLong( w, GCL_CBWNDEXTRA ) - 12)
#if	!defined( MAKELPARAM )
#define	MAKELPARAM( x, y )	(int)(((y)<<16) + (x))
#endif
#else
static	short	nWin32 = 0;
#define	EditBufOffset(w) (GetClassWord( w, GCW_CBWNDEXTRA ) -  4)
#define	EditPosOffset(w) (GetClassWord( w, GCW_CBWNDEXTRA ) -  8)
#define	EditQuoOffset(w) (GetClassLong( w, GCL_CBWNDEXTRA ) - 12)
#endif

#define	MODELINE	0
#define	MAXLASTFILES	8
#define	CREATE_OWN_FONT	1
#define	LINENBSIZE	8

static	HINSTANCE hInst;
static	HACCEL	  hAccTable;
static	HANDLE	  hPrev;
static	HWND	  hWnd;
static	HWND	  hDlgSignOn;
static	HWND	  hBar;
static	HWND	  hBTOpen;
static	HWND	  hBTSave;
static	HWND	  hBTFind;
static	HWND	  hBTReplace;
static	HWND	  hBTRecord;
static	HWND	  hBTPlay;
static	HWND	  hBTMake;
static	HWND	  hBTQuit;
static	HWND	  hEdWnd;
static	DWORD	  BackColor;
static	DWORD	  TextColor;
static	HBRUSH	  hBrush;
static	HBRUSH	  hInitBrush;
static	HFONT	  hFont;
static	HFONT	  hInitFont;
static	HCURSOR	  hCurWait;
static	HDC	  hDC;
#if	defined( _WIN64 )
static	UINT_PTR  AlarmID = 0;
#else
static	UINT	  AlarmID = 0;
#endif
static	int	  nOpen = 0;
static	int	  nDrawX;
static	int	  nDrawY;
static	int	  nCmd;
static	int	  nCharWidth;
static	int	  nCharHeight;
static	int	  nLeading;
static	int	  nParentWidth;
static	int	  nParentHeight;
static	int	  nBarSize;
static	int	  nEditPos;
static	int	  nEditOld;
static	int	  winstrokes;
static	WNDPROC	  EditWndProc;
static	FARPROC	  lpProcSignOn;
static	int	  xByteOffset;
static	char	  szConfig[ 128 ];
static	char	  szLastFile[MAXLASTFILES][NFILEN];
static	char	  szHelpFile1[NFILEN] = "";
static	char	  szHelpFile2[NFILEN] = "";
static	DWORD	  nlfFlag     = FALSE;
static	UINT	  nEditFlag   = FALSE;
static	UINT	  nVisibleBar = TRUE;
static	UINT	  nMaximized  = FALSE;
static	UINT	  nZoomed     = FALSE;
static	UINT	  nGetCode    = FALSE;

static	HWND	  fBar;		  /* Handle of FeetBar			    */
static	int	  fBarSize;	  /* Size of FeetBar			    */
static	RECT	  RCEditX;	  /* Position and size of X in FBar	    */
static	RECT	  RCEditY;	  /* Position and size of Y in FBar	    */
static	RECT	  RCRepMode;	  /* Position and size of repmode	    */
static	RECT	  RCCapsMode;	  /* Position and size of caps lock mode    */
static	RECT	  RCNumLMode;	  /* Position and size of num lock mode	    */
static	RECT	  RCMsgBox;	  /* Position and size of message box	    */
static	int	  nEditWidth;	  /* Width of X and Y rect edit in FeetBar  */
static	int	  nEditHeight;	  /* Height of X and Y rect edit in FeetBar */
static	int	  nRepModeWidth;  /* Replace mode width			    */
static	int	  nCapsModeWidth; /* Caps mode width			    */
static	int	  nNumLModeWidth; /* NumLock mode width			    */
static	HFONT	  hfFont;	  /* Font used for FeetBar		    */
static	LOGFONT	  lfFBar;	  /* LOGFONT structure in FeetBar	    */

#define	COLOR_BLACK	RGB(   0,   0,   0 )
#if	defined( SMALL_COLORS )
#define	COLOR_BLUE	RGB(   0,   0, 255 )
#else
#define	COLOR_BLUE	RGB(   0,   0, 128 )
#endif
#define	COLOR_GREEN	RGB(   0, 255,   0 )
#define	COLOR_CYAN	RGB(   0, 255, 255 )
#define	COLOR_RED	RGB( 255,   0,   0 )
#define	COLOR_MAGENTA	RGB( 255,   0, 255 )
#define	COLOR_YELLOW 	RGB( 255, 255,   0 )
#define	COLOR_WHITE	RGB( 255, 255, 255 )

#define	COLOR_GRAY	RGB( 128, 128, 128 )
#define	COLOR_LIGHTGRAY	RGB( 192, 192, 192 )

static	COLORREF colortable[] = {
	COLOR_BLACK,
	COLOR_BLUE,
	COLOR_GREEN,
	COLOR_CYAN,
	COLOR_RED,
	COLOR_MAGENTA,
	COLOR_YELLOW,
	COLOR_WHITE
};

/*
 *	new variables for common dialogs
 */

typedef UINT	(CALLBACK *LPFNHOOK)( HWND, UINT, WPARAM, LPARAM );

static	OPENFILENAME	ofn;
static	char		szFilterSpec[] =
				"C Files (*.C)\0*.C;*.H\0"
				"C++ Files (*.CPP, *.CC)\0*.CPP;*.CC;*.H;*.HPP\0"
				"Lisp Files (*.L*)\0*.L*\0"
				"Assembler Files (*.ASM*)\0*.ASM*\0"
				"All Files (*.*)\0*.*\0";
static	char		szFileName[ NFILEN ];
static	char		szFileTitle[ NFILEN ];

static	FINDREPLACE	frpl;
static	char		szFind[ NFILEN ];
static	char		szReplace[ NFILEN ];

static	UINT		wFRMsg;

/*
 *	Fonts
 */

static	CHOOSEFONT	FontChunk;
static	LOGFONT		lf;

static	int FAR	PASCAL	wingetfont( void );

/*
 *	Colors
 */

static	CHOOSECOLOR	ColorChunk;
static	void FAR PASCAL wingetcolor( void );

/*
 *	Printer
 */

static	BOOL		bAbort;
static	HWND		hAbortDlgWnd;
static	PRINTDLG	pd;
static	HDC		GetPrinterDC( void );
static	void		winprint( void );
static	void		winprintsetup( void );

int
system( const char *s )
{
	HCURSOR		hCurOld;
	char		cmd[ 512 ];
	char		*slwr;
	int		internal;
	int		i;
	int		res;
	static	char	*szDosCmd[] = {
		"cd",     "cls",    "copy",   "date",
		"del",    "erase",  "dir",    "echo",
		"for",    "if",     "md",     "mkdir",
		"pause",  "ren",    "rd",     "rmdir",
		"set",    "time",   "type",   "ver",
		"vol",    ">",      "<",      "|",
		NULL
	};

	internal = FALSE;
	for( slwr = (char *)s ; *slwr && !isspace( *slwr ) ; slwr++ )
		if( isupper( *slwr ) )
			*slwr = (char)tolower( *slwr );

	slwr = (char *)s;

	for( i = 0 ; szDosCmd[ i ] ; i++ )
		if( strncmp( s, szDosCmd[i], strlen( szDosCmd[i] ) ) == 0 ) {
			internal = TRUE;
			break;
		}

	if( internal ) {
		strcpy( cmd, getenv( "COMSPEC" ) );
		strcat( cmd, " /c " );
		strcat( cmd, slwr );
	} else	strcpy( cmd, slwr );

	hCurOld = SetCursor( hCurWait );
	ShowCursor( TRUE );

#if	!defined( _WINDOWS_SOURCE )
	res = (WinExec( (char *)cmd, SW_SHOWNORMAL ) > 31);
#else
	if( GetVersion() & WIN32S_FLAG )
		res = (WinExec( (char *)cmd, SW_SHOWNORMAL ) > 31);
	else	res = system32( (char *)slwr );
#endif

	ShowCursor( FALSE );
	SetCursor( hCurOld );

	return( res );
}

#if	defined( _WINDOWS_SOURCE )
VOID
ErrorExit( LPTSTR lpszMessage )
{
	MessageBox( NULL, lpszMessage, version, MB_OK );
	ExitProcess( 0 );
}

void
showlines( char *chBuf, int nLen )
{
	char	*s = chBuf;
	char	c;

	while( *s && nLen-- > 0 )
		switch( (c = *s++) ) {
		case '\r' :
			update( T );
			break;
		case '\n':
			update( T );
			(void)newline();
			break;
		default:
			(void)linsert( 1, c );
		}

}

int
system32( const char *cmd )
{
	SECURITY_ATTRIBUTES saAttr;
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO	    siStartInfo;
	HANDLE		    hChildStdoutRd;
	HANDLE		    hChildStdoutWr;
	HANDLE		    hOutputFile;
	DWORD		    dwRead;
	CHAR		    chBuf[ NLINE ];
	CHAR		    buf[ NFILEN ];
	BOOL		    bSuccess;
	DWORD		    nRetCode = TRUE;
#if	defined( _GET_EXIT_CODE )
	DWORD		    lpdwExitCode;
#endif

	saAttr.nLength		    = sizeof( SECURITY_ATTRIBUTES );
	saAttr.bInheritHandle	    = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	/* Set up members of STARTUPINFO structure. */

	if( pipe_process == T ) {
		if( !CreatePipe( &hChildStdoutRd, &hChildStdoutWr, &saAttr, 0) )
			ErrorExit( "Stdout pipe creation failed\n" );
		hOutputFile = hChildStdoutWr;
	} else	{
		hOutputFile = CreateFile(
					  "process.tmp",
					  GENERIC_WRITE,
					  0,
					  &saAttr,
					  CREATE_ALWAYS,
					  0,
					  NULL
					);

		if( hOutputFile == INVALID_HANDLE_VALUE )
			ErrorExit( "Can't open output file\n" );
	}

	siStartInfo.cb		= sizeof( STARTUPINFO );
	siStartInfo.lpReserved	= NULL;
	siStartInfo.lpReserved2	= NULL;
	siStartInfo.cbReserved2	= 0;
	siStartInfo.lpDesktop	= NULL;
	siStartInfo.lpTitle	= (char *)cmd;
	siStartInfo.wShowWindow	= SW_HIDE;
	siStartInfo.dwFlags	= STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
	siStartInfo.hStdOutput	= hOutputFile;
	siStartInfo.hStdError	= hOutputFile;
	siStartInfo.hStdInput	= NULL;

	/*
	 * Create the child process.
	 */

	strcpy( buf, getenv( "COMSPEC" ) );
	strcat( buf, " /c " );
	strcat( buf, cmd );

	bSuccess = CreateProcess(
			NULL,
			(char *)buf,   /* command line                       */	
			&saAttr,       /* process security attributes        */
			&saAttr,       /* primary thread security attributes */
			TRUE,          /* handles are inherited              */
			0,             /* creation flags                     */
			NULL,          /* use parent's environment           */
			NULL,          /* use parent's current directory     */
			&siStartInfo,  /* STARTUPINFO pointer                */
			&piProcInfo    /* receives PROCESS_INFORMATION       */
		   );

	if( bSuccess != TRUE ) {
		CloseHandle( hOutputFile );
		return( FALSE );
	}

	/*
	 * Change the priority from IDLE to NORMAL
	 */

	SetPriorityClass( piProcInfo.hProcess, NORMAL_PRIORITY_CLASS );

	/* 
	 * Close the write end of the pipe before reading from the read end 
	 */

	if( pipe_process == T ) {
		if( !CloseHandle( hChildStdoutWr ) )
			ErrorExit( "Closing handle failed" );

		process_pending		= T;
		for( ;; ) {
			dwRead = 0;
			if( wincheck() != TRUE ) {
				if( !TerminateProcess(piProcInfo.hProcess,0) )
					winerror( "TerminateProcess fails" );
				nRetCode = FALSE;
				break;
			}
			bSuccess = ReadFile(
					     hChildStdoutRd,
					     chBuf,
					     NLINE,
					     &dwRead,
					     NULL
					   );
			
			if( dwRead > 0 )
				showlines( chBuf, dwRead );

			if( bSuccess == FALSE || dwRead == 0 ) {
				dwRead = GetLastError();
				break;
			}
		}
		process_pending = NIL;
		update( T );
	} else	{
#if	defined( _GET_EXIT_CODE )
		lpdwExitCode = STILL_ACTIVE;
		while( lpdwExitCode == STILL_ACTIVE && bSuccess == TRUE ) {
			bSuccess = GetExitCodeProcess(
						       piProcInfo.hProcess,
						       &lpdwExitCode
						     );
			Sleep( 500 );
		}
#else
		WaitForSingleObject( piProcInfo.hProcess, INFINITE );
		CloseHandle( hOutputFile );
#endif
	}

	return( nRetCode );
}
#endif

/*
 *	Process message while waiting.
 */

static	void
winwait( void )
{
	MSG	msg;

	PeekMessage( &msg, hWnd, 0, 0, PM_NOREMOVE );

}

/*
 *	Formatted string on mode-line
 */

static	void
winwrite( char *fmt, ... )
{
	char	buf[ NLINE ];
	va_list	marker;
	va_start( marker, fmt );

	if( strcmp( fmt, "%L" ) == 0 ) {
		EDLINE *lp = (EDLINE *)va_arg( marker, EDLINE * );

		if( llength( lp ) < (NLINE-1) ) {
			(void)strncpy( buf, ltext( lp ), llength( lp ) );
			buf[ llength( lp ) ] = '\000';
		} else	{
			(void)strncpy( buf, ltext( lp ), NLINE-1 );
			buf[ NLINE-1 ] = '\000';
		}
	} else	vsprintf( buf, fmt, marker );

	va_end( marker );

	winmessage( buf );
}

/*
 *	Message on mode-line
 */

static	void
winmessage( char *msg )
{
	HDC	hfDC;
	HBRUSH	hBrush;
	HBRUSH	hOldBrush;
	HFONT	hOldFont;
	RECT	RCClr;

	mpresf = T;

	RCClr.top    = RCMsgBox.top + 1;
	RCClr.bottom = RCMsgBox.bottom;
	RCClr.right  = RCMsgBox.right - 1;
	RCClr.left   = RCMsgBox.left + 2;

	hfDC	  = GetDC( fBar );

	hBrush    = CreateSolidBrush( COLOR_LIGHTGRAY );
	hOldBrush = (HBRUSH)SelectObject( hfDC, hBrush );
	FillRect( hfDC, (LPRECT)&RCClr, hBrush );
	(void)SelectObject( hfDC, hOldBrush );
	DeleteObject( hBrush );

	hOldFont = (HFONT)SelectObject( hfDC, hfFont );
	SetBkColor( hfDC, COLOR_LIGHTGRAY );
	SetTextColor( hfDC, COLOR_BLACK );

	if( *msg == ':' )
		msg += 2;

	DrawText(
		  hfDC,
		  msg,
		  -1,
		  &RCClr,
		  DT_LEFT | DT_VCENTER | DT_SINGLELINE
		);
	(void)SelectObject( hfDC, hOldFont );

	ReleaseDC( fBar, hfDC );
}

static	void
wingetparm()
{
	FILE	*fd;

	/*
	 *	readin <WINDOWS-PATH>\emacs.ini
	 */

	if( GetModuleFileName( hInst, szConfig, sizeof( szConfig ) ) ) {
		char	*p;
		char	*s;

		for( s = p = (char *)&szConfig[ 0 ] ; *s ; s++ )
			if( *s == '\\' || *s == '/' )
				p = s;
		*(p+1) = '\000';
	}

	(void)strcat( szConfig, "emacs.ini" );

	if( (fd = ffopen( szConfig, ECSTR("r"), NULL )) != NULL ) {
		short	nVer;
		int	i;
		(void)fread( &nVer, sizeof( short ), 1, fd );
		if( nVer == nWin32 ) {
			(void)fread( &BackColor,     sizeof( DWORD ),   1, fd );
			(void)fread( &TextColor,     sizeof( DWORD ),   1, fd );
			(void)fread( &nlfFlag,       sizeof( DWORD ),   1, fd );
			(void)fread( &lf,            sizeof( LOGFONT ), 1, fd );
			(void)fread( &nVisibleBar,   sizeof( UINT ),    1, fd );
			(void)fread( &nMaximized,    sizeof( UINT ),    1, fd );
			(void)fread( &gnu_compatible,sizeof( UINT ),    1, fd );
			(void)fread( &binary_mode,   sizeof( UINT ),    1, fd );
			(void)fread( &backup_before_writing,   sizeof( UINT ),    1, fd );
			(void)fread( &nZoomed,       sizeof( UINT ),    1, fd );
			(void)fread( &IniPos,        sizeof( IniPos ),  1, fd );
			for( i = 0 ; i < MAXLASTFILES ; i++ )
				(void)fread( &szLastFile[i][0], NFILEN, 1, fd );
			(void)fread( &szHelpFile1[0], NFILEN, 1, fd );
			(void)fread( &szHelpFile2[0], NFILEN, 1, fd );
			(void)fclose( fd );

			DeleteObject( hBrush );
			hBrush = CreateSolidBrush( BackColor );
#if	defined( _WIN64 )
			SetClassLongPtr(hWnd,GCLP_HBRBACKGROUND,(LONG_PTR)hBrush );
#elif	defined( _WIN32 )
			SetClassLong( hWnd, GCL_HBRBACKGROUND, (LONG)hBrush );
#else
			SetClassWord( hWnd, GCW_HBRBACKGROUND, (WORD)hBrush );
#endif
			if( nlfFlag == CF_INITTOLOGFONTSTRUCT )
				hFont = CreateFontIndirect( &lf );

		} else	(void)fclose( fd );
	}

}

static	void
winsetparm()
{
	FILE	*fd;
	RECT	rcClient;

	GetWindowRect( hWnd, (LPRECT)&rcClient );

	IniPos.x = rcClient.left;
	IniPos.y = rcClient.top;
	IniPos.w = rcClient.right  - rcClient.left;
	IniPos.h = rcClient.bottom - rcClient.top;

	if( !IsIconic( hWnd )
	    && (fd = ffopen(szConfig, ECSTR("w"), NULL)) != NULL ) {
		int	i;

		nZoomed = (UINT)IsZoomed( hWnd );

		(void)fwrite( &nWin32,	      sizeof( short ),   1, fd );
		(void)fwrite( &BackColor,     sizeof( DWORD ),   1, fd );
		(void)fwrite( &TextColor,     sizeof( DWORD ),   1, fd );
		(void)fwrite( &nlfFlag,       sizeof( DWORD ),   1, fd );
		(void)fwrite( &lf,            sizeof( LOGFONT ), 1, fd );
		(void)fwrite( &nVisibleBar,   sizeof( UINT ),    1, fd );
		(void)fwrite( &nMaximized,    sizeof( UINT ),    1, fd );
		(void)fwrite( &gnu_compatible,sizeof( UINT ),    1, fd );
		(void)fwrite( &binary_mode,   sizeof( UINT ),    1, fd );
		(void)fwrite( &backup_before_writing, sizeof( UINT ), 1, fd );
		(void)fwrite( &nZoomed,       sizeof( UINT ),    1, fd );
		(void)fwrite( &IniPos,        sizeof( IniPos ),  1, fd );
		for( i = 0 ; i < MAXLASTFILES ; i++ )
			(void)fwrite( &szLastFile[ i ][ 0 ], NFILEN, 1, fd );
		(void)fwrite( &szHelpFile1[ 0 ], NFILEN, 1, fd );
		(void)fwrite( &szHelpFile2[ 0 ], NFILEN, 1, fd );
		(void)fclose( fd );
	}
}	

#if	defined( _WINDOWS_SOURCE )
static	FARPROC lpCtl3dRegister;
static	FARPROC lpCtl3dAutoSubclass;
static	FARPROC lpCtl3dUnregister;
static	FARPROC lpCtl3dDlgFramePaint;
#else
static	void (CALLBACK *lpCtl3dRegister)();
static	void (CALLBACK *lpCtl3dAutoSubclass)();
static	void (CALLBACK *lpCtl3dUnregister)();
static	void (CALLBACK *lpCtl3dDlgFramePaint)();
#endif

static	int	nCtl3D;

static	void
winopen( void )
{
	RECT		rcClient;
	WNDCLASS	wc;
	BYTE		bKeyState[ 256 ];
	HMODULE		hLib3D;
	DWORD		dwVersion;
	
	dwVersion = GetVersion();

	if( dwVersion > WIN32S_FLAG && LOBYTE( LOWORD( dwVersion ) ) >= 4 )
		hLib3D = (HMODULE)0;	/* no Ctrl3D with Windows95 */
	else	{
#if	defined( _WINDOWS_SOURCE )
		hLib3D = LoadLibrary( "ctl3d32.dll" );
#else
		hLib3D = LoadLibrary( "ctl3dv2.dll" );
		if( hLib3D < (HMODULE)32 )
			hLib3D = LoadLibrary( "ctl3d.dll" );
#endif
	}

	nCtl3D = FALSE;

	if( hLib3D >= (HMODULE)32 ) {
	    lpCtl3dRegister     = GetProcAddress(hLib3D, "Ctl3dRegister"     );
	    lpCtl3dAutoSubclass = GetProcAddress(hLib3D, "Ctl3dAutoSubclass" );
	    lpCtl3dDlgFramePaint= GetProcAddress(hLib3D, "Ctl3dDlgFramePaint");
	    lpCtl3dUnregister   = GetProcAddress(hLib3D, "Ctl3dUnregister"   );
	    nCtl3D		= TRUE;
	}

#if	defined( _CTRL3D )
	if( nCtl3D ) {
		lpCtl3dRegister( hInst );
		lpCtl3dAutoSubclass( hInst );
	}
#endif

	/*
	 *	Set the Insert key of the keyboard to untoggle
	 */

	GetKeyboardState( (LPBYTE) &bKeyState );
	bKeyState[ VK_INSERT ] &= ~(-1);
	SetKeyboardState( (LPBYTE) &bKeyState );

	if( monochrome_monitor != T ) {
		BackColor = colortable[ background_color ];
		TextColor = colortable[ foreground_color ];
	} else	{
		BackColor = GetSysColor( COLOR_WINDOW     );
		TextColor = GetSysColor( COLOR_WINDOWTEXT );
	}

	hBrush	 = CreateSolidBrush( BackColor );
	hCurWait = (HCURSOR)LoadCursor( NULL, IDC_WAIT );
	hFont	 = (HFONT)GetStockObject( SYSTEM_FIXED_FONT );
	nBarSize = GetSystemMetrics( SM_CYMENU );
	fBarSize = GetSystemMetrics( SM_CYMENU ) + 4;

	nEditWidth	= 60;
	nEditHeight	= fBarSize - 6;
	nRepModeWidth	= 40;
	nCapsModeWidth	= 40;
	nNumLModeWidth	= 40;

	if( hPrev == NULL ) {
		wc.style	 = CS_CLASSDC | CS_DBLCLKS | CS_HREDRAW | CS_VREDRAW;
		wc.lpfnWndProc	 = (WNDPROC)MainWndProc;
		wc.cbClsExtra	 = 0;
		wc.cbWndExtra	 = 0;
		wc.hInstance	 = hInst;
		wc.hIcon	 = LoadIcon(hInst,MAKEINTRESOURCE(EMACSICON));
		wc.hCursor	 = LoadCursor( (HINSTANCE)0, IDC_ARROW );
		wc.hbrBackground = hBrush;
		wc.lpszMenuName  = "EmacsMenu";
		wc.lpszClassName = "EmacsWClass";

		if( RegisterClass( &wc ) == 0 )
			return;

		wc.style	 = CS_DBLCLKS;
		wc.lpfnWndProc	 = (WNDPROC)BarWndProc;
		wc.cbClsExtra	 = 0;
		wc.cbWndExtra	 = 0;
		wc.hInstance	 = hInst;
		wc.hIcon	 = (HICON)0;
		wc.hCursor	 = (HCURSOR)0;
		wc.hbrBackground = hBrush;
		wc.lpszMenuName  = (LPSTR)NULL;
		wc.lpszClassName = "BarWClass";

		if( RegisterClass( &wc ) == 0 )
			return;

		wc.style	 = 0;/* CS_DBLCLKS; */
		wc.lpfnWndProc	 = (WNDPROC)fBarWndProc;
		wc.cbClsExtra	 = 0;
		wc.cbWndExtra	 = 0;
		wc.hInstance	 = hInst;
		wc.hIcon	 = (HICON)0;
		wc.hCursor	 = (HCURSOR)0;
		wc.hbrBackground = hBrush;
		wc.lpszMenuName  = (LPSTR)NULL;
		wc.lpszClassName = "fBarWClass";

		if( RegisterClass( &wc ) == 0 )
			return;

		/*
		 *	Create a new Class derived from standard EditClass
		 */

		GetClassInfo( NULL, "EDIT", &wc );
		xByteOffset	 = wc.cbWndExtra;
		wc.cbWndExtra	+= sizeof( EMCHAR * );
		EditWndProc	 = wc.lpfnWndProc;
		wc.lpfnWndProc	 = (WNDPROC)EmacsEditWndProc;
		wc.hInstance	 = hInst;
		wc.lpszClassName = "EmacsEdit";
		wc.style	 = CS_GLOBALCLASS;

		if( RegisterClass( &wc ) == 0 )
			return;

	}

	if( winversion() >= 310 )
		hWnd = CreateWindowEx(
				WS_EX_ACCEPTFILES,
				"EmacsWClass",
				"EmACT",
				WS_OVERLAPPEDWINDOW | WS_VSCROLL,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				HWND_DESKTOP,
				(HMENU)0,
				hInst,
				NULL
			   );
	else	hWnd = CreateWindow(
				"EmacsWClass",
				"EmACT",
				WS_OVERLAPPEDWINDOW | WS_VSCROLL,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				CW_USEDEFAULT,
				HWND_DESKTOP,
				(HMENU)0,
				hInst,
				NULL
			   );

	if( hWnd == NULL )
		return;
	else	hDC = GetDC( hWnd );	/* OWN hDC for hWnd */

	/*
	 * read in the user preferences
	 */

	wingetparm();

	if( (IniPos.w > 0) && (nMaximized == FALSE) ) {
	    if( IniPos.h < 200 )
	    	IniPos.h = 200;
	    MoveWindow( hWnd, IniPos.x, IniPos.y, IniPos.w, IniPos.h, FALSE );
	}

	ShowWindow( hWnd, (nMaximized | nZoomed) ? SW_SHOWMAXIMIZED : nCmd );

	hInitBrush = (HBRUSH)SelectObject( hDC, hBrush );
	hInitFont  = (HFONT)SelectObject( hDC, hFont  );

	wincharsize();

	GetClientRect( hWnd, (LPRECT)&rcClient );
	if( nVisibleBar == TRUE )
		rcClient.top += nBarSize;
	rcClient.bottom -= fBarSize;
	nDrawX		= 0;
	nDrawY		= 0;
	nParentWidth    = (int)(rcClient.right  - rcClient.left);
	nParentHeight   = (int)(rcClient.bottom - rcClient.top);

	TTYncol		= (int)(nParentWidth  / nCharWidth);
	TTYnrow		= (int)(nParentHeight / nCharHeight) - MODELINE;
	TTYinit		= T;
	line_number_mode= T;

	hBar		= CreateWindow(
				"BarWClass",
				"EmACT",
				WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE,
				0,
				0,
				nParentWidth,
				nBarSize,
				(HWND)hWnd,
				(HMENU)0,
				(HINSTANCE)hInst,
				(LPVOID)0
			);

	hBTOpen		  = wintoolbar( hBar, "Open",	  0L 	      );
	hBTSave		  = wintoolbar( hBar, "Save",	  0L 	      );
	hBTFind		  = wintoolbar( hBar, "Find",	  0L	      );
	hBTReplace	  = wintoolbar( hBar, "Replace",  0L	      );
	hBTRecord	  = wintoolbar( hBar, "Record",	  0L	      );
	hBTPlay		  = wintoolbar( hBar, "Play",	  WS_DISABLED );
	hBTMake		  = wintoolbar( hBar, "Make",	  0L	      );
	hBTQuit		  = wintoolbar( hBar, "Quit",	  0L	      );

	RCEditX.left 	  = 5;
	RCEditX.top 	  = 2;
	RCEditX.right 	  = RCEditX.left + nEditWidth;
	RCEditX.bottom	  = RCEditX.top  + nEditHeight;

	RCEditY.left 	  = RCEditX.right + 5;
	RCEditY.top 	  = 2;
	RCEditY.right 	  = RCEditY.left + nEditWidth;
	RCEditY.bottom	  = RCEditY.top  + nEditHeight;

	RCRepMode.top	  = 2;
	RCRepMode.right	  = nParentWidth - 10;
	RCRepMode.left	  = RCRepMode.right - nRepModeWidth;
	RCRepMode.bottom  = RCRepMode.top + nEditHeight;

	RCCapsMode.top	  = 2;
	RCCapsMode.right  = RCRepMode.left   - 5;
	RCCapsMode.left	  = RCCapsMode.right - nCapsModeWidth;
	RCCapsMode.bottom = RCCapsMode.top   + nEditHeight;

	RCNumLMode.top	  = 2;
	RCNumLMode.right  = RCCapsMode.left  - 5;
	RCNumLMode.left	  = RCNumLMode.right - nNumLModeWidth;
	RCNumLMode.bottom = RCNumLMode.top   + nEditHeight;

	RCMsgBox.top	  = 2;
	RCMsgBox.left	  = RCEditY.right    + 10;
	RCMsgBox.right	  = RCNumLMode.left  - 10;
	RCMsgBox.bottom   = RCMsgBox.top     + nEditHeight;

	fBar		  = CreateWindow(
				"fBarWClass",
				"EmACT",
				WS_CHILD | WS_CLIPSIBLINGS |
				WS_VISIBLE | WS_BORDER,
				0,
				rcClient.bottom,
				nParentWidth,
				fBarSize,
				(HWND)hWnd,
				(HMENU)0,
				(HINSTANCE)hInst,
				(LPVOID)0
			  );

	if( nVisibleBar == FALSE )
		winbuttonbar();

	pd.lStructSize    = sizeof( PRINTDLG );
	pd.hwndOwner      = (HWND)NULL;
	pd.hDevMode       = NULL;
	pd.hDevNames      = NULL;
	pd.Flags          = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
	pd.nCopies        = 1;

	UpdateWindow( hWnd );

	if( use_widgets != T ) {
		widget.w_error	   = winerror;
		widget.w_yn        = winyesno;
		widget.w_yesno     = winyesno;
		widget.w_confirm   = winconfirm;
		widget.w_title     = wintitle;
		widget.w_asker     = winasker;
		widget.w_edit      = winedit;
		widget.w_change    = winchange;
		widget.w_play      = winplay;
		widget.w_wait      = winwait;
		widget.w_message   = winmessage;
		widget.w_write     = winwrite;
		widget.w_adjust    = winadjust;
		widget.w_update	   = winupdate;
		widget.w_clipcopy  = winclipcopy;
		widget.w_clippaste = winclippaste;
	} else	{
		widget.w_error     = winerror;
		widget.w_yn        = winyesno;
		widget.w_yesno     = winyesno;
		widget.w_confirm   = winconfirm;
		widget.w_title     = wintitle;
		widget.w_play      = winplay;
		widget.w_wait      = winwait;
		widget.w_message   = winmessage;
		widget.w_write     = winwrite;
		widget.w_adjust    = winadjust;
		widget.w_clipcopy  = winclipcopy;
		widget.w_clippaste = winclippaste;
	}

	printer.p_print	= winprint;

	if( eargc == 1 ) {
		lpProcSignOn = MakeProcInstance( (FARPROC)SignOnProc, hInst );
		hDlgSignOn   = CreateDialog(
					     hInst,
					     "SignOnDlg",
					     hWnd,
					     (DLGPROC)lpProcSignOn
					   );

		AlarmID	     = SetTimer(
					 hWnd,
					 (UINT)1,
					 (UINT)(1000*5),
					 (TIMERPROC)0
				       );

		if( hDlgSignOn ) {
			/*
			 * Show the dialog and then return activation
			 * back to the parent
			 */
			 ShowWindow( hDlgSignOn, SW_SHOWNORMAL );
			 SetActiveWindow( hWnd );
		}
	}

	nOpen = 1;

	/*
	 *	fill in non-variant fields of OPENFILENAME struct.
	 */

	ofn.lStructSize		= sizeof( OPENFILENAME );
	ofn.hwndOwner		= hWnd;
	ofn.lpstrFilter		= szFilterSpec;
	ofn.lpstrCustomFilter	= NULL;
	ofn.nMaxCustFilter	= 0;
	ofn.nFilterIndex	= 1;
	ofn.lpstrFile		= szFileName;
	ofn.nMaxFile		= NFILEN;
	ofn.lpstrInitialDir	= NULL;
	ofn.lpstrFileTitle	= szFileTitle;
	ofn.nMaxFileTitle	= NFILEN;
	ofn.lpstrTitle		= NULL;
	ofn.lpstrDefExt		= "";
	ofn.Flags		= 0;

	/*
	 *	fill in non-variant fields of FINDREPLACE struct.
	 */

	frpl.lStructSize	= sizeof( FINDREPLACE );
	frpl.hwndOwner		= hWnd;
	frpl.hInstance		= hInst;
	frpl.Flags		= FR_HIDEWHOLEWORD | FR_DOWN | FR_MATCHCASE;
	frpl.lpstrFindWhat	= szFind;
	frpl.wFindWhatLen	= NFILEN;
	frpl.lpstrReplaceWith	= szReplace;
	frpl.wReplaceWithLen	= NFILEN;
	frpl.lpfnHook		= NULL;
	frpl.lpTemplateName	= NULL;

	wFRMsg = (UINT)RegisterWindowMessage( (LPSTR)FINDMSGSTRING );

}

static	HWND
wintoolbar( HWND hBar, char *szLabel, LONG nActivate )
{
	static	UINT	nButtonNb = IDM_TOOL;

#if	defined( _WIN64 )
	HWND	hwnd = CreateWindow(
			     "Button",
			     szLabel,
			     BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | nActivate,
			     80 * (nButtonNb - IDM_TOOL),
			     0,
			     80,
			     nBarSize,
			     (HWND)hBar,
			     (HMENU)(SIZE_T)nButtonNb,
			     (HINSTANCE)hInst,
			     (LPVOID)0
			   );
#else
	HWND	hwnd = CreateWindow(
			     "Button",
			     szLabel,
			     BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | nActivate,
			     80 * (nButtonNb - IDM_TOOL),
			     0,
			     80,
			     nBarSize,
			     (HWND)hBar,
			     (HMENU)nButtonNb,
			     (HINSTANCE)hInst,
			     (LPVOID)0
			   );
#endif

	nButtonNb++;

	return( hwnd );

}

static	void
winclose( void )
{
	winsetparm();

	(void)SelectObject( hDC, hInitBrush );
	(void)SelectObject( hDC, hInitFont  );
	DeleteObject( hBrush );
	DeleteObject( hFont  );
#if	defined( CREATE_OWN_FONT )
	DeleteObject( hfFont  );
#endif
	ReleaseDC( hWnd, hDC );
	TTYinit = NIL;
}

static	void
winputs( char *szText, int nSize )
{
	TextOut( hDC, nDrawX, nDrawY, szText, nSize );
	nDrawX += nSize * nCharWidth;
}

static	void
winputc( int c )
{
	char	aChar = (char)c;

	switch( aChar ) {

	case VK_RETURN :
		nDrawX = 0;
		nDrawY += nCharHeight;
		if( nDrawY > nParentHeight ) {
			winmove( 0, 0 );
			wineeop();
		}
		break;

	case VK_BACK :
		if( nDrawX >= nCharWidth ) {
			aChar = ' ';
			nDrawX -= nCharWidth;
			TextOut( hDC, nDrawX, nDrawY, (char *)&aChar, 1 );
		}
		break;

	default   :
		TextOut( hDC, nDrawX, nDrawY, (char *)&aChar, 1 );
		nDrawX += nCharWidth;
		break;
	}

}

static	void
winmove( int nRow, int nCol )
{
	nDrawX = nCol * nCharWidth;
	nDrawY = nRow * nCharHeight + nBarSize;
	current_col = (long)nCol;
}

static	int
wingetc( void )
{
	MSG	msg;
	int	nRet;
	static	unsigned int nMouseX;
	static	unsigned int nMouseY;
#if	defined( NO_TRACKING )
	static	int	     tracking;
#endif

	(void)SetScrollPos( hWnd, SB_VERT, average, TRUE );

	wincshow( TRUE );
	while( GetMessage( &msg, (HWND)0, 0, 0 ) ) {
		/*
		 * Watch for all messages that are for the signon dialog
		 */

		if( hDlgSignOn ) {

			/*
			 * Is the dialog still up, then look for msgs
			 * that will cause the signon to go away
			 */

			switch( msg.message ) {
			case WM_LBUTTONDOWN:
		 	case WM_RBUTTONDOWN:
			case WM_NCLBUTTONDOWN:
			case WM_NCRBUTTONDOWN:
			case WM_KEYDOWN:
			case WM_SYSKEYDOWN:
			case WM_MOUSEACTIVATE:
			case WM_TIMER:
				/*
				 *	Kill the signon dialog
				 */
				 if( AlarmID )
				 	KillTimer( hWnd, AlarmID );
				 DestroyWindow( hDlgSignOn );
				 hDlgSignOn = (HWND)0;
				 AlarmID    = 0;
				 FreeProcInstance( lpProcSignOn );
				 continue;
			}

			if( IsDialogMessage( hDlgSignOn, &msg ) )
				/*
				 * Is for the dialog, then skip Trans/Disp
				 */
				continue;

		}

		if( TranslateAccelerator( msg.hwnd, hAccTable, &msg ) )
			continue;

		TranslateMessage( &msg );

		if( msg.hwnd != hWnd && nGetCode == FALSE ) {
			DispatchMessage( &msg );
			if( editflag == NIL ) {
				editflag = T;
				return( CTLX|Ctrl|'C' );
			}
			continue;
		}

		switch( msg.message ) {

		case WM_LBUTTONDOWN:
			wincshow( FALSE );
			mevent.x = LOWORD(msg.lParam) / nCharWidth;
			mevent.y = (HIWORD(msg.lParam)-nBarSize) / nCharHeight;
			mevent.button = MButton1;
			nMouseX = mevent.x;
			nMouseY = mevent.y;
			if( GetKeyState( VK_SHIFT ) < 0 )
				mevent.button |= SHIFTBUTTON;
			if( GetKeyState( VK_CONTROL ) < 0 )
				mevent.button |= CTRLBUTTON;
			return( MEVT );

#if	defined( NO_TRACKING )
			AlarmID = SetTimer(hWnd,1,(UINT)(1000/20),(TIMERPROC)0);
			SetCapture( hWnd );
			tracking = 0;
			return( MEVT );
#endif
		case WM_LBUTTONUP:
#if	defined( NO_TRACKING )
			if( AlarmID )
			 	KillTimer( hWnd, AlarmID );
			AlarmID = 0;
			ReleaseCapture();
			wincshow( TRUE );
#endif
			break;

		case WM_LBUTTONDBLCLK:
			wincshow( FALSE );
			mevent.x = LOWORD(msg.lParam) / nCharWidth;
			mevent.y = (HIWORD(msg.lParam)-nBarSize) / nCharHeight;
			mevent.button = MButton4;
			return( MEVT );

		case WM_RBUTTONDBLCLK:
			wincshow( FALSE );
			mevent.x = LOWORD(msg.lParam) / nCharWidth;
			mevent.y = (HIWORD(msg.lParam)-nBarSize) / nCharHeight;
			mevent.button = MButton3;
			return( MEVT );

		case WM_RBUTTONDOWN:
		case WM_MBUTTONDOWN:
			wincshow( FALSE );
			mevent.x = LOWORD(msg.lParam) / nCharWidth;
			mevent.y = (HIWORD(msg.lParam)-nBarSize) / nCharHeight;
			mevent.button = MButton2;
			if( GetKeyState( VK_CONTROL ) < 0 )
				mevent.button |= CTRLBUTTON;
			return( MEVT );

		case WM_MOUSEMOVE :
#if	defined( NO_TRACKING )
			if( msg.wParam != MK_LBUTTON )
				break;
			if( tracking == 0 ) {
				tracking = 1;
				setmark();
			}
			wintrack();
#endif
			break;

#if	defined( NO_TRACKING )
		case WM_TIMER :
			if( tracking == 0 ) {
				tracking = 1;
				setmark();
			}
			wintrack();
			break;
#endif

		case WM_NCMOUSEMOVE :
		case WM_KEYUP :
			break;

		case WM_CHAR :
			wincshow( FALSE );
			if( mouse_avoidance_mode == T &&
			    (++winstrokes % mouse_avoidance_nudge) == 0 )
				/*
				 * every 5 strokes, move
				 * mouse to the bottom
				 */
				winmousebottom();
			if(((int)msg.wParam)==' ' && GetKeyState(VK_CONTROL)<0)
				/* CTRL-SPACE: it's a mark set */
				return( 0 );
			return( (int)msg.wParam );

		case WM_SYSCHAR :
			nRet = 0;
			switch( msg.wParam ) {
			case 'c': nRet = Ctrl|'^'; break;
			case 'C': nRet = Ctrl|'^'; break;
			}
			if( nRet ) {
				wincshow( FALSE );
				return( nRet );
			}
			wincshow( FALSE );
			DispatchMessage( &msg );
			wincshow( TRUE );
			if( editflag == NIL ) {
				editflag = T;
				return( CTLX|Ctrl|'C' );
			}
			break;

		case WM_KEYDOWN :
			nRet = 0;
			switch( msg.wParam ) {
			case VK_F1	:
				WinHelp(msg.hwnd,"emacs.hlp",HELP_INDEX,0L);
				break;
			case VK_F11	:
				if( szHelpFile1[ 0 ] ) {
					char	buf[ NPAT ];
					if( wordatcursor( buf, NPAT ) != T )
						WinHelp(
						         msg.hwnd,
							 szHelpFile1,
							 HELP_INDEX,
							 0L
						       );
					else	WinHelp(
							 msg.hwnd,
							 szHelpFile1,
							 HELP_PARTIALKEY,
							 (HELPBUF_TYPE)buf
						       );
				}
				break;
			case VK_F12	:
				if( szHelpFile2[0] ) {
					char	buf[ NPAT ];
					if( wordatcursor( buf, NPAT ) != T )
						WinHelp(
						         msg.hwnd,
							 szHelpFile2,
							 HELP_INDEX,
							 0L
						       );
					else	WinHelp(
							 msg.hwnd,
							 szHelpFile2,
							 HELP_PARTIALKEY,
							 (HELPBUF_TYPE)buf
						       );
				}
				break;
			case VK_CAPITAL :
				winshowstate( VK_CAPITAL, "CAP", &RCCapsMode );
				break;
			case VK_NUMLOCK :
				winshowstate( VK_NUMLOCK, "NUM", &RCNumLMode );
				break;
			case VK_INSERT	:
				winshowstate( VK_INSERT,  "RPL", &RCRepMode  );
				nRet = META|'I';
				break;
			case VK_CONTROL	: continue;
			case VK_SHIFT	: continue;
			case VK_END	:
				if( GetKeyState(VK_CONTROL) < 0 )
					nRet = CTLX|Ctrl|'N';
				else	nRet = META|'>';
				break;
			case VK_HOME	:
				if( GetKeyState(VK_CONTROL) < 0 )
					nRet = CTLX|Ctrl|'P';
				else	nRet = META|'<';
				break;
			case VK_PRIOR	: nRet = META|'V';	break;
			case VK_NEXT	: nRet = Ctrl|'V';	break;
			case VK_LEFT	:
				if( GetKeyState(VK_CONTROL) < 0 )
					nRet = META|'B';
				else	nRet = Ctrl|'B';
				break;
			case VK_UP	: nRet = Ctrl|'P';	break;
			case VK_RIGHT	:
				if( GetKeyState(VK_CONTROL) < 0 )
					nRet = META|'F';
				else	nRet = Ctrl|'F';
				break;
			case VK_DOWN	: nRet = Ctrl|'N';	break;
			case VK_DELETE	: nRet = Ctrl|'D';	break;
			case VK_F2	: nRet = Ctrl|'S';	break;
			case VK_F3	: nRet = META|Ctrl|'F';	break;
			case VK_F4	: nRet = CTLX|Ctrl|'I';	break;
			case VK_F5	: (void)switchscreen();	break;
			case VK_F6	: nRet = CTLX|METACH;	break;
			case VK_F7	: nRet = CTLX|'`';	break;
			case VK_F8	: nRet = CTLX|Ctrl|'S';	break;
			case VK_F9	: nRet = CTLX|'E';	break;
			}
			if( nRet ) {
				if( mouse_avoidance_mode == T &&
				    (++winstrokes % mouse_avoidance_nudge) == 0 )
				    	/*
				    	 * every   5   strokes,  move
				    	 * mouse to the bottom
					 */
					winmousebottom();

				wincshow( FALSE );
				return( nRet );
			}

		default :
			wincshow( FALSE );
			DispatchMessage( &msg );
			wincshow( TRUE );
			if( editflag == NIL ) {
				editflag = T;
				return( CTLX|Ctrl|'C' );
			}
		}
	}

	wincshow( FALSE );
	return( 0 );

}

#if	defined( NO_TRACKING )
static	void
wintrack( void )
{
	int	x = 0;
	int	i = 0;
	POINT	gpoint;

	GetCursorPos( &gpoint );
	ScreenToClient( hWnd, &gpoint );

	mevent.x = gpoint.x / nCharWidth;
	mevent.y = (gpoint.y-nBarSize) / nCharHeight;

	wincshow( FALSE );
	if( ((nDrawY-nBarSize)/nCharHeight) > mevent.y )
		backline();
	if( ((nDrawY-nBarSize)/nCharHeight) < mevent.y )
		forwline();

	x   = 0;
	i   = 0;

	gotobol();

	while( (x < mevent.x) && (i < llength( curwp->w_dotp )) ) {
		if( lgetc( curwp->w_dotp, i++ ) == '\t' )
			do
				++x;
			while( x % tab_display );
		else	++x;
		(void)forwchar();
	}

	update( T );
	wincshow( TRUE );
}
#endif

static	void
wineeop( void )
{
	RECT	rcClear;

	GetClientRect( hWnd, (LPRECT)&rcClear );
	rcClear.top    += nBarSize;
	rcClear.bottom -= fBarSize;
	FillRect( hDC, (LPRECT)&rcClear, hBrush );
}

static	void
wineeol( void )
{
	RECT	rcClear;

	rcClear.top    = nDrawY;
	rcClear.bottom = nDrawY + nCharHeight;
	rcClear.left   = nDrawX;
	rcClear.right  = nParentWidth + nCharWidth;

	FillRect( hDC, (LPRECT)&rcClear, hBrush );
}

static	void
winsi( void )
{
	SetBkColor( hDC, COLOR_LIGHTGRAY );
	SetTextColor( hDC, COLOR_BLACK );
}

static	void
winei( void )
{
	HPEN	hPen;
	HPEN	hOldPen;

	TextOut( hDC, TTYncol * nCharWidth, nDrawY - nCharHeight, " ", 1 );

	hPen    = CreatePen( PS_SOLID, 1, COLOR_WHITE );
	hOldPen	= (HPEN)SelectObject( hDC, hPen );
	MoveToEx( hDC, 0, nDrawY - nCharHeight, NULL );
	LineTo( hDC, nParentWidth, nDrawY - nCharHeight );
	(void)SelectObject( hDC, hOldPen );
	DeleteObject( hPen );

	hPen    = CreatePen( PS_SOLID, 1, COLOR_BLACK );
	hOldPen	= (HPEN)SelectObject( hDC, hPen );
	MoveToEx( hDC, 0, nDrawY - nLeading - 1 , NULL );
	LineTo( hDC, nParentWidth, nDrawY - nLeading - 1 );

	(void)SelectObject( hDC, hOldPen );
	DeleteObject( hPen );

	SetBkColor( hDC, BackColor );
	SetTextColor( hDC, TextColor );
}

static	void
winflush( void )
{
}

static	void
winbeep( void )
{
	MessageBeep( (UINT)-1 );
}

static	void
wincshow( int nFlag )
{
	RECT	rc;

	if( (IsIconic( hWnd ) != FALSE) || (nEditFlag == TRUE) )
		return;

	rc.left   = nDrawX;
	rc.top    = nDrawY;
	rc.right  = nDrawX + nCharWidth;
	rc.bottom = nDrawY + nCharHeight;

	SetBkColor( hDC, nFlag ? TextColor : BackColor );
	SetTextColor( hDC, nFlag ? BackColor : TextColor );
	ExtTextOut( hDC, nDrawX, nDrawY, ETO_CLIPPED, &rc, &curchar, 1, NULL );

	if( nFlag == TRUE ) {
		winshowcol( FALSE );
		winshowrow( FALSE );
	}

	SetBkColor( hDC, BackColor );
	SetTextColor( hDC, TextColor );

}

static	int
wincheck( void )
{
	MSG	msg;

	/*
	 *	Process messages intended for the abort dialog box
	 */

	while( PeekMessage( &msg, NULL, FALSE, FALSE, TRUE ) ) {
		if( TranslateAccelerator( msg.hwnd, hAccTable, &msg ) )
			continue;
		TranslateMessage( &msg );
		switch( msg.message ) {
		case WM_CHAR :
			if( msg.wParam == 0x07 )
				return( FALSE );
		}
		DispatchMessage( &msg );		
	}

	return( TRUE );
}

static void
winrawmode( void )
{
}

CMD
switchscreen( void )
{
	static	int	nFlag = 0;

	if( nFlag == 0 ) {
		ShowWindow( hWnd, SW_SHOWMAXIMIZED );
		nFlag = 1;
	} else	{
		ShowWindow( hWnd, SW_NORMAL );
		nFlag = 0;
	}

	(void)winsize( FALSE );

	return( T );
}

static	int
winversion( void )
{
	DWORD dwver = GetVersion();

	return( (int)(((dwver & 0xFF) * 100) + ((dwver >> 8) & 0xFF)) );
}

static	void
winsize( int fCharSize )
{
	RECT	rcClient;

	if( nOpen && !IsIconic( hWnd ) ) {
		wincshow( FALSE );

		GetClientRect( hWnd, (LPRECT)&rcClient );
		if( nVisibleBar == TRUE )
			rcClient.top    += nBarSize;
		rcClient.bottom -= fBarSize;

		if( (fCharSize == 0) &&
		    (nParentWidth ==(int)(rcClient.right -rcClient.left)) &&
		    (nParentHeight==(int)(rcClient.bottom-rcClient.top)) )
		    	return;

		nParentWidth  = (int)(rcClient.right  - rcClient.left);
		nParentHeight = (int)(rcClient.bottom - rcClient.top);

		wincharsize();

		vtfree();
		TTYncol = (int)(nParentWidth /nCharWidth);
		TTYnrow = (int)(nParentHeight/nCharHeight)-MODELINE;
		if( TTYnrow <= 1 )
			TTYnrow = 2;

		vtinit();
		resize();

		FillRect( hDC, (LPRECT)&rcClient, hBrush );
		InvalidateRect( hWnd, &rcClient, FALSE );
		MoveWindow( hBar, 0, 0, nParentWidth, nBarSize, TRUE );
		GetClientRect( hBar, (LPRECT)&rcClient );
		InvalidateRect( hBar, &rcClient, TRUE );

		GetClientRect( hWnd, (LPRECT)&rcClient );
		MoveWindow(
			    fBar,
			    0,
			    rcClient.bottom - fBarSize,
			    nParentWidth,
			    fBarSize,
			    TRUE
			  );
		GetClientRect( fBar, (LPRECT)&rcClient );
		InvalidateRect( fBar, &rcClient, TRUE );
	}
}

static	void
winadjust( void )
{
	RECT	rcClient;
	int	nWidth;
	int	nHeight;

	ShowWindow( hWnd, SW_NORMAL );
	GetWindowRect( hWnd, (LPRECT)&rcClient );
	nZoomed    = FALSE;
	nMaximized = FALSE;
	nWidth	   = nCharWidth * 81 + GetSystemMetrics( SM_CYVTHUMB );
	nHeight	   = rcClient.bottom - rcClient.top;
	SetWindowPos(
		    hWnd,
		    HWND_TOP,
		    rcClient.left,
		    rcClient.top,
		    nWidth,
		    nHeight,
		    SWP_SHOWWINDOW|SWP_FRAMECHANGED
		  );
	winsize( FALSE );
}

static	void
wincharsize( void )
{
	TEXTMETRIC	txMetric;
	ABC		abc;

	GetTextMetrics( hDC, &txMetric );

	if( (txMetric.tmPitchAndFamily & TMPF_TRUETYPE) == 0 )
		GetCharWidth( hDC, (UINT)'M', (UINT)'M', &nCharWidth );
	else	if( GetCharABCWidths( hDC, (UINT)'M', (UINT)'M', &abc ) )
			nCharWidth = (int)(abc.abcA + abc.abcB + abc.abcC);
	else	GetCharWidth( hDC, (UINT)'M', (UINT)'M', &nCharWidth );

	nCharHeight = (int)(txMetric.tmHeight + txMetric.tmInternalLeading);
	nLeading    = (int)txMetric.tmInternalLeading;

}

/*
 *	calls initialization function, processes message loop
 */

int	WINAPI
WinMain( HINSTANCE hInstance, HINSTANCE hPrevInst, LPSTR lpCmd, int nCmdShow )
{
	char	*argv[ 32 ];
	int	argc = 0;

	if( hPrevInst ) {
		MessageBox(
			    GetFocus(),
			    "is already loaded.",
			    version,
			    MB_ICONHAND | MB_SYSTEMMODAL
			  );
		return( 0 );
	}

	hInst	  = hInstance;
	hAccTable = LoadAccelerators( hInst, "EmacsAccelerator" );
	hPrev	  = hPrevInst;
	nCmd	  = nCmdShow;

	argv[ argc++ ] = "Emacs";

	while( *lpCmd ) {
		while( *lpCmd && *lpCmd == ' ' )
			lpCmd++;
		if( *lpCmd )
			argv[ argc++ ] = lpCmd;
		else	break;
		while( *lpCmd && *lpCmd != ' ' )
			lpCmd++;
		if( *lpCmd == ' ' )
			*lpCmd++ = '\000';
	}

	argv[ argc ] = NULL;

#if	defined( _WINDOWS_SOURCE )
	main( argc, argv );
#else
	emacs( argc, argv );
#endif

#if	defined( _CTRL3D )
	if( nCtl3D )
		lpCtl3dUnregister( hInst );
#endif

	return( 0 );

}

static	void
winnomem( void )
{
	MessageBox(
		    GetFocus(),
		    "Out of Memory",
		    NULL,
		    MB_ICONHAND | MB_SYSTEMMODAL
		  );
}

static	void
wincutcopy( WPARAM wParam )
{
	UINT	AllocFlags = GMEM_MOVEABLE|GMEM_DDESHARE;
	HANDLE	hClipData;
	LPSTR	lpClipData;

	/*
	 * After  SetClipboardData  is  called,  the  system owns the
	 * object   identified   by   the  hClipData  parameter.  The
	 * application  can  read  the  data,  but  must not free the
	 * handle  or  leave  it  locked.  If the hClipData parameter
	 * identifies  a  memory  object,  the  object must have been
	 * allocated   using   the   GlobalAlloc  function  with  the
	 * GMEM_MOVEABLE and GMEM_DDESHARE flags.
	 */

	if( kused == 0 )
		return;

	/*
	 *	Allocate memory and copy the string to it
	 */

	if( (hClipData = GlobalAlloc( AllocFlags, kused+1 )) == NULL ) {
		winnomem();
		return;
	}

	/*
	 *	Copy data
	 */

	lpClipData = (LPSTR)GlobalLock( hClipData );

	if( lpClipData == NULL ) {
		winnomem();
		return;
	}

	(void)strncpy( lpClipData, kbufp, kused );
	lpClipData[ kused ] = 0;

	GlobalUnlock( hClipData );

	/*
	 *	Clear the current contents of the clipboard, and set
	 *	the data handle to the new string.
	 */

	if( OpenClipboard( hWnd ) == 0 )
		WDGerror( "Can't open clipboard." );
	else	{
		EmptyClipboard();
		if( SetClipboardData( CF_OEMTEXT, hClipData ) == NULL )
			WDGerror( "Can't set clipboard data." );
		CloseClipboard();
	}

	if( wParam == IDM_CUT ) {
		(void)killregion();
		EnableMenuItem( GetMenu( hWnd ), IDM_CUT,  MF_GRAYED );
		EnableMenuItem( GetMenu( hWnd ), IDM_COPY, MF_GRAYED );
	}

	GlobalUnlock( hClipData );
}

static	void
winclipcopy( void )
{
	wincutcopy( WM_COPY );
}

static	void
winclippaste( void )
{
	LPSTR	lpClipData = (LPSTR)0;  /* clip data		*/
	HANDLE	hClipData  = (HANDLE)0;	/* handles to clip data	*/
#if	defined( _WINDOWS_SOURCE )
	UINT	fmt[] = { CF_TEXT, CF_OEMTEXT, CF_UNICODETEXT, CF_DSPTEXT };
#else
	UINT	fmt[] = { CF_TEXT, CF_OEMTEXT, CF_DSPTEXT };
#endif
	LPSTR	s;
	int	type  = -1;
	int	i;

	if( OpenClipboard( hWnd ) == FALSE )
		return;

	/*
	 *	get text from the clipboard
	 */

	for( i = 0 ; i < (sizeof( fmt ) / sizeof( fmt[0] )) ; ++i )
		if( (hClipData = GetClipboardData( fmt[i] )) != 0 ) {
			type = fmt[ i ];
			break;
		}

	if( hClipData == (HANDLE)0 ) {
		CloseClipboard();
		return;
	}

	if( (lpClipData = (LPSTR)GlobalLock( hClipData ) ) == (LPSTR)0 ) {
		WDGerror( "No memory left, file may be corrupted." );
		CloseClipboard();
		return;
	}

#if	defined( _CONVERT_WHEN_OEM )
	if( type == CF_OEMTEXT )
		OemToAnsi( lpClipData, lpClipData );
#endif

	kused = 0;

	for( s = lpClipData ; *s ; s++ )
		if( *s != '\r' )
			(void)kinsert( (int)*s );

	GlobalUnlock( hClipData );
	CloseClipboard();
}

LRESULT APIENTRY
MainWndProc( HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam )
{
	FARPROC		lpProc;
	PAINTSTRUCT	ps;
	char		curwd[ NFILEN ];
	static	int	nFindFlag = FALSE;
	static	int	nLastFile = -1;

	switch( message ) {
        case WM_INITMENU:
		if( wParam == (WPARAM)GetMenu( hwnd ) ) {
			HMENU	hMenu = (HMENU)wParam;
			HMENU	hSubMenu;
			BUFFER	*bp;
			int	i;

			if( curwp->w_markp != 0 ) {
				EnableMenuItem( hMenu, IDM_CUT,  MF_ENABLED );
				EnableMenuItem( hMenu, IDM_COPY, MF_ENABLED );
			} else	{
				EnableMenuItem( hMenu, IDM_CUT,  MF_GRAYED );
				EnableMenuItem( hMenu, IDM_COPY, MF_GRAYED );
			}

			if( (nLastFile == -1) && szLastFile[0][0] )
				AppendMenu(
					    GetSubMenu( GetMenu( hWnd ), 0 ),
					    MF_SEPARATOR,
					    0,
					    NULL
					  );

			for( i = 0 ; i < MAXLASTFILES ; i++ )
				if( (nLastFile < i) && szLastFile[i][0] ) {
					AppendMenu(
					    GetSubMenu( GetMenu( hWnd ), 0 ),
					    MF_ENABLED,
					    IDM_FILE + i,
					    szLastFile[ i ]
					  );
					nLastFile = i;
				} else	if( szLastFile[i][0] )
					  ModifyMenu(
					    GetSubMenu( GetMenu( hWnd ), 0 ),
					    IDM_FILE + i,
					    MF_BYCOMMAND | MF_ENABLED,
					    IDM_FILE + i,
					    szLastFile[ i ]
					  );

			hSubMenu = GetSubMenu( GetMenu( hWnd ), 4 );
			i	 = GetMenuItemCount(hSubMenu);

			if( szHelpFile1[ 0 ] )
				if( i >= 3 ) /* Help 1 is 3rd item in menu */
					ModifyMenu(
					  	    hSubMenu,
						    IDM_HELP_FILE1,
						    MF_BYCOMMAND | MF_ENABLED,
						    IDM_HELP_FILE1,
						    szHelpFile1
						  );
				else	AppendMenu(
						    hSubMenu,
						    MF_ENABLED,
						    IDM_HELP_FILE1,
						    szHelpFile1
						  );

			if( szHelpFile2[ 0 ] )
				if( i >= 4 ) /* Help 2 is 4th item in menu */
					ModifyMenu(
					  	    hSubMenu,
						    IDM_HELP_FILE2,
						    MF_BYCOMMAND | MF_ENABLED,
						    IDM_HELP_FILE2,
						    szHelpFile2
						  );
				else	AppendMenu(
						    hSubMenu,
						    MF_ENABLED,
						    IDM_HELP_FILE2,
						    szHelpFile2
						  );

			hSubMenu = GetSubMenu( GetMenu( hWnd ), 2 );

			for( i = GetMenuItemCount(hSubMenu)-1 ; i >= 0 ; i-- )
				DeleteMenu( hSubMenu, i, MF_BYPOSITION );

			for( bp=bheadp, i=0 ; bp!=NULL ; bp=bp->b_bufp, i++ )
				AppendMenu(
					    hSubMenu,
					    MF_ENABLED,
					    IDM_BUFFER + i,
					    &bp->b_bname[0]
				);

			if( OpenClipboard( hwnd ) ) {
			    if( IsClipboardFormatAvailable( CF_TEXT ) ||
			        IsClipboardFormatAvailable( CF_OEMTEXT ) )
				    EnableMenuItem(hMenu,IDM_PASTE,MF_ENABLED);
			    else    EnableMenuItem(hMenu,IDM_PASTE,MF_GRAYED);
			    CloseClipboard();
			    return( TRUE );
			}

			return( FALSE );

		}
		return( TRUE );

	case WM_COMMAND:
		switch( wParam ) {
		case IDM_OPEN     :
			(void)strcpy( curwd, curbp->b_fname );
			(void)updir( curwd, NOSLASH );
			ofn.lpstrInitialDir = curwd;
			if( GetOpenFileName( (LPOPENFILENAME)&ofn ) != 0 ) {
				(void)newfile( emstrlwr( szFileName ) );
				update( T );
			}
			break;
		case IDM_SAVE     :
			(void)filesave();
			break;
		case IDM_SAVEAS   :
			(void)strcpy( szFileName, curbp->b_fname );
			if( GetSaveFileName( (LPOPENFILENAME)&ofn) )
				if( writeout( szFileName ) )
					(void)strcpy(curbp->b_fname,szFileName);
			break;
		case IDM_PRINT    :
			winprint();
			break;
		case IDM_PRINTSETUP:
			winprintsetup();
			break;
		case IDM_QUIT     :
			if( exitemacs() != FALSE )
				return( 0L );
			break;
		case IDM_ABOUT    :
			lpProc = MakeProcInstance( (FARPROC)About, hInst );
			DialogBox( hInst, "AboutBox", hwnd, (DLGPROC)lpProc );
			FreeProcInstance( lpProc );
			break;
		case IDM_FILE + 0:
		case IDM_FILE + 1:
		case IDM_FILE + 2:
		case IDM_FILE + 3:
		case IDM_FILE + 4:
		case IDM_FILE + 5:
		case IDM_FILE + 6:
		case IDM_FILE + 7:
			(void)newfile( szLastFile[ wParam - IDM_FILE ] );
			update( T );
			break;
		case IDM_BUFFER + 0:
		case IDM_BUFFER + 1:
		case IDM_BUFFER + 2:
		case IDM_BUFFER + 3:
		case IDM_BUFFER + 4:
		case IDM_BUFFER + 5:
		case IDM_BUFFER + 6:
		case IDM_BUFFER + 7: {
			char	buffer[ NBUFN ];
			BUFFER *bp;
			GetMenuString(
					GetSubMenu( GetMenu( hWnd ), 2 ),
					(UINT)(wParam - IDM_BUFFER),
					&buffer[0],
					NFILEN,
					MF_BYPOSITION
				     );
			if( (bp = bfind( buffer, NIL, 0, 0)) == NULL )
				return( FALSE );
			(void)connectwindow( curwp, bp );
			update( T );
			}
			break;
                case IDM_CUT:
                case IDM_COPY:
			wincutcopy( wParam );
			update( T );
			break;
                case IDM_PASTE:
			winclippaste();
			(void)yank();
			update( T );
			break;
		case IDM_SEARCH   :
			FindText( (LPFINDREPLACE)&frpl );
			break;
		case IDM_RSEARCH  :
			(void)backsearch();
			break;
		case IDM_GREPLACE :
			ReplaceText( (LPFINDREPLACE)&frpl );
			break;
		case IDM_QREPLACE :
			ReplaceText( (LPFINDREPLACE)&frpl );
			break;
		case IDM_CFONTS:
			wingetfont();
			break;
		case IDM_CCOLORS:
			wingetcolor();
			break;
		case IDM_HELP_CMD :
			WinHelp( hwnd, "emacs.hlp", HELP_INDEX, 0L );
			break;
		case IDM_HELP_FILE1 :
			WinHelp( hwnd, szHelpFile1, HELP_INDEX, 0L );
			break;
		case IDM_HELP_FILE2 :
			WinHelp( hwnd, szHelpFile2, HELP_INDEX, 0L );
			break;
		case IDM_HELP :
			(void)describekey();
			break;
		case IDM_ANSI:
			(void)oemtoansi();
			CheckMenuItem( GetMenu(hWnd), IDM_OEM,  MF_UNCHECKED );
			CheckMenuItem( GetMenu(hWnd), IDM_ANSI, MF_CHECKED   );
			update( T );
			break;
		case IDM_OEM:
			(void)ansitooem();
			CheckMenuItem( GetMenu(hWnd), IDM_OEM,  MF_CHECKED   );
			CheckMenuItem( GetMenu(hWnd), IDM_ANSI, MF_UNCHECKED );
			update( T );
			break;
		case IDM_NORMAL :
			switchfund();
			update( T );
			break;
		case IDM_C :
			switchcc();
			update( T );
			break;
		case IDM_CPP :
			switchcpp();
			update( T );
			break;
		case IDM_JAVA :
			switchjava();
			update( T );
			break;
		case IDM_LISP :
			switchlisp();
			update( T );
			break;
		case IDM_ASSEMBLER :
			switchas();
			update( T );
			break;
		case IDM_PROLOG :
			switchprolog();
			update( T );
			break;
		case IDM_PARAMS :
			winparams();
			break;
		case IDM_BUTTONBAR :
			if( nVisibleBar == TRUE )
				nVisibleBar = FALSE;
			else	nVisibleBar = TRUE;
			winbuttonbar();
			winsize( FALSE );
			break;
		default :
			return( DefWindowProc(hwnd, message, wParam, lParam) );
		}
		break;

	case WM_DROPFILES: {
		char szDragFile[ NFILEN ];
		DragQueryFile((HDROP)wParam, 0, (LPSTR)&szDragFile[0], NFILEN);
		(void)newfile( emstrlwr( szDragFile ) );
		update( T );
		break;
		}

	case WM_MOUSEACTIVATE :
		DefWindowProc( hwnd, message, wParam, lParam );
		return( MA_ACTIVATEANDEAT );

	case WM_SIZE:
		winsize( FALSE );
		break;

	case WM_CREATE:
		/* Create the objects */
		break;

	case WM_CLOSE:
		editflag = NIL;
		return( 0L );

	case WM_SETFOCUS :
  		nFindFlag = FALSE;
		return( DefWindowProc( hwnd, message, wParam, lParam ) );

#if	defined( _WINDOWS_SOURCE )
	case WM_MOUSEWHEEL :
		if( (short)HIWORD( wParam ) > 0 )  /* zDelta */
			(void)forwpage();
		else	(void)backpage();
		update( T );
		(void)SetScrollPos( hwnd, SB_VERT, average, TRUE );
		break;
#endif

	case WM_PAINT :
		BeginPaint( hwnd, &ps );
		if( nOpen ) {
			if( sgarbf != T )
				sgarbf = EXPOSE;
			(void)SelectObject( hDC, hFont );
			update( T );
		}
		EndPaint( hwnd, &ps );
		break;

	case WM_VSCROLL :
#if	defined( _WINDOWS_SOURCE )
		switch( LOWORD( wParam ) ) {
#else
		switch( wParam ) {
#endif
		case SB_LINEUP	      : (void)backline(); break;
		case SB_LINEDOWN      : (void)forwline(); break;
		case SB_PAGEUP        : (void)backpage(); break;
		case SB_PAGEDOWN      : (void)forwpage(); break;
		case SB_THUMBPOSITION : {
#if	defined( _WINDOWS_SOURCE )
			int nPos = (int)HIWORD( wParam );
#else
			int nPos = (int)lParam;
#endif

			if( (repeat=(int)((nPos * maxlen) / 100)) <= 1 ) {
				repeat = 1;
				(void)gotobob();
			} else	(void)gotoline();
			}
			break;
		}
		update( T );
		(void)SetScrollPos( hwnd, SB_VERT, average, TRUE );
		break;

	default:
		if( message == wFRMsg ) {
			LPFINDREPLACE	lpFR	  = (LPFINDREPLACE)lParam;
			DWORD		dwFlags   = lpFR->Flags;
			int		replaced  = 0;

			lastflag	 = CFFSRC;
			case_sensitivity = (int)(dwFlags & FR_MATCHCASE);

			wincshow( FALSE );
			(void)strcpy( search_buffer, szFind );

			if( dwFlags & FR_FINDNEXT )
				if( dwFlags & FR_DOWN )
					nFindFlag = forwsearch();
				else	nFindFlag = backsearch();

			if( (dwFlags & FR_REPLACE) && nFindFlag ) {
				curwp->w_dotp  = found_p;
				curwp->w_doto  = found_o;
				curwp->w_flag |= WFHARD;
				subst( (int)strlen( szFind ), szReplace );
				replaced++;
				if( dwFlags & FR_DOWN )
					nFindFlag = forwsearch();
				else	nFindFlag = backsearch();
			}

			if( (dwFlags & FR_REPLACEALL) && nFindFlag )
				do {
					int	len = (int)strlen( szFind );

					curwp->w_dotp  = found_p;
					curwp->w_doto  = found_o;
					curwp->w_flag |= WFHARD;
					subst( len, szReplace );
					replaced++;
					nFindFlag = FALSE;
				} while( ffindstring() );

			update( T );
			WDGwrite( "Replaced %d occurence(s)", replaced );
			wincshow( TRUE );
		}

		return( DefWindowProc(hwnd, message, wParam, lParam) );
	}

	return( FALSE );
}

LRESULT APIENTRY
BarWndProc( HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam )
{
	PAINTSTRUCT	ps;
	HBRUSH		hBrush;
	HBRUSH		hOldBrush;
	HPEN		hOldPen;
	HPEN		hPen;
	char		curwd[ NFILEN ];
	static	int	nRecord   = 0;
	static	int	nToolFlag = 0;

	switch( message ) {
	case WM_COMMAND:
		if( nToolFlag == 1 ) {
			winbeep();
			return( FALSE );
		} else	nToolFlag = 1;

		switch( wParam ) {
		case IDM_TOOL_OPEN:
			wincshow( FALSE );
			(void)strcpy( curwd, curbp->b_fname );
			(void)updir( curwd, NOSLASH );
			ofn.lpstrInitialDir = curwd;
			if( GetOpenFileName( (LPOPENFILENAME)&ofn ) != 0 ) {
				(void)newfile( emstrlwr( szFileName ) );
				update( T );
			}
			wincshow( TRUE );
			break;
		case IDM_TOOL_SAVE:
			(void)filesave();
			break;
		case IDM_TOOL_FIND:
			FindText( (LPFINDREPLACE)&frpl );
			break;
		case IDM_TOOL_REPLACE:
			ReplaceText( (LPFINDREPLACE)&frpl );
			break;
		case IDM_TOOL_RECORD:
			wincshow( FALSE );
			if( nRecord ) {
				nRecord = 0;
				(void)ctlxrp();
			} else	{
				nRecord = 1;
				(void)ctlxlp();
			}
			update( T );
			wincshow( TRUE );
			break;
		case IDM_TOOL_PLAY:
			wincshow( FALSE );
			(void)ctlxe();
			update( T );
			wincshow( TRUE );
			break;
		case IDM_TOOL_MAKE:
			wincshow( FALSE );
			makefile();
			update( T );
			wincshow( TRUE );
			break;
		case IDM_TOOL_QUIT:
			if( exitemacs() != FALSE )
				return( 0L );
			break;
		}
		SetFocus( hWnd );
		nToolFlag = 0;
		break;
	case WM_MOUSEACTIVATE :
		break;
	case WM_PAINT :
		BeginPaint( hwnd, &ps );

		/*
		 * Fill the area whith LIGHT GRAY Brush.
		 */

		hBrush	  = CreateSolidBrush( COLOR_LIGHTGRAY );
		hOldBrush = (HBRUSH)SelectObject( ps.hdc, hBrush );
		FillRect( ps.hdc, (LPRECT)&ps.rcPaint, hBrush );
		(void)SelectObject( ps.hdc, hOldBrush );
		DeleteObject( hBrush );

		/*
		 * Draw BLACK line on the bottom.
		 */

		hPen    = CreatePen( PS_SOLID, 1, COLOR_BLACK );
		hOldPen	= (HPEN)SelectObject( ps.hdc, hPen );
		MoveToEx( ps.hdc, 0, nBarSize-1, NULL );
		LineTo( ps.hdc, nParentWidth, nBarSize-1 );
		(void)SelectObject( ps.hdc, hOldPen );
		DeleteObject( hPen );

		/*
		 * Draw GRAY line just after.
		 */

		hPen = CreatePen( PS_SOLID, 1, COLOR_GRAY );
		(void)SelectObject( ps.hdc, hPen );
		MoveToEx( ps.hdc, 0, nBarSize-2, NULL );
		LineTo( ps.hdc, nParentWidth, nBarSize-2 );
		(void)SelectObject( ps.hdc, hOldPen );
		DeleteObject( hPen );

		/*
		 * Draw WHITE line on the top.
		 */

		hPen = CreatePen( PS_SOLID, 1, COLOR_WHITE );
		(void)SelectObject( ps.hdc, hPen );
		MoveToEx( ps.hdc, 0, 0, NULL );
		LineTo( ps.hdc, nParentWidth, 0 );
		(void)SelectObject( ps.hdc, hOldPen );
		DeleteObject( hPen );

		EndPaint( hwnd, &ps );
		break;
	default:
		return( DefWindowProc( hwnd, message, wParam, lParam ) );
	}

	return( FALSE );
}

/*
 *	Processes messages for "About" dialog box
 */

LRESULT APIENTRY
About( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;
	char	szLicence[ 128 ];

	lParam = lParam;

	switch( message ) {
	case WM_INITDIALOG:
		GetWindowRect( hDlg, &aRec );
		OffsetRect( &aRec, -((int)aRec.left), -((int)aRec.top) );
		MoveWindow(
			    hDlg,
			    ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
			    (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
			    (int)aRec.right,
			    (int)aRec.bottom,
			    0
			  );
		SetDlgItemText( hDlg, ID_DATE, __DATE__ );
		(void)emsprintf1(szLicence,ECSTR("Licenced to '%s'"),licence);
		SetDlgItemText( hDlg, ID_LICENCE, szLicence );
		return( (LRESULT)TRUE );

	case WM_COMMAND:
		if( wParam == IDOK || wParam == IDCANCEL ) {
			EndDialog( hDlg, TRUE );
			return( (LONG)TRUE );
		}
		break;
	}

	return( (LONG)FALSE );
}

/*
 *	The SignOn dialog box procedure
 */

LRESULT APIENTRY
SignOnProc( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;
	char	szLicence[ 128 ];

	hDlg   = hDlg;
	wParam = wParam;
	lParam = lParam;

	switch( message ) {
	case WM_INITDIALOG :
		GetWindowRect( hDlg, &aRec );
		OffsetRect( &aRec, -((int)aRec.left), -((int)aRec.top) );
		MoveWindow(
			    hDlg,
			    ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
			    (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
			    (int)aRec.right,
			    (int)aRec.bottom,
			    0
			  );
		SetDlgItemText( hDlg, ID_DATE, __DATE__ );
		(void)emsprintf1(szLicence,ECSTR("Licenced to '%s'"),licence);
		SetDlgItemText( hDlg, ID_LICENCE, szLicence );
		return( (LRESULT)TRUE );
	case WM_ACTIVATE :
		if( wParam == WA_CLICKACTIVE )
			return( (LRESULT)FALSE );
		else	return( (LRESULT)TRUE  );
	}

	return( (LRESULT)FALSE );

}

/*
 *	Implementation of Widgets
 */

static	void
winerror( char *s )
{
	MessageBox( hWnd, s, "", MB_OK );
}

static	CMD
winyesno( char *s )
{
	int	res;

	res = winwarning( s );

	return( (res == T) ? T : NIL );

}

static	char	*
wintitle( char *buffer, char *fname )
{
	static	char	title[ 64 ] = { 0 };

	if( strcmp( buffer, title ) != 0 ) {
	  (void)emsprintf3(title,ECSTR("%s (%dx%d)"),buffer,TTYnrow,TTYncol );
	  SetWindowText( hWnd, title );
	}

	if( fname && *fname ) {
		int	i;
		char	fulln[ NFILEN ];

		(void)_fullpath( (char *)&fulln[ 0 ], fname, NFILEN-1 );

		for( i = MAXLASTFILES - 2 ; i >= 0 ; i-- )
			if( strcmp( fulln, szLastFile[ i ] ) == 0 ) {
				/*
				 * file is in the list, delete the entry.
				 */
				while( i < (MAXLASTFILES-1) ) {
				    (void)strcpy(szLastFile[i],szLastFile[i+1]);
				    i++;
				}
				szLastFile[MAXLASTFILES-1][0] = '\000';
				break;
			}

		for( i = MAXLASTFILES - 2 ; i >= 0 ; i-- )
			(void)strcpy( szLastFile[ i+1 ], szLastFile[ i ] );

		(void)strcpy( szLastFile[ 0 ], fulln );
	}

	return( buffer );
}

static	void
winmousebottom()
{
	RECT	rect;

	GetWindowRect( hWnd, &rect );

	SetCursorPos( rect.right - nCharWidth, rect.bottom - nCharHeight );
}

static	CMD
winasker( char *prompt, char *buf, int nbuf )
{
	buf[ 0 ] = '\000';

	return( GetText( ((*prompt == ':') ? prompt+2 : prompt), buf, nbuf ) );
}

static	CMD
winedit( char *prompt, char *buf, int nbuf )
{
	register int	cpos = 0;
	register int	c;

	if( kbdmop != NULL ) {
		/*
		 *	Execute a macro
		 */
		while( (c = *kbdmop++) != '\0' )
			buf[ cpos++ ] = (char)c;
		buf[ cpos ]	= '\0';
		complete.flag	= 0L;
		return( (buf[ 0 ] == 0) ? NIL : T );
	}

	return( GetText( ((*prompt == ':') ? prompt+2 : prompt), buf, nbuf ) );

}

static	CMD
winchange( char *msgo, char *msgn, char *opat, char *npat, int len )
{
	if( (mlreply( msgo, opat, len ) == T ) )
		return( (mlreply( msgn, npat, len ) != ABORT) ? T : NIL );
	else	return( NIL );
}

static	void
winplay( int nFlag )
{
	if( nFlag ) {
		SetWindowText( hBTRecord, "Record" );
		EnableWindow( hBTPlay, TRUE );
	} else	{
		SetWindowText( hBTRecord, "Stop"   );
		EnableWindow( hBTPlay, FALSE );
	}

	SetFocus( hWnd );
}

static	HDC
GetPrinterDC( void )
{
	HDC         hDC;
	LPDEVMODE   lpDevMode = NULL;
	LPDEVNAMES  lpDevNames;
	LPSTR       lpszDriverName;
	LPSTR       lpszDeviceName;
	LPSTR       lpszPortName;

	if( PrintDlg( (LPPRINTDLG)&pd ) == FALSE )
		return( (HDC)-1 );

	if( pd.hDC )
		hDC = pd.hDC;
	else	{
		if( !pd.hDevNames )
			return( (HDC)NULL );

		lpDevNames     = (LPDEVNAMES)GlobalLock( pd.hDevNames );
		lpszDriverName = (LPSTR)lpDevNames + lpDevNames->wDriverOffset;
		lpszDeviceName = (LPSTR)lpDevNames + lpDevNames->wDeviceOffset;
		lpszPortName   = (LPSTR)lpDevNames + lpDevNames->wOutputOffset;
		GlobalUnlock( pd.hDevNames );

		if( pd.hDevMode )
			lpDevMode = (LPDEVMODE)GlobalLock( pd.hDevMode );

		hDC = CreateDC(
				lpszDriverName,
				lpszDeviceName,
				lpszPortName,
				lpDevMode
			      );

		if( pd.hDevMode && lpDevMode )
			GlobalUnlock( pd.hDevMode );
	}

	if( pd.hDevNames ) {
		GlobalFree( pd.hDevNames );
		pd.hDevNames = NULL;
	}

	if( pd.hDevMode ) {
		GlobalFree( pd.hDevMode );
		pd.hDevMode = NULL;
	}

	return( hDC );
}

LRESULT APIENTRY
AbortProc( HDC hPr, int Code )
{
	/*
	 * hPr  for multiple printer display contexts code printing status
	 */

	MSG	msg;

	hPr  = hPr;
	Code = Code;

	if( !hAbortDlgWnd )
		/* If the abort dialog isn't up yet */
		return( (LRESULT)TRUE );

	/*
	 *	Process messages intended for the abort dialog box
	 */

	while( !bAbort && PeekMessage( &msg, NULL, FALSE, FALSE, TRUE ) ) {
		if( TranslateAccelerator( msg.hwnd, hAccTable, &msg ) )
			continue;
		if( !IsDialogMessage( hAbortDlgWnd, &msg ) ) {
			TranslateMessage( &msg );
			DispatchMessage( &msg );
		}
	}

	/*
	 *	bAbort is TRUE (return is FALSE) if the user has aborted
	 */

	return( bAbort ? (LRESULT)FALSE : (LRESULT)TRUE );
}

LRESULT APIENTRY
AbortDlg( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
	wParam	= wParam;
	lParam	= lParam;

	switch( msg ) {

	/* Watch for Cancel button, RETURN key, ESCAPE key, or SPACE BAR */

        case WM_COMMAND:
		return( (LRESULT)(bAbort = TRUE) );

        case WM_INITDIALOG:
		/* Set the focus to the Cancel box of the dialog */

		SetFocus( GetDlgItem( hDlg, IDCANCEL ) );
		SetDlgItemText( hDlg, IDC_FILENAME, (LPSTR)&curbp->b_bname[0] );
		return( (LRESULT)TRUE );
	}

	return( (LRESULT)FALSE );
}

static	void
winprint()
{
	int	Status;		/* printing status			*/
	int	nPageLength;	/* vert. resolution of printer device	*/
	int	nPageWidth;	/* horz. resolution of printer device	*/
	HCURSOR	hSaveCursor;	/* current cursor handle		*/
	HDC	hPr;		/* handle for printer device context	*/
	FARPROC lpAbortDlg;	/* abort dialog				*/
	FARPROC	lpAbortProc;	/* abort proc				*/
	TEXTMETRIC TextMetric;	/* information about char size		*/
	HFONT	hfPr;		/* font used for printing		*/
	HFONT	hOldFont;	/* old font used			*/
	LOGFONT	lfPr;		/* LOGFONT structure for the font	*/
	int	LineSpace;	/* spacing between lines		*/
	int	LinesPerPage;	/* lines per page			*/
	int	CurrentLine;	/* current line	in page			*/
	int	LineNumber;	/* current line number			*/
	int	CurrentPage;	/* current page				*/
	int	len;		/* length of the current line		*/
	EDLINE	*clp;		/* current line to print		*/
	time_t	aclock;		/* current time				*/
	char	*szTimeBuf;	/* ascii version of current time	*/
	char	*p;
	char	buf[ NLINE + LINENBSIZE ];
	static	HCURSOR	hHourGlass = (HCURSOR)0;
	union	{
		LPSTR	s;
		FARPROC	p;
	}	nowarn;

	if( hHourGlass == (HANDLE)0 )
		hHourGlass = LoadCursor( NULL, IDC_WAIT );

	hSaveCursor = SetCursor( hHourGlass );
	hPr	    = GetPrinterDC();

	if( hPr == (HDC)-1 )
		return;

	if( hPr == (HDC)NULL ) {
		char	szTemp[ NFILEN ];

		(void)emsprintf1(szTemp,ECSTR("Cannot print %s"),szFileName);
		MessageBox( hWnd, szTemp, NULL, MB_OK | MB_ICONHAND );
		return;
	}

	/*
	 * Define the abort function
	 */

	lpAbortDlg  = MakeProcInstance( (FARPROC)AbortDlg,  hInst );
	lpAbortProc = MakeProcInstance( (FARPROC)AbortProc, hInst );

	/*
	 * Load the font "Courier New" size 8
	 */

	lfPr.lfHeight		= -MulDiv(8,GetDeviceCaps(hPr,LOGPIXELSY),72);
	lfPr.lfWidth		= 0;
	lfPr.lfEscapement	= 0;
	lfPr.lfOrientation	= 0;
	lfPr.lfWeight		= FW_LIGHT;
	lfPr.lfItalic		= 0;
	lfPr.lfUnderline	= 0;
	lfPr.lfStrikeOut	= 0;
	lfPr.lfCharSet		= 0;
	lfPr.lfOutPrecision	= OUT_STRING_PRECIS;
	lfPr.lfClipPrecision	= CLIP_STROKE_PRECIS;
	lfPr.lfQuality		= DEFAULT_QUALITY;
	lfPr.lfPitchAndFamily	= FF_MODERN | FIXED_PITCH;

	if( winversion() >= 310 )
		wsprintf( lfPr.lfFaceName, "Courier New" );
	else	wsprintf( lfPr.lfFaceName, "Courier" );
	hfPr = CreateFontIndirect( &lfPr );

	hOldFont = (HFONT)SelectObject( hPr, hfPr );

	/*
	 *	The union is used for the invalid type converion
	 */

	nowarn.p = lpAbortProc;

	Escape( hPr, SETABORTPROC, FALSE, nowarn.s, 0L );

	if( Escape( hPr, STARTDOC, 12, "Emacs Print", (LPSTR)NULL ) < 0 ) {
		MessageBox(
			    hWnd,
			    "Unable to start print job",
			    NULL,
			    MB_OK | MB_ICONHAND
			  );
		FreeProcInstance( lpAbortDlg );
		FreeProcInstance( lpAbortProc );
		(void)SelectObject( hPr, hOldFont );
		DeleteObject( hfPr );
		DeleteDC( hPr );
	}

	/*
	 * Clears the abort flag
	 */

	bAbort = FALSE;

	/*
	 * Create the Abort dialog box (modeless)
	 */

	hAbortDlgWnd = CreateDialog(
				     hInst,
				     "AbortDlg",
				     hWnd,
				     (DLGPROC)lpAbortDlg
				   );

	if( hAbortDlgWnd == (HWND)0 ) {
		SetCursor( hSaveCursor );
		MessageBox(
			    hWnd,
			    "NULL Abort window handle",
			    NULL,
			    MB_OK | MB_ICONHAND
			  );
		return;
	}

	/* Now show Abort dialog */

	ShowWindow( hAbortDlgWnd, SW_NORMAL );

	/* Disable the main window to avoid reentrancy problems */

	EnableWindow( hWnd, FALSE );
	SetCursor( hSaveCursor );      /* Remove the hourglass */

	/*
	 * Get time in szTimeBuf and remove '\n'
	 */

	time( &aclock );
	szTimeBuf = asctime( localtime( &aclock ) );
	for( p = szTimeBuf ; *p ; p++ )
		if( *p == '\n' )
			*p = '\000';

	/*
	 * Since  you  may  have  more  than  one  line,  you need to
	 * compute  the  spacing  between  lines.  You can do that by
	 * retrieving  the  height of the characters you are printing
	 * and  advancing  their height plus the recommended external
	 * leading height.
	 */

	GetTextMetrics( hPr, &TextMetric );
	LineSpace = TextMetric.tmHeight + TextMetric.tmExternalLeading;

	/*
	 * Since  you  may  have more lines than can fit on one page,
	 * you  need to compute the number of lines you can print per
	 * page.  You can do that by retrieving the dimensions of the
	 * page and dividing the height by the line spacing.
	 */

	nPageLength	= GetDeviceCaps( hPr, VERTRES );
	nPageWidth	= GetDeviceCaps( hPr, HORZRES );
	LinesPerPage	= nPageLength / LineSpace - 1;
	LinesPerPage	= LinesPerPage - 2; /* 2 lines for header */

	/*
	 * You can output only one  line at  a  time,  so you need a
	 * count of the number of lines to print.   You can retrieve
	 * the count sending the EM_GETLINECOUNT message to the edit
	 * control.
	 */

	/* Keep track of the current line on the current page */

	CurrentLine = 1;
	CurrentPage = 1;
	LineNumber  = 1;

	/*
	 * For each line  you need to advance one line space.  Also,
	 * you need to check for the end of the page and start a new
	 * page if necessary.
	 */

	Status = 0;

	for( clp=firstline(curbp) ; clp!=lastline(curbp) ; clp=lforw(clp) ) {
		if( CurrentLine == 1 ) {
			(void)emsprintf3(
					  buf,
					  ECSTR("Page #%00d, %s - %s"),
					  CurrentPage,
					  szTimeBuf,
					  curbp->b_fname
				        );
			TabbedTextOut(
					hPr,
					0,
					CurrentLine * LineSpace,
					(LPSTR)buf,
					(int)strlen( buf ),
					0,
					NULL,
					0
				     );
			MoveToEx( hPr, 0, LineSpace*2, NULL );
			LineTo( hPr, nPageWidth, LineSpace*2 ); 
		}

		(void)emsprintf1( buf, ECSTR("%05d : "), LineNumber++ );

		if( llength( clp ) >= NLINE )
			len = NLINE - 1;
		else	len = llength( clp );

		strncpy( buf + LINENBSIZE, ltext( clp ), len );
		buf[ len + LINENBSIZE ] = '\000';
		TabbedTextOut(
				hPr,
				0,
				(CurrentLine + 2) * LineSpace,
				(LPSTR)buf,
				len + LINENBSIZE,
				0,
				NULL,
				0
			     );

		if( (++CurrentLine % LinesPerPage) == 0 ) {
			CurrentLine = 1;
			CurrentPage++;
			Status = Escape( hPr, NEWFRAME, 0, 0L, 0L );
			if( Status < 0 || bAbort )
				break;
			else	SelectObject( hPr, hfPr );
		}
	}

	if( Status >= 0 && !bAbort ) {
		Escape( hPr, NEWFRAME, 0, 0L, 0L );
		Escape( hPr, ENDDOC,   0, 0L, 0L );
	}

	EnableWindow( hWnd, TRUE );

	/*
	 * Destroy the Abort dialog box
	 */

	DestroyWindow( hAbortDlgWnd );
	FreeProcInstance( lpAbortDlg );
	FreeProcInstance( lpAbortProc );

	(void)SelectObject( hPr, hOldFont );
	DeleteObject( hfPr );
	DeleteDC( hPr );

}

static	void
winprintsetup( void )
{
	DWORD	FlagSave;

	FlagSave  = pd.Flags;
	pd.Flags |= PD_PRINTSETUP | PD_USEDEVMODECOPIES;
	PrintDlg( (LPPRINTDLG)&pd );
	pd.Flags  = FlagSave;          /* Remove option */
}

static	int FAR	PASCAL
wingetfont( void )
{
	DWORD	nFlags = CF_SCREENFONTS | /* CF_FIXEDPITCHONLY | */
			 CF_ANSIONLY    | CF_EFFECTS;
	char	szFaceName[ 64 ];

	GetTextFace( hDC, 63, (char *)&szFaceName[ 0 ] );

	if( nlfFlag != CF_INITTOLOGFONTSTRUCT ) {
		nlfFlag = CF_INITTOLOGFONTSTRUCT;
		(void)strcpy( lf.lfFaceName, szFaceName );
	}

	FontChunk.lpLogFont	 = &lf;
	FontChunk.lStructSize    = sizeof( CHOOSEFONT );
	FontChunk.hwndOwner      = hWnd;
	FontChunk.hDC       	 = (HDC)NULL;	/* screen fonts */
	FontChunk.Flags          = nFlags | nlfFlag;
	FontChunk.rgbColors      = TextColor; /* RGB( 0, 0, 0 ); */
	FontChunk.lCustData      = 0L;
	FontChunk.lpTemplateName = (LPSTR)NULL;
	FontChunk.hInstance      = (HINSTANCE)NULL;
	FontChunk.lpszStyle      = (LPSTR)NULL;
	FontChunk.nFontType      = SCREEN_FONTTYPE;
	FontChunk.nSizeMin       = 0;
	FontChunk.nSizeMax       = 0;

	if( ChooseFont( &FontChunk ) ) {
		TextColor = FontChunk.rgbColors;

		DeleteObject( hFont );
		hFont = CreateFontIndirect( &lf );
		(void)SelectObject( hDC, hFont );

		winsize( TRUE );

		nlfFlag = CF_INITTOLOGFONTSTRUCT;
		return( 1 );
	} else	return( 0 );

}

static void FAR PASCAL
wingetcolor( void )
{
	static	DWORD	CustColors[ 16 ];
	RECT		rcClient;

	ColorChunk.lStructSize	= sizeof( CHOOSECOLOR );
	ColorChunk.hwndOwner	= hWnd;
	ColorChunk.hInstance	= (HWND)NULL;
	ColorChunk.Flags	= 0;
	ColorChunk.lpCustColors	= &CustColors[ 0 ];

	if( ChooseColor( &ColorChunk ) ) {
		BackColor = ColorChunk.rgbResult;
		DeleteObject( hBrush );
		hBrush	 = CreateSolidBrush( BackColor );
		SelectObject( hDC, hBrush );
#if	defined( _WIN64 )
		SetClassLongPtr( hWnd,GCLP_HBRBACKGROUND, (LONG_PTR)hBrush );
#elif	defined( _WIN32 )
		SetClassLong(    hWnd, GCL_HBRBACKGROUND, (LONG)hBrush );
#else
		SetClassWord(    hWnd, GCW_HBRBACKGROUND, (WORD)hBrush );
#endif
		GetClientRect( hWnd, (LPRECT)&rcClient );
		InvalidateRect( hWnd, &rcClient, FALSE );
		(void)redrawscreen();
		update( T );
	}

}

static	void
winupdate( char *prompt, char *line )
{
	prompt	 = prompt;
	nEditPos = (int)strlen( line );

	SetDlgItemText( hEdWnd, IDG_EDIT, line );
	SendDlgItemMessage(
			    hEdWnd,
			    IDG_EDIT,
			    EM_SETSEL,
#if	defined( _WINDOWS_SOURCE )
			    (WPARAM)nEditPos,
			    (LPARAM)nEditPos
			   );
#else
			    (WORD)0,
			    MAKELONG( nEditPos, nEditPos )
			  );
#endif
}

/*
 *	GetText Widget
 */

static	char	szReturnText[ NLINE ];
static	char	szEditBuf[ NLINE ];
static	LPSTR	PromptText;

BOOL APIENTRY
GetText( LPSTR lpPrompt, LPSTR lpOldStr, int nLen )
{
	FARPROC	lpDlg;
	BOOL	RetCode;

	nEditFlag  = TRUE;
	PromptText = lpPrompt;
loop:
	(void)strcpy( szEditBuf, lpOldStr );

	completion = COMPLETE_ONE;
	lpDlg	   = MakeProcInstance( (FARPROC)GetTextDlg, hInst );
	RetCode	   = (BOOL)DialogBox(hInst,"GetText",(HWND)NULL,(DLGPROC)lpDlg);
	FreeProcInstance( lpDlg );

	switch( RetCode ) {
	case TRUE  : (void)strncpy( lpOldStr, szReturnText, nLen );
		     RetCode = T;
		     if( completion == COMPLETE_AGAIN )
		     	 goto loop;
		     break;
	case ABORT : winmessage( "Quit" ); break;
	default    : RetCode = NIL;
	}

	nEditFlag = FALSE;

	return( RetCode );
}

/*
 *	Let user text, and return.
 */

LRESULT APIENTRY
GetTextDlg( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;
	int	i;
	int	j;

	lParam = lParam;
	hEdWnd = hDlg;

	switch( message ) {
        case WM_COMMAND:
		switch( wParam ) {
                case IDOK:
			szEditBuf[ nEditPos++ ] = '\000';
			(void)strcpy( szReturnText, szEditBuf );

			complete.flag = 0L;

			if( kbdmip != NULL ) {
				if( kbdmip+nEditPos > &kbdm[ NKBDM - 3 ] )
					return( (LRESULT)ctrlg() );
				for( i = 0 ; i < nEditPos ; ++i )
					*kbdmip++ = szEditBuf[ i ];
			}

			EndDialog( hDlg, (WORD)nEditOld );
			return( (LRESULT)nEditOld );

		case IDCANCEL:
			/* actually an ESC */
			nEditOld = TRUE;
			szEditBuf[ nEditPos++ ] = '\000';
			(void)updir( szEditBuf, SLASH );
			winupdate( NULL, szEditBuf );
			return( 0L );

		case IDG_CANCEL:
			EndDialog( hDlg, (WORD)ABORT );
			return( (LRESULT)ABORT );
		}
		break;

        case WM_INITDIALOG:
		SetWindowText( hDlg, PromptText );
		GetWindowRect( hDlg, &aRec );
		OffsetRect( &aRec, -((int)aRec.left), -((int)aRec.top) );
		MoveWindow(
			    hDlg,
			    ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
			    (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
			    (int)aRec.right,
			    (int)aRec.bottom,
			    0
			  );

		for( i = 0, j = 0 ; szEditBuf[i] != '\000' ; i++ )
		   if( szEditBuf[i] < ' ' ) {
		          szReturnText[j++]='^';
		          szReturnText[j++]=(char)((int)szEditBuf[i]+(int)'@');
		   } else szReturnText[j++]=szEditBuf[i];

		szReturnText[j] = '\000';
		nEditPos	= (int)strlen( szReturnText );

		if( completion != COMPLETE_AGAIN )
			nEditOld = FALSE;

		SetDlgItemText( hDlg, IDG_EDIT, szReturnText );
		SendDlgItemMessage(
			hDlg,
			IDG_EDIT,
			EM_SETSEL,
#if	defined( _WINDOWS_SOURCE )
			(WPARAM)nEditPos,
			(LPARAM)nEditPos
			);
#else
			(WORD)0,
			MAKELONG( nEditPos, nEditPos )
			);
#endif
		SetFocus( GetDlgItem( hDlg, IDG_EDIT ) );
		return( (LRESULT)FALSE ); /* the focus is set to a control */
	}
	return( (LRESULT)FALSE );
}

LRESULT APIENTRY
EmacsEditWndProc( HWND hEdDlg, UINT wMsg, WPARAM wParam, LPARAM lParam )
{
	static	int nQuote   = FALSE;
	static	int nControl = FALSE;

	switch( wMsg ) {

#if	defined( _CTRL3D )
	case WM_NCPAINT :
	case WM_NCACTIVATE :
		if( nCtl3D ) {
			lpCtl3dDlgFramePaint(hEdDlg,wMsg,wParam,lParam);
			return( 0L );
		}
		break;
#endif

	case WM_GETDLGCODE:
		/*
		 *	This dialog box ignore TABSTOP
		 */
		return( DLGC_WANTCHARS | DLGC_WANTTAB );

	case WM_LBUTTONDOWN :
		winbeep();
		return( 0L );

	case WM_KEYDOWN :
		switch( wParam ) {
		case VK_CONTROL:
			nControl = TRUE;
			break;
		case VK_LEFT:
			SendMessage(hEdDlg,WM_CHAR,(WPARAM)0x08,(LPARAM)0L);
			return( 0L );
		case VK_RIGHT:
			return( 0L );
		}
		break;

	case EM_SETSEL:
		/*
		 * Force selection at the end of the buffer
		 */
#if	defined( _WINDOWS_SOURCE )
		wParam = (WPARAM)nEditPos;
		lParam = (LPARAM)nEditPos;
#else
		wParam = (WORD)0;
		lParam = MAKELONG( nEditPos, nEditPos );
#endif
		break;

	case WM_KEYUP :
		switch( wParam ) {
		case VK_CONTROL:
			nControl = FALSE;
			break;
		}
		break;

	case WM_CHAR :
		if( nQuote == FALSE ) switch( wParam ) {
		case 0x11:
			if( nControl == TRUE )
				nQuote = TRUE;
			return( 0L );
		case 0x03: /* ^C */
			nEditPos	= 0;
			nEditOld	= TRUE;
			szEditBuf[ 0 ]	= '\000';
			(void)winupdate( NULL, szEditBuf );			
			return( 0L );
		case 0x07: /* ^G */
			szEditBuf[ 0 ] = '\000';
			winbeep();
			SendMessage(
				     hEdWnd,
				     WM_COMMAND,
				     IDG_CANCEL,
				     MAKELPARAM( hEdDlg, 0 )
				   );
			EndDialog( hEdWnd, (WORD)ABORT );
			SetFocus( hWnd );
			return( 0L );
		case 0x12 : /* ^R */
		case 0x13 : /* ^S */
			SendMessage(
				     hEdWnd,
				     WM_COMMAND,
				     IDOK,
				     MAKELPARAM( hEdDlg, 0 )
				   );
			EndDialog( hEdWnd, (WORD)IDOK );
			SetFocus( hWnd );
			return( 0L );

		case 0x08:
			nEditOld = TRUE;

			if( nEditPos )
		 		nEditPos--;
			if( szEditBuf[ nEditPos ] < ' ' )
				CallWindowProc(
						(WNDPROC)EditWndProc,
						hEdDlg,
						wMsg,
						wParam,
						lParam
					      );
			goto ret;
		}

		if( complete.fn==filematch && nEditPos > 0 && wParam == ':' ) {
			/*
			 * Check for device change
			 */

			wParam   = szEditBuf[ nEditPos-1 ];
			nEditPos = 0;
			szEditBuf[ nEditPos++ ] = (char)wParam;
			szEditBuf[ nEditPos   ] = '\000';
			(void)winupdate( NULL, szEditBuf );			
			wParam   = ':';
			nEditOld = TRUE;
		}

		if( nEditOld == FALSE ) {
			if( complete.fn == filematch
			    && (char)wParam != ' ' && (char)wParam != '\t' ) {
				if( (char)wParam=='/' || (char)wParam=='\\' ) {
					nEditPos = 0;
					nEditOld = TRUE;
					SetDlgItemText( hEdWnd, IDG_EDIT, "" );
				}
				if( nEditPos>0 && szEditBuf[nEditPos-1]=='/' )
					nEditOld = TRUE;
			}
			else	if( complete.fn != filematch ) {
				nEditPos = 0;
				SetDlgItemText( hEdWnd, IDG_EDIT, "" );
			}
		}

		nEditOld = TRUE;
		nQuote   = FALSE;

		if( ((char)wParam==' '||(char)wParam=='\t') && complete.flag ) {
			char	*s;
			szEditBuf[ nEditPos++ ] = '\000';
			completion = COMPLETE_ONE;
			nGetCode   = TRUE;

			if( (s=(*complete.fn)(PromptText,szEditBuf)) != NULL ) {
				nGetCode   = FALSE;
				if( completion != COMPLETE_AGAIN ) {
					completion = COMPLETE_ONE;
					(void)strcpy( szReturnText, s );
					complete.flag = 0L;
					EndDialog( hEdWnd, (WORD)TRUE );
					SetFocus( hWnd );
					return( TRUE );
				} else	{
					completion = COMPLETE_ONE;
					(void)strcpy( szEditBuf, s );
					(void)winupdate( NULL, s );
					return( 0L );
				}
			} else	{
				nGetCode      = FALSE;
				complete.flag = 0;
				completion    = COMPLETE_FAIL;
				SendMessage(
					     hEdWnd,
					     WM_COMMAND,
					     IDG_CANCEL,
					     MAKELPARAM( hEdDlg, 0 )
					   );
				EndDialog( hEdWnd, (WORD)ABORT );
				SetFocus( hWnd );
				return( 0L );
			}
		}

		if( (szEditBuf[ nEditPos++ ] = (char)wParam) < ' ' ) {
			wParam += (int)'@';
			CallWindowProc(
					(WNDPROC)EditWndProc,
					hEdDlg,
					wMsg,
					(WPARAM)'^',
					lParam
				      );
		}
	}

ret:
	return(CallWindowProc((WNDPROC)EditWndProc,hEdDlg,wMsg,wParam,lParam));
}

/*
 *	Confirm for save before exit.
 */

static	CMD
winconfirm( LPSTR lpPrompt )
{
	FARPROC	lpDlg;
	BOOL	RetCode;

	PromptText = lpPrompt;
	lpDlg	   = MakeProcInstance( (FARPROC)WarningDlg, hInst );
	RetCode	   = (BOOL)DialogBox(hInst,"Confirm",(HWND)NULL,(DLGPROC)lpDlg);
	FreeProcInstance( lpDlg );

	return( RetCode );
}

/*
 *	Warn user
 */

static	CMD
winwarning( LPSTR lpPrompt )
{
	FARPROC	lpDlg;
	BOOL	RetCode;

	PromptText = lpPrompt;
	lpDlg	   = MakeProcInstance( (FARPROC)WarningDlg, hInst );
	RetCode	   = (BOOL)DialogBox(hInst,"Warning",(HWND)NULL,(DLGPROC)lpDlg);
	FreeProcInstance( lpDlg );

	return( RetCode );
}

/*
 *	Warn user.
 */

LRESULT APIENTRY
WarningDlg( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;
	lParam = lParam;
	wParam = wParam;

	switch( message ) {
        case WM_COMMAND:
		switch( wParam ) {
                case IDOK:
			EndDialog( hDlg, (WORD)T );
			return( (LRESULT)T );

		case IDNO:
			EndDialog( hDlg, (WORD)NIL );
			return( (LRESULT)NIL );

		case ID_QUIT:
			EndDialog( hDlg, (WORD)ABORT );
			return( (LRESULT)ABORT );
		}
		break;

        case WM_INITDIALOG:
		GetWindowRect( hDlg, &aRec );
		OffsetRect( &aRec, -((int)aRec.left), -((int)aRec.top) );
		MoveWindow(
			    hDlg,
			    ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
			    (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
			    (int)aRec.right,
			    (int)aRec.bottom,
			    0
			  );
		SetDlgItemText( hDlg, ID_WARNING, PromptText );
		return( (LRESULT)FALSE ); /* the focus is set to a control */
	}
	return( (LRESULT)FALSE );
}

/*
 *	Replace Widget
 */

static	LPSTR	OldText;
static	LPSTR	NewText;
static	LPSTR	OldStr;
static	LPSTR	NewStr;
static	int	nRLength;

/*
 *	Let user change a selection, and return.
 */

int FAR PASCAL 
Replace( LPSTR lpOldStr, LPSTR lpNewStr, LPSTR lpOldPat, LPSTR lpNewPat, UINT nLength )
{
	FARPROC	lpDlg;
	BOOL	RetCode;

	(void)strcpy( szReturnText, lpOldStr );

	OldStr	     = lpOldStr;
	NewStr	     = lpNewStr;
	OldText	     = lpOldPat;
	NewText	     = lpNewPat;
	nRLength     = nLength;

	lpDlg   = MakeProcInstance( (FARPROC)ReplaceDlg, hInst );
	RetCode = (BOOL)DialogBox(hInst,"Replace",(HWND)NULL,(DLGPROC)lpDlg);
	FreeProcInstance( lpDlg );

	if( RetCode == ABORT )
		winmessage( "Quit" );

	return( RetCode );

}

LRESULT APIENTRY
ReplaceDlg( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;

	lParam = lParam;
	hEdWnd = hDlg;

	switch( message ) {
        case WM_COMMAND:
		switch( wParam ) {
                case IDOK:
			if( GetFocus() == GetDlgItem( hDlg, IDR_OPAT ) ) {
				SetFocus( GetDlgItem( hDlg, IDR_NPAT ) );
				winbeep();
				return( (LRESULT)FALSE );
			}
			GetDlgItemText( hDlg, IDR_NPAT, NewText, nRLength );
			GetDlgItemText( hDlg, IDR_OPAT, OldText, nRLength );
			EndDialog( hDlg, (WORD)TRUE );
			return( (LRESULT)TRUE );

		case IDCANCEL:
			EndDialog( hDlg, (WORD)ABORT );
			return( (LRESULT)ABORT );
		}
		break;

        case WM_INITDIALOG:
		GetWindowRect( hDlg, &aRec );
		OffsetRect( &aRec, -((int)aRec.left), -((int)aRec.top) );
		MoveWindow(
			    hDlg,
			    ((GetSystemMetrics(SM_CXSCREEN)-aRec.right)/2+4)&~7,
			    (GetSystemMetrics(SM_CYSCREEN)-aRec.bottom)/2,
			    (int)aRec.right,
			    (int)aRec.bottom,
			    0
			  );
		SetDlgItemText( hDlg, IDR_OLD, OldStr );
		SetDlgItemText( hDlg, IDR_NEW, NewStr );
		nEditPos = 0;
		nEditOld = FALSE;
		SendDlgItemMessage(
			hDlg,			/* dialog handle          */
			IDR_OPAT,		/* where to send message  */
			EM_SETSEL,		/* select characters      */
			(WORD)0,		/* additional information */
			MAKELONG( 0, 0x7fff )   /* entire contents        */
			);
		SetFocus( GetDlgItem( hDlg, IDR_OPAT ) );
		return( (LRESULT)FALSE ); /* the focus is set to a control */
	}
	return( (LRESULT)FALSE );
}

static	void
winparams( void )
{
	FARPROC	lpfn;

	lpfn = MakeProcInstance( (FARPROC)ParamsDlgProc, hInst );
	DialogBox( hInst, "ParamsDlgBox", (HWND)NULL, (DLGPROC)lpfn );
	FreeProcInstance( lpfn );

}

static	void
winbuttonbar( void )
{
	if( nVisibleBar == FALSE ) {
		nBarSize = 0;
		ShowWindow( hBar, SW_HIDE );
		CheckMenuItem( GetMenu( hWnd ), IDM_BUTTONBAR, MF_UNCHECKED );
		SendMessage( hWnd, WM_PAINT, 0, 0L );
	} else	{
		nBarSize = GetSystemMetrics( SM_CYMENU );
		ShowWindow( hBar, SW_SHOW );
		CheckMenuItem( GetMenu( hWnd ), IDM_BUTTONBAR, MF_CHECKED );
		SendMessage( hWnd, WM_PAINT, 0, 0L );
	}
}


static	void	SetUpParameters( HWND hwnd );

LRESULT APIENTRY
ParamsDlgProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
	PAINTSTRUCT	ps;

	lParam = lParam;

	switch( msg ) {

       	case WM_CREATE:
      		break;

	case WM_INITDIALOG:
		CheckDlgButton( hwnd, ID_BACKUP_BEFORE_WRITING, (UINT)backup_before_writing );
		CheckDlgButton( hwnd, ID_SET_SHOW_GRAPHIC, (UINT)set_show_graphic );
		CheckDlgButton( hwnd, ID_CASE_FOLD_SEARCH, (UINT)case_sensitivity );
		CheckDlgButton( hwnd, ID_MAXIMIZED, (UINT)nMaximized );
		CheckDlgButton( hwnd, ID_GNUMODE, (UINT)gnu_compatible );
		CheckDlgButton( hwnd, ID_BINMODE, (UINT)binary_mode );
		SetDlgItemInt(  hwnd, ID_TAB_DISPLAY, tab_display, TRUE );
		SetDlgItemInt(  hwnd, ID_TAB_SIZE, tab_size, TRUE );
		SetDlgItemText( hwnd, ID_MAKE_NAME, make_name );
		SetDlgItemText( hwnd, ID_MAKE_ARGS, make_arg );
		SetDlgItemText( hwnd, ID_HELP_FILE1, szHelpFile1 );
		SetDlgItemText( hwnd, ID_HELP_FILE2, szHelpFile2 );
		break;

	case WM_PAINT:
		BeginPaint( hwnd, &ps );
		EndPaint( hwnd, &ps );
		break;

	case WM_CLOSE:
        	/* Closing the Dialog behaves the same as Cancel */
		PostMessage( hwnd, WM_COMMAND, ID_PARAM_CANCEL, 0L );
		break;

	case WM_COMMAND:
		switch( wParam ) {

		case ID_PARAM_SAVE:
			SetUpParameters( hwnd );
			EndDialog( hwnd, 0 );
			return( TRUE );

		case ID_PARAM_CANCEL:
			EndDialog( hwnd, 0 );
			break;

		case ID_HELP_SEARCH1:
		case ID_HELP_SEARCH2:
			ofn.hwndOwner	= hwnd;
			ofn.lpstrFilter	= "Help Files (*.HLP)\0*.hlp\0";

			if( GetOpenFileName( (LPOPENFILENAME)&ofn ) != 0 )
			    if( wParam == ID_HELP_SEARCH1 )
			    	 SetDlgItemText(hwnd,ID_HELP_FILE1,szFileName);
			    else SetDlgItemText(hwnd,ID_HELP_FILE2,szFileName);

			ofn.hwndOwner	= hWnd;
			ofn.lpstrFilter	= szFilterSpec;

			break;

		default:
			return( FALSE );
		}
		break;

	case WM_DESTROY :
	    	EndDialog( hwnd, 0 );

	default:
		return( FALSE );
	}

	return( TRUE );
}

static	void
SetUpParameters( HWND hwnd )
{
	BOOL 	bTranslated;

	backup_before_writing	= (IsDlgButtonChecked(
						      hwnd,
						      ID_BACKUP_BEFORE_WRITING
				 		     ) ? T : NIL);
	set_show_graphic	= (IsDlgButtonChecked(
						      hwnd,
						      ID_SET_SHOW_GRAPHIC
						    ) ? T : NIL);
	case_sensitivity	= (IsDlgButtonChecked(
						      hwnd,
						      ID_CASE_FOLD_SEARCH
						    ) ? T : NIL);
	nMaximized		= (UINT)IsDlgButtonChecked(
						      hwnd,
						      ID_MAXIMIZED
						    );
	gnu_compatible		= (IsDlgButtonChecked(
						       hwnd,
						       ID_GNUMODE
						     ) ? T : NIL);
	binary_mode		= (IsDlgButtonChecked(
						       hwnd,
						       ID_BINMODE
						     ) ? T : NIL);
	tab_size		= GetDlgItemInt(
						 hwnd,
						 ID_TAB_SIZE,
						 &bTranslated,
						 TRUE
						);
	tab_display		= GetDlgItemInt(
						 hwnd,
						 ID_TAB_DISPLAY,
						 &bTranslated,
						 TRUE
						);

	GetDlgItemText( hwnd, ID_MAKE_NAME,  make_name,   NCMDN  );
	GetDlgItemText( hwnd, ID_MAKE_ARGS,  make_arg,    NPAT   );
	GetDlgItemText( hwnd, ID_HELP_FILE1, szHelpFile1, NFILEN );
	GetDlgItemText( hwnd, ID_HELP_FILE2, szHelpFile2, NFILEN );

	if( tab_size <= 0 )
		tab_size = 8;
}

static void
windrawbox( HDC hDC, RECT *rcRect )
{
	HPEN	hPen;
	HPEN	hOldPen;

	/*
	 *	Draw WHITE line on the bottom and right of the rectangle
	 */

	hPen    = CreatePen( PS_SOLID, 1, COLOR_WHITE );
	hOldPen = (HPEN)SelectObject( hDC, hPen );
	MoveToEx( hDC, rcRect->left,  rcRect->bottom, NULL );
	LineTo(   hDC, rcRect->right, rcRect->bottom       );
	MoveToEx( hDC, rcRect->right, rcRect->bottom, NULL );
	LineTo(   hDC, rcRect->right, rcRect->top          );
	(void)SelectObject( hDC, hOldPen );
	DeleteObject( hPen );

	/*
	 *	Draw Grey line on top and left of the rectangle
	 */

	hPen = CreatePen( PS_SOLID, 1, COLOR_GRAY );
	(void)SelectObject( hDC, hPen );
	MoveToEx( hDC, rcRect->left,  rcRect->top, NULL );
	LineTo(   hDC, rcRect->right, rcRect->top       );
	MoveToEx( hDC, rcRect->left,  rcRect->top, NULL );
	LineTo(   hDC, rcRect->left,  rcRect->bottom    );
	(void)SelectObject( hDC, hOldPen );
	DeleteObject( hPen );

}

LRESULT APIENTRY
fBarWndProc( HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam )
{
	PAINTSTRUCT	ps;
	HBRUSH		hBrush;
	HBRUSH		hOldBrush;
	HPEN		hOldPen;
	HPEN		hPen;

	switch( message ) {
	case WM_CREATE :
#if	defined( CREATE_OWN_FONT )
		lfFBar.lfHeight		= -10*GetDeviceCaps(hDC,LOGPIXELSY)/72;
		lfFBar.lfWidth		= 0;
		lfFBar.lfEscapement	= 0;
		lfFBar.lfOrientation	= 0;
		lfFBar.lfWeight		= FW_NORMAL;
		lfFBar.lfItalic		= 0;
		lfFBar.lfUnderline	= 0;
		lfFBar.lfStrikeOut	= 0;
		lfFBar.lfCharSet	= ANSI_CHARSET;
		lfFBar.lfOutPrecision	= OUT_DEFAULT_PRECIS;
		lfFBar.lfClipPrecision	= CLIP_DEFAULT_PRECIS;
		lfFBar.lfQuality	= DEFAULT_QUALITY;
		lfFBar.lfPitchAndFamily	= VARIABLE_PITCH | FF_SWISS;
		if( winversion() >= 310 )
			wsprintf( lfFBar.lfFaceName, "Arial" );
		else	wsprintf( lfFBar.lfFaceName, "MS Sans Serif" );
		hfFont = CreateFontIndirect( &lfFBar );
#else
		hfFont = GetStockObject( SYSTEM_FONT );
#endif
		break;

	case WM_COMMAND:
		return( FALSE );

	case WM_PAINT :
		BeginPaint( hwnd, &ps );

		/*
		 * Fill the area whith LIGHT GRAY Brush.
		 */

		hBrush    = CreateSolidBrush( COLOR_LIGHTGRAY );
		hOldBrush = (HBRUSH)SelectObject( ps.hdc, hBrush );
		FillRect( ps.hdc, (LPRECT)&ps.rcPaint, hBrush );
		(void)SelectObject( ps.hdc, hOldBrush );
		DeleteObject( hBrush );

		/*
		 * Draw WHITE line on the top.
		 */

		hPen      = CreatePen( PS_SOLID, 1, COLOR_WHITE );
		hOldPen   = (HPEN)SelectObject( ps.hdc, hPen );
		MoveToEx( ps.hdc, 0, 0, NULL );
		LineTo( ps.hdc, nParentWidth, 0 );
		(void)SelectObject( ps.hdc, hOldPen );
		DeleteObject( hPen );

		/*
		 * Draw GRAY line on the bottom.
		 */

		hPen = CreatePen( PS_SOLID, 1, COLOR_LIGHTGRAY );
		(void)SelectObject( ps.hdc, hPen );
		MoveToEx( ps.hdc, 0, fBarSize + 1, NULL );
		LineTo( ps.hdc, nParentWidth, fBarSize + 1 );
		(void)SelectObject( ps.hdc, hOldPen );
		DeleteObject( hPen );

		RCRepMode.right   = nParentWidth     - 10;
		RCRepMode.left	  = RCRepMode.right  - nRepModeWidth;

		RCCapsMode.right  = RCRepMode.left   - 5;
		RCCapsMode.left	  = RCCapsMode.right - nCapsModeWidth;

		RCNumLMode.right  = RCCapsMode.left  - 5;
		RCNumLMode.left	  = RCNumLMode.right - nNumLModeWidth;

		RCMsgBox.left	  = RCEditY.right    + 10;
		RCMsgBox.right	  = RCNumLMode.left  - 10;

		windrawbox( ps.hdc, &RCEditX );
		windrawbox( ps.hdc, &RCEditY );

		if( RCNumLMode.left > (RCEditY.right + 20) ) {
			windrawbox( ps.hdc, &RCRepMode  );
			windrawbox( ps.hdc, &RCCapsMode );
			windrawbox( ps.hdc, &RCNumLMode );
			windrawbox( ps.hdc, &RCMsgBox   );
		}

		winshowcol( TRUE );
		winshowrow( TRUE );

		winshowstate( VK_INSERT,  "RPL", &RCRepMode  );
		winshowstate( VK_CAPITAL, "CAP", &RCCapsMode );
		winshowstate( VK_NUMLOCK, "NUM", &RCNumLMode );

		EndPaint( hwnd, &ps );
		break;

	default:
		return( DefWindowProc( hwnd, message, wParam, lParam ) );
	}

	return( FALSE );
}

static	void
winshowcol( int f )
{
	static	char	szXval[16];
	static	long	nOldX;

	if( (f == TRUE) || (nOldX != (current_col + 1)) ) {
		HDC	hfDC;
		RECT	RCClr;
		HFONT	hOldFont;

		RCClr.top    = RCEditX.top + 1;
		RCClr.bottom = RCEditX.bottom;
		RCClr.right  = RCEditX.right - 1;
		RCClr.left   = RCEditX.left + 2;

		hfDC = GetDC( fBar );

		hOldFont = (HFONT)SelectObject( hfDC, hfFont );
		SetBkColor( hfDC, COLOR_LIGHTGRAY );
		SetTextColor( hfDC, COLOR_LIGHTGRAY );
		DrawText( hfDC, szXval, -1, &RCClr, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );
		nOldX = current_col + 1;
		SetTextColor( hfDC, COLOR_BLACK );
		wsprintf( szXval, " Col %ld", nOldX );
		DrawText( hfDC, szXval, -1, &RCClr, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );
		(void)SelectObject( hfDC, hOldFont );

		ReleaseDC( fBar, hfDC );
	}
}

static	void
winshowrow( int f )
{
	static	char	szYval[16];
	static	long	nOldY;

	if( (f == TRUE) || (nOldY != current_row) ) {
		HDC	hfDC;
		HFONT	hOldFont;
		RECT	RCClr;

		RCClr.top    = RCEditY.top + 1;
		RCClr.bottom = RCEditY.bottom;
		RCClr.right  = RCEditY.right - 1;
		RCClr.left   = RCEditY.left + 2;

		hfDC = GetDC( fBar );
		hOldFont = (HFONT)SelectObject( hfDC, hfFont );
		SetBkColor( hfDC, COLOR_LIGHTGRAY );
		SetTextColor( hfDC, COLOR_LIGHTGRAY );
		DrawText( hfDC, szYval, -1, &RCClr, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );
		nOldY = current_row;
		SetTextColor( hfDC, COLOR_BLACK );
		wsprintf( szYval, " Li %ld", nOldY );
		DrawText( hfDC, szYval, -1, &RCClr, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );
		(void)SelectObject( hfDC, hOldFont );

		ReleaseDC( fBar, hfDC );
	}
}

static	void
winshowstate( WORD state, char *key, RECT *rcRect )
{
	if( RCNumLMode.left > (RCEditY.right + 10) ) {
		HDC	hfDC;
		HFONT	hOldFont;
		RECT	RCClr;

		RCClr.top    = rcRect->top + 1;
		RCClr.bottom = rcRect->bottom;
		RCClr.right  = rcRect->right - 1;
		RCClr.left   = rcRect->left + 2;

		hfDC = GetDC( fBar );

		hOldFont = (HFONT)SelectObject( hfDC, hfFont );
		SetBkColor( hfDC, COLOR_LIGHTGRAY );
		SetTextColor(
			      hfDC,
			      (((GetKeyState( state ) & 0x01) == 1) ?
			      	COLOR_BLACK :
				COLOR_LIGHTGRAY)
			    );
		DrawText(
			  hfDC,
			  key,
			  -1,
			  &RCClr,
			  DT_CENTER | DT_VCENTER | DT_SINGLELINE
			);

		(void)SelectObject( hfDC, hOldFont );

		ReleaseDC( fBar, hfDC );
	}
}

extern	CMD	NTansitooem( EDLINE *lp );
extern	CMD	NToemtoansi( EDLINE *lp );

CMD
NTansitooem( EDLINE *lp )
{
#if	defined( _WINDOWS_SOURCE )
	CharToOemBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#else
	AnsiToOemBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#endif
	return( T );
}

CMD
NToemtoansi( EDLINE *lp )
{
#if	defined( _WINDOWS_SOURCE )
	OemToCharBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#else
	OemToAnsiBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#endif
	return( T );
}
