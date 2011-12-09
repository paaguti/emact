#if	!defined( lint )
static	char rcsid[] = "$Id: ntterm.c,v 1.7 2008/11/29 08:57:38 jullien Exp $";
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

#if	defined( _WIN32 ) || defined( _WIN64 )

#if	defined( _MSC_VER ) && (_MSC_VER >= 1400)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

/*
 *	The routines in this file provide support for WINxx (NT) terminal
 */

#include "emacs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#if	defined( _ADD_TO_RECENT_DOCS )
#include <shlobj.h>
#endif
#include <process.h>
#include <io.h>
#include <signal.h>
#include <time.h>
#include <fcntl.h>
#if	!defined( __GNUC__ )
#include <tchar.h>
#else
#if	!defined( _T )
#define	_T(x)	x
#endif
#endif

#define	NTswitchscreen	switchscreen

#define	NT_MAXTITLE	128

static	HANDLE		NTcin;
static	HANDLE		NTcout;
static	DWORD		NToldmode;
static	WORD		NTcurcolor;
static	WORD		NTinvcolor;
static	WORD		NToldattrib;
static	DWORD		NToldcursize;
static	COORD		NToldcoord;
static	SMALL_RECT	NToldsize;
static	RECT		NToldwrect;
static	HWND		NThwnd;
static	HINSTANCE	NThinst;
static	HICON		NThicon;
static	HICON		NTholdicon;
static	EMCHAR		NToldtitle[ NT_MAXTITLE ] = { 0 };
static	int		NTrow	   = 0;
static	int		NTcol	   = 0;
static	int		NTstroke   = 0;
static	int		NTinitflg  = 0;
static	int		NTforceraw = 0;

/*
 *	Standard terminal interface dispatch table.
 */

static	void	NTopen( void );
static	void	NTclose( void );
static	int	NTgetc( void );
static	void	NTputc( int c );
static	void	NTputs( EMCHAR *s, int n );
static	void	NTflush( void );
static	void	NTmove( int x, int y );
static	void	NTeeol( void );
static	void	NTeeop( void );
static	void	NTbeep( void );
static	void	NTsi( void );
static	void	NTcshow( int flag );
static	int	NTcheck( void );
static	void	NTrawmode( void );
static	EMCHAR *NTtitle( EMCHAR *buf, EMCHAR *fname );
static	void	NTmovemousebottom( void );
static	void	NTclippaste( void );
static	void	NTclipcopy( void );
static	void	NThelp( DWORD key, DWORD mod );
static	int	NTsystem( const EMCHAR *cmd );
static	HDC	NTgetprinterdc( void );
static	void	NTprint( void );
static	void	NTsighandler( int sig );
static	void	NTbold( void );
static	void	NTbigcursor( void );
static	void	NTsmallcursor( void );
static	HICON	NTloadicon( void );
static	void	NTseticon( HICON hIcon );
static	HWND	NTconsolewin( void );

extern	CMD	NTansitooem( EDLINE *lp );
extern	CMD	NToemtoansi( EDLINE *lp );

#if	defined( _ADD_TO_RECENT_DOCS )
extern	CMD	NTaddrecent( EMCHAR *file );
#endif

#if	defined( _WINCONSOLE )
extern	BOOL	NTcreateconsole( void );
#endif

#if	!defined( MOUSE_WHEELED )
#define	MOUSE_WHEELED	0x0004
#endif


#if	defined( _USE_SIG_HANDLER )
static	BOOL	WINAPI NTsighandler( DWORD dwCtrlType );
#endif

TERM	term	= {
	0,
	0,
	0,
	NIL,
	NTopen,
	NTclose,
	NTgetc,
	NTputc,
	NTputs,
	NTflush,
	NTmove,
	NTeeol,
	NTeeop,
	NTbeep,
	NTsi,
	NTsi,
	NTcshow,
	NTcheck,
	NTrawmode
};

#define	NTRGB( r, g, b )	((r) | (g) | (b))

static	WORD	NTbackcolor[] = {
	NTRGB( 0,              0,                0               ),
	NTRGB( 0,              0,                BACKGROUND_BLUE ),
	NTRGB( 0,              BACKGROUND_GREEN, 0               ),
	NTRGB( 0,              BACKGROUND_GREEN, BACKGROUND_BLUE ),
	NTRGB( BACKGROUND_RED, 0,                0               ),
	NTRGB( BACKGROUND_RED, 0,                BACKGROUND_BLUE ),
	NTRGB( BACKGROUND_RED, BACKGROUND_GREEN, 0               ),
	NTRGB( BACKGROUND_RED, BACKGROUND_GREEN, BACKGROUND_BLUE ),
};

static	WORD	NTforecolor[] = {
	NTRGB( 0,              0,                0               ),
	NTRGB( 0,              0,                FOREGROUND_BLUE ),
	NTRGB( 0,              FOREGROUND_GREEN, 0               ),
	NTRGB( 0,              FOREGROUND_GREEN, FOREGROUND_BLUE ),
	NTRGB( FOREGROUND_RED, 0,                0               ),
	NTRGB( FOREGROUND_RED, 0,                FOREGROUND_BLUE ),
	NTRGB( FOREGROUND_RED, FOREGROUND_GREEN, 0               ),
	NTRGB( FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE ),
};

#define	NTLINENBSIZE	8

#if	defined( _USE_SIG_HANDLER )
static	BOOL WINAPI
NTsighandler( DWORD dwCtrlType )
{
	(void)dwCtrlType;

	SetWindowPos(
		      NThwnd,
		      HWND_TOP,
		      0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_SHOWWINDOW
		    );

	SetFocus( NThwnd );

	if( exitemacs() == T )
		return( TRUE );
	else	return( TRUE );
}
#endif

static	void
NTsighandler( int sig )
{
	(void)signal( sig, NTsighandler );

#if	defined( SIGBREAK )
	if( sig == SIGINT || sig == SIGBREAK ) {
#else
	if( sig == SIGINT ) {
#endif
		INPUT_RECORD	iBuf;
		DWORD		dwInputEvents = 1;

		iBuf.EventType			      = KEY_EVENT;
		iBuf.Event.KeyEvent.bKeyDown	      = TRUE;
		iBuf.Event.KeyEvent.wRepeatCount      = 1;
		iBuf.Event.KeyEvent.dwControlKeyState = LEFT_CTRL_PRESSED;
		iBuf.Event.KeyEvent.wVirtualKeyCode   = 'C';
#if	defined(__GNUC__) && !defined(__CYGWIN32__) && !defined(__MINGW32__)
		iBuf.Event.KeyEvent.AsciiChar	      = 3;
#else
		iBuf.Event.KeyEvent.uChar.AsciiChar   = 3;
#endif
		WriteConsoleInput( NTcin, &iBuf, 1, &dwInputEvents );
	}
}

typedef HWND (*MYPROC)(void);

#define	MAX_WINDOW_TITLE	64

static HWND
NTconsolewin( void )
{
#if	defined( HAVE_GETCONSOLEWINDOW )
	return( GetConsoleWindow() );
#else
	HINSTANCE hLib;
	MYPROC	  ProcAdd;
	HWND	  hWnd = NULL;

	/*
	 * Get a handle to the DLL module.
	 */

	hLib = LoadLibrary( _T("Kernel32") );

	/*
	 * If the handle is valid, try to get the function address.
	 */

	if( hLib != NULL ) {
		ProcAdd = (MYPROC)GetProcAddress( hLib, "GetConsoleWindow" );

		/*
		 * If the function address is valid, call the function.
		 */

		if( ProcAdd != NULL )
			hWnd = ProcAdd();

		/*
		 * Free the DLL module.
		 */

		(void)FreeLibrary( hLib );
	}

	/*
	 * If  it  fails  (GetConsoleWindow  is  not  defined in this
	 * Windows version), try to find the window handle using  the
	 * title name.
	 */

	if( hWnd == (HWND)NULL ) {
		TCHAR	newTitle[ MAX_WINDOW_TITLE ];

#if	defined( __GNUC__ ) && !defined( _UNICODE )
		sprintf(newTitle,_T("Console PID=%x"),
			(int)GetCurrentProcessId());
#else
		_stprintf(newTitle,_T("Console PID=%x"),
			  (int)GetCurrentProcessId());
#endif
		SetConsoleTitle( (LPTSTR)&newTitle[0] );
		Sleep( 100 );
		hWnd = FindWindow( NULL, newTitle );
	}

	return( hWnd );
#endif
}

static	void
NTopen( void )
{
	OSVERSIONINFO		   osvInfo;
	CONSOLE_SCREEN_BUFFER_INFO csInfo;
	CONSOLE_CURSOR_INFO 	   ccInfo;
	int			   fg = (foreground_color % 8);
	int			   bg = (background_color % 8);
	char*			   cp;

	if( NThwnd == (HWND)NULL )
		NThwnd = NTconsolewin();

	NTcout = CreateFile(
			      _T( "CONOUT$" ),
			      GENERIC_READ    | GENERIC_WRITE,
			      FILE_SHARE_READ | FILE_SHARE_WRITE,
			      NULL,
			      OPEN_EXISTING,
			      0,
			      NULL
			    );

	NTcin	= CreateFile(
			      _T( "CONIN$" ),
			      GENERIC_READ    | GENERIC_WRITE,
			      FILE_SHARE_READ | FILE_SHARE_WRITE,
			      NULL,
			      OPEN_EXISTING,
			      0,
			      NULL
			    );

	if( NTcout != INVALID_HANDLE_VALUE ) {
		GetConsoleScreenBufferInfo( NTcout, &csInfo );
		NToldattrib = csInfo.wAttributes;
	} else	NToldattrib = (WORD)(NTforecolor[ fg ] | NTbackcolor[ bg ]);

	NTrow		 = 0;
	NTcol		 = 0; 
	NTcurcolor	 = (WORD)(NTforecolor[ fg ] | NTbackcolor[ bg ]);
	NTinvcolor	 = (WORD)(NTforecolor[  0 ] | NTbackcolor[  7 ]);

	if( bold_font == T )
		NTcurcolor |= FOREGROUND_INTENSITY;

	GetWindowRect( NThwnd, &NToldwrect );

	GetConsoleScreenBufferInfo( NTcout, &csInfo );

	/*
	 *	Get system information.
	 */

	osvInfo.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
	GetVersionEx( &osvInfo );

	if( NTinitflg == 0 ) {
		NToldcoord.X	 = csInfo.dwSize.X;
		NToldcoord.Y	 = csInfo.dwSize.Y;
	
		NToldsize.Top	 = csInfo.srWindow.Top;
		NToldsize.Left	 = csInfo.srWindow.Left;
		NToldsize.Bottom = csInfo.srWindow.Bottom;
		NToldsize.Right	 = csInfo.srWindow.Right;
	}

#if	defined( _CHANGE_BUFFER_SIZE )
	if( osvInfo.dwPlatformId == VER_PLATFORM_WIN32_NT ) {
		csInfo.dwSize.X  = NLINE;	/* used to be 256 */
		SetConsoleScreenBufferSize( NTcout, csInfo.dwSize );
		TTYncol = csInfo.srWindow.Right  - csInfo.srWindow.Left + 1;
		TTYnrow = csInfo.srWindow.Bottom - csInfo.srWindow.Top;
	} else	{
		TTYncol = csInfo.dwSize.X;
		TTYnrow = csInfo.dwSize.Y - 1;
	}
#else
	TTYncol = csInfo.srWindow.Right  - csInfo.srWindow.Left + 1;
	TTYnrow = csInfo.srWindow.Bottom - csInfo.srWindow.Top;
#endif

	NTinitflg	   = 1;

	widget.w_title     = NTtitle;
	widget.w_clipcopy  = NTclipcopy;
	widget.w_clippaste = NTclippaste;

	printer.p_print	   = NTprint;

	GetConsoleMode( NTcin, &NToldmode );
	NTrawmode();

	if( (cp = getenv( "EMACSCP" )) != NULL ) {
		unsigned int newcp = (unsigned int)atoi( cp );
		SetConsoleCP( newcp );
		SetConsoleOutputCP( newcp );
	}

	SetFileApisToOEM();

	if( NToldtitle[ 0 ] != 0 )
	       SetConsoleTitle( (LPTSTR)NToldtitle );

#if	defined( _CONSOLE_HANDLER )
	SetConsoleCtrlHandler( NTsighandler, TRUE );
#endif

	(void)signal( SIGINT,   NTsighandler );	/* user break		*/

#if	defined( SIGBREAK )
	(void)signal( SIGBREAK, NTsighandler );	/* user Ctrl-break	*/
#endif

	GetConsoleCursorInfo( NTcout, &ccInfo );
	NToldcursize  = ccInfo.dwSize;
	NTbigcursor();

	/*
	 *	pipes hang on Windows 95.
	 */

	if( osvInfo.dwPlatformId == VER_PLATFORM_WIN32_NT )
		pipe_process = T;
	else	pipe_process = NIL;

#if	defined( _WIN64 )
	NThinst = (HINSTANCE)GetWindowLongPtr( NThwnd, GWLP_HINSTANCE );
#else
	NThinst = (HINSTANCE)GetWindowLong( NThwnd, GWL_HINSTANCE );
#endif
	NThicon = NTloadicon();

#if	!defined( _WINCONSOLE )
	if( NThicon != (HICON)NULL )
		NTseticon( NThicon );
#endif

	TTYinit   = T;
	mouseflag = T;

}

static	void
NTclose( void )
{
	CONSOLE_CURSOR_INFO 	   ccInfo;

	GetConsoleCursorInfo( NTcout, &ccInfo );
	ccInfo.dwSize   = NToldcursize;
	ccInfo.bVisible = TRUE;
	SetConsoleCursorInfo( NTcout, &ccInfo );

	FlushConsoleInputBuffer( NTcin );
	Sleep( 100 );
	SetConsoleMode( NTcin, NToldmode );

	/*
	 *	Restore screen size BEFORE !!
	 */

	SetConsoleWindowInfo( NTcout, TRUE, &NToldsize );

	/*
	 *	Restore buffer size AFTER !!
	 */

	SetConsoleScreenBufferSize( NTcout, NToldcoord );

	SetFileApisToANSI();
	GetConsoleTitle( (LPTSTR)NToldtitle, NT_MAXTITLE - 1 );
	SetConsoleTitle( ECSTR("EmACT Shell") );
	NTcurcolor = NToldattrib;
	NTeeop();
	NTseticon( NTholdicon );

	TTYinit = NIL;
}

#define	NTctrlp( x )	((x) & (RIGHT_CTRL_PRESSED|LEFT_CTRL_PRESSED))
#define	NTmetap( x )	((x) & LEFT_ALT_PRESSED)
/*
#define	NTshiftp( x )	((x) & SHIFT_PRESSED)
*/

static	int
NTgetc( void )
{
	INPUT_RECORD		   iBuf;
	CONSOLE_SCREEN_BUFFER_INFO csInfo;
	RECT			   rcNewSize;
	DWORD			   dwInputEvents;
	DWORD			   dwCKState;
	DWORD			   c;
	int			   nRet;

	if( NTforceraw ) {
		NTforceraw = 0;
		NTrawmode();
	}

	for( ;; ) {
		ReadConsoleInput( NTcin, &iBuf, 1, &dwInputEvents );

		GetWindowRect( NThwnd, &rcNewSize );
		if( (rcNewSize.right  - rcNewSize.left) != 
		    (NToldwrect.right - NToldwrect.left) ) {
		    	NToldwrect.right  = rcNewSize.right;
		    	NToldwrect.left	  = rcNewSize.left;
		    	NToldwrect.top	  = rcNewSize.top;
		    	NToldwrect.bottom = rcNewSize.bottom;
		    	NTswitchscreen();
		    }

		switch( iBuf.EventType ) {
		case KEY_EVENT:
			if( !iBuf.Event.KeyEvent.bKeyDown )
				break;

			dwCKState = iBuf.Event.KeyEvent.dwControlKeyState;

			switch( (c = iBuf.Event.KeyEvent.wVirtualKeyCode) ) {
			case VK_CONTROL	: continue;
			case VK_SHIFT	: continue;
			case VK_SPACE	:
				if( NTctrlp( dwCKState ) )
				    	nRet = Ctrl|'@';
				else	nRet = ' ';
				break;
			case VK_END	:
				if( NTctrlp( dwCKState ) )
				    	nRet = CTLX|Ctrl|'N';
				else	nRet = META|'>';
				break;
			case VK_HOME	:
				if( NTctrlp( dwCKState ) )
				    	nRet = CTLX|Ctrl|'P';
				else	nRet = META|'<';
				break;
			case VK_LEFT	:
				if( NTctrlp( dwCKState ) )
					nRet = META|'B';
				else	nRet = Ctrl|'B';
				break;
			case VK_RIGHT	:
				if( NTctrlp( dwCKState ) )
					nRet = META|'F';
				else	nRet = Ctrl|'F';
				break;
			case VK_ESCAPE	: nRet = METACH;	break;
			case VK_INSERT	: nRet = META|'I';	break;
			case VK_PRIOR	: nRet = META|'V';	break;
			case VK_NEXT	: nRet = Ctrl|'V';	break;
			case VK_UP	: nRet = Ctrl|'P';	break;
			case VK_DOWN	: nRet = Ctrl|'N';	break;
			case VK_DELETE	: nRet = Ctrl|'D';	break;
			case VK_F1	: WinHelp(
						   (HWND)NULL,
						   ECSTR("emacs.hlp"),
						   HELP_INDEX,
						   0L
						 );
					  nRet = 0;
					  break;
			case VK_F2	: nRet = Ctrl|'S';	break;
			case VK_F3	: nRet = META|Ctrl|'F';	break;
			case VK_F4	:
				if( NTmetap( dwCKState ) )
					nRet = CTLX|Ctrl|'C';
				else	nRet = CTLX|Ctrl|'I';
				break;
			case VK_F5	: NTswitchscreen(); nRet = 0; break;
			case VK_F6	: nRet = CTLX|METACH;	break;
			case VK_F7	: nRet = CTLX|'`';	break;
			case VK_F8	: nRet = CTLX|Ctrl|'S';	break;
			case VK_F9	: nRet = CTLX|'E';	break;
			case VK_F10	: NTbold(); nRet = 0;	break;
			case VK_F11	:
			case VK_F12	:
 					  NThelp( c, dwCKState );
					  nRet = 0;
					  break;
			default		: nRet = 0;
			}

			if( mouse_avoidance_mode == T &&
			    (++NTstroke % mouse_avoidance_nudge) == 0 )
				/* every N strokes, move mouse to the bottom */
				NTmovemousebottom();

			if( nRet ) {
				if( nRet == ' ' )
					NTsmallcursor();
				else	NTbigcursor();
				return( nRet );
			}

#if	defined(__GNUC__) && !defined(__CYGWIN32__) && !defined(__MINGW32__)
			nRet=(unsigned char)iBuf.Event.KeyEvent.AsciiChar;
#else
			nRet=(unsigned char)iBuf.Event.KeyEvent.uChar.AsciiChar;
#endif

#if	defined( _WIN95_BUG_FIXED_WITH_FRENCH_KBD )
			if( nRet == 0 )
				switch( iBuf.Event.KeyEvent.wVirtualKeyCode ) {
				case 0x32 : return( '~' );
				case 0x37 : return( '`' );
				}
#endif
			if( nRet ) {
				if( NTmetap( dwCKState ) )
					return( META|(toupper(nRet)) );
				else	{
					NTsmallcursor();
					return( nRet );
				}
			}

			break;

		case MOUSE_EVENT:
			/*
			 * was this a click event? Is any button down or not?
			 */

			if(iBuf.Event.MouseEvent.dwEventFlags==MOUSE_MOVED) {
				NTstroke = 0; /* reset NTstroke after move */
				break;
			}

			if(iBuf.Event.MouseEvent.dwEventFlags&MOUSE_WHEELED) {
				DWORD b = iBuf.Event.MouseEvent.dwButtonState;

				if( b & 0x80000000 )
					return( Ctrl|'V' );
				else	return( META|'V' );
			}

			if( !iBuf.Event.MouseEvent.dwButtonState )
				break;

			mevent.x      = iBuf.Event.MouseEvent.dwMousePosition.X;
			mevent.y      = iBuf.Event.MouseEvent.dwMousePosition.Y;
			mevent.button = 0;

			switch( iBuf.Event.MouseEvent.dwButtonState ) {
			case FROM_LEFT_1ST_BUTTON_PRESSED :
				mevent.button = MButton1; break;
			case FROM_LEFT_2ND_BUTTON_PRESSED :
				mevent.button = MButton2; break;
			case FROM_LEFT_3RD_BUTTON_PRESSED :
				mevent.button = MButton3; break;
			case FROM_LEFT_4TH_BUTTON_PRESSED :
				mevent.button = MButton4; break;
			case RIGHTMOST_BUTTON_PRESSED :
				mevent.button = MButton2; break;
			}

			dwCKState = iBuf.Event.MouseEvent.dwControlKeyState;

			if( dwCKState & SHIFT_PRESSED )
				mevent.button |= SHIFTBUTTON;

			if( dwCKState & (RIGHT_CTRL_PRESSED|LEFT_CTRL_PRESSED) )
				mevent.button |= CTRLBUTTON;

			return( MEVT );

		case WINDOW_BUFFER_SIZE_EVENT:
			GetConsoleScreenBufferInfo( NTcout, &csInfo );

			vtfree();
			TTYncol = csInfo.dwSize.X;
			TTYnrow = csInfo.dwSize.Y - 1;

			if( TTYnrow <= 1 )
				TTYnrow = 2;

			vtinit();
			resize();
		}
	}
}

static	void
NTputc( int c )
{
	static	EMCHAR	aChar[] = { 'X', 0 };

	aChar[0] = (EMCHAR)c;
	WriteConsole( NTcout, &aChar, 1, NULL, NULL );
}

static void
NTputs( EMCHAR *str, int cnt )
{
	CHAR_INFO	ciBuffer[ NLINE ];
	SMALL_RECT	srWindow;
	COORD		dwSize;
	COORD		dwOrigin;
	int		i;

	/*
	 * The rectangle coordinate may seem strange,  but it runs on
	 * both NT and Windows95.
	 */

	srWindow.Left   = (SHORT)NTcol;
	srWindow.Top    = (SHORT)NTrow;
	srWindow.Right  = (SHORT)(NTcol + cnt - 1);
	srWindow.Bottom = (SHORT)NTrow;

	if( cnt > NLINE )
		cnt = NLINE;

	for( i = 0 ; i < cnt ; i++ ) {
#if	defined( _UNICODE )
		ciBuffer[ i ].Char.UnicodeChar = *str++;
#else
		ciBuffer[ i ].Char.AsciiChar   = *str++;
#endif
		ciBuffer[ i ].Attributes       = NTcurcolor;
	}

	dwSize.X	= (SHORT)cnt;
	dwSize.Y	= (SHORT)1;
	dwOrigin.X	= (SHORT)0;
	dwOrigin.Y	= (SHORT)0;

	WriteConsoleOutput(
			    NTcout,
			    (PCHAR_INFO)&ciBuffer,
			    dwSize,
			    dwOrigin,
			    &srWindow
			  );

	NTcol += (SHORT)cnt;
}

static	void
NTflush( void )
{
	COORD	dwCursorPosition;

	dwCursorPosition.X = (SHORT)NTcol;
	dwCursorPosition.Y = (SHORT)NTrow;

	SetConsoleCursorPosition( NTcout, dwCursorPosition );
}

static	void
NTmove( int row, int col )
{
	NTrow = row;
	NTcol = col;
}

static	void
NTeeol( void )
{
	COORD			   cr;
	DWORD			   dwWritten;
	CONSOLE_SCREEN_BUFFER_INFO screen_info;

	GetConsoleScreenBufferInfo( NTcout, &screen_info );

	cr.X = (SHORT)NTcol;
	cr.Y = (SHORT)NTrow;

	/*
	 *	this  clears  the  characters first,  and then clears
	 *	the attribute.
	 */

	FillConsoleOutputCharacter(
				    NTcout,
				    (TCHAR)' ', 
				    (DWORD)(screen_info.dwSize.X - NTcol),
				    cr, 
				    &dwWritten
				  );
	FillConsoleOutputAttribute(
				    NTcout,
				    NTcurcolor,
				    (DWORD)(screen_info.dwSize.X - NTcol),
				    cr,
				    &dwWritten
				  );
}

static	void
NTeeop( void )
{
	COORD			   cr;
	DWORD			   dwWritten;
	CONSOLE_SCREEN_BUFFER_INFO screen_info;

	GetConsoleScreenBufferInfo( NTcout, &screen_info );

	cr.X = (SHORT)0;
	cr.Y = (SHORT)0;

	/*
	 *	this  clears  the  characters first,  and then clears
	 *	the attribute.
	 */

	FillConsoleOutputCharacter(
				    NTcout,
				    (TCHAR)' ', 
				    (DWORD)(screen_info.dwSize.Y * screen_info.dwSize.X),
				    cr, 
				    &dwWritten
				  );
	FillConsoleOutputAttribute(
				    NTcout,
				    NTcurcolor,
				    (DWORD)(screen_info.dwSize.Y * screen_info.dwSize.X),
				    cr,
				    &dwWritten
				  );
}

static	void
NTbeep( void )
{
	/*
	 *	Beep( Frequecy, Duration ).
	 *	Parameters are ignored on Windows 95.
	 */

	Beep( 0x333, 0x10 );
}

static	void
NTsi( void )
{
	WORD	swap = NTcurcolor;

	NTcurcolor = NTinvcolor;
	NTinvcolor = swap;
}

static	void
NTcshow( int flag )
{
	curflag = flag;
	NTflush();
}

static	int
NTcheck( void )
{
/*
	INPUT_RECORD	iBuf;
	DWORD		dwInputEvents;

	PeekConsoleInput( NTcin, &iBuf, 1, &dwInputEvents );

	return( dwInputEvents > 0 );
*/ 
	return( 0 );
}

static	void
NTrawmode( void )
{
	DWORD	dwMode;

	SetConsoleMode( NTcin, NToldmode );
	GetConsoleMode( NTcin, &dwMode );
	dwMode &= (DWORD)~ENABLE_PROCESSED_INPUT;
	dwMode &= (DWORD)~ENABLE_WINDOW_INPUT;
	dwMode &= (DWORD)~ENABLE_LINE_INPUT;
	dwMode &= (DWORD)~ENABLE_ECHO_INPUT;
	dwMode |= ENABLE_MOUSE_INPUT;
	SetConsoleMode( NTcin, dwMode );
}

static	EMCHAR *
NTtitle( EMCHAR *buf, EMCHAR *fname )
{
	static	EMCHAR	title[ 64 ] = { 0 };

	/*
	 * The next two lines remove warning on args not used !!
	 */

	if( buf != fname )
		fname = buf;

	if( emstrcmp( buf, title ) != 0 ) {
		(void)emsprintf3(
				  title,
				  ECSTR("%s (%dx%d)"),
				  fname,
				  TTYnrow,
				  TTYncol
			        );
#if	defined( _NOTDEF )
		{
		char	cvtitle[ 64 ];

		(void)OemToChar( title, cvtitle );
		SetConsoleTitle( (LPTSTR)cvtitle );
		}
#else
		SetConsoleTitle( (LPTSTR)title );
#endif
	}

	return( buf );
}

static	void
NTmovemousebottom( void )
{
	RECT	rect;

	if( NThwnd == (HWND)NULL )
		return;

	GetWindowRect( NThwnd, &rect );

	SetCursorPos( rect.right, rect.bottom );
}

CMD
NTswitchscreen( void )
{
	CONSOLE_SCREEN_BUFFER_INFO csInfo;

	vtfree();

	GetConsoleScreenBufferInfo( NTcout, &csInfo );
	TTYncol = csInfo.srWindow.Right  - csInfo.srWindow.Left + 1;
	TTYnrow = csInfo.srWindow.Bottom - csInfo.srWindow.Top;

	vtinit();
	NTeeop();

	resize();
	(void)redrawscreen();
	(void)update( T );

	return( T );
}

CMD
NTansitooem( EDLINE *lp )
{
#if	!defined( _UNICODE )
	CharToOemBuff( ltext( lp ), ltext( lp ), (DWORD)llength( lp ) );
	return( T );
#else
	return( lp ? T : NIL );
#endif
}

CMD
NToemtoansi( EDLINE *lp )
{
#if	!defined( _UNICODE )
	OemToCharBuff( ltext( lp ), ltext( lp ), (DWORD)llength( lp ) );
	return( T );
#else
	return( lp ? T : NIL );
#endif
}

/*
 *	Paste from clipboard data. We are not the owner of clipboard.
 */

static	void
NTclippaste( void )
{
	UINT	fmt[]     = { CF_OEMTEXT, CF_TEXT, CF_UNICODETEXT, CF_DSPTEXT };
	HANDLE	hClipData = (HANDLE)0;	/* handles to clip data	*/
	LPSTR	lpClipData;
	LPSTR	s;
	int	type  = -1;
	int	i;

	if( OpenClipboard( NThwnd ) == FALSE ) {
		WDGerror( ECSTR("Can't open clipboard") );
		return;
	}

	/*
	 *	get text from the clipboard
	 */

	for( i = 0 ; i < (int)(sizeof( fmt ) / sizeof( fmt[0] )) ; ++i )
		if( (hClipData = GetClipboardData( fmt[i] )) != 0 ) {
			type = (int)fmt[ i ];
			break;
		}

	if( hClipData == (HANDLE)0 ) {
		WDGerror( ECSTR("Can't find clipboard data") );
		CloseClipboard();
		return;
	}

	if( (lpClipData = (LPSTR)GlobalLock( hClipData ) ) == (LPSTR)0 ) {
		WDGerror( ECSTR("No memory left, file may be corrupted.") );
		CloseClipboard();
		return;
	}

	if( type == -1 ) {
		WDGerror( ECSTR("Can't find a valid clipboard data.") );
		CloseClipboard();
		return;
	}

#if	defined( _CONVERT_WHEN_OEM )
	if( type == CF_OEMTEXT )
		OemToAnsi( lpClipData, lpClipData );
#endif	/* _CONVERT_WHEN_OEM */

	kused = 0;

	for( s = lpClipData ; *s ; s++ )
		if( *s != '\r' )
			(void)kinsert( (int)*s );

	GlobalUnlock( hClipData );
	CloseClipboard();
}

/*
 *	Copy to clipboard data. We are the owner of the clipboard.
 */

static	void
NTclipcopy( void )
{
	UINT	AllocFlags = GMEM_MOVEABLE|GMEM_DDESHARE;
	HANDLE	hClipData;
	EMCHAR *lpClipData;

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
		WDGerror( ECSTR("No memory left, file may be corrupted.") );
		return;
	}

	/*
	 *	Copy data
	 */

	lpClipData = (EMCHAR *)GlobalLock( hClipData );

	if( lpClipData == NULL ) {
		WDGerror( ECSTR("No memory left, file may be corrupted.") );
		return;
	}

	(void)emstrncpy( lpClipData, kbufp, kused );
	lpClipData[ kused ] = 0;

	GlobalUnlock( hClipData );

	/*
	 *	Clear the current contents of the clipboard, and set
	 *	the data handle to the new string.
	 */

	if( OpenClipboard( NThwnd ) == 0 )
		WDGerror( ECSTR("Can't open clipboard.") );
	else	{
		EmptyClipboard();
		if( SetClipboardData( CF_OEMTEXT, hClipData ) == NULL )
			WDGerror( ECSTR("Can't set clipboard data.") );
		CloseClipboard();
	}
}

static	void
NThelp( DWORD dwKey, DWORD dwCKState )
{
	EMCHAR	buf[ NPAT ];
	EMCHAR	*p;
	EMCHAR	*s;

	switch( dwKey ) {
	case VK_F11 :
		if( NTmetap( dwCKState ) )
			p = &helpfile3[0];
		else	p = &helpfile1[0];
		break;
	case VK_F12 :
		if( NTmetap( dwCKState ) )
			p = &helpfile4[0];
		else	p = &helpfile2[0];
		break;
	default	    :
		return;
	}

	if( !*p || ffaccess( p ) != FIOSUC ) {
		NTbeep();
		WDGwrite( ECSTR("Can't find associated help file.") );
		return;
	}

	if( ((s = emstrrchr( p, '.' )) != NULL) &&
	    (emstrnicmp( s, ECSTR(".html"), 5 ) == 0  ||
	    (emstrnicmp( s, ECSTR(".htm"),  4 ) == 0) ||
	    (emstrnicmp( s, ECSTR(".chm"),  4 ) == 0) ) ) {
		NTsystem( p );

		/*
		 * ShellExecute and ShellExecuteEx hangs on return
		 */

#if	defined( USE_SHELLEXECUTE )
		ShellExecute(HWND_DESKTOP,"Open",p,NULL,NULL,SW_SHOWNORMAL);
#endif

#if	defined( USE_SHELLEXECUTEEX )
		SHELLEXECUTEINFO shExec;

		memset( &shExec, 0, sizeof( SHELLEXECUTEINFO ) );
		shExec.cbSize		= sizeof( SHELLEXECUTEINFO );
		shExec.fMask		= 0;
		shExec.hwnd		= NThwnd; 
		shExec.lpVerb		= "Open"; 
		shExec.lpFile		= p;
		shExec.lpParameters	= NULL;
		shExec.lpDirectory	= NULL; 
		shExec.nShow		= SW_SHOWNORMAL; 
		/* shExec.hInstApp is set on return. */

		if( ShellExecuteEx( &shExec ) == 0 )
			WDGwrite( "Exec error" );
#endif
		return;
	}

	if( wordatcursor( buf, NPAT ) != T )
		WinHelp( NThwnd, p, HELP_INDEX, 0L );
#if	defined( _WIN64 )
	else	WinHelp( NThwnd, p, HELP_PARTIALKEY, (ULONG_PTR)buf );
#else
	else	WinHelp( NThwnd, p, HELP_PARTIALKEY, (DWORD)buf );
#endif

}

static int
NTsystem( const EMCHAR *cmd )
{
	SECURITY_ATTRIBUTES saAttr;
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO	    siStartInfo;
	BOOL		    bSuccess;
	EMCHAR		    buf[ NFILEN ];

	memset( &saAttr, 0, sizeof( SECURITY_ATTRIBUTES ) );
	saAttr.nLength		    = sizeof( SECURITY_ATTRIBUTES );
	saAttr.bInheritHandle	    = FALSE;
	saAttr.lpSecurityDescriptor = NULL;

	/* Set up members of STARTUPINFO structure. */

	memset( &siStartInfo, 0, sizeof( STARTUPINFO ) );
	siStartInfo.cb		= sizeof( STARTUPINFO );
	siStartInfo.lpReserved	= NULL;
	siStartInfo.lpReserved2	= NULL;
	siStartInfo.cbReserved2	= 0;
	siStartInfo.lpDesktop	= NULL;
	siStartInfo.lpTitle	= NULL;
	siStartInfo.wShowWindow	= SW_SHOWNORMAL;
	siStartInfo.dwFlags	= STARTF_USESHOWWINDOW;
	siStartInfo.hStdOutput	= NULL;
	siStartInfo.hStdError	= NULL;
	siStartInfo.hStdInput	= NULL;

	emstrcpy( &buf[0], ffgetenv( ECSTR("COMSPEC") ) );
	emstrcat( &buf[0], ECSTR(" /c ") );
	emstrcat( &buf[0], cmd );

	/*
	 * Create the child process.
	 */

	bSuccess = CreateProcess(
			NULL,
			(EMCHAR *)buf, /* command line                       */	
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
		WDGwrite(
			  ECSTR("Exec fails with error code = %x"),
		          GetLastError()
			);
	} else	{

		/*
		 * Change the priority from IDLE to NORMAL
		 */

		SetPriorityClass( piProcInfo.hProcess, NORMAL_PRIORITY_CLASS );

		/*
		 * WaitForSingleObject( piProcInfo.hProcess, INFINITE );
		 */
	}

	NTforceraw = 1;
	Sleep( 2000 );
	NTrawmode();

	return( bSuccess );
}

#if	defined( _PRINT_HOOK )
UINT APIENTRY
NTsetuphookproc( HWND hdlg, UINT uiMsg, WPARAM wParam, LPARAM lParam )
{
	switch( uiMsg ) {
	case WM_INITDIALOG :
		SetForegroundWindow( hdlg );
		SetFocus( hdlg );
	}

	return( DefWindowProc(hdlg, uiMsg, wParam, lParam) );

}
#endif

static	HDC
NTgetprinterdc( void )
{
	PRINTDLG      pd;
	HDC           hDC;
	LPDEVMODE     lpDevMode = NULL;
	LPDEVNAMES    lpDevNames;
	EMCHAR *      lpszDriverName;
	EMCHAR *      lpszDeviceName;
	EMCHAR *      lpszPortName;

	memset( &pd, 0, sizeof( PRINTDLG ) );
	pd.lStructSize    = sizeof( PRINTDLG );
	pd.hwndOwner      = GetTopWindow( GetDesktopWindow() ); // NULL;
	pd.hDevMode       = NULL;
	pd.hDevNames      = NULL;
	pd.Flags          = PD_RETURNDC | 
//			    PD_ENABLESETUPHOOK | PD_ENABLEPRINTHOOK |
			    PD_NOSELECTION | PD_NOPAGENUMS;
	pd.nCopies        = 1;
//	pd.lpfnSetupHook  = NTsetuphookproc;
//	pd.lpfnPrintHook  = NTsetuphookproc;

	if( PrintDlg( (LPPRINTDLG)&pd ) == FALSE )
		return( (HDC)-1 );

	if( pd.hDC )
		hDC = pd.hDC;
	else	{
		if( !pd.hDevNames )
			return( (HDC)NULL );

		lpDevNames     = (LPDEVNAMES)GlobalLock( pd.hDevNames );
		lpszDriverName = (EMCHAR *)lpDevNames + lpDevNames->wDriverOffset;
		lpszDeviceName = (EMCHAR *)lpDevNames + lpDevNames->wDeviceOffset;
		lpszPortName   = (EMCHAR *)lpDevNames + lpDevNames->wOutputOffset;
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

static	void
NTprint( void )
{
	int	   Status;	/* printing status			*/
	int	   nPageLength;	/* vert. resolution of printer device	*/
	int	   nPageWidth;	/* horz. resolution of printer device	*/
	HCURSOR	   hSaveCursor;	/* current cursor handle		*/
	HDC	   hPr;		/* handle for printer device context	*/
	TEXTMETRIC TextMetric;	/* information about char size		*/
	HFONT	   hfPr;	/* font used for printing		*/
	HFONT	   hOldFont;	/* old font used			*/
	LOGFONT	   lfPr;	/* LOGFONT structure for the font	*/
	int	   LineSpace;	/* spacing between lines		*/
	int	   LinesPerPage;/* lines per page			*/
	int	   CurrentLine;	/* current line	in page			*/
	int	   LineNumber;	/* current line number			*/
	int	   CurrentPage;	/* current page				*/
	int	   len;		/* length of the current line		*/
	EDLINE	   *clp;	/* current line to print		*/
	time_t	   aclock;	/* current time				*/
	EMCHAR	   *szTimeBuf;	/* ascii version of current time	*/
	EMCHAR	   *p;
	EMCHAR	   buf[ NLINE + NTLINENBSIZE ];
	static	   HCURSOR	hHourGlass = (HCURSOR)0;
	union	{
		char   *s;
		FARPROC	p;
	}	nowarn;

	if( hHourGlass == (HCURSOR)0 )
		hHourGlass = LoadCursor( NULL, IDC_WAIT );

	hSaveCursor = SetCursor( hHourGlass );
	hPr	    = NTgetprinterdc();

	if( hPr == (HDC)-1 )
		return;

	if( hPr == (HDC)NULL ) {
	    MessageBox(NThwnd,
		       ECSTR("Cannot print."),  NULL, MB_OK|MB_ICONHAND );
	    return;
	}

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

	emstrcpy( lfPr.lfFaceName, ECSTR("Courier New") );

	hfPr = CreateFontIndirect( &lfPr );

	hOldFont = (HFONT)SelectObject( hPr, hfPr );

	/*
	 *	The union is used for the invalid type converion
	 */

	nowarn.p = (FARPROC)NULL;

	Escape( hPr, SETABORTPROC, FALSE, nowarn.s, 0L );

	if( Escape( hPr, STARTDOC, 12, "Emacs Print", (EMCHAR *)NULL ) < 0 ) {
		MessageBox(
			    NThwnd,
			    ECSTR("Unable to start print job"),
			    NULL,
			    MB_OK | MB_ICONHAND
			  );
		(void)SelectObject( hPr, hOldFont );
		DeleteObject( hfPr );
		DeleteDC( hPr );
	}

	SetCursor( hSaveCursor );      /* Remove the hourglass */

	/*
	 * Get time in szTimeBuf and remove '\n'
	 */

	time( &aclock );
	szTimeBuf = (EMCHAR *)asctime( localtime( &aclock ) );
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
					  ECSTR("Page #%03d, %s - %s"),
					  CurrentPage,
					  szTimeBuf,
					  curbp->b_fname
				        );
			TabbedTextOut(
					hPr,
					0,
					CurrentLine * LineSpace,
					(EMCHAR *)buf,
					emstrlen( buf ),
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

		(void)emstrncpy( buf + NTLINENBSIZE, ltext(clp), (size_t)len );

		buf[ len + NTLINENBSIZE ] = '\000';

		TabbedTextOut(
				hPr,
				0,
				(CurrentLine + 2) * LineSpace,
				(EMCHAR *)buf,
				len + NTLINENBSIZE,
				0,
				NULL,
				0
			     );

		if( (++CurrentLine % LinesPerPage) == 0 ) {
			CurrentLine = 1;
			CurrentPage++;
			Status = Escape( hPr, NEWFRAME, 0, 0L, 0L );
			if( Status < 0 )
				break;
			else	SelectObject( hPr, hfPr );
		}
	}

	if( Status >= 0 ) {
		Escape( hPr, NEWFRAME, 0, 0L, 0L );
		Escape( hPr, ENDDOC,   0, 0L, 0L );
	}

	(void)SelectObject( hPr, hOldFont );
	DeleteObject( hfPr );
	DeleteDC( hPr );

}

static void
NTbold( void )
{
	if( bold_font == NIL ) {
		bold_font = T;
		NTcurcolor |= (WORD)FOREGROUND_INTENSITY;
	} else	{
		bold_font = NIL;
		NTcurcolor &= (WORD)~FOREGROUND_INTENSITY;
	}

	(void)redrawscreen();
	(void)update( T );
}

#if	defined( _ADD_TO_RECENT_DOCS )
extern	CMD
NTaddrecent( EMCHAR *file )
{
	SHAddToRecentDocs( SHARD_PATH, file );
	return( T );
}
#endif

static	void
NTsmallcursor( void )
{
	CONSOLE_CURSOR_INFO 	   ccInfo;

	ccInfo.dwSize   = NToldcursize;
	ccInfo.bVisible = TRUE;
	SetConsoleCursorInfo( NTcout, &ccInfo );
}

static	void
NTbigcursor( void )
{
	CONSOLE_CURSOR_INFO 	   ccInfo;

	ccInfo.dwSize   = 100;	/* 100% of cell */
	ccInfo.bVisible = TRUE;
	SetConsoleCursorInfo( NTcout, &ccInfo );
}

static	HICON
NTloadicon( void )
{
	HMODULE	hExe;		/* handle to loaded .EXE file	*/
	HRSRC	hResource;	/* handle for FindResource	*/
	HRSRC	hMem;		/* handle for LoadResource	*/
	BYTE    *lpResource;	/* pointer to resource data	*/
	EMCHAR	name[64];	/* name				*/

	NTholdicon = (HICON)SendMessage(
					 NThwnd,
					 WM_GETICON,
					 (WPARAM)ICON_SMALL,
					 (LPARAM)NULL
				       );

	/*
	 * Load the file from which to copy the icon from.
	 */

	hExe = LoadLibrary( eargv[0] );

	if( hExe == (HMODULE)NULL ) {
		(void)emsprintf1( name, ECSTR("%s.exe"), eargv[0] );
		hExe = LoadLibrary( name );
	}

	if( hExe == (HMODULE)NULL )
		return( (HICON)NULL );

	/*
	 * Find the bits for icon#1.
	 */

	hResource  = FindResource(
				   hExe,
				   MAKEINTRESOURCE(1),		/* "#1" */
				   MAKEINTRESOURCE((WORD)((size_t)RT_ICON))
				   );
	
	if( hResource == 0 )
		return( (HICON)NULL );

	hMem	   = LoadResource( hExe, hResource );

	if( hMem == 0 )
		return( (HICON)NULL );

	lpResource = LockResource( hMem );
	NThicon	   = CreateIconFromResourceEx(
					       (PBYTE)lpResource,
					       SizeofResource(hExe,hResource),
					       TRUE,
					       0x00030000, /* 3.x */
					       0,
					       0,
					       LR_DEFAULTCOLOR
					     );

	return( NThicon );
}

static	void
NTseticon( HICON hIcon )
{
	SendMessage( NThwnd, WM_SETICON, (WPARAM)ICON_SMALL, (LPARAM)hIcon );
}

#if	defined( _WINCONSOLE )
#if	defined( _WIN64 )
typedef	intptr_t	llong;
#else
typedef	long		llong;
#endif	/* _WIN64 */

static BOOL WINAPI ConsoleEventHandler( DWORD dwCtrlType );

static BOOL WINAPI
ConsoleEventHandler( DWORD dwCtrlType )
{
	switch( dwCtrlType ) {
	case CTRL_CLOSE_EVENT:
		return( TRUE );
	default:
		return( FALSE );
	}
}

BOOL
NTcreateconsole( void )
{
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int			   hCrt;
	FILE *			   hf;
	HANDLE			   hConsoleOutput;
	SMALL_RECT		   sr;

	if( AllocConsole() == 0 )
		return( FALSE );

	/*
	 * Catch the close of this console
	 */

	SetConsoleCtrlHandler( ConsoleEventHandler, TRUE );

	/*
	 * Taken form MSDN. Redirect output to this console.
	 */

	hCrt = _open_osfhandle((llong)GetStdHandle(STD_OUTPUT_HANDLE),_O_TEXT);
	hf   = _fdopen( hCrt, "w" );
	*stdout = *hf;
	(void)setvbuf( stdout, NULL, _IONBF, 0 );

	hCrt = _open_osfhandle((llong)GetStdHandle(STD_ERROR_HANDLE),_O_TEXT);
	hf   = _fdopen( hCrt, "w" );
	*stderr = *hf;
	(void)setvbuf( stderr, NULL, _IONBF, 0 );

	hCrt = _open_osfhandle((llong)GetStdHandle(STD_INPUT_HANDLE),_O_TEXT);
	hf   = _fdopen( hCrt, "r" );
	*stdin = *hf;

	hConsoleOutput = GetStdHandle( STD_OUTPUT_HANDLE );

	GetConsoleScreenBufferInfo( hConsoleOutput, &csbi );
	csbi.dwSize.Y = (SHORT)(csbi.dwMaximumWindowSize.Y - 1);
	SetConsoleScreenBufferSize( hConsoleOutput, csbi.dwSize );

	sr.Left   = 0;
	sr.Top    = 0;
	sr.Right  = 79;
	sr.Bottom = (SHORT)(csbi.dwMaximumWindowSize.Y - 2);

	SetConsoleWindowInfo( hConsoleOutput, TRUE, &sr );

	if( NThwnd == (HWND)NULL )
		NThwnd = NTconsolewin();

	SetWindowPos(
		      NThwnd,
		      HWND_TOP,
		      GetSystemMetrics( SM_CXICONSPACING ),
		      0,
		      0,
		      0,
		      SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE
		    );

	return( TRUE );
}

#include	<tchar.h>

extern	int	_tmain( int argc, TCHAR* argv[] );
extern	BOOL	NTcreateconsole( void );

#define	MAX_CONSOLE_ARGUMENTS	128

int	PASCAL
_tWinMain( HINSTANCE hInstance, HINSTANCE hPInst, LPSTR cmdLine, int nCmdShow )
{
	TCHAR	*argv[ MAX_CONSOLE_ARGUMENTS ];
	TCHAR	*cmdline;
	TCHAR	*p;
	int	argc;
	int	res;

	(void)hInstance;
	(void)hPInst;
	(void)cmdLine;
	(void)nCmdShow;

	cmdline = _tcsdup( GetCommandLine() );
	p	= cmdline;
	argc	= 0;

	while( *p ) {
		while( *p && _istspace( *p ) )
			p++;
		if( *p )
			argv[ argc++ ] = p;
		else	break;
		while( *p && !_istspace( *p ) )
			p++;
		if( _istspace( *p ) )
			*p++ = '\000';
	}

	argv[ argc ] = NULL;

	NTcreateconsole();

	res = _tmain( argc, argv );

	free( cmdline );
	FreeConsole();

	return( res );

}

#endif

#endif
