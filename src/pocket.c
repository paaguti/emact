#if	!defined( lint )
static	char rcsid[] = "$Id: pocket.c,v 1.4 2008/06/18 15:45:33 jullien Exp $";
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
 *	pocket.c : The routines in this file provide support for WINDOWS.
 */

#define	NOCOMM		/* No COMM driver routines (for Warnings) */

#if	!defined( STRICT )
#define	STRICT		/* Strict type checking for Windows & NT  */
#endif

#include	<windows.h>
#include	<commdlg.h>
#include	<shellapi.h>
#include	<commctrl.h>
#include	<aygshell.h>
#include	"emacs.h"
#include	"newres.h"
#include	"pocket.h"
#include	"resource.h"

/*
 *	Private variables
 */

static	HWND      hCurWnd;		/* Current Window		*/
static	HINSTANCE hInst;		/* hInstance			*/
static	HDC	  hTTyDC;		/* TTy DC			*/
static	DWORD	  BackColor;		/* Current background color	*/
static	DWORD	  TextColor;		/* Current foreground color	*/
static	HBRUSH	  hBrush;		/* Current brush		*/
static	HFONT	  hFont;		/* Current font			*/
static	int	  nCol;			/* Maximum column		*/
static	int	  nRow;			/* Maximum row			*/
static	int	  nParentWidth;		/* Current window width		*/
static	int	  nParentHeight;	/* Current window height	*/
static	int	  nCharWidth;		/* Width of a char		*/
static	int	  nCharHeight;		/* Height of a char		*/

/*
 *	Standard terminal interface dispatch table.
 */

static	void	winopen( void );
static	void	winclose( void );
static	int	wingetc( void );
static	void	winputc( int c );
static	void	winputs( TCHAR *szText, int nSize );
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

/*
 *	Private variables :
 */

#define	MAXROW		128
#define	MAXCOL		256
#define	MINCOL		 80
#define	MINROW		 60

#define	MAXPATHNAME	256

#define	COLOR_BLACK	RGB(   0,   0,   0 )
#define	COLOR_BLUE	RGB(   0,   0, 128 )
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

static	HANDLE	hPrev;			/* Previous instance		*/
static	HANDLE	hAccTable;		/* Accelerator table		*/
static	LPSTR	lpClipBuf  = NULL;	/* Pointers to clip buffer	*/
static	LPSTR	lpClipPtr  = NULL;	/* Current ClipPtr		*/
static	int	nCmd	   = 0;		/* Command line			*/
static	int	nDrawX	   = 0;		/* X cursor			*/
static	int	nDrawY	   = 0;		/* Y cursor			*/
static  int	nHSPos	   = 0;		/* Horizontal Scroll Position	*/
static	int	nVSPos	   = 0;		/* Vertical   Scroll Position	*/
static	int	nOpen      = 0;		/* Open flag			*/

static	OPENFILENAME	ofn;
static	CHOOSECOLOR	ColorChunk;
static	LOGFONT		lf;

static	void	winqueryfont( void );
static	void	winsetfontsize( int size );
static	void	wingetcolor( int *color );
static	void	winsettextattrib( void );
static	int	winstringwidth( TCHAR *strg, int len );
static	void	winupdatescrollbar( void );
static	void	winupdaterect( RECT *rcClear, int Pos );
static	void	winprinterror( TCHAR *txt , BOOL fexit );
static	int	winquit( void );

/*
 *	Application Name
 */

static	TCHAR*	szAppName = _T("EmACT");

/*
 *	new variables for common dialogs
 */

static  TCHAR* szFilterSpec =
		_T("C Files (*.c)\0*.c;*.h\0")
		_T("C++ Files (*.cpp, *.cc)\0*.cpp;*.cc;*.h;*.hpp\0")
		_T("Lisp Files (*.l*)\0*.l*\0")
		_T("Text Files (*.txt)\0*.txt\0")
		_T("All Files (*.*)\0*.*\0");

static	TCHAR	szFileName[ MAXPATHNAME ];
static	TCHAR	szFileTitle[ MAXPATHNAME ];

/*
 *	External functions to be implemented by the Windows application.
 */

int
winsystem( TCHAR *s )
{
	return( _wcesystem( s ) );
}

static void
winyield( void )
{
	MSG	msg;

	while( PeekMessage( &msg, (HWND)NULL, 0, 0, PM_REMOVE ) ) {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	}
}

static void
winsettextattrib( void )
{
	TEXTMETRIC	txMetric;
	RECT		rcClient;

	GetClientRect( hCurWnd, (LPRECT)&rcClient );
	GetTextMetrics( hTTyDC, &txMetric );

	nParentWidth  = rcClient.right  - rcClient.left;
	nParentHeight = rcClient.bottom - rcClient.top;
	nCharWidth    = txMetric.tmAveCharWidth;
	nCharHeight   = txMetric.tmHeight + txMetric.tmExternalLeading;
	nCol	      = (short)(nParentWidth /nCharWidth);
	nRow	      = (short)(nParentHeight/nCharHeight);

	if( nCol > MAXCOL ) nCol = MAXCOL;
	if( nRow > MAXROW ) nRow = MAXROW;
	if( nCol < MINCOL ) nCol = MINCOL;

	TTYncol		= (int)nCol;
	TTYnrow		= (int)nRow - 1;
	TTYinit		= T;
	line_number_mode= T;

}

#define	_WS_EMACSWINDOW		WS_VISIBLE | WS_HSCROLL // WS_EX_CAPTIONOKBTN 
#define	CS_EMACS		CS_HREDRAW | CS_VREDRAW

#if	defined( _VERTICAL_SCROLLBAR )
#define	WS_EMACSWINDOW	_WS_EMACSWINDOW | WS_VSCROLL
#else
#define	WS_EMACSWINDOW	_WS_EMACSWINDOW
#endif

static void
winopen( void )
{
	SHMENUBARINFO mbi;
	RECT	      recw;
	RECT	      recb;
	HICON	      hIcon;

#if	defined( _EMACS_WITH_SYSTEM_COLORS )
	BackColor = colortable[ background_color ];
	TextColor = colortable[ foreground_color ];
#else
	BackColor = GetSysColor( COLOR_WINDOW     );
	TextColor = GetSysColor( COLOR_WINDOWTEXT );
#endif
	hBrush	  = CreateSolidBrush( BackColor );
	hIcon	  = LoadIcon( hInst, MAKEINTRESOURCE( EMACTICON ) );

	if( hPrev == NULL ) {
		WNDCLASS wc;

		wc.style	 = CS_EMACS;
		wc.lpfnWndProc	 = (WNDPROC)MainWndProc;
		wc.cbClsExtra	 = 0;
		wc.cbWndExtra	 = 0;
		wc.hInstance	 = hInst;
		wc.hIcon	 = hIcon;
		wc.hCursor	 = LoadCursor( (HANDLE)0, IDC_ARROW );
		wc.lpszMenuName  = NULL;
		wc.hbrBackground = hBrush;
		wc.lpszClassName = _T("EmACTWClass");

		if( RegisterClass( &wc ) == 0 ) {
			winprinterror( _T("Can't register class"), TRUE );
			return;
		}

		wc.lpszMenuName	 = NULL;
		wc.lpszClassName = _T("StdWClass");

		if( RegisterClass( &wc ) == 0 ) {
			winprinterror( _T("Can't register class"), TRUE );
		}
	} else	winprinterror( _T("winopen1"), FALSE );


	hCurWnd = CreateWindow(
				 _T("EmACTWClass"),
				 szAppName,
				 WS_EMACSWINDOW,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 (HWND)0,
				 (HMENU)0,
				 hInst,
				 NULL
			       );

	if( hCurWnd == NULL )
		winprinterror( _T("Can't create Window"), TRUE );

	hTTyDC	  = GetDC( hCurWnd );

	/*
	 *	Create a fixed font.
	 */

	winsetfontsize( 13 );

	(void)SelectObject( hTTyDC, hBrush );
	(void)SelectObject( hTTyDC, hFont  );

	winsettextattrib();

	SetClassLong( hCurWnd, GCL_HICON, (LONG)hIcon );

	nDrawX	      = 0;
	nDrawY	      = 0;

	SetBkColor(   hTTyDC, BackColor );
	SetTextColor( hTTyDC, TextColor );

	wineeop();

	/*
	 *	fill in non-variant fields of OPENFILENAME struct.
	 */

	ofn.lStructSize		= sizeof( OPENFILENAME );
	ofn.hwndOwner		= NULL;
	ofn.hInstance		= NULL;
	ofn.lpstrFilter		= szFilterSpec;
	ofn.lpstrCustomFilter	= NULL;
	ofn.nMaxCustFilter	= 0;
	ofn.nFilterIndex	= 1;
	ofn.lpstrFile		= szFileName;
	ofn.nMaxFile		= MAXPATHNAME;
	ofn.lpstrFileTitle	= szFileTitle;
	ofn.nMaxFileTitle	= MAXPATHNAME;
	ofn.lpstrTitle		= NULL;
	ofn.lpstrDefExt		= _T("lsp");
	ofn.Flags		= 0;

	memset( &mbi, 0, sizeof( SHMENUBARINFO ) );
	mbi.cbSize     = sizeof( SHMENUBARINFO );
	mbi.hwndParent = hCurWnd;
	mbi.nToolBarId = IDR_MENUBAR;
	mbi.hInstRes   = hInst;
	mbi.nBmpId     = 0;
	mbi.cBmpImages = 0;  

	if( !SHCreateMenuBar( &mbi ) )
		winprinterror( _T("SHCreateMenuBar Failed"), TRUE );

	GetWindowRect( hCurWnd,    &recw );
	GetWindowRect( mbi.hwndMB, &recb );
	MoveWindow(
		    hCurWnd,
		    recw.left,
		    recw.top,
		    recw.right  - recw.left,
		    (recw.bottom - recw.top) - (recb.bottom - recb.top),
		    TRUE
		  );

	/*
	{
	  INITCOMMONCONTROLSEX icex;

	  icex.dwSize = sizeof(icex);
	  icex.dwICC  = ICC_BAR_CLASSES | ICC_COOL_CLASSES;
	  InitCommonControlsEx( &icex );
	}

	SHDoneButton( hCurWnd, SHDB_SHOW        );
	SHFullScreen( hCurWnd, SHFS_HIDETASKBAR );
	*/

	SetScrollRange( hCurWnd, SB_HORZ, 0, MINCOL, TRUE );

#if	defined( _VERTICAL_SCROLLBAR )
	SetScrollRange( hCurWnd, SB_VERT, 0, MINROW, TRUE );
#endif

	winsettextattrib();

	(void)ShowWindow( hCurWnd, nCmd );
	if( UpdateWindow( hCurWnd ) == 0 )
		winprinterror( _T("Can't update window"), TRUE );

	nOpen = 1;

}

static void
winclose( void )
{
	DeleteObject( hBrush );
	DeleteObject( hFont  );
	ReleaseDC( hCurWnd, hTTyDC );
	PostQuitMessage( 0 );
}

static	void
winupdaterect( RECT *rcClear, int Pos )
{
	rcClear->top    = nDrawY * nCharHeight;
	rcClear->bottom = (nDrawY + 1) * nCharHeight;
	rcClear->left   = rcClear->right = Pos;
}

static void
winputs( TCHAR *szText, int nSize )
{
	ExtTextOut(
		    hTTyDC,
		    nDrawX * nCharWidth,
		    nDrawY * nCharHeight,
		    ETO_CLIPPED,
		    NULL,
		    &szText[0],
		    nSize,
		    (const int *)NULL
		  );

	nDrawX += nSize;
}

static void
winupdatescrollbar( void )
{
#if	defined( _OLD )
	int visible = (nParentWidth / nCharWidth);

	if( (nDrawX - nHSPos + 5) > visible ) {
		nHSPos += 5;
		SetScrollPos( hCurWnd, SB_HORZ, nHSPos, TRUE );
		InvalidateRect( hCurWnd, (LPRECT)NULL, TRUE );
	}

	if( nHSPos > nDrawX ) {
		nHSPos = nDrawX - 5;
		if( nHSPos < 5 ) nHSPos = 0;
		SetScrollPos( hCurWnd, SB_HORZ, nHSPos, TRUE );
		InvalidateRect( hCurWnd, (LPRECT)NULL, TRUE );
	}
#endif
}

static void
winputc( int aChar )
{
	TCHAR	c[] = { (TCHAR)'?', (TCHAR)0 };

	c[0] = (TCHAR)aChar;

	winputs( (TCHAR *)&c, 1 );
}

static void
winmove( int nRow, int nCol )
{
	nDrawX = nCol;
	nDrawY = nRow;

	winupdatescrollbar();
}

static int
wingetc( void )
{
	static	int nextchar = 0;
	int	ret;
	MSG	msg;

	if( nextchar ) {
		ret 	 = nextchar;
		nextchar = 0;
		return( ret );
	}

	if( lpClipPtr ) {
		int	aChar = (int)*lpClipPtr++;

		if( *lpClipPtr == '\000' ) {
			free( lpClipBuf );
			lpClipBuf = NULL;
			lpClipPtr = NULL;
		}

		return( aChar );
	}

	wincshow( TRUE );

	while( GetMessage( &msg, (HWND)0, 0, 0 ) ) {
		if( TranslateAccelerator( msg.hwnd, hAccTable, &msg ) )
			continue;

		TranslateMessage( &msg );

		switch( msg.message ) {

		case WM_CHAR :
			wincshow( FALSE );
			return( msg.wParam );

		case WM_KEYDOWN :
			wincshow( FALSE );
			switch( msg.wParam ) {
			case VK_UP	: return( 0x10 );	/* ^P */
			case VK_DOWN	: return( 0x0E );	/* ^N */
			case VK_RIGHT	: return( 0x06 );	/* ^F */
			case VK_LEFT	: return( 0x02 );	/* ^B */
			case VK_DELETE	: return( 0x04 );	/* ^D */
			case VK_HOME	: return( 0x01 );	/* ^A */
			case VK_END	: return( 0x05 );	/* ^E */
			case VK_NEXT	: return( 0x16 );	/* ^V */
			case VK_PRIOR	: nextchar = 'V';
				          return( 0x1B );	/* ESC V */
			default		: wincshow( TRUE );
			}
			continue;

		case WM_MOUSEMOVE :
			break;

		default :
			wincshow( FALSE );
			DispatchMessage( &msg );
			if( lpClipPtr )
				return( wingetc() ); /* data in clipboard */
			wincshow( TRUE );
		}

	}

	return( (int)msg.wParam );

}

static void
wineeop( void )
{
	RECT	rcClear;

	GetClientRect( hCurWnd, (LPRECT)&rcClear );
	FillRect( hTTyDC, (LPRECT)&rcClear, hBrush );
}

static void
wineeol( void )
{
	RECT	rcClear;

	rcClear.top    = nDrawY * nCharHeight;
	rcClear.bottom = (nDrawY + 1) * nCharHeight;
	rcClear.left   = nDrawX * nCharWidth;
	rcClear.right  = nCol   * nCharWidth;

	FillRect( hTTyDC, (LPRECT)&rcClear, hBrush );
}

static void
winsi( void )
{
	SetBkColor(   hTTyDC, TextColor );
	SetTextColor( hTTyDC, BackColor );
}

static void
winei( void )
{
	SetBkColor(   hTTyDC, BackColor );
	SetTextColor( hTTyDC, TextColor );
}
 
static void
winflush( void )
{
}

static void
winbeep( void )
{
	MessageBeep( (UINT)-1 );
}

static void
wincshow( int nFlag )
{
	if( nFlag != NIL ) {
		CreateCaret( hCurWnd, (HBITMAP)NULL, nCharWidth, nCharHeight );
		(void)SetCaretPos(
				   (nDrawX - nHSPos) * nCharWidth,
				   nDrawY * nCharHeight
				 );
		ShowCaret( hCurWnd );
	} else	{
		DestroyCaret();
		HideCaret( hCurWnd );
	}
}

static int
winstringwidth( TCHAR *strg, int len )
{
	SIZE	sz;
	
	GetTextExtentPoint( hTTyDC, strg, len, &sz );

	return( sz.cx );
}

static void
winpaste( HWND hWnd )
{
	int	type;			/* Data type			*/
	HANDLE	hClipData;		/* handles to clip data		*/
	LPSTR	lpClipData;		/* Clip Data (adress 16:16)	*/

	if( OpenClipboard( hWnd ) ) {

		/*
		 *	get text from the clipboard
		 */

		if( (hClipData = GetClipboardData(CF_OEMTEXT)) != (HANDLE)0 )
			if( (hClipData=GetClipboardData(CF_TEXT))!=(HANDLE)0 ) {
				CloseClipboard();
				return;
			} else	type = CF_TEXT;
		else	type = CF_OEMTEXT;

		if( lpClipBuf != NULL )
			free( lpClipBuf );

		if( (lpClipBuf=(char *)malloc((UINT)GlobalSize(hClipData)))==NULL ) {
			CloseClipboard();
			return;
		}

		if( (lpClipData = GlobalLock( hClipData )) == NULL ) {
			CloseClipboard();
			return;
		}

		(void)strcpy( lpClipBuf, lpClipData );

		lpClipPtr = lpClipBuf;
		GlobalUnlock( hClipData );
		CloseClipboard();

#if	!defined( _WIN32_WCE )
		if( type == CF_OEMTEXT )
			OemToAnsi( lpClipBuf, lpClipBuf );
#endif
	}
}

static	void
winqueryfont( void )
{
}

static	void
winsetfontsize( int size )
{
	/*
	 *	Create a fixed font.
	 */

	if( hFont )
		DeleteObject( hFont  );

	memset( (char *)&lf, 0, sizeof( lf ) );
	lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
	lf.lfHeight         = size;
	lf.lfWeight         = FW_NORMAL;
	hFont	            = CreateFontIndirect( &lf ); /* SYSTEM_FONT */

	if( hFont == NULL )
		winprinterror( _T("Can't create Font"), FALSE );

}

static void
wingetcolor( int *color )
{
	static	DWORD	CustColors[ 16 ];
	RECT		rcClient;

	ColorChunk.lStructSize	= sizeof( CHOOSECOLOR );
	ColorChunk.hwndOwner	= hCurWnd;
	ColorChunk.hInstance	= NULL;
	ColorChunk.Flags	= 0;
	ColorChunk.lpCustColors	= &CustColors[ 0 ];

	if( ChooseColor( &ColorChunk ) ) {
		*color = ColorChunk.rgbResult;
		GetClientRect( hCurWnd, (LPRECT)&rcClient );
		InvalidateRect( hCurWnd, &rcClient, FALSE );
	}

}

int WINAPI
WinMain( HINSTANCE hInstance, HINSTANCE hPInst, LPWSTR lpCmdLine, int nCmdShow )
{
	TCHAR	*argv[ 32 ];
	int	argc = 0;

	if( hPInst ) {
		MessageBox(
			    GetFocus(),
			    _T("Application is alredy loaded."),
			    _T(""),
			    MB_ICONHAND /* | MB_SYSTEMMODAL */
			  );
		return( 0 );
	}

	hInst	  = hInstance;
	hAccTable = LoadAccelerators( hInst, _T("EmACTAccelerator") );
	hPrev	  = hPInst;
	nCmd	  = (int)nCmdShow;

	argv[ argc++ ] = szAppName;

	while( *lpCmdLine ) {
		while( *lpCmdLine && *lpCmdLine == ' ' )
			lpCmdLine++;
		if( *lpCmdLine )
			argv[ argc++ ] = lpCmdLine;
		else	break;
		while( *lpCmdLine && *lpCmdLine != ' ' )
			lpCmdLine++;
		if( *lpCmdLine == ' ' )
			*lpCmdLine++ = '\000';
	}

	argv[ argc ] = NULL;

	emacs( argc, argv );

	return( 0 );
}

static void
winprinterror( TCHAR *txt, BOOL bexit )
{
	DWORD err = GetLastError();
	TCHAR msg[256];

	(void)wsprintf( msg, _T("ERROR ID # %d, %s"), err, txt );
	MessageBox( NULL, msg, NULL, MB_OK );

	if( bexit )
		exit( 0 );
}

static void
winrawmode( void )
{
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
				/*
				 * Ctrl-G
				 */
				return( FALSE );
		}
		DispatchMessage( &msg );		
	}

	return( TRUE );
}

CMD
switchscreen( void )
{
	return( T );
}

static int
winquit( void )
{
	if( exitemacs() != FALSE )
		return( 0 );
	else	return( 1 );
}

LRESULT APIENTRY
MainWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
	PAINTSTRUCT	ps;
	HDC		hDC;
	RECT		rcClear;
	HFONT		hOldFont;
	HBRUSH		hOldBrush;

	switch( message ) {

	case WM_COMMAND:
		switch( wParam ) {
		case IDM_LOAD     :
			ofn.lpstrInitialDir = _wcegetenv( _T("EMACS") );

			if( GetOpenFileName( (LPOPENFILENAME)&ofn ) != 0 ) {
				(void)newfile( szFileName );
				update( T );
			}
			break;
		case IDM_QUIT :
			return( winquit() );
		case IDM_ESCAPE :
			PostMessage(hCurWnd,WM_CHAR,(WPARAM)0x1B,(LPARAM)0);
			break;
		case IDM_FONT :
			winqueryfont();
			winsettextattrib();
			GetClientRect( hCurWnd, (LPRECT)&rcClear );
			InvalidateRect( hCurWnd, &rcClear, TRUE );
			break;
		case IDM_FONT11 :
			winsetfontsize( 11 );
			winsettextattrib();
			GetClientRect( hCurWnd, (LPRECT)&rcClear );
			InvalidateRect( hCurWnd, &rcClear, TRUE );
			break;
		case IDM_FONT13 :
			winsetfontsize( 13 );
			winsettextattrib();
			GetClientRect( hCurWnd, (LPRECT)&rcClear );
			InvalidateRect( hCurWnd, &rcClear, TRUE );
			break;
		case IDM_BACKCOLOR :
			wingetcolor( (int *)&BackColor );
			DeleteObject( hBrush );
			hBrush = CreateSolidBrush( BackColor );
			break;
		case IDM_TEXTCOLOR :
			wingetcolor( (int *)&TextColor );
			break;
		case IDM_ABOUT :
			DialogBox(
				   hInst,
				   _T("AboutBox"),
				   hWnd,
				   (DLGPROC)About
				 );
			break;
                case IDM_PASTE:
			winpaste( hWnd );
			break;
		default :
			return( DefWindowProc(hWnd, message, wParam, lParam) );
		}
		return( TRUE );

	case WM_SETTINGCHANGE: {
		SHACTIVATEINFO sai;
		memset( &sai, 0, sizeof( SHACTIVATEINFO ) );

		/*
		 * This  will  force  a  HWND resize depending on the
		 * SIP condition
		 */
		SHHandleWMSettingChange( hWnd, -1, 0, &sai );
		break;
		}
	case WM_SIZE:
		if( nOpen && vtrunning() ) {
			wincshow( FALSE );
			vtfree();
			winsettextattrib();
			if( TTYnrow <= 1 )
				TTYnrow = 2;
			vtinit();
			resize();
			InvalidateRect( hCurWnd, (LPRECT)NULL, TRUE );
		}
		break;
	case WM_CREATE:
		/*
		 *	Create the objects
		 */
		break;

	case WM_CLOSE :
		if( hWnd == hCurWnd ) {
			return( winquit() );
		}
		break;
	case WM_HSCROLL : {
		HWND		Control = (HWND)lParam;
		SCROLLINFO	si;
		int		type;

		if( Control == (HWND)NULL ) {
			Control = hWnd;
			type	= SB_HORZ;
		} else	if( Control == hCurWnd ) {
			type	= SB_HORZ;
		} else	{
			type	= SB_CTL;
		}

		si.cbSize = sizeof( SCROLLINFO );
		si.fMask  = SIF_POS;
		GetScrollInfo( Control, type, &si );
		nHSPos	  = si.nPos;

		switch( (int)LOWORD(wParam) ) {
		case SB_LINELEFT      :
			nHSPos = max( nHSPos-1, 0);       break;
		case SB_LINERIGHT     :
			nHSPos = min( nHSPos+1, MINCOL);  break;
		case SB_PAGELEFT  :
			nHSPos = max( nHSPos-10, 0);      break;
		case SB_PAGERIGHT :
			nHSPos = min( nHSPos+10, MINCOL); break;
		case SB_THUMBPOSITION :
			nHSPos = HIWORD( wParam ); break;
		}

		SetScrollPos( Control, type, nHSPos, TRUE );
		SetViewportOrgEx( hTTyDC, -(nHSPos * nCharWidth), 0, NULL );
		InvalidateRect( hWnd, NULL, TRUE );
		break;
	}

	case WM_VSCROLL : {
		HWND		Control = (HWND)lParam;
		SCROLLINFO	si;
		int		type;

		if( Control == (HWND)NULL ) {
			Control = hWnd;
			type	= SB_VERT;
		} else	if( Control == hCurWnd ) {
			type	= SB_VERT;
		} else	{
			type	= SB_CTL;
		}

		si.cbSize = sizeof( SCROLLINFO );
		si.fMask  = SIF_POS;
		GetScrollInfo( Control, type, &si );
		nVSPos	  = si.nPos;

		switch( (int)LOWORD(wParam) ) {
		case SB_LINEUP    :
			if( nVSPos > 0   ) nVSPos--; break;
		case SB_LINEDOWN  :
			if( nVSPos < MAXROW ) nVSPos++; break;
		case SB_PAGEUP    :
			if( nVSPos >= 10 ) nVSPos -= 10; break;
		case SB_PAGEDOWN  :
			if( nVSPos <= (MAXROW-10) ) nVSPos += 10; break;
		case SB_THUMBPOSITION :
			nVSPos = HIWORD( wParam ); break;
		}

		SetScrollPos( Control, type, nVSPos, TRUE );
		InvalidateRect( hWnd, NULL, TRUE );

		break;
	}

	case WM_PAINT :
		hDC	  = BeginPaint( hWnd, &ps );
		hOldFont  = SelectObject( hDC, hFont  );
		hOldBrush = SelectObject( hDC, hBrush );

		SetBkColor(   hDC, BackColor );
		SetTextColor( hDC, TextColor );

		FillRect( hDC, (LPRECT)&ps.rcPaint, hBrush );

		if( nOpen && (hWnd == hCurWnd) ) {
			if( sgarbf != T )
				sgarbf = EXPOSE;
			update( T );
		}

		(void)SelectObject( hDC, hOldBrush );
		(void)SelectObject( hDC, hOldFont  );
		EndPaint( hWnd, &ps );
		break;

	case WM_LBUTTONDOWN:
		wincshow( FALSE );
		mevent.x      = (LOWORD(lParam) / nCharWidth) + nHSPos;
		mevent.y      = HIWORD(lParam) / nCharHeight;
		mevent.button = MButton1;
		if( GetKeyState( VK_SHIFT ) < 0 )
			mevent.button |= SHIFTBUTTON;
		if( GetKeyState( VK_CONTROL ) < 0 )
			mevent.button |= CTRLBUTTON;
		findwind();
		update( NIL );
		wincshow( TRUE );
		break;

	default:
		return( DefWindowProc( hWnd, message, wParam, lParam ) );
	}

	return( (LRESULT)0 );
}

LRESULT APIENTRY
About( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	rt;

	(void)lParam;
	(void)rt;

	switch( message ) {
	case WM_INITDIALOG:
		{
		SHINITDLGINFO shidi;
		shidi.dwMask  = SHIDIM_FLAGS;
		shidi.dwFlags = SHIDIF_DONEBUTTON | SHIDIF_SIZEDLG | SHIDIF_SIZEDLGFULLSCREEN;
		shidi.hDlg    = hDlg;
		SHInitDialog( &shidi );
		return( TRUE );
		}
		return( TRUE );

	case WM_COMMAND:
		if( (LOWORD(wParam) == IDOK) || (LOWORD(wParam) == IDCANCEL) ) {
			EndDialog( hDlg, LOWORD(wParam) );
			return( TRUE );
		}
		break;
	}

	return( FALSE );
}
