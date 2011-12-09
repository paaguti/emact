#if	!defined( lint )
static	char rcsid[] = "$Id: xpterm.c,v 1.20 2009/05/02 12:25:44 jullien Exp $";
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
 *	xpterm.c : The routines in this file provide support for WINDOWS.
 */

#if	!defined( STRICT )
#define	STRICT		/* Strict type checking for Windows */
#endif

#if	defined( _MSC_VER ) && (_MSC_VER >= 1400)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#define	_CRT_NON_CONFORMING_SWPRINTFS	1
#endif

#define	_WIN32_WINNT 0x0501

#if	defined( _WIDECHARS )
#if	!defined( _UNICODE )
#define	_UNICODE
#endif
#if	!defined( UNICODE )
#define	UNICODE
#endif
#endif

#include	<windows.h>
#include	<tchar.h>
#include	"emacs.h"
#include	"xpterm.h"

#if	defined( _UNICODE )
#define	system	_wsystem
#endif

/*
 *	Constant definitions
 */

#define	XP_EMACS_WITH_SYSTEM_COLORS
#define	XP_EMACS_ALTERNATE_FONT_SIZE	12
#define	XP_EMACS_STANDARD_FONT_SIZE	10
#define	XP_EMACS_DEFAULT_FONT_SIZE	XP_EMACS_STANDARD_FONT_SIZE
#define	XP_EMACS_MENU			MAKEINTRESOURCE( EMACTMENU )
#define	XP_EMACS_APPNAME		_T("emacs")
#define	XP_EMACS_LOAD_MESSAGE		_T("EmACTLoadMessage")
#define	XP_EMACS_FIND_MESSAGE		_T("EmACTFindMessage")
#define	XP_EMACS_CLASS			_T("EmACTWClass")
#define	XP_EMACS_CS			CS_BYTEALIGNCLIENT|CS_HREDRAW|CS_VREDRAW
#define	XP_EMACS_MAXPATH		256

/*
 *	Private variables
 */

static	HWND      xpwnd;		/* Current Window		*/
static	HMENU	  xpmenu;		/* Application Menu		*/
static	HINSTANCE xpinst;		/* hInstance			*/
static	HDC	  xpdc;			/* TTy DC			*/
static	DWORD	  xpbgcolor;		/* Current background color	*/
static	DWORD	  xpfgcolor;		/* Current foreground color	*/
static	HBRUSH	  xpbrush;		/* Current brush		*/
static	HFONT	  xpfont;		/* Current font			*/
static	int	  xpcharwidth;		/* Width of a char		*/
static	int	  xpcharheight;		/* Height of a char		*/
static	HANDLE	  xpprev;		/* Previous instance		*/
static	HANDLE	  xpacctable;	        /* Accelerator table		*/
static	LPSTR	  xpclipbuf  = NULL;	/* Pointers to clip buffer	*/
static	LPSTR	  xpclipptr  = NULL;	/* Current ClipPtr		*/
static	int	  xpstrokes  = 0;	/* to check for mouse move	*/
static	int	  xpcmdshow  = 0;	/* Command line			*/
static	int	  xpdrawxpos = 0;	/* X cursor bit position	*/
static	int	  xpdrawx    = 0;	/* X cursor			*/
static	int	  xpdrawy    = 0;	/* Y cursor			*/
static	int	  xpopenp    = 0;	/* Open flag			*/
static	int	  xpchar     = -1;	/* Next char			*/
static	int       xpcursoron = 0;	/* Showing cursor		*/
static	UINT	  xploadmsg;		/* Registered load message	*/
static	UINT	  xpfindmsg;		/* Registered find message	*/
static	HWND	  xpcurdlg   = NULL;	/* Current dialog box		*/

#if	defined( _UNICODE )
#define	XP_DRAW_X	(xpdrawxpos)
#define	XP_DRAW_Y	(xpdrawy * xpcharheight)
#else
#define	XP_DRAW_X	(xpdrawx * xpcharwidth)
#define	XP_DRAW_Y	(xpdrawy * xpcharheight)
#endif

/*
 *	Shared memory
 */

#if	defined( _MSC_VER )
#define	XP_EMACS_SHMEM
#pragma	data_seg( ".shared" )
_declspec(allocate( ".shared" )) TCHAR	xpshared[ XP_EMACS_MAXPATH ];
#pragma	data_seg()

#pragma	comment( linker, "/SECTION:.shared,RWS" )
#endif

/*
 *	User color definitions
 */

#define	XPCOLOR_BLACK		RGB(   0,   0,   0 )
#define	XPCOLOR_BLUE		RGB(   0,   0, 128 )
#define	XPCOLOR_GREEN		RGB(   0, 255,   0 )
#define	XPCOLOR_CYAN		RGB( 128, 255, 255 )
#define	XPCOLOR_RED		RGB( 255,   0,   0 )
#define	XPCOLOR_MAGENTA		RGB( 255,   0, 255 )
#define	XPCOLOR_YELLOW 		RGB( 255, 255,   0 )
#define	XPCOLOR_WHITE		RGB( 255, 255, 255 )

/*
 *	Other color definitions
 */

#define	XPCOLOR_GRAY		RGB( 128, 128, 128 )
#define	XPCOLOR_LIGHTGRAY	RGB( 192, 192, 192 )
#define	XPCOLOR_DARK_CYAN	RGB(  96, 192, 192 )
#define	XPCOLOR_DARK_BLUE	RGB(   0,  36,  86 )

static	COLORREF  xpcolortable[] = {
	XPCOLOR_BLACK,
	XPCOLOR_DARK_BLUE,	/* XPCOLOR_DARK_BLUE */
	XPCOLOR_GREEN,
	XPCOLOR_CYAN,
	XPCOLOR_RED,
	XPCOLOR_MAGENTA,
	XPCOLOR_YELLOW,
	XPCOLOR_WHITE
};

/*
 *	Other constants
 */

#define	XP_NO_CHAR	-1

/*
 *	Structure for GetOpenFileName
 */

static	OPENFILENAME	xpofn;

/*
 *	Structure for FindText
 */

static	FINDREPLACE	xpfind;

/*
 *	Standard terminal interface dispatch table.
 */

static	void	xpopen( void );
static	void	xpclose( void );
static	int	xpgetc( void );
static	void	xpputc( int c );
static	void	xpputs( TCHAR *szText, int nSize );
static	void	xpmove( int row, int col );
static	void	xpeeol( void );
static	void	xpeeop( void );
static	void	xpflush( void );
static	void	xpbeep( void );
static	void	xpei( void );
static	void	xpsi( void );
static	void	xpcshow( int f );
static	int	xpcheck( void );
static	void	xprawmode( void );
static  int	xpdecodechar( int c );
static	void	xpmousebottom( void );
static	void	xpcutcopy( WPARAM wParam );
static	void	xphelp( HWND hwnd );
static	void	xpclippaste( void );
static	void	xpclipcopy( void );
static	TCHAR * xptitle( TCHAR *buf, TCHAR *fname );
static	void	xpprint( void );

/*
 *	Tools
 */

static	int	xpgetcurwidth( void );
static	void	xpsetfontsize( int size );
static	void	xpchangefont( int size );
static	void	xpsettextattrib( void );
static	void	xpprinterror( TCHAR *txt , BOOL fexit );
static	int	xpquit( void );
static	int	xpsystemspawn( const TCHAR *cmd );
static  void	xpshowlines( TCHAR *chBuf, int nLen );
static	HDC	xpgetprinterdc( void );
static	void	xpfindreplace( int replacep );
static	int	xpfindmessage( LPFINDREPLACE lpfr );

#if	defined( _UNICODE )
static	EMCHAR	xpunicodegen( void );
#endif

/*
 *	Windows entry point and callbacks
 */

int	WINAPI	 _tWinMain( HINSTANCE, HINSTANCE, LPTSTR, int );
static  LRESULT  APIENTRY xpmainwndproc( HWND, UINT, WPARAM, LPARAM );
static  LRESULT  APIENTRY xpabout( HWND, UINT, WPARAM, LPARAM );

/*
 *	Terminal instance
 */

TERM	term = {
	0,
	0,
	0,
	NIL,
	xpopen,
	xpclose,
	xpgetc,
	xpputc,
	xpputs,
	xpflush,
	xpmove,
	xpeeol,
	xpeeop,
	xpbeep,
	xpsi,
	xpei,
	xpcshow,
	xpcheck,
	xprawmode
};

/*
 *	Process message while waiting.
 */

static	void
xpwait( void )
{
	MSG	msg;

	PeekMessage( &msg, xpwnd, 0, 0, PM_NOREMOVE );
}

static int
xpgetcurwidth()
{
#if	defined( _UNICODE )
	INT	width;

	GetCharWidth32( xpdc, (UINT)curchar, (UINT)curchar, &width );
	return( (int)width );
#else
	return( xpcharwidth );
#endif
}

static void
xpsettextattrib( void )
{
	TEXTMETRIC	txMetric;
	RECT		rcClient;
	ABC		abc;
	int	        nParentWidth;	/* Current window width		*/
	int	        nParentHeight;	/* Current window height	*/

	GetClientRect( xpwnd, (LPRECT)&rcClient );

	GetTextMetrics( xpdc, &txMetric );

	nParentWidth  = rcClient.right  - rcClient.left;
	nParentHeight = rcClient.bottom - rcClient.top;
	xpcharwidth   = txMetric.tmAveCharWidth;
	xpcharheight  = txMetric.tmHeight + txMetric.tmExternalLeading;

	if( (txMetric.tmPitchAndFamily & TMPF_TRUETYPE) == 0 )
		GetCharWidth32( xpdc, (UINT)'M', (UINT)'M', &xpcharwidth );
	else	if( GetCharABCWidths( xpdc, (UINT)'M', (UINT)'M', &abc ) )
		xpcharwidth = (int)(abc.abcA + abc.abcB + abc.abcC);
	else	GetCharWidth32( xpdc, (UINT)'M', (UINT)'M', &xpcharwidth );

	TTYncol		= (int)(nParentWidth  / xpcharwidth);
	TTYnrow		= (int)(nParentHeight / xpcharheight) - 1;
	TTYinit		= T;
	line_number_mode= T;
}

static void
xpchangefont( int font )
{
	xpsetfontsize( font );
	xpsettextattrib();

	if( TTYnrow <= 1 )
		TTYnrow = 2;

	if( xpopenp && vtrunning() ) {
		vtinit();
		resize();
		InvalidateRect( xpwnd, (LPRECT)NULL, TRUE );
	}
}

static void
xpopen( void )
{
	static TCHAR  filename[ XP_EMACS_MAXPATH ];
	static TCHAR* filefilter =
		_T("C/C++ Files (*.c, *.cpp, *.cc)\0*.c;*.cpp;*.cc;*.h;*.hpp\0")
		_T("C# Files (*.cs)\0*.cs\0")
		_T("Java Files (*.java*)\0*.l*\0")
		_T("Lisp Files (*.lsp, *.lisp, *.l*)\0*.lsp;*.lisp;*.l*\0")
		_T("Shell Files (*.sh)\0*.sh\0")
		_T("Text Files (*.txt)\0*.txt\0")
		_T("TDB Files (*.tdb)\0*.tdb\0")
		_T("All Files (*.*)\0*.*\0");

	RECT	      rc;
	HICON	      hIcon;
	BYTE	      bKeyState[ 256 ];	/* MS says 256, so 256! */

	/*
	 *	Set the Insert key of the keyboard to untoggle
	 */

	GetKeyboardState( (LPBYTE) &bKeyState );
	bKeyState[ VK_INSERT ] &= ~(-1);
	SetKeyboardState( (LPBYTE) &bKeyState );

	if( system_colors == NIL ) {
		xpbgcolor = xpcolortable[ background_color ];
		xpfgcolor = xpcolortable[ foreground_color ];
	} else	{
		xpbgcolor = GetSysColor( COLOR_WINDOW     );
		xpfgcolor = GetSysColor( COLOR_WINDOWTEXT );
	}

	xpbrush	  = CreateSolidBrush( xpbgcolor );
	hIcon	  = LoadIcon( xpinst, MAKEINTRESOURCE( EMACTICON ) );

	if( xpprev == NULL ) {
		WNDCLASS wc;

		wc.style	 = XP_EMACS_CS;
		wc.lpfnWndProc	 = (WNDPROC)xpmainwndproc;
		wc.cbClsExtra	 = 0;
		wc.cbWndExtra	 = 0;
		wc.hInstance	 = xpinst;
		wc.hIcon	 = hIcon;
		wc.hCursor	 = LoadCursor( (HANDLE)0, IDC_ARROW );
		wc.lpszMenuName  = NULL; /* XP_EMACS_MENU */
		wc.hbrBackground = xpbrush;
		wc.lpszClassName = XP_EMACS_CLASS;

		if( RegisterClass( &wc ) == 0 ) {
			xpprinterror( _T("Can't register class"), TRUE );
			return;
		}
	} else	{
		xpprinterror( _T("xpopen1"), FALSE );
	}

	xpmenu = LoadMenu( xpinst, XP_EMACS_MENU );

	xpwnd  = CreateWindowEx(
				 WS_EX_ACCEPTFILES,
				 XP_EMACS_CLASS,
				 XP_EMACS_APPNAME,
				 WS_OVERLAPPEDWINDOW,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 CW_USEDEFAULT,
				 (HWND)0,
				 (HMENU)0,
				 xpinst,
				 NULL
			       );

	if( xpwnd == NULL )
		xpprinterror( _T("Can't create Window"), TRUE );

	if( show_menu )
		SetMenu( xpwnd, xpmenu );

	xpdc = GetDC( xpwnd );

	/*
	 *	Create a fixed font.
	 */

	xpsetfontsize( XP_EMACS_DEFAULT_FONT_SIZE );

	(void)SelectObject( xpdc, xpbrush );
	(void)SelectObject( xpdc, xpfont  );

	xpsettextattrib();

#if	defined( _WIN32 )
	SetClassLongPtr( xpwnd, GCLP_HICON, (LONG)(LONG_PTR)hIcon );
#else
	SetClassLongPtr( xpwnd, GCLP_HICON, (LONG_PTR)hIcon );
#endif
	xpdrawx    = 0;
	xpdrawy    = 0;
	xpdrawxpos = 0;

	SetBkColor(   xpdc, xpbgcolor );
	SetTextColor( xpdc, xpfgcolor );

	/*
	 *	fill in non-variant fields of OPENFILENAME struct.
	 */

	xpofn.lStructSize	= sizeof( OPENFILENAME );
	xpofn.hwndOwner		= NULL;
	xpofn.hInstance		= NULL;
	xpofn.lpstrFilter	= filefilter;
	xpofn.lpstrCustomFilter	= NULL;
	xpofn.nMaxCustFilter	= 0;
	xpofn.nFilterIndex	= 1;
	xpofn.lpstrFile		= filename;
	xpofn.nMaxFile		= XP_EMACS_MAXPATH;
	xpofn.lpstrFileTitle	= NULL;
	xpofn.nMaxFileTitle	= 0;
	xpofn.lpstrTitle	= NULL;
	xpofn.lpstrDefExt	= _T("lsp");
	xpofn.Flags		= OFN_EXPLORER     | OFN_ENABLESIZING    |
				  OFN_HIDEREADONLY | OFN_FORCESHOWHIDDEN ;

	/*
	 * Set clipborad functions.
	 */

	widget.w_clipcopy       = xpclipcopy;
	widget.w_clippaste      = xpclippaste;
	widget.w_title          = xptitle;
	widget.w_wait		= xpwait;
	printer.p_print	        = xpprint;

	xpsettextattrib();

	/*
	 * Set the window on the left, taking care of icons
	 */

	SystemParametersInfo( SPI_GETWORKAREA, 0, &rc, 0 );

	SetWindowPos(
		      xpwnd,
		      HWND_TOP,
		      GetSystemMetrics( SM_CXICONSPACING ),
		      0,
		      xpcharwidth * (80+1),
		      rc.bottom - rc.top,
		      SWP_NOZORDER | SWP_NOACTIVATE
		    );

	xpsettextattrib();

	(void)ShowWindow( xpwnd, xpcmdshow );

	if( UpdateWindow( xpwnd ) == 0 )
		xpprinterror( _T("Can't update window"), TRUE );

	xpopenp = 1;

}

static void
xpfindreplace( int replacep )
{
	static int    firstinit = 0;
	static EMCHAR szFind[ NLINE ];
	static EMCHAR szReplace[ NLINE ];

	if( firstinit == 0 ) {
		memset( (char *)&xpfind, 0, sizeof( FINDREPLACE ) );
		xpfind.lStructSize      = sizeof( FINDREPLACE );
		xpfind.hwndOwner        = xpwnd;
		xpfind.hInstance        = xpinst;
		xpfind.lpstrFindWhat    = szFind;
		xpfind.wFindWhatLen     = NLINE;
		xpfind.lpstrReplaceWith = szReplace;
		xpfind.wReplaceWithLen  = NLINE;
		xpfind.Flags		= FR_HIDEWHOLEWORD|FR_DOWN|FR_MATCHCASE;
	}

	if( replacep == NIL )
		xpcurdlg = FindText( &xpfind );
	else	xpcurdlg = ReplaceText( &xpfind );
}

static int
xpfindmessage( LPFINDREPLACE lpfr )
{
	DWORD		dwFlags   = lpfr->Flags;
	int		replaced  = 0;
	int		nFindFlag = T;
	EMCHAR		*szFind   = lpfr->lpstrFindWhat;
	EMCHAR		*szReplace= lpfr->lpstrReplaceWith;

	lastflag	 = CFFSRC;
	case_sensitivity = (int)(dwFlags & FR_MATCHCASE);

	xpcshow( FALSE );
	(void)emstrcpy( search_buffer, szFind );

	if( dwFlags & FR_DIALOGTERM ) {
		xpcurdlg = NULL;
		return( 0 );
	}

	if( (dwFlags & FR_FINDNEXT) && nFindFlag ) {
		if( dwFlags & FR_DOWN )
			nFindFlag = forwsearch();
		else	nFindFlag = backsearch();
	}

	if( (dwFlags & FR_REPLACE) && nFindFlag ) {
		curwp->w_dotp  = found_p;
		curwp->w_doto  = found_o;
		curwp->w_flag |= WFHARD;
		subst( (int)emstrlen( szFind ), szReplace );
		replaced++;
		if( dwFlags & FR_DOWN )
			nFindFlag = forwsearch();
		else	nFindFlag = backsearch();
	}

	if( (dwFlags & FR_REPLACEALL) && nFindFlag )
		do {
			int	len = (int)emstrlen( szFind );

			curwp->w_dotp  = found_p;
			curwp->w_doto  = found_o;
			curwp->w_flag |= WFHARD;
			subst( len, szReplace );
			replaced++;
			nFindFlag = FALSE;
		} while( ffindstring() );

	update( T );
	if( replaced )
		WDGwrite( ECSTR("Replaced %d occurence(s)"), replaced );
	xpcshow( TRUE );
	return( 0 );
}

static void
xpclose( void )
{
	DeleteObject( xpbrush );
	DeleteObject( xpfont  );
	ReleaseDC( xpwnd, xpdc );
	DestroyWindow( xpwnd );
	PostQuitMessage( 0 );
}

static void
xpputs( TCHAR *szText, int nSize )
{
	SIZE size;

	ExtTextOut(
		    xpdc,
		    XP_DRAW_X,
		    XP_DRAW_Y,
		    ETO_CLIPPED,
		    (RECT *)NULL,
		    &szText[0],
		    nSize,
		    (const int *)NULL
		  );

	GetTextExtentPoint32( xpdc, szText, nSize, &size );

	xpdrawx    += nSize;
	xpdrawxpos += size.cx;
}

static void
xpputc( int aChar )
{
	TCHAR	c[] = { (TCHAR)'?', (TCHAR)0 };

	c[0] = (TCHAR)aChar;

	xpputs( (TCHAR *)&c, 1 );
}

/*
 *	Find actual buffer position from mouse pointer.
 */

static int
xpposfrompoint( int x, int y )
{
#if	defined( _UNICODE )
	VIDEO** video = getvscreen();
	EMCHAR*	lpString = video[y]->text;
	SIZE	size;
	int	i;

	for( i = 0 ; i < TTYncol; i++ ) {
		GetTextExtentPoint32( xpdc, lpString, i, &size );
		if( size.cx > (x * xpcharwidth) ) {
			return( i - 1 );
		}
	}

#endif
	(void)y;
	return( x );
}

static void
xpmove( int row, int col )
{
#if	defined( _UNICODE )
	VIDEO** video = getvscreen();
	EMCHAR*	lpString = video[row]->text;
	SIZE	size;

	GetTextExtentPoint32( xpdc, lpString, col, &size );

	xpdrawxpos = size.cx;
#endif
	xpdrawx    = col;
	xpdrawy    = row;
}

#define	CTRLP	(GetKeyState(VK_CONTROL) < 0)

static int
xpdecodechar( int c )
{
	int	aChar = XP_NO_CHAR;

	switch( c ) {
	case VK_UP    : aChar = Ctrl|'P'; break;
	case VK_DOWN  : aChar = Ctrl|'N'; break;
	case VK_DELETE: aChar = Ctrl|'D'; break;
	case VK_NEXT  : aChar = Ctrl|'V'; break;
	case VK_INSERT: aChar = META|'I'; break;
	case VK_END   : aChar = CTRLP?(CTLX|Ctrl|'N'):(META|'>'); break;
	case VK_HOME  : aChar = CTRLP?(CTLX|Ctrl|'P'):(META|'<'); break;
	case VK_PRIOR : aChar = META|'V'; break;
	case VK_LEFT  : aChar = CTRLP?(META|'B'):(Ctrl|'B'); break;
	case VK_RIGHT : aChar = CTRLP?(META|'F'):(Ctrl|'F'); break;
	case VK_F1    : xphelp( xpwnd ); break;
	case VK_F2    : aChar = Ctrl|'S'; break;
	case VK_F3    : aChar = META|Ctrl|'F'; break;
	case VK_F4    : aChar = CTLX|Ctrl|'I'; break;
	case VK_F5    : break; /* switchscreen(); */
	case VK_F6    : aChar = CTLX|METACH; break;
	case VK_F7    : aChar = CTLX|'`'; break;
	case VK_F8    : aChar = CTLX|Ctrl|'S'; break;
	case VK_F9    : aChar = CTLX|'E'; break;
#if	defined( _UNICODE )
		case VK_F12   : aChar = xpunicodegen(); break;
#endif
	}

	return( aChar );
}

static int
xpgetc( void )
{
	int	aChar = XP_NO_CHAR;
	MSG	msg;

	/*
	 *	Check if a char is in clipboard
	 */

	if( xpclipptr ) {
		aChar = (int)*xpclipptr++;

		if( *xpclipptr == '\000' ) {
			free( xpclipbuf );
			xpclipbuf = NULL;
			xpclipptr = NULL;
		}

		return( aChar );
	}

	if( xpchar != XP_NO_CHAR )
		return( xpchar );

	/*
	 *	Wait for a char (or a mouse event)
	 */

	while( aChar == XP_NO_CHAR ) {
		xpcshow( TRUE );

		if( GetMessage( &msg, (HWND)0, 0, 0 ) == 0 ) {
			return( (int)msg.wParam );
		}

		if( xpcurdlg && IsDialogMessage(xpcurdlg, &msg) ) {
			/*
			 * Process Dialog Box
			 */
			continue;
		}

		if( TranslateAccelerator( msg.hwnd, xpacctable, &msg ) ) {
			continue;
		}

		TranslateMessage( &msg );
		DispatchMessage( &msg );

		if( xpclipptr )
			xpchar = xpgetc(); /* data in clipboard */

		if( xpchar != XP_NO_CHAR ) {
			aChar  = xpchar;
			xpchar = XP_NO_CHAR;
		}
	}

	xpcshow( FALSE );

	if( mouse_avoidance_mode == T ) {
	    if( (aChar != MEVT) && (++xpstrokes%mouse_avoidance_nudge)==0 ) {
		/*
		 * every 3 strokes, move mouse to the bottom
		 */
		xpmousebottom();
	    }
	}

	return( aChar );

}

static void
xpeeop( void )
{
	RECT	rcClear;

	GetClientRect( xpwnd, (LPRECT)&rcClear );
	FillRect( xpdc, (LPRECT)&rcClear, xpbrush );
}

static void
xpeeol( void )
{
	RECT	rcClear;

	rcClear.top    = xpdrawy * xpcharheight;
	rcClear.bottom = (xpdrawy + 1) * xpcharheight;
	rcClear.left   = XP_DRAW_X;
	rcClear.right  = TTYncol * xpcharwidth;

	FillRect( xpdc, (LPRECT)&rcClear, xpbrush );
}

static void
xpsi( void )
{
	if( xpfgcolor == XPCOLOR_CYAN )
		SetBkColor( xpdc, XPCOLOR_WHITE );
	else	SetBkColor( xpdc, xpfgcolor     );
	SetTextColor( xpdc, xpbgcolor );
}

static void
xpei( void )
{
	SetBkColor(   xpdc, xpbgcolor );
	SetTextColor( xpdc, xpfgcolor );
}
 
static void
xpflush( void )
{
}

static void
xpbeep( void )
{
	/*
	 *	Beep( Frequecy, Duration ).
	 *	Parameters are ignored on Windows 95.
	 */

#if	defined( XP_EMACS_SOUND_CARD_BEEP )
	MessageBeep( (UINT)-1 );
#else
	Beep( 0x333, 0x10 );
#endif
}

static void
xpcshow( int nFlag )
{
#if	defined( XP_HARDWARE_CARET )
	if( nFlag != NIL ) {
		CreateCaret( xpwnd, (HBITMAP)NULL, xpcharwidth, xpcharheight );
		(void)SetCaretPos( XP_DRAW_X, XP_DRAW_Y );
		ShowCaret( xpwnd );
	} else	{
		DestroyCaret();
		HideCaret( xpwnd );
	}
	xpcursoron = nFlag;
#else
	int	x;
	int	y;

	if( nFlag != xpcursoron )
		xpcursoron = nFlag;
	else	return;

	if( (IsIconic( xpwnd ) != FALSE) )
		return;

	x = XP_DRAW_X;
	y = XP_DRAW_Y;

	SetBkColor(   xpdc, nFlag ? xpfgcolor : xpbgcolor );
	SetTextColor( xpdc, nFlag ? xpbgcolor : xpfgcolor );
	ExtTextOut(   xpdc, x, y, ETO_CLIPPED, NULL, &curchar, 1, NULL );
	SetBkColor(   xpdc, xpbgcolor );
	SetTextColor( xpdc, xpfgcolor );
#endif
}

static	void
xpsetfontsize( int size )
{
#if	defined( XP_EMACS_CREATE_FONT )
	LOGFONT		lf;

	/*
	 *	Create a fixed font.
	 */

	if( xpfont )
		DeleteObject( xpfont );

	memset( (char *)&lf, 0, sizeof( lf ) );
	lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
	lf.lfHeight         = -MulDiv(size,GetDeviceCaps(xpdc,LOGPIXELSY),72);
	lf.lfWeight         = FW_BOLD;
	lf.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	xpfont	            = CreateFontIndirect( &lf ); /* SYSTEM_FONT */

	if( xpfont == NULL )
		xpprinterror( _T("Can't create Font"), FALSE );

	if( xpdc != NULL )
		(void)SelectObject( xpdc, xpfont );
#else
	if( size == XP_EMACS_DEFAULT_FONT_SIZE )
		xpfont = (HFONT)GetStockObject( SYSTEM_FIXED_FONT );
	else	xpfont = (HFONT)GetStockObject( ANSI_FIXED_FONT );

	if( xpdc != NULL )
		(void)SelectObject( xpdc, xpfont );
#endif
}


static void
xpprinterror( TCHAR *txt, BOOL bexit )
{
	DWORD err = GetLastError();
	TCHAR msg[256];

	(void)wsprintf( msg, _T("ERROR ID # %d, %s"), err, txt );
	MessageBox( NULL, msg, NULL, MB_OK );

	if( bexit )
		exit( 0 );
}

static void
xprawmode( void )
{
}

static	int
xpcheck( void )
{
	MSG	msg;

	/*
	 *	Process messages intended for the abort dialog box
	 */

	while( PeekMessage( &msg, NULL, FALSE, FALSE, TRUE ) ) {
		if( TranslateAccelerator( msg.hwnd, xpacctable, &msg ) )
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
xpquit( void )
{
	static TCHAR * warning =
	       _T("Modified buffer exist, do you really want to exit?");

	if( confirm_unsaved_buffer == T ) {
		int ret;

		if( anycb( ANYCB_CHECK ) == NIL ) {
			(void)killemacs();
			return( TRUE );
		}
	
	        ret = MessageBox(
			          NULL,
				  warning,
				  _T("EmACT"),
				  MB_ICONQUESTION | MB_APPLMODAL | MB_TOPMOST |MB_YESNO
				);

		if( ret == IDYES ) {
			(void)killemacs();
			return( TRUE );
		} else	{
			return( FALSE );
		}
	} else	{
		(void)killemacs();
		return( TRUE );
	}
}

static LRESULT APIENTRY
xpmainwndproc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
	PAINTSTRUCT	ps;

	switch( message ) {

	case WM_COMMAND:
		switch( wParam ) {
		case IDM_LOAD:
			xpofn.lpstrInitialDir = NULL;
			if( GetOpenFileName( (LPOPENFILENAME)&xpofn ) != 0 ) {
				(void)newfile( xpofn.lpstrFile );
				update( T );
			}
			break;
		case IDM_PRINT:
			xpprint();
			break;
		case IDM_QUIT:
			return( xpquit() );
		case IDM_STANDARDFONT:
			xpchangefont( XP_EMACS_STANDARD_FONT_SIZE );
			break;
		case IDM_ALTERNATEFONT:
			xpchangefont( XP_EMACS_ALTERNATE_FONT_SIZE );
			break;
		case IDM_UTF8ENCODING:
			utf8encoding();
			update( T );
			break;
		case IDM_UTF16ENCODING:
			utf16encoding();
			update( T );
			break;
		case IDM_DEFAULTENCODING:
			systemencoding();
			update( T );
			break;
		case IDM_HELPEMACS:
			xphelp( xpwnd );
			break;
		case IDM_ABOUT:
			DialogBox(
				   xpinst,
				   _T("AboutBox"),
				   hWnd,
				   (DLGPROC)xpabout
				 );
			break;
                case IDM_CUT:
			killregion();
			update( T );
			break;
                case IDM_COPY:
			copyregion();
			break;
                case IDM_PASTE:
			yank();
			update( T );
			break;
		case IDM_FIND:
			xpfindreplace( NIL );
			break;
		case IDM_REPLACE:
			xpfindreplace( T );
			break;
		default :
			return( DefWindowProc(hWnd, message, wParam, lParam) );
		}
		return( TRUE );

	 case WM_ACTIVATE:
		if( hWnd != xpwnd )
			break;

		if( system_colors )
			break;

		if( xpbgcolor == XPCOLOR_DARK_BLUE ) {
			if( LOWORD(wParam) == WA_INACTIVE )
				xpfgcolor = XPCOLOR_DARK_CYAN;
			else	xpfgcolor = xpcolortable[ foreground_color ];

			sgarbf = T;
			update( T );
			InvalidateRect( xpwnd, (LPRECT)NULL, TRUE );
			UpdateWindow( xpwnd );
		}

		break;

	case WM_SIZE:
		if( xpopenp && vtrunning() && !IsIconic(xpwnd) ) {
			vtfree();
			xpsettextattrib();
			if( TTYnrow <= 1 )
				TTYnrow = 2;
			vtinit();
			resize();
			sgarbf = T;
			update( T );
			InvalidateRect( xpwnd, (LPRECT)NULL, TRUE );
			UpdateWindow( xpwnd );
		}
		break;

	case WM_CREATE:
		/*
		 *	Create the objects
		 */
		break;

	case WM_CLOSE :
		return( xpquit() );

	case WM_QUERYENDSESSION :
		return( xpquit() );

	case WM_PAINT :
		xpcshow( FALSE );
		BeginPaint( hWnd, &ps );
		if( xpopenp ) {
			if( sgarbf != T )
				sgarbf = EXPOSE;
			update( T );
		}
		EndPaint( hWnd, &ps );
		break;

	case WM_KEYDOWN :
		xpchar = xpdecodechar( (int)wParam );
		break;

	case WM_CHAR :
		if( CTRLP && ((int)wParam)==' ' )
			/* CTRL-SPACE: it's a mark set */
			xpchar = 0;
		else	xpchar = (int)wParam;
		break;

	case WM_LBUTTONDOWN:
	case WM_RBUTTONDOWN:
	case WM_MBUTTONDOWN:
		mevent.x      = LOWORD(lParam) / xpcharwidth;
		mevent.y      = HIWORD(lParam) / xpcharheight;
		mevent.x      = xpposfrompoint( mevent.x, mevent.y );
		mevent.button = (message==WM_LBUTTONDOWN)?MButton1:MButton2;
		if( GetKeyState( VK_SHIFT ) < 0 )
			mevent.button |= SHIFTBUTTON;
		if( GetKeyState( VK_CONTROL ) < 0 )
			mevent.button |= CTRLBUTTON;
		xpchar = MEVT;
		break;

	case WM_MOUSEWHEEL :
		if( (short)HIWORD( wParam ) < 0 )  /* zDelta */
			(void)forwpage();
		else	(void)backpage();
		update( T );
		break;

	case WM_DROPFILES: {
		TCHAR szDragFile[ NFILEN ];
		DragQueryFile((HDROP)wParam,0,(TCHAR *)&szDragFile[0],NFILEN);
		(void)newfile( szDragFile );
		update( T );
		break;
		}

	default:
		if( message == xploadmsg ) {
			if( lParam ) {
				(void)newfile( (TCHAR *)lParam );
				update( T );
			}
			return( (LRESULT)0 );
		} else	if( message == xpfindmsg ) {
			FINDREPLACE *lpfr = (FINDREPLACE *)lParam;
			xpfindmessage( lpfr );
			return( (LRESULT)0 );
		}
		return( DefWindowProc( hWnd, message, wParam, lParam ) );
	}

	return( (LRESULT)0 );
}

static LRESULT APIENTRY
xpabout( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
	RECT	aRec;

	(void)lParam;

	switch( message ) {
	case WM_INITDIALOG:
#if	defined( XP_EMACS_CENTERED_ABOUT )
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
#else
		(void)aRec;
#endif
		return( (LRESULT)TRUE );

	case WM_COMMAND:
		if( (LOWORD(wParam) == IDOK) || (LOWORD(wParam) == IDCANCEL) ) {
			EndDialog( hDlg, LOWORD(wParam) );
			return( TRUE );
		}
		break;
	}

	return( FALSE );
}

static	void
xpmousebottom()
{
	RECT	rect;

	GetWindowRect( xpwnd, &rect );
	SetCursorPos( rect.right - xpcharwidth, rect.bottom - xpcharheight );
}

static	void
xphelp( HWND hwnd )
{
	WinHelp( hwnd, _T("emacs.hlp"), HELP_INDEX, 0L );
}

static	TCHAR *
xptitle( TCHAR *buf, TCHAR *fname )
{
	static	TCHAR	title[ 64 ] = { 0 };

	/*
	 * The next two lines remove warning on args not used !!
	 */

	if( buf != fname )
		fname = buf;

	if( _tcscmp( buf, title ) != 0 ) {
		(void)_stprintf(
				 title,
				 _T("%s (%dx%d)"),
				 fname,
				 TTYnrow,
				 TTYncol
			       );
		SetWindowText( xpwnd, title );
	}

	return( buf );
}

static	void
xpcutcopy( WPARAM wParam )
{
	UINT	AllocFlags = GMEM_MOVEABLE|GMEM_DDESHARE;
	HANDLE	hClipData;
	LPTSTR	lpClipData;
	size_t	size = (kused + 1) * sizeof (EMCHAR );

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

	if( (hClipData = GlobalAlloc( AllocFlags, size )) == NULL ) {
		return;
	}

	/*
	 *	Copy data
	 */

	lpClipData = (LPTSTR)GlobalLock( hClipData );

	if( lpClipData == NULL ) {
		return;
	}

	(void)_tcsncpy( lpClipData, kbufp, kused );
	lpClipData[ kused ] = 0;

	GlobalUnlock( hClipData );

	/*
	 *	Clear the current contents of the clipboard, and set
	 *	the data handle to the new string.
	 */

	if( OpenClipboard( xpwnd ) == 0 )
		WDGerror( _T("Can't open clipboard.") );
	else	{
		EmptyClipboard();
#if	defined( _UNICODE )
		if( SetClipboardData( CF_UNICODETEXT, hClipData ) == NULL )
			WDGerror( _T("Can't set UNICODE clipboard data.") );
#else
		if( SetClipboardData( CF_TEXT, hClipData ) == NULL )
			WDGerror( _T("Can't set clipboard data.") );
#endif
		CloseClipboard();
	}

	if( wParam == IDM_CUT ) {
		(void)killregion();
		EnableMenuItem( GetMenu( xpwnd ), IDM_CUT,  MF_GRAYED );
		EnableMenuItem( GetMenu( xpwnd ), IDM_COPY, MF_GRAYED );
	}

	GlobalUnlock( hClipData );
}

static	void
xpclipcopy( void )
{
	xpcutcopy( WM_COPY );
}

static void
xpclippaste( void )
{
#if	defined( _UNICODE )
	UINT	fmt[]     = { CF_UNICODETEXT, CF_OEMTEXT, CF_TEXT, CF_DSPTEXT };
#else
	UINT	fmt[]     = { CF_OEMTEXT, CF_TEXT, CF_UNICODETEXT, CF_DSPTEXT };
#endif
	HANDLE	hClipData = (HANDLE)0;	/* handles to clip data		*/
	LPTSTR	lpClipData= (LPTSTR)0;	/* Clip Data 			*/
	LPTSTR	s;			/* string forn clipboard	*/
	int	type  = -1;		/* Data type			*/
	int	i;

	if( OpenClipboard( xpwnd ) == FALSE ) {
		WDGerror( _T("Can't find clipboard data") );
		return;
	}

	/*
	 *	get text from the clipboard
	 */

	for( i = 0 ; i < (sizeof( fmt ) / sizeof( fmt[ 0 ] )) ; ++i )
		if( (hClipData = GetClipboardData( fmt[ i ] )) != 0 ) {
			type = fmt[ i ];
			break;
		}

	if( hClipData == (HANDLE)0 ) {
		WDGerror( _T("Can't find clipboard data") );
		CloseClipboard();
		return;
	}

	if( (lpClipData = (LPTSTR)GlobalLock( hClipData ) ) == (LPTSTR)0 ) {
		WDGerror( _T("No memory left, file may be corrupted.") );
		CloseClipboard();
		return;
	}

	if( type == -1 ) {
		WDGerror( _T("Can't find a valid clipboard data.") );
		CloseClipboard();
		return;
	}

#if	0
	if( type == CF_OEMTEXT )
		OemToAnsi( (LPSTR)lpClipData, (LPSTR)lpClipData );
#endif

	kused = 0;

	for( s = lpClipData ; *s ; s++ )
		if( *s != '\r' )
			(void)kinsert( (int)*s );

	GlobalUnlock( hClipData );
	CloseClipboard();
}

CMD
NTansitooem( EDLINE *lp )
{
#if	!defined( _UNICODE )
	CharToOemBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#else
	(void)lp;
#endif
	return( T );
}

CMD
NToemtoansi( EDLINE *lp )
{
#if	!defined( _UNICODE )
	OemToCharBuff( ltext( lp ), ltext( lp ), llength( lp ) );
#else
	(void)lp;
#endif
	return( T );
}

/*
 *	Calling external command
 */

int
system( const TCHAR *s )
{
	HCURSOR		hCurOld;
	TCHAR		cmd[ 512 ];
	TCHAR		*slwr;
	int		internal;
	int		i;
	int		res;
	static	TCHAR	*szDosCmd[] = {
		_T("cd"),     _T("cls"),    _T("copy"),   _T("date"),
		_T("del"),    _T("erase"),  _T("dir"),    _T("echo"),
		_T("for"),    _T("if"),     _T("md"),     _T("mkdir"),
		_T("pause"),  _T("ren"),    _T("rd"),     _T("rmdir"),
		_T("set"),    _T("time"),   _T("type"),   _T("ver"),
		_T("vol"),    _T(">"),      _T("<"),      _T("|"),
		NULL
	};

	internal = FALSE;
	for( slwr = (TCHAR *)s ; *slwr && !isspace( *slwr ) ; slwr++ )
		if( isupper( *slwr ) )
			*slwr = (TCHAR)tolower( *slwr );

	slwr = (TCHAR *)s;

	for( i = 0 ; szDosCmd[ i ] ; i++ )
		if( _tcsncmp( s, szDosCmd[i], _tcslen( szDosCmd[i] ) ) == 0 ) {
			internal = TRUE;
			break;
		}

	if( internal ) {
		_tcscpy( cmd, _tgetenv( _T("COMSPEC") ) );
		_tcscat( cmd, _T(" /c ") );
		_tcscat( cmd, slwr );
	} else	_tcscpy( cmd, slwr );

	hCurOld = SetCursor( (HCURSOR)LoadCursor( NULL, IDC_WAIT ) );

	ShowCursor( TRUE );

#if	defined( XP_EMACS_OLD_EXEC )
	res = (WinExec( (TCHAR *)cmd, SW_SHOWNORMAL ) > 31);
#else
	res = xpsystemspawn( (TCHAR *)slwr );
	if( res == TRUE )
		res = 0; /* unix system returns 0 on success */
#endif

	ShowCursor( FALSE );
	SetCursor( hCurOld );

	return( res );
}

static void
xpshowlines( TCHAR *chBuf, int nLen )
{
	TCHAR	*s = chBuf;
	TCHAR	c;

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

static int
xpsystemspawn( const TCHAR *cmd )
{
	SECURITY_ATTRIBUTES saAttr;
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO	    siStartInfo;
	HANDLE		    hChildStdoutRd = (HANDLE)0;
	HANDLE		    hChildStdoutWr = (HANDLE)0;
	HANDLE		    hOutputFile;
	DWORD		    dwRead;
	char		    chBuf[ NLINE + 1 ];
	TCHAR		    buf[ NFILEN + 1 ];
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
		if( !CreatePipe(&hChildStdoutRd,&hChildStdoutWr,&saAttr,0) ) {
			xpprinterror(_T("Stdout pipe creation failed\n"),FALSE);
			return( FALSE );
		}
		hOutputFile = hChildStdoutWr;
	} else	{
		hOutputFile = CreateFile(
					  _T("process.tmp"),
					  GENERIC_WRITE,
					  0,
					  &saAttr,
					  CREATE_ALWAYS,
					  0,
					  NULL
					);

		if( hOutputFile == INVALID_HANDLE_VALUE )
			xpprinterror( _T("Can't open output file\n"), FALSE );
	}

	siStartInfo.cb		= sizeof( STARTUPINFO );
	siStartInfo.lpReserved	= NULL;
	siStartInfo.lpReserved2	= NULL;
	siStartInfo.cbReserved2	= 0;
	siStartInfo.lpDesktop	= NULL;
	siStartInfo.lpTitle	= (TCHAR *)cmd;
	siStartInfo.wShowWindow	= SW_HIDE;
	siStartInfo.dwFlags	= STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
	siStartInfo.hStdOutput	= hOutputFile;
	siStartInfo.hStdError	= hOutputFile;
	siStartInfo.hStdInput	= NULL;

	/*
	 * Create the child process.
	 */

	_tcscpy( buf, _tgetenv( _T("COMSPEC") ) );
	_tcscat( buf, _T(" /c ") );
	_tcscat( buf, cmd );

	bSuccess = CreateProcess(
			NULL,
			(TCHAR *)buf,  /* command line                       */	
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
			xpprinterror( _T("Closing handle failed"), FALSE );

		process_pending = T;
		for( ;; ) {
			dwRead = 0;
			if( xpcheck() != TRUE ) {
				if( !TerminateProcess(piProcInfo.hProcess,0) ) {
					xpprinterror(
					    _T("TerminateProcess fails"),
					     FALSE
					    );
					nRetCode = FALSE;
				}
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
			
#if	defined( _UNICODE )
			if( bSuccess && dwRead > 0 ) {
				int	k;
				TCHAR	tchBuf[ NLINE + 1];
				char *  s = (char *)chBuf;
				for( k = 0; k < (int)dwRead && k < NLINE; ++k )
					tchBuf[k] = (TCHAR)s[k];
				tchBuf[k] = (EMCHAR)'\000';

				xpshowlines( tchBuf, k );
			}
#else
			if( bSuccess && dwRead > 0 )
				xpshowlines( chBuf, dwRead );
#endif
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

/*
 *	Printing
 */

static	HDC
xpgetprinterdc( void )
{
	PRINTDLG      pd;
	HDC           hDC;
	LPDEVMODE     lpDevMode = NULL;
	LPDEVNAMES    lpDevNames;
	TCHAR *       lpszDriverName;
	TCHAR *       lpszDeviceName;
	TCHAR *       lpszPortName;

	memset( &pd, 0, sizeof( pd ) );
	pd.lStructSize    = sizeof( pd );
	pd.hwndOwner      = GetTopWindow( GetDesktopWindow() ); /* NULL */
	pd.hDevMode       = NULL;
	pd.hDevNames      = NULL;
	pd.Flags          = PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
	pd.nCopies        = 1;

	if( PrintDlg( (LPPRINTDLG)&pd ) == FALSE )
		/*
		 * cancelled by by user
		 */
		return( (HDC)((size_t)-1) );

	if( pd.hDC )
		hDC = pd.hDC;
	else	{
		if( !pd.hDevNames )
			return( (HDC)NULL );

		lpDevNames     = (LPDEVNAMES)GlobalLock( pd.hDevNames );
		lpszDriverName = (TCHAR *)lpDevNames + lpDevNames->wDriverOffset;
		lpszDeviceName = (TCHAR *)lpDevNames + lpDevNames->wDeviceOffset;
		lpszPortName   = (TCHAR *)lpDevNames + lpDevNames->wOutputOffset;
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

#define	XPLINENBSIZE	8

static	void
xpprint( void )
{
	static	   HCURSOR	hHourGlass = (HCURSOR)0;

	int	   Status;	/* printing status			*/
	int	   nPageLength;	/* vert. resolution of printer device	*/
	int	   nPageWidth;	/* horz. resolution of printer device	*/
	HCURSOR	   hSaveCursor;	/* current cursor handle		*/
	HDC	   hPr;		/* handle for printer device context	*/
	TEXTMETRIC TextMetric;	/* information about char size		*/
	HFONT	   hfPr;	/* font used for printing		*/
	HFONT	   hOldFont;	/* old font used			*/
	DOCINFO	   di;		/* Document info			*/
	LOGFONT	   lfPr;	/* LOGFONT structure for the font	*/
	int	   LineSpace;	/* spacing between lines		*/
	int	   LinesPerPage;/* lines per page			*/
	int	   CurrentLine;	/* current line	in page			*/
	int	   LineNumber;	/* current line number			*/
	int	   CurrentPage;	/* current page				*/
	int	   len;		/* length of the current line		*/
	EDLINE	   *clp;	/* current line to print		*/
	time_t	   aclock;	/* current time				*/
	TCHAR	   *szTimeBuf;	/* ascii version of current time	*/
	TCHAR	   *p;
	TCHAR	   buf[ NLINE + XPLINENBSIZE ];

	if( hHourGlass == (HCURSOR)0 )
		hHourGlass = LoadCursor( NULL, IDC_WAIT );

	hSaveCursor = SetCursor( hHourGlass );
	hPr	    = xpgetprinterdc();

	if( hPr == (HDC)((size_t)-1) )
		return;

	if( hPr == (HDC)NULL ) {
	    MessageBox( NULL, _T("Cannot print."), NULL, MB_OK|MB_ICONHAND );
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

	emstrcpy( lfPr.lfFaceName, _T("Courier New") );

	hfPr = CreateFontIndirect( &lfPr );

	hOldFont = (HFONT)SelectObject( hPr, hfPr );

	SetAbortProc( hPr, (ABORTPROC)NULL );

	memset( &di, 0, sizeof( di ) );
	di.cbSize       = sizeof( di );
	di.lpszDocName  = curbp->b_fname;
	di.lpszOutput   = NULL;
	di.lpszDatatype = NULL;
	di.fwType       = 0;

	if( StartDoc( hPr, &di ) < 0 ) {
		MessageBox(
			    NULL,
			    _T("Unable to start print job"),
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
	szTimeBuf = _tasctime( localtime( &aclock ) );
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

	nPageWidth	= GetDeviceCaps( hPr, HORZRES );
	nPageLength	= GetDeviceCaps( hPr, VERTRES );

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
			StartPage( hPr );
			(void)emsprintf3(
					  buf,
					  _T("Page #%00d, %s - %s"),
					  CurrentPage,
					  szTimeBuf,
					  curbp->b_fname
				        );
			TabbedTextOut(
					hPr,
					0,
					CurrentLine * LineSpace,
					(TCHAR *)buf,
					(int)_tcslen( buf ),
					0,
					NULL,
					0
				     );
			MoveToEx( hPr, 0, LineSpace * 2, NULL );
			LineTo( hPr, nPageWidth, LineSpace * 2 ); 
		}

		(void)emsprintf1( buf, _T("%05d : "), LineNumber++ );

		if( llength( clp ) >= NLINE )
			len = NLINE - 1;
		else	len = llength( clp );

		(void)_tcsncpy( buf + XPLINENBSIZE, ltext( clp ), len );

		buf[ len + XPLINENBSIZE ] = '\000';

		TabbedTextOut(
				hPr,
				0,
				(CurrentLine + 2) * LineSpace,
				(TCHAR *)buf,
				len + XPLINENBSIZE,
				0,
				NULL,
				0
			     );

		if( (++CurrentLine % LinesPerPage) == 0 ) {
			CurrentLine = 1;
			CurrentPage++;
			Status = EndPage( hPr );
			if( Status < 0 )
				break;
			else	SelectObject( hPr, hfPr );
		}
	}

	if( Status >= 0 ) {
		EndPage( hPr );
		EndDoc( hPr );
	}

	(void)SelectObject( hPr, hOldFont );
	DeleteObject( hfPr );
	DeleteDC( hPr );

}

#if	defined( _UNICODE )
/*
 * UNICODE generator for tests.
 * Every call returns the next Japanese character that represents "fibonacci".
 * Bound to F12. When called with Shift-F12, reset to first character sequence.
 */
static	EMCHAR
xpunicodegen()
{
	static	EMCHAR	fib[] = {
		0x30D5,
		0x30A3,
		0x30DC,
		0x30CA,
		0x30C3,
		0x30C1,
		0x0020,
		0
	};
	static	int pos = 0;

	if( fib[ pos ] == 0 || GetAsyncKeyState(VK_SHIFT) )
		pos = 0;

	return( fib[ pos++ ] );
}
#endif

/*
 * Main entry point
 */

int WINAPI
_tWinMain(HINSTANCE hInstance, HINSTANCE hPInst, LPTSTR lpCmdLine, int nCmdShow)
{
	HANDLE	hMutex;
	TCHAR	*argv[ 64 ];
	int	argc  = 0;
	int	share = 1;

	if( hPInst ) {
		MessageBox(
			    GetFocus(),
			    _T("Application is alredy loaded."),
			    _T(""),
			    MB_ICONHAND /* | MB_SYSTEMMODAL */
			  );
		return( 0 );
	}

	xpinst	  = hInstance;
	xpacctable= LoadAccelerators( xpinst, _T("EmACTAccelerator") );
	xpprev	  = hPInst;
	xpcmdshow = (int)nCmdShow;
	xploadmsg = RegisterWindowMessage( XP_EMACS_LOAD_MESSAGE );
	xpfindmsg = RegisterWindowMessage( FINDMSGSTRING );

#if	defined( _UNICODE )
	argv[ argc++ ] = XP_EMACS_APPNAME;
	if( lpCmdLine && *lpCmdLine ) {
		int	n;
		int	i;
		LPWSTR  *s = CommandLineToArgvW( (LPWSTR)lpCmdLine, &n );

		for( i = 0 ; i < n ; ++i ) {
			argv[ argc ] = (TCHAR *)s[ i ];
			if( _tcscmp( argv[ argc ], _T("-noshare") ) == 0 ||
			    _tcscmp( argv[ argc ], _T("-ns")      ) == 0 ) {
				share = 0;
				continue;
			}
			++argc;
		}
	}
	argv[ argc ] = (TCHAR *)0;
#else
	argv[ argc++ ] = XP_EMACS_APPNAME;
	while( *lpCmdLine ) {
		TCHAR	endstr;

		while( *lpCmdLine && *lpCmdLine == ' ' )
			lpCmdLine++;

		if( *lpCmdLine == '\000' )
			break;

		if( *lpCmdLine == '"' ) {
			++lpCmdLine;
			endstr = '"';
		} else	endstr = ' ';

		argv[ argc ] = (TCHAR *)lpCmdLine;

		while( *lpCmdLine && *lpCmdLine != endstr )
			lpCmdLine++;

		if( *lpCmdLine == endstr ) {
			if( *lpCmdLine == '"' )
				*lpCmdLine = '\000';
			*lpCmdLine++ = '\000';
		}

		if( _tcscmp( argv[ argc ], _T("-noshare") ) == 0 ||
		    _tcscmp( argv[ argc ], _T("-ns")      ) == 0 ) {
			share = 0;
			argv[ argc ] = (TCHAR *)NULL;
		} else	++argc;
	}

	argv[ argc ] = (TCHAR *)0;
#endif

	/*
	 * Create a Mutex that check for multiple instances.
	 */

	hMutex = CreateMutex( NULL, FALSE, XP_EMACS_CLASS );

	if( GetLastError() == ERROR_ALREADY_EXISTS ) {
		/*
		 * Another EmACT is running. Find its Window.
		 */
		xpwnd = FindWindow( XP_EMACS_CLASS, NULL );
	}

	if( xpwnd != NULL && share != 0 ) {
		DWORD_PTR	res;
		int		i;

		for( i = 1 ; i < argc ; ++i ) {
			_tfullpath( xpshared, argv[i], XP_EMACS_MAXPATH );

			SendMessageTimeout(
					    xpwnd,
					    xploadmsg,
					    IDM_LOAD,
					    (LPARAM)xpshared,
					    SMTO_BLOCK | SMTO_ABORTIFHUNG,
					    200, /* in ms */
					    &res
					  );
		}

		/*
		 * Connect to the running EmACT and exit.
		 */
		
		if( IsIconic( xpwnd ) )
			ShowWindow( xpwnd, SW_RESTORE );

		SetForegroundWindow( xpwnd );

		if( hMutex != 0 )
			ReleaseMutex( hMutex );

		return( 0 );
	}

	/*
	 * Enter emacs Wonderland
	 */

	emacs( argc, argv );

	if( hMutex != 0 )
		ReleaseMutex( hMutex );

	return( 0 );
}
