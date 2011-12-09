#if	defined( _PM20 ) && !defined( lint )
static	char rcsid[] = "$Id: pm20.c,v 1.3 2008/04/10 05:29:25 jullien Exp $";
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

#if	defined( _PM20 )

/*
 *	pm20.c :	Specific functions for Presentation Manager.
 */

#define INCL_PM
#define	INCL_DOSPROCESS
#define	INCL_DOSFILEMGR
#define	INCL_DOSMISC
#define	INCL_ERRORS

/*
 *	For common DLG
 */

#define INCL_WINFRAMEMGR
#define INCL_WINSWITCHLIST
#define INCL_WINMLE
#define INCL_WINSTDFILE

#include	<io.h>
#include	"emacs.h"
#include	"pm20.h"

#if	!defined( FDS_OPEN_DIALOG )
#define	FDS_OPEN_DIALOG		0x00000001L
#define	FDS_SAVEAS_DIALOG	0x00000002L
#else
#define	_PMWIDGETS
#endif

/*
 *	Forward declarations.
 */

static	void	PMopen( void );
static	void	PMclose( void );
static	int	PMgetc( void );
static	void	PMputc( int c );
static	void	PMputs( EMCHAR *s, int n );
static	void	PMmove( int row, int col );
static	void	PMeeol( void );
static	void	PMeeop( void );
static	void	PMflush( void );
static	void	PMbeep( void );
static	void	PMei( void );
static	void	PMsi( void );
static	void	PMcshow( int flag );
static	int	PMcheck( void );
static	void	PMrawmode( void );

static	void	PMerror( EMCHAR *s );
static	int	PMyesno( EMCHAR *s );
static	EMCHAR *PMtitle( EMCHAR *s, EMCHAR *p );
static	ULONG	PMfile( CHAR *szTemplate, ULONG lStyle );

TERM	term = {
	0,
	0,
	0,
	NIL,
	PMopen,
	PMclose,
	PMgetc,
	PMputc,
	PMputs,
	PMflush,
	PMmove,
	PMeeol,
	PMeeop,
	PMbeep,
	PMsi,
	PMei,
	PMcshow,
	PMcheck,
	PMrawmode
};

/*
 *	Private variables.
 */

#define	KBDBUFLEN	255
#define	CTRLCHAR( x )	((USHORT)((x) & 0x1F))

#define	EMEXITMSG	"Do you really want to exit ?"

static	HAB 	hab;
static	QMSG 	qmsg;

static  HMQ 	hmq;
static  HWND 	hwndTTY;
static  HWND 	hwndFrame;
static	HPS	hpsTTY;
static	USHORT 	szKbdBuff[ KBDBUFLEN + 1 ];
static	CHAR	szFullFile[ CCHMAXPATH ] = "*.C";
static	LONG	nInitPM = 0;
static	LONG	lCharWidth;
static	LONG	lCharHeight;
static	LONG	lCharDescend;
static	LONG	lxClient;
static	LONG	lyClient;
static	LONG	lxCursor;
static	LONG	lyCursor;
static	LONG	ltyXMax;
static	LONG	ltyYMax;
static	LONG	lBackColor;
static	LONG	lForeColor;
static	int	nShowCursorFlag = 0;
static	int 	nFirstChar	= -1;
static	int 	nNextChar	= 0;

static	int	colortable[] = {
	CLR_BLACK,
	CLR_BLUE,
	CLR_GREEN,
	CLR_CYAN,
	CLR_RED,
	CLR_PINK,
	CLR_YELLOW,
	CLR_WHITE
};

static	void	PMpaint( HWND hwnd );
static	void	PMaddchar( USHORT aChar );
static	void	PMcursor( int col, int row );
static	void	PMupdatecursor( void );
static	void	PMconvrect(HWND hwnd,SHORT x,SHORT y,SHORT w,SHORT h, RECTL *r);

MRESULT EXPENTRY MainWndProc( HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2 );

/*
 *	Font Creation
 */

static	BOOL PMQueryFonts( HPS hps );
static	LONG PMCreateLogFont(HPS hps,LONG lc,USHORT Face,USHORT Siz,USHORT Sel);

#define FACE_SYSTEM	0
#define FACE_COUR	1
#define FACE_HELV	2
#define FACE_TIMES	3

#define SIZE_06		0
#define SIZE_08		1
#define SIZE_10		2
#define SIZE_12		3
#define SIZE_14		4
#define SIZE_18		5

#define	MAX_FONT_NAME	4
#define	MAX_FONT_SIZE	6

static	LONG  alMatch[MAX_FONT_NAME][MAX_FONT_NAME];

static	SHORT sFontSize[MAX_FONT_SIZE] = {
	60,
	80,
	100,
	120,
	140,
	180
};

static	CHAR  *szFacename[ MAX_FONT_NAME ] = {
	"System Proportional",
	"Courier",
	"Helv",
	"Tms Rmn"
};

/*
 *	Main Proc for the TTY Window.
 */

#define	ControlKey()	(WinGetKeyState( HWND_DESKTOP, VK_CTRL ) < 0)


MRESULT EXPENTRY
MainWndProc( HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2 )
{
	USHORT	fs;
	UCHAR	aChar;
	RECTL	rcl;
	ULONG	lReturn;

	switch( msg ) {

	case WM_COMMAND:
		switch( SHORT1FROMMP( mp1 ) ) {
		case IDM_FILENEW :
		case IDM_FILEOPEN :
			if( PMfile( "*.*", FDS_OPEN_DIALOG ) == ID_OK ) {
				(void)newfile( szFullFile );
				update( T );
			}
			break;
		case IDM_FILESAVE :
			(void)filesave();
			break;
		case IDM_FILESAVEAS :
			if( PMfile(curbp->b_fname, FDS_SAVEAS_DIALOG) == ID_OK )
				;
			break;
		case IDM_FILEEXIT :
			lReturn = WinMessageBox(
					  HWND_DESKTOP,
					  hwndFrame,
					  EMEXITMSG,
					  version,
					  0,
					  MB_YESNO|MB_DEFBUTTON2|MB_ICONQUESTION
					 );
			
			if( lReturn == MBID_YES ) {
				PMclose();
				exit( 0 );
			}

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
		}
		break;

	case WM_INITMENU:
		break;

	case WM_CREATE:
		break;

	case WM_SIZE:
		if( nInitPM && initflag ) {

			/*
			 *	Resize the Terminal Window
			 */

			WinQueryWindowRect( hwndTTY, &rcl );
			WinFillRect( hpsTTY, &rcl, lBackColor );
			lxClient = rcl.xRight - rcl.xLeft;
			lyClient = rcl.yTop   - rcl.yBottom;
			ltyXMax	 = (lxClient / lCharWidth);
			ltyYMax	 = (lyClient / lCharHeight);

			if( ltyYMax <= 1 )
				break;

			vtfree();

			TTYnrow = ltyYMax - 1;
			TTYncol = ltyXMax;

			if( TTYnrow <= 1 )
				TTYnrow = 2;

			vtinit();
			resize();
		}
		break;

	case WM_SETFOCUS:
		break;
			
	case WM_CLOSE:
		WinPostMsg( hwnd, WM_QUIT, 0L, 0L );
		break;

	case WM_PAINT:
		PMpaint( hwnd );
		break;

	case WM_CHAR:
		fs	= SHORT1FROMMP( mp1 );
		aChar	= CHAR1FROMMP( mp2 );

		if( (fs & KC_KEYUP) )
			return( (MRESULT)0 );

		if( fs & KC_CHAR)
			PMaddchar( aChar );
		else	if( fs & KC_VIRTUALKEY ) {
				switch( SHORT2FROMMP( mp2 ) ) {
				case VK_SHIFT   :
				case VK_CTRL    :
				case VK_ALT     :
				case VK_ALTGRAF :
					return( (MRESULT)0 );
				case VK_SPACE   :
					if( ControlKey() )
						setmark();
					else	PMaddchar( ' ' );
					return( (MRESULT)1 );
				case VK_F2      :
					PMaddchar( CTRLCHAR( 'S' ) );
					return( (MRESULT)1 );
				case VK_F3      :
					PMaddchar( 0x1B );
					PMaddchar( CTRLCHAR( 'F' ) );
					return( (MRESULT)1 );
				case VK_F4      :
					PMaddchar( CTRLCHAR( 'X' ) );
					PMaddchar( CTRLCHAR( 'I' ) );
					return( (MRESULT)1 );
				case VK_F5      :
					(void)switchscreen();
					return( (MRESULT)1 );
				case VK_F7      :
					PMaddchar( CTRLCHAR( 'X' ) );
					PMaddchar( '`' );
					return( (MRESULT)1 );
				case VK_F8      :
					PMaddchar( CTRLCHAR( 'X' ) );
					PMaddchar( CTRLCHAR( 'S' ) );
					return( (MRESULT)1 );
				case VK_F9      :
					PMaddchar( CTRLCHAR( 'X' ) );
					PMaddchar( 'E' );
					return( (MRESULT)1 );
				case VK_HOME    :
					if( ControlKey() ) {
						PMaddchar( CTRLCHAR( 'X' ) );
						PMaddchar( CTRLCHAR( 'P' ) );
					} else	{
						PMaddchar( 0x1B );
						PMaddchar( '<'  );
					}
					return( (MRESULT)1 );
				case VK_END     :
					if( ControlKey() ) {
						PMaddchar( CTRLCHAR( 'X' ) );
						PMaddchar( CTRLCHAR( 'N' ) );
					} else	{
						PMaddchar( 0x1B );
						PMaddchar( '>'  );
					}
					return( (MRESULT)1 );
				case VK_UP      :
					PMaddchar( CTRLCHAR( 'P' ) );
					return( (MRESULT)1 );
				case VK_PAGEUP  :
					PMaddchar( 0x1B );
					PMaddchar( 'V'  );
					return( (MRESULT)1 );
				case VK_PAGEDOWN:
					PMaddchar( CTRLCHAR( 'V' ) );
					return( (MRESULT)1 );
				case VK_LEFT    :
					if( ControlKey() ) {
						PMaddchar( 0x1B );
						PMaddchar( 'B'  );
					} else	PMaddchar( CTRLCHAR( 'B' ) );
					return( (MRESULT)1 );
				case VK_RIGHT   :
					if( ControlKey() ) {
						PMaddchar( 0x1B );
						PMaddchar( 'F'  );
					} else	PMaddchar( CTRLCHAR( 'F' ) );
					return( (MRESULT)1 );
				case VK_DOWN    :
					PMaddchar( CTRLCHAR( 'N' ) );
					return( (MRESULT)1 );
				case VK_INSERT :
					PMaddchar( 0x1B );
					PMaddchar( 'I' );
					return( (MRESULT)1 );
				case VK_DELETE :
					PMaddchar( CTRLCHAR( 'D' ) );
					return( (MRESULT)1 );
				default :
					PMaddchar( aChar );
				}
		} else	if( fs & KC_CTRL )
				PMaddchar( CTRLCHAR( (unsigned long)aChar ) );
		else	return( (MRESULT)0 );

		return( (MRESULT)1 );

	case WM_BUTTON1DOWN : {
		SHORT x = SHORT1FROMMP( mp1 );
		SHORT y = SHORT2FROMMP( mp1 );

		mevent.x = x / lCharWidth;
		mevent.y = ltyYMax - ((y + lCharDescend) / lCharHeight) - 1;
		mevent.button = MButton1;
		PMaddchar( MEVT );
		return( WinDefWindowProc( hwnd, msg, mp1, mp2 ) );
	}

	default:
		return( WinDefWindowProc( hwnd, msg, mp1, mp2 ) );

	}

	return( (MRESULT)0 );
}

/*
 *	Repaint the client Window after the screen has been garbaged.
 */

static	void
PMpaint( HWND hwnd )
{
	HPS	hps;
	RECTL	rcl;

	hps = WinBeginPaint( hwnd, (HPS)NULL, (PRECTL)&rcl );

	if( nInitPM ) {
		int	nOldCursor = nShowCursorFlag;
		/*
		 *	clear invalidated region
		 */

		if( nOldCursor )
			WinShowCursor( hwndTTY, NIL );

		WinFillRect( hpsTTY, &rcl, lBackColor );

		sgarbf = EXPOSE;
		update( T );

		PMupdatecursor();
		PMcursor( (int)lxCursor, (int)lyCursor );

		if( nOldCursor )
			WinShowCursor( hwndTTY, T );

	}

	/*
	 *	clear invalidated region
	 */

	WinEndPaint( hps );
}

static	void
PMaddchar( USHORT aChar )
{
	szKbdBuff[ nNextChar ] = aChar;

	if( nFirstChar < 0 )
		nFirstChar = nNextChar;

	if( nNextChar >= KBDBUFLEN )
		nNextChar = 0;
	else 	nNextChar += 1;
}

BOOL
PMQueryFonts( HPS hps )
{
	FONTMETRICS	*pfm;
	HDC		hdc;
	LONG		lHorzRes;
	LONG		lVertRes;
	LONG		lRequestFonts;
	LONG		lNbFonts;
	SHORT		sIndex;
	SHORT		sFace;
	SHORT		sSize;

	hdc = GpiQueryDevice( hps );
	DevQueryCaps( hdc, CAPS_HORIZONTAL_FONT_RES, 1L, &lHorzRes );
	DevQueryCaps( hdc, CAPS_VERTICAL_FONT_RES,   1L, &lVertRes );

	for( sFace = 0 ; sFace < MAX_FONT_NAME ; sFace++ ) {
		lRequestFonts = 0;
		lNbFonts = GpiQueryFonts(
					  hps,
					  QF_PUBLIC,
					  szFacename[ sFace ],
					  &lRequestFonts,
					  0L,
					  NULL
					);
		if( lNbFonts == 0 )
			continue;

		if( lNbFonts * sizeof( FONTMETRICS) >= 65536L )
			return( NIL );

		pfm=(FONTMETRICS *)malloc((SHORT)lNbFonts*sizeof(FONTMETRICS));

		if( pfm == NULL )
			return( NIL );

		GpiQueryFonts(
			       hps,
			       QF_PUBLIC,
			       szFacename[sFace],
			       &lNbFonts,
			       (LONG)sizeof( FONTMETRICS ),
			       pfm
			     );

		for( sIndex = 0 ; sIndex < (SHORT)lNbFonts ; sIndex++ )
			if( pfm[sIndex].sXDeviceRes == (SHORT)lHorzRes &&
			    pfm[sIndex].sYDeviceRes == (SHORT)lVertRes &&
			    (pfm[sIndex].fsDefn & 1) == 0 ) {
				for( sSize=0 ; sSize < MAX_FONT_SIZE ; sSize++ )
					if( pfm[sIndex].sNominalPointSize
					    == sFontSize[sSize] )
					    	break;

				if( sSize != MAX_FONT_SIZE )
				    alMatch[sFace][sSize] = pfm[sIndex].lMatch;
			    }

		free( pfm );
	}

	return( T );
}

static	LONG
PMCreateLogFont( HPS hps, LONG lc, USHORT Face, USHORT Size, USHORT fsSel )
{
	static FATTRS fat;

	if( Face > 3 || Size > 5 || alMatch[ Face ][ Size ] == 0 )
		return( NIL );

	fat.usRecordLength = sizeof( fat );
	fat.fsSelection    = fsSel;
	fat.lMatch	   = alMatch[ Face ][ Size ];
	fat.fsSelection	   = FATTR_SEL_BOLD;

	(void)strcpy( fat.szFacename, szFacename[ Face ] );

	return( GpiCreateLogFont( hps, NULL, lc, &fat ) );

}

/*
 *	Virtual terminal emulator.
 */

static	void
PMopen( void )
{
	ULONG		ulCreate;
	HDC		hdc;
	SIZEL		sizl = { 0, 0 };
	RECTL		rcl;
	FONTMETRICS	fm;

	/*
	 *	init application
	 */

	if( nInitPM )
		/*
		 *	Do it only once.
		 */
		return;

	hab = WinInitialize( 0 );
	if( !hab )
		exit( -1 );

	hmq = WinCreateMsgQueue( hab, 64 );

	if( !hmq ) {
		long	x = WinGetLastError( hab );
		char	buf[ 100 ];

		(void)emsprintf1( buf, ECSTR("%lX\n"), x );
		PMerror( buf );
		exit( -1 );
	}


 	/*
	 *	create main window
	 */

	WinRegisterClass( hab, version, MainWndProc, 0L, 0 );

	ulCreate = FCF_TITLEBAR		| FCF_SYSMENU		| FCF_MINMAX  |
		   FCF_SIZEBORDER	| FCF_SHELLPOSITION	| FCF_ICON    |
		   FCF_MENU;

	hwndFrame = WinCreateStdWindow(
					HWND_DESKTOP,
					WS_VISIBLE,
					&ulCreate,
					version,
					version,
					0L,
					(HMODULE)0,
					IDR_MAIN,	/* ressource */
					&hwndTTY
				      );

	if( !hwndFrame ) {
		long	x = WinGetLastError( hab );
		char	buf[ 100 ];

		(void)emsprintf1( buf, ECSTR("%lX\n"), x );
		PMerror( buf );
		exit( -1 );
	}

	hdc    = WinOpenWindowDC( hwndTTY );
	hpsTTY = GpiCreatePS( hab, hdc, &sizl, PU_PELS|GPIT_NORMAL|GPIA_ASSOC );

	PMQueryFonts( hpsTTY );

	if( !PMCreateLogFont( hpsTTY, 1L, FACE_COUR, SIZE_10, 0 ) )
		return;

	GpiSetAttrMode( hpsTTY, AM_PRESERVE );
	GpiSetCharSet( hpsTTY, 1L );

	GpiSetMix( hpsTTY, FM_OVERPAINT );
	GpiSetBackMix( hpsTTY, FM_OVERPAINT );
	GpiQueryFontMetrics( hpsTTY, sizeof( fm ), &fm );
	lCharWidth   = fm.lAveCharWidth;
	lCharHeight  = fm.lMaxBaselineExt;
	lCharDescend = fm.lMaxDescender;

	PMconvrect( hwndFrame, 80, 80, 80 * lCharWidth, 25 * lCharHeight, &rcl );

	WinSetWindowPos(
			 hwndFrame,
			 HWND_TOP,
			 80,
			 80,
			 512,
			 300,
			 SWP_MAXIMIZE|SWP_MOVE|SWP_SIZE|SWP_ACTIVATE|SWP_SHOW
			);

/*
	WinSetWindowPos(
			 hwndFrame,
			 HWND_TOP,
			 rcl.xRight,
			 rcl.yTop,
			 rcl.xRight - rcl.xLeft,
			 rcl.yTop   - rcl.yBottom,
			 SWP_MOVE|SWP_SIZE|SWP_ACTIVATE|SWP_SHOW
			);
*/

	WinQueryWindowRect( hwndTTY, &rcl );
	lxClient = rcl.xRight - rcl.xLeft;
	lyClient = rcl.yTop   - rcl.yBottom;
	ltyXMax	 = (lxClient / lCharWidth);
	ltyYMax	 = (lyClient / lCharHeight);

	TTYnrow = ltyYMax - 1;
	TTYncol = ltyXMax;
	TTYinit = T;

	PMcursor( 0, 0 );
	PMei();
	PMeeop();

	if( eargc == 1 )
		PMerror( "Copyright (c) 1986-96, C. Jullien" );

	PMupdatecursor();

/*
	widget.w_error = PMerror;
	widget.w_yesno = PMyesno;
*/
	widget.w_title = PMtitle;

	nInitPM   = T;
	mouseflag = 1;

}

static	void
PMclose( void )
{
	/*
	 *	clean up
	 */

	if( nInitPM )
		nInitPM = NIL;
	else	return;

	WinDestroyWindow( hwndTTY );
	WinDestroyMsgQueue( hmq );
	WinTerminate( hab );
	TTYinit = NIL;

}

static	int
PMgetc( void )
{
	USHORT	c;
	int	n;
	
	while( nFirstChar < 0 ) {
		PMcshow( T );
		if( WinGetMsg( hab, &qmsg, (HWND)NULL, 0, 0 ) ) {
			PMcshow( NIL );
			WinDispatchMsg( hab, &qmsg );
		} else	{
			PMcshow( NIL );
			PMyesno( EMEXITMSG );

			if( n == MBID_YES ) {
				PMclose();
				exit( 0 );
			}

			PMupdatecursor();
		}

	}

	c = szKbdBuff[ nFirstChar ];

	if( nFirstChar >= KBDBUFLEN )
		nFirstChar = 0;
	else	nFirstChar += 1;

	if( nFirstChar == nNextChar ) {
		nFirstChar = -1;
		nNextChar = 0;
	}

	return( c );
}

static	void
PMputc( int c )
{

	CHAR	aChar = (char)c;

	switch( aChar ) {

	case '\n':
		if( lyCursor >= ltyYMax ) {
			lxCursor = 0;
			lyCursor = 0;
		} else	++lyCursor;
	    	break;

	case '\r':
		lxCursor = 0;
		break;

	case '\b':
		if( lxCursor > 1 )
			--lxCursor;
		else 	PMbeep();
		break;

	default:
		GpiCharString( hpsTTY, 1, (char *)&aChar );
		lxCursor++;
		break;	
	}

}

static	void
PMputs( EMCHAR *szBuf, int nLength )
{
	GpiCharString( hpsTTY, nLength, (char *)szBuf );
	lxCursor += nLength;
}

static	void
PMeeop( void )
{
	RECTL	rcl;

	WinQueryWindowRect( hwndTTY, &rcl );
	WinFillRect( hpsTTY, &rcl, lBackColor );
}

static	void
PMflush( void )
{
	WinUpdateWindow( hwndTTY );
}

static	void
PMeeol( void )
{
	RECTL	rcl;
	LONG	y = (ltyYMax - lyCursor - 1) * lCharHeight + lCharDescend;

	WinQueryWindowRect( hwndTTY, &rcl ); /* just to fill xRight */
	rcl.xLeft   = lxCursor * lCharWidth;
	rcl.yBottom = y - lCharDescend;
	rcl.xRight  = rcl.xRight;
	rcl.yTop    = y + lCharHeight - lCharDescend;
	WinFillRect( hpsTTY, &rcl, lBackColor );

}

static	void
PMmove( int x, int y )
{
	PMcursor( y, x );
}

static	void
PMupdatecursor()
{
	WinCreateCursor(
			 hwndTTY,
			 (SHORT)0,
			 (SHORT)0,
			 (SHORT)lCharWidth,
			 (SHORT)6,
			 (USHORT)(CURSOR_SOLID|CURSOR_FLASH),
			 NULL
		       );
}


static	void
PMcursor( int x, int y )
{

	POINTL	ptlDraw;

	/*
	 * For text (in char size) :		For PM (in pixel size) :
	 *
	 * (0,0)
	 *	+--> x				^
	 *    y	|			      y	|
	 *	|				|
	 *	v				+--> x
	 *				   (0,0)
	 */

	lxCursor = x;
	lyCursor = y;

	ptlDraw.x = lxCursor * lCharWidth;
	ptlDraw.y = (ltyYMax - lyCursor - 1) * lCharHeight + lCharDescend;

	GpiMove( hpsTTY, &ptlDraw );
}

void
PMcshow( int i )
{
	if( nShowCursorFlag == i )
		return;
	else	nShowCursorFlag = i;

	if( nShowCursorFlag ) {
		POINTL	ptlDraw;

		ptlDraw.x = lxCursor * lCharWidth;
		ptlDraw.y = (ltyYMax-lyCursor-1) * lCharHeight + lCharDescend;

		WinCreateCursor(
				 hwndTTY,
				 (SHORT)ptlDraw.x,
				 (SHORT)(ptlDraw.y - 3),
				 (SHORT)0,
				 (SHORT)0,
				 (USHORT)CURSOR_SETPOS,
				 (PRECTL)0
			       );
		WinShowCursor( hwndTTY, T );
	} else	WinShowCursor( hwndTTY, NIL );
}

static	void
PMsi( void )
{
	GpiSetColor( hpsTTY,     lForeColor = CLR_BLACK );
	GpiSetBackColor( hpsTTY, lBackColor = CLR_WHITE );

}

static	void
PMei( void )
{
	GpiSetColor( hpsTTY,     lForeColor = colortable[ foreground_color ] );
	GpiSetBackColor( hpsTTY, lBackColor = colortable[ background_color ] );
}

static	void
PMbeep( void )
{
	WinAlarm( HWND_DESKTOP, WA_WARNING );
}

CMD
switchscreen( void )
{
	return( NIL );
}

/*
 *	Implementation of Widgets
 */

static	void
PMerror( EMCHAR *s )
{
	WinMessageBox(
			HWND_DESKTOP,
			hwndFrame,
			(char *)s,
			version,
			0,
			MB_OK | MB_ICONASTERISK
		     );

	PMupdatecursor();
}

static	int
PMyesno( EMCHAR *s )
{
	int	res;

	res = WinMessageBox(
				HWND_DESKTOP,
				hwndFrame,
				(char *)s,
				version,
				0,
				MB_YESNO | MB_DEFBUTTON2 | MB_ICONQUESTION
			   );

	PMupdatecursor();

	return( (res == MBID_YES) ? T : NIL );
}

static	EMCHAR *
PMtitle( EMCHAR *s, EMCHAR *p )
{
	static	char	title[ 64 ] = { 0 };

	(void)p;

	if( strcmp( (char *)s, title ) != 0 ) {
		(void)strcpy( title, (char *)s );
		WinSetWindowText( hwndFrame, title );
	}

	return( s );
}

static	void
PMconvrect( HWND hwnd, SHORT x, SHORT y, SHORT w, SHORT h, RECTL *r )
{
	SHORT	wx;
	SHORT	wy;
	SHORT	ww;
	SHORT	wh;
	SHORT	bitxmax;
	SHORT	bitymax;
	ULONG 	style;
	LONG 	xborder;
	LONG	yborder;

	bitxmax = (SHORT)WinQuerySysValue( HWND_DESKTOP, SV_CXSCREEN );
	bitymax = (SHORT)WinQuerySysValue( HWND_DESKTOP, SV_CYSCREEN );
	style	= WinQueryWindowULong( hwnd, QWL_STYLE );
	wy	= bitymax - y - h;

	if( style & FCF_SIZEBORDER ) {
		xborder = WinQuerySysValue( HWND_DESKTOP, SV_CXSIZEBORDER );
		yborder = WinQuerySysValue( HWND_DESKTOP, SV_CYSIZEBORDER );
	} else	{
		xborder = WinQuerySysValue( HWND_DESKTOP, SV_CXBORDER );
		yborder = WinQuerySysValue( HWND_DESKTOP, SV_CYBORDER );
	}
		wx = x  - (SHORT)xborder;
		wy = wy - (SHORT)yborder;
		ww = w  + (SHORT)(2 * xborder);
		wh = h  + (SHORT)(2 * yborder);

	if( style & FCF_MINMAX )
		wh += (SHORT)WinQuerySysValue( HWND_DESKTOP, SV_CYTITLEBAR );

	WinSetRect( hab, r, wx, wy, ww, wh );
}

#define	MESSAGELEN	128

static	ULONG
PMfile( CHAR *szTemplate, ULONG lStyle )
{
#if	defined( _PMWIDGETS )

	FILEDLG	fdg;
	static	CHAR	szTitle[ MESSAGELEN ]  = "File Selection";
	static	CHAR	szButton[ MESSAGELEN ] = "~OK";
	static	CHAR	*szITypeList[]  = {
		"*.ASM",
		"*.C",
		"*.CPP",
		"*.CC",
		"*.H",
		"*.HPP",
		"*.LL",
		"*.LSP",
		"*.JAVA",
		NULL
	};

	(void)strcpy( fdg.szFullFile, szTemplate );

	fdg.cbSize		= sizeof( FILEDLG );
	fdg.pszTitle		= szTitle;
	fdg.pszOKButton		= szButton;
	fdg.ulUser		= 0L;
	fdg.fl			= FDS_CENTER | FDS_PRELOAD_VOLINFO | lStyle;
	fdg.pfnDlgProc		= NULL;
	fdg.lReturn		= 0L;
	fdg.lSRC		= 0L;
	fdg.hMod		= (HMODULE)0;
	fdg.usDlgId		= 0;	/* IDD_FILEOPEN; */
	fdg.x			= 0;
	fdg.y			= 0;
	fdg.pszIType		= 0L;
	fdg.papszITypeList	= (PAPSZ)szITypeList;
	fdg.pszIDrive		= "C:";
	fdg.papszIDriveList	= 0L;
	fdg.sEAType		= 0;
	fdg.papszFQFilename	= 0;
	fdg.ulFQFCount		= 0;

	/*
	 *	get the file
	 */

	if( !WinFileDlg( HWND_DESKTOP, hwndTTY, (PFILEDLG)&fdg ) )
		return( 0L );

	/*
	 *  Upon sucessful return of a file, open it for reading
	 */

	if( fdg.lReturn == ID_OK )
		(void)strcpy( szFullFile, fdg.szFullFile );

	return( fdg.lReturn );
#else
	szTemplate = szTemplate;
	lStyle	   = lStyle;
	return( 0L );
#endif
}

static int
PMcheck( void )
{
	return( 0 );
}

static void
PMrawmode( void )
{
}

#endif	/* _PM20 */
