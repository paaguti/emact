/*
 * static char rcsid[] = "$Id: pocket.h,v 1.2 2006/05/02 10:59:52 jullien Exp $";
 */

#include "newres.h"

#define IDM_FILE	100
#define IDM_LOAD	101
#define IDM_BREAK	102
#define	IDM_PRINT	103
#define IDM_QUIT	104
#define IDM_ESCAPE	201
#define IDM_CUT		202
#define IDM_COPY	203
#define IDM_PASTE	204
#define IDM_OPTIONS	300
#define IDM_FONT	301
#define	IDM_FONT11	302
#define	IDM_FONT13	303
#define IDM_BACKCOLOR	304
#define IDM_TEXTCOLOR	305
#define IDM_HELP	500
#define IDM_HELPEMACS	501
#define IDM_ABOUT	502
#define	IDM_MENU	600

#define	EMACTICON	1
#define	EMACTMENU	1

#if	!defined( _WIN32_WCE )
int	PASCAL	 WinMain( HINSTANCE, HINSTANCE, LPSTR, int );
#endif

LRESULT APIENTRY MainWndProc( HWND, UINT, WPARAM, LPARAM );
LRESULT APIENTRY About( HWND, UINT, WPARAM, LPARAM );
