/*
 * static char rcsid[] = "$Id: winterm.h,v 1.2 2006/05/02 10:59:52 jullien Exp $";
 */

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

#ifndef	__WINTERM_H
#define	__WINTERM_H

#if	defined( __cplusplus )
extern	"C" {
#endif

#if	!defined( _WINDOWS_SOURCE )

#if	!defined( UINT )
#define	UINT		unsigned int
#endif

#if	!defined( APIENTRY )
#define	APIENTRY	FAR PASCAL __export
#endif

#endif

#define IDM_OPEN		100
#define IDM_SAVE		101
#define IDM_SAVEAS		102
#define IDM_QUIT		103
#define	IDM_PRINT		104
#define	IDM_PRINTSETUP		105
#define IDM_ABOUT		106
#define IDM_FILE		107

#define	IDM_CUT			200
#define IDM_COPY		201
#define IDM_PASTE		202
#define	IDM_SEARCH		203
#define	IDM_RSEARCH		204
#define	IDM_GREPLACE		205
#define	IDM_QREPLACE		206

#define	IDM_BUFFER		300

#define	IDM_CFONTS		400
#define	IDM_CCOLORS		401
#define	IDM_ANSI		411
#define	IDM_OEM			412
#define	IDM_NORMAL		422
#define	IDM_C			423
#define	IDM_CPP			424
#define	IDM_LISP		425
#define	IDM_ASSEMBLER		426
#define	IDM_PROLOG		427
#define	IDM_PARAMS		428
#define	IDM_BUTTONBAR		429
#define	IDM_JAVA		430

#define	IDM_HELP_CMD		500
#define	IDM_HELP		501
#define	IDM_HELP_FILE1		502
#define	IDM_HELP_FILE2		503

#define	IDM_TOOL		600
#define	IDM_TOOL_OPEN		600
#define	IDM_TOOL_SAVE		601
#define	IDM_TOOL_FIND		602
#define	IDM_TOOL_REPLACE	603
#define	IDM_TOOL_RECORD		604
#define	IDM_TOOL_PLAY		605
#define	IDM_TOOL_MAKE		606
#define	IDM_TOOL_QUIT		607

#define	ID_DATE			700
#define	ID_LICENCE		701

#define	EMACSICON		1
#define	ALTERNATEICON		2

/*
 *	Widgets definition
 */

#define	IDC_GETFILE			400

#define IDC_FILENAME			(IDC_GETFILE + 0)
#define IDC_EDIT			(IDC_GETFILE + 1)
#define IDC_FILES			(IDC_GETFILE + 2)
#define IDC_PATH			(IDC_GETFILE + 3)
#define IDC_LISTBOX			(IDC_GETFILE + 4)

#define	ID_GETTEXT			500

#define	IDG_OK				(ID_GETTEXT + 0)
#define	IDG_CANCEL			(ID_GETTEXT + 1)
#define	IDG_EDIT			(ID_GETTEXT + 2)
#define	IDG_STATIC			(ID_GETTEXT + 3)

#define	ID_REPLACE			600

#define	IDR_OK				(ID_REPLACE + 0)
#define	IDR_CANCEL			(ID_REPLACE + 1)
#define	IDR_OLD				(ID_REPLACE + 2)
#define	IDR_NEW				(ID_REPLACE + 3)
#define	IDR_OPAT			(ID_REPLACE + 4)
#define	IDR_NPAT			(ID_REPLACE + 5)
#define	IDR_STATIC			(ID_REPLACE + 6)

#define	ID_WARNING			700
#define	ID_QUIT				701

#define	ID_PARAMETERS			900

#define ID_SET_SHOW_GRAPHIC		(ID_PARAMETERS + 1)
#define ID_CASE_FOLD_SEARCH		(ID_PARAMETERS + 2)
#define ID_PARAM_SAVE			(ID_PARAMETERS + 3)
#define ID_TABULATION_DISPLAY		(ID_PARAMETERS + 4)
#define ID_TAB_SIZE			(ID_PARAMETERS + 5)
#define ID_BACKUP_BEFORE_WRITING	(ID_PARAMETERS + 6)
#define ID_PARAM_CANCEL			(ID_PARAMETERS + 7)
#define ID_TAB_DISPLAY			(ID_PARAMETERS + 8)
#define ID_MAKE_NAME			(ID_PARAMETERS + 9)
#define ID_MAKE_ARGS			(ID_PARAMETERS + 10)
#define ID_MAXIMIZED			(ID_PARAMETERS + 11)
#define ID_GNUMODE			(ID_PARAMETERS + 12)
#define ID_HELP_FILE1			(ID_PARAMETERS + 13)
#define ID_HELP_FILE2			(ID_PARAMETERS + 14)
#define	ID_GROUPBOX			(ID_PARAMETERS + 15)
#define	ID_FRAMEBOX			(ID_PARAMETERS + 16)
#define	ID_BINMODE			(ID_PARAMETERS + 17)
#define ID_HELP_SEARCH1			(ID_PARAMETERS + 18)
#define ID_HELP_SEARCH2			(ID_PARAMETERS + 19)

BOOL 	APIENTRY 	  GetText( LPSTR, LPSTR, int );
int 	APIENTRY	  Replace( LPSTR, LPSTR, LPSTR, LPSTR, UINT );

LRESULT	APIENTRY MainWndProc( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY About( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY GetTextDlg( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY WarningDlg( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY ReplaceDlg( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY EmacsEditWndProc( HWND, UINT, WPARAM, LPARAM );
LRESULT APIENTRY ParamsDlgProc( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY BarWndProc( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY fBarWndProc( HWND, UINT, WPARAM, LPARAM );
LRESULT APIENTRY AbortProc( HDC, int );
LRESULT APIENTRY AbortDlg( HWND, UINT, WPARAM, LPARAM );
LRESULT	APIENTRY SignOnProc( HWND, UINT, WPARAM, LPARAM );

#if	defined( __cplusplus )
}
#endif

#endif
