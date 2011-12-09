#if	!defined( lint )
static	char rcsid[] = "$Id: os2term.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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

#if	defined( _OS2 ) && !defined( _PM20 )

/*
 *	The routines in this file provide support for OS2 terminal
 */

#define		INCL_SUB
#define		INCL_DOSMISC

#include	"emacs.h"
#include	<signal.h>
#include	<os2.h>

#define Cell( c )	((char *)&c)	/* 32 bits or 16 bits LARGE	*/

#define	Mono_Normal  	7		/* Text   : Light Grey/Black	*/
#define	Mono_Status	112		/* Status : Light Grey/Black	*/
#define	Color_Normal  	30		/* Text   : Light Grey/Blue	*/
#define	Color_Status	112		/* Status : Dark  Grey/White	*/

static	HKBD		OS2hkbd;
static	HVIO		OS2hvio;
static	KBDINFO		OS2nkbdstatus;
static	KBDINFO		OS2okbdstatus;
static	VIOMODEINFO	OS2nviomi;
static	VIOMODEINFO	OS2oviomi;
static	VIOFONTINFO	OS2fontinfo;
static	int		OS2curcolor;
static	int		OS2invcolor;
static	int		OS2extended;
static	int		OS2row;
static	int		OS2col;
static	int		OS2ostatus = 0;

/*
 *	Standard terminal interface dispatch table.
 */

static	void	OS2open( void );
static	void	OS2close( void );
static	int	OS2getc( void );
static	void	OS2putc( int c );
static	void	OS2puts( EMCHAR *s, int n );
static	void	OS2flush( void );
static	void	OS2move( int x, int y );
static	void	OS2eeol( void );
static	void	OS2eeop( void );
static	void	OS2beep( void );
static	void	OS2si( void );
static	void	OS2cshow( int flag );
static	int	OS2check( void );
static	void	OS2rawmode( void );
static	int	OS2testkey( void );

TERM	term	= {
	0,
	0,
	0,
	NIL,
	OS2open,
	OS2close,
	OS2getc,
	OS2putc,
	OS2puts,
	OS2flush,
	OS2move,
	OS2eeol,
	OS2eeop,
	OS2beep,
	OS2si,
	OS2si,
	OS2cshow,
	OS2check,
	OS2rawmode
};

#if	!defined( IO_WAIT )
#define	IO_WAIT	0
#endif

#if	defined( _OS2V2 )
#define	RAW_MASK	KEYBOARD_ECHO_OFF | KEYBOARD_BINARY_MODE
/*

 	KBDINFO.fsMask

	#define KEYBOARD_ECHO_ON                0x0001
	#define KEYBOARD_ECHO_OFF               0x0002
	#define KEYBOARD_BINARY_MODE            0x0004
	#define KEYBOARD_ASCII_MODE             0x0008
	#define KEYBOARD_MODIFY_STATE           0x0010
	#define KEYBOARD_MODIFY_INTERIM         0x0020
	#define KEYBOARD_MODIFY_TURNAROUND      0x0040
	#define KEYBOARD_2B_TURNAROUND          0x0080
	#define KEYBOARD_SHIFT_REPORT           0x0100
*/
#else
#define	RAW_MASK	0x04	/* 16 bits as reported */
#endif

static	void
OS2open( void )
{
	OS2row		 = 0;
	OS2col		 = 0; 

	OS2nkbdstatus.cb = sizeof( OS2nkbdstatus );
	OS2okbdstatus.cb = sizeof( OS2okbdstatus );

#if	defined( _OPEN_KBD )
	KbdOpen( &OS2hkbd );
#else
	OS2hkbd = 0;
	OS2hvio = 0;
#endif

	if( OS2ostatus == 0 ) {
		VioGetFont( &OS2fontinfo, OS2hvio );
		KbdGetStatus( &OS2okbdstatus, OS2hkbd );
		OS2ostatus = 1;
	}

	OS2fontinfo.cxCell = 8;
	OS2fontinfo.cyCell = 8;
	VioSetFont( &OS2fontinfo, OS2hvio );

	KbdGetStatus( &OS2nkbdstatus, OS2hkbd );
	OS2nkbdstatus.fsMask = RAW_MASK;
	KbdSetStatus( &OS2nkbdstatus, OS2hkbd );

	OS2oviomi.cb = sizeof( OS2oviomi );
	OS2nviomi.cb = sizeof( OS2nviomi );
	VioGetMode( &OS2oviomi, OS2hvio );
	VioGetMode( &OS2nviomi, OS2hvio );

	/*
	 *	used to test with (OS2nviomi.vres) > 350 to change size.
	 */

	if( screen_height < 25 || screen_height > 60 )
		screen_height = 25;

	OS2nviomi.row = screen_height;
	VioSetMode( &OS2nviomi, OS2hvio );

	TTYnrow   = OS2nviomi.row - 1;
	TTYncol   = OS2nviomi.col;

	if( monochrome_monitor == T ) {
		OS2curcolor = Mono_Normal;
		OS2invcolor = Mono_Status;
	} else	{
		if( foreground_color != -1 && background_color != -1 )
			OS2curcolor = (background_color<<4) | foreground_color;
		else	OS2curcolor = Color_Normal;
		OS2invcolor = Color_Status;
	}
	
	if( black_on_white == T ) {
		int swap = OS2curcolor;

		OS2curcolor = OS2invcolor;
		OS2invcolor = swap;
	}

	mouseflag = mouse_init();

	(void)signal( SIGBREAK, SIG_IGN );
	(void)signal( SIGINT, SIG_IGN );

	TTYinit = T;

}

static	void
OS2close( void )
{
	(void)signal( SIGINT,   SIG_DFL );
	(void)signal( SIGBREAK, SIG_DFL );

	OS2curcolor = Mono_Normal;
	OS2move( 0, 0 );
	OS2eeop();

	VioSetMode( &OS2oviomi, OS2hvio );
	KbdSetStatus( &OS2okbdstatus, OS2hkbd );
	TTYinit = NIL;
}

static	int
OS2getc( void )
{
	int	c;
	KBDKEYINFO	keydata;
#if	!defined( _MOUSE )

	KbdCharIn( &keydata, IO_WAIT, OS2hkbd );
	if( keydata.chChar == ' ' && keydata.fsState & KBDSTF_CONTROL )
		return( 0 );

	if(  keydata.chChar != 0 && keydata.chChar != 0xE0 )
		return( keydata.chChar );
	else	return( keydata.chScan | SPCL );
#else
	mouse_show();

	for( ;; ) {
		if( OS2testkey() ) {
			mouse_hide();
			KbdCharIn( &keydata, IO_WAIT, OS2hkbd );
			if( keydata.chChar == ' ' &&
			    keydata.fsState & KBDSTF_CONTROL )
			    	return( 0 );

			    if(  keydata.chChar != 0 && keydata.chChar != 0xE0 )
			    	return( keydata.chChar );
			    else	return( keydata.chScan | SPCL );
		}

		if( mouseflag && mouse_click() ) {
			mouse_hide();
			return( MEVT );
		}
	}
#endif
}

static	void
OS2putc( c )
int	c;
{
	unsigned char aChar = (unsigned char)c;
	unsigned int  cell;

	switch( aChar ) {
	case '\n':
		if( OS2row <= TTYnrow ) {
			++OS2row;
			OS2col = 0;
		}
		break;
	case '\r': 
		OS2col = 0;
		break;
	case '\b':
		if( OS2col > 0 )
			--OS2col;
		break;
	default  :
		cell = (OS2curcolor<<8) + aChar;
		VioWrtNCell( Cell( cell ), 1, OS2row, OS2col++, OS2hvio );
	}
}

void
OS2puts( EMCHAR *str, int cnt )
{
	VioWrtCharStrAtt(str,cnt,OS2row,OS2col,(PBYTE)&OS2curcolor,OS2hvio);
	OS2col += cnt;
}

static	void
OS2flush()
{
	VioSetCurPos( OS2row, OS2col, OS2hvio );
}

static	void
OS2move( int row, int col )
{
	OS2row = row;
	OS2col = col;
}

static	void
OS2eeol( void )
{
	unsigned int cell = (OS2curcolor<<8) + ' ';

	VioWrtNCell( Cell(cell), (TTYncol - OS2col), OS2row, OS2col, OS2hvio );
}

static	void
OS2eeop( void )
{
	unsigned int cell = (OS2curcolor<<8) + ' ';

	OS2eeol();
	VioScrollDn( OS2row, 0, -1, -1, -1, Cell( cell ), OS2hvio );
}

static	void
OS2beep( void )
{
	(void)DosBeep( 0x500, 0x20 );
}

static	void
OS2si( void )
{
	int swap = OS2curcolor;

	OS2curcolor = OS2invcolor;
	OS2invcolor = swap;
}

static	void
OS2cshow( int flag )
{
	curflag = flag;
	OS2flush();
}

static	int
OS2testkey( void )
{
	KBDKEYINFO	keydata;

	(void)KbdPeek( &keydata, OS2hkbd );

	return( keydata.chChar != 0 );
}

CMD
switchscreen( void )
{
#if	defined( _NEED_WORK )
	vtfree();
	OS2close();

	if( OS2nviomi.vres > 350 )
	     OS2nviomi.row = (OS2nviomi.row==50) ? 25 : 50;
	else OS2nviomi.row = (OS2nviomi.row==screen_height)?25:screen_height;

	TTYnrow   = OS2nviomi.row - 1;
	TTYncol   = OS2nviomi.col;
	VioSetMode( &OS2nviomi, OS2hvio );

	resize();
	(void)redrawscreen();
	update( T );
#endif
	return( T );
}

static	int
OS2check( void )
{
	return( 0 );
}

static	void
OS2rawmode( void )
{
	OS2nkbdstatus.fsMask = RAW_MASK;
	KbdSetStatus( &OS2nkbdstatus, OS2hkbd );

}

#endif
