#if	!defined( lint )
static	char rcsid[] = "$Id: mouse.c,v 1.3 2008/06/19 12:13:31 jullien Exp $";
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
 *	The  functions  in  this  file  implement  commands  that are
 *	handled by pointer device like a mouse.  Only Microsoft mouse
 *	for  MS-DOS  and  OS/2  operating  systems  in  text mode are
 *	implemented here.
 *
 *	Implemtation functions:
 *
 *	int	mouse_init(  void );
 *	void	mouse_stop(  void );
 *	void	mouse_show(  void );
 *	void	mouse_hide(  void );
 *	int	mouse_click( void );
 *	void	mouse_move(  unsigned int x, unsigned int y );
 *
 */

#if	defined( _MOUSE )

#if	defined( _DOS ) && !defined( _OS2 )

/* LINTLIBRARY */

#include	"emacs.h"
#include	"mouse.h"
#include	<dos.h>

struct	Mouse	{
	int	m1;
	int	m2;
	int	m3;
	int	m4;
};

extern	int	_define(rint86x,(int, union REGS *, union REGS *, struct SREGS *));
static	void	_define(mouse,(int call));

#define	MOUSE_INIT		0x0000
#define	MOUSE_SHOW		0x0001
#define	MOUSE_HIDE		0x0002
#define	MOUSE_GET_EVENT		0x0003
#define	MOUSE_MOVE		0x0004
#define	MOUSE_SET_VERTICAL	0x0008
#define	MOUSE_SET_SHAPE		0x000A
#define	MOUSE_SET_CURSOR	0x0010

#define	SYSMOUSE		0x33

#define	MOUSENAME		regs.x.ax
#define	M1			regs.x.ax
#define	M2			regs.x.bx
#define	M3			regs.x.cx
#define	M4			regs.x.dx

#define	SYSTEM( call )		(void)int86x( (call), &regs, &regs, &segs )

static	union	REGS	regs;
static	struct	SREGS	segs;
static	struct	Mouse	mousevect;

static	void
mouse( int call )
{
	if( !mouse_flag )
		return;

	if( call == MOUSE_INIT )
		segread( &segs );

	MOUSENAME	= call;
	M2		= mousevect.m2;
	M3		= mousevect.m3;
	M4		= mousevect.m4;
	SYSTEM( SYSMOUSE );
	mousevect.m1	= M1;
	mousevect.m2	= M2;
	mousevect.m3	= M3;
	mousevect.m4	= M4;
}

int
mouse_init( void )
{
	int	flag;

	if( (flag = mouse_flag) != NIL ) {
		if( !mouseflag ) {
			mouse( MOUSE_INIT );
			if( (flag = mousevect.m1) != 0 ) {
				mousevect.m1 = 0;
				mousevect.m2 = 0;
				mousevect.m3 = (int)0xFF00;
				mousevect.m4 = (int)0x00B3; /* decimal 179 */
				mouse( MOUSE_SET_SHAPE );
			}
		}

		mousevect.m2 = 0;
		mousevect.m3 = 0;
		mousevect.m4 = ((screen_height > 25) ? 350 : 200) - 8;
		mouse( MOUSE_SET_VERTICAL );
		if( mouseflag )
			mouse_show();
	}

	return( flag );

}

void
mouse_stop( void )
{
	mouse_hide();
}

void
mouse_show( void )
{
	mouse( MOUSE_SHOW );
}

void
mouse_hide( void )
{
	mouse( MOUSE_HIDE );
}

void
mouse_move( unsigned int x, unsigned int y )
{
	mousevect.m3 = 8 * x;
	mousevect.m4 = 8 * y;
	mouse( MOUSE_MOVE );
}

int
mouse_click( void )
{
	mouse( MOUSE_GET_EVENT );
	mevent.x	= (unsigned int)(mousevect.m3/8);
	mevent.y	= (unsigned int)(mousevect.m4/8);
	mevent.button	= mousevect.m2;

	do
		mouse( MOUSE_GET_EVENT );
	while( mousevect.m2 );

	if( mevent.button ) {
		regs.x.ax = 0x0200;
		(void)int86x( 0x16, &regs, &regs, &segs );
		if( regs.x.ax & 0x3 )
			/* Left or right shift ? */
			return( mevent.button |= SHIFTBUTTON );
		if( regs.x.ax & 0x4 )
			/* Control ? */
			return( mevent.button |= CTRLBUTTON );
	}

	return( mevent.button );
}

#endif	/* MS-DOS */

#if	defined( _OS2 ) && !defined( _PM20 )

#define	 INCL_SUB
#include <os2.h>
#include <stdio.h>
#include "emacs.h"
#include "mouse.h"

#if	!defined( MHK_BUTTON1 )
   #define MHK_BUTTON1                0x0001
   #define MHK_BUTTON2                0x0002
   #define MHK_BUTTON3                0x0004
#endif

#if	!defined( MOUSE_MOTION )
   #define MOUSE_MOTION                 0x0001
   #define MOUSE_MOTION_WITH_BN1_DOWN   0x0002
   #define MOUSE_BN1_DOWN               0x0004
   #define MOUSE_MOTION_WITH_BN2_DOWN   0x0008
   #define MOUSE_BN2_DOWN               0x0010
   #define MOUSE_MOTION_WITH_BN3_DOWN   0x0020
   #define MOUSE_BN3_DOWN               0x0040
#endif

static	HMOU		OS2mouse;
static	PTRLOC		OS2moupos;
static	NOPTRRECT	OS2mourtrec  = {    0,    0,   24,   79 };
static	BYTE		OS2newmask[] = { 0x00, 0xff, 0xb3, 0x00 };

int
mouse_init( void )
{
	BYTE	 abBuffer[ 4 ];
	PTRSHAPE moupsInfo;
	int	 i;

	mouse_flag = (MouOpen( 0L, &OS2mouse ) == 0);

	if( mouse_flag ) {
		USHORT	fsEvents = (MOUSE_BN1_DOWN |
				    MOUSE_BN2_DOWN |
				    MOUSE_BN3_DOWN);
		MouSetEventMask( &fsEvents, OS2mouse );
		moupsInfo.cb = sizeof( abBuffer );
		MouGetPtrShape( abBuffer, &moupsInfo, OS2mouse );
		for( i = 0 ; i < 4 ; i++ )
			abBuffer[ i ] = OS2newmask[ i ];
		MouSetPtrShape( abBuffer, &moupsInfo, OS2mouse );
	}

	return( mouse_flag );
}

void
mouse_show( void )
{
	if( !mouse_flag )
		return;

	MouDrawPtr( OS2mouse );
}

void
mouse_hide( void )
{
	if( !mouse_flag )
		return;

	MouRemovePtr( &OS2mourtrec, OS2mouse );
}

void
mouse_stop( void )
{
	mouse_hide();
	MouClose( OS2mouse );
}

void
mouse_move( unsigned int x, unsigned int y )
{
	if( !mouse_flag )
		return;

	OS2moupos.col = x;
	OS2moupos.row = y;
	MouSetPtrPos( &OS2moupos, OS2mouse );
}

int
mouse_click( void )
{
	static	MOUEVENTINFO	mouevEvent;
	static	USHORT		fWait = MOU_NOWAIT;
	static	int		click = 0;

	int	x;
	int	y;

	if( !mouse_flag )
		return( 0 );

	if( click ) {
		int	ret = click;
		click = 0;
		return( ret );
	}

	MouReadEventQue( &mouevEvent, &fWait, OS2mouse );
	MouGetPtrPos( &OS2moupos, OS2mouse );

	mevent.x = OS2moupos.col;
	mevent.y = OS2moupos.row;

	switch( mouevEvent.fs ) {
	case MOUSE_BN1_DOWN :
		return( click = MButton1 );
	case MOUSE_BN2_DOWN :
		return( click = MButton2 );
	default	    :
		return( 0 );
	}

}

#endif	/* defined( _DOS ) && defined( _OS2 ) */

#else	/* NO MOUSE */

int	mouse_init(void);
void	mouse_stop(void);
void	mouse_hide(void);
void	mouse_show(void);
int	mouse_click(void);

int
mouse_init( void )
{
	return( 0 );
}

void
mouse_stop( void )
{
}

void
mouse_hide( void )
{
}

void
mouse_show( void )
{
}

int
mouse_click( void )
{
	return( 0 );
}

#endif	/* _MOUSE  */
