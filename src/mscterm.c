#if	!defined( lint )
static	char rcsid[] = "$Id: mscterm.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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
 *	The  routines  in  this  file provide support for MS-DOS with
 *	Microsoft C Compiler.
 */

#include	"emacs.h"

#if	(defined( _MSC_VER ) || defined( _QC )) && !defined( _ANSITERM )
#if	defined( _MSC_VER ) && (_MSC_VER < 700)

#include	<graph.h>
#include        <dos.h>

#define	NROW		25
#define NCOL		80

#define	Mono_Normal  	7		/* Text   : Light Grey/Black	*/
#define	Mono_Status	112		/* Status : Light Grey/Black	*/
#define	Color_Normal  	30		/* Text   : Light Grey/Blue	*/
#define	Color_Status	112		/* Status : Dark  Grey/White	*/

static	int	_define(pcgetc,(void));
static  void	_define(pcopen,(void));
static  void	_define(pcclose,(void));
static  void	_define(pceeol,(void));
static  void	_define(pceeop,(void));
static	void	_define(pcputc,(int c));
static	void	_define(pcflush,(void));
static	void	_define(pccshow,(int flag));
static  void	_define(pcbeep,(void));
static  void	_define(pcsi,(void));
static  void	_define(pcmove,(int row, int col));
static	void	_define(pcputc,(int c));
static	void	_define(pcputs,(EMCHAR *s, int n));
static	int	_define(pccheck,(void));
static	void	_define(pcrawmode,(void));

/*
 *	Standard  terminal interface  dispatch table.  Most  of  the
 *	fields point into "termio" code.
 */

TERM	term	= {
	NROW-1,
	NCOL,
	0,
	NIL,
	pcopen,
	pcclose,
	pcgetc,
	pcputc,
	pcputs,
	pcflush,
	pcmove,
	pceeol,
	pceeop,
	pcbeep,
	pcsi,
	pcsi,
	pccshow,
	pccheck,
	pcrawmode
};

int	curcolor;		/* Current color		*/
int	invcolor;		/* Inverse color		*/

struct	VIDEOCONFIG {
	int far	*videomem;	/* Address of video memory	*/
	int	mrows;		/* Matrix rows			*/
	int	mcols;		/* Matrix columns		*/
	int	rescan;		/* CGA rescan flag		*/
	int	palette;	/* Palette flag			*/
}	cfg;

int	far	*videobuffer = (int far *)0xb0000000L;

void
putline( int x, int y, int far *inbuffer, int n, int attrib )
{
	int	start = (x * cfg.mcols + y) * 2;
	int	bsize = TTYncol - y;

	_asm	{

	push	ds
	mov	ax,attrib	; Load attribute
	mov	ah,al		; in ah
	mov	dx,NIL		; default fast redisplay
	mov     cx,fast_redisplay
	cmp     cx,T
	je	fast
	mov	dx,cfg.rescan	; Load recan flag
fast:	mov	bx,start
	mov	cx,bsize	; up to last column 
	les	di,videobuffer
	lds	si,inbuffer	; Load src = screen buffer
	add	di,bx		; (x * NCOLS + y) * sizeof( CELL )
        cld			; DF = 0 (direction flag)
	cmp	dx,NIL		; If not CGA no rescan check
	je	notcga

	mov	dx,03DAh
wait0:
	sti
	nop
	cli
	lodsb			; Load character and save in BX
	or	al,al
	je	getout
	mov	bx,ax
	cli			; begin of the crucial section
	mov	ah,09h		; Mask for retrace
wait1:	in	al,dx		; get status
	rcr	al,1
	jb	wait1		; and loop
wait2:	in	al,dx		; get status
	and	al,ah		; display enable?
	jz	wait2		; wait until not
	sti			; reset interrup
	mov	ax,bx		; Restore character and
	stosw			;   move to video memory
	sti
	loop	wait0		; Next
	jmp	short getout	; Done for CGA

notcga:				; Non-CGA version
	lodsb
	or	al,al
	je	getout
	stosw
	loop	notcga

getout:
	pop	ds
	}

}

static	void
configinit( void )
{
	struct videoconfig vc;
	unsigned char	*setting;
	union	REGS	regs;
	struct	SREGS	segs;

	/*
	 *	Get configuration and set variables based on adapter.
	 */

	cfg.rescan = NIL;
	cfg.mrows  = 25;
	cfg.mcols  = 80;
	cfg.palette= T;

	_getvideoconfig( &vc );
	switch( vc.adapter ) {
        case _CGA:
		cfg.rescan = T;
		break;
        case _EGA:
        case _OEGA:
	case _VGA:
        case _OVGA:
		cfg.mrows = (screen_height > 25) ? 43 : 25;
		fast_redisplay = T;
		break;
        default:
		cfg.palette= NIL;
		break;
	}

	/*
	 *	Set variables based on mode.
	 */

	switch( vc.mode ) {
        case _HERCMONO:
        case _ERESNOCOLOR:
        case _TEXTMONO:
		cfg.videomem = (int far *)0xb0000000L;
		_setvideomoderows( _TEXTMONO, cfg.mrows );
		break;
        case _TEXTBW40:
        case _TEXTBW80:
        case _TEXTC80:
		cfg.videomem = (int far *)0xb8000000L;
		_setvideomoderows( _TEXTBW80, cfg.mrows );
		break;
        default:
		if( int86x( 0x11, &regs, &regs, &segs ) & 0x48 ) {
			cfg.videomem = (int far *)0xb0000000L;
			_setvideomoderows( _TEXTMONO, cfg.mrows );
		} else	{
			cfg.videomem = (int far *)0xb8000000L;
			_setvideomoderows( _TEXTC80, cfg.mrows );
		}
		break;
	}

	if( (setting = getenv( "EMACS" )) != (char *)NULL ) {
		/*
		 *	set	EMACS=R66 C128
		 */
		setting++;			/* skip 'R'	*/
		TTYnrow = atoi( setting );
		while( !separatorp( *++setting ) )
			;
		setting += 2;			/* skip 'C'	*/
		TTYncol = atoi( setting );
	} else	{
		TTYnrow = cfg.mrows-1;
		TTYncol = cfg.mcols;
	}

}

CMD
switchscreen( void )
{
	if( cfg.mrows == 25 )
		return( T );

	if( TTYnrow == 24 ) {
		TTYnrow = cfg.mrows-1;
		_setvideomoderows( _TEXTC80, cfg.mrows );
	} else	{
		TTYnrow = 24;
		_setvideomoderows( _TEXTC80, 25 );
	}

	if( cfg.palette ) {
		_remappalette(  1, 0x150000L );
		_remappalette( 14, 0x2a3f3fL );
	}

	(void)onlywind();
	update( T );
	(void)redrawscreen();

	return( T );
}

static	void
pcopen( void )
{
	extern	int	initflag;

	if( !initflag ) {
		configinit();
		videobuffer = cfg.videomem;
	}

	mouseflag  = mouse_init();

	_setvideomoderows( _TEXTC80, TTYnrow+1 );
	_displaycursor( _GCURSORON );

	if( monochrome_monitor == T ) {
		curcolor = Mono_Normal;
		invcolor = Mono_Status;
	} else	{
		if( foreground_color != -1 && background_color != -1 )
			curcolor = (background_color << 4) | foreground_color;
		else	curcolor = Color_Normal;
		invcolor = Color_Status;
	}
	
	if( black_on_white == T ) {
		int swap = curcolor;
		curcolor = invcolor;
		invcolor = swap;
	}

	if( cfg.palette ) {
		_remappalette(  1, 0x150000L );
		_remappalette( 14, 0x2a3f3fL );
	}
	TTYinit = T;

}

static	void
pcclose( void )
{
	mouse_stop();
	_setvideomode( _DEFAULTMODE );
	TTYinit = NIL;
}

int
testkey( void )
{
	int	c;

	_asm	{
 		mov	ah,06h		; direct i/o
 		mov	dl,0FFh		; ask for a keyboard character
 		int	021h		; call MS-DOS
 		jnz	ready		; got one
 		mov	ax,-1		; not yet
 		jmp	short retkey
ready:		xor	ah,ah		; a char
		cmp	al,0		; extended ?
		jne	isspace
 		mov	ah,06h		; direct i/o
 		mov	dl,0FFh		; ask for a keyboard character
 		int	021h		; call MS-DOS
		or	ax,0100h	; mark as extended
		jmp	short retkey
isspace:
		cmp	ax,020h		; chek for space
		jne	short retkey	; no ? return
		mov	bx,ax		; save char
		mov	ah,02h		; ask for ctrl
		int	016h		; keyboard flags
		and	al,4		; bit 2 is for ctrl
		jz	short space	; no ctrl ? really a space
		mov	bx,0h		; Ctrl-' ' means 0 !!
space:
		mov	ax,bx		; set char
retkey:
		mov	c,ax		; return c
	}

	return( c );
}

int
waitkey( void )
{
	int	c;

	_asm	{
 		mov	ah,07h		; direct i/o
 		int	021h		; call MS-DOS
		and	ax,0FFh
		cmp	al,0		; extended ?
		jne	isspace
 		mov	ah,07h		; direct i/o
 		int	021h		; call MS-DOS
		or	ax,0100h	; mark as extended
		jmp	short retkey
isspace:
		cmp	ax,020h		; chek for space
		jne	short retkey	; no ? return
		mov	bx,ax		; save char
		mov	ah,02h		; ask for ctrl
		int	016h		; keyboard flags
		and	al,4		; bit 2 is for ctrl
		jz	short space	; no ctrl ? really a space
		mov	bx,0h		; Ctrl-' ' means 0 !!
space:
		mov	ax,bx		; set char
retkey:
		mov	c,ax		; return c
	}

	return( c );
}

static	void
pcputc( int ch )
{
	union	REGS	regs;
	struct	SREGS	segs;

	/*
	 *	write char to screen with current attrs
	 */

	regs.h.ah = 9;
	regs.h.al = (unsigned char)ch;
	regs.x.bx = curcolor;
	regs.x.cx = 1;
	(void)int86x( 0x10, &regs, &regs, &segs );
	regs.h.ah = 14;
	(void)int86x( 0x10, &regs, &regs, &segs );
}

static	int
pcgetc( void )
{
	int	c;

	if( !mouseflag )
		if( (c = waitkey()) & 0x100 )
			return( SPCL|(c&0xff) );
		else	return( c );

	mouse_show();

	for( ;; ) {
		if( (c = testkey()) != 0xffff ) {
			mouse_hide();
			if( c & 0x100 )
				return( SPCL|(c&0xff) );
			else	return( c );
		}

		if( mouseflag && mouse_click() ) {
			mouse_hide();
			return( MEVT );
		}

	}
}

static	void
pceeol( void )
{
	union	REGS	regs;
	struct	SREGS	segs;

	/*
	 *	read cursor position function code
	 */

	regs.x.ax = 0x0300;
	regs.x.bx = 0x0000;
	(void)int86x( 0x10, &regs, &regs, &segs );

	/*
	 *	clear to end of line
	 */

	regs.x.ax = 0x0600;
	regs.x.cx = regs.x.dx;
	regs.x.dx = (regs.h.dh << 8) | (TTYncol - 1);
	regs.h.bh = (unsigned char)curcolor;
	(void)int86x( 0x10, &regs, &regs, &segs );

}

static	void
pceeop( void )
{
	union	REGS	regs;
	struct	SREGS	segs;

	regs.x.ax = 0x0600;
	regs.x.cx = 0x0000;
	regs.x.dx = (TTYnrow << 8) | (TTYncol - 1);
	regs.h.bh = (unsigned char)curcolor;
	(void)int86x( 0x10, &regs, &regs, &segs );
}

static	void
pcputc( int c )
{
	switch( (char)c ) {
	case '\n':
	case '\r':
	case '\b':
		(void)fputc( c, stdout );
		break;
	default  :
		tputc( c, curcolor );
	}
}

static	int
pccheck( void )
{
	return( 0 );
}

static	void
pcrawmode( void )
{
}

static	void
pcputs( EMCHAR *s, int n )
{
	while( n-- )
		TTYputc( (int)*s++ );
}

#define	TIMER	040h
#define	PORT_B	061h

static	void
pcbeep( void )
{
	_asm	{
		mov	bx,1
		mov	al,10110110B
		out	TIMER+3,al
		jmp	short $+2
		mov	ax,200h			; standard tone = 533
		out	TIMER+2,al
		jmp	short $+2
		mov	al,ah
		out	TIMER+2,al
		in	al,PORT_B
		mov	ah,al
		jmp	short $+2
		or	al,03
		out	PORT_B,al
		mov	cx,0FFFFh
		xor	cx,cx
sound:		loop	sound
		dec	bl
		jnz	sound
		mov	al,ah
		out	PORT_B,al
	}
}

static	void
pcsi( void )
{
	int	swap = curcolor;

	curcolor = invcolor;
	invcolor = swap;
}

static	void
pcmove( int row, int col )
{
	union	REGS	regs;
	struct	SREGS	segs;

	regs.x.ax = 0x0200;
	regs.x.bx = 0x0000;
	regs.h.dh = (unsigned char)row;
	regs.h.dl = (unsigned char)col;
	(void)int86x( 0x10, &regs, &regs, &segs );

}

#endif
#endif
