#if	!defined( lint )
static	char rcsid[] = "$Id: ibmterm.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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
 *	most C Compilers.
 */

#include	"emacs.h"

#if	defined( _PCTERM )

static	int	_define(pcgetc,(void));
static  void	_define(pcopen,(void));
static  void	_define(pcclose,(void));
static  void	_define(pceeol,(void));
static  void	_define(pceeop,(void));
static	void	_define(pcputc,(int c));
static  void	_define(pcbeep,(void));
static	void	_define(pcputs,(EMCHAR *s, int n));
static  void	_define(pcsi,(void));
static	void	_define(pcflush,(void));
static	void	_define(pccshow,(int flag));
static  void	_define(pcmove,(int row, int col));

#if	defined( __cplusplus )
extern	"C"	{
#endif
extern	void	_define(videoinit,(void));
extern	void	_define(cleol,(int color));
extern	void	_define(cleos,(int color));
extern	void	_define(tputc,(int c, int color));
extern	void	_define(curpos,(int row, int col));
extern	void	_define(beep,(void));
extern	int	_define(testkey,(void));
extern	int	_define(waitkey,(void));
extern	int	_define(videorows,(void));
extern	void	_define(putline,(int row,int col,char *line,int nbchar,int color));
#if	defined( __cplusplus )
}
#endif
static	int	_define(pccheck,(void));
static	void	_define(pcrawmode,(void));

#include	<dos.h>

extern	int	initflag;

#define	NROWSTD		25		/* Standard vertical resolution	*/
#define	NROWMID		43		/* Medium resolution 43 lines	*/
#define	NROWMAX		50		/* Max resolution 50 lines	*/
#define NCOL		80		/* Horizontal resolution	*/

#define	NO_BORDER			/* Defined if colored borders	*/

#define	Mono_Normal  	7		/* Text   : Light Grey/Black	*/
#define	Mono_Status	112		/* Status : Light Grey/Black	*/
#define	Color_Normal  	30		/* Text   : Light Grey/Blue	*/
#define	Color_Status	112		/* Status : Dark  Grey/White	*/

/*
 *	Standard  terminal interface  dispatch table.  Most  of  the
 *	fields point into "termio" code.
 */

TERM	term	= {
	NROWSTD-1,
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

static	int	pccol;
static	int	pcrow;
static	int	pccurcolor;
static	int	pcinvcolor;

#if	defined( __cplusplus )
extern	"C"	{
#endif
extern	short	int	rows;
extern	short	int	cols;
#if	defined( __cplusplus )
}
#endif

static	unsigned char	pcoldemul;
static	unsigned int	pcoldmode;
static	unsigned int	pcoldcurs;
static	unsigned int	pcoldnrows;
static	unsigned int	pcstrokes;
static	unsigned int	pcversion = 6;

static	int allmode[] = { NROWSTD, NROWMID, NROWMAX };
static	int curmode   = -1;

static	union	{
	unsigned char far *val;
	unsigned long      adr;
} info;

static	union	REGS	regs;
static	struct	SREGS	segs;

static	void
pcopen()
{
	int	i;

	if( curmode == -1 ) {
		curmode = 0;
		for( i = 0 ; i < (sizeof(allmode)/sizeof(allmode[0])) ; i++ )
			if( allmode[ i ] <= screen_height )
				curmode = i;
		screen_height = allmode[ i ];
	}

	if( !initflag ) {
		regs.x.bx = 0x0000;
		regs.x.cx = 0x0000;
		regs.x.dx = 0x0000;
		regs.x.ax = 0x1130;
		(void)int86x( 0x10, &regs, &regs, &segs );
		pcoldnrows  = regs.x.dx + 1;
		videoinit();

		/*
		 *	get current video display mode and save it
		 */

		if( allmode[curmode] != NROWSTD ) {
#if	!defined( __SC__ )
			info.adr  = 0x00400087L;
#else
			long	newpage = 0x00400087L;
			int	off 	= (unsigned int)(newpage);
			int	seg	= (unsigned int)(newpage>>16)+(off>>4);

			newpage	  = ((unsigned long)seg << 16) | (off & 0x0F);
			info.val  = _x386_mk_protected_ptr( newpage );
			pcversion = _osmajor;

			if( getenv( "OS" ) )
				pcversion = 7;

#endif
			if( pcversion < 7 )
				pcoldemul = *info.val;

			regs.x.ax = 0x0F00;
			(void)int86x( 0x10, &regs, &regs, &segs );
			pcoldmode = regs.x.ax & 0xff;

			regs.x.ax = 0x0300;
			regs.x.bx = 0x0000;
			(void)int86x( 0x10, &regs, &regs, &segs );
			pcoldcurs = regs.x.cx;
		}

	}

	mouseflag = mouse_init();

	if( allmode[curmode] != NROWSTD ) {
		/*
		 *	get EGA/VGA information bios call
		 *	return 0x10 if EGA/VGA no present.
		 */

		regs.x.ax = 0x1200;
		regs.x.bx = 0x0010;
		(void)int86x( 0x10, &regs, &regs, &segs );

		if( regs.x.bx != 0x10 ) {

			regs.x.ax = 0x1A00;

			if( (int86x(0x10, &regs, &regs, &segs)&0xFF) == 0x1A ) {
				/*
				 * It's  a VGA,  limit display to 350
				 * lines  (43  rows).  The  same way,
				 * regs.x.ax  =  0x1202 limit display
				 * to 400 lines (50 rows).
				 */

				switch( allmode[curmode] ) {
				case NROWMID : regs.x.ax = 0x1201; break;
				case NROWMAX : regs.x.ax = 0x1202; break;
				default      : regs.x.ax = 0x1202;
				}

				regs.x.bx = 0x0030;
				(void)int86x( 0x10, &regs, &regs, &segs );
			}

			regs.x.ax = pcoldmode & 0xff;
			(void)int86x( 0x10, &regs, &regs, &segs );

			/*
			 * load the 8x8 character set, and set up the bios ROWS
			 */

			regs.x.ax = 0x1112;
			regs.x.bx = 0x0000;
			(void)int86x( 0x10, &regs, &regs, &segs );

			/*
			 *	get  the  current  value  of the bios
			 *	data   INFO  byte,  reset  bit  0  to
			 *	disable  cursor  emulation  mode  and
			 *	put  the  nev  value back in the bios
			 *	INFO byte
			 */

			if( pcversion < 7 )
				*info.val  |= 0x0001;

			/*
			 *	select  the  EGA bios alternate print
			 *	screen routine so it prints 43 rows
			 */

			regs.x.ax = 0x1200;
			regs.x.bx = 0x0020;
			(void)int86x( 0x10, &regs, &regs, &segs );

			/*
			 *	set terminal dimension.
			 */

			TTYnrow = (rows=allmode[curmode])-1;
			TTYncol = (cols=NCOL);

			regs.x.ax = 0x0100;
			regs.x.cx = 0x0607;
			(void)int86x( 0x10, &regs, &regs, &segs );

#if	defined( _BORDER )

			/*
			 *	set the color border
			 */

			regs.x.ax = 0x1001;
			regs.x.bx = (background_color << 8);
			(void)int86x( 0x10, &regs, &regs, &segs );
#endif

		}

	} else	{

		/*
		 *	no   EGA/VGA  found,  set  standard  terminal
		 *	dimension.
		 */

		TTYnrow = (rows=NROWSTD)-1;
		TTYncol = (cols=NCOL);
	}

	if( monochrome_monitor == T ) {
		pccurcolor = Mono_Normal;
		pcinvcolor = Mono_Status;
	} else	{
		if( foreground_color != -1 && background_color != -1 )
			pccurcolor = (background_color << 4) | foreground_color;
		else	pccurcolor = Color_Normal;
		pcinvcolor = Color_Status;
	}

	if( black_on_white == T ) {
		int swap;
		swap	   = pccurcolor;
		pccurcolor = pcinvcolor;
		pcinvcolor = swap;
	}
	TTYinit = T;
}

static	void
pcclose()
{
	/*
	 *	put system back in original video mode.
	 */

	if( allmode[ curmode ] != NROWSTD ) {
		if( pcversion < 7 )
			*info.val = pcoldemul;

		regs.x.ax = pcoldmode & 0xff;
		(void)int86x( 0x10, &regs, &regs, &segs );

		regs.x.ax = 0x0100;
		regs.x.bx = 0x0000;
		regs.x.cx = pcoldcurs;
		(void)int86x( 0x10, &regs, &regs, &segs );

	}

	switch( pcoldnrows ) {
	case NROWMID :
		regs.x.ax = 0x1201;
		regs.x.bx = 0x0030;
		(void)int86x( 0x10, &regs, &regs, &segs );
		/*
		 * load the 8x8 character set, and set up the bios ROWS
		 */

		regs.x.ax = 0x1112;
		regs.x.bx = 0x0000;
		(void)int86x( 0x10, &regs, &regs, &segs );
		break;
	case NROWMAX :
		regs.x.ax = 0x1202;
		regs.x.bx = 0x0030;
		(void)int86x( 0x10, &regs, &regs, &segs );
		/*
		 * load the 8x8 character set, and set up the bios ROWS
		 */

		regs.x.ax = 0x1112;
		regs.x.bx = 0x0000;
		(void)int86x( 0x10, &regs, &regs, &segs );
		break;
	}

	mouse_stop();
	TTYinit = NIL;

}

static	int
pcgetc()
{
	int	c;

	if( !mouseflag )
		if( (c = waitkey()) & 0x100 )
			return( SPCL|(c&0xff) );
		else	return( c );

	mouse_show();

	for( ;; ) {
		if( (c = testkey()) != ((int)0xffff) ) {
			mouse_hide();
			if( mouse_avoidance_mode == T &&
			    (++pcstrokes % mouse_avoidance_nudge) == 0 )
				mouse_move( TTYncol, TTYnrow );
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
pceeol()
{
	cleol( pccurcolor );
}

static	void
pceeop()
{
	cleos( pccurcolor );
}

static	void
pcputc( c )
int	c;
{
	switch( c ) {
	case '\n':
	case '\r':
	case '\b':
		(void)fputc( c, stdout );
		break;
	default  :
		tputc( c, pccurcolor );
	}
}

static	void
pcbeep()
{
	beep();
}

static	void
pcputs( EMCHAR *s, int n )
{
	putline( pcrow, pccol, (char *)s, n, pccurcolor );
	pccol += n;
	curpos( pcrow, pccol );
}

static	void
pcflush()
{
}

static	void
pccshow( int flag )
{
	curflag = flag;
}

static	void
pcsi()
{
	int	swap = pccurcolor;

	pccurcolor = pcinvcolor;
	pcinvcolor = swap;
}

static	void
pcmove( int row, int col )
{
	curpos( (pcrow = row), (pccol = col) );
}

CMD
switchscreen()
{
	vtfree();
	pcclose();

	curmode  = (curmode+1) % (sizeof(allmode)/sizeof(allmode[0]));
	initflag = 0;

	pcopen();
	vtinit();
	cleos( pccurcolor );

	resize();
	(void)redrawscreen();
	(void)update( T );

	return( T );
}

static	int
pccheck()
{
	return( 0 );
}

static	void
pcrawmode()
{
}

#endif
