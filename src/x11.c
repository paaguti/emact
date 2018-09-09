#if	!defined( lint )
static	char rcsid[] = "$Id: x11.c,v 1.12 2013/02/20 06:06:25 jullien Exp $";
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
 *	The routines in this file provide support for X11.
 */

#include	"emacs.h"

#if	defined( _X11 )

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include "x11.ico"

static	int	_define(X11nearestcolor,(Display *dpy, const char* name, XColor *rgb));
static	void	_define(X11open,(void));
static	void	_define(X11close,(void));
static	int	_define(X11event,(void));
static	int	_define(X11getc,(void));
static	void	_define(X11putc,(int c));
static	void	_define(X11puts,(EMCHAR *s, int n));
static	void	_define(X11move,(int row, int col));
static	void	_define(X11eeol,(void));
static	void	_define(X11eeop,(void));
static	void	_define(X11flush,(void));
static	void	_define(X11beep,(void));
static	void	_define(X11ei,(void));
static	void	_define(X11si,(void));
static	void	_define(X11cshow,(int flag));
static	void	_define(X11cursor,(int flag));
static	int	_define(X11check,(void));
static	void	_define(X11rawmode,(void));
static	void	_define(X11clipcopy,(void));
static	void	_define(X11clippaste,(void));
static	EMCHAR*	_define(X11title,(EMCHAR *buf, EMCHAR *fname));
static	void	_define(X11movecursor,(int x, int y));

#if	!defined( _X11_ONLY )
extern	TERM	cursesterm;
#endif

TERM	term = {
	0,
	0,
	0,
	NIL,
	X11open,
	X11close,
	X11getc,
	X11putc,
	X11puts,
	X11flush,
	X11move,
	X11eeol,
	X11eeop,
	X11beep,
	X11si,
	X11ei,
	X11cshow,
	X11check,
	X11rawmode
};

#define	COLOR_TABLE_SIZE	8

static	const char* X11rgbcolors[COLOR_TABLE_SIZE] = {
	"rgb:0000/0000/0000",	/* black	*/
	"rgb:0000/0000/7fff",	/* blue		*/
	"rgb:0000/ffff/0000",	/* green	*/
	"rgb:0000/dfff/ffff",	/* cyan		*/
	"rgb:ffff/0000/0000",	/* red		*/
	"rgb:ffff/0000/ffff",	/* magenta	*/
	"rgb:ffff/ffff/0000",	/* yellow	*/
	"rgb:ffff/ffff/ffff"	/* white	*/
};

static unsigned long X11pixelcolors[8];

#define	MAXCOLOR	3

#define	BORDER		X11color[ 0 ]
#define	BACKGROUND	X11color[ 1 ]
#define	FOREGROUND	X11color[ 2 ]

#define	DEFAULTBDR	"Green"
#define	DEFAULTBGD4	"Blue"
#define	DEFAULTFGD4	"Yellow"
#define	DEFAULTBGD8	X11rgbcolors[ 1 ]	/* Blue */
#define	DEFAULTFGD8	X11rgbcolors[ 3 ]	/* Cyan	*/
#if	defined( __linux__ )
#define	DEFAULTFNT	"fixed"
#define	DEFAULTGEO	"80x30+0+0"
#else
#define	DEFAULTFNT	"8x13"
#define	DEFAULTGEO	"80x60+0+0"
#endif

#define	KEYSIZE		80
#define	MAXLINESIZE	256

static	Window		X11win;
static	XFontStruct *	X11fntinfo;
static	GC		X11gcstd;
static	GC		X11gcinv;
static	const char *	X11name[ MAXCOLOR ];
static	unsigned long	X11color[ MAXCOLOR ];
static	int		X11hfnt;
static	int		X11wfnt;
static	Display	*	X11dpy;
static	int		X11row;
static	int		X11col;
static	int		X11x;
static	int		X11y;
static	unsigned int	X11width;
static	unsigned int	X11height;
static	int		X11char;
static	int		X11debug	= 0;
static	int		X11shift	= 0;
static	int		X11ctrl		= 0;
static	int		X11meta		= 0;
static	int		X11expose	= 0;
static	int		X11term		= 1;
static	const char *	X11foreground	= (const char *)NULL;
static	const char *	X11background	= (const char *)NULL;
static	const char *	X11border	= (const char *)NULL;
static	const char *	X11font		= (const char *)NULL;
static	const char *	X11geometry	= (const char *)NULL;
static	const char *	X11display	= (const char *)NULL;

/*
 * A replacement for XAllocColor.  This function should never fail to
 * allocate  a color.  When XAllocColor fails,  we return the nearest
 * matching  color.  If we have to allocate many colors this function
 * isn't  a  great  solution;  the  XQueryColors() could be done just
 * once.
 */

static	int
X11nearestcolor( Display *dpy, const char *name, XColor *rgb )
{
	int	 screen = XDefaultScreen( dpy );
	Colormap cmap   = XDefaultColormap( dpy, screen );
	int	 depth  = XDisplayPlanes( dpy, screen );
	XColor	 *ctable;
	XColor	 subcolor;
	XColor	 exact;
	size_t	 i;
	int	 res;
	int	 bestmatch;
	size_t	 cmapsize;
	double	 mindist;        /* 3*2^16^2 exceeds long int precision. */

	if( !XParseColor( dpy, cmap, name, rgb ) )
		return( 0 );

	/*
	 * First try just using XAllocColor.
	 */

	if( XAllocColor( X11dpy, cmap, rgb ) )
		return( 1 );

	if( X11debug )
		(void)printf( "Can't allocate color %s\n\r", name );

	res = XLookupColor( dpy, cmap, name, &exact, rgb );

	if( res == 0 ) {
		(void)printf( "XLookupColor Fails for %s\r\n", name );
		return( 0 );
	}

	/*
	 * Retrieve color table entries.
	 */

	if( depth > 16 )
		cmapsize = 8192;		 /* first 8192 entries	 */
	else	cmapsize = (size_t)(1 << depth); /* compute real entries */

	ctable   = (XColor *)malloc( cmapsize * sizeof( XColor ) );

	for( i = 0 ; i < cmapsize ; ++i )
		ctable[i].pixel = i;

	XQueryColors( dpy, cmap, ctable, (int)cmapsize );

	/*
	 * Find best match.
	 */

	bestmatch = -1;
	mindist   = 0.0;

	for( i = 0 ; i < cmapsize ; ++i ) {
		double dr   = (double)rgb->red   - (double)ctable[i].red;
		double dg   = (double)rgb->green - (double)ctable[i].green;
		double db   = (double)rgb->blue  - (double)ctable[i].blue;
		double dist = dr * dr + dg * dg + db * db;

		if( bestmatch < 0 || dist < mindist ) {
		  bestmatch = (int)i;
			mindist   = dist;
		}
	}

	/*
	 * Return result.
	 */

	subcolor.red   = ctable[bestmatch].red;
	subcolor.green = ctable[bestmatch].green;
	subcolor.blue  = ctable[bestmatch].blue;

	free( ctable );

	if( !XAllocColor( dpy, cmap, &subcolor ) )
		subcolor.pixel = (unsigned long)bestmatch;

	*rgb = subcolor;

	return( 1 );
}

static	void
X11open( void )
{
	Pixmap		icon;
	XSizeHints	sizehints;
	XWMHints	wmhints;
	XGCValues	val;
	Atom		delwin;
	Atom		protocol;
	int		i;
	int		depth;
	int		height;
	int		screen;
	char		**cargv = aargv;
	int		cargc   = eargc;

	if( (X11term == 0) || (X11dpy = XOpenDisplay( X11display )) == NULL ) {
#if	!defined( _X11_ONLY )
		/*
		 *	Switch to curses term and try again.
		 */
		pterm = &cursesterm;
		TTYopen();
		return;
#else
		(void)fprintf( stderr, "Can't open display.\n" );
		exit( 0 );
#endif
	}

	XSynchronize( X11dpy, 1 );

	screen = XDefaultScreen( X11dpy );
	depth  = XDisplayPlanes( X11dpy, screen );
	height = XDisplayHeight( X11dpy, screen );

	if( (X11font == NULL) &&
	    ((X11font = XGetDefault(X11dpy,cargv[0],"font")) == NULL) )
		X11font = DEFAULTFNT;

	if( ((X11name[0] = X11border) == NULL) &&
	    ((X11name[0] = XGetDefault(X11dpy,cargv[0],"border")) == NULL) )
	    	X11name[ 0 ] = DEFAULTBDR;

#if	defined( _CDE12 ) || defined( __linux__ )
	/*
	 * CDE  1.2  has  a  default color for all application so the
	 * two following tests allways match.
	 */

	if( ((X11name[1] = X11background) == NULL) && 
	    ((X11name[1] = XGetDefault(X11dpy,cargv[0],"background")) == NULL) )
		X11name[ 1 ] = (( depth > 4 ) ? DEFAULTBGD8 : DEFAULTBGD4);

	if( ((X11name[2] = X11foreground) == NULL) && 
	    ((X11name[2] = XGetDefault(X11dpy,cargv[0],"foreground")) == NULL) )
		X11name[ 2 ] = (( depth > 4 ) ? DEFAULTFGD8 : DEFAULTFGD4);
#else
	X11name[ 1 ] = (( depth > 4 ) ? DEFAULTBGD8 : DEFAULTBGD4);
	X11name[ 2 ] = (( depth > 4 ) ? DEFAULTFGD8 : DEFAULTFGD4);
#endif

	if( (X11geometry == NULL) &&
	    ((X11geometry = XGetDefault(X11dpy,cargv[0],"geometry")) == NULL) )
		X11geometry = DEFAULTGEO;

	XParseGeometry( X11geometry, &X11x, &X11y, &X11width, &X11height );

	if( X11debug ) {
		(void)printf( "Depth       = %i\r\n", depth );
		(void)printf( "X11geometry = %s\r\n", X11geometry  );
		(void)printf( "X11name[0]  = %s\r\n", X11name[ 0 ] );
		(void)printf( "X11name[1]  = %s\r\n", X11name[ 1 ] );
		(void)printf( "X11name[2]  = %s\r\n", X11name[ 2 ] );
	}

	while( ((size_t)X11y * (size_t)X11height) > (size_t)height )
		--X11y;

	if( depth > 1 ) {
		XColor	 rgb;

		/*
		 *	Read standard color scheme
		 */

		for( i = 0 ; i < COLOR_TABLE_SIZE ; i++ ) {
			(void)X11nearestcolor( X11dpy, X11rgbcolors[i], &rgb );

			X11pixelcolors[i] = rgb.pixel;

			if( X11debug )
			    (void)printf(
					  "color table %d %s: %lx\n\r",
					  i,
					  X11rgbcolors[i],
					  X11pixelcolors[i]
				        );
		}

		BORDER	   = X11pixelcolors[1];
		FOREGROUND = X11pixelcolors[foreground_color % 8];
		BACKGROUND = X11pixelcolors[background_color % 8];

		if( X11debug ) {
		    (void)printf(
		    		  "Default foreground (index %d) = %lx\n\r",
				  foreground_color,
				  FOREGROUND
				);
		    (void)printf(
		    		  "Default background (index %d) = %lx\n\r",
				  background_color,
				  BACKGROUND
				);
		}

		/*
		 *	Set user preferences
		 */

		for( i = 0 ; i < MAXCOLOR ; i++ ) {
			(void)X11nearestcolor( X11dpy, X11name[i], &rgb );
			X11color[ i ] = rgb.pixel;

			if( X11debug )
			    (void)printf(
					  "User color %s: %lx\n\r",
					  X11name[i],
					  X11color[i]
					);
		}

	} else	{
		/*
		 *	depth = 1 (Black & White)
		 */
		BORDER	   = BlackPixel( X11dpy, screen );
		FOREGROUND = BlackPixel( X11dpy, screen );
		BACKGROUND = WhitePixel( X11dpy, screen );
	}

 	if( (X11fntinfo = XLoadQueryFont( X11dpy, X11font )) == NULL ) {
		(void)printf( "Invalid font name %s\n", X11font );
		exit( 1 );
	} else	{
		X11wfnt = XTextWidth( X11fntinfo, "_", 1 );
		X11hfnt = X11fntinfo->ascent + X11fntinfo->descent;
	}

	X11x	   = 0;
	X11y	   = 0;
	X11width  *= (unsigned int)X11wfnt;
	X11height *= (unsigned int)X11hfnt;
	X11win	   = XCreateSimpleWindow(
					  X11dpy,
					  XRootWindow( X11dpy, screen ),
					  X11x, X11y, X11width, X11height,
					  2,
					  BORDER,
					  BACKGROUND
					);

	icon = XCreateBitmapFromData(
				      X11dpy,
				      X11win,
				      (char *)x11_bits,
				      x11_width,
				      x11_height
				    );

	sizehints.flags		= PPosition | PSize | PMinSize;
	sizehints.x		= X11x;
	sizehints.y		= X11y;
	sizehints.width		= (int)X11width;
	sizehints.height	= (int)X11height;
	sizehints.min_width	= 100;
	sizehints.min_height	= 100;

	XSetStandardProperties(
				X11dpy,
				X11win,
				cargv[0], cargv[cargc], icon, cargv, cargc,
				&sizehints
			      );

	wmhints.flags		= IconPixmapHint | InputHint | StateHint;
	wmhints.input		= True;
	wmhints.initial_state	= NormalState;
	wmhints.icon_pixmap	= icon;

	XSetWMHints( X11dpy, X11win, &wmhints );

	XSelectInput(
		      X11dpy,
		      X11win,
		      ExposureMask	|
		      KeyPressMask	| KeyReleaseMask	|
		      ButtonPressMask	| StructureNotifyMask	|
		      EnterWindowMask	| LeaveWindowMask	|
		      KeymapStateMask
		    );

	val.foreground	= FOREGROUND;
	val.background	= BACKGROUND;
	X11gcstd	= XCreateGC(
				     X11dpy,
				     X11win,
				     (GCForeground|GCBackground),
				     &val
				   );

	val.foreground	= BACKGROUND;
	val.background	= FOREGROUND;
	X11gcinv	= XCreateGC(
				     X11dpy,
				     X11win,
				     (GCForeground|GCBackground),
				     &val
				   );

	XSetFont( X11dpy, X11gcstd, X11fntinfo->fid );
	XSetFont( X11dpy, X11gcinv, X11fntinfo->fid );
	XDefineCursor( X11dpy, X11win, XCreateFontCursor( X11dpy, XC_xterm ) );

	delwin   = XInternAtom( X11dpy, "WM_DELETE_WINDOW", 0 );
	protocol = XInternAtom( X11dpy, "WM_PROTOCOLS",     0 );

	XChangeProperty(
			 X11dpy,
			 X11win,
			 protocol,
			 XA_ATOM,
			 32,
			 PropModeReplace,
			 (unsigned char *)&delwin,
			 1
		       );

	X11col		   = 0;
	X11row		   = 0;

	TTYnrow		   = (int)(X11height / X11hfnt - 1);
	TTYncol 	   = (int)(X11width  / X11wfnt);
	TTYinit		   = T;

	mouseflag 	   = 1;

	widget.w_clipcopy  = X11clipcopy;
	widget.w_clippaste = X11clippaste;
	widget.w_title     = X11title;

	XMapWindow( X11dpy, X11win );

	while( X11event() != Expose )
		;

	X11flush();
}

static	void
X11close( void )
{
	XFlush( X11dpy );
	XUnloadFont( X11dpy, X11fntinfo->fid );
	XFreeGC( X11dpy, X11gcstd );
	XFreeGC( X11dpy, X11gcinv );
	XDestroyWindow( X11dpy, X11win );
	XCloseDisplay( X11dpy );
	TTYinit = NIL;
}

static	void
X11eeop( void )
{
	XClearWindow( X11dpy, X11win );
}

static	void
X11eeol( void )
{
	XClearArea(
		    X11dpy,
		    X11win,
		    X11col * X11wfnt,
		    X11row * X11hfnt,
		    0,
		    (unsigned int)X11hfnt,
		    False
		  );
}

static	void
X11move( int row, int col )
{
	X11row = row;
	X11col = col;
}

static	int
X11getc( void )
{
	static	int strokes = 0;


	X11char = 0;

	while( X11char == 0 )
		(void)X11event();

	if( mouse_avoidance_mode==T && ++strokes >= mouse_avoidance_nudge ) {
		strokes = 0;

		/*
		 * every N (default = 5) strokes, move mouse to the bottom
		 */

		X11movecursor( TTYncol + 1, TTYnrow + 1 );
	}

	return( X11char );
}

static	int
X11event( void )
{
	XEvent		event;
	Region		region;
	XRectangle	clip;
	char		buf[ KEYSIZE + 80 ]; /* magic, it's a X11 server BUG */
	KeySym		keysym;
	XComposeStatus	cmp;
	int		nbkeys;
#if	defined( AIX )
	char		*key;
#endif

	X11cursor( T );
	XNextEvent( X11dpy, &event );
	X11cursor( NIL );

	switch( event.type ) {

	case Expose:
		if( X11expose == 0 ) {
			X11expose = 1;
			XSetInputFocus(
					X11dpy,
					X11win,
					RevertToParent,
					CurrentTime
				      );
			return( Expose );
		}

		region = XCreateRegion();

		do	{
			clip.x	    = (short)event.xexpose.x;
			clip.y	    = (short)event.xexpose.y;
			clip.width  = (unsigned short)event.xexpose.width;
			clip.height = (unsigned short)event.xexpose.height;
			XUnionRectWithRegion( &clip, region, region );
		} while( XCheckTypedEvent( X11dpy, Expose, &event ) );

		XSetRegion( X11dpy, X11gcstd, region );
		sgarbf = EXPOSE;
		update( T );
		XDestroyRegion( region );

		clip.x		= (short)X11x;
		clip.y		= (short)X11y;
		clip.width	= (unsigned short)X11width;
		clip.height	= (unsigned short)X11height;
		XSetClipRectangles(X11dpy, X11gcstd, 0, 0, &clip, 1, Unsorted);
		break;

	case ConfigureNotify:
		if( TTYinit   == T &&
		    X11width  == (unsigned int)event.xconfigure.width  &&
		    X11height == (unsigned int)event.xconfigure.height )
		    	/*
		    	 *	no changes !
		    	 */
		    	break;


		if( X11expose != 0 )
			vtfree();

		X11width  = (unsigned int)event.xconfigure.width;
		X11height = (unsigned int)event.xconfigure.height;
		TTYncol   = event.xconfigure.width  / X11wfnt;
		TTYnrow   = event.xconfigure.height / X11hfnt - 1;
		if( TTYnrow <= 1 )
			TTYnrow = 2;

		if( X11expose != 0 ) {
			vtinit();
			resize();
		}

		break;

	case MappingNotify:
		XRefreshKeyboardMapping( &event.xmapping );
		break;

	case ButtonPress:
		XRaiseWindow( X11dpy, X11win );
		mevent.x      = event.xbutton.x / X11wfnt;
		mevent.y      = event.xbutton.y / X11hfnt;
		mevent.button = event.xbutton.button;

		if( X11shift )
			mevent.button |= SHIFTBUTTON;

		if( X11ctrl )
			mevent.button |= CTRLBUTTON;

		return( X11char = MEVT );

	case KeyRelease:
		(void)XLookupString(
				     (XKeyEvent *)&event.xkey,
				     buf,
				     KEYSIZE,
				     (KeySym *)&keysym,
				     (XComposeStatus *)&cmp
				   );

		if( keysym == XK_Shift_L || keysym == XK_Shift_R )
			X11shift = 0;
		if( keysym == XK_Control_L || keysym == XK_Control_R )
			X11ctrl  = 0;
		if( keysym == XK_Meta_L )
			X11meta	 = 0;

		break;

	case EnterNotify:
		X11cshow( T );
		break;

	case LeaveNotify:
		X11cshow( NIL );
		break;

	case KeyPress:
		nbkeys = XLookupString(
					(XKeyEvent *)&event.xkey,
					buf,
					KEYSIZE,
					(KeySym *)&keysym,
					(XComposeStatus *)&cmp
				      );

		if( keysym == XK_Shift_L || keysym == XK_Shift_R )
			X11shift = 1;

		if( keysym == XK_Control_L || keysym == XK_Control_R )
			X11ctrl = 1;

		if( keysym == XK_Meta_L )
			X11meta = 1;

		if( (keysym >= XK_Shift_L && keysym <= XK_Hyper_R) ||
		    (keysym == XK_Mode_switch) )
			break;  

		switch( keysym ) {
		case XK_Down:		X11char = Ctrl|'N';	break;
		case XK_Up:		X11char = Ctrl|'P';	break;
		case XK_Right:		if( X11ctrl )
						X11char = META|'F';
					else	X11char = Ctrl|'F';
					break;
		case XK_Left:		if( X11ctrl )
						X11char = META|'B';
					else	X11char = Ctrl|'B';
					break;
		case XK_Home:		if( X11ctrl )
						X11char = CTLX|Ctrl|'P';
					else	X11char = META|'<';
					break;
		case XK_End:		if( X11ctrl )
						X11char = CTLX|Ctrl|'N';
					else	X11char = META|'>';
					break;
		case XK_Insert:		X11char = META|'I';	break;
#if	defined( sun ) || defined( __linux__ ) || defined( ultrix )
		case XK_Delete:		X11char = Ctrl|'H';	break;
#else
		case XK_Delete:		X11char = Ctrl|'D';	break;
#endif
		case XK_Prior:		X11char = META|'V';	break;
		case XK_Next:		X11char = Ctrl|'V';	break;
		case XK_F1:		X11char = META|'?';	break;
		case XK_F2:		X11char = Ctrl|'S';	break;
		case XK_F3:		X11char = META|Ctrl|'F';break;
		case XK_F4:		X11char = CTLX|Ctrl|'I';break;
		case XK_F5:		X11char = METACH;	break;
		case XK_F6:		X11char = CTLX|METACH;	break;
		case XK_F7:		X11char = CTLX|'`';	break;
		case XK_F8:		X11char = CTLX|Ctrl|'S';break;
		case XK_F9:		X11char = CTLX|'E';	break;
		case XK_F11:		X11char = META|'M';	break;
		case XK_space:		X11char = (X11ctrl? (Ctrl|'@') :' ');
					break;
		default:
#if	defined( AIX )
			key	= (char *)XLookupMapping( &event.xkey, &cmp );
			X11char = (int)(*key & MAX_EMCHAR);
#else
			X11char = (int)(*buf & MAX_EMCHAR);
#endif

			if( X11debug )
			    (void)printf(
					  "nbkey=%d, key=%04x, mcs=%d%d%d\n\r",
					  nbkeys,
					  (int)keysym,
					  X11meta,
					  X11ctrl,
					  X11shift
					);

			if( nbkeys == 0 ) {
				int keychar = (int)keysym;

				/*
				 * No characters in buffer, use keysym
				 */

				if( X11ctrl ) {
					if( islower( keychar ) )
						X11char = toupper( keychar );
					X11char -= '@';
				} else	X11char  = keychar;

				if( X11debug )
				    (void)printf( "X11char = %x\n\r", X11char );
			}

			if( X11meta ) {
				if( isalpha( X11char ) )
				      X11char = (int)(META|toupper( X11char ));
				else  X11char = (int)(META|X11char);
			}
		}
		break;

	default:
		/*
		 *	Ignore all other events
		 */
		break;
	}

	return( event.type );
}

static	void
X11putc( int c )
{
	unsigned  char	ascii = (unsigned char)c;

	switch( ascii ) {

	case '\n' :
		if( X11row < TTYnrow ) {
			X11row++;
			X11col = 0;
		}
		break;

	case '\r' :
		X11col = 0;
		break;

	case '\b' :
		if( X11col > 0 )
			X11col--;
		break;

	default:
		XDrawImageString(
				  X11dpy,
				  X11win,
				  X11gcstd,
				  X11col++ * X11wfnt,
				  X11row   * X11hfnt + X11fntinfo->ascent,
				  (char *)&ascii,
				  1
				);
	}
}

static	void
X11puts( EMCHAR *s, int n )
{
	char ascii[ MAXLINESIZE ];
	emutoa( s, ascii, MAXLINESIZE );

	XDrawImageString(
			  X11dpy,
			  X11win,
			  X11gcstd,
			  X11col * X11wfnt,
			  X11row * X11hfnt + X11fntinfo->ascent,
			  ascii,
			  n
			);
	X11col += n;
}

static	void
X11ei( void )
{
	XSetForeground( X11dpy, X11gcstd, FOREGROUND );
	XSetBackground( X11dpy, X11gcstd, BACKGROUND );
}

static	void
X11si( void )
{
	XSetForeground( X11dpy, X11gcstd, BACKGROUND );
	XSetBackground( X11dpy, X11gcstd, FOREGROUND );
}

static	void
X11beep( void )
{
	/*
	 * Volume range from -100 to 100
	 */

	XBell( X11dpy, -75 );
}

static	void
X11flush( void )
{
	XFlush( X11dpy );
}

static	void
X11cursor( int flag )
{
	char ascii = (char )curchar;

	XDrawImageString(
			  X11dpy,
			  X11win,
			  (flag ? X11gcinv : X11gcstd),
			  X11col * X11wfnt,
			  X11row * X11hfnt + X11fntinfo->ascent,
			  &ascii,
			  1
			);

}

static	void
X11cshow( int flag )
{
	X11cursor( curflag = flag );
	XSync( X11dpy, False );
}

static	int
X11check( void )
{
	X11char = 0;

	XSync( X11dpy, False );
	while( XEventsQueued( X11dpy, QueuedAlready ) ) {
		(void)X11event();
		if( X11char == 0x07 )
			return( T );
	}

	return( NIL );

}

static	void
X11rawmode( void )
{
}

static	void
X11clipcopy( void )
{
#if	defined( XlibSpecificationRelease ) && (XlibSpecificationRelease > 4)
	char *ascii = (char *)kbufp;
	/*
	 * Copy kill-buffer to Clipboard.
	 */

	if( kbufp && kused )
		XStoreBytes( X11dpy, ascii, (int)kused );
#endif
}

static	void
X11clippaste( void )
{
#if	defined( XlibSpecificationRelease ) && (XlibSpecificationRelease > 4)
	int	len;
	int	i;
	char	*buf = XFetchBytes( X11dpy, &len );

	/*
	 * Copy Clipboard to kill-buffer.
	 */

	kused = 0;

	for( i = 0 ; i < len ; i++ )
		(void)kinsert( (int)buf[ i ] );

	XFree( buf );
#endif
}

static	EMCHAR *
X11title( EMCHAR *buf, EMCHAR *fname )
{
	static	EMCHAR	title[ 64 ] = { 0 };

	(void)fname;

	if( emstrcmp( buf, title ) != 0 ) {
		char	ascii[ 64 ];
		int	i;
		(void)emsprintf3(title,ECSTR("%s (%dx%d)"),buf,TTYnrow,TTYncol);
		for( i = 0 ; title[i] ; ++i )
			ascii[ i ] = (char)title[ i ];
		ascii[ i ] = 0;
		XStoreName( X11dpy, X11win, ascii ); 
	}

	return( buf );
}

static	void
X11movecursor( int x, int y )
{
	XWarpPointer(
		      X11dpy,
		      X11win,
		      X11win,
		      None,
		      None,
		      None,
		      None,
		      x * X11wfnt,
		      y * X11hfnt + X11fntinfo->ascent
		    );
}

int
X11emacs( int argc, char *argv[] )
{
	int	arg;
	int	i;
	EMCHAR	**arglist;
	int	argcount;
	int	res;

	aargv = argv;

	/*
	 *	First, read from command line the X values.
	 */

	for( arg = 1 ; arg < argc ; arg++ ) {
		if( strcmp( argv[ arg ], "-bg" )	 == 0 ||
		    strcmp( argv[ arg ], "-background" ) == 0 ) {
			X11background	= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-fg" )	 == 0 ||
		    strcmp( argv[ arg ], "-foreground" ) == 0 ) {
			X11foreground	= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-fn" )   == 0 ||
		    strcmp( argv[ arg ], "-font" ) == 0 ) {
			X11font		= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-bd" )     == 0 ||
		    strcmp( argv[ arg ], "-border" ) == 0 ) {
			X11border	= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-t" )        == 0 ||
		    strcmp( argv[ arg ], "-terminal" ) == 0 ||
		    strcmp( argv[ arg ], "-nw" )       == 0 ) {
			X11term         = 0;
			continue;
		}
		if( strcmp( argv[ arg ], "-geometry" ) == 0 ) {
			X11geometry	= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-display" ) == 0 ) {
			X11display	= argv[ ++arg ];
			continue;
		}
		if( strcmp( argv[ arg ], "-debug" ) == 0 ) {
			X11debug	= 1;
			continue;
		}
		if( *argv[ arg ] == '-' ) {
			(void)printf( "Usage: %s\n", argv[ 0 ] );
			(void)printf( "\t[-background <background-color>]\n" );
			(void)printf( "\t[-border     <border-color>]\n"     );
			(void)printf( "\t[-display    <disaply-name>]\n"     );
			(void)printf( "\t[-foreground <foreground-color>]\n" );
			(void)printf( "\t[-font       <font-name>]\n"        );
			(void)printf( "\t[-geometry   <initial-position>]\n" );
			(void)printf( "\t[-terminal]\n"                      );
			(void)printf( "\t[-debug]\n"	                     );
			return( 1 );
		}
		break;
	}

	argcount = (argc-arg)+1;
	arglist  = (EMCHAR **)malloc( sizeof(EMCHAR *) * (size_t)(argcount+1) );

	/*
	 *	copy program name
	 */

	arglist[0] = emnewstring( argv[0] );

	/*
	 *	copy arg list
	 */

	for( i = 1 ; i < argcount ; ++i )
		arglist[i] = emnewstring( argv[arg++] );
	arglist[ i ] = NULL;
		
	/*
	 *	call the real entry point
	 */

	res = emacs( argcount, arglist );

	for( i = 0 ; i < argcount ; ++i )
		free( arglist[i] );
	free( arglist );

	return( res );
}

#endif
