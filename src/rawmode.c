#if	!defined( lint ) && defined( _ANSITERM )
static	char rcsid[] = "$Id: rawmode.c,v 1.2 2006/05/02 10:36:38 jullien Exp $";
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
 *  Routines  to  set and reset raw mode on stdin/stdout DOS.  Thanks
 *  to Mark Zbikowski
 *
 *  Use with buffered I/O with call :
 *
 *	char outbuf[ BUFSIZ ];
 *	setbuf( stdout, outbuf );
 */

#include "emacs.h"

#if	defined( _DOS ) && defined( _ANSITERM )

#include <dos.h>

#define DEVICE		0x80
#define RAW		0x20
#define IOCTL		0x44

#define STDIN		fileno( stdin )
#define STDOUT		fileno( stdout )

#define GETBITS		0
#define SETBITS		1

static	unsigned	ostdin;
static	unsigned	ostdout;
static	unsigned	obreak;

static	unsigned	ioctl( unsigned handle, unsigned mode, unsigned val );
static	unsigned	breakctl( unsigned mode, unsigned value );

#define BREAKCHECK	0x33

/*
 *	Call  this  to  set  raw  mode; Not only sets raw mode, but also
 *	turns off ^C trapping on random DOS calls.  All character  input
 *	from  stdin should be done with DOS fnt call 7 to avoid ^C input.
 */

void
setraw()
{
	if( (ostdin = ioctl( STDIN, GETBITS, 0 )) & DEVICE )
		(void)ioctl( STDIN, SETBITS, ostdin | RAW );

	if( (ostdout = ioctl( STDOUT, GETBITS, 0 )) & DEVICE )
		(void)ioctl( STDOUT, SETBITS, ostdout | RAW );

	obreak	= breakctl( GETBITS, 0 );
	(void)breakctl( SETBITS, 0 );
}

void
restraw()
{
	if( ostdin )
		(void)ioctl( STDIN, SETBITS, ostdin );
	if( ostdout )
		(void)ioctl( STDOUT, SETBITS, ostdout );
	if( obreak )
		(void)breakctl( SETBITS, obreak );
}

static	unsigned
ioctl( unsigned handle, unsigned mode, unsigned value )
{
	union REGS regs;

	regs.h.ah = IOCTL;
	regs.h.al = mode;
	regs.x.bx = handle;
	regs.h.dl = value;
	regs.h.dh = 0;
	return( regs.x.dx );
	intdos( &regs, &regs );
}

static	unsigned
breakctl( unsigned mode, unsigned value )
{
	union REGS regs;

	regs.h.ah = BREAKCHECK;
	regs.h.al = mode;
	regs.h.dl = value;
	intdos( &regs, &regs );
	return( regs.x.dx & 0xff );
}

#endif
