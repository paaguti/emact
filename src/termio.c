#if	!defined( lint )
static	char rcsid[] = "$Id: termio.c,v 1.3 2008/03/16 06:37:27 jullien Exp $";
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
 *	The  functions  in  this  file  negotiate  with the operating
 *	system  for  characters,  and  write  characters  in a barely
 *	buffered fashion on the display. All operating systems.
 */

#include	"emacs.h"

#if defined(_ANSITERM)||defined(_VT52)||defined(_TERMCAP)||defined(_TERMINFO)

#if	defined( _VMS )

#define NIBUF	128			/* Input buffer size		*/
#define NOBUF	1024			/* Output buffer size		*/
#define EFN	0			/* Event flag			*/

static	char	obuf[NOBUF];		/* Output buffer		*/
static	int	nobuf;			/* # of bytes in above		*/
static	char	ibuf[NIBUF];		/* Input buffer			*/
static	int	nibuf;			/* # of bytes in above		*/
static	int	ibufi;			/* Read index			*/
static	int	oldmode[2];		/* Old TTY mode bits		*/
static	int	newmode[2];		/* New TTY mode bits		*/
static	short	iochan;			/* TTY I/O channel		*/

#endif

#if	defined( _UNIX )
#if	defined( _POSIX_SOURCE ) || defined( HAVE_TERMIOS_H )
#include <termios.h>
static struct termios	ostate;
static struct termios	nstate;
#else
#include <sgtty.h>
static	struct	sgttyb	ostate;
static	struct	sgttyb	nstate;
#endif
#endif

/*
 *	This  function  is  called once to set up the terminal device
 *	streams.  On VMS,  it translates SYS$INPUT until it finds the
 *	terminal, then assigns a channel to it and sets it raw.
 */

void
ttopen()
{
#if	defined( _VMS )
	struct	dsc$descriptor	idsc;
	struct	dsc$descriptor	odsc;
	char	oname[40];
	int	iosb[2];
	int	status;

	odsc.dsc$a_pointer = "SYS$INPUT";
	odsc.dsc$w_length  = strlen( odsc.dsc$a_pointer );
	odsc.dsc$b_dtype   = DSC$K_DTYPE_T;
	odsc.dsc$b_class   = DSC$K_CLASS_S;
	idsc.dsc$b_dtype   = DSC$K_DTYPE_T;
	idsc.dsc$b_class   = DSC$K_CLASS_S;

	do	{
		idsc.dsc$a_pointer = odsc.dsc$a_pointer;
		idsc.dsc$w_length  = odsc.dsc$w_length;
		odsc.dsc$a_pointer = &oname[0];
		odsc.dsc$w_length  = sizeof( oname );
		status = LIB$SYS_TRNLOG( &idsc, &odsc.dsc$w_length, &odsc );
		if( status != SS$_NORMAL && status != SS$_NOTRAN )
			exit( status );
		if( oname[ 0 ] == 0x1B ) {
			odsc.dsc$a_pointer += 4;
			odsc.dsc$w_length  -= 4;
		}
	} while( status == SS$_NORMAL );

	status = SYS$ASSIGN( &odsc, &iochan, 0, 0 );
	if( status != SS$_NORMAL )
		exit( status );
	status = SYS$QIOW(
			   EFN,
			   iochan,
			   IO$_SENSEMODE,
			   iosb,
			   0,
			   0,
			   oldmode,
			   sizeof( oldmode ),
			   0,
			   0,
			   0,
			   0
			 );
	if( status != SS$_NORMAL || (iosb[0]&0xFFFF) != SS$_NORMAL )
		exit( status );
	newmode[0] = oldmode[0];
	newmode[1] = oldmode[1] | TT$M_PASSALL | TT$M_NOECHO;
	status = SYS$QIOW(
			   EFN,
			   iochan,
			   IO$_SETMODE,
			   iosb,
			   0,
			   0,
			   newmode,
			   sizeof( newmode ),
			   0,
			   0,
			   0,
			   0
			 );
	if( status!=SS$_NORMAL || (iosb[0]&0xFFFF)!=SS$_NORMAL )
		exit( status );
#endif
#if	defined( _UNIX )
#if	defined( _POSIX_SOURCE ) || defined( HAVE_TERMIOS_H )
	(void)tcgetattr( 1, &ostate );
	(void)tcgetattr( 1, &nstate );
	nstate.c_cc[ VMIN  ] = 6;
	nstate.c_cc[ VTIME ] = 1;
	nstate.c_iflag &= ~(IGNBRK|INLCR|ICRNL|IXON|IXOFF);
	nstate.c_iflag |= BRKINT;
	nstate.c_oflag &= ~(OPOST);
	nstate.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
	nstate.c_lflag |= ISIG;
	(void)tcsetattr( 1, TCSANOW, &nstate );
#else
	(void)gtty( 1, &ostate );		/* save old state	 */
	(void)gtty( 1, &nstate );		/* get base of new state */
	nstate.sg_flags |= RAW|CBREAK;		/* set raw mode		 */
	nstate.sg_flags &= ~(ECHO|CRMOD);	/* no echo for now...	 */
	(void)stty( 1, &nstate );		/* set mode		 */
#endif
#endif
}

/*
 *	This  function gets called just before we go back home to the
 *	command  interpreter.  On  VMS it puts the terminal back in a
 *	reasonable state.
 */

void
ttclose()
{
#if	defined( _VMS )
	int	status;
	int	iosb[1];

	ttflush();
	status = SYS$QIOW(
			   EFN,
			   iochan,
			   IO$_SETMODE,
			   iosb,
			   0,
			   0,
			   oldmode,
			   sizeof( oldmode ),
			   0,
			   0,
			   0,
			   0
			 );
	if( status != SS$_NORMAL || (iosb[0]&0xFFFF) != SS$_NORMAL )
		exit( status );
	status = SYS$DASSGN( iochan );
	if( status != SS$_NORMAL )
		exit( status );
#endif
#if	defined( _UNIX )
#if	defined( _POSIX_SOURCE ) || defined( HAVE_TERMIOS_H )
	(void)tcsetattr( 1, TCSANOW, &ostate );
#else
	(void)stty( 1, &ostate );
#endif
#endif
}

/*
 *	Write characters to the display.  On VMS,  terminal output is
 *	buffered,  and  we  just put the characters in the big array,
 *	after cheching for overflow.
 */

void
ttputc( int c )
{
#if	defined( _VMS )
	if( nobuf >= NOBUF )
		ttflush();
	obuf[ nobuf++ ] = c;
#else
	(void)fputc( c, stdout );
#endif
}

void
ttputs( EMCHAR *s, int n )
{
	while( n-- )
		TTYputc( (int)*s++ );
}

/*
 *	Show or hide the logical cursor.  On machines with a hardware
 *	cursor nothing is done.
 */

void
ttcshow( int flag )
{
	curflag = flag;
	ttflush();
}

/*
 *	Flush  terminal  buffer.  Does  real  work where the terminal
 *	output  is buffered up.  A no-operation on systems where byte
 *	at a time terminal I/O is done.
 */

void
ttflush()
{
#if	defined( _VMS )
	int	status;
	int	iosb[2];

	status = SS$_NORMAL;
	if( nobuf != 0 ) {
		status = SYS$QIOW(
				   EFN,
				   iochan,
				   IO$_WRITELBLK|IO$M_NOFORMAT,
				   iosb,
				   0,
				   0,
				   obuf,
				   nobuf,
				   0,
				   0,
				   0,
				   0
				 );
		if( status == SS$_NORMAL )
			status = iosb[0] & 0xFFFF;
		nobuf = 0;
	}
#endif
#if	defined( _UNIX ) || defined( _ANSITERM )
	(void)fflush( stdout );
#endif
}

/*
 *	Read  a  character  from the terminal,  performing no editing
 *	and  doing  no  echo at all.  More complex in VMS that almost
 *	anyplace else, which figures.
 */

int
ttgetc()
{
#if	defined( _VMS )
	int	status;
	int	iosb[2];
	int	term[2];

	while( ibufi >= nibuf ) {
		ibufi = 0;
		term[0] = 0;
		term[1] = 0;
		status = SYS$QIOW(
				   EFN,
				   iochan,
				   IO$_READLBLK|IO$M_TIMED,
				   iosb,
				   0,
				   0,
				   ibuf,
				   NIBUF,
				   0,
				   term,
				   0,
				   0
				 );
		if( status != SS$_NORMAL )
			exit( status );
		status = iosb[0] & 0xFFFF;
		if( status != SS$_NORMAL && status != SS$_TIMEOUT )
			exit( status );
		nibuf = (iosb[0]>>16) + (iosb[1]>>16);

		if( nibuf == 0 ) {
			status = SYS$QIOW(
					   EFN,
					   iochan,
					   IO$_READLBLK,
					   iosb,
					   0,
					   0,
					   ibuf,
					   1,
					   0,
					   term,
					   0,
					   0
					 );
			if( status != SS$_NORMAL
			|| (status = (iosb[0]&0xFFFF)) != SS$_NORMAL )
				exit( status );
			nibuf = (iosb[0]>>16) + (iosb[1]>>16);
		}
	}
	return( ibuf[ibufi++] & MAX_EMCHAR );	/* Allow multinational	*/
#else
	return( fgetc( stdin ) & MAX_EMCHAR );
#endif	/* _VMS */
}

#endif
