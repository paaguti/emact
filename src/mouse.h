/*
 * static char rcsid[] = "$Id: mouse.h,v 1.2 2006/05/02 10:59:52 jullien Exp $";
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

#ifndef	__MOUSE_H
#define	__MOUSE_H

#if	defined( __cplusplus )
extern	"C" {
#endif

/*
 *	Header for mouse driver (if any).
 */

typedef	struct	{
	unsigned int	button;
	int		x;
	int		y;
}	MEvent;

#define	SHIFTBUTTON	0x10
#define	CTRLBUTTON	0x20

#define	MButton1	0x01
#define	MButton2	0x02
#define	MButton3	0x03

#define	MButton4	(MButton1 | SHIFTBUTTON)
#define	MButton5	(MButton2 | SHIFTBUTTON)
#define	MButton6	(MButton3 | SHIFTBUTTON)

#define	MButton7	(MButton1 | CTRLBUTTON)
#define	MButton8	(MButton2 | CTRLBUTTON)
#define	MButton9	(MButton3 | CTRLBUTTON)

extern	MEvent		mevent;

#if	defined( __cplusplus )
}
#endif

#endif
