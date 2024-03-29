#if	!defined( lint )
static	char rcsid[] = "$Id: version.c,v 1.2 2006/05/02 10:36:39 jullien Exp $";
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
 *	version.c :
 *
 *	Set  the  version  name,  the revision number and the licence
 *	name  of  the  Emacs  you  are  using.  Please,	never  change
 *	those variables yourself.
 */

#include	"version.h"
#include	"emacs.h"

EMCHAR	*licence = ECSTR("Christian Jullien");
EMCHAR	*version = VERSION;
