/*
 * static auto rcsid("$Id: version.h,v 1.4 2018/08/15 05:46:17 jullien Exp $");
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

#if !defined( __VERSION_H )
#define __VERSION_H

/*
 * Version.h :
 *
 * Set  the  revision  number  of  the  current  EmACT  version.
 * Please, never change this variable yourself.
 */

#define EMACS_MAJOR        "3"
#define EMACS_MINOR        "00"
#define EMACS_RELEASE      "0"
#define EMACS_VERSION      "3.00.0"
#define EMACS_COPYRIGHT    "Copyright (c) 1985 - 2019 Eligis"
#define EMACS_FULLVERSION  "Version 3.00.0"

#define EMFILEVERSION      "3, 00, 0, 0\0"
#define EMPRODUCTVERSION   "3.00.0\0"
#define EMCOPYRIGHT        "Copyright (c) 1985 - 2019 Eligis\0"
#define EMVERSIONINFO      3,00,0,0

#define VERSION ECSTR("EmACT: v3.00.0")

#endif
