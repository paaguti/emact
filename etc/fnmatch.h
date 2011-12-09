#if	!defined( __FNMATCH_H )
#define	__FNMATCH_H

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

#if	defined( __cplusplus )
extern "C" {
#endif

/*
 * Bits set in the FLAGS argument to `fnmatch'.
 */

#define	FNM_PATHNAME	(1 << 0) /* No wildcard can ever match `/'.	    */
#define	FNM_NOESCAPE	(1 << 1) /* Backslashes don't quote special chars.  */
#define	FNM_PERIOD	(1 << 2) /* Leading `.' is matched only explicitly. */

#if	!defined( _POSIX_C_SOURCE ) || _POSIX_C_SOURCE < 2
#define	FNM_CASEFOLD	(1 << 3) /* Compare without regard to case.	    */
#endif

/*
 * Value returned by `fnmatch' if STRING does not match PATTERN.
 */

#define	FNM_NOMATCH	1

/*
 * Match STRING against the filename pattern PATTERN,  returning zero
 * if it matches, FNM_NOMATCH if not.
 */

extern int fnmatch( const char *pattern, const char *string, int flags );

#if	defined( __cplusplus )
}
#endif

#endif /* _FNMATCH_H */
