#if	!defined( lint )
static	char *sccsid = "@(#)fnmatch.c	(c) C. Jullien 2009/05/02";
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
 *	fnmatch.c :
 */

#include <errno.h>
#include <ctype.h>
#include "fnmatch.h"

#define fold(c)	 ((char)((flags&FNM_CASEFOLD) && isupper((int)c) ? \
			 tolower((int)c) \
			 : (c)))

#if	defined( _DOS ) || defined( _NT )
#define	slashp(c) (((c) == '/') || ((c) == '\\'))
#else
#define	slashp(c) ((c) == '/')
#endif

/*
 * Match STRING against the filename pattern PATTERN, returning zero if
 * it matches, nonzero if not.
 */

int
fnmatch( pattern, string, flags )
const char *pattern;
const char *string;
int	    flags;
{
	const char *p = pattern;
	const char *n = string;
	char	    c;

#if	defined( _DOS ) || defined( _NT )
	flags |= FNM_CASEFOLD | FNM_NOESCAPE;
#endif

	while( (c = *p++) != '\0' ) {
		c = fold( c );
		switch( c ) {
		case '?':
			if( *n == '\0' )
				return( FNM_NOMATCH );
			else if( (flags & FNM_PATHNAME) && slashp( *n ) )
				return( FNM_NOMATCH );
			else if( (flags & FNM_PERIOD) && *n == '.' &&
				 (n == string ||
				  ((flags & FNM_PATHNAME) && slashp( n[-1] ))) )
					return( FNM_NOMATCH );
				break;

		case '\\':
			if( !(flags & FNM_NOESCAPE) ) {
				c = *p++;
				c = fold( c );
			}
			if( fold( *n ) != c )
				return( FNM_NOMATCH );
			break;

		case '*': {
			char	c1;

			if( (flags & FNM_PERIOD) && *n == '.' &&
			    (n == string ||
			     ((flags & FNM_PATHNAME) && slashp( n[-1] ))) )
				return( FNM_NOMATCH );

			for( c = *p++; c == '?' || c == '*'; c = *p++, ++n )
				if( ((flags & FNM_PATHNAME) && slashp( *n )) ||
				    (c == '?' && *n == '\0') )
					return( FNM_NOMATCH );

			if( c == '\0' )
				return( 0 );

			c1 = (!(flags & FNM_NOESCAPE) && c == '\\') ? *p : c;
			c1 = fold( c1 );
			for( --p; *n != '\0'; ++n )
				if( (c == '[' || fold( *n ) == c1) &&
				    fnmatch( p, n, flags & ~FNM_PERIOD) == 0 )
				    return( 0 );
			return( FNM_NOMATCH );
			}

		case '[': {
			/*
			 * Nonzero if the sense of the character class 
			 * is inverted.
			 */
			int	not;

			if( *n == '\0' )
				return( FNM_NOMATCH );

			if( (flags & FNM_PERIOD) && *n == '.' &&
			    (n == string ||
			     ((flags & FNM_PATHNAME) && slashp( n[-1] ))) )
			   	return( FNM_NOMATCH );

			not = (*p == '!' || *p == '^');
			if( not )
				++p;

			c = *p++;
			for( ;; ) {
				char	cstart	= c;
				char	cend;

				if( !(flags & FNM_NOESCAPE) && c == '\\' )
					cstart = cend = *p++;

				cstart = cend = fold( cstart );

				if( c == '\0' )
					/* [ (unterminated) loses.  */
					return( FNM_NOMATCH );

				c = *p++;
				c = fold( c );

				if( (flags & FNM_PATHNAME) && slashp(c) )
					/* [/] can never match.  */
					return( FNM_NOMATCH );

				if( c == '-' && *p != ']' ) {
					cend = *p++;
					if( !(flags&FNM_NOESCAPE) && cend=='\\')
						cend = *p++;
					if( cend == '\0' )
						return( FNM_NOMATCH );
					cend = fold( cend );

					c = *p++;
				}

				if( fold( *n ) >= cstart && fold( *n ) <= cend )
					goto matched;

				if( c == ']' )
					break;
			}
			if( !not )
				return( FNM_NOMATCH );
			break;

matched:
			/* Skip the rest of the [...] that already matched.  */
			while( c != ']' ) {
				if( c == '\0' )
					/* [... (unterminated) loses.  */
					return( FNM_NOMATCH );

				c = *p++;
				if( !(flags & FNM_NOESCAPE) && c == '\\' )
					/* P1003.1a is unclear if this is ok */
					++p;
			}
			if( not )
				return( FNM_NOMATCH );
			}
			break;

		default:
			if( c != fold( *n ) )
				return( FNM_NOMATCH );
		}

		++n;
	}

	if( *n == '\0' )
		return( 0 );
	else	return( FNM_NOMATCH );
}
