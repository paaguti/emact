#if	!defined( lint )
static	char rcsid[] = "$Id: search.c,v 1.6 2012/10/21 12:21:49 jullien Exp $";
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
 *	The  functions in this file implement commands that search in
 *	the  forward and backward directions.  There are  no  special
 *	characters in the search strings.
 */

#include	"emacs.h"

static	EMCHAR	 *NOMATCH	= ECSTR("No match.");
static	int	 upline		= 0;

#define	QRYGNU	ECSTR("Help: ' ' yes, DEL no, '.' change and exit, '!' all, ESC quit.")

extern	int	commento;
extern	int	indento;
extern	EDLINE	*indentp;

EDLINE	*found_p;
int	found_o;

static  int	_define(replace,(int prompt));
static  int	_define(quotep,(struct EDLINE *l, int i));
static  int	_define(instringp,(struct EDLINE *clp, int cbo));
static  void	_define(mlmatch,(struct EDLINE *clp, int cbo));
static	int	_define(cmpchars,(int bc, int pc));
static	int	_define(readpattern,(EMCHAR *prompt));
static	void	_define(saveindent,(struct EDLINE *clp, int cbo));
static	int	_define(bfindstring,(void));

/*
 *	Internal search forward. The string to be search is in 'pat'.
 *	This function, set global variable found_p and found_o to the
 *	the position of next match.
 */

int
ffindstring( void )
{
	EDLINE	*clp;
	int	cbo;
	EDLINE	*tlp;
	int	tbo;
	EMCHAR	c;
	EMCHAR	*s;

	clp = curwp->w_dotp;
	cbo = curwp->w_doto;

	while( clp != lastline( curbp ) ) {
		if( cbo == llength( clp ) ) {
			clp = lforw( clp );
			cbo = 0;
			c   = '\n';
		} else	c = lgetc( clp, cbo++ );

		if( cmpchars( (EMCHAR)c, search_buffer[ 0 ] ) != NIL ) {
			tlp = clp;
			tbo = cbo;
			for( s = &search_buffer[ 1 ] ; *s != 0 ; s++ ) {
				if( tlp == lastline( curbp ) )
					return( NIL );
				if( tbo == llength( tlp ) ) {
					tlp = lforw( tlp );
					tbo = 0;
					c   = '\n';
				} else	c = lgetc( tlp, tbo++ );
				if( cmpchars( c, *s ) == NIL )
					break;
			}
			if( *s == 0 ) {
				found_p  = tlp;
				found_o  = tbo;
				return( T );
			}
		}
	}

	return( NIL );
}

/*
 *	Internal search backward. The string to be search is in 'pat'.
 *	This function, set global variable found_p and found_o to the
 *	the position of next match.
 */

static int
bfindstring( void )
{
	EDLINE	*clp;
	int	cbo;
	EDLINE	*tlp;
	int	tbo;
	EMCHAR	c;
	EMCHAR	*epp;
	EMCHAR	*pp;

	for( epp = &search_buffer[ 0 ] ; epp[1] != 0 ; ++epp )
		;

	clp = curwp->w_dotp;
	cbo = curwp->w_doto;

	for( ;; ) {
		if( cbo == 0 ) {
			clp = lback( clp );
			if( clp == lastline( curbp ) )
				return( NIL );
			cbo = llength( clp ) + 1;
		}

		if( --cbo == llength( clp ) )
			c = '\n';
		else	c = lgetc( clp, cbo );

		if( cmpchars(c, *epp) != NIL ) {
			tlp = clp;
			tbo = cbo;

			for( pp = epp ; pp != &search_buffer[ 0 ] ; pp-- ) {
				if( tbo == 0 ) {
					tlp = lback( tlp );
					if( tlp == lastline( curbp ) )
						return( NIL );
					tbo = llength( tlp )+1;
				}
				if( --tbo == llength( tlp ) )
					c = '\n';
				else	c = lgetc( tlp, tbo );
				if( cmpchars( c, *(pp-1) ) == NIL )
					break;
			}
			if( pp == &search_buffer[ 0 ] ) {
				found_p  = tlp;
				found_o  = tbo;
				return( T );
			}
		}
	}
}

/*
 *	Auxiliary function for 'global' and 'query'.
 */

static	int
replace( int prompt )
{
	int	patl;
	int	replaced;
	EDLINE	*clp = firstline( curbp );
	int	cln  = 0;
	int	c    = 0;
	int	cbo;
	EMCHAR	*msgo;
	EMCHAR	*msgn;
	EMCHAR	opat[ NPAT ];
	EMCHAR	npat[ NPAT ];

	if( freadonly() )
		return( NIL );

	/*
	 * compute the line number of curwp->dotp. Since this pointer
	 * may change in a substitution we can't save it as the usual
	 * way.
	 */

 	for( ; clp != curwp->w_dotp ; clp = lforw( clp ) )
		cln++;

	cbo = curwp->w_doto;

	if( prompt == T ) {
		msgo = ECSTR("Query replace old string: ");
		msgn = ECSTR("Query replace new string: ");
	} else	{
		msgo = ECSTR("Replace old string: ");
		msgn = ECSTR("Replace new string: ");
	}

	if( WDGchange( msgo, msgn, (EMCHAR *)opat, (EMCHAR *)npat, NPAT ) != T )
		return( NIL );
	else	(void)emstrcpy( search_buffer, opat );

	patl = emstrlen( opat );

	for( replaced = 0 ; ffindstring() == T && c != 'q' && c != '.' ; ) {
		curwp->w_dotp  = found_p;
		curwp->w_doto  = found_o;
		curwp->w_flag |= WFHARD;

		if( prompt == NIL ) {
			subst( patl, (EMCHAR *)npat );
			replaced++;
		} else	{
			WDGmessage( ECSTR("Query-Replace mode ") );
			update( T );
			for( c = '?' ; c == '?' ; ) {
				switch( c = TTYgetc() ) {
				case '!' :
					subst( patl, (EMCHAR *)npat );
					replaced++;
					update( T );
					prompt = NIL;
					break;
				case ',' :
					subst( patl, (EMCHAR *)npat );
					replaced++;
					update( T );
					prompt = NIL;
					break;
				case 'Y' :
				case 'y' :
				case ' ' :
				case '.' :
					subst( patl, (EMCHAR *)npat );
					replaced++;
					update( T );
					break;
				case 0x07 :	/* ABORT */
					WDGwrite( ECSTR("Quit") );
					c = 'q';
					break;
				case 'Q' :
				case 'q' :
				case 0x1B: /* ESC */
					c = 'q';
					break;
				case 'N'  :
				case 'n'  :
				case 0x08 :
					break;
				default  :
					WDGmessage( QRYGNU );
					c = '?';
				}
			}
		}
	}

 	for( clp = firstline( curbp ) ; cln-- ; )
		clp = lforw( clp );

	curwp->w_dotp  = clp;
	curwp->w_doto  = cbo;
	curwp->w_flag |= WFHARD;

	if( c == 'q' )
		return( NIL );

	WDGwrite( ECSTR("Replaced %d occurence(s)"), replaced );
	return( T );
}

/*
 *	Substitute 'newstr'  string of 'length' characters at current
 *	position in the buffer. Call lchange to ensure that redisplay
 *	is done in.
 */

void
subst( int length, EMCHAR *newstr )
{

	int	i;
	int	owmode;
	int	obmode;

 	owmode		= curwp->w_emode;
 	obmode		= curbp->b_emode;
	curwp->w_emode	= FUNDAMENTAL;
	curbp->b_emode	= FUNDAMENTAL;

	for( i = 0 ; i < length ; i++ )
		(void)backdel();

	for( ; *newstr ; newstr++ )
		if( *newstr == '\n' )
			(void)lnewline();
		else	(void)linsert( 1, *newstr );

	curbp->b_emode = obmode;
	curwp->w_emode = owmode;
	lchange( WFHARD );
}

/*
 *	Compare two characters.  The "bc" comes from the  buffer. It
 *	has it's case folded out. The "pc" is from the pattern.
 */

static	int
cmpchars( int bc, int pc )
{
	if( case_sensitivity == NIL ) {
		if( bc >= 'a' && bc <= 'z' )
			bc -= 0x20;
		if( pc >= 'a' && pc <= 'z' )
			pc -= 0x20;
	}

	if( bc == pc )
		return( T  );
	else	return( NIL );
}

/*
 *	Read a pattern.  Stash it in the external variable "pat". The
 *	"pat"  is not updated if the user types in an empty line.  If
 *	the  user  typed an empty line,  and there is no old pattern,
 *	it  is  an  error.  Display the old pattern,  in the style of
 *	Jeff Lomicka. There is some do-it-yourself control expansion.
 */

static	int
readpattern( EMCHAR *prompt )
{
	complete.flag = 0L;

	return( WDGedit( prompt, search_buffer, NPAT ) );
}

/*
 *	Auxiliary  function  witch  returns  T if "." is not a quoted
 *	character like 'c' in C mode or #/c in Lisp mode.
 */

static	int
quotep( EDLINE *l, int i )
{
	int	c;

	if( i < 1 )
		return( NIL );

	if( (c = lgetc( l, i-1 )) == '\'' &&
	    (curwp->w_emode==CMODE      ||
	     curwp->w_emode==CPPMODE    ||
	     curwp->w_emode==CSHARPMODE ||
	     curwp->w_emode==PERLMODE   ||
	     curwp->w_emode==SHELLMODE  ||
	     curwp->w_emode==JAVAMODE) ) {
		if( i >= 3 && (c = lgetc( l, i-3 )) == '\'' )
			return( NIL );
		else	return( T );
	} else	if( curwp->w_emode==LISPMODE && c=='/' &&
		    i<2 && lgetc(l,i-2)=='#' )
		return( T );

	return( NIL );
}

static	int
instringp( EDLINE *clp, int cbo )
{
	int	dblquote = 0;
	int	i;

	for( i = cbo ; i >= 0 ; i-- )
		if( lgetc( clp, i ) == '"' && !quotep( clp, i ) )
			dblquote = !dblquote;
	return( dblquote );
}

/*
 *	If auto-match  should  change  page  display,   the  matched
 *	character comes in mode-line at the bottom of the screen.
 */

static	void
mlmatch( EDLINE *clp, int cbo )
{
	int	i = 0;
	int	j = 0;
	int	pos   = 0;
	int	count = 0;
	int	maxchar;
	EMCHAR	c;
	EMCHAR	*s;
	EMCHAR	mlline[ NLINE ];

	s	= ltext( clp );
	maxchar	= llength( clp );
	
	while( (c = *s++) != '\000' && (i < TTYncol-1) && (i < maxchar+j) ) {
		if( count++ == cbo )
			pos = i;
		if( c == '\t' ) {
			do	{
				mlline[ i++ ] = ' ';
				j++;
			} while( i % tab_display );
			/*
			 * j  is  only  the count of extra character.
			 * Since \t always print at least one  blank,
			 * we must take count this one and  decrement
			 * j.
			 */
			--j;
		} else	mlline[ i++ ] = c;
	}

	mlline[ i ] = '\0';

	WDGwrite( ECSTR("%s"), mlline );

	if( widget.w_write == mlwrite ) {
		TTYmove( TTYnrow, pos );
		TTYflush();
	}
}

int
rmatchc( int patc, int printflag )
{
	EDLINE	*clp;
	int	cbo;
	int	nbmatch	= 0;
	int	lisp	= (curwp->w_emode == LISPMODE);
	EMCHAR	matchpat;
	int	max;
	int	c;

	switch( patc ) {
	case '(' : matchpat = ')'; break;
	case '{' : matchpat = '}'; break;
	case '[' : matchpat = ']'; break;
	default	 : return( NIL );
	}

	if( lgetdot() == matchpat )
		(void)forwchar();

	cbo = curwp->w_doto;
	clp = curwp->w_dotp;

	if( lisp )
		max = lastlisp( clp ) + 1;
	else	max = llength( clp );

	while( clp != lastline( curbp ) ) {
		if( cbo >= max ) {
			clp = lforw( clp );
			cbo = 0;
			if( lisp )
				max = lastlisp( clp ) + 1;
			else	max = llength( clp );
			c   = '\n';
		} else	c = lgetc( clp, cbo++ );

		if( quotep( clp, cbo-1 ) )
			continue;
		if( c == patc ) {
			nbmatch++;
			continue;
		}
		if( c == matchpat && --nbmatch == 0 ) {
			curwp->w_dotp  = clp;
			curwp->w_doto  = cbo - 1;
			curwp->w_flag |= WFMOVE;
			return( T );
		}
	}

	if( printflag == T )
		WDGmessage( NOMATCH );

	return( NIL );
}

int
lmatchc( int patc, int printflag )
{
	EDLINE	*clp	= curwp->w_dotp;
	int	cbo	= curwp->w_doto;
	int	mode	= curwp->w_emode;
	int	nbmatch	= 0;
	int	strp	= 0;
	int	indx;
	EMCHAR	matchpat;
	int	c;

	commento = 0;
	indx	 = (cbo >= llength( clp )) ? cbo - 1 : cbo;

	if( quotep( clp, indx ) || instringp( clp, indx ) )
		return( NIL );

	switch( patc ) {
	case '>' : matchpat = '<'; break;
	case ')' : matchpat = '('; break;
	case '}' : matchpat = '{'; break;
	case ']' : matchpat = '['; break;
	default	 : return( NIL );
	}

	if( lgetc( clp, indx ) == matchpat )
		(void)backchar();

	cbo = curwp->w_doto;
	clp = curwp->w_dotp;

	if( mode == LISPMODE && (cbo > lastlisp( clp ) + 1) )
		return( NIL );

	for( ;; ) {
	   while( cbo < 0 || 
	          ((mode==CMODE      ||
		    mode==CPPMODE    ||
		    mode==CSHARPMODE ||
		    mode==PERLMODE   ||
		    mode==JAVAMODE) && 
		  commento != 0)) {
			clp = lback( clp );
			upline++;
			if( clp == lastline( curbp ) ) {
				if( printflag == T )
					WDGmessage( NOMATCH );
				return( NIL );
			}
			switch( mode ) {
			case CMODE      :
			case CPPMODE    :
			case CSHARPMODE :
			case PERLMODE   :
			case JAVAMODE   :
			case FORTRANMODE:
			case PROLOGMODE :
			case SHELLMODE  :
				cbo = lastc( clp );
				break;
			case LISPMODE   :
				cbo = lastlisp( clp );
				break;
			default         :
				cbo = llength( clp );
			}
		}

		if( (cbo == llength( clp )) || cbo < 0 ) {
			--cbo;
			continue;
		} else	c = lgetc( clp, cbo-- );

		if( quotep(clp, cbo+1) || (c=='"' && (strp=!strp)!=0) || strp )
			continue;

		if( c == patc ) {
			nbmatch++;
			continue;
		}

		if( c == matchpat && --nbmatch == 0 ) {
			saveindent( clp, ++cbo );
			curwp->w_dotp  = clp;
			curwp->w_doto  = cbo;
			curwp->w_flag |= WFMOVE;
			return( T );
		}
	}
}

/*
 *	Save the current indentation point in (indentp, indento).
 */

static	void
saveindent( EDLINE *clp, int cbo )
{
	indentp = clp;

	switch( curwp->w_emode ) {
	case LISPMODE	: indento = cbo; break;
	case CMODE	:
	case CPPMODE	:
	case CSHARPMODE	:
	case PERLMODE	:
	case JAVAMODE	:
	case FORTRANMODE:
	case SHELLMODE  :
	case PROLOGMODE : indento = leftmargin( clp ); break;
	default		: indento = 0;
	}

}

/*
 *	The function automatch search  backward in the  file to find
 *	a special caracter (in general '(', '[' or '{') depending on
 *	the mode set. The redisplay screen at cursor wait 1/3 second
 *	and go  back to the  previous  position to the corresponding
 *	('}', ']' or ')').
 */

int
automatch( int c, int f )
{
	EDLINE	*clp	= curwp->w_dotp;
	int	cbo	= curwp->w_doto;
	int	crow	= currow - (int)curwp->w_toprow;
	EDLINE	*mlp	= (EDLINE *)NULL;
	int	mbo	= 0;
	int	s;

	upline = 0;
	if( (s = (backchar() && lmatchc( c, f ))) != NIL && f == T ) {
		if( upline <= crow )  {
			update( T );
			TTYcshow( T );
			waitmatch( 1 );
			TTYcshow( NIL );
		} else	{
			mlp = curwp->w_dotp;
			mbo = curwp->w_doto;
		}
	}

	curwp->w_dotp  = clp;
	curwp->w_doto  = cbo;
	curwp->w_flag |= WFMOVE;

	if( mlp != (EDLINE *)NULL ) {
		mlmatch( mlp, mbo );
		waitmatch( 1 );
	}

	return( s ? T : NIL );

}

/*
 *	Wait 1/3 second for every match.
 */

#if	defined( _SC_CLK_TCK )
#define	TIC_SECOND	((unsigned long)sysconf( _SC_CLK_TCK ))
#else
#if	defined( CLOCKS_PER_SEC ) && !defined( CLK_TCK )
#define	CLK_TCK		CLOCKS_PER_SEC
#endif
#define	TIC_SECOND	((unsigned long)CLK_TCK)
#endif

#if	defined( sun )
#define	TEMPO		((TIC_SECOND / 3) * 10000)
#else
#define	TEMPO		(TIC_SECOND / 3)
#endif

void
waitmatch( int n )
{
	clock_t	tm = clock() + (clock_t)TEMPO * n;

	while( clock() < tm )
		WDGwait();
}

/*
 *	Search  forward.  Get  a search string  from  the  user,  and
 *	search, beginning at ".", for the string. If found, reset the
 *	"." to be just after the match string,  and [perhaps] repaint
 *	the display. Bound to "M-S".
 */

CMD
forwsearch( void )
{
	int	s;

	thisflag |= CFFSRC;

	if( !(lastflag & (CFFSRC | CFBSRC)) ) {
		if( (s = readpattern( ECSTR("Search: ") )) != T ) {
			thisflag &= ~CFFSRC;
			return( s );
		}
	} else	WDGwrite( ECSTR("Search \"%s\": "), search_buffer );

	if( lastflag & CFFAIL ) {
		lastflag &= ~CFFAIL;
		(void)gotobob();
		return( forwsearch() );
	}

	if( ffindstring() == T ) {
		curwp->w_dotp  = found_p;
		curwp->w_doto  = found_o;
		curwp->w_flag |= WFMOVE;
		return( T );
	} else	{
		TTYbeep();
		WDGwrite( ECSTR("Failing search: %s"), search_buffer );
		thisflag |= CFFAIL;
		return( NIL );
	}
}

/*
 *	Reverse  search.  Get  a  search string from  the user, and
 *	search,  starting at "." and proceeding toward the front of
 *	the  buffer.  If  found "." is left pointing  at the  first
 *	character of  the pattern  [the  last  character  that  was
 *	matched]. Bound to "M-R".
 */

CMD
backsearch( void )
{
	int	s;

	thisflag |= CFBSRC;

	if( !(lastflag & (CFFSRC | CFBSRC)) ) {
		if( (s = readpattern( ECSTR("Search backward: ") )) != T ) {
			thisflag &= ~CFBSRC;
			return( s );
		}
	} else	WDGwrite( ECSTR("Search backward \"%s\": "), search_buffer );

	if( lastflag & CFFAIL ) {
		lastflag &= ~CFFAIL;
		(void)gotoeob();
		return( backsearch() );
	}

	if( bfindstring() ) {
		curwp->w_dotp  = found_p;
		curwp->w_doto  = found_o;
		curwp->w_flag |= WFMOVE;
		return( T );
	} else	{
		TTYbeep();
		WDGwrite( ECSTR("Failing search backward: %s"), search_buffer );
		thisflag |= CFFAIL;
		return( NIL );
	}

}

/*
 *	Global replace.  Get  two strings (old and new) from the user
 *	and search from the current position all occurences of  'old'
 *	and replace them by 'new'. Bound to 'M-&'.
 */

CMD
global( void )
{
	return( replace( NIL ) );
}

/*
 *	Query replace.  Get two strings  (old and new)  from the user
 *	and search from the current position all occurences  of 'old'
 *	and replace them by  'new'  according to  the response of the
 *	user until completion or 'q' answer. Bound to 'M-%'.
 */

CMD
query( void )
{
	return( replace( T ) );
}

/*
 *	Find definition.  This  function  search in whole  text for a
 *	specific  definition.  The pattern is different for C or Lisp
 *	mode.  For C mode, a definition is of the form  "pattern"  in
 *	column 0 and of the form "(XXXX pattern" for Lisp mode.
 */

CMD
getdefinition( void )
{
	int	s;
	EDLINE	*clp;
	int	cbo;
	int	len;
	EMCHAR	save[ NPAT ];

	if( curwp->w_emode == FUNDAMENTAL )
		return( NIL );

	(void)emstrcpy( save, search_buffer );
	*search_buffer = '\0';

	if( (s = readpattern( ECSTR("Search-definition: ") )) != T )
		return( s );

	clp		= curwp->w_dotp;
	cbo		= curwp->w_doto;
	len		= emstrlen( search_buffer );
	curwp->w_dotp	= firstline( curbp );
	curwp->w_doto	= 0;

	while( ffindstring() == T ) {
		curwp->w_dotp	= found_p;
		curwp->w_doto	= found_o;
		switch( curwp->w_emode ) {
		case ASMODE	:
		case CMODE	:
		case CPPMODE	:
		case CSHARPMODE	:
		case PERLMODE	:
		case JAVAMODE	:
		case FORTRANMODE:
		case PROLOGMODE :
		case SHELLMODE  :
			if( found_o == len ) {
				curwp->w_flag |= WFMOVE;
				(void)emstrcpy( search_buffer, save );
				(void)backline();
				(void)reposition();
				return( T );
			}
			break;
		case LISPMODE :
			if( backword() == NIL ||
			    backword() == NIL ||
			    backchar() == NIL ||
			    curwp->w_doto != 0 || lgetc(curwp->w_dotp,0)!='(') {
		    		curwp->w_dotp	= found_p;
				curwp->w_doto	= found_o;
			} else	{
				curwp->w_flag  |= WFMOVE;
				(void)emstrcpy( search_buffer, save );
				(void)backline();
				(void)reposition();
				return( T );
				}
			break;
		}
	}

	curwp->w_dotp = clp;
	curwp->w_doto = cbo;
	(void)emstrcpy( search_buffer, save );

	WDGmessage( ECSTR("Not found") );
	return( NIL );

}

/*
 *	Complete the current word with previous token in the previous
 *	text. If the word found is not correct, another ESC/ continue
 *	the search. Bound to ESC-/.
 */

#define	REJECT_HISTORY	32
#define	REJECT_WORDSIZE	32

typedef	CMD (*DIRFNP)( void );

static	struct	{
	EMCHAR	startw[ REJECT_WORDSIZE ];
	int	size;
} reject[ REJECT_HISTORY ];

CMD
completeword( void )
{
	static	int	rejectnb = 0;
	static	int	lasto    = 0;
	static	int	indx	 = 0;
	static	DIRFNP	find	 = bfindstring;

	EDLINE	*clp;
	EMCHAR	buf[ NPAT ];
	EMCHAR	save[ NPAT ];
	int	cbo;
	int	slen;
	int	i;
	int	k;
	int	s;

	thisflag |= CFCPLT;

	if( lastflag & CFCPLT ) {
		/*
		 *	Last command was ESC/, add match to reject history,
		 *	delete the word and continue.
		 */

		if( rejectnb < REJECT_HISTORY ) {
		    for(i=lasto,k=0;i<curwp->w_doto&&k<(REJECT_WORDSIZE-1);i++)
		    	reject[ rejectnb ].startw[k++] = lgetc(curwp->w_dotp,i);
			reject[ rejectnb ].startw[k] = '\000';
			reject[ rejectnb ].size      = k;
			rejectnb++;
		}

		while( curwp->w_doto > lasto )
			/*
			 *	Undo previous completion.
			 */
			backdel();
	} else	{
		/*
		 *	It's  a  new completion,  clear reject buffer
		 *	indx and start with a backward search.
		 */

		find	 = bfindstring;
		rejectnb = 0;
		indx	 = 0;
	}

	clp	= curwp->w_dotp;
	cbo	= curwp->w_doto;
	lasto	= curwp->w_doto;

	/*
	 *	Copy the start of match
	 */

	(void)backword();

	for( slen = 0, i = curwp->w_doto ; i < cbo ; i++ )
		buf[ slen++ ] = lgetc( clp, i );
	buf[ slen ] = 0;

	(void)emstrcpy( save, search_buffer );
	(void)emstrcpy( search_buffer, buf );

loop:
	if( lastflag & CFCPLT ) {
		curwp->w_dotp = found_p;
		curwp->w_doto = found_o;
	}

	while( (s = find()) == T ) {
		/*
		 *	A  match is found,  point at the start of the
		 *	match  to  check  if the matching sequence is
		 *	at the start of a new word.
		 */

		if( find == ffindstring ) {
			/*
			 *	For  forward  search,  make the match
			 *	point  on  the left most character of
			 *	the match.
			 */
			found_o -= slen;
		}
		if( found_o > 0 ) {
			curwp->w_dotp = found_p;
			curwp->w_doto = found_o-1;
			if( inword() ) {
				/* match the middle of a word */
				if( find == ffindstring ) {
					found_o       += slen;
					curwp->w_doto  = found_o;
				}
				continue;
			}
		}
		break;
	}

	if( s == T ) {
		/*
		 *	Set  current  dot  to  point on the left most
		 *	character of the match.
		 */

		curwp->w_dotp = found_p;
		curwp->w_doto = found_o + slen;

		/*
		 *	Copy new characters in a temporary buffer.
		 */

		for( i = 0 ; inword() ; ++i ) {
			buf[ i ] = lgetdot();
			(void)forwchar();
		}

		buf[ i ] = '\000';

		/*
		 *	Search  in  reject  buffers  to  see  if  the
		 *	current match has not already rejected.
		 */

		for( k = 0 ; k < rejectnb ; k++ )
			if( (i == reject[ k ].size) &&
			    emstrcmp( buf, reject[ k ].startw ) == 0 ) {
				if( find == ffindstring ) {
					found_o       += slen;
					curwp->w_doto  = found_o;
				} else	curwp->w_doto  = found_o;
			    	goto loop;
			    }

		/*
		 *	Reset current dot position and add completion
		 */

		curwp->w_dotp = clp;
		curwp->w_doto = cbo;

		for( i = 0 ; buf[ i ] != '\000' ; i++ )
			if( (s = linsert( 1, buf[ i ] )) != T )
				break;

	} else	{
		curwp->w_dotp  = clp;
		curwp->w_doto  = cbo;

		if( find == bfindstring ) {
			/*
			 * Start forward search from dot.
			 */

			found_p = clp;
			found_o = cbo;
			find    = ffindstring;
			goto loop;
		} else	{
			EMCHAR	tagbuf[NLINE];

			indx = completeintag( indx, search_buffer, tagbuf );

			if( indx != 0 ) {
				/*
				 *	Search  in  reject buffers to
				 *	see  if the current match has
				 *	not already rejected.
				 */

				for( k = 0 ; k < rejectnb ; k++ )
				     if( emstrcmp(tagbuf,reject[k].startw)==0 )
					 continue;

				/*
				 *	Reset  current  dot  position
				 *	and add completion
				 */

				curwp->w_dotp = clp;
				curwp->w_doto = cbo;

				for( i = slen ; tagbuf[ i ] != '\000' ; i++ )
					if( (s = linsert( 1, tagbuf[i] )) != T )
						break;

				(void)emstrcpy( search_buffer, save );
				return( T );
			}

			thisflag &= ~CFCPLT;
			if( rejectnb == 0 )
			     WDGwrite(
				ECSTR("No dynamic expansion for '%s' found"),
				search_buffer
			     );
			else WDGwrite(
				ECSTR("No further dynamic expansion for '%s' found"),
				search_buffer
			     );
		}
	}

	(void)emstrcpy( search_buffer, save );
	return( s );
}

CMD
diffwindows( void )
{
	WINSCR	*wp1;
	WINSCR	*wp2;

	wp1 = curwp;

	if( (wp2 = curwp->w_wndp) == NULL ) {
		wp2 = wheadp;
		if( wp2 == wp1 ) {
			WDGmessage( ECSTR("no other window") );
			return( NIL );
		}
	}

	wp1->w_dotp  = firstline( wp1->w_bufp );
	wp1->w_doto  = 0;
	wp1->w_flag |= WFHARD;

	wp2->w_dotp  = firstline( wp2->w_bufp );
	wp2->w_doto  = 0;
	wp2->w_flag |= WFHARD;

	return( comparewindows() );

}

/*
 *	Compare the current window and the next one (if any)  starting
 *	at current position of the two windows. Display the first line
 *	which is different and change the position of the two windows.
 *	This command is not bound.
 */

CMD
comparewindows( void )
{
	EDLINE	*lp1;
	EDLINE	*lp2;
	int	lo1;
	int	lo2;
	BUFFER	*bp1;
	BUFFER	*bp2;
	WINSCR	*wp1;
	WINSCR	*wp2;
	int	flag = NIL;

	wp1 = curwp;

	if( (wp2 = curwp->w_wndp) == NULL ) {
		wp2 = wheadp;
		if( wp2 == wp1 ) {
			WDGmessage( ECSTR("no other window") );
			return( NIL );
		}
	}

	bp1 = wp1->w_bufp;
	lp1 = wp1->w_dotp;
	lo1 = wp1->w_doto;

	bp2 = wp2->w_bufp;
	lp2 = wp2->w_dotp;
	lo2 = wp2->w_doto;

	if( lastflag & CFCOMP ) {
		/*
		 *	continue on next line
		 */

		if( lp1 != lastline( bp1 ) )
			lp1 = lforw( lp1 );
		if( lp2 != lastline( bp2 ) )
			lp2 = lforw( lp2 );
		lo1 = 0;
		lo2 = 0;
	}

	while( (lp1 != lastline( bp1 )) && (lp2 != lastline( bp2 )) ) {
		if((llength( lp1 )-lo1) == (llength( lp2 )-lo2) &&
		   ((llength( lp1 )-lo1) == 0 ||
		   emstrncmp(ltext(lp1)+lo1,
			     ltext(lp2)+lo2,
			     (size_t)(llength(lp1)-lo1))==0)){
		    	lp1 = lforw( lp1 );
			lp2 = lforw( lp2 );
			lo1 = 0;
			lo2 = 0;
		} else	{
			while(
			       (lo1 < llength( lp1 )) &&
			       (lo1 < llength( lp2 )) &&
			       (lgetc( lp1, lo1 ) == lgetc( lp2, lo2)) ) {
			       	lo1++;
			       	lo2++;
			       }
			wp2->w_dotp  = lp2;
			wp2->w_doto  = lo2;
			wp2->w_flag |= WFMOVE;

			wp1->w_dotp  = lp1;
			wp1->w_doto  = lo1;
			wp1->w_flag |= WFMOVE;

			thisflag    |= CFCOMP;
			return( showcpos() );
		}
	}

	if( lp1 != lastline( bp1 ) ) {
		WDGwrite( ECSTR("Line(s) added to %s"), bp1->b_fname );
		flag = T;
	}

	if( lp2 != lastline( bp2 ) ) {
		WDGwrite( ECSTR("Line(s) added to %s"), bp2->b_fname );
		flag = T;
	}

	if( flag == T ) {
		wp2->w_dotp  = lp2;
		wp2->w_doto  = 0;
		wp2->w_flag |= WFMOVE;

		wp1->w_dotp  = lp1;
		wp1->w_doto  = 0;
		wp1->w_flag |= WFMOVE;

		thisflag    |= CFCOMP;
		return( T );
	}

	if( lastflag & CFCOMP )
		WDGmessage( ECSTR("no other change.") );
	else	WDGmessage( ECSTR("no change.") );

	return( T );
}

/*
 *	The  functions in this file implement commands that search in
 *	the forward and backward directions for matching a character.
 */

CMD
matchrpar( void )
{
	return( rmatchc( '(', T ) );
}

CMD
matchrcur( void )
{
	return( rmatchc( '{', T ) );
}

CMD
matchrbra( void )
{
	return( rmatchc( '[', T ) );
}

CMD
matchlpar( void )
{
	return( lmatchc( ')', T ) );
}

CMD
matchlcur( void )
{
	return( lmatchc( '}', T ) );
}

CMD
matchlbra( void )
{
	return( lmatchc( ']', T ) );
}
