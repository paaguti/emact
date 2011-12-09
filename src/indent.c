#if	!defined( lint )
static	char rcsid[] = "$Id: indent.c,v 1.5 2008/06/19 12:13:30 jullien Exp $";
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
 *	This file manage the indentation rules of C & Lisp languages.
 */
 
#include	"emacs.h"
 
static	EDLINE * _define(nextcindent,(void));
static	CMD	 _define(nexttab,(int col));
static	CMD	 _define(indent,(void));
static	int	 _define(lispindent,(void));
static	int	 _define(sgmlindent,(void));
static	int	 _define(cindent,(void));

int	commento;
int	indento;
EDLINE	*indentp;

/*
 *	Returns  the  left  margin  of  a  given line up to the 'max'
 *	position. Tabs are expanded to spaces.
 */

int
leftmargin( EDLINE *lp )
{
	int	ncol = 0;
	EMCHAR	*buf = ltext( lp );
	int	max  = llength( lp );
	EMCHAR	c;

	while( max-- > 0 && separatorp( *buf ) ) {
		if( (c = *buf++) == '\t' )
			do
				++ncol;
			while( ncol % tab_display );
		else	{
			if( !self_insert( c ) )
				++ncol;
			++ncol;
		}
	}
	return( ncol );
}

/*
 *	Insert spaces/tabulation up to a given position
 */

static	CMD
nexttab( int col )
{
	int	i;

	if( col < 0 ) {
		TTYbeep();
		WDGmessage(ECSTR("nexttab invalid index"));
		return( NIL );
	}

	if( curwp->w_emode == SGMLMODE   ||
	    curwp->w_emode == CSHARPMODE ||
	    curwp->w_emode == LISPMODE   ||
	    curwp->w_emode == JAVAMODE   )
		return( linsert( col, ' ' ) );

	if( ((i=col/tab_display) != 0 && linsert(i, '\t') == NIL) ||
	    ((i=col%tab_display) != 0 && linsert(i, ' ' ) == NIL) )
	    	return( NIL );
	else	return(  T  );
}

/*
 *	Next  C  indent.  Compute  the  next  level of indentation in
 *	CMODE, CPPMODE, CSHARPMODE, PERLMODE or JAVAMODE.
 */

static	EDLINE	*
nextcindent( void )
{
	EDLINE	*llp;
	EDLINE	*oclp;
	EDLINE	*clp;
	int	ocbo;
	int	c;
	int	i;
	int	llflag;

	llflag = (curwp->w_dotp == lastline( curbp ));

	(void)linsert( 1, '}' );

	oclp		= curwp->w_dotp;
	ocbo		= curwp->w_doto-1;
	i		= lmatchc( '}', NIL );
	curwp->w_dotp	= oclp;
	curwp->w_doto	= ocbo;
	clp		= curwp->w_dotp;

	(void)forwdel();	/* delete '}' */

	if( i != T )
		return( clp );

	commento = 0;
	llp	 = indentp;

	while( curwp->w_dotp != llp ) {
		i = lastc( curwp->w_dotp );

		if( i > 0 && !commento )
			c = lgetc( curwp->w_dotp, i - 1 );
		else	if( (curwp->w_dotp = lback( curwp->w_dotp )) == llp )
				break;
		else	continue;

		/*
		 * At this point,  curwp is the first non empty line,
		 * i  is  the index of the last valid character and c
		 * is value of this character.
		 */

		clp = curwp->w_dotp;

		if( c == '}' ) {
			/*
			 * The last statement is a closing block. unindent.
			 */
			curwp->w_doto = i - 1;
			if( lmatchc( '}', NIL ) == T ) {
				if( curwp->w_dotp == indentp )
				     curwp->w_dotp = lback( curwp->w_dotp );
				else while( curwp->w_dotp != indentp ) {
					 curwp->w_dotp=lback(curwp->w_dotp);
					 if( curwp->w_dotp == llp ) {
					     WDGmessage(ECSTR("Indent error"));
					     break;
					 }
				}
				clp = curwp->w_dotp;
				continue;
			} else	break;
		}

		do	{
			if( (curwp->w_dotp = lback( curwp->w_dotp )) == llp ) {
				curwp->w_dotp = oclp;
				curwp->w_doto = ocbo;
				return( clp );
			}
			i = lastc( curwp->w_dotp );
		} while( i == 0 );

		c = lgetc( curwp->w_dotp, i - 1 );

		if( c != ')' && c != '{' )
			break;
		else	clp = curwp->w_dotp;
	}

	curwp->w_dotp = oclp;
	curwp->w_doto = ocbo;

	if( llflag ) {
		/*
		 * Special at the end, delete the newline added by '}'
		 */
		(void)forwdel();
	}

	return( clp );
}

/*
 *	Delete one level of indentation in CMODE,  CPPMODE,  PERLMODE
 *	CSHARPMODE, or JAVAMODE buffer.
 */

int
unindent( int c, int f )
{
	int	max;

	max = curwp->w_doto;

	if( max > 1 && lgetc( curwp->w_dotp, max-1 ) == '\'' )
		return( linsert( 1, c ) );

	if( linsert(1,c)!=T || automatch(c,f)!=T || backdel()!=T )
		return( NIL );

	if( (indentp==curwp->w_dotp)&&(max>1)&&lgetc(curwp->w_dotp,max-1)!='{' )
		return( linsert( 1, c ) );

	while( curwp->w_doto > 0 ) {
		if( !separatorp( lgetc( curwp->w_dotp, curwp->w_doto-1 )) ) {
			if( lnewline() == NIL )
				return( NIL );
			else	break;
		} else	{
			if( backdel() == NIL )
				return( NIL );
		}
	}

	if( nexttab( indento ) == NIL )
	    	return( NIL );

	return( linsert( 1, c ) );

}

/*
 *	Returns the last position of the C statement of a line.
 */

int
lastc( EDLINE *line )
{
	EMCHAR	*buf = ltext( line );
	int	n    = llength( line ) - 1;

	for( ; n >= 0 ; n-- )
		switch( buf[ n ] ) {
		case ' ' :
		case '\t':
			break;
		default  :
			if( commento == 1 ) {
				int	i;
				for( i = n ; i > 0 ; i-- )
					if( buf[i]=='*' && buf[i-1]=='/' ) {
						commento = 0;
						break;
					}
				if( i == 0 )
					return( n + 1 );
				else	n = i - 1;
			} else	return( n + 1 );
		}

	return( 0 );
}

/*
 *	Try to indent according to C/C++/Java indentation rules.
 */

static	int
cindent( void )
{
	EDLINE	*clp	= curwp->w_dotp;
	int	nindent = 0;
	int	ncol;
	int	n;
	int	i;

	i = lastc( clp );

	/*
	 *	Search the first non empty line above the current one.
	 */

	while( clp != lastline( curbp ) )
		if( (i = lastc( clp )) > 0 )
			break;
	else	clp = lback( clp );

	switch( lgetc( clp, i - 1 ) ) {
	case '{' :
		n = llength(clp) - 6;
		for( i = 0 ; i < n ; i++ ) {
			if( lgetc( clp, i ) == 's' &&
			    !emstrncmp( ltext(clp)+i, ECSTR("switch"), 6 ) )
				break;
		}
		if( i >= n )
			nindent++;
		break;
	case '}' :
		i = lmatchc( '{', NIL );
		if( i == T ) {
			ncol = leftmargin( indentp );
			return( nexttab( ncol ) );
		}
		break;
	case '*' :
		n = llength( clp );
		if( n >= (i - 2) && lgetc( clp, i - 2 ) == '/' ) {
			/*
			 * start of a C comment
			 */
			ncol = leftmargin( clp );
			return( nexttab( ncol+1 ) );
		}
		break;
	case '/' :
		n = llength( clp );
		if( n >= (i-2) ) {
			EMCHAR c = lgetc( clp, i - 2 );

			if( c == '/' ) {
				/*
				 * start of C++ comment
				 */
				ncol = leftmargin( clp );

				return( nexttab( ncol ) );
			}

			if( c == '*' ) {
				/*
				 * end of C comment
				 */

				i -= 2;
				while( i > 1 ) {
					if( lgetc( clp, i ) == '*' &&
					    lgetc( clp, i-1 ) == '/' )
						break;
					i--;
				}
						
				if( i > 1 )
					break;

				ncol = leftmargin( clp );
				if( ncol > 0 )
					return( nexttab( ncol-1 ) );
				else	return( nexttab( ncol   ) );
			}
		}
		break;
	case ')' :
	case ':' :
		if( separatorp( lgetc( clp, 0 )) )
			nindent++;
		break;
	default	 :
		clp = nextcindent();
	}

	ncol = leftmargin( clp );

	if( nindent ) {
		int ntab;
		if( (curwp->w_emode==JAVAMODE) || (curwp->w_emode==CSHARPMODE) )
			ntab = java_indent;
		else	ntab = tab_size;
		ncol = ntab * ((ncol + ntab) / ntab);
	}

	return( nexttab( ncol ) );

}

/*
 *	Returns the last position of the lisp statement in buf.
 */

int
lastlisp( EDLINE *line )
{
	EMCHAR	*buf = ltext( line );
	int	n    = llength( line );
	int	dblq = 0;
	int	i    = 0;
	int	j;

	commento = -1;

	while( i < n )
		switch( buf[ i ] ) {
		case '"' :
			/*
			 * Is  character  '"' as a Lisp char (#\;) or
			 * quoted in string ("..\"..") ?
			 */
			if( i == 0 || buf[i-1] != '\\' )
				/*
				 * No, increment double-quote count.
				 */
				dblq++;
			i++;
			break;
		case ';' :
			if( ((dblq % 2) == 1) || (i > 0 && buf[i-1] == '\\') ) {
				/*
				 * In string or quoted, continue
				 */
				i++;
				break;
			}
			commento = i;
			return( i - 1 );
		case ' ' :
		case '\t':
			j = i - 1;
			do if( i++ >= n-1 )
				return( j );
			while( separatorp( buf[ i ] ) );

			if( (dblq % 2) == 0 && buf[ i ] == ';' ) {
				/*
				 * It's not in string or quoted.
				 */
				commento = i;
				return( j );
			}
			break;
		default  :
			i++;
		}

	return( n - 1 );
}

/*
 *	Try to indent according to the current Lisp indentation rules.
 */

#define	lambdap( l, n ) (emstrncmp( lchadr((l),(n)), ECSTR("lambda"), 6 ) == 0)

static int
lispindent( void )
{
	EDLINE	*clp  = curwp->w_dotp;
	int	cbo   = curwp->w_doto;
	int	sexpr = 0;
	int	lambda= 0;
	int	max;
	int	i;
	int	j;
	int	c;

	/*
	 *	Search the first non empty line above the current one.
	 */

	indentp = clp;
	while( indentp != lastline( curbp ) ) {
		if( lastlisp( indentp ) > 0 )
			break;
		else	indentp = lback( indentp );
	}

	max = lastlisp( indentp );

	if( max < 0 ) {
		if( commento >= 0 )
			indento = commento;
	} else	if( lgetc( indentp, max ) == ')' && lmatchc( ')', T ) ) {
		max = lastlisp( indentp );

		for( i = 0 ; i <= max ; i++ ) {
			if( (c = lgetc( indentp, i )) == '(' ) {
				if( indento == i )
					/*
					 * case:  "      ( ... )"
					 * indent here ->_
					 */
					break;

				/*
				 * get  the first unclosed parenthese
				 * from right to left.
				 */

				for( j = max ; j >= i ; j-- ) {
					switch( lgetc( indentp, j ) ) {
					case '(' : sexpr++; break;
					case ')' : sexpr--; break;
					}
					if( sexpr > 0 ) {
						i = j;
						break;
					}
				}

				if( (max - i) > 6 && lambdap(indentp, i+1) )
					/*
					 * case: "  (xxx (lambda"
					 * indent here    ->_
					 */
					lambda++;

				if( indento == i )
					/*
					 * case: "  (xxx (yyy"
					 * indent here ->_
					 */
					break;

				if( sexpr > 0 )
					/*
					 * unclosed   parenthese  was
					 * found,    go    into   the
					 * expression.
					 */
					indento = ++i;

				if( indento>0 && lgetc(indentp,indento)=='(' )
					/*
					 * case:  " ... (( ... )"
					 * indent here ->_
					 */
					break;
				else	{
					/*
					 * case:  "      (xxx yyy ... ( ... )"
					 * indent here      ->_
					 */

					if( lambda ) {
						/*
						 * case: "  (xxx (lambda"
						 * indent here    ->_
						 */
						indento = i + 2;
						break;
					}

					while(isalnum((int)lgetc(indentp,i++)))
						;

					if( indento == 1 )
						indento = 3;	/* (def...  */
					else	indento = i;	/* standard */
					}
				break;
			}

			if( !separatorp( c ) ) {
				if( i == indento - 1 &&
				    ( lgetc( indentp, i ) == '\'' ||
				      lgetc( indentp, i ) == ','  ||
				      lgetc( indentp, i ) == '`' ) )
					/*
					 * case   " ... '( ... )"
					 * case   " ... `( ... )"
					 * case   " ... ,( ... )"
					 * indent here ->_
					 */
					break;
				else	/*
					 * case:  "      xxx ( ... )"
					 * indent here ->_
					 */

					indento = i;
				break;
			}
		}
	} else	for( i = max, j = 0 ; i >= 0 ; i-- ) {
			if( (c = lgetc( indentp, i )) == ')' ) {
				j++;
				continue;
			}
			if( c == '(' && --j < 0 ) {
				/*
				 * case:  "      (xxx yy"
				 * indent here      ->_
				 */
				i++;
				while(isalnum((int)lgetc(indentp,i))&&i<= max)
					i++;
				indento = i + 1;
				break;
			}
			if( !separatorp( c ) )
				/*
				 * case:  "      xxx yy"
				 * indent here ->_
				 */
				indento = i;
		}
	
	for( i = 0, j = 0 ; i < indento ; i++ ) {
		if( lgetc( indentp, i ) == '\t' )
			do
				++j;
			while( j % tab_display );
		else	j++;
	}

	indento	= j;
	curwp->w_dotp	= clp;
	curwp->w_doto	= cbo;
	curwp->w_flag  |= WFMOVE;

	return( nexttab( indento ) );
}

/*
 *	Try to indent according to the SGML indentation rules.
 */

static int
sgmlindent( void )
{
	EDLINE	*clp = curwp->w_dotp;
	EMCHAR	*buf = ltext( clp );
	int	n    = llength( clp );
	int	i;
	int	ncol;

	/*
	 * find the first non-empty line above.
	 */

	while( clp != lastline( curbp ) )
		if( lastc( clp ) > 0 )
			break;
		else	clp = lback( clp );

	for( i = 0 ; i < n ; ++i ) {
		switch( buf[i] ) {
		case ' ' :
		case '\t':
			break;
		case '<' :
			/*
			 * WAP tags only!
			 */
			if( !emstrncmp( &buf[i+1], ECSTR("card"),     4 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("do"),       2 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("onevent"),  7 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("p"),        1 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("select"),   6 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("table"),    5 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("template"), 8 ) ||
			    !emstrncmp( &buf[i+1], ECSTR("wml"),      3 ) )
				return( nexttab( i + 3 ) );
			else	return( nexttab( i ) );
		default:
			return( nexttab( i ) );
		}
	
	}

	ncol = leftmargin( clp );

	return( nexttab( ncol ) );
}

/*
 *	Insert  enough  tabs  and  spaces  to respect the indentation
 *	rules of the language in use (C,  C++,  Java or Lisp). Insert
 *	a  newline  by  calling  the  standard  routine.  Insert  the
 *	indentation  by  inserting  the  right  number  of  tabs  and
 *	spaces.  Return  T  all  ok  or NIL if one of the subcommands
 *	failed.
 */

static CMD
indent( void )
{
	EDLINE	*clp	= curwp->w_dotp;
	int	nindent = 0;
	int	ncol;
	int	i;

	/*
	 *	Try to indent.
	 */

	switch( curwp->w_emode ) {
	case CMODE       :
	case CPPMODE     :
	case CSHARPMODE  :
	case PERLMODE    :
	case JAVAMODE    :
		return( cindent() );

	case LISPMODE :
		return( lispindent() );

	case SGMLMODE    :
		return( sgmlindent() );

	case PROLOGMODE  :
	case FORTRANMODE :
	case SHELLMODE   :
		i = 0;

		/*
		 * find the first non-empty line above.
		 */

		while( clp != lastline( curbp ) )
			if( (i = lastc( clp )) > 0 )
				break;
			else	clp = lback( clp );

		switch( lgetc( clp, i - 1 ) ) {
		case '-' :
			if( i >= 2 && lgetc( clp, i - 2 ) == ':' ) {
				if( lnewline() != NIL )
					return( linsert( 1, '\t' ) );
				else	return( NIL );
			}
			break;
		case '.' :
			return( lnewline() );
		}
		break;
	}

	ncol = leftmargin( clp );

	if( nindent )
		ncol = tab_size * ((ncol + tab_size) / tab_size);

	return( nexttab( ncol ) );
}

/*
 *	Reindent current line.
 */

CMD
tabindent( void )
{
	(void)openline();
	(void)indent();
	(void)forwdel();

	if( curwp->w_doto != llength(curwp->w_dotp) && lgetdot() == '}' ) {
		(void)forwdel();
		(void)unindent( '}', NIL );
		(void)backchar();
	}

	return( T );
}

/*
 *	This  function  indent  the  current  line  with the language
 *	rules  in  use  (Lisp  or  C).  First,  cursor  moves  to the
 *	beginning  of  the  line  and then execute a tab (actually an
 *	indentation) and goes to the next line. Bound to C-X-I.
 */

CMD
indentline( void )
{
	int n    = repeat;
	int save = repeat;

	repeat = 1;

	if( curwp->w_emode == FUNDAMENTAL ||
	    curwp->w_emode == DIRED       ||
	    llength(curwp->w_dotp) == 0 ) {
		(void)forwline();
		repeat = save;
		return( NIL );
	}

	while( n-- && gotobol()==T && tabindent()==T && forwline()==T )
		;

	repeat = save;

	return( T );
}

/*
 *	Break  the  current  line  in  two part.  It removes trailing
 *	blanks/tabs  at the left part and indent the right.  Bound to
 *	C-J.
 */

CMD
newlineindent( void )
{
	(void)justonespace();
	(void)backchar();
	(void)openline();
	(void)forwline();
	(void)gotobol();
	(void)tabindent();

	return( T );
}

/*
 *	Move to the indentation point of the current line.
 *	Bound to M-m.
 */

CMD
backtoindent( void )
{
	int	 i;

	(void)gotobol();
	for( i = 0 ; i < llength( curwp->w_dotp ) ; i++ )
		if( separatorp( lgetdot() ) )
			(void)forwchar();
		else	break;
	return( T );
}

/*
 *	This  function  search for the beginning of a LISP expression
 *	and  set point to the first parenthese.  The beginning of the
 *	is the first line with '(' at position 0. Bound to M-C-B.
 */

CMD
blispexpr( void )
{
	EMCHAR	c;

	switch( curwp->w_emode ) {
	case CMODE      :
	case CPPMODE    :
	case CSHARPMODE :
	case PERLMODE   :
	case JAVAMODE   :
		c = '{';
		break;
	case LISPMODE   :
		c = '(';
		break;
	default:
		return( NIL );
	}

	while( lgetc( curwp->w_dotp, 0 ) != c && backline() == T )
		;

	curwp->w_doto = 0;

	return( T );
}

/*
 *	This  function  search  for  the end of a LISP expression and
 *	set  point  to  the  last  parenthese.  First  seach  for the
 *	beginning   of   the   expression,  then  match  the  current
 *	parenthese. Bound to M-C-E.
 */

CMD
elispexpr( void )
{
	switch( curwp->w_emode ) {
	case CMODE      :
	case CPPMODE    :
	case CSHARPMODE :
	case PERLMODE   :
	case JAVAMODE   :
		if( blispexpr() == T && matchrcur() == T )
			return( T );
		else	return( NIL );
	case LISPMODE   :
		if( blispexpr() == T && matchrpar() == T )
			return( T );
		else	return( NIL );
	default:
		return( NIL );
	}
}

/*
 *	Insert only one blank around two words. Bound to M-SPACE
 */

CMD
justonespace( void )
{
	EMCHAR	c;

	if( curwp->w_doto == llength( curwp->w_dotp ) ) {
		/*
		 *	At the end, back one char.
		 */
		(void)backchar();
		if( (c = lgetdot()) != ' ' && c != '\t' ) {
			(void)forwchar();
			(void)linsert( 1, ' ' );
			return( T );
		}
	}

	do
		if( backchar() == NIL )
			break;
	while( curwp->w_doto >= 0 && ((c=lgetdot()) == ' ' || c == '\t') );

	if( (c = lgetdot()) != ' ' && c != '\t' )
		(void)forwchar();

	(void)linsert( 1, ' ' );

	while( (curwp->w_doto < llength(curwp->w_dotp)) &&
	       ((c=lgetdot()) == ' ' || c == '\t') )
		(void)forwdel();

	return( T );

}
