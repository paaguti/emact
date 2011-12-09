#if	!defined( lint )
static	char *sccsid = "@(#)sed.c	(c) C. Jullien 2009/05/02";
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
 *	sed -- stream editor
 */

/*
 *   Sed  copies  the  named  files  (standard  input default) to the
 *   standard output,  edited according to a script of commands.  The
 *   -f  option  causes the script to be taken from file sfile; these
 *   options accumulate.  If there is just one -e option and no -f's,
 *   the  flag  -e  may  be  omitted.  The  -n  option suppresses the
 *   default output.
 *
 *   A  script  consists  of editing commands,  one per line,  of the
 *   following form:
 *
 *        [address [, address] ] function [arguments]
 *
 *   In  normal  operation sed cyclically copies a line of input into
 *   a  pattern  space  (unless  there  is something left after a `D'
 *   command),  applies  in  sequence  all  commands  whose addresses
 *   select  that pattern space,  and at the end of the script copies
 *   the  pattern  space to the standard output (except under -n) and
 *   deletes the pattern space.
 *
 *   An  address  is  either a decimal number that counts input lines
 *   cumulatively  across  files,  a `$' that addresses the last line
 *   of input,  or a context address,  `/regular expression/', in the
 *   style of ed(1) modified thus:
 *
 *        The escape sequence `\n' matches a newline embedded  in
 *        the pattern space.
 *
 *   A command line with no addresses selects every pattern space.
 *
 *   A  command line with one address selects each pattern space that
 *   matches the address.
 *
 *   A  command  line  with two addresses selects the inclusive range
 *   from  the  first  pattern  space  that matches the first address
 *   through the next pattern space that matches the second.  (If the
 *   second  address  is  a  number  less  than  or equal to the line
 *   number  first  selected,  only one line is selected.) Thereafter
 *   the process is repeated, looking again for the first address.
 *
 *   Editing  commands  can  be  applied only to non-selected pattern
 *   spaces by use of the negation function `!' (below).
 *
 *   In  the  following  list  of  functions  the  maximum  number of
 *   permissible   addresses   for  each  function  is  indicated  in
 *   parentheses.
 *
 *   An argument denoted text consists of one or more lines,  all but
 *   the last of which end with `\' to hide the newline.  Backslashes
 *   in  text  are treated like backslashes in the replacement string
 *   of  an  `s'  command,  and may be used to protect initial blanks
 *   and  tabs  against  the  stripping  that is done on every script
 *   line.
 *
 *   An  argument denoted rfile or wfile must terminate the com- mand
 *   line  and  must be preceded by exactly one blank.  Each wfile is
 *   created  before  processing  begins.  There  can  be  at most 10
 *   distinct wfile arguments.
 *
 *   (1)a\
 *   text
 *        Append.  Place  text  on the output before reading the next
 *        input line.
 *
 *   (2)b label
 *        Branch  to  the `:' command bearing the label.  If label is
 *        empty, branch to the end of the script.
 *
 *   (2)c\
 *   text
 *        Change.  Delete  the pattern space.  With 0 or 1 address or
 *        at the end of a 2-address range,  place text on the output.
 *        Start the next cycle.
 *
 *   (2)d Delete the pattern space.  Start the next cycle.
 *
 *   (2)D Delete the initial segment of the pattern space through the
 *        first newline.  Start the next cycle.
 *
 *   (2)g Replace the contents of the pattern space by  the  contents
 *        of the hold space.
 *
 *   (2)G Append the contents of the hold space to the pattern space.
 *
 *   (2)h Replace the contents of  the hold space  by the contents of
 *        the pattern space.
 *
 *   (2)H Append the contents of the pattern space to the hold space.
 *
 *   (1)i\
 *   text
 *        Insert.  Place text on the standard output.
 *
 *   (2)n Copy the pattern space to the standard output.  Replace the
 *        pattern space with the next line of input.
 *
 *   (2)N Append the next line of input to the pattern space with
 *        an   embedded   newline.    (The  current  line  number
 *        changes.)
 *
 *   (2)p Print.  Copy the pattern space to the standard output.
 *
 *   (2)P Copy the initial segment of the pattern  space  through the
 *        first newline to the standard output.
 *
 *   (1)q Quit.  Branch to the end of the script.  Do not start a new
 *        cycle.
 *
 *   (2)r rfile
 *        Read the contents of rfile. Place them on the output before
 *        reading the next input line.
 *
 *   (2)s/regular expression/replacement/flags
 *        Substitute  the  replacement  string  for  instances of the
 *        regular expression in the pattern space.  Any character may
 *        be  used  instead  of  `/'.  For  a  fuller description see
 *        ed(1). Flags is zero or more of
 *
 *        g Global.  Substitute for all nonoverlapping  instances
 *          of  the regular expression rather than just the first
 *          one.
 *
 *        p Print the pattern space if a replacement was made.
 *
 *        w wfile
 *          Write.  Append  the  pattern  space  to  wfile  if  a
 *          replacement was made.
 *
 *   (2)t label
 *        Test.  Branch  to  the `:' command bearing the label if any
 *        substitutions  have been made since the most recent reading
 *        of an input line or execution of a `t'.  If label is empty,
 *        branch to the end of the script.
 *
 *   (2)w wfile
 *        Write.  Append the pattern space to wfile.
 *
 *   (2)x Exchange the contents of the pattern and hold spaces.
 *
 *   (2)y/string1/string2/
 *        Transform.   Replace   all  occurrences  of  characters  in
 *        string1  with  the corresponding character in string2.  The
 *        lengths of string1 and string2 must be equal.
 *
 *   (2)! function
 *        Don't.  Apply  the function (or group,  if function is `{')
 *        only to lines not selected by the address(es).
 *
 *   (0): label
 *        This  command  does  nothing;  it bears a label for `b' and
 *        `t' commands to branch to.
 *
 *   (1)= Place the current  line number  on the standard output as a
 *        line.
 *
 *   (2){ Execute the following commands through a matching  `}' only
 *        when the pattern space is selected.
 *
 *   (0)  An empty command is ignored.
 */

#if	defined(_WIN32) || defined(_WIN64)
#define	_CRT_SECURE_NO_DEPRECATE	1
#define	_CRT_NONSTDC_NO_DEPRECATE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#if	defined( _DOS ) || defined( _NT )
#include <io.h>
#endif

struct	reptr	{
	char	*ad1;
	char	*ad2;
	union	{
		char		*RE1;
		struct reptr	*LB1;
	}	repe1;
	char	*rhs;
	FILE	*fcode;
	char	command;
	char	gfl;
	char	pfl;
	char	inar;
	char	negfl;
};

#define	re1		repe1.RE1
#define	lb1		repe1.LB1

#define	LABLENGTH	8

struct	label	{
	struct reptr	*chain;
	struct reptr	*address;
	char		asc[LABLENGTH + 1];
};

#if	defined( _ISO ) || defined( __STDC__ ) || defined( __cplusplus )
#define	_define( fun, args )		fun args
#else
#define	_define( fun, args )		fun ()
#endif

extern	int		_define(main,(int argc, char *argv[]));
extern	int		_define(sed,(int argc, char *argv[]));

static	void		_define(usage,(void));
static	void		_define(fcomp,(void));
static	char *		_define(compsub,(char *rhsbuf));
static	char *		_define(compile,(char *expbuf));
static	int		_define(rline,(char *lbuf));
static	char *		_define(address,(char *expbuf));
static	int		_define(cmp,(char *a, char *b));
static	char *		_define(text,(char *textbuf));
static	struct label *	_define(search,(struct label *ptr));
static	void		_define(dechain,(void));
static	char *		_define(ycomp,(char *expbuf));
static	void		_define(execute,(char *file));
static	int		_define(match,(char *expbuf, int gf));
static	int		_define(advance,(char *alp, char *aep));
static	int		_define(substitute,(struct reptr *ipc));
static	void		_define(dosub,(char *rhsbuf));
static	char *		_define(place,(char *asp, char *al1, char *al2));
static	void		_define(command,(struct reptr *ipc));
static	char *		_define(gline,(char *addr));
static	int 		_define(ecmp,(char *a, char *b, int count));
static	void		_define(arout,(void));
static	void		_define(sederror,(char *fmt, char *arg));

#define	VERSION		"sed v1.5 by C. Jullien\n"

#define CBRA		1
#define	CCHR		2
#define	CDOT		4
#define	CCL		6
#define	CNL		8
#define	CDOL		10
#define	CEOF		11
#define CKET		12
#define CNULL		13
#define CLNUM		14
#define CEND		16
#define CDONT		17
#define	CBACK		18

#define	STAR		01

#define NLINES		256
#define	DEPTH		20
#define PTRSIZE		256
#define RESIZE		(16 * 1024)
#define	ABUFSIZE	20
#define	LBSIZE		4096
#define	ESIZE		256
#define	LABSIZE		64
#define NBRA		9
#define	BUFREAD		8192
#define	MAXFD		12
#define	MAXFNAME	40

#define ACOM		001
#define BCOM		020
#define CCOM		002
#define	CDCOM		025
#define	CNCOM		022
#define COCOM		017
#define	CPCOM		023
#define DCOM		003
#define ECOM		015
#define EQCOM		013
#define FCOM		016
#define GCOM		027
#define CGCOM		030
#define HCOM		031
#define CHCOM		032
#define ICOM		004
#define LCOM		005
#define NCOM		012
#define PCOM		010
#define QCOM		011
#define RCOM		006
#define SCOM		007
#define TCOM		021
#define WCOM		014
#define	CWCOM		024
#define	YCOM		026
#define XCOM		033

static	FILE		*fin;
static	FILE		*fcode[MAXFD];
static	struct reptr	*abuf[ABUFSIZE+1];
static	struct reptr	**aptr;
static	struct reptr	*ptrend;
static	struct reptr	ptrspace[PTRSIZE+1];
static	struct reptr	*rep;
static	struct reptr	**cmpend[DEPTH];
static	struct reptr	*pending;
static	struct label	ltab[LABSIZE];
static	struct label	*labtab;
static	struct label	*lab;
static	struct label	*labend;
static	long		lnum;
static	long		tlno[NLINES];
static	int		nlno;
static	int		nfiles;
static	int		eflag;
static	int		dolflag;
static	int		sflag;
static	int		jflag;
static	int		numbra;
static	int		delflag;
static	int		nflag;
static	int		gflag;
static	int		fhandle;
static	int		idepth;
static	int		eargc;
static	char		**eargv;
static	char		*lastre;
static	char		ibuf[BUFREAD];
static	char		*cbp;
static	char		*ebp;
static	char		genbuf[LBSIZE+1];
static	char		*loc1;
static	char		*loc2;
static	char		*locs;
static	char		seof;
static	char		*hend;
static	char		*lcomend;
static	char		linebuf[LBSIZE+1];
static	char		holdsp[LBSIZE+1];
static	char		*spend;
static	char		*hspend;
static	char		*braelist[NBRA];
static	char		*braslist[NBRA];
static	char		fname[MAXFD][MAXFNAME];
static	char		*cp;
static	char		*creend;
static	char		*clbend;
static	char		respace[RESIZE+1];
static	char		*badp;
static	char		bad;
static	char		compfl;

static	char		BADCMD[]	 = "Unrecognized command: %s\n";
static	char		CANTCREATE[]	 = "Cannot create %s\n";
static	char		CANTOPEN[]	 = "Cannot open %s\n";
static	char		CMDGARBLED[] 	 = "Command garbled: %s\n";
static	char		DUPLICATELABEL[] = "Duplicate labels: %s\n";
static	char		FRENULL[]	 = "First RE may not be null\n";
static	char		LONGLABEL[]	 = "Label too long: %s\n";
static	char		NOADDRESS[]	 = "No addresses allowed: %s\n";
static	char		NOPATTERN[]	 = "Cannot open pattern-file: %s\n";
static	char		ONLYADRESS[]	 = "Only one address allowed: %s\n";
static	char		OUTLINELONG[]	 = "Output line too long.\n";
static	char		REBOTCH[]	 = "RE botch, %o";
static	char		RETOLONG[]	 = "RE too long: %s\n";
static	char		TOOAPPENDS[]	 = "Too many appends after line %ld\n";
static	char		TOOCLOSEBRACES[] = "Too many }'s";
static	char		TOOCOMMANDS[]	 = "Too many commands: %s\n";
static	char		TOOLABELS[]	 = "Too many labels: %s\n";
static	char		TOOLINENUMS[]	 = "Too many line numbers\n";
static	char		TOOOPENBRACES[]	 = "Too many {'s";
static	char		TOOREADS[]	 = "Too many reads after line %ld\n";
static	char		TOOTEXT[]	 = "Too much text: %s\n";
static	char		TOOWFILE[]	 = "Too many files in w commands\n";
static	char		UNDEFLABEL[]	 = "Undefined label: %s\n";

static	unsigned char	bittab[]  = {
	1,
	2,
	4,
	8,
	16,
	32,
	64,
	128
};

static	char	*trans[040]  = {
	"\\01",
	"\\02",
	"\\03",
	"\\04",
	"\\05",
	"\\06",
	"\\07",
	"<-",
	">-",
	"\n",
	"\\13",
	"\\14",
	"\\15",
	"\\16",
	"\\17",
	"\\20",
	"\\21",
	"\\22",
	"\\23",
	"\\24",
	"\\25",
	"\\26",
	"\\27",
	"\\30",
	"\\31",
	"\\32",
	"\\33",
	"\\34",
	"\\35",
	"\\36",
	"\\37"
};

static	char	rub[] = { "\\177" };

#if	!defined( _SEDLIB )
int
main( argc, argv )
int	argc;
char	*argv[];
{
	return( sed( argc, argv ) );
}
#endif

int
sed( argc, argv )
int	argc;
char	*argv[];
{
	eargc	 = argc;
	eargv	 = argv;
	labtab	 = ltab;
	fhandle  = -1;
	badp	 = &bad;
	aptr	 = abuf;
	lab	 = labtab + 1;		/* 0 reserved for end-pointer */
	rep	 = ptrspace;
	rep->ad1 = respace;
	clbend	 = &linebuf[LBSIZE];
	hend	 = &holdsp[LBSIZE];
	lcomend	 = &genbuf[71];
	ptrend	 = &ptrspace[PTRSIZE];
	creend	 = &respace[RESIZE];
	labend	 = &labtab[LABSIZE];
	lnum	 = 0;
	pending	 = 0;
	idepth	 = 0;
	spend	 = linebuf;
	hspend	 = holdsp;
	fcode[0] = stdout;
	nfiles	 = 1;

	if( eargc == 1 )
		return( 0 );

	while( --eargc > 0 && (++eargv)[0][0] == '-' )
		switch( eargv[0][1] ) {
		case 'V':
			/*
			 * Version information
			 */

			fprintf( stdout, VERSION );
			return( 0 );

		case 'n':
			/*
			 * no default output
			 */

			nflag++;
			continue;

		case 'f':
			/*
			 * take the next argument as pattern-file
			 */

			if( eargc-- <= 0 )
				return( 2 );

			if( (fin = fopen( *++eargv, "r" )) == NULL )
				sederror( NOPATTERN, *eargv);

			fcomp();
			fclose( fin );
			continue;

		case 'e':
			/*
			 * take the next argument as expression
			 */

			eflag++;
			fcomp();
			eflag = 0;
			continue;

		case 'g':
			/*
			 * force global flag
			 */

			gflag++;
			continue;

		case '?':
			/*
			 * display syntax
			 */

			usage();
			return( 0 );

		default:
			/*
			 * Unknown flag
			 */

			usage();
			fprintf( stdout, "Unknown flag: %c\n", eargv[0][1] );
			return( 1 );
		}

	if( compfl == 0 ) {
		eargv--;
		eargc++;
		eflag++;
		fcomp();
		eargv++;
		eargc--;
		eflag = 0;
	}

	if( idepth )
		sederror( TOOOPENBRACES, NULL );

	labtab->address = rep;

	dechain();

	if( eargc <= 0 )
		execute( (char *)NULL );
	else	while( --eargc >= 0 )
			execute(*eargv++);

	fclose( stdout );

	return( 0 );
}

static void
usage()
{
	printf("Usage: sed [-gnV] [-e script] [-f pattern-file] [file ..]\n");
}

static void
sederror( fmt, arg )
char	*fmt;
char	*arg;
{
	if( arg != NULL )
		(void)fprintf( stderr, fmt, arg );
	else	(void)fprintf( stderr, fmt );
	exit( 2 );
}

static void
fcomp()
{
	char		*p;
	char		*op;
	char		*tp;
	struct reptr	*pt;
	struct reptr	*pt1;
	int		i;
	struct label	*lpt;

	compfl	= 1;
	op	= lastre;

	if( rline( linebuf ) < 0 )
		return;

	if( *linebuf == '#' ) {
		if( linebuf[1] == 'n' )
			nflag = 1;
	} else	{
		cp = linebuf;
		goto comploop;
	}

	for( ;; ) {
		if( rline( linebuf ) < 0 )
			break;

		cp = linebuf;

comploop:
		while( *cp == ' ' || *cp == '\t' )
			cp++;
		if( *cp == '\0' || *cp == '#' )
			continue;
		if( *cp == ';' ) {
			cp++;
			goto comploop;
		}

		p = address( rep->ad1 );

		if( p == badp )
			sederror( CMDGARBLED, linebuf );

		if( p == rep->ad1 ) {
			if( op )
				rep->ad1 = op;
			else	sederror( FRENULL, NULL );
		} else	if( p == 0 ) {
				p = rep->ad1;
				rep->ad1 = 0;
		} else	{
			op = rep->ad1;
			if( *cp == ',' || *cp == ';' ) {
				cp++;
				if( (rep->ad2 = p) > creend )
					sederror( TOOTEXT, linebuf );

				p = address( rep->ad2 );
				if( p == badp || p == 0 )
					sederror( CMDGARBLED, linebuf );

				if( p == rep->ad2 )
					rep->ad2 = op;
				else	op = rep->ad2;

			} else	rep->ad2 = 0;
		}

		if( p > creend )
			sederror( TOOTEXT, linebuf );

		while( *cp == ' ' || *cp == '\t' )
			cp++;

swit:
		switch( *cp++ ) {
		case '!':
			rep->negfl = 1;
			goto swit;
		case '{':
			rep->command	 = BCOM;
			rep->negfl	 = (char)!(rep->negfl);
			cmpend[idepth++] = &rep->lb1;

			if( ++rep >= ptrend )
				sederror( TOOCOMMANDS, linebuf );

			rep->ad1 = p;
			if( *cp == '\0' )
				continue;

			goto comploop;
		case '}':
			if( rep->ad1 )
				sederror( NOADDRESS, linebuf );

			if( --idepth < 0 )
				sederror( TOOCLOSEBRACES, NULL );

			*cmpend[idepth] = rep;

			rep->ad1 = p;
			continue;
		case '=':
			rep->command = EQCOM;
			if( rep->ad2 )
				sederror( ONLYADRESS, linebuf );
			break;
		case ':':
			if( rep->ad1 )
				sederror( NOADDRESS, linebuf );

			while( *cp++ == ' ' )
				;
			cp--;

			tp = lab->asc;
			while( (*tp++ = *cp++) != 0 )
				if( tp >= &(lab->asc[LABLENGTH]) )
					sederror( LONGLABEL, linebuf );

			*--tp = '\0';

			if( (lpt = search( lab )) != NULL ) {
				if( lpt->address ) {
				     sederror( DUPLICATELABEL, linebuf );
				}
			} else	{
				lab->chain = 0;
				lpt = lab;
				if( ++lab >= labend )
					sederror( TOOLABELS, linebuf );
			}
			lpt->address = rep;
			rep->ad1 = p;

			continue;
		case 'a':
			rep->command = ACOM;
			if( rep->ad2 )
				sederror( ONLYADRESS, linebuf );

			if( *cp == '\\' )
				cp++;
			if( *cp++ != '\n' )
				sederror( CMDGARBLED, linebuf );

			rep->re1 = p;
			p = text( rep->re1 );
			break;
		case 'c':
			rep->command = CCOM;
			if( *cp == '\\' )
				cp++;
			if( *cp++ != ('\n') )
				sederror( CMDGARBLED, linebuf );

			rep->re1 = p;
			p = text( rep->re1 );
			break;
		case 'i':
			rep->command = ICOM;
			if( rep->ad2 )
				sederror( ONLYADRESS, linebuf );

			if( *cp == '\\' )
				cp++;

			if( *cp++ != ('\n') )
				sederror( CMDGARBLED, linebuf );

			rep->re1 = p;
			p = text( rep->re1 );
			break;
		case 'g':
			rep->command = GCOM;
			break;
		case 'G':
			rep->command = CGCOM;
			break;
		case 'h':
			rep->command = HCOM;
			break;
		case 'H':
			rep->command = CHCOM;
			break;
		case 't':
			rep->command = TCOM;
			goto jtcommon;
		case 'b':
			rep->command = BCOM;
jtcommon:
			while( *cp++ == ' ' )
				;
			cp--;

			if( *cp == '\0' ) {
				if( (pt = labtab->chain) != NULL ) {
					while( (pt1 = pt->lb1) != NULL )
						pt = pt1;
					pt->lb1 = rep;
				} else	labtab->chain = rep;
				break;
			}
			tp = lab->asc;
			while( (*tp++ = *cp++) != 0 )
				if( tp >= &(lab->asc[LABLENGTH]) )
					sederror( LONGLABEL, linebuf );

			cp--;
			*--tp = '\0';

			if( (lpt = search( lab )) != NULL ) {
				if( lpt->address ) {
					rep->lb1 = lpt->address;
				} else	{
					pt = lpt->chain;
					while( (pt1 = pt->lb1)!=NULL )
						pt = pt1;
					pt->lb1 = rep;
				}
			} else	{
				lab->chain = rep;
				lab->address = 0;
				if( ++lab >= labend )
				      sederror( TOOLABELS, linebuf );
			}
			break;
		case 'n':
			rep->command = NCOM;
			break;
		case 'N':
			rep->command = CNCOM;
			break;
		case 'p':
			rep->command = PCOM;
			break;
		case 'P':
			rep->command = CPCOM;
			break;
		case 'r':
			rep->command = RCOM;
			if( rep->ad2 )
				sederror( ONLYADRESS, linebuf );

			if( *cp++ != ' ' )
				sederror( CMDGARBLED, linebuf );

			rep->re1 = p;
			p = text( rep->re1 );
			break;
		case 'd':
			rep->command = DCOM;
			break;
		case 'D':
			rep->command = CDCOM;
			rep->lb1 = ptrspace;
			break;
		case 'q':
			rep->command = QCOM;
			if( rep->ad2 )
				sederror( ONLYADRESS, linebuf );

			break;
		case 'l':
			rep->command = LCOM;
			break;
		case 's':
			rep->command = SCOM;
			seof = *cp++;
			rep->re1 = p;
			p = compile( rep->re1 );
			if( p == badp )
				sederror( CMDGARBLED, linebuf );

			if( p == rep->re1 ) {
				rep->re1 = op;
			} else	op = rep->re1;

			if( (rep->rhs = p) > creend )
				sederror( TOOTEXT, linebuf );

			if( (p = compsub( rep->rhs )) == badp )
				sederror( CMDGARBLED, linebuf );

			if( *cp == 'g' ) {
				cp++;
				rep->gfl++;
			} else	if( gflag )
				rep->gfl++;

			if( *cp == 'p' ) {
				cp++;
				rep->pfl = 1;
			}

			if( *cp == 'P' ) {
				cp++;
				rep->pfl = 2;
			}

			if( *cp == 'w' ) {
				cp++;
				if( *cp++ != ' ' )
					sederror( CMDGARBLED, linebuf );

				if( nfiles >= (MAXFD-2) )
					sederror( TOOWFILE, NULL );

				text( fname[nfiles] );

				for( i = nfiles - 1; i >= 0; i-- )
					if( cmp(fname[nfiles],fname[i]) == 0 ) {
						rep->fcode = fcode[i];
						goto done;
					}

				rep->fcode=fopen( fname[nfiles], "w" );
				if( rep->fcode == NULL )
					sederror( CANTOPEN, fname[nfiles] );

				fcode[nfiles++] = rep->fcode;
			}
			break;
		case 'w':
			rep->command = WCOM;

			if( *cp++ != ' ' )
				sederror( CMDGARBLED, linebuf );

			if( nfiles >= (MAXFD-2) )
				sederror( TOOWFILE, NULL );

			text( fname[nfiles] );
			for( i = nfiles - 1; i >= 0; i-- )
				if( cmp(fname[nfiles], fname[i]) == 0 ) {
					rep->fcode = fcode[i];
					goto done;
				}

			rep->fcode = fopen( fname[nfiles], "w" );

			if( rep->fcode == NULL )
				sederror( CANTCREATE, fname[nfiles] );

			fcode[nfiles++] = rep->fcode;
			break;
		case 'x':
			rep->command = XCOM;
			break;
		case 'y':
			rep->command = YCOM;
			seof = *cp++;
			rep->re1 = p;
			p = ycomp( rep->re1 );
			if( p == badp )
				sederror( CMDGARBLED, linebuf );

			if( p > creend )
				sederror( TOOTEXT, linebuf );

			break;
		default:
			sederror( BADCMD, linebuf );
		}
done:
		if( ++rep >= ptrend )
			sederror( TOOCOMMANDS, linebuf );

		rep->ad1 = p;

		if( *cp++ != '\0' ) {
			if( cp[-1] == ';' )
				goto comploop;
			sederror( CMDGARBLED, linebuf );
		}

	}
	rep->command = 0;
	lastre = op;
}

static char *
compsub( rhsbuf )
char	*rhsbuf;
{
	char	*p;
	char	*q;

	p = rhsbuf;
	q = cp;

	for( ;; ) {
		if( (*p = *q++) == '\\' ) {
			*p = *q++;
			if( *p > numbra + '0' && *p <= '9' )
				return( badp );
			*p++ |= 0200;
			continue;
		}
		if( *p == seof ) {
			*p++ = '\0';
			cp   = q;
			return( p );
		}
		if( *p++ == '\0' )
			return( badp );
	}
}

static char *
compile( expbuf )
char	*expbuf;
{
	char	*ep;
	char	*sp;
	char	*lastep;
	char	*cstart;
	char	*bracketp;
	int	cclcnt;
	int	closed;
	int	c;
	char	bracket[NBRA];
	char	neg;

	if( *cp == seof ) {
		cp++;
		return( expbuf );
	}

	ep	 = expbuf;
	lastep	 = 0;
	bracketp = bracket;
	closed   = numbra = 0;
	sp	 = cp;

	if( *sp == '^' ) {
		*ep++ = 1;
		sp++;
	} else	*ep++ = 0;

	for( ;; ) {
		if( ep >= &expbuf[ESIZE] ) {
			cp = sp;
			return( badp );
		}
		if( (c = *sp++) == seof ) {
			if( bracketp != bracket ) {
				cp = sp;
				return( badp );
			}
			cp = sp;
			*ep++ = CEOF;
			return( ep );
		}
		if( c != '*' )
			lastep = ep;

		switch( c ) {

		case '\\':
			if( (c = *sp++) == '(' ) {
				if( numbra >= NBRA ) {
					cp = sp;
					return( badp );
				}
				*bracketp++ = (char)numbra;
				*ep++	    = CBRA;
				*ep++	    = (char)numbra++;
				continue;
			}
			if( c == ')' ) {
				if( bracketp <= bracket ) {
					cp = sp;
					return( badp );
				}
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;
			}

			if( c >= '1' && c <= '9' ) {
				if( (c -= '1') >= closed )
					return( badp );

				*ep++ = CBACK;
				*ep++ = (char)c;
				continue;
			}
			if( c == '\n' ) {
				cp = sp;
				return( badp );
			}
			if( c == 'n' ) {
				c = '\n';
			}
			goto defchar;

		case '\0':
			continue;
		case '\n':
			cp = sp;
			return( badp );

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if( lastep == 0 )
				goto defchar;
			if( *lastep == CKET ) {
				cp = sp;
				return( badp );
			}
			*lastep |= STAR;
			continue;

		case '$':
			if( *sp!= seof )
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if( &ep[17] >= &expbuf[ESIZE] )
				sederror( RETOLONG, linebuf );

			*ep++ = CCL;

			neg = 0;
			if( (c = *sp++) == '^' ) {
				neg = 1;
				c = *sp++;
			}

			cstart = sp;
			do	{
				if( c == '\0' )
					sederror( CMDGARBLED, linebuf );

				if( c=='-' && sp>cstart && *sp!=']' ) {
					for( c = sp[-2]; c<*sp; c++ )
						ep[c>>3] |= bittab[c&07];
				}
				if( c == '\\' ) {
					switch( c = *sp++ ) {
					case 'n':
						c = '\n';
						break;
					}
				}

				ep[c >> 3] |= bittab[c & 07];
			} while( (c = *sp++) != ']' );

			if( neg )
				for( cclcnt = 0; cclcnt < 16; cclcnt++ )
					ep[cclcnt] ^= -1;
			ep[0] &= 0376;

			ep += 16;

			continue;

defchar:
		default:
			*ep++ = CCHR;
			*ep++ = (char)c;
		}
	}
}

static	int
rline( lbuf )
char	*lbuf;
{
	static char	*saveq;
	char		*p;
	char		*q;
	int		t;

	p = lbuf - 1;

	if( eflag ) {
		if( eflag > 0 ) {
			eflag = -1;
			if( eargc-- <= 0 )
				exit( 2 );
			q = *++eargv;
			while( (*++p = *q++) != 0 ) {
				if( *p == '\\' ) {
					if( (*++p = *q++) == '\0' ) {
						saveq = 0;
						return( -1 );
					} else	continue;
				}
				if( *p == '\n' ) {
					*p = '\0';
					saveq = q;
					return( 1 );
				}
			}
			saveq = 0;
			return( 1 );
		}
		if( (q = saveq) == 0 )
			return( -1 );

		while( (*++p = *q++) != 0 ) {
			if( *p == '\\' ) {
				if( (*++p = *q++) == '0' ) {
					saveq = 0;
					return( -1 );
				} else	continue;
			}
			if( *p == '\n' ) {
				*p = '\0';
				saveq = q;
				return( 1 );
			}
		}
		saveq = 0;
		return( 1 );
	}

	while( (t = getc( fin )) != EOF ) {
		*++p = (char)t;
		if( *p == '\\' ) {
			t = getc( fin );
			*++p = (char)t;
		} else	if( *p == '\n' ) {
				*p = '\0';
				return( 1 );
		}
	}
	*++p = '\0';
	return( -1 );
}

static char *
address( expbuf )
char	*expbuf;
{
	char	*rcp;
	long	lno;

	if( *cp == '$' ) {
		cp++;
		*expbuf++ = CEND;
		*expbuf++ = CEOF;
		return( expbuf );
	}

	if( *cp == '/' ) {
		seof = '/';
		cp++;
		return( compile( expbuf ) );
	}

	rcp = cp;
	lno = 0;

	while( *rcp >= '0' && *rcp <= '9' )
		lno = lno * 10 + *rcp++ - '0';

	if( rcp > cp ) {
		*expbuf++ = CLNUM;
		*expbuf++ = (char)nlno;
		tlno[nlno++] = lno;

		if( nlno >= NLINES )
			sederror( TOOLINENUMS, NULL );

		*expbuf++ = CEOF;
		cp	  = rcp;
		return( expbuf );
	}
	return( 0 );
}

static int
cmp( a, b )
char	*a;
char	*b;
{
	char	*ra;
	char	*rb;

	ra = a - 1;
	rb = b - 1;

	while( *++ra == *++rb )
		if( *ra == '\0' )
			return( 0 );
	return( 1 );
}

static char *
text( textbuf )
char	*textbuf;
{
	char	*p;
	char	*q;

	p = textbuf;
	q = cp;

	while( *q == '\t' || *q == ' ' )
		q++;

	for( ;; ) {

		if( (*p = *q++) == '\\' )
			*p = *q++;
		if( *p == '\0' ) {
			cp = --q;
			return( ++p );
		}
		if( *p == '\n' )
			while( *q == '\t' || *q == ' ' )
				q++;
		p++;
	}
}

static struct label *
search( ptr )
struct label	*ptr;
{
	struct label	*rp;

	for( rp = labtab ; rp < ptr ; rp++ )
		if( cmp( rp->asc, ptr->asc ) == 0 )
			return( rp );

	return( 0 );
}

static void
dechain()
{
	struct label	*lptr;
	struct reptr	*rptr;
	struct reptr	*trptr;

	for( lptr = labtab ; lptr < lab ; lptr++ ) {

		if( lptr->address == 0 )
			sederror( UNDEFLABEL, lptr->asc );

		if( lptr->chain ) {
			rptr = lptr->chain;
			while( (trptr = rptr->lb1) != NULL ) {
				rptr->lb1 = lptr->address;
				rptr	  = trptr;
			}
			rptr->lb1 = lptr->address;
		}
	}
}

static char *
ycomp( expbuf )
char	*expbuf;
{
	int	c;
	char	*ep;
	char	*tsp;
	char	*sp;

	ep = expbuf;
	sp = cp;

	for( tsp = cp ; *tsp != seof ; tsp++ ) {
		if( *tsp == '\\' )
			tsp++;
		if( *tsp == '\n' )
			return( badp );
	}
	tsp++;

	while( (c = (int)(*sp++ & 0177)) != seof ) {
		if( c == (int)'\\' && *sp == 'n' ) {
			sp++;
			c = (int)'\n';
		}
		if( (ep[c] = *tsp++) == '\\' && *tsp == 'n' ) {
			ep[c] = '\n';
			tsp++;
		}
		if( ep[c] == seof || ep[c] == '\0' )
			return( badp );
	}
	if( *tsp != seof )
		return( badp );
	cp = ++tsp;

	for( c = 0 ; !(c & 0200) ; c++ )
		if( ep[c] == 0 )
			ep[c] = (char)c;

	return( ep + 0200 );
}

static	void
execute( file )
char	*file;
{
	char		*p1;
	char		*p2;
	struct reptr	*ipc;
	int		c;
	char		*execp;

	if( file ) {
		if( (fhandle = open(file, O_RDONLY)) < 0 ) {
			fprintf( stderr, CANTOPEN, file );
		}
	} else	fhandle = 0;

	ebp = ibuf;
	cbp = ibuf;

	if( pending ) {
		ipc = pending;
		pending = 0;
		goto yes;
	}

	for( ;; ) {
		if( (execp = gline( linebuf )) == badp ) {
			if( fhandle != -1 ) {
				close( fhandle );
				fhandle = -1;
			}
			return;
		}

		spend = execp;

		for( ipc = ptrspace; ipc->command; ) {

			p1 = ipc->ad1;
			p2 = ipc->ad2;

			if( p1 ) {

				if( ipc->inar ) {
					if( *p2 == CEND ) {
						/* not used :
						p1 = 0;
						 */
					} else	if( *p2 == CLNUM ) {
						c = p2[1];
						if( lnum > tlno[c] ) {
							ipc->inar = 0;
							if( ipc->negfl )
								goto yes;
							ipc++;
							continue;
						}
						if( lnum == tlno[c] ) {
							ipc->inar = 0;
						}
					} else	if( match( p2, 0 ) ) {
						ipc->inar = 0;
					}
				} else	if( *p1 == CEND ) {
					if( !dolflag ) {
						if( ipc->negfl )
							goto yes;
						ipc++;
						continue;
					}

				} else	if( *p1 == CLNUM ) {
					c = p1[1];
					if( lnum != tlno[c] ) {
						if( ipc->negfl )
							goto yes;
						ipc++;
						continue;
					}
					if( p2 )
						ipc->inar = 1;
				} else	if( match( p1, 0 ) ) {
						if( p2 )
							ipc->inar = 1;
				} else	{
					if( ipc->negfl )
						goto yes;
					ipc++;
					continue;
				}
			}

			if( ipc->negfl ) {
				ipc++;
				continue;
			}
yes:
			command( ipc );

			if( delflag )
				break;

			if( jflag ) {
				jflag = 0;
				if( (ipc = ipc->lb1) == 0 ) {
					break;
				}
			} else	ipc++;

		}

		if( !nflag && !delflag ) {
			for( p1 = linebuf ; p1 < spend ; p1++ )
				putchar( *p1 );
			putchar( '\n' );
		}

		if( aptr > abuf ) {
			arout();
		}

		delflag = 0;

	}
}

static int
match( expbuf, gf )
char	*expbuf;
int	gf;
{
	char	*p1;
	char	*p2;
	char	c;

	if( gf ) {
		if( *expbuf )
			return( 0 );
		p1 = linebuf;
		p2 = genbuf;
		while( (*p1++ = *p2++) != 0 )
			;
		locs = p1 = loc2;
	} else	{
		p1 = linebuf;
		locs = 0;
	}

	p2 = expbuf;
	if( *p2++ ) {
		loc1 = p1;
		if( *p2 == CCHR && p2[1] != *p1 )
			return( 0 );
		return( advance( p1, p2 ) );
	}

	/*
	 * fast check for first character
	 */

	if( *p2 == CCHR ) {
		c = p2[1];
		do	{
			if( *p1 != c )
				continue;
			if( advance( p1, p2 ) ) {
				loc1 = p1;
				return( 1 );
			}
		} while( *p1++ );
		return( 0 );
	}

	do
		if( advance( p1, p2 ) ) {
			loc1 = p1;
			return( 1 );
		}
	while( *p1++ );

	return( 0 );
}

static int
advance( alp, aep )
char	*alp;
char	*aep;
{
	char	*lp;
	char	*ep;
	char	*curlp;
	char	c;
	char	*bbeg;
	int	ct;

	lp = alp;
	ep = aep;
	for( ;; ) switch( *ep++ ) {

	case CCHR:
		if( *ep++ == *lp++ )
			continue;
		return( 0 );

	case CDOT:
		if( *lp++ )
			continue;
		return( 0 );

	case CNL:
	case CDOL:
		if( *lp == 0 )
			continue;
		return( 0 );

	case CEOF:
		loc2 = lp;
		return( 1 );

	case CCL:
		c = (char)(*lp++ & 0177);
		if( ep[c>>3] & bittab[c & 07] ) {
			ep += 16;
			continue;
		}
		return( 0 );

	case CBRA:
		braslist[(int)*ep++] = lp;
		continue;

	case CKET:
		braelist[(int)*ep++] = lp;
		continue;

	case CBACK:
		bbeg = braslist[(int)*ep];
		ct   = (int)(braelist[(int)*ep++] - bbeg);

		if( ecmp( bbeg, lp, ct ) ) {
			lp += ct;
			continue;
		}
		return( 0 );

	case CBACK|STAR:
		bbeg  = braslist[(int)*ep];
		ct    = (int)(braelist[(int)*ep++] - bbeg);
		curlp = lp;
		while( ecmp( bbeg, lp, ct ) )
			lp += ct;

		while( lp >= curlp ) {
			if( advance( lp, ep ) )
				return( 1 );
			lp -= ct;
		}
		return( 0 );

	case CDOT|STAR:
		curlp = lp;
		while( *lp++ )
			;
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while( *lp++ == *ep )
			;
		ep++;
		goto star;

	case CCL|STAR:
		curlp = lp;
		do
			c = (char)(*lp++ & 0177);
		while( ep[c>>3] & bittab[c & 07] );
		ep += 16;

star:
		if( --lp == curlp )
			continue;

		if( *ep == CCHR ) {
			c = ep[1];
			do	{
				if( *lp != c )
					continue;
				if( advance( lp, ep ) )
					return( 1 );
			} while( lp-- > curlp );
			return( 0 );
		}

		if( *ep == CBACK ) {
			c = *(braslist[(int)ep[1]]);
			do	{
				if( *lp != c )
					continue;
				if( advance( lp, ep ) )
					return( 1 );
			} while( lp-- > curlp );
			return( 0 );
		}

		do	{
			if( lp == locs )
				break;
			if( advance( lp, ep ) )
				return( 1 );
		} while( lp-- > curlp );
		return( 0 );

	default:
		fprintf( stderr, REBOTCH, *--ep );
	}
}

static	int
substitute( ipc )
struct reptr	*ipc;
{
	if( match( ipc->re1, 0 ) == 0 )
		return( 0 );

	sflag = 1;

	dosub( ipc->rhs );

	if( ipc->gfl )
		while( *loc2 ) {
			if( match( ipc->re1, 1 ) == 0 )
				break;
			dosub( ipc->rhs );
		}

	return( 1 );
}

static	void
dosub( rhsbuf )
char	*rhsbuf;
{
	char	*lp;
	char	*sp;
	char	*rp;
	int	c;

	lp = linebuf;
	sp = genbuf;
	rp = rhsbuf;

	while( lp < loc1 )
		*sp++ = *lp++;
	while( (c = *rp++) != 0 ) {
		if( c == '&' ) {
			sp = place( sp, loc1, loc2 );
			continue;
		} else	if( c&0200 && (c &= 0177) >= '1' && c < NBRA+'1' ) {
			    sp = place( sp, braslist[c-'1'], braelist[c-'1'] );
			    continue;
		}
		*sp++ = (char)(c&0177);
		if( sp >= &genbuf[LBSIZE] )
			fprintf( stderr, OUTLINELONG );
	}
	lp   = loc2;
	loc2 = sp - genbuf + linebuf;

	while( (*sp++ = *lp++) != 0 )
		if( sp >= &genbuf[LBSIZE] )
			fprintf( stderr, OUTLINELONG );

	lp = linebuf;
	sp = genbuf;
	while( (*lp++ = *sp++) != 0 )
		;
	spend = lp-1;
}

static	char *
place( asp, al1, al2 )
char	*asp;
char	*al1;
char	*al2;
{
	char	*sp;
	char	*l1;
	char	*l2;

	sp = asp;
	l1 = al1;
	l2 = al2;

	while( l1 < l2 ) {
		*sp++ = *l1++;
		if( sp >= &genbuf[LBSIZE] )
			fprintf( stderr, OUTLINELONG );
	}
	return( sp );
}

static	void
command( ipc )
struct reptr	*ipc;
{
	int	i;
	char	*p1;
	char	*p2;
	char	*p3;
	char	*execp;

	switch( ipc->command ) {
	case ACOM:
		*aptr++ = ipc;
		if( aptr >= &abuf[ ABUFSIZE ] )
			fprintf( stderr, TOOAPPENDS, lnum );
		*aptr = 0;
		break;
	case CCOM:
		delflag = 1;
		if( !ipc->inar || dolflag ) {
			for( p1 = ipc->re1 ; *p1 ; )
				putchar( *p1++ );
			putchar( '\n' );
		}
		break;
	case DCOM:
		delflag++;
		break;
	case CDCOM:
		p1 = p2 = linebuf;

		while( *p1 != '\n' ) {
			if( *p1++ == 0 ) {
				delflag++;
				return;
			}
		}

		p1++;
		while( (*p2++ = *p1++) != 0 )
			;
		spend = p2-1;
		jflag++;
		break;
	case EQCOM:
		fprintf( stdout, "%ld\n", lnum );
		break;
	case GCOM:
		p1 = linebuf;
		p2 = holdsp;
		while( (*p1++ = *p2++) != 0 )
			;
		spend = p1-1;
		break;
	case CGCOM:
		*spend++ = '\n';
		p1 = spend;
		p2 = holdsp;
		while( (*p1++ = *p2++) != 0 )
			if( p1 >= clbend )
				break;
		spend = p1-1;
		break;
	case HCOM:
		p1 = holdsp;
		p2 = linebuf;
		while( (*p1++ = *p2++) != 0 )
			;
		hspend = p1-1;
		break;
	case CHCOM:
		*hspend++ = '\n';
		p1 = hspend;
		p2 = linebuf;
		while( (*p1++ = *p2++) != 0 )
			if( p1 >= hend )
				break;
		hspend = p1-1;
		break;
	case ICOM:
		for( p1 = ipc->re1 ; *p1 ; )
			putchar( *p1++ );
		putchar( '\n' );
		break;
	case BCOM:
		jflag = 1;
		break;
	case LCOM:
		p1 = linebuf;
		p2 = genbuf;
		genbuf[72] = 0;
		while( *p1 )
			if( *p1 >= 040 ) {
				if( *p1 == 0177 ) {
					p3 = rub;
					while( (*p2++ = *p3++) != 0 )
					     if( p2 >= lcomend ) {
					     	*p2 = '\\';
						fprintf(stdout, "%s\n", genbuf);
						p2 = genbuf;
					     }
					p2--;
					p1++;
					continue;
				}
				*p2++ = *p1++;
				if( p2 >= lcomend ) {
					*p2 = '\\';
					fprintf( stdout, "%s\n", genbuf );
					p2 = genbuf;
				}
			} else	{
				p3 = trans[*p1-1];
				while( (*p2++ = *p3++) != 0 )
					if( p2 >= lcomend ) {
						*p2 = '\\';
						fprintf(stdout, "%s\n", genbuf);
						p2 = genbuf;
					}
				p2--;
				p1++;
			}
		*p2 = 0;
		fprintf( stdout, "%s\n", genbuf );
		break;
	case NCOM:
		if( !nflag ) {
			for( p1 = linebuf ; p1 < spend ; p1++ )
				putchar( *p1 );
			putchar( '\n' );
		}

		if( aptr > abuf )
			arout();

		if( (execp = gline( linebuf )) == badp ) {
			pending = ipc;
			delflag = 1;
			break;
		}
		spend = execp;
		break;
	case CNCOM:
		if( aptr > abuf )
			arout();

		*spend++ = '\n';
		if( (execp = gline( spend )) == badp ) {
			pending = ipc;
			delflag = 1;
			break;
		}
		spend = execp;
		break;
	case PCOM:
		for( p1 = linebuf ; p1 < spend ; p1++ )
			putchar( *p1 );
		putchar( '\n' );
		break;
	case CPCOM:
cpcom:
		for( p1 = linebuf; *p1 != '\n' && *p1 != '\0'; )
			putchar( *p1++ );
		putchar( '\n' );
		break;
	case QCOM:
		if( !nflag ) {
			for( p1 = linebuf ; p1 < spend ; p1++ )
				putchar( *p1 );
			putchar( '\n' );
		}
		if( aptr > abuf )
			arout();
		fclose( stdout );
		exit( 0 );
	case RCOM:
		*aptr++ = ipc;
		if( aptr >= &abuf[ABUFSIZE] )
			fprintf( stderr, TOOREADS, lnum );
		*aptr = 0;

		break;
	case SCOM:
		i = substitute( ipc );
		if( ipc->pfl && i ) {
			if( ipc->pfl == 1 ) {
				for( p1 = linebuf ; p1 < spend ; p1++ )
					putchar( *p1 );
				putchar( '\n' );
			} else	goto cpcom;
		}
		if( i && ipc->fcode )
			goto wcom;
			break;
	case TCOM:
		if( sflag == 0 )
			break;
		sflag = 0;
		jflag = 1;
		break;
wcom:
	case WCOM:
		fprintf( ipc->fcode, "%s\n", linebuf );
		break;
	case XCOM:
		p1 = linebuf;
		p2 = genbuf;
		while( (*p2++ = *p1++) != 0 )
			;
		p1 = holdsp;
		p2 = linebuf;
		while( (*p2++ = *p1++) != 0 )
			;
		spend = p2 - 1;
		p1 = genbuf;
		p2 = holdsp;
		while( (*p2++ = *p1++) != 0 )
			;
		hspend = p2 - 1;
		break;
	case YCOM:
		p1 = linebuf;
		p2 = ipc->re1;
		while( (*p1 = p2[(int)*p1]) != 0 )
			p1++;
		break;
	}
}

static	char *
gline( addr )
char	*addr;
{
	char	*p1;
	char	*p2;
	int	c;

	p1 = addr;
	p2 = cbp;

	for( ;; ) {
		if( p2 >= ebp ) {
			/*
			 * We  are  at the end of the buffer,  try to
			 * read more character form the input stream.
			 */

			if( p1 > addr && fhandle == -1 ) {
				/*
				 * This  test  has  been added to let
				 * the   sed  treat  the  last  line,
				 * event  if  it  does not end with a
				 * new line. CJ 1997/02/23.
				 */
				break;
			}

			if( fhandle == -1 )
				/*
				 * the handle is already closed.
				 */
				return( badp );

			if( (c = read(fhandle, ibuf, BUFREAD)) <= 0 ) {
				close(fhandle);
				fhandle = -1;
				if( eargc == 0 )
					dolflag = 1;
			}

			p2  = ibuf;
			ebp = ibuf + c;
		}

		if( (c = *p2++) == '\n' ) {
			if( p2 >= ebp ) {
				if( (c = read(fhandle, ibuf, BUFREAD)) <= 0 ) {
					close(fhandle);
					fhandle = -1;
					if( eargc == 0 )
						dolflag = 1;
				}

				p2  = ibuf;
				ebp = ibuf + c;
			}
			break;
		}
		if( c )
			if( p1 < clbend )
				*p1++ = (char)c;
	}
	lnum++;
	*p1 = 0;
	cbp = p2;

	return( p1 );
}

static int
ecmp( a, b, count )
char	*a;
char	*b;
int	count;
{
	while( count-- )
		if( *a++ != *b++ )
			return( 0 );
	return( 1 );
}

static void
arout()
{
	FILE	*fi;
	char	*p1;
	char	c;
	int	t;

	aptr = abuf - 1;
	while( *++aptr )
		if( (*aptr)->command == ACOM ) {
			for( p1 = (*aptr)->re1 ; *p1 ; )
				putchar( *p1++ );
			putchar( '\n' );
		} else	{
			if( (fi = fopen( (*aptr)->re1, "r" )) == NULL )
				continue;
			while( (t = getc( fi )) != EOF ) {
				c = (char)t;
				putchar( c );
			}
			fclose( fi );
		}

	aptr	= abuf;
	*aptr	= 0;
}
