#if	!defined( lint )
static	char rcsid[] = "$Id: xmalloc.c,v 1.3 2008/04/04 18:42:31 jullien Exp $";
#endif	/* lint */

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
 *	@(#)xmalloc.c :
 *
 *	This  is  a very fast storage allocator.  It allocates blocks
 *	of  a  small number of different sizes,  and keeps free lists
 *	of each size.  Blocks that don't exactly fit are passed up to
 *	the next larger size.  In this implementation,  the available
 *	sizes  are (2^n)-4 (or -16) bytes long.  This is designed for
 *	use  in  a  program that uses vast quantities of memory,  but
 *	bombs when it runs out.
 *
 *	nextfree[i]  is  the pointer to the next free memory block of
 *	size  2  ^ (i+MINBLOCK).  The smallest allocatable block size
 *	is  SMALLEST_SIZE bytes.  The overhead information will go in
 *	the  first  int  of the block,  and the returned pointer will
 *	point to the second.
 *
 *	nmalloc[i]  is  the  difference between the number of mallocs
 *	and  frees  for  a given block size.  It is defined only when
 *	_XALLOX_STATS is on.
 *
 *	These  routines  are  also  tuned to C++ in that free(0) is a
 *	noop    and    a    failed    malloc    automatically   calls
 *	(*_new_handler)().  If  your  compiler  supports the separate
 *	redefinitions  of new and delete for arrays that is new[] and
 *	delete[] operators, you must set _NEW_ARRAY_OPERATOR.
 *
 *	Note  :  this  file  use protypes so that it can compile with
 *	ISO/ANSI  C  compiles  as  well  as  C++ compilers.  You must
 *	remove declarations if you still use Old-C compilers.
 */

#if	!defined( _XALLOC_DEFAULT )

#if	defined( _WINDOWS ) || defined( _WIN32 ) || defined( _WIN64 )
#include <windows.h>
#endif	/* _WINDOWS */

#if	defined( unix ) || defined( _POSIX_SOURCE ) || defined( _XOPEN_SOURCE )
#include <unistd.h>
#endif	/* unix || _POSIX_SOURCE || _XOPEN_SOURCE */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if	defined( _DLL )
#if	!defined( _XIMPORT )
#define _XIMPORT __declspec(dllexport)
#endif  /* _XIMPORT */
#else   /* _DLL */
#if	!defined( _XIMPORT )
#define _XIMPORT
#endif  /* _XIMPORT */
#endif  /* _DLL */

#include "xmem.h"

#if	defined( __cplusplus )
extern "C" {
#endif	/* __cplusplus */

#define	NO_XALLOC_DEBUG		/* defined when debugging is wanted	   */
#define	NO_XALLOC_STATS		/* defined when stats wanted		   */
#define	NO_XALLOC_RANGE_CHECK	/* defined when range checking wanted	   */
#define	NO_XALLOC_TYPE_CHECK	/* defined when array checking wanted	   */
#define	NO_XALLOC_TRACE		/* defined when trace __FILE__,__LINE__ on */

#if	!defined( _XALLOC_ALIGN )
#define	_XALLOC_ALIGN	8	/* allocation alignment		*/
#endif	/* _XALLOC_ALIGN */

#if	defined( _XALLOC_REPLACE )
#if	defined( free )
#undef	free
#endif	/* free */
#if	defined( malloc )
#undef	malloc
#endif	/* malloc */
#if	defined( realloc )
#undef	realloc
#endif	/* realloc */
#if	defined( calloc )
#undef	calloc
#endif	/* calloc */

_XIMPORT void	free(void *mem)			  { _xfree(mem);		      }
_XIMPORT void *	malloc(size_t size)		  { return( _xmalloc(size) );	      }
_XIMPORT void *	calloc(size_t nelem, size_t size) { return( _xcalloc(nelem, size) );  }
_XIMPORT void *	realloc(void *mem, size_t nbytes) { return( _xrealloc(mem, nbytes) ); }

#endif	/* _XALLOC_REPLACE */

static	void	*sysgetmem( size_t size );
static	int	morecore( int bucket );
static	void	getpool( void );

static	int	gotpool = 0;	/* flag initial free blocks	*/

#if	defined( _XALLOC_TRACE ) || defined( _XALLOC_STATS )
int	_traceflag = 0;

static	void	_ftrace( const char* o, size_t s, void* p );
static	FILE*	_tracefd = NULL;
static	FILE*	_logfd   = stderr;

#define	trace( o, s, p )	_ftrace( o, s, p )
#define	traceon()		_traceflag = 1
#define	traceoff()		_traceflag = 0

static	void
_ftrace( const char* o, size_t s, void* p )
{
	/*
	 * WARNING!!  we  use  C  printf functions to avoid confusion
	 * with operator<< that may use new or delete internally.
	 */

	if( !gotpool )
		getpool();

	if( _traceflag ) {
	    if( _tracefd == NULL )
		_tracefd = fopen( "newdel.log", "w" );

	    if( s >= 0 )
	    	 (void)fprintf(_tracefd, "%-9s: %p (%d bytes)\n", o, p, s);
	    else (void)fprintf(_tracefd, "%-9s: %p\n", o, p);
	}
}

#else	/* _XALLOC_TRACE || _XALLOC_STATS */
#define	trace( o, s, p )
#define	traceon()
#define	traceoff()
#endif	/* _XALLOC_TRACE || _XALLOC_STATS */

typedef	unsigned char _byte;		/* small int in the range 0,255 */
typedef	unsigned int  _int4;		/* 32 bit integer		*/

#define ISALLOC		((_byte)0xF7)	/* magic byte for allocation	*/
#define ISFREE		((_byte)0x54)	/* magic byte for free block	*/
#define	ISTAGED		((_byte)0xD9)	/* magic byte for tagged block	*/

#define	MAXBLOCK	30		/* max number of blocks		*/
#define	MINBLOCK	3		/* min block size (1<<3) == 8	*/
#define	BLOCKSIZE	8		/* i.e. 1<<MINBLOCK bytes	*/
#define	SMALLEST_SIZE	8		/* smallest allocation size	*/

#if	defined( _M_IA64 ) || defined( _IA64 ) || defined( _WIN64 )
typedef	unsigned __int64	XULPTR;
#else
typedef	unsigned long		XULPTR;
#endif	/* _M_IA64 || _IA64 || _WIN64 */

#define	ALIGN(x)    (((XULPTR)(x)+(_XALLOC_ALIGN-1))&~(_XALLOC_ALIGN-1))
#define	ISALIGN(x)  (((XULPTR)(x) & (_XALLOC_ALIGN-1)) == 0)

#if	defined( _XALLOC_STATS )
static	long	nmalloc[ MAXBLOCK ];	/* stats between mallocs/frees	*/
static	long	allocount;		/* number of allocations	*/
static	long	freecount;		/* number of frees		*/
#endif	/* _XALLOC_STATS */

/*
 *	If  range  checking  is not turned on,  all we have is a flag
 *	indicating   whether   memory   is  allocated,  an  index  in
 *	nextfree[],  and  a  size field; to xrealloc() memory we copy
 *	either  size  bytes or 1<<(index+MINBLOCK) bytes depending on
 *	whether  the  former can hold the exact size (given the value
 *	of 'index').  If range checking is on, we always need to know
 *	how  much  space  is allocated,  so the 'size' field is never
 *	used.
 */

struct	mhead	{
	_byte		mh_alloc;	/* ISALLOC or ISFREE		*/
	_byte		mh_index;	/* index in nextfree[]		*/
#if	defined( _XALLOC_TYPE_CHECK )
	_byte		mh_type;	/* allocation type		*/
	_byte		mh_pad1;	/* padded byte (not used)	*/
#else	/* _XALLOC_TYPE_CHECK */
	_byte		mh_pad0;	/* padded byte (not used)	*/
	_byte		mh_pad1;	/* padded byte (not used)	*/
#endif	/* _XALLOC_TYPE_CHECK */
	size_t		mh_size;	/* size, if < 0x10000		*/
#if	defined( _XALLOC_TRACE )
	struct mhead *	mh_prev;	/* prev allocated block		*/
	struct mhead *	mh_next;	/* next allocated block		*/
	const char *	mh_file;	/* file that make allocation	*/
	_int4		mh_line;	/* line that make allocation	*/
#endif	/* _XALLOC_TRACE */
#if	defined( _XALLOC_RANGE_CHECK ) || defined( _XALLOC_TRACE )
	size_t		mh_nbytes;	/* number of bytes allocated	*/
	_int4	 	mh_magic4;	/* should be == MAGIC4		*/
#endif	/* _XALLOC_RANGE_CHECK || _XALLOC_TRACE */
};

typedef	struct mhead 	MHEAD;
typedef	struct mhead *	PMHEAD;

static	PMHEAD	nextfree[MAXBLOCK];	/* free list of size 2**(i+MINBLOCK) */

#define	HEADSIZE		(ALIGN( sizeof( MHEAD ) ))
#define	HEADPTR( ptr ) 		((PMHEAD)((char *)ptr - HEADSIZE))

#define	ALLOC_NEW_ARRAY		((_byte)0xA7)
#define	ALLOC_NEW_NOARRAY	((_byte)0xB7)
#define	ALLOC_MALLOC		((_byte)0xC7)

/*
 *	Access  free-list  pointer  of  a  block.  It  is  stored  at
 *	block+4.  This  is not a field in the mhead structure because
 *	sizeof(  struct  mhead  ) must describe the overhead for when
 *	the  block  is  in  use,  and  we  do  not want the free-list
 *	pointer to count in that.
 */

#define CHAIN( a )	(*(PMHEAD *)(sizeof( char * ) + (char *)( a )))

static	int	xabort( void );

#define DONT_ABORT_ON_ERRORS

static	int
xabort( void )
{
#if	defined( _ABORT_ON_ERRORS )
	abort();
#endif	/* _ABORT_ON_ERRORS */
	return( 0 );
}

static void _xmalloc_error( const char *e, MHEAD* p, const char *f, unsigned int l );

#if	defined( __LCC__ )
static void * _xmalloc_unused( void* p );
static void *
_xmalloc_unused( void* p )
{
	return( p );
}
#else
#define	_xmalloc_unused( p )	((void)p)
#endif

static void
_xmalloc_error( const char *e, MHEAD *p, const char *f, unsigned int l )
{
#if	defined( _XALLOC_TRACE )
	if( p && p->mh_file )
		if( p->mh_alloc == ISFREE || p->mh_alloc == ISALLOC )
			(void)fprintf(
				       _logfd,
				       "Block from %s(%d)\n\r",
				       p->mh_file,
				       p->mh_line
				     );
		else	(void)fprintf( _logfd, "Block at address %p\n\r", p );
#else	/* _XALLOC_TRACE */
	_xmalloc_unused( &p );
#endif	/* _XALLOC_TRACE */

#if	defined( _XALLOC_DEBUG )
	(void)fprintf( _logfd, "Assertion '%s' fails: %s(%d)\n\r", e, f, l );
 	(void)fflush( _logfd ); /* just in case user buffered it */
#else	/* _XALLOC_DEBUG */
	_xmalloc_unused( &e );
	_xmalloc_unused( &f );
	_xmalloc_unused( &l );
#endif	/* _XALLOC_DEBUG */

	(void)xabort();
}

#define	XALLOC_ERROR( e, p ) _xmalloc_error( e, p, __FILE__, __LINE__ )

#if	defined( _XALLOC_DEBUG )
#define ASSERT(c,p)	(void)((c)||(_xmalloc_error(#c,p,__FILE__,__LINE__),0))
#else	/* _XALLOC_DEBUG */
#define ASSERT(c,p)	(void)((c)||xabort())
#endif	/* _XALLOC_DEBUG */

#if	defined( _XALLOC_RANGE_CHECK )

/*
 *	To implement range checking,  we write magic values in at the
 *	beginning  and  end  of  each allocated block,  and make sure
 *	they are undisturbed whenever a xfree or a xrealloc occurs.
 */

#define MAGIC1	0x55		/* 4 bytes following the block's real space */
#define MAGIC4	0x55555555	/* 4 bytes before the block's real space    */
#define EXTRA	4		/* 4 bytes extra for MAGIC1s		    */
#else	/* _XALLOC_RANGE_CHECK */
#define EXTRA	0		/* 0 bytes extra for MAGIC1s		    */
#endif	/* _XALLOC_RANGE_CHECK */

/*
 *	Ask  system  for more memory size index.  The first time,  it
 *	allocate  two  block  of each size up to 1k bytes and a block
 *	of the requested size.
 */

static int
morecore( int nu )
{
	char	*cp;
	int	nblks;
	size_t	size;

	/*
	 *	On  initial  startup,  get two blocks of each size up
	 *	to 1k bytes
	 */

	if( !gotpool )
		getpool();

	/*
	 *	Take  at least 2k,  and figure out how many blocks of
	 *	the desired size we're about to get
	 */

	nblks = 1;
	size  = nu;

	if( size < BLOCKSIZE ) {
		size  = BLOCKSIZE;
		nblks = 1 << (BLOCKSIZE - nu);
	}

	if( (cp = (char *)sysgetmem( 1 << (size + MINBLOCK))) == NULL )
		/*
		 *	No more room!
		 */
		return( -1 );

	if( ISALIGN( cp ) ) {
		/*
		 * shouldn't happen, but just in case ...
		 */
		cp = (char *)ALIGN( cp );
		nblks--;
	}

	/*
	 *	save new header and link the nblks blocks together
	 */

	nextfree[ nu ] = (PMHEAD)cp;
	size	       = (1 << (nu + MINBLOCK));

	for( ;; ) {
		((PMHEAD)cp)->mh_alloc = ISFREE;
		((PMHEAD)cp)->mh_index = (_byte)nu;
		if( --nblks <= 0 )
			break;
		CHAIN( (PMHEAD)cp ) = (PMHEAD)( cp + size );
		cp += size;
	}

	CHAIN( (PMHEAD)cp ) = NULL;

	return( 0 );
}

#if	(_XALLOC_ALIGN <= 64)

/*
 *	Build  initial free list for the first blocks.  Get one block
 *	of each size up to 1K bytes.  It's called twice at startup to
 *	allocate 2 blocks of each size: 8, 16, 32, 64, 128, 256, 512.
 */

static	void
getpool( void )
{
	int	nu;
	char	*cp;
	int	i;

	ASSERT( ((_XALLOC_ALIGN % 4) == 0), 0 ); /* power of 2 >= 4	  */
	ASSERT( (SMALLEST_SIZE == 8),       0 ); /* smallest size == 8	  */
	ASSERT( (sizeof( _int4 ) == 4),     0 ); /* _int4 is 4 bytes long */

	for( i = 0 ; i < 2 ; i++ ) {

		/*
		 *	Get initial block for storage
		 */

		if( (cp = (char *)sysgetmem(1<<(BLOCKSIZE+MINBLOCK))) == NULL )
			return;

		/*
		 *	Divide  it  into an initial 8-word block plus
		 *	one block of size 2**nu for nu=MINBLOCK..10.
		 */

		CHAIN( cp )   = nextfree[ 0 ];
		nextfree[ 0 ] = (PMHEAD)cp;
		((PMHEAD)cp)->mh_alloc = ISFREE;
		((PMHEAD)cp)->mh_index = 0;
		cp = (char *)ALIGN( cp + SMALLEST_SIZE );

		/*
		 *	Block from 8 to 512 bytes.
		 */

		for( nu = 0 ; nu < (BLOCKSIZE-1) ; nu++ ) {
			CHAIN( cp )    = nextfree[ nu ];
			nextfree[ nu ] = (PMHEAD)cp;
			((PMHEAD)cp)->mh_alloc = ISFREE;
			((PMHEAD)cp)->mh_index = (_byte)nu;
			cp = (char *)ALIGN( cp  + (SMALLEST_SIZE << nu) );
		}
	}

	gotpool = 1;

}

#else	/* _XALLOC_ALIGN <= 64 */

static	void
getpool( void )
{
	gotpool = 1;

}

#endif	/* _XALLOC_ALIGN <= 64 */

#if	defined( _XALLOC_TRACE )

/*
 *	The  following  functions  are  used to save trace (as file +
 *	line) of the calling routine.  When trace is ON,  headers are
 *	4  pointers  bigger  to  save  __FILE__,  __LINE__ and doubly
 *	linked list informations.
 */

static void	xmalloc_remove_node( PMHEAD ptr, int index );
static void	xmalloc_add_node( PMHEAD ptr, int index );
static void	xmalloc_tag_used( void );

static	PMHEAD	nextused[MAXBLOCK];	/* used list of size 2**(i+MINBLOCK) */

static	const char*	_trace_file;
static  int		_trace_line;

_XIMPORT int
_xmalloc_set_tag( const char* file, int line )
{
	_trace_file = file;
	_trace_line = line;

	return( line );
}

_XIMPORT void *
_xmalloc_tag_block( void* mem, const char* file, int line )
{
	PMHEAD	p;

	if( mem == NULL )
		return( NULL );

	p = HEADPTR( mem );

	ASSERT( (p->mh_alloc == ISALLOC), p );

	if( _trace_file ) {
		p->mh_file  = _trace_file;
		p->mh_line  = _trace_line;
		_trace_file = (const char*)0;
		_trace_line = 0;
	} else	{
		p->mh_file  = file;
		p->mh_line  = (_int4)line;
	}

	return( mem );
}

/*
 *	Traced malloc defined as a macro in xmem.h
 */

_XIMPORT void *
_xmalloc_trace_malloc( size_t size, const char *file, int line )
{
	return( _xmalloc_tag_block( _xmalloc( size ), file, line ) );
}

/*
 *	Traced calloc defined as a macro in xmem.h
 */

_XIMPORT void *
_xmalloc_trace_calloc( size_t nelem, size_t size, const char *file, int line )
{
	return( _xmalloc_tag_block( _xcalloc( nelem, size ), file, line ) );
}

/*
 *	Traced realloc defined as a macro in xmem.h
 */

_XIMPORT void *
_xmalloc_trace_realloc( void *mem, size_t size, const char *file, int line )
{
	return( _xmalloc_tag_block( _xrealloc( mem, size ), file, line ) );
}

/*
 *	Add  a  new  allocated  node  in nextused array.  The node is
 *	always inserted on top. Take care of the doubly linked-list.
 */

static void
xmalloc_add_node( PMHEAD ptr, int index )
{
	PMHEAD	p = nextused[ index ];

	ptr->mh_next = p;
	ptr->mh_prev = 0;
	ptr->mh_pad1 = (_byte)0;

	if( p )
		p->mh_prev = ptr;

	nextused[ index ] = ptr;
}

/*
 *	Remove  an  allocated node from nextused array.  The node may
 *	be  everywhere  in the doubly linked-list.  To prevent bug if
 *	the node is deleted twice, the link must be broken.
 */

static void
xmalloc_remove_node( PMHEAD ptr, int index )
{
	PMHEAD	prev = ptr->mh_prev;
	PMHEAD	next = ptr->mh_next;

	if( prev )
		prev->mh_next     = next;
	else	nextused[ index ] = next;

	if( next )
		next->mh_prev = prev;

	ptr->mh_next = 0;
	ptr->mh_prev = 0;
	ptr->mh_pad1 = (_byte)0;
}

/*
 *	Tag  all  allocated  blocks  so that when we find an untagged
 *	block, it's a leak candidate.
 */

static void
xmalloc_tag_used( void )
{
	int	i;

	for( i = 0 ; i < MAXBLOCK ; ++i ) {
		PMHEAD	p;
		for( p = nextused[i] ; p ; p = p->mh_next )
			p->mh_pad1 = (_byte)1;
	}
}

/*
 *	Find untagged block and display file and line origine.
 */

static void
xmalloc_find_untagged( void )
{
	static const char unknown[] = "'unknown module' ";

	PMHEAD	p;
	int	i;

	for( i = 0 ; i < MAXBLOCK ; ++i )
	     for( p = nextused[i] ; p ; p = p->mh_next )
		  if( p->mh_pad1 == (_byte)0 )
		      if( p->mh_file )
			   (void)fprintf(
			   		  _logfd,
					  "%9d bytes leaks from %s(%d)\n\r",
					  p->mh_nbytes,
					  p->mh_file,
					  p->mh_line
				       );
		      else (void)fprintf(
		      			  _logfd,
					  "%8d bytes leaks from %s\n\r",
					  p->mh_nbytes,
					  unknown
					);
}

#endif	/* _XALLOC_TRACE */

/*
 *	get a block of n bytes.
 */

_XIMPORT void	*
_xmalloc( size_t n )
{
	PMHEAD	p;
	size_t	nbytes;
	size_t	shiftr;
	int	nunits;
#if	defined( _XALLOC_RANGE_CHECK )
	char	*m;
#endif	/* _XALLOC_RANGE_CHECK */

	/*
	 *	Figure  out how many bytes are required,  rounding up
	 *	to   the  nearest  multiple  of  _XALLOC_ALIGN,  then
	 *	figure  out  which  nestf[]  area  to  use.  Both the
	 *	beginning  of  the  header  and  the beginning of the
	 *	block should be on an eight byte boundary.
	 */

	nbytes = (size_t)ALIGN(n + HEADSIZE + EXTRA);
	shiftr = (nbytes - 1) >> 2;

	/*
	 *	Find the bucket by shifting right until 0.
	 */

	for( nunits = 0 ; (shiftr >>= 1) != 0 ; ++nunits )
		continue;

	/*
	 *	If  there  are no blocks of the appropriate size,  go
	 *	get some
	 */

	if( nextfree[ nunits ] == 0 )
		if( morecore( nunits ) == -1 )
			return( NULL );

	/*
	 *	Get one block off the list, and set the new list head
	 */

	if( (p = nextfree[ nunits ]) == 0 )
		return( NULL );

	nextfree[ nunits ] = CHAIN( p );

	/*
	 *	Check  for  free  block  clobbered.  If  not for this
	 *	check,  we  would  gobble  a clobbered free chain ptr
	 *	and bomb out on the NEXT allocate of this size block
	 */

	if( p->mh_alloc != ISFREE || p->mh_index != (_byte)nunits ) {
		XALLOC_ERROR( "_xmalloc: block on free list clobbered", p );
		return( 0 );
	}

	/*
	 *	Fill in the info,  and if range checking,  set up the
	 *	magic numbers
	 */

	p->mh_alloc = ISALLOC;

#if	defined( _XALLOC_TYPE_CHECK )
	p->mh_type  = ALLOC_MALLOC;
#endif	/* _XALLOC_TYPE_CHECK */

#if	defined( _XALLOC_RANGE_CHECK )
	p->mh_nbytes = n;
	p->mh_magic4 = MAGIC4;

	/*
	 *	Get  the location n after the beginning of the user's
	 *	space.
	 */

	m = (char *)p + HEADSIZE + n;

	*m++	= MAGIC1;
	*m++	= MAGIC1;
	*m++	= MAGIC1;
	*m	= MAGIC1;

#else	/* _XALLOC_RANGE_CHECK */
	p->mh_size = n;
#endif	/* _XALLOC_RANGE_CHECK */

#if	defined( _XALLOC_TRACE )
	p->mh_file = (const char*)0;
	p->mh_line = 0;
#endif	/* _XALLOC_TRACE */

#if	defined( _XALLOC_STATS )
	nmalloc[ nunits ]++;
	allocount++;
#endif	/* _XALLOC_STATS */

#if	defined( _XALLOC_TRACE )
	xmalloc_add_node( p, nunits );
#endif	/* _XALLOC_TRACE */

#if	defined( _XALLOC_DEBUG ) && defined( _CHECK_ALIGN )
	if( !ISALIGN( (void *)((char *)p + HEADSIZE)) )
		xabort();
#endif	/* _XALLOC_DEBUG && _CHECK_ALIGN */

	return( (void *)((char *)p + HEADSIZE) );
}

/*
 *	Get a cleared block.
 */

_XIMPORT void *
_xcalloc( size_t nelem, size_t size )
{
	void*	mem = _xmalloc( size *= nelem );

#if	defined( _BSD )
	(void)bzero( mem, size );
#else	/* _BSD */
	(void)memset( mem, 0, size );
#endif	/* _BSD */

	return( mem );
}

_XIMPORT void
_xfree( void *mem )
{
	PMHEAD	p;
	char	*ap = (char *)mem;
	int	nunits;

	if( ap == NULL )
		return;

	p = HEADPTR( ap );

#if	defined( _XALLOC_TYPE_CHECK )
	ASSERT( (p->mh_type == ALLOC_MALLOC), p );
#endif	/* _XALLOC_TYPE_CHECK */

#if	!defined( _XALLOC_RANGE_CHECK )
	if( p->mh_alloc != ISALLOC )
		xabort();
#else	/* _XALLOC_RANGE_CHECK */
	if( p->mh_alloc != ISALLOC ) {
		if( p->mh_alloc == ISFREE )
			XALLOC_ERROR("xfree: block on free list clobbered", p);
		else	XALLOC_ERROR("xfree: invalid block",p);
		return;
	}

	/*
	 *	check if user has written before it's address space
	 */

	
	ASSERT( (p->mh_magic4 == MAGIC4), p );

	/*
	 *	check if user has written after it's address space
	 */

	ap += p->mh_nbytes;

	ASSERT( (*ap++ == MAGIC1), p );
	ASSERT( (*ap++ == MAGIC1), p );
	ASSERT( (*ap++ == MAGIC1), p );
	ASSERT( (*ap   == MAGIC1), p );
#endif	/* _XALLOC_RANGE_CHECK */

	nunits = (int)(p->mh_index);
	ASSERT( (nunits < MAXBLOCK), p );
	p->mh_alloc = ISFREE;

	/*
	 *	Put this block on the free list.
	 */

	CHAIN( p )       = nextfree[ nunits ];
	nextfree[nunits] = p;

#if	defined( _XALLOC_TRACE )
	xmalloc_remove_node( p, nunits );
#endif	/* _XALLOC_TRACE */

#if	defined( _XALLOC_STATS )
	nmalloc[ nunits ]--;
	freecount++;
#endif	/* _XALLOC_STATS */
}

_XIMPORT void	*
_xrealloc( void *mem, size_t n )
{
	PMHEAD	p;
	size_t	tocopy;
	size_t	nbytes;
	size_t	nunits;
	char	*newmem;

	if( mem == NULL )
		return( _xmalloc( n ) );

	if( n == 0 ) {
		_xfree( mem );
		return( NULL );
	}

	p       = HEADPTR( mem );
	nunits	= (size_t)(p->mh_index);

	ASSERT( (p->mh_alloc == ISALLOC), p );

#if	defined( _XALLOC_RANGE_CHECK )
	newmem = (char *)mem + (tocopy = p->mh_nbytes);
	ASSERT( (p->mh_magic4 == MAGIC4), p );
	ASSERT( (*newmem++    == MAGIC1), p );
	ASSERT( (*newmem++    == MAGIC1), p );
	ASSERT( (*newmem++    == MAGIC1), p );
	ASSERT( (*newmem      == MAGIC1), p );
#else	/* _XALLOC_RANGE_CHECK */
	if( p->mh_index >= 13 )
		/*
		 *	64 k or more
		 */
		tocopy = (size_t)((1<<(int)(p->mh_index+MINBLOCK))-HEADSIZE);
	else	tocopy = p->mh_size;
#endif	/* _XALLOC_RANGE_CHECK */

	/*
	 *	See if desired size rounds to same power of 2 as actual size.
	 */

	nbytes = (size_t)ALIGN( n + HEADSIZE + EXTRA );

	/*
	 *	If ok, use the same block, just marking its size as changed.
	 */

	if( (nbytes  >   ((unsigned)4<<nunits)) &&
	    (nbytes  <=  ((unsigned)8<<nunits)) ) {
#if	defined( _XALLOC_RANGE_CHECK )
		char	*m;

		m    = (char *)mem + tocopy;
		*m++ = 0;
		*m++ = 0;
		*m++ = 0;
		*m   = 0;
		p->mh_nbytes = n;
		m    = (char *)mem + n;
		*m++ = MAGIC1;
		*m++ = MAGIC1;
		*m++ = MAGIC1;
		*m   = MAGIC1;
#else	/* _XALLOC_RANGE_CHECK */
		p->mh_size = n;
#endif	/* _XALLOC_RANGE_CHECK */
		return( mem );
	}

	if( n < tocopy )
		tocopy = n;

	if( (newmem = (char *)_xmalloc( n ) ) == NULL )
		return( 0 );

#if	defined( _BSD )
	(void)bcopy( mem, newmem, tocopy );
#else	/* _BSD */
	(void)memcpy( newmem, mem, tocopy );
#endif	/* _BSD */

	_xfree( mem );

	return( newmem );

}

#if	defined( _XALLOC_VALLOC )
_XIMPORT void *
xvalloc( size_t i );
{
	int	valsiz = getpagesize();
	int	j;
	void	*cp = malloc( i + (valsiz-1) );

	j = ((int)cp + (valsiz-1)) &~ (valsiz-1);
	return( (void *)j );
}
#endif	/* _VALLOC */

#if	defined( _XALLOC_STATS )

/*
 *	Return  statistics  describing  allocation  of blocks of size
 *	2**n.
 */

struct	xmalloc_stats {
	unsigned long blocksize;
	unsigned long nfree;
	unsigned long nused;
};

typedef	struct xmalloc_stats XMALLOC_STATS;

/*
 *	Compute the stats for block N.
 */

static	XMALLOC_STATS xmalloc_stats( int size );

static	XMALLOC_STATS
xmalloc_stats( int size )
{
	XMALLOC_STATS	v;
	PMHEAD		p;

	if( size < 0 || size >= MAXBLOCK ) {
		v.blocksize = 0L;
		v.nused     = 0L;
		v.nfree     = 0L;
		return( v );
	}

	v.nfree	    = 0;
	v.blocksize = 1 << (size + MINBLOCK);
	v.nused     = nmalloc[ size ];

	for( p = nextfree[ size ] ; p ; p = CHAIN( p ) )
		v.nfree++;

	return( v );
}

_XIMPORT void
xmalloc_stats_print()
{
	XMALLOC_STATS	v;
	int		i;

	(void)fprintf(_logfd,"         | Size     | Free     | Used     |\n\r");

	for( i = 0 ; i < (MAXBLOCK-1) ; i++ ) {
		v = xmalloc_stats( i );
		if( i < 10 || v.nfree != 0L || v.nused != 0L )
			(void)fprintf(
				       _logfd,
				       "Block %2d | %8lu | %8ld | %8ld |\n\r",
				       i,
				       v.blocksize,
				       v.nfree,
				       v.nused
				     );
	}

	(void)fprintf(
		       _logfd,
		       "\n%d bytes used (%d alloc / %d free).\n\r",
		       xmalloc_mem_used(),
		       allocount,
		       freecount
		     );
}

_XIMPORT long
xmalloc_mem_used()
{
	int	i;
	long	size_used = 0L;

	for( i = 0 ; i < MAXBLOCK ; ++i ) {
		int	allocation_size = (1 << (i + MINBLOCK));

		size_used += nmalloc[ i ] * allocation_size;
	}

	return( size_used );
}

_XIMPORT long
xmalloc_mem_free( void )
{
	int	i;
	long	size_unused = 0L;

	for( i = 0; i < MAXBLOCK ; i++ ) {
		int	allocation_size = (1 << (i + MINBLOCK));
		PMHEAD	p;

		for( p = nextfree[ i ] ; p ; p = CHAIN( p ) )
			size_unused += allocation_size;
	}

	return( size_unused );
}

_XIMPORT void
xmalloc_set_log( const char *file )
{
	if( _logfd != stderr ) {
		(void)fflush( _logfd );
		(void)fclose( _logfd );
	}

	if( file != (const char *)NULL )
		_logfd = fopen( file, "w" );
	else	_logfd = (FILE*)NULL;

	if( _logfd == (FILE*)NULL )
		_logfd = stderr;
}

_XIMPORT void
xmalloc_set_logfd( FILE *fd )
{
	_logfd = fd;
}

static	unsigned long	_memory_saved;	/* last saved memory state */
static	unsigned long	_alloc_saved;	/* last allocation state   */
static	unsigned long	_free_saved;	/* last free state	   */
static	const char*	_file_saved;	/* last file state	   */
static	int		_line_saved;	/* last line state	   */

_XIMPORT void
_xmalloc_save_state( const char* file, int line )
{
#if	defined( _XALLOC_TRACE )
	xmalloc_tag_used();
#endif	/* _XALLOC_TRACE */

	_memory_saved = xmalloc_mem_used();
	_alloc_saved  = allocount;
	_free_saved   = freecount;
	_file_saved   = file;
	_line_saved   = line;
}

_XIMPORT void
_xmalloc_check_leaks( const char *file, int line )
{
	unsigned long	diffs = xmalloc_mem_used() - _memory_saved;

	if( diffs ) {
		(void)fprintf(
			       _logfd,
			       "LEAKS: %s(%d) found %lu bytes leaks",
			       file,
			       line,
			       diffs
			     );
		(void)fprintf(
			       _logfd,
			       " from saved state %s(%d).\n\r",
			       _file_saved,
			       _line_saved
			     );

#if	defined( _XALLOC_TRACE )
		xmalloc_find_untagged();
#endif	/* _XALLOC_TRACE */

		(void)fprintf(
			       _logfd,
			       "       in %lu alloc / %lu free.\n\r\n\r",
			       allocount - _alloc_saved,
			       freecount - _free_saved
			     );
		(void)fflush( _logfd );	/* just in case user buffered it */

	}
}

#endif	/* _XALLOC_STATS */

/*
 *	Real memory request. This is system dependant.
 */

#if	(defined(unix) || defined(__GNUC__) || defined(__SC__)) && !defined(_SYSGETMEM)

#define	_SYSGETMEM

#if	defined( __SC__ )
#include <unistd.h>
#endif	/* __SC__ */

#if	defined( _MUST_DECLARE_SBRK )
#if	defined( __cplusplus )
extern "C" void *sbrk( int incr );
#else	/* __cplusplus */
extern void *sbrk( int incr );
#endif	/* __cplusplus */
#endif	/* _MUST_DECLARE_SBRK */

static	void	*
sysgetmem( size_t n )
{
	void	*p = sbrk( n + _XALLOC_ALIGN );

	if( (p == (char *)-1) )
		return( NULL );

	return( (void *)ALIGN( p ) );

}

#endif	/* unix || __SC__ */

#if	(defined( _WIN32 ) || defined( _WIN64 )) && !defined( _SYSGETMEM )

#define	_SYSGETMEM

static	HANDLE	_allocHeap;

#define	INITIAL_SIZE	0x4000		/* 16k bytes */

static	void *
sysgetmem( size_t n )
{
	HANDLE	hMem;

	if( _allocHeap == 0 )
		_allocHeap = HeapCreate( 0, INITIAL_SIZE, 0 );

	hMem = HeapAlloc( _allocHeap, 0, (DWORD)n + _XALLOC_ALIGN );

	if( hMem == (HANDLE)0 )
		return( NULL );

	return( (void *)ALIGN( hMem ) );
}
#endif	/* _WIN32 || _WIN64 */

#if	defined( _WINDOWS ) && !defined( _SYSGETMEM )

#define	_SYSGETMEM

static	HANDLE	_allocHeap;

static	void *
sysgetmem( size_t n )
{
	HANDLE	hMem = GlobalAlloc( GMEM_FIXED, (DWORD)n + _XALLOC_ALIGN );

	if( hMem == (HANDLE)0 )
		return( NULL );

	return( (void *)ALIGN( GlobalLock( hMem ) ) );
}
#endif	/* _WINDOWS */

#if	!defined( _SYSGETMEM )

#define	_SYSGETMEM

static	void *
sysgetmem( size_t n )
{
	void	*p = malloc( n );

	/*
	 *	Assumes that real malloc is correctly aligned.
	 */

	return( p );
}

#endif	/* _SYSGETMEM */

#if	defined( __cplusplus )
}
#endif	/* __cplusplus */

/*
 *	C++ new/delete
 */

#if	defined( __cplusplus )

#if	defined( new )
#undef	new			/* the real new please !! */
#endif	/* new */

#if	!defined( _HAVE_NEW_HANDLER )
typedef void (_CCALL *new_handler)();
#endif	/* _HAVE_NEW_HANDLER */

new_handler	__new_handler;

#if	defined( _HAVE_NAMESPACE )
namespace std {}
using namespace std;
#endif	/* _HAVE_NAMESPACE */

#if	defined( _HAVE_NOTHROW_TYPE )
#if	!defined( __GNUC__ )
static nothrow_t empty_nothrow;
extern _XIMPORT const nothrow_t nothrow = empty_nothrow;
#endif	/* __GNUC__ */
#endif	/* _HAVE_NOTHROW_TYPE */

_XIMPORT extern	new_handler set_new_handler( new_handler new_p ) THROW_ALL;
static	void*		__raw_alloc( size_t size ) THROW_ALL;

#if	!defined( _HAVE_LIBRARY_HANDLER )

_XIMPORT new_handler
set_new_handler( new_handler new_p ) THROW_ALL
{
	new_handler old_handler = __new_handler;

	__new_handler = new_p;
	return( old_handler );
}

#endif	/* _HAVE_LIBRARY_HANDLER */

static	void*
__raw_alloc( size_t size ) THROW_ALL
{
	void*	ptr;

	if( size == 0 )
		size++;

	for( ;; ) {
		ptr = _xmalloc( size );
		if( ptr != 0 || __new_handler == 0 )
			break;

		(*__new_handler)();
	}

	return( ptr );
}

void* _CCALL
operator new( size_t size ) THROW_BAD_ALLOC
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	void*	ptr = __raw_alloc( size );

	trace( "new", size, ptr );

#if	defined( _HAVE_EXCEPTIONS )
	if( ptr == 0 )
		throw bad_alloc();
#endif	/* _HAVE_EXCEPTIONS */

#if	defined( _XALLOC_TYPE_CHECK )
	p = HEADPTR( ptr );
	p->mh_type = ALLOC_NEW_NOARRAY;
#endif	/* _XALLOC_TYPE_CHECK */

#if	defined( _XALLOC_TRACE )
	_xmalloc_tag_block( ptr, 0, 0 );
#endif	/* _XALLOC_TRACE */

	return( ptr );
}

#if	defined( _HAVE_NOTHROW_TYPE )

void* _CCALL
operator new( size_t size, const nothrow_t& )
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	void*	ptr = __raw_alloc( size );

	trace( "new (nothrow)", size, ptr );

	if( ptr == 0 )
		return 0;

#if	defined( _XALLOC_TYPE_CHECK )
	p = HEADPTR( ptr );
	p->mh_type = ALLOC_NEW_NOARRAY;
#endif	/* _XALLOC_TYPE_CHECK */

#if	defined( _XALLOC_TRACE )
	_xmalloc_tag_block( ptr, 0, 0 );
#endif	/* _XALLOC_TRACE */

	return( ptr );
}

void* _CCALL
operator new[]( size_t size, const nothrow_t& )
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	void*	ptr = __raw_alloc( size );

	trace( "new(nothrow&)[]", size, ptr );

#if	defined( _XALLOC_TYPE_CHECK )
	p   = (PMHEAD)((char *)ptr - HEADSIZE);
	p->mh_type = ALLOC_NEW_ARRAY;
#endif	/* _XALLOC_TYPE_CHECK */

#if	defined( _XALLOC_TRACE )
	_xmalloc_tag_block( ptr, 0, 0 );
#endif	/* _XALLOC_TRACE */

	return( ptr );
}

#endif	/* _HAVE_NOTHROW_TYPE */


void _CCALL
operator delete( void* ptr ) THROW_ALL
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	trace( "delete", -1, ptr );

	if( ptr ) {
#if	defined( _XALLOC_TYPE_CHECK )
		p = (PMHEAD)((char *)ptr - HEADSIZE);
		ASSERT( (p->mh_type == ALLOC_NEW_NOARRAY), p );
		p->mh_type = ALLOC_MALLOC;
#endif	/* _XALLOC_TYPE_CHECK */
		_xfree( ptr );
	}
}

#if	defined( _HAVE_ARRAY_OPERATOR )

void* _CCALL
operator new[]( size_t size ) THROW_BAD_ALLOC
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	void*	ptr = __raw_alloc( size );

	trace( "new[]", size, ptr );

#if	defined( _HAVE_EXCEPTIONS )
	if( ptr == 0 )
		throw bad_alloc();
#endif	/* _HAVE_EXCEPTIONS */

#if	defined( _XALLOC_TYPE_CHECK )
	p   = (PMHEAD)((char *)ptr - HEADSIZE);
	p->mh_type = ALLOC_NEW_ARRAY;
#endif	/* _XALLOC_TYPE_CHECK */

#if	defined( _XALLOC_TRACE )
	_xmalloc_tag_block( ptr, 0, 0 );
#endif	/* _XALLOC_TRACE */

	return( ptr );
}

void _CCALL
operator delete[]( void* ptr ) THROW_ALL
{
#if	defined( _XALLOC_TYPE_CHECK )
	PMHEAD	p;
#endif	/* _XALLOC_TYPE_CHECK */

	trace( "delete[]", -1, ptr );

	if( ptr ) {
#if	defined( _XALLOC_TYPE_CHECK )
		p = (PMHEAD)((char *)ptr - HEADSIZE);
		ASSERT( (p->mh_type == ALLOC_NEW_ARRAY), p );
		p->mh_type = ALLOC_MALLOC;
#endif	/* _XALLOC_TYPE_CHECK */
		_xfree( ptr );
	}
}
#endif	/* _HAVE_ARRAY_OPERATOR */

#if	defined( _XALLOC_TRACE )

/*
 *	C++ checked new and delete operators
 */

void*	_CCALL
operator new( size_t size, const char *file, int line ) THROW_BAD_ALLOC
{

	return( _xmalloc_tag_block( operator new( size ), file, line ) );
}

#if	defined( _HAVE_ARRAY_OPERATOR )
void*	_CCALL
operator new[]( size_t size, const char *file, int line ) THROW_BAD_ALLOC
{
	return( _xmalloc_tag_block( operator new[]( size ), file, line ) );
}

#endif	/* _HAVE_ARRAY_OPERATOR */

#endif	/* _XALLOC_TRACE */

#endif	/* __cplusplus */

#endif	/* _XALLOC_DEFAULT */
