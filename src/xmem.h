/*
 * static char rcsid[] = "$Id: xmem.h,v 1.4 2012/10/21 12:20:32 jullien Exp $";
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

/*
 *	Allocation replacement.
 */

#ifndef	__XMEM_H
#define	__XMEM_H

#include <sys/types.h>
#include <stdlib.h>

#if	defined( _DLL )
#if	!defined( _XIMPORT )
#define _XIMPORT __declspec(dllimport)
#endif  /* _XIMPORT */
#else   /* _DLL */
#if	!defined( _XIMPORT )
#define _XIMPORT
#endif  /* _XIMPORT */
#endif  /* _DLL */

#if	defined( _XALLOC_DEFAULT )

#define	xmalloc(x)	malloc(x)
#define	xrealloc(x,n)	realloc(x,n)
#define	xcalloc(x)	calloc(x)
#define	xfree(x)	free(x)
#define	xcfree(x)	(free(x),x=0)
#define	xnew		new
#define	xdelete		delete

#else	/* _XALLOC_DEFAULT */

#if	defined( __cplusplus )
extern "C" {
#endif	/* __cplusplus */

_XIMPORT extern	void	_xfree( void *cp );
_XIMPORT extern	void *	_xmalloc( size_t size );
_XIMPORT extern	void *	_xcalloc( size_t nelem, size_t size );
_XIMPORT extern	void *	_xrealloc( void *cp, size_t nbytes );

#if	defined( _XALLOC_DEBUG )
#if	!defined( __ECPP__ )
#define	_XALLOC_TYPE_CHECK
#endif	/* __ECPP__ */
#define	_XALLOC_RANGE_CHECK
#define	_XALLOC_STATS
#define	_XALLOC_TRACE
#endif	/* _XALLOC_DEBUG */

#if	defined( _XALLOC_DEBUG ) || defined( _XALLOC_STATS )
_XIMPORT extern	void	xmalloc_stats_print( void );
_XIMPORT extern	long	xmalloc_mem_used( void );
_XIMPORT extern	long	xmalloc_mem_free( void );
_XIMPORT extern	void	xmalloc_set_log( const char *file );
_XIMPORT extern	void	xmalloc_set_logfd( FILE *fd );
_XIMPORT extern	void	_xmalloc_save_state( const char *file, int line );
_XIMPORT extern	void	_xmalloc_check_leaks( const char *file, int line );
_XIMPORT extern	void*	_xmalloc_tag_block( void* mem, const char* file, int line );
_XIMPORT extern	int	_xmalloc_set_tag( const char* file, int line );

#define	xmalloc_save_state() _xmalloc_save_state( __FILE__, __LINE__ );
#define	xmalloc_check_leaks() _xmalloc_check_leaks( __FILE__, __LINE__ );
#endif	/* _XALLOC_DEBUG || _XALLOC_STATS */

#if	defined( _XALLOC_DEBUG )
_XIMPORT extern	void *	_xmalloc_trace_malloc( size_t size, const char *file, int line );
_XIMPORT extern	void *	_xmalloc_trace_calloc( size_t nelem, size_t size, const char *file, int line );
_XIMPORT extern	void *  _xmalloc_trace_realloc( void *mem, size_t size, const char *file, int line );
#endif	/* _XALLOC_DEBUG */

#if	defined( __cplusplus )
}
#endif	/* __cplusplus */

#if	defined( __cplusplus )

//	C++ SPECIFIC

//
//	An ISO/IEC 14882 C++ compiler must have the following features.
//

#define	_HAVE_ARRAY_OPERATOR	/* C++ compiler support new[] and delete[] */
#define	_HAVE_EXCEPTIONS	/* C++ compiler support exceptions	   */
#define	_HAVE_NAMESPACE		/* C++ compiler support namespaces	   */
#define	_HAVE_NEW_HANDLER	/* C++ compiler support new_handler type   */
#define	_HAVE_NOTHROW_DELETE	/* C++ compiler support nothrow_t delete   */
#define	_HAVE_NOTHROW_TYPE	/* C++ compiler support nothrow_t type	   */
#define	_HAVE_THROW_SPEC	/* C++ compiler support throw spec.	   */
#define	_HAVE_STD_HEADERS	/* C++ compiler support standard headers.  */

//	MS VC++ 5.0

#if	defined( _MSC_VER ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#if	( _MSC_VER > 1000 )
#include <new>
#include <exception>
#define	THROW_ALL		throw()
#define	THROW_BAD_ALLOC		throw( std::bad_alloc )
#undef	_HAVE_NOTHROW_DELETE
#else	/* _MSC_VER > 1000 */
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define	_HAVE_LIBRARY_HANDLER
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_ARRAY_OPERATOR
#undef	_HAVE_NAMESPACE
#endif	/* _MSC_VER > 1000 */
#define _CCALL			__cdecl
#define	_MUST_DECLARE_OPERATORS
#endif	/* _MSC_VER */

//	Borland C++ 5.x

#if	defined( __BORLANDC__ ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define _CCALL
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#define	_MUST_DECLARE_OPERATORS
#endif	/* __BORLANDC__ */

//	GNU g++ / egcs

#if	defined( __GNUC__ ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new>
#define	THROW_ALL		throw()
#define	THROW_BAD_ALLOC		throw( std::bad_alloc )
#define _CCALL
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_NAMESPACE
#endif	/* __GNUC__ */

//	SCO CC

#if	defined( __SCOCC__ ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define	_HAVE_LIBRARY_HANDLER
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_ARRAY_OPERATOR
#undef	_HAVE_NAMESPACE
#undef	_HAVE_NEW_HANDLER
#define _CCALL
#define	_MUST_DECLARE_OPERATORS
#endif	/* __SCOCC__ */

//	HP CC

#if	defined( _HPUX_SOURCE ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define	_HAVE_LIBRARY_HANDLER
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_ARRAY_OPERATOR
#undef	_HAVE_NAMESPACE
#undef	_HAVE_NEW_HANDLER
#define _CCALL
#define	_MUST_DECLARE_OPERATORS
#endif	/* _HPUX_SOURCE */

//	IBM xlC C++ Compiler

#if	defined( __XLC__ ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define _CCALL
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_ARRAY_OPERATOR
#undef	_HAVE_NEW_HANDLER
#undef	_HAVE_NAMESPACE
#endif	/* __XLC__ */

//	DYNIX Sequent ec++ C++ Compiler

#if	(defined(_SEQUENT_) || defined(__ECPP__)) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#define _CCALL
#define	_MUST_DECLARE_OPERATORS
#define	_HAVE_LIBRARY_HANDLER
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_NEW_HANDLER
#undef	_HAVE_NAMESPACE
#endif	/* _SEQUENT_ || __ECPP__ */

//	Symantec 7.x

#if	defined( __SC__ ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_STD_HEADERS
#undef	_HAVE_NEW_HANDLER
#undef	_HAVE_NAMESPACE
#define _CCALL			cdecl
#endif	/* __SC__ */

//	Sun CC 4.2

#if	defined( __SUNPRO_CC ) && !defined( _NEW_DECLARED )
#define	_NEW_DECLARED
#include <new.h>
#undef	_HAVE_NOTHROW_DELETE
#undef	_HAVE_EXCEPTIONS
#undef	_HAVE_NOTHROW_TYPE
#undef	_HAVE_ARRAY_OPERATOR
#undef	_HAVE_STD_HEADERS
#undef	_HAVE_NEW_HANDLER
#undef	_HAVE_NAMESPACE
#endif	/* __SUNPRO_CC */

#if	!defined( THROW_ALL )
#define	THROW_ALL
#define	THROW_BAD_ALLOC
#endif	/* THROW_ALL */

#if	!defined( _CCALL )
#define _CCALL
#endif	/* _CCALL */

#if	!defined( _HAVE_NOTHROW_TYPE )
#if	defined( _HAVE_NAMESPACE )
namespace std { struct nothrow_t {}; }
#else	/* _HAVE_NAMESPACE */
struct std { struct nothrow_t {}; };
#endif	/* _HAVE_NAMESPACE */

_XIMPORT extern const std::nothrow_t nothrow;
#endif	/* _HAVE_NOTHROW_TYPE */

/*
 *	C++ replacement for new and delete operators.
 */

//_XIMPORT extern _CCALL new_handler set_new_handler( new_handler new_p ) THROW_ALL;

// 17.4.3.4 Replacement functions

#if	defined( _MUST_DECLARE_OPERATORS )
extern	void	_CCALL operator delete( void* ptr ) THROW_ALL;
extern	void*	_CCALL operator new( size_t size )  THROW_BAD_ALLOC;
#if	defined( _HAVE_NOTHROW_TYPE )
extern	void*	_CCALL operator new( size_t size, const std::nothrow_t& );
#endif	/* _HAVE_NOTHROW_TYPE */
#if	defined( _HAVE_NOTHROW_DELETE )
extern	void	_CCALL operator delete( void* ptr, const std::nothrow_t& );
#endif	/* _HAVE_NOTHROW_DELETE */

#if	defined( _HAVE_ARRAY_OPERATOR )
extern	void	_CCALL operator delete[]( void* ptr ) THROW_ALL;
extern	void*	_CCALL operator new[]( size_t size )  THROW_BAD_ALLOC;
#if	defined( _HAVE_NOTHROW_TYPE )
extern	void*	_CCALL operator new[]( size_t size, const std::nothrow_t& );
#endif	/* _HAVE_NOTHROW_TYPE */
#if	defined( _HAVE_NOTHROW_DELETE )
extern	void	_CCALL operator delete[]( void* ptr, const std::nothrow_t& );
#endif	/* _HAVE_NOTHROW_DELETE */
#endif	/* _HAVE_ARRAY_OPERATOR */
#endif	/* _MUST_DECLARE_OPERATORS */

/*
 *	C++ checked new and delete operators
 */

extern	void*	_CCALL operator new( size_t size, const char *file, int line )  THROW_BAD_ALLOC;
#if	defined( _HAVE_NOTHROW_TYPE )
extern	void*	_CCALL operator new( size_t size, const std::nothrow_t&, const char *file, int line );
#endif	/* _HAVE_NOTHROW_TYPE */
#if	defined( _HAVE_ARRAY_OPERATOR )
extern	void*	_CCALL operator new[]( size_t size, const char *file, int line )  THROW_BAD_ALLOC;
#if	defined( _HAVE_NOTHROW_TYPE )
extern	void*	_CCALL operator new[]( size_t size, const std::nothrow_t&, const char *file, int line );
#endif	/* _HAVE_NOTHROW_TYPE */
#endif	/* _HAVE_ARRAY_OPERATOR */

#endif	/* __cplusplus */

#if	defined( _ISO_CPP_SYNOPIS )

namespace std {
	class bad_alloc;
	struct nothrow_t {};
	extern const nothrow_t nothrow;
	typedef void (*new_handler)();
	new_handler set_new_handler(new_handler new_p) throw();
}

void*	operator new(std::size_t size) throw(std::bad_alloc);
void*	operator new(std::size_t size, const std::nothrow_t&) throw();
void	operator delete(void* ptr) throw();
void	operator delete(void* ptr, const std::nothrow_t&) throw();
void*	operator new[](std::size_t size) throw(std::bad_alloc);
void*	operator new[](std::size_t size, const std::nothrow_t&) throw();
void	operator delete[](void* ptr) throw();
void	operator delete[](void* ptr, const std::nothrow_t&) throw();
void*	operator new (std::size_t size, void* ptr) throw();
void*	operator new[](std::size_t size, void* ptr) throw();
void	operator delete (void* ptr, void*) throw();
void	operator delete[](void* ptr, void*) throw();

#endif

/*
 *	Define allocation replacement
 */

#if	defined( _XALLOC_TRACE )
#define	xmalloc(x)	_xmalloc_trace_malloc((x),__FILE__,__LINE__)
#define	xcalloc(x)	_xmalloc_trace_calloc((x),__FILE__,__LINE__)
#define xrealloc(x,n)	_xmalloc_trace_realloc((x),(n),__FILE__,__LINE__)
#define	xfree(x)	_xfree(x)
#define	xcfree(x)	(_xfree(x),x=0)
#define	xnew		new(__FILE__,__LINE__)
#define	xdelete		delete
#else	/* _XALLOC_TRACE */
#define	xmalloc(x)	_xmalloc(x)
#define	xrealloc(x,n)	_xrealloc(x,n)
#define	xcalloc(x)	_xcalloc(x)
#define	xfree(x)	_xfree(x)
#define	xcfree(x)	(_xfree(x),x=0)
#define	xnew		new
#define	xdelete		delete
#endif	/* _XALLOC_TRACE */

#if	defined( _XALLOC_REPLACE )
#define	malloc(x)	xmalloc(x)
#define	realloc(x,n)	xrealloc(x,n)
#define	calloc(x)	xcalloc(x)
#define	free(x)		xfree(x)
#define	cfree(x)	xcfree(x)
#endif	/* _XALLOC_REPLACE */

#if	defined( _XALLOC_REPLACE ) && defined( _XALLOC_TRACE )
/*
 *	This is a BIG hack.  We define new as a macro but we call the
 *	real  operator  new  using new.  We also use operator ?: that
 *	force  evaluation  from  left to rigth (very few in C/C++) so
 *	that _xmalloc_set_tag is always called just before new!
*/
#define	new	(_xmalloc_set_tag( __FILE__, __LINE__ ) == -1) ? 0 : new
#endif	/* _XALLOC_REPLACE && _XALLOC_TRACE */

#endif	/* _XALLOC_DEFAULT */

#endif	/* __XNEW_H */
