#
# Autoconf script for emact.
# By Christian Jullien (jullien@eligis.com)
# $Id: configure.ac,v 1.6 2018/09/08 07:14:39 jullien Exp $
#
# use autoconf-2.68 (works also with autoconf-2.13)

dnl Tested on:
dnl
dnl   Port name
dnl   ---------
dnl   aarch64-unknown-linux-gnu
dnl   armv7l-unknown-linux-gnueabi
dnl   i686-pc-cygwin
dnl   mips64el-unknown-linux-gnu
dnl   powerpc-ibm-aix7.1.3.0
dnl   powerpc64le-unknown-linux-gnu
dnl   sparc-sun-solaris2.10
dnl   x86-64-unknown-openbsd6.4
dnl   x86_64-apple-darwin17.7.0
dnl   x86_64-pc-linux-gnu
dnl   x86_64-unknown-cygwin
dnl   x86_64-unknown-freebsd11.0

# Process this file with autoconf to produce a configure script

AC_INIT(emacs.h)

# where to find install.sh, config.sub, and config.guess

AC_CONFIG_AUX_DIR(./conf)

# use ./config.h.in template.

AC_CONFIG_HEADER(./config.h)

# make sure we are using a recent autoconf version

AC_PREREQ(2.63)

AC_CONFIG_MACRO_DIR([m4])

NAME=emact
VERSION=`grep EMACS_VERSION version.h | sed -e "s/.*\"\(.*\)\"/\1/g"`

# User options

AC_CANONICAL_SYSTEM

# Checks for specific system

AC_AIX

# Checks for C++ (Must be called after AC_AIX)

enable_cxx=yes
AC_PROG_CXX
AC_LANG([C++])
AX_CXX_COMPILE_STDCXX(11, noext, mandatory)
#AX_CHECK_COMPILE_FLAG([-Wsuggest-override],
#                      [CXXWARN="$CXXWARN -Wsuggest-override"])

STATIC_BUILD=std
AC_SUBST(STATIC_BUILD)

AC_CHECK_PROGS(MAKE, gmake, make)
if test $MAKE = no
then
  AC_MSG_ERROR(Can\'t find gmake or make.)
fi

AC_CHECK_PROGS(AR, ar gar, no)
if test $AR = no; then
  AC_MSG_ERROR(Can\'t find ar or gar.)
fi
AC_SUBST(AR)

AC_CHECK_PROGS(RANLIB, ranlib, no)
if test $RANLIB = no; then
  RANLIB="echo INFO: no ranlib on this system "
fi
AC_SUBST(RANLIB)

AC_CHECK_PROGS(LDCONFIG, ldconfig, no)
if test $LDCONFIG = no; then
  LDCONFIG="echo INFO: no ldconfig on this system "
fi
AC_SUBST(LDCONFIG)

PKG_PROG_PKG_CONFIG

m4_pattern_allow([PKG_CHECK_EXISTS]) 

HAVE_CURSES=0

AX_WITH_CURSES

#dnl There should be a better way to find if a curses.h exists.
if test "x$ax_cv_header_ncursesw_curses_h"       = "xyes" \
     -o "x$ax_cv_header_ncursesw_h"              = "xyes" \
     -o "x$ax_cv_header_ncurses_h_with_ncursesw" = "xyes" \
     -o "x$ax_cv_header_ncurses_curses_h"        = "xyes" \
     -o "x$ax_cv_header_ncurses_h"               = "xyes" \
     -o "x$ax_cv_header_curses_h"                = "xyes"
then
  if test "x$ax_cv_curses" != "xno" -o "x$ax_cv_ncurses" != "xno"
  then
    LIBS="-l$ax_cv_curses_which $LIBS"
    HAVE_CURSES=1
  fi
fi

# get type size

AC_CHECK_SIZEOF(void *, 4)
AC_CHECK_SIZEOF(int,    4)
AC_CHECK_SIZEOF(long,   4)
AC_CHECK_SIZEOF(size_t, 4)

# Save cache

AC_CACHE_SAVE

# OS specifics, sets {EXE,OBJ}EXT

AC_EXEEXT
AC_OBJEXT

dnl Optimization and Warnings options

AX_CHECK_COMPILE_FLAG([-m64],
                      [MODEL="-m64"])
AX_CHECK_COMPILE_FLAG([-Wall],
                      [CXXFLAGS="$CXXFLAGS -Wall"])
AX_CHECK_COMPILE_FLAG([-Wextra],
                      [CXXFLAGS="$CXXFLAGS -Wextra"])
AX_CHECK_COMPILE_FLAG([-Wshadow],
                      [CXXFLAGS="$CXXFLAGS -Wshadow"])
AX_CHECK_COMPILE_FLAG([-Wunused],
                      [CXXFLAGS="$CXXFLAGS -Wunused"])
AX_CHECK_COMPILE_FLAG([-Wunused-value],
                      [CXXFLAGS="$CXXFLAGS -Wunused-value"])
AX_CHECK_COMPILE_FLAG([-Wunused-variable],
                      [CXXFLAGS="$CXXFLAGS -Wunused-variable"])
AX_CHECK_COMPILE_FLAG([-Wunused-parameter],
                      [CXXFLAGS="$CXXFLAGS -Wunused-parameter"])
AX_CHECK_COMPILE_FLAG([-Wunused-label],
                      [CXXFLAGS="$CXXFLAGS -Wunused-label"])
AX_CHECK_COMPILE_FLAG([-Wunused-function],
                      [CXXFLAGS="$CXXFLAGS -Wunused-function"])
AX_CHECK_COMPILE_FLAG([-fomit-frame-pointer],
                      [CXXFLAGS="$CXXFLAGS -fomit-frame-pointer"])

if test `uname` != Darwin
then
  AX_CHECK_COMPILE_FLAG([-finline-functions],
                        [CXXFLAGS="$CXXFLAGS -finline-functions"])
  AX_CHECK_COMPILE_FLAG([-finline-limit=4096],
                        [CXXFLAGS="$CXXFLAGS -finline-limit=4096"])
fi

AC_PATH_XTRA

#echo "X_LIBS=$X_LIBS X_PRE_LIBS=$X_PRE_LIBS -lX11 X_EXTRA_LIBS=$X_EXTRA_LIBS"

if test "x$have_x" = "xyes"
then
  CXXFLAGS="$CXXFLAGS $X_CFLAGS"
  for i in $X_LIBS $X_PRE_LIBS -lX11 $X_EXTRA_LIBS
  do
    LIBS="$LIBS $i"
  done
fi

# Local options

AEXT="a"
AROPTS="-c -r"
PIC=""
SHARED=#"shared-std"
SOEXT="so"

if test `uname` != Darwin
then
  SONAME=soname
else
  SONAME=install_name
fi

AC_SUBST(NAME)
AC_SUBST(MODEL)
AC_SUBST(AEXT)
AC_SUBST(AROPTS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)
AC_SUBST(CXX)
AC_SUBST(CXXWARN)
AC_SUBST(LDFLAGS)
AC_SUBST(PIC)
AC_SUBST(SHARED)
AC_SUBST(SOEXT)
AC_SUBST(SONAME)
AC_SUBST(VERSION)
AC_SUBST(HAVE_CURSES)

# Save result in Makefile (using Makefile.in template).

AC_OUTPUT(Makefile ${NAME}.spec ${NAME}.pc)

echo \
"------------------------------------------------------------------------
Configuration:

 Name:		${NAME}
 Version:	${VERSION}
 Source:        ${srcdir}
 Compiler:	${CXX}
 C++ Flags:     ${CXXFLAGS}
 Build:         ${build}
 Host:          ${host}
 Target:        ${target}
 Install:       ${prefix}

See ./config.h for further configuration information.
------------------------------------------------------------------------"
