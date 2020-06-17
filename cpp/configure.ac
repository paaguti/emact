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

dnl Process this file with autoconf to produce a configure script

AC_PREREQ([2.69])
AC_INIT([emacs],
        [3.0.0],
	[jullien@eligis.com],
	[emacs-3.0.0],
	[http://www.eligis.com/])
AC_REVISION([$Revision: 1.106 $])
AC_COPYRIGHT([(c) Copyright Eligis. 1988-2020])

dnl where to find install.sh, config.sub, and config.guess
AC_CONFIG_AUX_DIR([conf])
dnl where to find source code
AC_CONFIG_SRCDIR([emacs.h])

dnl use src/config.h.in template.

AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])

NAME=emact
VERSION=`grep EMACS_VERSION version.h | sed -e "s/.*\"\(.*\)\"/\1/g"`

dnl Checks for programs.
dnl AC_PROG_CXX
dnl AC_PROG_AWK
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

dnl User options

AC_CANONICAL_TARGET

SYSTEM_TYPE="$target_vendor-$target_os"
MACHINE_TYPE="$target_cpu"
AC_SUBST(SYSTEM_TYPE)
AC_DEFINE_UNQUOTED([SYSTEM_TYPE], ["$SYSTEM_TYPE"], [Define the system type])
AC_SUBST(MACHINE_TYPE)
AC_DEFINE_UNQUOTED([MACHINE_TYPE], ["$MACHINE_TYPE"], [Define the machine type])

dnl Get the exact CC compiler used (in case of alias)
AC_MSG_CHECKING(for the exact C compiler version)
CLANG=
GNUCC=
AS_IF([`$CC -v 2>&1 | grep gcc   > /dev/null`], [GNUCC=$CC],
      [`$CC -v 2>&1 | grep clang > /dev/null`], [CLANG=$CC])

AS_IF([test -n "$CLANG" -a -z "`echo $CC | grep clang`"],
      [GCC_IS_AN_ALIAS="$CC is an alias for clang"
       CC=clang])

CCVERSION=`${CC} -v 2>&1 | grep " version " | sed -e "s/^[[A-Za-z ]]*\([[.0-9]]*\).*/\1/g"`
AC_MSG_RESULT([$CC v$CCVERSION])

AS_IF([test -n "$GCC_IS_AN_ALIAS"],
      [AC_MSG_NOTICE([$GCC_IS_AN_ALIAS])])

dnl Checks for C++

AC_MSG_CHECKING(for C++)
AC_ARG_ENABLE([cxx],
  [AS_HELP_STRING([--enable-cxx],
    [enable C++ lib [default=yes]])],
  [],
  [enable_cxx=yes])
AC_MSG_RESULT($enable_cxx)
AS_IF([test "$enable_cxx" = "yes"],
  [AX_CXX_COMPILE_STDCXX([17], [noext], [optional])
   AS_IF([test $HAVE_CXX17 -eq 0],
         [AX_CXX_COMPILE_STDCXX([14], [noext], [optional])
          AS_IF([test $HAVE_CXX14 -eq 0],
                [AX_CXX_COMPILE_STDCXX([11], [noext], [mandatory])])])
   AS_IF([`$CXX --version 2>&1 | grep clang > /dev/null`],
         [CLANGXX=$CXX],
         [CLANGXX=])])

AC_LANG([C++])

AX_CHECK_COMPILE_FLAG([-Werror -m64],
                      [AS_VAR_APPEND([CXXFLAGS], [" -m64"])])
AX_CHECK_COMPILE_FLAG([-pipe],
                      [AS_VAR_APPEND([CXXFLAGS], [" -pipe"])])
AX_CHECK_COMPILE_FLAG([-Wall],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wall"])])
AX_CHECK_COMPILE_FLAG([-Wextra],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wextra"])])
AX_CHECK_COMPILE_FLAG([-Wshadow],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wshadow"])])
AX_CHECK_COMPILE_FLAG([-Wunused],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wunused"])])
AX_CHECK_COMPILE_FLAG([-Werror -Wsuggest-override],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wsuggest-override"])])
AX_CHECK_COMPILE_FLAG([-Werror -Wno-psabi],
                      [AS_VAR_APPEND([CXXFLAGS], [" -Wno-psabi"])])
AX_CHECK_COMPILE_FLAG([-Werror -finline-limit=4096],
                      [AS_VAR_APPEND([CXXFLAGS], [" -finline-limit=4096"])])

AX_CHECK_COMPILE_FLAG([-Werror -Wconversion],
                      [AS_VAR_APPEND([WLIBFLAGS], [" -Wconversion"])])

AC_CHECK_PROGS([AR], gcc-ar ar gar, no)
AS_IF([test "$AR" != "no"],
      [AC_SUBST(AR)],
      [AC_MSG_ERROR(Can\'t find gcc-ar, ar or gar.)])

AC_CHECK_PROGS([STRIP], gcc-strip strip gstrip, no)
AS_IF([test "$STRIP" != "no"],
      [AC_SUBST(STRIP)],
      [AC_MSG_ERROR(Can\'t find gcc-strip, strip or strip.)])

AC_CHECK_PROGS([MAKE], gmake gnumake make, no)
AS_IF([test "$MAKE" != "no"],
      [AC_SUBST(MAKE)],
      [AC_MSG_ERROR(Can\'t find gmake, gnumake or make.)])

AC_CHECK_PROGS([LDCONFIG], ldconfig, no)
AS_IF([test "$LDCONFIG" != "no"],
      [AC_SUBST(LDCONFIG)],
      [LDCONFIG="echo INFO: no ldconfig on this system"])

AC_CHECK_PROGS(COMPRESS, gzip bzip2 zip compress, no)
AS_IF([test "$COMPRESS" != "no"],
      [AC_SUBST(COMPRESS)],
      [AC_MSG_ERROR(Can\'t find any compressor.)])

dnl ensure we use posix find instead of Microsoft FIND if on Cygwin.
AS_IF([test -x /usr/bin/find],
      [FIND=/usr/bin/find],
      [FIND=find])
AC_SUBST(FIND)

STATIC_BUILD=std
AC_SUBST(STATIC_BUILD)

PKG_PROG_PKG_CONFIG

m4_pattern_allow([PKG_CHECK_EXISTS]) 

AX_WITH_CURSES

dnl There should be a better way to find if a curses.h exists.
AS_IF([test "x$ax_cv_header_ncursesw_curses_h"       = "xyes" \
         -o "x$ax_cv_header_ncursesw_h"              = "xyes" \
         -o "x$ax_cv_header_ncurses_h_with_ncursesw" = "xyes" \
         -o "x$ax_cv_header_ncurses_curses_h"        = "xyes" \
         -o "x$ax_cv_header_ncurses_h"               = "xyes" \
         -o "x$ax_cv_header_curses_h"                = "xyes"],
      [AS_IF([test "x$ax_cv_curses" = "xyes"],
             [AS_VAR_APPEND([LIBS], [" -l$ax_cv_curses_which"])])])

dnl get type size

AC_CHECK_SIZEOF(void *, 4)
AC_CHECK_SIZEOF(int,    4)
AC_CHECK_SIZEOF(long,   4)
AC_CHECK_SIZEOF(size_t, 4)

AC_CACHE_SAVE

dnl Optimization and Warnings options

AC_PATH_XTRA
AS_IF([test "x$with_x" = "xyes"],
      [AC_DEFINE([HAVE_X11], [1], [Define if X11 is wanted.])
       AS_VAR_APPEND([CXXFLAGS], [" $X_CFLAGS"])
       AS_VAR_APPEND([LIBS], [" $X_LIBS $X_PRE_LIBS -lX11 $X_EXTRA_LIBS"])])

dnl Local options

AEXT="a"
AROPTS="-c -r"
PIC=""
SHARED=#"shared-std"
SOEXT="so"

SONAME=soname
AS_CASE([${target_cpu}-${target_os}],
  [*darwin*], [
    dnl libol.dylib:
    dnl $(CC) $(CFLAGS) -dynamiclib -install_name "libol.1.dylib"
    dnl                 -current_version 1.0.6
    dnl                 -compatibility_version 1.0 -o libol.1.dylib $(OBJS)
    SONAMEFLAG="install_name"
    ])

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

dnl Save result in src/Makefile (using src/Makefile.in template).

AC_CONFIG_FILES([Makefile ${NAME}.spec ${NAME}.pc])
AC_OUTPUT

AS_ECHO("--------------------------------------------------------------------")
AS_ECHO("Configuration:")
AS_ECHO("")
AS_ECHO(" Name:          ${NAME}")
AS_ECHO(" Version:       ${VERSION}")
AS_ECHO(" Source:        ${srcdir}")
AS_ECHO(" Compiler:      ${CXX} ${CCVERSION}")
AS_ECHO(" Flags:         ${CXXFLAGS}")
AS_ECHO(" Build:         ${build}")
AS_ECHO(" Host:          ${host}")
AS_ECHO(" Target:        ${target}")
AS_ECHO(" Install:       ${prefix}")
AS_ECHO("")
AS_ECHO("See src/config.h for further configuration information.")
AS_ECHO("--------------------------------------------------------------------")
