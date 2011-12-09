@ECHO	OFF
REM	@(#) install.bat	(c) by C. Jullien - 2008/03/21

CLS

IF [%1] == [] GOTO usage

IF NOT EXIST %1\nul		MKDIR %1
IF NOT EXIST \usr\nul		MKDIR \usr
IF NOT EXIST \usr\lib\nul	MKDIR \usr\lib
IF NOT EXIST \usr\lib\emacs\nul	MKDIR \usr\lib\emacs

ECHO	EmACT editor
ECHO	Copyright (c) by Christian Jullien
ECHO.
ECHO	1) Copy the binaries in %1

IF EXIST *.exe  COPY *.exe  %1 > nul
IF EXIST *.hlp  COPY *.hlp  %1 > nul

ECHO	2) Copy libraries and macros in \usr\lib\emacs

COPY	*.lsp \usr\lib\emacs > nul

ECHO.

GOTO	done

:usage
	ECHO	USAGE: %0 directory-for-binary
	ECHO.
	ECHO		Ex: %0 c:\dos
	ECHO		Ex: %0 c:\windows
	ECHO		Ex: %0 c:\winnt
	ECHO.

:done
