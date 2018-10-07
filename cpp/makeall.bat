@echo off
rem	Build EmACT with different C Compilers.

goto :main

rem
rem	Build
rem
:makebuild
	cd etc
	nmake -nologo -q -f makefile.nt build.exe
	cd ..
	etc\build src\build.cnt src\build.h

	echo.;;-----------------
	for /f %%i in (src\build.cnt) do @echo Build version: %%i
	echo.;;-----------------

	cd src
	emtags *.c *.h
	cd ..
	cd src\pocket\setup
	nmake -nologo
	cd ..\..\..
	goto :eof

:rebuild
	cl 2>&1 | find "x86" >nul
	if %ERRORLEVEL%==0 (
	  set CPU=x86
	) else (
	  set CPU=amd64
	)
	del *.obj
	nmake -nologo -f makefile.xp unicode
	copy ed.exe emactu-%CPU%.exe
	del *.obj
	nmake -nologo -f makefile.xp
	copy ed.exe emacta-%CPU%.exe
	goto :eof

rem
rem	VC++ 64 bit AMD64/EM64T
rem
:win64
	echo.
	setlocal
	set MSVCROOT=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build
	call "%MSVCROOT%\vcvarsall.bat" amd64
	del *.obj
	nmake -nologo -f makefile.xp unicode
	nmake -nologo -f makefile.xp
	goto :eof

rem
rem	VC++ 32 bit
rem
:win32
	echo.
	setlocal
	set CPU=i386
	call :rebuild
	goto :eof

:main
rem	call :makebuild
rem	call :win64
	call :win32
	goto :eof

:main-alternate
	call :win64
