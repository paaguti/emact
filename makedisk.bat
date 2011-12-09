@echo	off
rem	Build EmACT distribution disk. 2010/05/10.

setlocal
set EMACSDIR=c:\usr\jullien\emacs
set EMACS_MAJOR=2
set EMACS_MINOR=55
set EMACS_RELEASE=0
set VERSION=%EMACS_MAJOR%-%EMACS_MINOR%
set ARCHIVE=emact-%EMACS_MAJOR%.%EMACS_MINOR%.%EMACS_RELEASE%
set TMPDIR=c:\tmp\%ARCHIVE%

if [%1] == [/f] goto format
if [%1] == [-f] goto format

goto main

:format
 dir a: > nul
 if not errorlevel 1 if not exist a:\emacs.zip goto wrongdisk
 echo Insert a new disk in drive a:
 pause
 echo.>RETURN.txt
 format a: /u /v:EmACT#%EMACS_MAJOR%-%EMACS_MINOR% < RETURN.txt
 echo.
 del RETURN.txt
 echo Insert Emacs disk in drive A: and press ENTER
 pause
 goto main

:maketmp
 if exist %TMPDIR% rmdir %TMPDIR% /s/q 2>&1 > nul
 mkdir %TMPDIR%							> nul
 mkdir %TMPDIR%\admin						> nul
 mkdir %TMPDIR%\conf						> nul
 mkdir %TMPDIR%\doc						> nul
 mkdir %TMPDIR%\etc						> nul
 mkdir %TMPDIR%\lib						> nul
 mkdir %TMPDIR%\src						> nul
 mkdir %TMPDIR%\src\pocket					> nul
 goto :eof

:updatelibs
 copy c:\usr\lib\emacs		lib				> nul
 copy lib\emacs.lsp		lib\EmACT.lsp			> nul
 goto :eof

:compress
 upx --best --force --crp-ms=100000 emacsxpa-i386.exe
 upx --best --force --crp-ms=100000 emacsxpu-i386.exe
 goto :eof

rem
rem	Copy distribution files
rem
:copyfiles
 copy ChangeLog			%TMPDIR%\ChangeLog		> nul
 copy COPYING			%TMPDIR%\COPYING		> nul
 copy README			%TMPDIR%\README			> nul
 copy convunix			%TMPDIR%\convunix		> nul
 copy configure			%TMPDIR%\configure		> nul
 copy configure.in		%TMPDIR%\configure.in		> nul
 copy emacs.dsp			%TMPDIR%\emacs.dsp		> nul
 copy emacs.dsw			%TMPDIR%\emacs.dsw		> nul
 copy emacs.hlp			%TMPDIR%\emacs.hlp		> nul
 copy install.bat		%TMPDIR%\install.bat		> nul
 copy Makefile			%TMPDIR%\Makefile		> nul
 copy makedisk.bat		%TMPDIR%\makedisk.bat		> nul
 copy savefile			%TMPDIR%\savefile		> nul
 copy emacsxpa-i386.exe		%TMPDIR%\emacsxpa-i386.exe	> nul
 copy emacsxpu-i386.exe		%TMPDIR%\emacsxpu-i386.exe	> nul
 copy etc\emgrep.exe		%TMPDIR%\emgrep.exe		> nul
 copy etc\emsed.exe		%TMPDIR%\emsed.exe		> nul
 copy etc\emtags.exe		%TMPDIR%\emtags.exe		> nul
 copy admin\tag.sh		%TMPDIR%\admin\tag.sh		> nul
 copy admin\update-version.sh   %TMPDIR%\admin\update-version.sh> nul
 copy conf\config.guess		%TMPDIR%\conf\config.guess	> nul
 copy conf\config.sub		%TMPDIR%\conf\config.sub	> nul
 copy conf\install-sh		%TMPDIR%\conf\install-sh	> nul
 copy conf\ltconfig		%TMPDIR%\conf\ltconfig		> nul
 copy conf\ltmain.sh		%TMPDIR%\conf\ltmain.sh		> nul
 copy conf\missing		%TMPDIR%\conf\missing		> nul
 copy doc\emacs.doc		%TMPDIR%\doc\emacs.doc		> nul
 copy doc\emacsen.doc		%TMPDIR%\doc\emacsen.doc	> nul
 copy doc\licencep.doc		%TMPDIR%\doc\licencep.doc	> nul
 copy etc\makefile.bnt		%TMPDIR%\etc\makefile.bnt	> nul
 copy etc\makefile.gnt		%TMPDIR%\etc\makefile.gnt	> nul
 copy etc\makefile.lcc		%TMPDIR%\etc\makefile.lcc	> nul
 copy etc\makefile.msc		%TMPDIR%\etc\makefile.msc	> nul
 copy etc\makefile.nt		%TMPDIR%\etc\makefile.nt	> nul
 copy etc\makefile.sc		%TMPDIR%\etc\makefile.sc	> nul
 copy etc\makefile.unx		%TMPDIR%\etc\makefile.unx	> nul
 copy etc\build.c		%TMPDIR%\etc\build.c		> nul
 copy etc\emtags.c		%TMPDIR%\etc\emtags.c		> nul
 copy etc\diffh.c		%TMPDIR%\etc\diffh.c		> nul
 copy etc\grep.c		%TMPDIR%\etc\grep.c		> nul
 copy etc\fnmatch.c		%TMPDIR%\etc\fnmatch.c		> nul
 copy etc\fnmatch.h		%TMPDIR%\etc\fnmatch.h		> nul
 copy etc\lowerdir.c		%TMPDIR%\etc\lowerdir.c		> nul
 copy etc\sed.c			%TMPDIR%\etc\sed.c		> nul
 copy lib\cpp.lsp		%TMPDIR%\lib\cpp.lsp		> nul
 copy lib\c.lsp			%TMPDIR%\lib\c.lsp		> nul
 copy lib\simple.lsp		%TMPDIR%\lib\simple.lsp		> nul
 copy lib\lelisp.lsp		%TMPDIR%\lib\lelisp.lsp		> nul
 copy lib\xilog.lsp		%TMPDIR%\lib\xilog.lsp		> nul
 copy lib\emacs.lsp		%TMPDIR%\lib\emacs.lsp		> nul
 copy lib\openlisp.lsp		%TMPDIR%\lib\openlisp.lsp	> nul
 copy lib\startup.lsp		%TMPDIR%\lib\startup.lsp	> nul
 copy lib\sort.lsp		%TMPDIR%\lib\sort.lsp		> nul
 copy lib\defstruc.lsp		%TMPDIR%\lib\defstruc.lsp	> nul
 copy lib\defclass.lsp		%TMPDIR%\lib\defclass.lsp	> nul
 copy lib\flet.lsp		%TMPDIR%\lib\flet.lsp		> nul
 copy lib\java.lsp		%TMPDIR%\lib\java.lsp		> nul
 copy lib\pretty.lsp		%TMPDIR%\lib\pretty.lsp		> nul
 copy lib\setf.lsp		%TMPDIR%\lib\setf.lsp		> nul
 copy src\pocket\build.bat	%TMPDIR%\src\pocket\build.bat	> nul
 copy src\pocket\emacs.inf	%TMPDIR%\src\pocket\emacs.inf	> nul
 copy src\pocket\emacs.ico	%TMPDIR%\src\pocket\emacs.ico	> nul
 copy src\pocket\emacs.vcp	%TMPDIR%\src\pocket\emacs.vcp	> nul
 copy src\pocket\emacs.vcw	%TMPDIR%\src\pocket\emacs.vcw	> nul
 copy src\Makefile.in 		%TMPDIR%\src\Makefile.in	> nul
 copy src\makefile.bnt		%TMPDIR%\src\makefile.bnt	> nul
 copy src\makefile.gnt		%TMPDIR%\src\makefile.gnt	> nul
 copy src\makefile.iva		%TMPDIR%\src\makefile.iva	> nul
 copy src\makefile.lcc		%TMPDIR%\src\makefile.lcc	> nul
 copy src\makefile.msc		%TMPDIR%\src\makefile.msc	> nul
 copy src\makefile.nt		%TMPDIR%\src\makefile.nt	> nul
 copy src\makefile.os2		%TMPDIR%\src\makefile.os2	> nul
 copy src\makefile.pm2		%TMPDIR%\src\makefile.pm2	> nul
 copy src\makefile.sc		%TMPDIR%\src\makefile.sc	> nul
 copy src\makefile.scn		%TMPDIR%\src\makefile.scn	> nul
 copy src\makefile.wnt		%TMPDIR%\src\makefile.wnt	> nul
 copy src\makefile.xp		%TMPDIR%\src\makefile.xp	> nul
 copy src\build.cnt		%TMPDIR%\src\build.cnt		> nul
 copy src\build.h		%TMPDIR%\src\build.h		> nul
 copy src\config.h.in 		%TMPDIR%\src\config.h.in	> nul
 copy src\defines.h		%TMPDIR%\src\defines.h		> nul
 copy src\dirent.h		%TMPDIR%\src\dirent.h		> nul
 copy src\emacs.h		%TMPDIR%\src\emacs.h		> nul
 copy src\mouse.h		%TMPDIR%\src\mouse.h		> nul
 copy src\pocket.h		%TMPDIR%\src\pocket.h		> nul
 copy src\pm20.h		%TMPDIR%\src\pm20.h		> nul
 copy src\version.h		%TMPDIR%\src\version.h		> nul
 copy src\wintport.h		%TMPDIR%\src\wintport.h		> nul
 copy src\winterm.h		%TMPDIR%\src\winterm.h		> nul
 copy src\xmem.h		%TMPDIR%\src\xmem.h		> nul
 copy src\xpterm.h		%TMPDIR%\src\xpterm.h		> nul
 copy src\ansi.c		%TMPDIR%\src\ansi.c		> nul
 copy src\basic.c		%TMPDIR%\src\basic.c		> nul
 copy src\buffer.c		%TMPDIR%\src\buffer.c		> nul
 copy src\charutil.c		%TMPDIR%\src\charutil.c		> nul
 copy src\curses.c		%TMPDIR%\src\curses.c		> nul
 copy src\dirent.c		%TMPDIR%\src\dirent.c		> nul
 copy src\display.c		%TMPDIR%\src\display.c		> nul
 copy src\emacs.c		%TMPDIR%\src\emacs.c		> nul
 copy src\file.c		%TMPDIR%\src\file.c		> nul
 copy src\filecomp.c		%TMPDIR%\src\filecomp.c		> nul
 copy src\fileio.c		%TMPDIR%\src\fileio.c		> nul
 copy src\ibmterm.c		%TMPDIR%\src\ibmterm.c		> nul
 copy src\indent.c		%TMPDIR%\src\indent.c		> nul
 copy src\line.c		%TMPDIR%\src\line.c		> nul
 copy src\llemacs.c		%TMPDIR%\src\llemacs.c		> nul
 copy src\main.c		%TMPDIR%\src\main.c		> nul
 copy src\minibuf.c		%TMPDIR%\src\minibuf.c		> nul
 copy src\mlisp.c		%TMPDIR%\src\mlisp.c		> nul
 copy src\mouse.c		%TMPDIR%\src\mouse.c		> nul
 copy src\mscterm.c		%TMPDIR%\src\mscterm.c		> nul
 copy src\newterm.c		%TMPDIR%\src\newterm.c		> nul
 copy src\nexterr.c		%TMPDIR%\src\nexterr.c		> nul
 copy src\ntterm.c		%TMPDIR%\src\ntterm.c		> nul
 copy src\openlisp.c		%TMPDIR%\src\openlisp.c		> nul
 copy src\options.c		%TMPDIR%\src\options.c		> nul
 copy src\os2term.c		%TMPDIR%\src\os2term.c		> nul
 copy src\pm20.c		%TMPDIR%\src\pm20.c		> nul
 copy src\pocket.c		%TMPDIR%\src\pocket.c		> nul
 copy src\random.c		%TMPDIR%\src\random.c		> nul
 copy src\rawmode.c		%TMPDIR%\src\rawmode.c		> nul
 copy src\region.c		%TMPDIR%\src\region.c		> nul
 copy src\search.c		%TMPDIR%\src\search.c		> nul
 copy src\spawn.c		%TMPDIR%\src\spawn.c		> nul
 copy src\tcap.c		%TMPDIR%\src\tcap.c		> nul
 copy src\termio.c		%TMPDIR%\src\termio.c		> nul
 copy src\tinfo.c		%TMPDIR%\src\tinfo.c		> nul
 copy src\unicode.c		%TMPDIR%\src\unicode.c		> nul
 copy src\version.c		%TMPDIR%\src\version.c		> nul
 copy src\vt52.c		%TMPDIR%\src\vt52.c		> nul
 copy src\window.c		%TMPDIR%\src\window.c		> nul
 copy src\winterm.c		%TMPDIR%\src\winterm.c		> nul
 copy src\wintport.c		%TMPDIR%\src\wintport.c		> nul
 copy src\word.c		%TMPDIR%\src\word.c		> nul
 copy src\x11.c			%TMPDIR%\src\x11.c		> nul
 copy src\xmalloc.c		%TMPDIR%\src\xmalloc.c		> nul
 copy src\xpterm.c		%TMPDIR%\src\xpterm.c		> nul
 copy src\pcterm.asm		%TMPDIR%\src\pcterm.asm		> nul
 copy src\putline.asm		%TMPDIR%\src\putline.asm	> nul
 copy src\pm20.def		%TMPDIR%\src\pm20.def		> nul
 copy src\winterm.def		%TMPDIR%\src\winterm.def	> nul
 copy src\ntterm.rc		%TMPDIR%\src\ntterm.rc		> nul
 copy src\pocket.rc		%TMPDIR%\src\pocket.rc		> nul
 copy src\pm20.rc		%TMPDIR%\src\pm20.rc		> nul
 copy src\winterm.rc		%TMPDIR%\src\winterm.rc		> nul
 copy src\xpterm.rc		%TMPDIR%\src\xpterm.rc		> nul
 copy src\pm20.ico		%TMPDIR%\src\pm20.ico		> nul
 copy src\winterm0.ico		%TMPDIR%\src\winterm0.ico	> nul
 copy src\winterm1.ico		%TMPDIR%\src\winterm1.ico	> nul
 copy src\x11.ico		%TMPDIR%\src\x11.ico		> nul
 copy src\emact.spec.in		%TMPDIR%\src\emact.spec.in	> nul
 copy src\macros.inc		%TMPDIR%\src\macros.inc		> nul
 copy src\link.dos		%TMPDIR%\src\link.dos		> nul
 copy src\link.os2		%TMPDIR%\src\link.os2		> nul
 copy src\link.sc		%TMPDIR%\src\link.sc		> nul
 copy src\emacs.reg		%TMPDIR%\src\emacs.reg		> nul
 copy src\emacs.rtf		%TMPDIR%\src\emacs.rtf		> nul
 copy src\emacs.hpj		%TMPDIR%\src\emacs.hpj		> nul
 goto :eof

:makezip
 cd %TMPDIR%\..
 if exist a:\%ARCHIVE%.zip del a:\%ARCHIVE%.zip
 zip -9 -r a:\%ARCHIVE%.zip %ARCHIVE%
 copy a:\%ARCHIVE%.zip %EMACSDIR% > nul
 copy c:\usr\bin\unzip.exe a:\    > nul
 cd %EMACSDIR%

:maketar
 if exist %ARCHIVE%.tar del %ARCHIVE%.tar
 cd %TMPDIR%\..
 zip -9 -r -q %ARCHIVE%.zip %ARCHIVE%
 chmod a+x %ARCHIVE%.zip
 copy %ARCHIVE%.zip %EMACSDIR% > nul
 del *.exe /s > nul
 tar cf %ARCHIVE%.tar %ARCHIVE%
 copy %ARCHIVE%.tar %EMACSDIR%\%ARCHIVE%.tar > nul
 chmod a+x %ARCHIVE%.tar
 gzip -9 %ARCHIVE%.tar
 chmod a+x %ARCHIVE%.tar.gz
 move %ARCHIVE%.tar.gz %EMACSDIR%\%ARCHIVE%.tar.gz > nul
 cd %EMACSDIR%
 echo Pocket PC install
 pushd src\pocket
 call build
 popd
 goto :eof

:updatesite
 copy emacs-Pocket2003.exe ftp\emact-Pocket2003.exe > nul
 copy doc\emacsen.doc      ftp > nul
 copy %ARCHIVE%.tar.gz     ftp > nul
 copy %ARCHIVE%.zip        ftp > nul
 copy COPYING              ftp > nul
 copy README               ftp > nul
 copy ChangeLog            ftp > nul
 goto :eof

:clean
 rmdir %TMPDIR% /s/q
 goto :eof

:wrongdisk
 echo This disk already contains data. Abort!
 goto end

:main
 call :updatelibs
 call :maketmp
 call :copyfiles
 call :maketar
 call :updatesite
 call :clean

:end
