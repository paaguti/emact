@echo off
rem build.bat creates the EmACT CAB files - C. Jullien: 2008/04/10
rem

setlocal

rem
rem You Must modify the following directories to point to the correct locations.
rem

set EMFILES="emacs.inf"
set CABPROG="cabtools\cabwiz.exe"
set WSEPROG="%ProgramFiles%\WinZip Self-Extractor\wzipse32.exe"
set CEDEVICE=ppc300_arm
set CEDEVICE=pocket2003

if exist "%ProgramFiles(x86)%\WinZip Self-Extractor\wzipse32.exe" set WSEPROG="%ProgramFiles(x86)%\WinZip Self-Extractor\wzipse32.exe"

if not exist %EMFILES% goto usage
if not exist %CABPROG% goto usage
if not exist %WSEPROG% goto usage

%CABPROG% %EMFILES% /err emacs.err /cpu %CEDEVICE%
if not exist emacs.%CEDEVICE%.CAB   goto error

move emacs.%CEDEVICE%.CAB setup > nul
pushd setup
zip -9 -q emacs-Pocket2003.zip licence.txt emacs.ini emacs.%CEDEVICE%.CAB setup.exe
%WSEPROG% emacs-Pocket2003.zip -setup -t ..\emacs.diz -le -st"EmACT for Pocket PC" -c setup.exe
move emacs-Pocket2003.exe c:\usr\jullien\emacs > nul
del  emacs-Pocket2003.zip
del  emacs.%CEDEVICE%.CAB
popd
del  emacs.%CEDEVICE%.DAT
goto end

:usage
echo Edit this batch file to point to the correct directories
echo    CABPROG = %CABPROG%
echo    EMFILES = %EMFILES%
echo    WSEPROG = %WSEPROG%
goto end

:error
if exist emacs.err type emacs.err

:end
