#
#	Generic Makefile for 'EmACT'	(c) C. Jullien 2008/04/10
#

# -----------------------------------------------------------------------------

# Select a valid SYSTEM in :
#
#	DOS Versions
#
#		dos-bor		;; Borland C/C++ 3.1
#		dos-msc		;; Microsoft Visual C/C++
#		dos-sym		;; Symantec C/C++ 7.1 (32 bits)
#
#	Windows 3.x Versions
#
#		win-bor		;; Borland C/C++ 3.1 (Windows)
#		win-msc		;; Microsoft Visual C/C++ (Windows)
#
#	Windows NT/2000 Versions
#
#		nt-bor		;; Borland C/C++ 4.0 (Character)
#		nt-msc		;; Microsoft VC++ (Character)
#		nt-sym		;; Symantec C/C++ 7.1 (32 bits)
#		nt-lcc		;; LCC compiler
#		nt-gcc		;; MinGW32
#		nt-gcc64	;; MinGW64
#		wnt-msc		;; Microsoft VC++ (Windows)
#
#	Windows XP Versions
#
#		xp-msc		;; Microsoft VC++ (GUI)
#		xpu-msc		;; Microsoft VC++ (GUI UNICODE)
#
#	OS/2 2.x/3.x Versions
#
#		os2-msc		;; Use and old MSC 6.0 C Compiler (Character)
#		os2-iva		;; IBM Visual Age for OS/2 (Character)
#		pm-bor		;; Borland C/C++ 1.5 for OS/2 PM
#		pm-iva		;; IBM Visual Age for OS/2 PM

SYSTEM=invalid-system	# display error
DEL=			# if equal 'no', (MSDOS) don't erase old object files
ISO=			# if equal _ISO compile using ISO/IEC 9845:1990 C
CMD=cmd /c

all: $(SYSTEM)

# --- Autoconf entry ---

auto:
	@(cd src; $(MAKE) -f Makefile all)

# --- UN*X Versions ---

invalid-system:
	@echo Invalid system.

scan:
	@echo	"You can use ISO if fscan is defined with prototypes"
	-@cc -E src/machine.c | grep fscan
	@echo	"You can use POSIX if posix is greater than 199000L"
	-@cc -E -D_POSIX_SOURCE src/machine.c | grep posix

# --- DOS Versions ---

ZIPEXE	= lzexe		# can use pklite, or replace by ECHO if you have none.

dos-bor:
	@echo DOS Borland
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.bc
	@cd ..
	@copy src\ed.exe emacsdos.exe > nul
	@$(ZIPEXE) emacsdos.exe
	@if exist *.OLD del *.OLD > nul

dos-msc:
	@echo DOS Microsoft
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.msc
	@cd ..
	@copy src\ed.exe emacsdos.exe > nul
	@$(ZIPEXE) emacsdos.exe
	@if exist *.OLD del *.OLD > nul

dos-sym:
	@echo DOS Symantec
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -f makefile.sc
	@cd ..
	@copy src\ed.exe emacs386.exe > nul

# --- Windows 3.1 Versions ---

win-bor:
	@echo Windows Borland
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.bw
	@cd ..
	@copy src\ed.exe emacswin.exe > nul
	@copy src\emacs.hlp > nul

win-msc:
	@echo Windows Microsoft
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.win
	@cd ..
	@copy src\ed.exe emacswin.exe > nul
	@copy src\emacs.hlp > nul

# --- Windows NT Versions ---

nt-bor:
	@echo Console NT Borland
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.bnt
	@cd ..
	@copy src\ed.exe emacsbnt.exe > nul

nt-msc:
	@echo Console NT Microsoft
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.nt
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.nt emmta32.lib
	@cd ..
	@copy src\ed.exe emacsnt.exe > nul
	@if exist ..\openlisp\README copy src\emmta32.lib ..\openlisp\src > nul

nt-sym:
	@echo Console NT Symantec
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.scn
	@cd ..
	@copy src\ed.exe emacsnt.exe > nul

nt-lcc:
	@echo Console NT lcc
	@cd src
	@-if [$(DEL)] == [] if exist *.obj del *.obj > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.lcc
	@cd ..
	@copy src\ed.exe emacslcc.exe > nul

nt-gcc:
	@echo Console NT MinGW 32
	@cd src
	@-$(CMD) "if [$(DEL)] == [] if exist *.o del *.o > nul"
	@$(MAKE) -$(MAKEFLAGS) -f makefile.gnt "CPUFLAG=-march=pentium -mwin32"
	@cd ..
	@copy src\ed.exe emacsgnt.exe > nul

nt-gcc64:
	@echo Console NT MinGW 64
	@cd src
	@-$(CMD) "if [$(DEL)] == [] if exist *.o del *.o > nul"
	@$(MAKE) -$(MAKEFLAGS) -f makefile.gnt \
	        "CROSSPREFIX=x86_64-w64-mingw32-" \
		"ARCH=64"
	@cd ..
	@copy src\ed.exe emacsgnt64.exe > nul

wnt-msc:
	@echo Windows NT Microsoft
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@-if [$(DEL)] == [] if exist *.PDB del *.PDB > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.wnt
	@cd ..
	@copy src\ed.exe emacswnt.exe > nul

wnt-bor:
	@echo Windows NT Borland
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.bw3
	@cd ..
	@copy src\emacs.exe emacswnt.exe > nul

xp-msc:
	@echo Microsoft Windows XP $(CPU)
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.xp \
	         CHARSET=_ANSICHARS CPU=$(CPU)
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@cd ..
	@copy src\ed.exe emacsxpa-$(CPU).exe > nul

xpu-msc:
	@echo Microsoft Windows XP UNICODE $(CPU)
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.xp \
	         CHARSET=_WIDECHARS CPU=$(CPU)
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@cd ..
	@copy src\ed.exe emacsxpu-$(CPU).exe > nul

# --- Windows 2000 IA64 Versions

i64-msc:
	@echo Console Windows 2000 - 64bits Microsoft
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -nologo -f makefile.i64
	@cd ..
	@copy src\ed.exe emacsi64.exe > nul

# --- OS/2 2.x/3.x Versions ---

os2-msc:
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.pm2
	@cd ..
	@copy src\ed.exe emacsos2.exe > nul

os2-iva:
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@$(MAKE) -$(MAKEFLAGS) -f makefile.iva
	@cd ..
	@copy src\ed.exe emacsos2.exe > nul

pm-bor:
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@make -$(MAKEFLAGS) -f makefile.bpm
	@cd ..
	@copy src\ed.exe emacsos2.exe > nul

pm-iva:
	@cd src
	@-if [$(DEL)] == [] if exist *.OBJ del *.OBJ > nul
	@-if [$(DEL)] == [] if exist *.RES del *.RES > nul
	@make -$(MAKEFLAGS) -f makefile.iva
	@cd ..
	@copy src\ed.exe emacsos2.exe > nul

# --- System tools ---

clean:
	@$(MAKE) $(MAKEOPT) -f src/Makefile clean

install:
	@$(MAKE) $(MAKEOPT) -f src/Makefile install

uninstall:
	@$(MAKE) $(MAKEOPT) -f src/Makefile uninstall

tarball:
	@$(MAKE) $(MAKEOPT) -f src/Makefile tarball

RPM:
	@rpm -ba src/emacs.spec
