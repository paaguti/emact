#
#	@(#)	EmACT	(c) by Christian Jullien	2008/04/10
#
# Optional flags :
#
#	-D_ANSITERM	to use with ANSI terminal
#	-D_DIRECTORY	to use with file completion
#	-D_MLISP	to use with user defined macros.
#	-D_OPENLISP	to use with OpenLisp language.
#	-D_MOUSE	to use with Microsoft mouse.

SCDIR	= c:\scn
CC	= $(SCDIR)\bin\sc
AR	= $(SCDIR)\bin\lib
AS	= ml

#	Flags:

LISPVER	= _MLISP
LISPLIB	=
LISPINC	=

#LISPVER	= _OPENLISP
#LISPLIB	= olsym32.lib
#LISPINC	= -Iopenlisp

MACFLAGS= -D$(LISPVER) -D_DOS -D_DIRECTORY -D_MOUSE
CPU	= -4
OPTIM	= -o
SCFLAGS	= -v0 -mx -a4 -r -s- -w- -wx -I. -HX
CFLAGS	= $(SCFLAGS) $(CPU) $(OPTIM) $(MACFLAGS) $(LISPINC)
AFLAGS	= -nologo -c -I. $(MACFLAGS) -D__SC__
EMACSLIB= emsym32.lib
ARFLAG	= +

OBJECTS	= ansi.obj	\
	  basic.obj	\
	  buffer.obj	\
	  charutil.obj	\
	  dirent.obj	\
	  display.obj	\
	  emacs.obj	\
	  file.obj	\
	  filecomp.obj	\
	  fileio.obj	\
	  ibmterm.obj	\
	  indent.obj	\
	  line.obj	\
	  llemacs.obj	\
	  main.obj	\
	  minibuf.obj	\
	  mlisp.obj	\
	  mouse.obj	\
	  mscterm.obj	\
	  nexterr.obj	\
	  openlisp.obj	\
	  options.obj	\
	  random.obj	\
	  rawmode.obj	\
	  region.obj	\
	  search.obj	\
	  spawn.obj	\
	  termio.obj	\
	  version.obj	\
	  unicode.obj	\
	  window.obj	\
	  winterm.obj	\
	  word.obj	\
	  xmalloc.obj

ed.exe:	$(OBJECTS)
	@echo linking ..
	@$(CC) -oed $(SCFLAGS) -L/nologo -L/stack:0x8000 @link.sc $(LISPLIB)

.c.obj:
	@echo $<
	@$(CC) $(CFLAGS) -c $<

putline.obj:	putline.asm macros.inc
	@$(AS) $(AFLAGS) putline.asm

pcterm.obj:	pcterm.asm macros.inc
	@$(AS) $(AFLAGS) pcterm.asm

#	EmACT library

$(EMACSLIB):	$(OBJECTS)
	@if exist $(EMACSLIB) del $(EMACSLIB) > nul
	@$(AR)	$(EMACSLIB) y   $(ARFLAG) ansi.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) basic.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) buffer.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) dirent.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) display.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) emacs.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) file.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) filecomp.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) fileio.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) ibmterm.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) indent.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) line.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) llemacs.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) mscterm.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) minibuf.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) mlisp.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) mouse.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) nexterr.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) openlisp.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) options.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) pcterm.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) putline.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) random.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) rawmode.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) region.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) search.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) spawn.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) version.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) window.obj;
	@$(AR)	$(EMACSLIB)	$(ARFLAG) word.obj;

tags:
	emtags *.c *.h
