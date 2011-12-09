#
#	Makefile for 'EmACT tools' (c) C. Jullien 	1998/04/24
#

#	-D_ISO			For ISO/IEC 9899:1990 C Compilers

#	Tools:

CC	= \scn\bin\sc
DEFS	= -D_ISO -D_DOS -DC_CALL=__cdecl
CPU	= -4
CFLAGS	= -a4 -mx $(CPU) -o -s- -w $(DEFS)
LDFLAGS	= -mx -v0 -L/nologo -L/stack:0x8000

all: emgrep.exe emtags.exe emsed.exe

emgrep.exe:	grep.obj
	@$(CC) $(LDFLAGS) -oemgrep.exe grep.obj

emtags.exe:	emtags.obj
	@$(CC) $(LDFLAGS) -oemtags.exe emtags.obj

emsed.exe:	sed.obj
	@$(CC) $(CFLAGS) -oemsed.exe sed.obj

.c.obj:
	@$(CC) $(CFLAGS) $<
