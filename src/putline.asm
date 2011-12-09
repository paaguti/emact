;;============================================================================
;; Date:	"2009/05/02"
;; Title:	"putline.asm"
;; Author:	"Christian Jullien"
;;
;;		Quick redisplay screen for PC / AT (memory mapped)
;;
;; This  program  is  free  software;  you can redistribute it and/or
;; modify  it  under  the  terms of the GNU General Public License as
;; published  by  the  Free  Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;; 
;; This  program  is  distributed in the hope that it will be useful,
;; but  WITHOUT ANY WARRANTY;  without  even the implied  warranty of
;; MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You  should have received a copy of the GNU General Public License
;; along  with  this  program;  if  not,  write  to the Free Software
;; Foundation,  Inc.,  59  Temple  Place  -  Suite  330,  Boston,  MA
;; 02111-1307, USA.
;;============================================================================
 
	include	macros.inc

ifndef		_ANSITERM

;-*-:05-23-94:	Add support for Symantec C/C++ in Protected Mode.
;-*-:06-15-88:	Putcell & sgetc added
;-*-:02-06-88:	Work in protected mode (A.I. Architects)
;-*-:02-14-87:	Can change memory model on command line (ex: -DSmall)
;-*-:09-16-86:	Compact Model Support Model (MS C 4.00)
;-*-:09-13-86:	Big Model support.
;-*-:09-04-86:	Eliminate snow in any case.
;-*-:05-23-86: 	Initial release

;	videoinit( void )		initialise screen segment
;	putline(  row, col, line, att )	put line at (row, col)
;	putcells( row, col, line, n )	put n cells at (row, col)
;	sputc(    row, col, char, att )	put char at (row, col)
;	sgetc(    row, col )		get cell at (row, col)

ncolumns	equ	80
nrows		equ	60
 
ifdef		__SC__
CodeSection
extrn		__x386_mk_protected_ptr:dword
EndCode
endif

DataSection

ADDR_6845	dw	?			; 6845 data port
STAT_6845	dw	?			; 6845 status port
oscreen_base	dw	0B000h			; screen segment
init_flag	dw	0			; initialized ?

EndData

ifndef	__SC__
		extrn	_fast_redisplay:word
endif

CodeSection
 
ifdef	__SC__

loadsi		macro	arg
		mov	esi,arg		; arg in esi
		endm

prologue	macro
		push	es
		push	ds
		pushf
		xor	esi,esi		; clear esi
		xor	edi,edi		; clear esi
		xor	ebx,ebx		; clear ebx
		xor	eax,eax		; clear eax
		mov	ecx,1		; fast redisplay = t
		endm
		
epilogue	macro
		popf
		pop	ds
		pop	es
		endm

else

loadsi		macro	arg
		mov	si,arg
		endm

prologue	macro
		push	si			; save SI
		push	di			; save DI
		push	es			; save ES:
		push	ds			; save DS:
		pushf				; save flags
		mov	bx,DGROUP		; source is in DGROUP
		mov	ds,bx			; set source (DS:)
		mov	bx,seg _fast_redisplay	; load segment
		mov	es,bx			; into es
		mov	bx,offset _fast_redisplay ; load offset
		mov	cx,es:[bx]		; fast redisplay flag
		endm
		
epilogue	macro
		popf				; restore flags
		pop	ds			; restore DS:
		pop	es			; restore ES:
		pop	di			; restore DI:
		pop	si			; restore SI:
		endm

endif

;	NAME
;			videoinit
;
;	DESCRIPTION
;			get system parameters for the video card
;
;	USAGE
;		   	videoinit();
;
;	RETURNS
;			void
 
define		videoinit
ifndef		__SC__
if		Memory_Model eq Large_Model
		mov	bx,DGROUP		; source is in DGROUP
		mov	ds,bx			; set source (DS:)
endif
endif
		cmp	init_flag,0		; already defined ?
		jnz	init_exit		; exit if so
		mov	init_flag,1		; and set it if not
		xor	dx,dx			; use default dx
		xor	bx,bx			; use default bx
		mov	ax,01130h		; EGA BIOS present ?
		cmp	dl,0			; still 0 (no == EGA/VGA)
		jne	video_color		; Color
		int	011h			; get equipment
		and	ax,00110000b		; mask for display equipment
		cmp	ax,00110000b		; is it B&W controler card ?
		jne	video_color		; Color
video_BW:	mov	ADDR_6845,003B4h	; adress of B&W card
		mov	al,02h			; 80*25 B&W
		jmp	short video_end		; set mode
video_color:	mov	ADDR_6845,003D4h	; adress of color card
		mov	al,03h			; 80*25 Color
		mov	oscreen_base,0B800h	; base is 0B800h in color !
video_end:	mov	bx,ADDR_6845		; compute the status
		add	bx,6			; port adress
		mov	STAT_6845,bx		; for fast no-flickering
		xor	ah,ah			; Set mode in al
		int	010h			; call BIOS
ifdef		_OS286
		push	si
		mov	si,0Bh			; set the 32 Bits addr
		mov	bx,00000h		; low word
		cmp	oscreen_base,0B000h	; SI:BX = 0B0000h (mono)
		je	real_window
		mov	bx,08000h		; SI:BX = 0B8000h (color)
real_window:	mov	cx,0			; high bit size = 0
		mov	dx,60*80*2		; the size of the screen
		mov	ax,0E803h
		int	021h			; create a window segment
		mov	oscreen_base,ax		; new selector
		pop	si
endif	;	_OS286
ifdef		__SC__
		pusha				; push all registers
		xor	eax,eax			; clear EAX
		mov	ax,oscreen_base		; copy the screen base
		shl	eax,4			; convert to absolute address
		push	eax			; push the address
		call	__x386_mk_protected_ptr	; get a new selector
		add	esp,4			; clear th stack
		mov	oscreen_base,dx		; store the selector
		popa				; restore all register
endif
init_exit:
endef		videoinit


;	NAME
;			putline
;
;	DESCRIPTION
;			Write a string directly on screen at (row, col)
;
;	USAGE
;			putline( row, col, line, n, attribute );
;
;	RETURNS
;			void


define		putline
		prologue

		mov	bx,oscreen_base		; screen location
		mov	es,bx			; set destination (ES:)
		mov	ax,arg1			; source is the argument
		cmp	ax,0			; >= 0
		jl	done			; out of screen
		cmp	ax,59			; < 60 lines
		jg	done			; out of screen
		mov	bx,ncolumns		; number of columns
		mul	bx			; source line = start * 80
		mov	bx,arg2			; get col
		cmp	bx,0			; >= 0
		jl	done			; out of screen
		cmp	bl,79			; < 80 col
		jg	done			; out of screen
		add	ax,bx			; source = nrow * 80 + ncol
		shl	ax,1			; takes account of attribute
		mov	di,ax			; di is the destination
if		Pointer_Size eq _32Bits
		loadsi	arg3			; get source offset in arg3
		mov	bx,arg4			; get segment on stack
		mov	ds,bx			; set source segment (DS:)
		mov	dx,arg5			; get nb char to write
		mov	bh,byte ptr arg6	; get color / attribute
else
		loadsi	arg3			; get source offset in arg3
		mov	dx,arg4			; get nb char to write
		mov	bh,byte ptr arg5	; get color / attribute
endif
		cld				; forward
		cmp	cx,0			; fast display ?
		jnz	fast
			
putc:
		lodsb				; get character
		or	al,al			; EOL?
		jz	done			; the end
		mov	bl,al			; character & attribute (bx)
 
 		call	retrace

		mov	ax,bx			; restore character
		stosw				; store char & attribute

		inc	cx			; inc counter
		cmp	cx,ncolumns		; end of line ?
		jge	done			; no more 80 col
		jmp	short putc

fast:
		mov	ah,bh			; set attribute
		xor	cx,cx			; raz counter
putfast:	lodsb				; get character
		or	al,al			; EOL?
		jz	done			; the end
		stosw				; store char & attribute
		inc	cx			; inc counter
		cmp	cx,dx			; end of line ?
		jge	done			; no more 80 col
		jmp	short putfast		; loop
done:
		epilogue
endef		putline
 
ifdef		__SC__

;	NAME
;			putcells
;
;	DESCRIPTION
;			Write cells directly on screen at (row, col)
;
;	USAGE
;			putcells( row, col, cells, len );
;
;	RETURNS
;			void
 
define		putcells
		prologue

		mov	bx,oscreen_base		; screen location
		mov	es,bx			; set destination (ES:)
		mov	ax,arg1			; source is the argument
		cmp	ax,0			; >= 0
		jl	cdone			; out of screen
		cmp	ax,59			; < 60 lines
		jg	cdone			; out of screen
		mov	bx,ncolumns		; number of columns
		mul	bx			; source line = start * 80
		mov	bx,arg2			; get col
		cmp	bx,0			; >= 0
		jl	cdone			; out of screen
		cmp	bl,79			; < 80 col
		jg	cdone			; out of screen
		add	ax,bx			; source = nrow * 80 + ncol
		shl	ax,1			; takes account of attribute
		mov	di,ax			; di is the destination
		loadsi	arg3			; get source offset in arg3
if		Pointer_Size eq _32Bits
		mov	bx,arg4			; get segment on stack
		mov	ds,bx			; set source segment (DS:)
		mov	bh,byte ptr arg5	; get length in bh
else
		mov	bh,byte ptr arg4	; get length in bh
endif
		cld				; forward
		cmp	cx,0			; fast display ?
		jnz	cfast
			
cputc:
		or	bh,bh			; all done ?
		jz	cdone			; the end
		lodsw				; get character/attrib in ax
		push	ax			; save char
 
 		call	retrace

		pop	ax			; restore character
		stosw				; store char & attribute

		dec	bh			; one more character
		inc	cx			; inc counter
		cmp	cx,ncolumns		; end of line ?
		jge	cdone			; no more 80 col
		jmp	short cputc

cfast:
		xor	cx,cx			; raz counter
cputfast:
		or	bh,bh			; all done ?
		jz	cdone			; the end
		lodsw				; get character
		stosw				; store char & attribute
		dec	bh			; one more character
		inc	cx			; inc counter
		cmp	cx,ncolumns		; end of line ?
		jge	cdone			; no more 80 col
		jmp	short cputfast		; loop
cdone:
		epilogue
endef		putcells
 
;	NAME
;			sputc
;
;	DESCRIPTION
;			Write a char directly on screen at (row, col)
;
;	USAGE
;			sputc( row, col, char, attribute );
;
;	RETURNS
;			void
 
define		sputc
		prologue

		mov	bx,oscreen_base		; screen location
		mov	es,bx			; set destination (ES:)
		mov	ax,arg1			; source is the argument
		cmp	ax,0			; >= 0
		jl	sdone			; out of screen
		cmp	ax,59			; < 60 lines
		jg	sdone			; out of screen
		mov	bx,ncolumns		; number of columns
		mul	bx			; source line = start * 80
		mov	bx,arg2			; get col
		cmp	bx,0			; >= 0
		jl	sdone			; out of screen
		cmp	bl,79			; < 80 col
		jg	sdone			; out of screen
		add	ax,bx			; source = nrow * 80 + ncol
		shl	ax,1			; takes account of attribute
		mov	di,ax			; di is the destination
		mov	ax,arg3			; get char in arg3
		mov	bl,al			; and set bl (icone)
		mov	ax,arg4			; get color
		mov	bh,al			; set in bh
		cld				; forward
		cmp	cx,0			; fast display ?
		jnz	sfast			; no flickering screen

 		call	retrace

sfast:		mov	ax,bx			; char & attrib in ax
		stosw				; store char & attribute

sdone:		epilogue
endef		sputc
 
;	NAME
;			sgetc
;
;	DESCRIPTION
;
;			Get a char directly on screen at (row, col)
;
;	USAGE
;			sgetc( row, col );
;
;	RETURNS
;			cell at (row, col)
 
define		sgetc
		prologue

		mov	bx,oscreen_base		; screen location
		mov	ds,bx			; set source (DS:)
		mov	ax,arg1			; source is the argument
		cmp	ax,0			; >= 0
		jl	gdone			; out of screen
		cmp	ax,59			; < 60 lines
		jg	gdone			; out of screen
		mov	bx,ncolumns		; number of columns
		mul	bx			; source line = start * 80
		mov	bx,arg2			; get col
		cmp	bx,0			; >= 0
		jl	gdone			; out of screen
		cmp	bl,79			; < 80 col
		jg	gdone			; out of screen
		add	ax,bx			; source = nrow * 80 + ncol
		shl	ax,1			; takes account of attribute
		mov	si,ax			; resSI is the source
		cld				; forward
		cmp	cx,0			; fast display ?
		jnz	gfast			; no flickering screen
			
		call	retrace

		lodsw				; load char & attribute
 		jmp	short gdone

gfast:		lodsw				; load char & attribute

gdone:		epilogue
endef		sgetc
 
endif	;	__SC__

;		wait for horizontal retrace (port in dx & modify ax)

retrace	proc	near
		push	dx			; save dx
		mov	dx,STAT_6845		; get status port adress
		cli				; begin of the crucial section
		mov	ah,09h			; Mask for retrace
wait1:		in	al,dx			; get status
		rcr	al,1
		jb	wait1			; and loop
wait2:		in	al,dx			; get status
		and	al,ah			; display enable?
		jz	wait2			; wait until not
		sti				; reset interrup
 		pop	dx			; restore dx
		ret				; then return
retrace	endp

EndCode

endif	;	_ANSITERM
		end
