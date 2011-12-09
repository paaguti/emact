;;============================================================================
;; Date:	"2009/05/02"
;; Title:	"pcterm.asm"
;; Author:	"Christian Jullien"
;;
;;		Screen and keyboard interface routines for MS-DOS
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

;;-*-:02-02-95:	Add waitkey function.
;;-*-:11-09-94:	Ctrl-space returns 0 (same as Ctrl-@).
;;-*-:06-17-94:	Protected mode support for machines with more than 16 Mo.
;;-*-:14-06-94:	preserve EBP register across video calls in protected mode.
;;-*-:22-05-94:	add support for Symantec C/C++ in protected mode.
;;-*-:27-11-89:	merge with keyboard.asm.
;;-*-:02-14-87:	can change memory model on command line (ex: -DSmall).
;;-*-:09-16-86:	compact Model Support Model (MS C 4.00).
;;-*-:07-07-86:	cleos bug fixed.
;;-*-:24-03-86:	get equipment added.
;;-*-:22-03-86:	cleol bug fixed.
;;-*-:15-12-85:	windows attribute.
;;-*-:09-12-85: Initial release.
;;
;;			masm pcterm.asm /ml

;;	int	testkey( void )			test presence of key stoke
;;	int	waitkey( void )			get key stoke
;;	void	beep( void )			sounds the beeper
;;	void	cleol( int color )		clear end of line
;;	void	cleos( int color )		clear end of screen
;;	void	curpos( int row, int col )	set cursor postion to (row, col)
;;	void	equipment( void )		get equipment
;;	void	tputc( int c, int color )	write char at current position

video		macro
		push	RegBP			; save ebp register
		int	VideoInt		; call bios video io
		pop	RegBP			; restaure ebp register
		endm

;;	equates for bios interface.

;;	the interrupt and codes for the screen interface interrupt.

VideoInt	equ	10h			; interrupt for screen
                                               
SetCursor	equ	 2			; set cursor position
ReadCursor	equ	 3			; read cursor location
ScrollUp	equ	 6			; scroll screen up
ScrollDown	equ	 7			; scroll screen nown
ReadChar	equ	 8			; read a character from screen
WriteCharAttrib	equ	 9			; write char and attributes

;;	caution: must change column number if 40 column mode

ncolumns	equ	80
nrows		equ	25

	public	_cols
	public	_rows

DataSection

_cols		dw	ncolumns		; current number of columns
_rows		dw	nrows			; current number of rows

EndData

CodeSection


;;	curpos		sets cursor at any location.
;;			Usage:	curpos(new_row, new_column);

define		curpos
		mov	RegDX,arg2		; column
		mov	RegAX,arg1		; row
		mov	dh,al			; dh = row
		mov	bh,0			; force page zero
		mov	ah,SetCursor		; set cursor location
		video				; call bios
endef		curpos


;;	cleol		clear rest of line.
;;			Usage:	cleol(color);

define		cleol
		xor	RegDX,RegDX		; clear dx (for 32 bits)
		mov	bh,0			; page 0
		mov	ah,ReadCursor		; see where we are
		video				; call bios
		xor	RegCX,RegCX		; clear cx (for 32 bits)
		mov	cx,dx			; current pos was in dx
		mov	dl,byte ptr _cols	; clear end of line
		dec	dl			; last column is _cols-1
		mov	RegBX,arg1		; current color
		mov	bh,bl			; in bh
		xor	al,al			; for a clear window
		mov	ah,ScrollUp		; write the blanks
		video				; call bios
endef		cleol


;;	cleos		clear rest of screen.
;;			Usage:	cleos( color );

define		cleos
		push	arg1			; push color
		call	_cleol			; clear rest of line
		add	RegSP,ArgSize		; 1 argument pushed
		mov	bh,0			; page 0
		mov	ah,ReadCursor		; see where we are
		video				; call Video
		mov	al,0			; clear entire window
		mov	ch,dh			; current row
		inc	ch			; +1
		cmp	ch,byte ptr _rows	; see if in last line
		jz	cleared			; all done
		mov	cl,0			; first column
		mov	dh,byte ptr _rows	; last rows
		dec	dh			; last rows is height-1
		mov	dl,byte ptr _cols	; clear entire width
		dec	dl			; last column is width-1
		mov	RegBX,arg1		; current color
		mov	bh,bl			; in bh
		mov	ah,ScrollUp		; for a ScrollUp to clear
		video				; do the clear
cleared:
endef		cleos

;;	equipment()	Get Equipment of the computer
;;			Usage: equipment();
;;			Return values:
;;
;;				BIT 15,14	# of printer
;;				BIT 13		NOT used
;;				BIT 12		Game IO
;;				BIT 11,10,9	# of RS232
;;				BIT 8		NOT used
;;				BIT 7,6		# of diskette drive
;;				BIT 5,4		Initial video mode
;;				BIT 3,2		Planar RAM size
;;				BIT 1		NOT used
;;				BIT 0		IPL from the system

define		equipment
		int	011h			; get equipment
endef		equipment

;;	tputc		Write a character to the screen and
;;			increments the cursor position.
;;			Usage:	tputc( character, color );

define		tputc
		mov	RegBX,arg2		; attribute used
		mov	bh,0			; current page
		mov	RegCX,1			; only one char
		mov	al,arg1			; character to write
		mov	ah,WriteCharAttrib	; write char with attribute
		int	VideoInt		; call bios
endef		tputc

;;	testkey		return character if any available. otherwise -1.

define		testkey
 		mov	ah,06h			; direct i/o
 		mov	dl,0FFh			; ask for a keyboard character
 		int	021h			; call MS-DOS
 		jnz	ready			; got one
 		mov	ax,-1			; not yet
 		jmp	short retkey
ready:		xor	ah,ah			; it's a char !
		and	RegAX,0FFh
		cmp	al,0			; extended ?
		jne	isspace
 		mov	ah,06h			; direct i/o
 		mov	dl,0FFh			; ask for a keyboard character
 		int	021h			; call MS-DOS
		or	RegAX,0100h		; mark as extended
		jmp	short retkey
isspace:
		cmp	ax,020h			; chek for space
		jne	short retkey		; no ? return
;;
;;	Use status key, ret code in al :
;;
;;				BIT 7	= insert on
;;				BIT 6	= Caps Lock on
;;				BIT 5	= Num Lock on
;;				BIT 4	= Scroll Lock on
;;				BIT 3	= Alt key down
;;				BIT 2	= Ctrl key down
;;				BIT 1	= Left shift key down
;;				BIT 0	= Right shift key down
;;
		mov	bx,ax			; save char
		mov	ah,02h			; ask for ctrl
		int	016h			; keyboard flags
		and	al,4			; bit 2 is for ctrl
		jz	short space		; no ctrl ? really a space
		mov	bx,0h			; Ctrl-' ' means 0 !!
space:
		mov	ax,bx			; set char
retkey:
endef		testkey

;;	waitkey		read a character

define		waitkey
 		mov	ah,07h			; direct i/o
 		int	021h			; call MS-DOS
		and	RegAX,0FFh
		cmp	al,0			; extended ?
		jne	wisspace
 		mov	ah,07h			; direct i/o
 		int	021h			; call MS-DOS
		or	RegAX,0100h		; mark as extended
		jmp	short wretkey
wisspace:
		cmp	ax,020h			; chek for space
		jne	short wretkey		; no ? return
;;
;;	Use status key, ret code in al :
;;
;;				BIT 7	= insert on
;;				BIT 6	= Caps Lock on
;;				BIT 5	= Num Lock on
;;				BIT 4	= Scroll Lock on
;;				BIT 3	= Alt key down
;;				BIT 2	= Ctrl key down
;;				BIT 1	= Left shift key down
;;				BIT 0	= Right shift key down
;;
		mov	bx,ax			; save char
		mov	ah,02h			; ask for ctrl
		int	016h			; keyboard flags
		and	al,4			; bit 2 is for ctrl
		jz	short wspace		; no ctrl ? really a space
		mov	bx,0h			; Ctrl-' ' means 0 !!
wspace:
		mov	ax,bx			; set char
wretkey:
endef		waitkey

;;	beep		sounds the beeper

TIMER		equ	040h
PORT_B		equ	061h

define		beep
		mov	bx,1
		mov	al,10110110B
		out	TIMER+3,al
		jmp	short $+2
		mov	ax,200h			; standard tone = 533
		out	TIMER+2,al
		jmp	short $+2
		mov	al,ah
		out	TIMER+2,al
		in	al,PORT_B
		mov	ah,al
		jmp	short $+2
		or	al,03
		out	PORT_B,al
		mov	RegCX,0FFFFh
sound:		loop	sound
		dec	bl
		jnz	sound
		mov	al,ah
		out	PORT_B,al
beepout:
endef		beep


EndCode

endif	;	_ANSITERM

		end
