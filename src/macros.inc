;;
;;	sccsid = "@(#)macros.inc	(c) Christian Jullien 2009/05/02"
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

ifdef	__SC__
	.386

_TEXT	segment dword use32 public 'CODE'	;size is 0
_TEXT	ends
_DATA	segment dword use32 public 'DATA'	;size is 0
_DATA	ends
CONST	segment dword use32 public 'CONST'	;size is 0
CONST	ends
_BSS	segment dword use32 public 'BSS'	;size is 0
_BSS	ends

DGROUP	group	CONST,_BSS,_DATA

	assume	cs:_TEXT, ds:DGROUP

CodeSection	macro
_TEXT	segment dword use32 public 'CODE'	;size is 206
		endm

EndCode		macro
_TEXT	ends
		endm

DataSection	macro
_DATA	segment dword use32 public 'DATA'	;size is 48
		endm

EndData		macro
_DATA	ends
		endm

FirstArg	equ	8
ArgSize		equ	4
Pointer_Size	equ	-1
_32Bits		equ	-2

;		Standard code at the begining of a new procedure

define		macro	name
		public	_&name
_&name		proc	near
		push	ebp			; save ebp
		mov	ebp,esp			; to access arguments
		push	ebx
		push	ecx
		push	edx
		push	esi
		push	edi
		endm

;		Standard code at the end of a new procedure
 
endef		macro	name
		pop	edi
		pop	esi
		pop	edx
		pop	ecx
		pop	ebx
		mov	esp,ebp			; restore sp
		pop	ebp			; and bp
		ret				; then return
_&name		endp
		endm

arg1		equ	[ebp +  FirstArg]
arg2		equ	[ebp +  FirstArg + 1 * ArgSize]
arg3		equ	[ebp +  FirstArg + 2 * ArgSize]
arg4		equ	[ebp +  FirstArg + 3 * ArgSize]
arg5		equ	[ebp +  FirstArg + 4 * ArgSize]
arg6		equ	[ebp +  FirstArg + 5 * ArgSize]

RegAX		equ	eax
RegBX		equ	ebx
RegCX		equ	ecx
RegDX		equ	edx
RegSP		equ	esp
RegBP		equ	ebp
Memory_Model	equ	-1

endif	; __SC__

ifndef	__SC__

Small_Model	equ	0
Compact_Model	equ	1
Large_Model	equ	2
Medium_Model	equ	3
 
_16Bits		equ	0
_32Bits		equ	1
 
ifdef	smodel
Memory_Model	equ	Small_Model
Pointer_Size	equ	_16Bits
endif

ifdef	cmodel
Memory_Model	equ	Compact_Model
Pointer_Size	equ	_32Bits
endif

ifdef	mmodel
Memory_Model	equ	Medium_Model
Pointer_Size	equ	_16Bits
endif

ifdef	lmodel
Memory_Model	equ	Large_Model
Pointer_Size	equ	_32Bits
endif

ifndef	Memory_Model
Memory_Model	equ	Small_Model
Pointer_Size	equ	_16Bits
endif

;	Static Name Aliases
;
 
ifndef	__TURBOC__
CONST		segment word public 'CONST'
CONST		ends
_BSS		segment word public 'BSS'
_BSS		ends
_DATA		segment word public 'DATA'
_DATA		ends
DGROUP		group	CONST,	_BSS,	_DATA
endif

ifdef	__TURBOC__
_BSS		segment word public 'BSS'
_BSS		ends
_DATA		segment word public 'DATA'
_DATA		ends
DGROUP		group	_BSS,	_DATA
endif

if		Memory_Model eq Large_Model or Memory_Model eq Medium_Model

CodeSection	macro
_PCTERM_TEXT	segment byte public 'CODE'
		endm

EndCode		macro
_PCTERM_TEXT	ends
		endm

CodeSection
EndCode

		assume  cs: _PCTERM_TEXT, ds: DGROUP, ss: DGROUP, es: DGROUP

else		; SMALL MEMORY MODEL

CodeSection	macro
_TEXT		segment byte public 'CODE'
		endm

EndCode		macro
_TEXT		ends
		endm

CodeSection
EndCode

		assume  cs: _TEXT, ds: DGROUP, ss: DGROUP, es: DGROUP
endif
 
DataSection	macro
_DATA	segment	word public 'DATA'
		endm

EndData		macro
_DATA	ends
		endm

if		Memory_Model eq Large_Model or Memory_Model eq Medium_Model
FirstArg	equ	6
ArgSize		equ	2
else
FirstArg	equ	4
ArgSize		equ	2
endif

;		Standard code at the begining of a new procedure

define		macro	name
		public	_&name
if		Memory_Model eq Large_Model or Memory_Model eq Medium_Model
_&name		proc	far
else
_&name		proc	near
endif
		push	bp			; save bp
		mov	bp,sp			; to access arguments
		endm
 
;		Standard code at the end of a new procedure
 
endef		macro	name
		mov	sp,bp			; restore sp
		pop	bp			; and bp
		ret				; then return
_&name		endp
		endm

arg1		equ	[bp +  FirstArg]
arg2		equ	[bp +  FirstArg + 1 * ArgSize]
arg3		equ	[bp +  FirstArg + 2 * ArgSize]
arg4		equ	[bp +  FirstArg + 3 * ArgSize]
arg5		equ	[bp +  FirstArg + 4 * ArgSize]
arg6		equ	[bp +  FirstArg + 5 * ArgSize]

RegAX		equ	ax
RegBX		equ	bx
RegCX		equ	cx
RegDX		equ	dx
RegSP		equ	sp
RegBP		equ	bp

endif	; __SC__
