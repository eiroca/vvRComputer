;******************************************************************************
;
; prelim.z80 - Preliminary Z80 tests - Copyright (C) 1994  Frank D. Cringle
; zexlax.z80 - Z80 instruction set exerciser - Copyright (C) 1994  Frank D. Cringle
; 8080 CPU support - Copyright (C) Ian Bartholomew
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;******************************************************************************
; compile with RetroAssembler
; Tab Size = 10
;

.lib

; display low nibble in a
.function phex1()
	push_Regs()
	TextPrintHex1()
	pop_Regs()
.endfunction

; display byte in a
.function phex2()
	push_Regs()
	TextPrintHex2()
	pop_Regs()
.endfunction

; display word in HL
.function phex4()
	push_Regs()
	TextPrintHex4()
	pop_Regs()
.endfunction

; display hex
; display the big-endian 32-bit value pointed to by hl
.function phex8()
	push_Regs()
	mvi	b, 4
@Loop	mov	a,m
	phex2()
	inx	h
	dcr	b
	jnz	@Loop
	pop_Regs()
.endfunction

; display hex string (pointer in hl, byte count in b)
.function hexstr()
@Loop	mov	a,m
	phex2()
	inx	h
	dcr	b
	jnz	@Loop
.endfunction

; fill memory at hl, bc bytes with value in E
.function fillmem()
	push_Regs()
	mov	m, E
	mov	d, h
	mov	e, l
	inx	d
	dcx	b
@Loop	mov	a, m
	stax	d
	inx	h
	inx	d
	dcx	b
	mov	a, b
	ora	c
	jnz	@Loop
	pop_Regs()
.endfunction

; clear memory at hl, bc bytes
.function clrmem()
	push_Regs()
	mvi	E, 0
@Loop	mov	M, E
	inx	h
	dcx	b
	mov	a, b
	ora	c
	jnz	@Loop
	pop_Regs()
.endfunction

; clear dword at HL
.function clrdword()
	push	PSW
	push	H
	xra	A
	mov	M, A
	inx	H
	mov	M, A
	inx	H
	mov	M, A
	inx	H
	mov	M, A
	pop	H
	pop	PSW
.endfunction

; fill dword at HL with A
.function filldword()
	push	H
	mov	M, A
	inx	H
	mov	M, A
	inx	H
	mov	M, A
	inx	H
	mov	M, A
	pop	H
.endfunction
