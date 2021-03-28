; Copyright (C) 2020-2021 Enrico Croce - AGPL >= 3.0
;
; This program is free software: you can redistribute it and/or modify it under the terms of the
; GNU Affero General Public License as published by the Free Software Foundation, either version 3
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
; Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License along with this program.
; If not, see <http://www.gnu.org/licenses/>.
;
;
; compile with RetroAssembler
; Tab Size = 10

	.target	"8080"

.lib

; Push all the registers
.macro push_Regs()
	push	PSW
	push	B
	push	D
	push	H
.endmacro

; Pull all the registers
.macro pop_Regs()
	pop	H
	pop	D
	pop	B
	pop	PSW
.endmacro

; HL <- SP
.macro _HLSP()
	lxi	H, 0
	dad	SP
.endmacro

; HL <- BC
.macro _HLBC()
	mov       H, B
	mov	L, C
.endmacro
; HL <- DE
.macro _HLDE()
	mov       H, D
	mov	L, E
.endmacro

; Z80 LDI emulation (but destroy A)
; A <- (HL)
; (DE) <- A
; DE <- DE + 1
; HL <- HL + 1
; BC <- BC - 1
.macro _LDI()
	mov	A, M
	stax	D
	inx	H
	inx	D
	dcx	B
.endmacro

; Repeats LDI until BC=0. Note that if BC=0 before this instruction is called, it will loop around until BC=0 again.
; Destrys A (A <- 0)
.macro _LDIR()
@Loop1	_LDI()
	mov	A, B
	ora	C
	jnz	@Loop1
.endmacro

; HL <- (HL)
; destroys A (A<-L)
.macro _HL_ind()
	mov	A, M
	inx	H
	mov	H, M
	mov	L, A
.endmacro

; HL <- (addr)+off
; destroys D (D <-off)
.macro _HL_ptr(addr, off)
	lhld	addr
	lxi	D, off
	dad	D
.endmacro

.macro SwapNibble()
; Swap the nibble in A  $1F -> $F1
	rlc
	rlc
	rlc
	rlc
.endmacro
