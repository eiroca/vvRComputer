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
	.target	"8080"

.macro	DebugTrace(msg)
	WriteTrace()
.if	(msg == 1) && (DEBUG >= 2)
 	; Print Instruction and state
	push_Regs()
	C_WRITESTR(crlf)
	lxi	h,iut
	mvi	b,IUT_SIZE
	hexstr()
    	C_WRITE(' ')
    	mvi	b,STATESIZE
    	lxi	h,msbt
	hexstr()
	pop_Regs()
.endif
.if	(msg == 2) && (DEBUG >= 2)
	; Print CRC and state
	push_Regs()
	C_WRITESTR(crlf)
	lxi	h,crcval
	phex8()
	C_WRITE(' ')
	lxi	h,msat
	mvi	b,STATESIZE
	hexstr()
	pop_Regs()
.endif
.if	(msg == 3) && (DEBUG >= 2)
	push_Regs()
	C_WRITESTR(crlf)
	lxi	h,counter
	mvi	b,MASKSIZE
	hexstr()
	C_WRITESTR(crlf)
	lxi	h,counter+MASKSIZE
	mvi	b,MASKSIZE
	hexstr()
	pop_Regs()
.endif
.if	(msg == 4) && (DEBUG >= 2)
	push_Regs()
	C_WRITESTR(crlf)
	lxi	h,shifter
	mvi	b,MASKSIZE
	hexstr()
	C_WRITESTR(crlf)
	lxi	h,shifter+MASKSIZE
	mvi	b,MASKSIZE
	hexstr()
	pop_Regs()
.endif
.if	(msg == 5) && (DEBUG >= 5)
	push_Regs()

	C_WRITESTR(crlf)
	C_WRITE('A')
	C_WRITE(' ')
	pop_Regs()
	push_Regs()
	TextPrintHex2()

	C_WRITE(' ')
	C_WRITE('B')
	C_WRITE(' ')
	pop_Regs()
	push_Regs()
	mov	H,B
	mov	L,C
	TextPrintHex4()

	C_WRITE(' ')
	C_WRITE('D')
	C_WRITE(' ')
	pop_Regs()
	push_Regs()
	xchg
	TextPrintHex4()

	C_WRITE(' ')
	C_WRITE('H')
	C_WRITE(' ')
	pop_Regs()
	push_Regs()
	TextPrintHex4()

	pop_Regs()
.endif
.endMacro
