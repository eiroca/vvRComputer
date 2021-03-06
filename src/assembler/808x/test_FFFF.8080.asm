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
; 8080 vComputer Firmware
; Bank FF (boot loader)
;
; compile with RetroAssembler

	.target	"8080"
	.format	"bin"

.include "./lib/libUtils.8080.asm"
.include "./lib/libToolKit.8080.asm"

.macro ToolKitEntry(ID, DstBank=$F, BlockSize=1, SrcBlock, VT_Address)
	.byte	ID
	.byte	DstBank * 16 + BlockSize
	.word	SrcBlock
	.word	VT_Address
.endmacro

.macro CallToolKit(TK_ID, TK_call)
	rst	7
	.byte	TK_ID
	.byte	TK_call
.endmacro

.segment "Fake"
;make sure 4k bank
	.org	$0FFF
	hlt

.code
RST_0	.org	$0000
	nop		; work-around for RetroAssembler bug
	jmp	Boot

RST_1	.org	$0008
	hlt

RST_2	.org	$0010
	hlt

RST_3	.org	$0018
	hlt

RST_4	.org	$0020
	hlt

RST_5	.org	$0028
	hlt

RST_6	.org	$0030
	hlt

; Toolkit call
; RST 7
;
RST_7	.org	$0038
	jmp	RToolKit

RST_V	.org	$0040
	hlt

Boot	.org	$0200
	lxi	SP, Stack
	jmp	TestCode

TestCode	.org	$0400
	lda	bcd	;load the bcd number
	call	bcd2bin
	sta	bin
	CallToolKit(1, 1)
	hlt

; Data
bcd	.byte	$99
bin	.byte	$00

; Toolkits
	.org	$0500
; Call a ToolKit
;
; call RToolKit
; .byte toolkit
; .byte function
RToolKit	pop	H
	mov	B, M
	inx	H
	mov	C, H
	inx	H
	push	H
	ret
; Call a ToolKit
; Input B -> Toolkit Number
; Input C -> Toolkit Function
DoToolKit	hlt

TK_List	.byte	1	; How many toolkits
	ToolKitEntry(1, 0, 1, 0, TK_01)

TK_01	.byte	1	; Number of entrypoints
	.word	bcd2bin	; Toolkit tool 1

Stack	.equ	$1000
