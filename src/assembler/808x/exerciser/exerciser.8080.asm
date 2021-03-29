;******************************************************************************
;
; prelim.z80 - Preliminary Z80 tests - Copyright (C) 1994 Frank D. Cringle
; zexlax.z80 - Z80 instruction set exerciser - Copyright (C) 1994 Frank D. Cringle
; 8080 CPU support - Copyright (C) Ian Bartholomew
; Revision and improvements - Copyright (C) Enrico Croce
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
;
; Run a diagnostic test on an 8080/8085 CPU (emulator)
;
; It uses calls to BDOS (C_WRITE and C_WRITESTR) to report progess and status
; and this calls are assumed intercepted by the emulator (always work).
;
; These exerciser is split in two
; 1. Preliminary check. To test the minimum instructions needed to proceed
; 2. A full test list is executed using
;
; Initially errors are 'reported' by halting.
; Later errors are reported by outputting an address
;
;******************************************************************************
;
; compile with RetroAssembler
; Tab Size = 10
;
	.target	"8080"
	.format	"bin"

	.setting "OmitUnusedFunctions", true

	.include "../lib/libCPM.8080.asm"
	.include "../lib/libText.8080.asm"
	.include "../lib/libUtils.8080.asm"
	.include "../lib/libCRC.8080.asm"

	.include "exerciser-utils.8080.asm"
	.include "exerciser-prelim.8080.asm"
	.include "exerciser-tester.8080.asm"
	.include "exerciser-testlist.8080.asm"

.code
	.org	PROGSTART

; Skip configuration
COMStart	jmp	Main

CTestRun	.byte	$00	; test to run ($00= all)
CBrkOnErr	.byte	$00	; break on errors ($00= no, $FF=yes)

; MUST BE in a well knwon orign segment (otherwise references in testsuite fail)
; machine state before test
msbt	.ds	2	; memop
	;;.ds	4	; iy,ix
	.ds	6	; HL,DE,BC
flgsbt	.ds	1	; F
	.ds	1	; A
spbt	.ds	2	; stack pointer

; machine state after test
msat	.ds	2	; memop
msatSt	;;.ds	4	; iy,ix
	.ds	6	; HL,DE,BC
flgsat	.ds	1	; F
	.ds	1	; A
spat	.ds	2	; stack pointer

StkMrkBT	.equ	msbt+2
StkMrkAT	.equ	msat+STATESIZE-2

; Run Exerciser
Main	lxi	sp, StackEnd
	TextHome()
	TextPrint(WelcomMsg)
Phase1	WriteTrace()
	PreChecks()
	TextPrint(PreOKMsg)
Phase2	WriteTrace()
	TestSuite()
	lda	TestOKs
	ora	A
	jz	@AllOk
	TextPrint(TestKOMsg)
	jmp	@Exit
@AllOk	TextPrint(TestOKMsg)
@Exit	EXIT()

; Report prechecks failure to the console
PreTestKO	TextPrint(PreKOMsg)
	EXIT()

; Report testsuite failure to the console
TestError	TextPrint(ErrorMsg)
	xthl		; Recover Caller Address
	TextPrintHex4()
	TextNL()
	EXIT()

.segment "Resources"
WelcomMsg
	.ascii "8080/8085 instruction exerciser (c) 2021\r\n"
	.ascii "This program is free software under\r\n"
	.ascii "GNU General Public License v2\r\n$"
ErrorMsg	.ascii "Test Error @$"
PreKOMsg	.ascii "Preliminary tests: failed!\r\n$"
PreOKMsg	.ascii "Preliminary tests: OK\r\n$"
TestOKMsg	.ascii "Test suite passed successfully!\r\n$"
TestKOMsg	.ascii "Test suite failed!             \r\n$"

.segment "Stack"
StackStrt	.ds 256, $FF
StackEnd	.word $FFFF
