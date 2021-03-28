;************************************************************
;                8080/8085 CPU TEST/DIAGNOSTIC
;************************************************************
;
; compile with RetroAssembler
; Tab Size = 10

	.target	"8080"
	.format	"prg"

	.setting "OmitUnusedFunctions", true

	.include "../lib/libCPM.8080.asm"
	.include "../lib/libText.8080.asm"
	.include "../lib/libUtils.8080.asm"

.code
	.org	$0100
Main	lxi 	SP, StackEnd
	TextHome()
	TextPrint(WelcomMsg)

; TEST JUMP INSTRUCTIONS AND FLAGS
JUMPTest	TextPrint(JMPTstMsg)
	.include "cpudiag_JMP.8080.asm"
	TextPrint(OKMsg)

; TEST ACCUMULATOR IMMEDIATE INSTRUCTIONS
AIMMTest	TextPrint(AIMTstMsg)
	.include "cpudiag_AIM.8080.asm"
	TextPrint(OKMsg)

; TEST CALLS AND RETURNS
CALLTest	TextPrint(CALTstMsg)
	.include "cpudiag_CAL.8080.asm"
	TextPrint(OKMsg)

; TEST "MOV","INR",AND "DCR" INSTRUCTIONS
MOVITest	TextPrint(MVITstMsg)
	.include "cpudiag_MVI.8080.asm"
	TextPrint(OKMsg)

; TEST ARITHMETIC AND LOGIC INSTRUCTIONS
AritTest	TextPrint(ALUTstMsg)
	.include "cpudiag_ALU.8080.asm"
	TextPrint(OKMsg)

Epilogue	TextPrint(CPUOKMsg)
	EXIT()

; Report Failure to the console
CPUError:	TextPrint(KOMsg)
	TextPrint(ErrorMsg)
	xthl		; Recover Caller Address
	TextPrintHex4()
	TextNL()
	EXIT()

.data
WelcomMsg	.ascii "8080/8085 CPU Diagnostic\r\n$"
JMPTstMsg	.ascii "JMP tests: $"
AIMTstMsg	.ascii "Accumulator and immediates tests: $"
CALTstMsg	.ascii "Call & Rets tests: $"
MVITstMsg	.ascii "MOV, INR and DCR tests: $"
ALUTstMsg	.ascii "Arithmetic and logic tests: $"
CPUOKMsg	.ascii "CPU tests passed!\r\n$"
ErrorMsg	.ascii "ERROR @$"
KOMsg	.ascii "failed!\r\n$"
OKMsg	.ascii "OK\r\n$"

.segment "Stack"
StackStrt	.ds 256, 0	; 256 bytes of stack
StackEnd	.dw $A5A5
