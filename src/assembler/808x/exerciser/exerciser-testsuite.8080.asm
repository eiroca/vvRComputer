;******************************************************************************
;
; prelim.z80 - Preliminary Z80 tests - Copyright (C) 199\  Frank D. Cringle
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
;
; Modified to exercise an 8080 by Ian Bartholomew, February 2009
;
; I have made the following changes -
;
; Converted all mnemonics to 8080 and rewritten any Z80 code used
; in the original exerciser.  Changes are tagged with a #idb in the
; source code listing.
;
; Removed any test descriptors that are not used.
;
; Changed the macro definitions to work in M80
;
; The machine state snapshot has been changed to remove the IX/IY registers.
; They have been replaced by two more copies of HL to obviate the need
; for major changes in the exerciser code.
;
; Changed flag mask in all tests to $FF to reflect that the 8080, unlike the 8085
; and Z80, does define the unused bits in the flag register - [S Z 0 AC 0 P 1 C]
;
;******************************************************************************
; compile with RetroAssembler
; Tab Size = 10
;

; For the purposes of this test program, the machine state consists of:
;	a 2 byte memory operand, followed by
;	the registers iy,ix,hl,de,bc,af,sp
; for a total of STATESIZE bytes.

; The program tests instructions (or groups of similar instructions)
; by cycling through a sequence of machine states, executing the test
; instruction for each one and running a 32-bit crc over the resulting
; machine states.  At the end of the sequence the crc is compared to
; an expected value that was found empirically on a real Z80.

; A test case is defined by a descriptor which consists of:
;	a flag mask byte,
;	the base case,
;	the incement vector,
;	the shift vector,
;	the expected crc,
;	a short descriptive message.
;
; The flag mask byte is used to prevent undefined flag bits from
; influencing the results.  Documented flags are as per Mostek Z80
; Technical Manual.
;
; The next three parts of the descriptor are 20 byte vectors
; corresponding to a IUT_SIZE byte instruction and a STATESIZE byte machine state.
; The first part is the base case, which is the first test case of
; the sequence.  This base is then modified according to the next 2
; vectors.  Each 1 bit in the increment vector specifies a bit to be
; cycled in the form of a binary counter.  For instance, if the byte
; corresponding to the accumulator is set to $FF in the increment
; vector, the test will be repeated for all 256 values of the
; accumulator.  Note that 1 bits don't have to be contiguous.  The
; number of test cases 'caused' by the increment vector is equal to
; 2^(number of 1 bits).  The shift vector is similar, but specifies a
; set of bits in the test case that are to be successively inverted.
; Thus the shift vector 'causes' a number of test cases equal to the
; number of 1 bits in it.

; The total number of test cases is the product of those caused by the
; counter and shift vectors and can easily become unweildy.  Each
; individual test case can take a few milliseconds to execute, due to
; the overhead of test setup and crc calculation, so test design is a
; compromise between coverage and execution time.

; This program is designed to detect differences between
; implementations and is not ideal for diagnosing the causes of any
; discrepancies.  However, provided a reference implementation (or
; real system) is available, a failing test case can be isolated by
; hand using a binary search of the test space.

.segment "Resources"
okmsg	.ascii	" OK \r\n$"
ermsg1	.ascii	" KO - CRC:$"
ermsg2	.ascii	" found:$"
crlf	.ascii	"\r\n$"

.segment "Constants"
IUT_SIZE	.equ	4
STATESIZE	.equ	16
TESTSIZE	.equ	IUT_SIZE+STATESIZE
MASKSIZE	.equ	20

OP_HLT	.equ	$76

.data

TestOKs	.ds	1	; Results of the test(s) ($00=OK)

curTest	.ds	2

PTestIUT	.ds	2
PTestSta	.ds	2
PIncMask	.ds	2
PShtMask	.ds	2

counter	.ds	MASKSIZE, $00
 	.ds	MASKSIZE, $00
cntbyt	.ds	2
cntbit	.ds	1

shifter	.ds	MASKSIZE, $00
	.ds	MASKSIZE, $00
shfbyt	.ds	2
shfbit	.ds	1


; machine state after test
msat	.ds	2	; memop
msatSt	.ds	4	; iy,ix
	.ds	6	; HL,DE,BC
flgsat	.ds	1	; F
	.ds	1	; A
StkMrkAT	.equ	*	;
spat	.ds	2	; stack pointer

hlsav	.ds	2
spsav	.ds	2	; saved stack pointer

.lib
.function TestSuite()
; Init TestSuite
	DebugTrace(0)
	xra	A
	sta	TestOKs
	lda	CTestRun
	cpi	$00
	jz	@TestAll
; Single Test in the testsuite (index 1..)
	mov	E,A
	mvi	D,$00
	lxi	H, TestList-2
	dad	D
	dad	D
	_HL_ind()			; HL <- (TestList + 2*D - 2)
	RunTest()
	jmp	@TestsDone
; Test all the testsuite
@TestAll	lxi	H, TestList-2
	shld	curTest
@TestLoop	lhld	curTest
	inx	H
	inx	H
	DebugTrace(5)
	shld	curTest
	_HL_ind()			; curTest++; HL<-(curTest)
	mov	A, H
	ora	L
	jz	@TestsDone
	RunTest()
	lda	CBrkOnErr
	ora	A
	jz	@TestLoop
	lda	TestOKs
	ora	A
	lxi	H,TestOKs
	mov	B,M
	mvi	C,0
	DebugTrace(5)
	jz	@TestLoop
@TestsDone
.endfunction

; start test pointed to by (hl)
.function RunTest()
TestStat	DebugTrace(5)

	push	h
	shld	PTestIUT		; + 00
	lxi	d, IUT_SIZE
	dad	d
	shld	PTestSta		; + 04
	lxi	d, STATESIZE
	dad	d
	shld	PIncMask		; +20
	lxi	d, TESTSIZE
	dad	d
	shld	PShtMask		; +40

	_HL_ptr(PTestIUT, TESTSIZE*3+4)	; pointer to test description
	xchg
	C_WRITESTR_D()				; show test name

	; copy initial instruction under test
	lhld	PTestIUT
	lxi	d,iut
	lxi	b,IUT_SIZE
	_ldir()

	; copy initial machine state
	lhld	PTestSta
	lxi	d,msbt
	lxi	b,STATESIZE
	_ldir()

	lhld	PIncMask		; point to incmask
	lxi	d,counter
	initmask()

	lhld	PShtMask		; point to scanmask
	lxi	d,shifter
	initmask()
	lxi	h,shifter
	mvi	m,1		; first bit

	DebugTrace(3)
	DebugTrace(4)

	initcrc()		; initialise crc

; test loop
TestLoop	lda	IUT
	cpi	OP_HLT		; pragmatically avoid halt intructions
	jz	TestDone
	ani	$DF		; skip illegal instructions
	cpi	$DD
	jz	TestDone

; execute the test instruction
test	push	psw
	push	b
	push	d
	push	h
	DebugTrace(1)
	di			; disable interrupts
	_HLSP()		; save stack pointer
	shld	spsav
	lxi	sp,StkMrkBT	; point to test-case machine state
	pop	h		; dummy ix
	pop	h		; dummy iy
	pop	h
	pop	d
	pop	b
	pop	psw
	shld	hlsav
	lhld	spbt
	sphl
	lhld	hlsav
IUT	.ds	IUT_SIZE, $00	; max IUT_SIZE byte instruction under test
	shld	hlsav
	lxi	h,0
	jc	@temp1		;jump on the state of the C flag set in the test
	dad	sp		;this code will clear the C flag (0 + nnnn = nc)
	jmp	@temp2		;C flag is same state as before
@temp1	dad	sp		;this code will clear the C flag (0 + nnnn = nc)
	stc			;C flage needs re-setting to preserve state
@temp2	shld	spat
	lhld	hlsav
	lxi	sp,StkMrkAT
	push	psw		; save other registers
	push	b
	push	d
	push	h
	push	h		; dummy iy
	push	h		; dummy ix
	lhld	spsav		; restore stack pointer
	sphl
	ei			; enable interrupts
	lhld	msbt		; copy memory operand
	shld	msat
	lxi	H, flgsat		; flags after test
	lda       CFlgMskAn
	mov	D, A
	lda       CFlgMskOr
	mov	E, A
	mov	A, M
	ana	D
	ora	E
	mov	M, A
; update CRC
	mvi	B, STATESIZE		; total of STATESIZE bytes of state
	lxi	D, msat
	lxi	H, crcval
@crcLoop	ldax	d
	inx	d
	updcrc()			; accumulate crc of this test case
	dcr	b
	jnz	@crcLoop
	DebugTrace(2)
	pop	h
	pop	d
	pop	b
	pop	psw

; Update test status
TestDone	NextCounter()			; increment the counter
	jz	@SkipShft		; shift the scan bit
	NextShifter()
@SkipShft	pop	h		; pointer to test case
	jnz	TestExit		; done if shift returned NZ
	push	h
	mvi	a,1		; initialise count and shift scanners
	sta	cntbit
	sta	shfbit
	lxi	h,counter
	shld	cntbyt
	lxi	h,shifter
	shld	shfbyt
	; setup iut
	lhld	PTestIUT		; pointer to test case IUT
	lxi	d,iut
	mvi	b,IUT_SIZE	; bytes in iut field
	setup()
	; setup machine state
	lhld	PTestSta		; pointer to test case STATE
	lxi	d,msbt
	mvi	b,STATESIZE	; bytes in machine state
	setup()
	jmp	TestLoop

; Test Finalization
TestExit	_HL_ptr(PTestIUT, TESTSIZE*3)	; point to expected crc
	cmpcrc()
	lxi	d,okmsg
	jz	@tlpok
	mvi 	A, 1		; Mark test failed
	sta	TestOKs
	lxi	d,ermsg1
	push	H
	C_WRITESTR_D()
	pop	H
	phex8()
	lxi	d,ermsg2
	C_WRITESTR_D()
	lxi	h,crcval
	phex8()
	lxi	d,crlf
@tlpok	C_WRITESTR_D()
.endfunction

; initialise counter or shifter
; de = pointer to work area for counter or shifter
; hl = pointer to mask
.function initmask()
	DebugTrace(0)
	push	d
	xchg
	lxi	b,MASKSIZE*2
	clrmem()		; clear work area
	xchg
	mvi	b,TESTSIZE	; byte counter
	mvi	c,1		; first bit
	mvi	d,0		; bit counter
; Count "1" bit in mask
@imlp	mov	e,m
@imlp1	mov	a,e
	ana	c
	jz	@Loop2
	inr	d
@Loop2	mov	a,c
	rlc
	mov	c,a
	cpi	$01
	jnz	@imlp1
	inx	h
	dcr	b
	jnz	@imlp
; got number of 1-bits in mask in reg d
	mov	a,d
	ani	$F8
	rrc
	rrc
	rrc			; divide by 8 (get byte offset)
	mov	l,a
	mvi	h,0
	mov	a,d
	ani	$07		; bit offset
	inr	a
	mov	b,a
	mvi	a,$80
;
@imlp3	rlc
	dcr	b
	jnz	@imlp3		; A <- 2 ^ (count(1 in mask) % 8)
	pop	d
	dad	d
	lxi	d,MASKSIZE
	dad	d		; HL = @Shifter + count(1 in mask) / 8 + MASKSIZE
	mov	m,a
.endfunction

; multi-byte counter
.function NextCounter()
	DebugTrace(0)
	push	b
	push	d
	push	h
	lxi	h,counter		; 20 byte counter starts here
	lxi	d,MASKSIZE	; somewhere in here is the stop bit
	xchg
	dad	d
	xchg
@Loop	inr	m
	mov	a,m
	cpi	$00
	jnz	@break	; overflow to next byte
	inx	h
	inx	d
	jmp	@Loop
@break	mov	b,a
	ldax	d
	ana	b		; test for terminal value
	jz	@cntend
	mvi	m,0		; reset to zero
@cntend	pop	b
	pop	d
	pop	h
	DebugTrace(3)
.endfunction

; multi-byte shifter
.function NextShifter()
	DebugTrace(4)
	push	b
	push	d
	push	h
	lxi	h,shifter	; 20 byte shift register starts here
	lxi	d,shifter+MASKSIZE
@shflp	mov	a,m
	ora	a
	jnz	@shflp1
	inx	h
	inx	d
	jmp	@shflp
@shflp1	mov	b,a
	ldax	d
	ana	b
	jnz	@shlpe
	mov	a,b
	rlc
	cpi	1
	jnz	@shflp2
	mvi	m,0
	inx	h
	inx	d
@shflp2	mov	m,a
	xra	a		; set Z
@shlpe	pop	h
	pop	d
	pop	b
.endfunction

; setup a field of the test case
; B  = number of bytes
; HL = pointer to base case
; DE = destination
.function setup()
	DebugTrace(1)
@Loop
	push	b
	push	d
	push	h
	mov	c,m		; get base byte
	lxi	d,TESTSIZE
	dad	d		; point to incmask
; Process IncMask
	mov	a,m
	cpi	0
	jz	@subshf
	mvi	b,8		; 8 bits
@subclp	rrc
	push	psw
	mvi	a,0
	jnc	@Change1	; get next counter bit if mask bit was set
	nxtcbit()
@Change1	xra	c		; flip bit if counter bit was set
	rrc
	mov	c,a
	pop	psw
	dcr	b
	jnz	@subclp
	mvi	b,8
; Process ShiftMask
@subshf	lxi	d,TESTSIZE
	dad	d		; point to shift mask
	mov	a,m
	cpi	0
	jz	@substr
	mvi	b,8		; 8 bits
@sbshf1	rrc
	push	psw
	mvi	a,0
	jnc	@Change2	; get next shifter bit if mask bit was set
	nxtsbit()
@Change2	xra	c		; flip bit if shifter bit was set
	rrc
	mov	c,a
	pop	psw
	dcr	b
	jnz	@sbshf1
;
@substr	pop	h
	pop	d
	mov	a,c
	stax	d		; mangled byte to destination
	inx	d
	inx	h
	pop	b
	dcr	b
	jnz	@Loop
.endfunction

; get next counter bit in low bit of a
.function nxtcbit()
	DebugTrace(0)
 	push	b
	push	h
	lhld	cntbyt
	mov	b,m		; B<- (cntbyt)
	lxi	h,cntbit
	mov	a,m
	mov	c,a		; C<- cntbit
	rlc
	mov	m,a
	cpi	1
	jnz	@ncb1
	lhld	cntbyt
	inx	h
	shld	cntbyt
@ncb1	mov	a,b
	ana	c
	pop	h
	pop	b
	rz
	mvi	a,1
.endfunction

; get next shifter bit in low bit of a
.function nxtsbit()
	DebugTrace(0)
 	push	b
	push	h
	lhld	shfbyt
	mov	b,m
	lxi	h,shfbit
	mov	a,m
	mov	c,a
	rlc
	mov	m,a
	cpi	1
	jnz	@nsb1
	lhld	shfbyt
	inx	h
	shld	shfbyt
@nsb1	mov	a,b
	ana	c
	pop	h
	pop	b
	rz
	mvi	a,1
.endfunction
