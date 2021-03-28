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

.data
.macro testdef(insn1,insn2,insn3,insn4,memop,_iy,_ix,_hl,_de,_bc,_flags,_acc,_sp)
	.byte	insn1, insn2, insn3, insn4
	.word	memop
	.word	_iy
	.word	_ix
	.word	_hl
	.word	_de
	.word	_bc
	.byte	_flags
	.byte	_acc
	.word	_sp
.endmacro

test_adi	testdef($C6,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ADI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$CD,$02,$03,$CE						; expected crc
	.ascii	"ADI nn ................... $"

test_sui	testdef($D6,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; SUI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$E5,$DA,$50,$07						; expected crc
	.ascii	"SUI nn ................... $"

test_ani	testdef($E6,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ANI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$83,$83,$F6,$45						; expected crc
	.ascii	"ANI nn ................... $"

test_ori	testdef($F6,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ORI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$B2,$20,$30,$B1						; expected crc
	.ascii	"ORI nn ................... $"

test_aci	testdef($CE,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ACI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$6F,$26,$39,$8B						; expected crc
	.ascii	"ACI nn ................... $"

test_sbi	testdef($DE,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; SBI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$2C,$47,$E3,$3B						; expected crc
	.ascii	"SBI nn ................... $"

test_xri	testdef($EE,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; XRI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$BC,$EA,$13,$69						; expected crc
	.ascii	"XRI nn ................... $"

test_cpi	testdef($FE,$00,$00,$00, $9140,$7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; CPI
	testdef($00,$00,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	testdef($00,$FF,$00,$00, $0000,$0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	.byte	$B9,$22,$49,$6F						; expected crc
	.ascii	"CPI nn ................... $"


; add hl,<bc,de,hl,sp> (19,456 cycles)
; dad <B,D,H,SP> (19,456 cycles)
tst_add16	testdef($09,$00,$00,$00, $c4a5,$c4c7,$d226,$a050,$58ea,$8566,$c6,$de,$9bc9)
	testdef($30,$00,$00,$00,   0, 0, 0,$f821,  0,  0,  0, 0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0, 0, 0,   -1, -1, -1,$D7, 0, -1)	; (38 cycles)
	.byte	$14, $47, $4b, $a6					; expected crc
	.ascii	"dad <b,d,h,sp> ........... $"

; aluop a,nn (28,672 cycles)
; aluop nn (28,672 cycles)
tst_alu8i testdef($C6,$00,$00,$00, $9140,$7e3c,$7a67,$df6d,$5b61,$0b29,$10,$66,$85b2)
	testdef($38,$00,$00,$00,   0, 0, 0, 0, 0, 0,  0, -1, 0)		; (2048 cycles)
	testdef($00, -1,$00,$00,   0, 0, 0, 0, 0, 0,$D7,  0, 0)		; (14 cycles)
	.byte	$9e, $92, $2f, $9e					; expected crc
	.ascii	"aluop nn ................. $"

; aluop a,<b,c,d,e,h,l,(hl),a> (753,664 cycles)
; aluop <b,c,d,e,h,l,(hl),a> (753,664 cycles)
tst_alu8r	testdef($80,$00,$00,$00, $c53e,$573a,$4c4d,msbt,$e309,$a666,$d0,$3b,$adbb)
	testdef($3f,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0, -1,  0)	; (16,384 cycles)
	testdef($00,$00,$00,$00, $FF,  0,  0,  0, -1, -1,$D7,  0,  0)	; (46 cycles)
	.byte	$cf, $76, $2c, $86					; expected crc
	.ascii	"aluop <b,c,d,e,h,l,m,a> .. $"

tst_daa	testdef($27,$00,$00,$00, $2141,$09fa,$1d60,$a559,$8d5b,$9079,$04,$8e,$299d)
; <daa,cpl,scf,ccf>
; <DAA,CMA,STC,CMC>
	testdef($18,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7, -1,  0)	; (65,536 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (1 cycle)
	.byte	$bb,$3f,$03,$0c					; expected crc
	.ascii	"<daa,cma,stc,cmc> ........ $"

; <inc,dec> a (3072 cycles)
tst_inca	testdef($3C,$00,$00,$00, $4adf,$d5d8,$e598,$8a2b,$a7b0,$431b,$44,$5a,$d030)
	testdef($01,$00,$00,$00,    0,  0,  0,  0,  0,  0,  0, -1,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,    0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$ad,$b6,$46,$0e					; expected crc
	.ascii	"<inr,dcr> a .............. $"

; <inc,dec> b (3072 cycles)
tst_incb	testdef($04,0,0,0,$d623,$432d,$7a61,$8180,$5a86,$1e85,$86,$58,$9bbb)
	testdef($01,$00,$00,$00,   0,  0,  0,  0,  0,$ff00,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,    0,$D7,  0,  0)	; (6 cycles)
	.byte	$83,$ed,$13,$45					; expected crc
	.ascii	"<inr,dcr> b .............. $"

; <inc,dec> bc (1536 cycles)
tst_incbc	testdef($03,$00,$00,$00, $cd97,$44ab,$8dc9,$e3e3,$11cc,$e8a4,$02,$49,$2a4d)
	testdef($08,$00,$00,$00,   0,  0,  0,  0,  0,$f821,  0,  0,  0)	; (256 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,    0,$D7,  0,  0)	; (6 cycles)
	.byte	$f7,$92,$87,$cd					; expected crc
	.ascii	"<inx,dcx> b .............. $"

; <inc,dec> c (3072 cycles)
tst_incc	testdef($0C,$00,$00,$00, $d789,$0935,$055b,$9f85,$8b27,$d208,$95,$05,$0660)
	testdef($01,$00,$00,$00,   0,  0,  0,  0,  0,$FF,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$e5,$f6,$72,$1b					; expected crc
	.ascii	"<inr,dcr> c .............. $"

; <inc,dec> d (3072 cycles)
tst_incd	testdef($14,$00,$00,$00, $a0ea,$5fba,$65fb,$981c,$38cc,$debc,$43,$5c,$03bd)
	testdef($01,$00,$00,$00,   0,  0,  0,  0,$ff00,  0,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,    0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$15,$b5,$57,$9a					; expected crc
	.ascii	"<inr,dcr> d .............. $"

; <inc,dec> de (1536 cycles)
tst_incde	testdef($13,$00,$00,$00, $342e,$131d,$28c9,$0aca,$9967,$3a2e,$92,$f6,$9d54)
	testdef($08,$00,$00,$00,   0,  0,  0,  0,$f821,  0,  0,  0,  0)	; (256 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,    0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$7f,$4e,$25,$01			; expected crc
	.ascii	"<inx,dcx> d .............. $"

; <inc,dec> e (3072 cycles)
tst_ince	testdef($1C,$00,$00,$00, $602f,$4c0d,$2402,$e2f5,$a0f4,$a10a,$13,$32,$5925)
	testdef($01,$00,$00,$00,   0,  0,  0,  0,$FF,  0,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$cf,$2a,$b3,$96					; expected crc
	.ascii	"<inr,dcr> e .............. $"

; <inc,dec> h (3072 cycles)
tst_inch	testdef($24,$00,$00,$00, $1506,$f2eb,$e8dd,$262b,$11a6,$bc1a,$17,$06,$2818)
	testdef($01,$00,$00,$00,   0,  0,  0,$ff00,  0,  0,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,    0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$12,$b2,$95,$2c					; expected crc
	.ascii	"<inr,dcr> h .............. $"

; <inc,dec> hl (1536 cycles)
tst_inchl	testdef($23,$00,$00,$00, $c3f4,$07a5,$1b6d,$4f04,$e2c2,$822a,$57,$e0,$c3e1)
	testdef($08,$00,$00,$00,   0,  0,  0,$f821,  0,  0,  0,  0,  0)	; (256 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,    0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$9f,$2b,$23,$c0					; expected crc
	.ascii	"<inx,dcx> h .............. $"

; <inc,dec> l (3072 cycles)
tst_incl	testdef($2C,$00,$00,$00, $8031,$a520,$4356,$b409,$f4c1,$dfa2,$d1,$3c,$3ea2)
	testdef($01,$00,$00,$00,   0,  0,  0,$FF,  0,  0,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$ff,$57,$d3,$56					; expected crc
	.ascii	"<inr,dcr> l .............. $"

; <inc,dec> (hl) (3072 cycles)
tst_incm	testdef($34,$00,$00,$00, $b856,$0c7c,$e53e,msbt,$877e,$da58,$15,$5c,$1f37)
	testdef($01,$00,$00,$00, $FF,  0,  0,  0,  0,  0,  0,  0,  0)	; (512 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$92,$e9,$63,$bd					; expected crc
	.ascii	"<inr,dcr> m .............. $"

; <inc,dec> sp (1536 cycles)
tst_incsp	testdef($33,$00,$00,$00, $346f,$d482,$d169,$deb6,$a494,$f476,$53,$02,$855b)
	testdef($08,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,$f821)	; (256 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$d5,$70,$2f,$ab					; expected crc
	.ascii	"<inx,dcx> sp ............. $"

; ld hl,(nnnn) (16 cycles)
tst_ld162	testdef($2a,<msbt,>msbt,0,$9863,$7830,$2077,$b1fe,$b9fa,$abb8,$04,$06,$6015)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (1 cycle)
	testdef($00,$00,$00,$00,  -1,  0,  0,  0,  0,  0,  0,  0,  0)	; (16 cycles)
	.byte	$a9,$c3,$d5,$cb					; expected crc
	.ascii	"lhld nnnn ................ $"

; ld (nnnn),hl (16 cycles)
tst_ld166	testdef($22,<msbt,>msbt,$00, $d003,$7772,$7f53,$3f72,$64ea,$e180,$10,$2d,$35e9)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (1 cycle)
	testdef($00,$00,$00,$00,   0,  0,  0, -1,  0,  0,  0,  0,  0)	; (16 cycles)
	.byte	$e8,$86,$4f,$26					; expected crc
	.ascii	"shld nnnn ................ $"

; ld <bc,de,hl,sp>,nnnn (64 cycles)
tst_ld16i	testdef($01,$00,$00,$00, $5c1c,$2d46,$8eb9,$6078,$74b1,$b30e,$46,$d1,$30cc)
	testdef($30,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (4 cycles)
	testdef($00,$FF,$FF,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (16 cycles)
	.byte	$fc,$f4,$6e,$12					; expected crc
	.ascii	"lxi <b,d,h,sp>,nnnn ...... $"

; ld a,<(bc),(de)> (44 cycles)
tst_ld8bd	testdef($0A,$00,$00,$00, $b3a8,$1d2a,$7f8e,$42ac,msbt,msbt,$c6,$b1,$ef8e)
	testdef($10,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (2 cycles)
	testdef($00,$00,$00,$00, $FF,  0,  0,  0,  0,  0,$D7, -1,  0)	; (22 cycles)
	.byte	$2b,$82,$1d,$5f					; expected crc
	.ascii	"ldax <b,d> ............... $"

; ld <b,c,d,e,h,l,(hl),a>,nn (64 cycles)
tst_ld8im	testdef($06,$00,$00,$00, $c407,$f49d,$d13d,$0339,$de89,$7455,$53,$c0,$5509)
	testdef($38,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (8 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0, -1,  0)	; (8 cycles)
	.byte	$ea,$a7,$20,$44					; expected crc
	.ascii	"mvi <b,c,d,e,h,l,m,a>,nn . $"

; ld <b,c,d,e,h,l,a>,<b,c,d,e,h,l,a> (3456 cycles)
tst_ld8rr	testdef($40,$00,$00,$00, $72a4,$a024,$61ac,msbt,$82c7,$718f,$97,$8f,$ef8e)
	testdef($3f,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (64 cycles)
	testdef($00,$00,$00,$00, $FF,  0,  0,  0, -1, -1,$D7, -1,  0)	; (54 cycles)
	.byte	$10,$b5,$8c,$ee					; expected crc
	.ascii	"mov <bcdehla>,<bcdehla> .. $"

; ld a,(nnnn) / ld (nnnn),a (44 cycles)
tst_lda	testdef($32,<msbt,>msbt,$00, $fd68,$f4ec,$44a0,$b543,$0653,$cdba,$d2,$4f,$1fd8)
	testdef($08,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (2 cycle)
	testdef($00,$00,$00,$00, $FF,  0,  0,  0,  0,  0,$D7, -1,  0)	; (22 cycles)
	.byte	$ed,$57,$af,$72					; expected crc
	.ascii	"sta nnnn / lda nnnn ...... $"

; <rlca,rrca,rla,rra> (6144 cycles)
tst_rot80 testdef($07,$00,$00,$00, $cb92,$6d43,$0a90,$c284,$0c53,$f50e,$91,$eb,$40fc)
	testdef($18,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0, -1,  0)	; (1024 cycles)
	testdef($00,$00,$00,$00,   0,  0,  0,  0,  0,  0,$D7,  0,  0)	; (6 cycles)
	.byte	$e0,$d8,$92,$35					; expected crc
	.ascii	"<rlc,rrc,ral,rar> ........ $"

; ld (<bc,de>),a (96 cycles)
tst_stabd	testdef($02,$00,$00,$00, $0c3b,$b592,$6cff,$959e,msbt,msbt+1,$c1,$21,$bde7)
	testdef($18,$00,$00,$00,   0,  0,  0,  0,  0,  0,  0,  0,  0)	; (4 cycles)
	testdef($00,$00,$00,$00,  -1,  0,  0,  0,  0,  0,  0, -1,  0)	; (24 cycles)
	.byte	$2b,$04,$71,$e9					; expected crc
	.ascii	"stax <b,d> ............... $"


TestList
.word	test_adi		; 2
.word	test_sui		; 2
.word	test_ani		; 2
.word	test_ori		; 2
.word	test_aci		; 2
.word	test_sbi		; 2
.word	test_xri		; 2
.word	test_cpi		; 2

	.word	tst_alu8i		; 2
	.word	tst_alu8r		; 3
	.word	tst_daa		; 4
	.word	tst_add16		; 1
	.word	tst_inca		; 5
	.word	tst_incbc		; 6
	.word	tst_incb		; 7
	.word	tst_incc		; 8
	.word	tst_incd		; 9
	.word	tst_incde		; 10
	.word	tst_ince		; 11
	.word	tst_inch		; 12
	.word	tst_inchl		; 13
	.word	tst_incl		; 14
	.word	tst_incm		; 15
	.word	tst_incsp		; 16
	.word	tst_ld162		; 17
	.word	tst_ld166		; 18
	.word	tst_ld16i		; 19
	.word	tst_ld8bd		; 20
	.word	tst_ld8im		; 21
	.word	tst_ld8rr		; 22
	.word	tst_lda		; 23
	.word	tst_rot80		; 24
	.word	tst_stabd		; 25
	.word	0

.code
