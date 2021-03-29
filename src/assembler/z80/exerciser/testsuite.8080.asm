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

.segment "Constants"

TestList
	.word	test_add
	.word	test_adi
	.word	test_sub
	.word	test_sui
	.if TESTMODE == 8080
	.word	tt80_ana
	.word	tt80_ani
	.endif
	.if TESTMODE == 8085
	.word	tt85_ana
	.word	tt85_ani
	.endif
	.word	test_ora
	.word	test_ori
	.word	test_adc
	.word	test_aci
	.word	test_sbb
	.word	test_sbi
	.word	test_xra
	.word	test_xri
	.word	test_cmp
	.word	test_cpi
	.word	test_daa
	.word	test_rot
	.word	test_dad
	.word	test_inca
	.word	test_incb
	.word	test_incc
	.word	test_incd
	.word	test_ince
	.word	test_inch
	.word	test_incl
	.word	test_incm
	.word	test_inxb
	.word	test_inxd
	.word	test_inxh
	.word	test_inxs
	.word	test_ldhl
	.word	test_sthl
	.word	test_lxi
	.word	test_mov
	.word	test_mvi
	.word	test_ldax
	.word	test_stax
	.word	test_ldst
	.word	0

; 8080/8085 tests
test_add	deftst($80,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; ADD
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$27,$F3,$A0,$67						; expected crc
	.ascii	"add <b,c,d,e,h,l,m,a> .... $"

test_adi	deftst($C6,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ADI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$B8,$6D,$F6,$87						; expected crc
	.ascii	"adi nn ................... $"

test_sub	deftst($90,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; SUB
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$B7,$6F,$DD,$D1						; expected crc
	.ascii	"sub <b,c,d,e,h,l,m,a> .... $"

test_sui	deftst($D6,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; SUI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$90,$B5,$A5,$4E						; expected crc
	.ascii	"sui nn ................... $"

test_ora	deftst($B0,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; ORA
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$1B,$63,$BD,$0A						; expected crc
	.ascii	"ora <b,c,d,e,h,l,m,a> .... $"

test_ori	deftst($F6,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ORI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$C7,$4F,$C5,$F8						; expected crc
	.ascii	"ori nn ................... $"

test_adc	deftst($88,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; ADC
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$70,$E0,$94,$88						; expected crc
	.ascii	"adc <b,c,d,e,h,l,m,a> .... $"

test_aci	deftst($CE,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ACI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$1A,$49,$CC,$C2						; expected crc
	.ascii	"aci nn ................... $"

test_sbb	deftst($98,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; SBB
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$15,$4C,$BC,$50						; expected crc
	.ascii	"sbb <b,c,d,e,h,l,m,a> .... $"

test_sbi	deftst($DE,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; SBI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$59,$28,$16,$72						; expected crc
	.ascii	"sbi nn ................... $"

test_xra	deftst($A8,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; XRA
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$84,$65,$A9,$8B						; expected crc
	.ascii	"xra <b,c,d,e,h,l,m,a> .... $"

test_xri	deftst($EE,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; XRI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$C9,$85,$E6,$20						; expected crc
	.ascii	"xri nn ................... $"

test_cmp	deftst($B8,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; CMP
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$05,$A9,$C9,$0E						; expected crc
	.ascii	"cmp <b,c,d,e,h,l,m,a> .... $"

test_cpi	deftst($FE,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; CPI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$CC,$4D,$BC,$26						; expected crc
	.ascii	"cpi nn ................... $"

test_daa	deftst($27,$00,$00,$00, $2141, $09fa,$1d60,$a559,$8d5b,$9079,$04,$8e,$299d)	; DAA,CMA,STC,CMC
	deftst($18,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$FF,$0000)	; x 65,536 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 1 cycle
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$97,$43,$B4,$90						; expected crc
	.ascii	"<daa,cma,stc,cmc> ........ $"

test_rot deftst($07,$00,$00,$00, $cb92, $6d43,$0a90,$c284,$0c53,$f50e,$91,$eb,$40fc)	; RLC,RRC,RAL,RAR
	deftst($18,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 1024 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$F0,$26,$9B,$36						; expected crc
	.ascii	"<rlc,rrc,ral,rar> ........ $"

test_dad	deftst($09,$00,$00,$00, $C4A5, $C4C7,$D226,$A050,$58EA,$8566,$C6,$DE,$9BC9)	; DAD
	deftst($30,$00,$00,$00, $0000, $0000,$0000,$F821,$0000,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$FFFF,$FFFF,$FFFF,$D7,$00,$FFFF)	; x 38 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$D8,$6E,$58,$E4						; expected crc
	.ascii	"dad <b,d,h,sp> ........... $"

test_inca	deftst($3C,$00,$00,$00, $4adf, $d5d8,$e598,$8a2b,$a7b0,$431b,$44,$5a,$d030)	; INR,DCR A
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$99,$65,$34,$8C						; expected crc
	.ascii	"<inr,dcr> a .............. $"

test_incb	deftst($04,$00,$00,$00, $d623, $432d,$7a61,$8180,$5a86,$1e85,$86,$58,$9bbb)	; INR,DCR B
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$ff00,$00,$00,$0000)	; x 512 cycle
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$E1,$83,$98,$DD						; expected crc
	.ascii	"<inr,dcr> b .............. $"

test_incc	deftst($0C,$00,$00,$00, $d789, $0935,$055b,$9f85,$8b27,$d208,$95,$05,$0660)	; INR,DCR C
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$00FF,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$24,$23,$3F,$1B						; expected crc
	.ascii	"<inr,dcr> c .............. $"

test_incd	deftst($14,$00,$00,$00, $a0ea, $5fba,$65fb,$981c,$38cc,$debc,$43,$5c,$03bd)	; INR,DCR D
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$0000,$ff00,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$B7,$32,$8F,$56						; expected crc
	.ascii	"<inr,dcr> d .............. $"

test_ince	deftst($1C,$00,$00,$00, $602f, $4c0d,$2402,$e2f5,$a0f4,$a10a,$13,$32,$5925)	; INR,DCR E
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$0000,$00FF,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	 $BF,$64,$DB,$7E						; expected crc
	.ascii	"<inr,dcr> e .............. $"

test_inch	deftst($24,$00,$00,$00, $1506, $f2eb,$e8dd,$262b,$11a6,$bc1a,$17,$06,$2818)	; INR,DCR H
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$FF00,$0000,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$D2,$82,$76,$CA						; expected crc
	.ascii	"<inr,dcr> h .............. $"

test_incl	deftst($2C,$00,$00,$00, $8031, $a520,$4356,$b409,$f4c1,$dfa2,$d1,$3c,$3ea2)	; INR,DCR L
	deftst($01,$00,$00,$00, $0000, $0000,$0000,$00FF,$0000,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$48,$12,$39,$0E						; expected crc
	.ascii	"<inr,dcr> l .............. $"

test_incm	deftst($34,$00,$00,$00, $b856, $0c7c,$e53e, msbt,$877e,$da58,$15,$5c,$1f37)	; INR,DCR M
	deftst($01,$00,$00,$00, $00FF, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 512 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$07,$CF,$F5,$0F						; expected crc
	.ascii	"<inr,dcr> m .............. $"

test_inxb	deftst($03,$00,$00,$00, $cd97, $44ab,$8dc9,$e3e3,$11cc,$e8a4,$02,$49,$2a4d)	; INX,DCX B
	deftst($08,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$f821,$00,$00,$0000)	; x 256 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$CE,$3F,$A7,$5D						; expected crc
	.ascii	"<inx,dcx> b .............. $"

test_inxd	deftst($13,$00,$00,$00, $342e, $131d,$28c9,$0aca,$9967,$3a2e,$92,$f6,$9d54)	; INX,DCX D
	deftst($08,$00,$00,$00, $0000, $0000,$0000,$0000,$f821,$0000,$00,$00,$0000)	; x 256 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$30,$A8,$77,$B0						; expected crc
	.ascii	"<inx,dcx> d .............. $"

test_inxh	deftst($23,$00,$00,$00, $c3f4, $07a5,$1b6d,$4f04,$e2c2,$822a,$57,$e0,$c3e1)	; INX,DCX H
	deftst($08,$00,$00,$00, $0000, $0000,$0000,$f821,$0000,$0000,$00,$00,$0000)	; x 256 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$ED,$03,$FC,$7E						; expected crc
	.ascii	"<inx,dcx> h .............. $"

test_inxs	deftst($33,$00,$00,$00, $346f, $d482,$d169,$deb6,$a494,$f476,$53,$02,$855b)	; INX,DCX SP
	deftst($08,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$f821)	; x 256 cycles
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 6 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$37,$22,$AA,$2A						; expected crc
	.ascii	"<inx,dcx> sp ............. $"

test_ldhl	deftst($2a,<msbt,>msbt,$00, $9863, $7830,$2077,$b1fe,$b9fa,$abb8,$04,$06,$6015)	; LHLD
	deftst($00,$00,$00,$00,     $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; (1 cycle)
	deftst($00,$00,$00,$00,     $FFFF, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; (16 cycles)
	deftst($FF,$00,$00,$FF,     $FFFF, $FFFF,$FFFF,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$6D,$1E,$EB,$35						; expected crc
	.ascii	"lhld nnnn ................ $"

test_sthl	deftst($22,<msbt,>msbt,$00, $d003, $7772,$7f53,$3f72,$64ea,$e180,$10,$2d,$35e9)	; SHLD
	deftst($00,$00,$00,$00,     $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; (1 cycle)
	deftst($00,$00,$00,$00,     $0000, $0000,$0000,$FFFF,$0000,$0000,$00,$00,$0000)	; (16 cycles)
	deftst($FF,$00,$00,$FF,     $FFFF, $FFFF,$FFFF,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$2C,$5B,$71,$D8						; expected crc
	.ascii	"shld nnnn ................ $"

test_lxi	deftst($01,$AA,$55,$00, $5c1c, $2d46,$8eb9,$6078,$74b1,$b30e,$46,$d1,$30cc)	; LXI
	deftst($30,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 4 cycles
	deftst($00,$FF,$FF,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 16 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$FC,$49,$49,$8B						; expected crc
	.ascii	"lxi <b,d,h,sp>,nnnn ...... $"

test_mov	deftst($40,$00,$00,$00, $72a4, $a024,$61ac, msbt,$82c7,$718f,$97,$8f,$ef8e)	; MOV
	deftst($3f,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 64 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$FF,$0000)	; x 54 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$76,$4E,$53,$98						; expected crc
	.ascii	"mov <R,m>,<R,m> .......... $"

test_mvi	deftst($06,$A5,$00,$00, $c407, $f49d,$d13d, msbt,$de89,$7455,$53,$A5,$5509)	; MVI
	deftst($38,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 8 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 16 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$E2,$EF,$0D,$98						; expected crc
	.ascii	"mvi <b,c,d,e,h,l,m,a>,nn . $"

test_ldax	deftst($0A,$00,$00,$00, $b3a8, $1d2a,$7f8e,$42ac, msbt, msbt,$c6,$b1,$ef8e)	; LDAX
	deftst($10,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 2 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$0000,$0000,$D7,$FF,$0000)	; x 22 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$0000,$0000,$D5,$FF,$FFFF)
	.byte	$86,$F6,$A3,$65						; expected crc
	.ascii	"ldax <b,d> ............... $"

test_stax	deftst($02,$00,$00,$00, $0c3b, $b592,$6cff,$959e,msbt,msbt+1,$c1,$21,$bde7)	; STAX
	deftst($18,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 4 cycles
	deftst($00,$00,$00,$00, $FFFF, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 24 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$0000,$0000,$D5,$FF,$FFFF)
	.byte	$32,$40,$64,$7A						; expected crc
	.ascii	"stax <b,d> ............... $"

test_ldst	deftst($32,<msbt,>msbt,$00, $fd68, $f4ec,$44a0,$b543,$0653,$cdba,$d2,$4f,$1fd8)	; LDA/STA
	deftst($08,$00,$00,$00,     $0000, $0000,$0000,$0000,$0000,$0000,$00,$00,$0000)	; x 2 cycle
	deftst($00,$00,$00,$00,     $00FF, $0000,$0000,$0000,$0000,$0000,$D7,$FF,$0000)	; x 22 cycles
	deftst($FF,$00,$00,$FF,     $FFFF, $FFFF,$FFFF,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$F6,$18,$7E,$EB						; expected crc
	.ascii	"sta nnnn / lda nnnn ...... $"

; 8080 specific tests
tt80_ana	deftst($A0,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; ANA
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$B9,$B9,$1B,$1C						; expected crc
	.ascii	"ana <b,c,d,e,h,l,m,a> .... $"

tt80_ani	deftst($E6,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ANI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$F6,$EC,$03,$0C						; expected crc
	.ascii	"ani nn ................... $"

; 8085 specific tests
tt85_ana	deftst($A0,$00,$00,$00, $C53E, $573A,$4C4D, msbt,$E309,$A666,$D0,$3B,$ADBB)	; ANA
	deftst($07,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 2048 cycles
	deftst($00,$00,$00,$00, $00FF, $0000,$0000,$0000,$FFFF,$FFFF,$D7,$00,$0000)	; x 46 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$0000,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$53,$F7,$01,$80						; expected crc
	.ascii	"ana <b,c,d,e,h,l,m,a> .... $"

tt85_ani	deftst($E6,$00,$00,$00, $9140, $7E3C,$7A67,$DF6D,$5B61,$0B29,$10,$66,$85B2)	; ANI
	deftst($00,$00,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$00,$FF,$0000)	; x 256 cycles
	deftst($00,$FF,$00,$00, $0000, $0000,$0000,$0000,$0000,$0000,$D7,$00,$0000)	; x 14 cycles
	deftst($FF,$FF,$FF,$FF, $FFFF, $0000,$0000,$FFFF,$FFFF,$FFFF,$D5,$FF,$FFFF)
	.byte	$F6,$EC,$03,$0C
	.ascii	"ani nn ................... $"

.macro deftst(_iut1,_iut2,_iut3,_iut4,_memop,_iy,_ix,_hl,_de,_bc,_flags,_acc,_sp)
	.byte	_iut1, _iut2, _iut3, _iut4
	.word	_memop
;;	.word	_iy, _ix
	.word	_hl, _de, _bc
	.byte	_flags, _acc
	.word	_sp
.endmacro
