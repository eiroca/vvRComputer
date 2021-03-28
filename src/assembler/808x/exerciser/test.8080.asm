.target	"8080"
.format	"prg"

.code
	.org	$0100

	mvi A, $0
	sui -1
	nop
hlt

Var1	.var	0
regs1	.loop	32
	mvi A, $18
	sui Var1
	nop
	Var1 = Var1 + 1
	.endloop
	HLT
