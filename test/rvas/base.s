/* Every instruction occ's RV64 back end emits, one per line, so that
   GNU as can be asked what each one encodes to.  The set was measured
   rather than chosen: compiling the whole corpus for this machine and
   tallying the mnemonics gives 96, and OCaml's runtime/riscv.S and
   ocamlopt's emitter ask for no family this does not cover.

   Assembled with -mno-relax: occas does not relax, and R_RISCV_RELAX is
   an invitation rather than an instruction, so the comparison is between
   two non-relaxing assemblers. */
	.text
	.globl	sample
	.type	sample, @function
sample:
	/* loads and stores, the only way to reach memory */
	lb	t0, 0(sp)
	lh	t1, 2(sp)
	lw	t2, 4(sp)
	ld	t3, 8(sp)
	lbu	t4, 1(sp)
	lhu	t5, 2(sp)
	lwu	t6, 4(sp)
	lb	a0, -2048(s0)
	ld	a1, 2047(s0)
	sb	t0, 0(sp)
	sh	t1, 2(sp)
	sw	t2, 4(sp)
	sd	t3, 8(sp)
	sd	a0, -2048(s0)
	flw	ft0, 4(sp)
	fld	ft1, 8(sp)
	fsw	ft0, 4(sp)
	fsd	ft1, 8(sp)

	/* register arithmetic, the R format */
	add	a0, a1, a2
	sub	a0, a1, a2
	and	a0, a1, a2
	or	a0, a1, a2
	xor	a0, a1, a2
	sll	a0, a1, a2
	srl	a0, a1, a2
	sra	a0, a1, a2
	slt	a0, a1, a2
	sltu	a0, a1, a2
	addw	a0, a1, a2
	subw	a0, a1, a2
	sllw	a0, a1, a2
	srlw	a0, a1, a2
	sraw	a0, a1, a2
	mul	a0, a1, a2
	mulw	a0, a1, a2
	mulh	a0, a1, a2
	mulhu	a0, a1, a2
	div	a0, a1, a2
	divu	a0, a1, a2
	divw	a0, a1, a2
	divuw	a0, a1, a2
	rem	a0, a1, a2
	remu	a0, a1, a2
	remw	a0, a1, a2
	remuw	a0, a1, a2

	/* immediates, the I format */
	addi	a0, a1, 7
	addi	a0, a1, -2048
	addi	a0, a1, 2047
	addiw	a0, a1, 7
	andi	a0, a1, 255
	ori	a0, a1, 16
	xori	a0, a1, -1
	slti	a0, a1, 0
	sltiu	a0, a1, 1
	slli	a0, a1, 3
	srli	a0, a1, 32
	srai	a0, a1, 63
	slliw	a0, a1, 3
	srliw	a0, a1, 4
	sraiw	a0, a1, 5

	/* the twenty-bit upper immediate, the U format */
	lui	a0, 1
	lui	a0, 524287
	auipc	a0, 0

	/* the pseudo-instructions that stand for those */
	mv	a0, a1
	not	a0, a1
	neg	a0, a1
	negw	a0, a1
	seqz	a0, a1
	snez	a0, a1
	sltz	a0, a1
	sgtz	a0, a1
	sext.w	a0, a1
	nop

	/* li, whose sequence is the assembler's own choice */
	li	a0, 0
	li	a0, 1
	li	a0, -1
	li	a0, 2047
	li	a0, 2048
	li	a0, -2048
	li	a0, -2049
	li	a0, 4096
	li	a0, 0x7fffffff
	li	a0, 0x80000000
	li	a0, -2147483648
	li	a0, 0xffffffff
	li	a0, 0x100000000
	li	a0, 0x123456789abcdef

	/* branches and jumps to a nearby label */
	beq	a0, a1, .Lhere
	bne	a0, a1, .Lhere
	blt	a0, a1, .Lhere
	bge	a0, a1, .Lhere
	bltu	a0, a1, .Lhere
	bgeu	a0, a1, .Lhere
	beqz	a0, .Lhere
	bnez	a0, .Lhere
	blez	a0, .Lhere
	bgez	a0, .Lhere
	bltz	a0, .Lhere
	bgtz	a0, .Lhere
	bgt	a0, a1, .Lhere
	ble	a0, a1, .Lhere
	bgtu	a0, a1, .Lhere
	bleu	a0, a1, .Lhere
.Lhere:
	j	.Lhere
	jal	.Lhere
	jalr	t2
	jalr	ra, 0(t2)
	jr	t2
	ret

	/* floating point, single and double */
	fadd.s	fa0, fa1, fa2
	fsub.s	fa0, fa1, fa2
	fmul.s	fa0, fa1, fa2
	fdiv.s	fa0, fa1, fa2
	fsqrt.s	fa0, fa1
	fadd.d	fa0, fa1, fa2
	fsub.d	fa0, fa1, fa2
	fmul.d	fa0, fa1, fa2
	fdiv.d	fa0, fa1, fa2
	fsqrt.d	fa0, fa1
	fneg.s	fa0, fa1
	fneg.d	fa0, fa1
	fabs.s	fa0, fa1
	fabs.d	fa0, fa1
	fmv.s	fa0, fa1
	fmv.d	fa0, fa1
	feq.s	a0, fa1, fa2
	flt.s	a0, fa1, fa2
	fle.s	a0, fa1, fa2
	feq.d	a0, fa1, fa2
	flt.d	a0, fa1, fa2
	fle.d	a0, fa1, fa2
	fcvt.s.d	fa0, fa1
	fcvt.d.s	fa0, fa1
	fcvt.w.s	a0, fa1, rtz
	fcvt.wu.s	a0, fa1, rtz
	fcvt.l.s	a0, fa1, rtz
	fcvt.lu.s	a0, fa1, rtz
	fcvt.w.d	a0, fa1, rtz
	fcvt.wu.d	a0, fa1, rtz
	fcvt.l.d	a0, fa1, rtz
	fcvt.lu.d	a0, fa1, rtz
	fcvt.s.w	fa0, a1
	fcvt.s.wu	fa0, a1
	fcvt.s.l	fa0, a1
	fcvt.s.lu	fa0, a1
	fcvt.d.w	fa0, a1
	fcvt.d.wu	fa0, a1
	fcvt.d.l	fa0, a1
	fcvt.d.lu	fa0, a1
	fmv.x.w	a0, fa1
	fmv.w.x	fa0, a1
	fmv.x.d	a0, fa1
	fmv.d.x	fa0, a1

	/* the A extension, and the fences */
	fence
	fence	rw, rw
	fence	r, rw
	fence	rw, w
	fence	iorw, iorw
	lr.w	t0, (t2)
	lr.w.aqrl	t0, (t2)
	lr.d	t0, (t2)
	lr.d.aqrl	t0, (t2)
	sc.w	t1, t0, (t2)
	sc.w.rl	t1, t0, (t2)
	sc.d	t1, t0, (t2)
	sc.d.rl	t1, t0, (t2)
	amoswap.w	t0, t1, (t2)
	amoswap.w.aqrl	t0, t1, (t2)
	amoswap.d.aqrl	t0, t1, (t2)
	amoadd.w	t0, t1, (t2)
	amoadd.w.aqrl	t0, t1, (t2)
	amoadd.d	t0, t1, (t2)
	amoadd.d.aqrl	t0, t1, (t2)
	amoand.d.aqrl	t0, t1, (t2)
	amoor.d.aqrl	t0, t1, (t2)
	amoxor.d.aqrl	t0, t1, (t2)
	unimp
	.size	sample, .-sample
