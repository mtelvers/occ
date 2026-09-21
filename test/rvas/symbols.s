/* The forms that name a symbol, which decide the relocations occas has
   to produce: the twenty-high/twelve-low addressing pair, a call, a
   thread-local offset, and a branch or jump whose target is in another
   section.  Assembled with -mno-relax. */
	.text
	.globl	uses
	.type	uses, @function
uses:
	/* fixed addressing: the pair the machine is built around */
	lui	a0, %hi(object)
	addi	a0, a0, %lo(object)
	lui	a1, %hi(object)
	lw	a2, %lo(object)(a1)
	sw	a2, %lo(object)(a1)
	lui	a3, %hi(object+8)
	addi	a3, a3, %lo(object+8)

	/* position-independent addressing, which is a pair too */
.Lpcrel:
	auipc	a4, %pcrel_hi(object)
	addi	a4, a4, %pcrel_lo(.Lpcrel)
.Lpcrel2:
	auipc	a5, %got_pcrel_hi(object)
	ld	a5, %pcrel_lo(.Lpcrel2)(a5)

	/* a call, and a tail call */
	call	elsewhere
	call	elsewhere@plt
	tail	elsewhere
	jal	elsewhere

	/* thread-local, both models */
	lui	a0, %tprel_hi(tls_object)
	add	a0, a0, tp, %tprel_add(tls_object)
	addi	a0, a0, %tprel_lo(tls_object)
	lw	a1, %tprel_lo(tls_object)(a0)

	/* a branch whose target the assembler cannot measure */
	beq	a0, a1, elsewhere
	j	elsewhere
	ret
	.size	uses, .-uses

	.data
	.globl	object
	.type	object, @object
	.size	object, 16
object:
	.quad	object
	.quad	elsewhere+4

/* the address pseudo-instructions, which are a pair of instructions and
   a label of their own; what "la" means depends on ".option" */
	.text
	.globl	addresses
	.type	addresses, @function
addresses:
	lla	a0, object
	la	a1, object
	la.tls.ie a2, tls_object
	la.tls.gd a3, tls_object
	.option pic
	la	a4, object
	.option nopic
	la	a5, object
	ret
	.size	addresses, .-addresses
