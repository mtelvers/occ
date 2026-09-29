/* The bit-test family, which the native compiler emits for a test
   against a single bit: the index is a register (0F A3 and its three
   neighbours, the operation in the opcode) or an immediate (0F BA, the
   operation in ModRM.reg).  Assembled with --64. */
	.text
bits:
	btq	%rcx, %rax
	btsq	%rcx, %rax
	btrq	%rcx, %rax
	btcq	%rcx, %rax
	btq	%rcx, (%rax)
	btsq	%rcx, (%rax)
	btrq	%r13, 24(%r12,%rbx,8)
	btcq	%rcx, (%rax)
	btq	$3, %rax
	btsq	$63, %rax
	btrq	$0, (%rax)
	btcq	$7, 16(%rsp)
	btl	%ecx, %eax
	btsl	%ecx, (%rax)
	btrl	$5, %r9d
	btcl	%r10d, %r11d
	btw	%cx, %ax
	btsw	$1, (%rax)
	bts	%rcx, %rax
	ret
