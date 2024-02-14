.data
.align 4
.text
.global main
main:
  // push {fp, lr}
  stp fp, lr, [sp, #-16]!
  mov fp, sp
  // Stack pointer unchanged, no stack allocated arguments
  mov x8, #-1
  mov x0, x8
  // statement primitives do not return results (but will clobber r0/rax)
  bl exit
  mov x0, #0
  // pop {fp, lr}
  ldp fp, lr, [sp], #16
  ret
