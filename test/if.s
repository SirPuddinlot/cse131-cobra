section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 1
  cmp rax, 1
  je else_1
  mov rax, 2
  jmp endif_2
else_1:
  mov rax, 4
endif_2:
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret

