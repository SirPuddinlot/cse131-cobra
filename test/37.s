section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 74
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret
