section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 146
  test rax, 1
  jne error_invalid_argument
  add rax, 2
  test rax, 1
  jne error_invalid_argument
  sub rax, 2
  test rax, 1
  jne error_invalid_argument
  sub rax, 2
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret

