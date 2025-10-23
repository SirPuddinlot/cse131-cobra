section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 10
  mov [rsp - 8], rax
  mov rax, 20
  mov [rsp - 8], rax
  mov [rsp - 16], rax
  mov rax, 6
  mov rcx, rax
  or rcx, [rsp - 16]
  test rcx, 1
  jne error_invalid_argument
  add rax, [rsp - 16]
  jo error_overflow
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret

