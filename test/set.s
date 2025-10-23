section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 10
  mov [rsp - 8], rax
  mov rax, [rsp - 8]
  mov [rsp - 16], rax
  mov rax, 2
  mov rcx, rax
  or rcx, [rsp - 16]
  test rcx, 1
  jne error_invalid_argument
  add rax, [rsp - 16]
  jo error_overflow
  mov [rsp - 8], rax
  mov rax, [rsp - 8]
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret

