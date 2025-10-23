section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 4
  mov [rsp - 8], rax
  mov rax, 6
  mov [rsp - 16], rax
  mov rax, 0
  mov [rsp - 24], rax
  mov rax, 0
  mov [rsp - 32], rax
  mov rax, 0
  mov [rsp - 40], rax
loop_start_1:
  mov rax, [rsp - 32]
  mov [rsp - 48], rax
  mov rax, [rsp - 8]
  mov rcx, rax
  or rcx, [rsp - 48]
  test rcx, 1
  jne error_invalid_argument
  cmp [rsp - 48], rax
  mov rax, 1
  mov rcx, 3
  cmovl rax, rcx
  cmp rax, 1
  je else_3
  mov rax, 0
  mov [rsp - 40], rax
loop_start_5:
  mov rax, [rsp - 40]
  mov [rsp - 48], rax
  mov rax, [rsp - 16]
  mov rcx, rax
  or rcx, [rsp - 48]
  test rcx, 1
  jne error_invalid_argument
  cmp [rsp - 48], rax
  mov rax, 1
  mov rcx, 3
  cmovl rax, rcx
  cmp rax, 1
  je else_7
  mov rax, [rsp - 24]
  test rax, 1
  jne error_invalid_argument
  sub rax, 2
  mov [rsp - 24], rax
  mov rax, [rsp - 40]
  test rax, 1
  jne error_invalid_argument
  add rax, 2
  mov [rsp - 40], rax
  jmp endif_8
else_7:
  mov rax, [rsp - 24]
  jmp loop_end_6
endif_8:
  jmp loop_start_5
loop_end_6:
  mov rax, [rsp - 32]
  test rax, 1
  jne error_invalid_argument
  add rax, 2
  mov [rsp - 32], rax
  jmp endif_4
else_3:
  mov rax, [rsp - 24]
  jmp loop_end_2
endif_4:
  jmp loop_start_1
loop_end_2:
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret

