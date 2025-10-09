section .text
global our_code_starts_here
our_code_starts_here:
  mov rax, 5
  mov [rsp - 8], rax
  mov rax, [rsp - 8]
  add rax, 1

  ret
