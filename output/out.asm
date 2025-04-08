section .text 
	global _start 
_start: 
; I_STACK_PTR_ADD
	add rsp, 0 
; I_PUSH
	push 2 
; I_PUSH
	push 1 
; I_ADD
	pop rax 
	pop rbx 
	add rax, rbx 
	push rax 
; I_PRINT_INT
; I_PRINT_NEWLINE
