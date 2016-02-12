
[BITS 64]
		;; déclaration des symboles
		global main			;; export du symbole
		extern printf		;; symbole importé

		;; code
		section .text		;; déclaration d une section pour contenir le code

main:						;; déclaration d un label
		push rbp			;; prologue
		mov rbp, rsp

		mov rdi, FormatStr	;; passage du premier argument de printf
		call printf			;; appel à printf

		mov rsp, rbp		;; epilogue
		pop rbp

		mov rax, 60			;; numéro du syscall “exit”
		xor rdi, rdi		;; passage du premier (& unique) paramètre dans rdi  (mov rdi, 0h <=> rdi ^= rdi)
		syscall				;; appel au noyau pour exécution

		ret					;; sortie de la fonction

		;; read only data
		section .rodata		;; déclaration d une section pour contenir les données initialisées
		FormatStr db 'Hello World !',0Ah,0