
[BITS 64]
		;; déclaration des symboles
		global main			;; export du symbole
		extern printf		;; symbole importé

		;; code
		section .text		;; déclaration d une section pour contenir le code

main:						;; déclaration d un label
		push rbp			;; prologue
		mov rbp, rsp

		mov rdi, 1			;; Premier argument : sortie
		mov rsi, MessageStr	;; Deuxieme argument : chaine à afficher
		mov rdx, MessageLen	;; Troisieme argument : longueur de la chaine
		mov rax, 1			;; Numéro du syscall à afficher : ici 1 = write
		syscall

		mov rdi, FormatStr	;; passage du premier argument de printf
		mov rsi, MessageStr	;; passage du deuxieme argument de printf
		mov rdx, MessageLen	;; passage du troisieme argument de printf
		call printf			;; appel à printf

		mov rsp, rbp		;; epilogue
		pop rbp

		mov rax, 60			;; numéro du syscall “exit”
		xor rdi, rdi		;; passage du premier (& unique) paramètre dans rdi  (mov rdi, 0h <=> rdi ^= rdi)
		syscall				;; appel au noyau pour exécution

		ret					;; sortie de la fonction

		;; read only data
		section .rodata		;; déclaration d une section pour contenir les données initialisées
		
		FormatStr db 0Ah,'Message: [%s][%d]',0Ah,0
		MessageStr db 'Hello World !',0
		MessageLen equ $ - (MessageStr + 1)