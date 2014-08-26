
	org 0x1060	; INT74 (IRQ12) = PS/2 Mouse driver IRQ handler.

	CPU	186

	PUSH	AX
	PUSH	1234h			; Push Status word 				(word at offset 02h)
	PUSH	0012h			; Push X coordinate data 		(byte at offset 05h)
	PUSH	0034h			; Push Y coordinate data		(byte at offset 08h)
	PUSH	0000h			; Push 0000
	CALL 	1234h:5678h		; Call the callback function 	(2*word at offset 0Eh)
	ADD		SP,4*2			; Pop the pushed words
	MOV     AL,20h
	OUT     20h,AL			; Send EOI
	POP		AX
	IRET
