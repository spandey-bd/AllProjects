
	org 0x2F0	; INT11 = BIOS GET MEMORY SIZE handler.

	CPU	186

	STI
	PUSH    DS
	MOV     AX,0040h
	MOV     DS,AX
	MOV     AX,[0013h]
	POP     DS
	IRET	
