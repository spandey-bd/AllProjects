
	org 0x2E0	; INT11 = BIOS GET EQUIPMENT LIST handler.

	CPU	186

	STI
	PUSH    DS
	MOV     AX,0040h
	MOV     DS,AX
	MOV     AX,[0010h]
	POP     DS
	IRET	