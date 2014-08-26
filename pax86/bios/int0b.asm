
	org 0x1020	; INT0B (IRQ3) = Mouse driver IRQ handler.

	CPU	186

	PUSHA
	PUSH    DS
	PUSH	ES
	PUSH    CS
	POP     DS
	MOV     AX,[1010h]
	MOV     BX,[1012h]
	MOV     CX,[1014h]
	MOV     DX,[1016h]
	MOV     SI,[1018h]
	MOV     DI,[101Ah]
	CALL    FAR [101Ch]
	POP		ES
	POP     DS
	MOV     AL,20h
	OUT     20h,AL
	POPA
	IRET

	align	4