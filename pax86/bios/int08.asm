
		org 0xA00	; INT8 (IRQ0) Timer interrupt default handler.
		
		CPU	186

		PUSH    DS
		PUSH    AX
		MOV     AX,0040h
		MOV     DS,AX
		DEC     BYTE [0040h]
		XOR     AX,AX
		INC     WORD [006Ch]
		JNZ     NoOverflow
		INC     WORD [006Eh]
NoOverflow:		
		CMP     WORD [006Eh],18h
		JNZ     Int8Done
		CMP     WORD [006Ch],00B0h
		JNZ     Int8Done
		MOV     [006Eh],AX
		MOV     [006Ch],AX
		MOV     BYTE [0070h],01h
Int8Done:		
		INT     1Ch
		CLI							; We don't want to get another interrupt before the IRET. Fixes Darkseed hanging.
		MOV     AL,20h
		OUT     20h,AL
		POP     AX
		POP     DS
		IRET
