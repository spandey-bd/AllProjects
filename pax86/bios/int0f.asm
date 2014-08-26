
	org 0x1000	; INT0F (IRQ7) = SoundBlaster default IRQ handler.

	CPU	186

	PUSH    AX
	MOV     AL,20h
	OUT     20h,AL
	POP     AX
	IRET

	align	4
