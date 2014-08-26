
		org 0x100	; INT9 (IRQ1) Keyboard interrupt default handler.
		
		CPU	186

		; Code based on Art of Assembly: 20.5 The Keyboard Interrupt Service Routine

		PUSH    DS
		PUSH    AX
		PUSH    CX
		MOV     AX,0040h
		MOV     DS,AX
		MOV     AL,0ADh
		CALL    SetCmd
		CLI
		IN      AL,60h
		CMP     AL,0EEh
		JZ      QuitInt9
		CMP     AL,0FAh
		JNZ     NotAck
		OR      BYTE [0097h],10h
		JMP     QuitInt9
NotAck:		
		CMP     AL,0FEh
		JNZ     NotResend
		OR      BYTE [0097h],20h
		JMP     QuitInt9
NotResend:		
		MOV     AH,4Fh
		STC
		INT     15h									; Call keyboard intercept INT 15
		JNB     QuitInt9							; If carry clear, we should ignore this scan code
		CALL    PutInBuffer							; Else go put it into keyboard buffer
QuitInt9:		
		MOV     AL,0AEh
		CALL    SetCmd
		MOV     AL,20h
		OUT     20h,AL
		POP     CX
		POP     AX
		POP     DS
		IRET
		;-------
		; SetCmd subroutine, at address 0142h
		;-------
SetCmd:	
		PUSH    CX
		PUSH    AX
		CLI
		XOR     CX,CX
Wait4Empty:		
		IN      AL,64h
		TEST    AL,02h
		LOOPNZ  Wait4Empty
		POP     AX
		OUT     64h,AL
		STI
		POP     CX
		RET
		;-------
		; PutInBuffer subroutine, at address 0153h
		;-------
PutInBuffer:		
		PUSH    DS
		PUSH    BX
		MOV     BX,40h
		MOV     DS,BX
		CMP     AL,0E0h				; Is it 0E0h ?
		JNZ     TryE1
		OR      BYTE [0096h],02h
		AND     BYTE [0096h],0FEh
		JMP     Done
TryE1:		
		CMP     AL,0E1h				; Is it 0E1h ?
		JNZ     TryIns
		OR      BYTE [0096h],01h
		AND     BYTE [0096h],0FDh
		JMP     Done
TryIns:		
		CMP     AL,52h				; Is it 'Ins' scan code?
		JNZ     TryInsUp
		OR      BYTE [0018h],80h
		JMP     DoPIB
TryInsUp:		
		CMP     AL,0D2h				; Is it 'Ins' up scan code?
		JNZ     TryLShift
		AND     BYTE [0018h],7Fh
		XOR     BYTE [0017h],80h
		JMP		QuitPIB
TryLShift:		
		CMP		AL,2Ah				; Is it 'Left Shift' ?
		JNZ     TryLShiftUp
		OR      BYTE [0017h],02h
		JMP     QuitPIB
TryLShiftUp:		
		CMP     AL,0AAh				; Is it 'Left Shift' up?
		JNZ     TryRShift
		AND     BYTE [0017h],0FDh
		JMP     QuitPIB
TryRShift:		
		CMP     AL,036h				; Is it 'Right Shift' ?
		JNZ     TryRShiftUp
		OR      BYTE [0017h],01h
		JMP     QuitPIB
TryRShiftUp:		
		CMP     AL,0B6h				; Is it 'Right Shift' up?
		JNZ     TryAlt
		AND     BYTE [0017h],0FEh
		JMP     QuitPIB
TryAlt:		
		CMP     AL,38h				; Is it 'Alt' ?
		JNZ     TryAltUp
		OR      BYTE [0017h],08h
		JMP     QuitPIB
TryAltUp:		
		CMP     AL,0B8h				; Is it 'Alt' up?
		JNZ     TryCtrl
		AND     BYTE [0017h],0F7h
		JMP     QuitPIB
TryCtrl:		
		CMP     AL,1Dh				; Is it 'Ctrl' ?
		JNZ     TryCtrlUp
		OR      BYTE [0017h],04h
		JMP     QuitPIB
TryCtrlUp:		
		CMP     AL,9Dh				; Is it 'Ctrl' up?
		JNZ     TryCaps
		AND     BYTE [0017h],0FBh
		JMP     QuitPIB
TryCaps:		
		CMP     AL,3Ah				; Is it 'Caps' ?
		JNZ     TryCapsUp
		OR      BYTE [0018h],40h
		XOR     BYTE [0017h],40h
		JMP     QuitPIB
TryCapsUp:		
		CMP     AL,0BAh				; Is it 'Caps' up?
		JNZ     DoPIB
		AND     BYTE [0018h],0BFh
		; CALL	SetLEDs
		JMP     QuitPIB
DoPIB:		
		TEST    AL,80h
		JNZ     QuitPIB				; Ignore KeyUps when putting scan codes into buffer
		CALL    Convert
		TEST    AX,AX
		JZ      QuitPIB
		; Put the actual character into keyboard buffer
		PUSH    CX
		MOV     CX,AX
		MOV     AH,05h
		INT     16h
		POP     CX
QuitPIB:		
		AND     BYTE [0096h],0FCh
Done:		
		POP     BX
		POP     DS
		RET	
		;-------
		; Convert subroutine
		;-------
Convert:		
		PUSH    BX
		TEST    AL,80h
		JZ      DownScanCode
		MOV     AH,AL
		MOV     AL,00h
		JMP     CSDone
DownScanCode:		
		MOV     BH,00h
		MOV     BL,AL
		SHL     BX,1
		SHL     BX,1
		SHL     BX,1
		TEST    BYTE [0017h],08h			; Is 'Alt' down?
		JZ      NoAlt						; Nope
		ADD     BL,03h						; Use Alt table
		JMP     DoConvert
NoAlt:		
		TEST    BYTE [0017h],04h			; Is 'Ctrl' down?
		JZ      NoCtrl
		ADD     BL,02h						; Use Ctrl table
		JMP     DoConvert
NoCtrl:		
		CMP     AL,47h						; Code affected by Caps Lock?
		JB      DoCapsLock					; Jump if Yes
		TEST    BYTE [0017h],03h			; Left or Right Shift?
		JZ      DoConvert					; Nope, DoConvert
		ADD     BL,01h						; Use the Shift table
		JMP     DoConvert					; DoConvert
DoCapsLock:		
		TEST    BYTE [0017h],40h			; CapsLock active?
		JZ      DoShift						; Nope
		TEST    BYTE [0017h],03h			; Left or Right Shift?
		JZ      CapsOnly					; Nope
		ADD     BL,06h						; Use Caps + Shift table
		JMP     DoConvert
CapsOnly:		
		ADD     BL,05h						; Use Caps table
		JMP     DoConvert
DoShift:		
		TEST    BYTE [0017h],03h			; Left or Right Shift?
		JZ      DoConvert					; Nope, DoConvert
		ADD     BL,01h						; Use Shift table
DoConvert:		
		SHL     BX,1
		MOV     AX,[CS:BX+0300h]			; Scancode table must begin at F000:0300!
CSDone:		
		POP     BX
		RET	
