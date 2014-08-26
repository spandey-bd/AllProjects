
		org 0xA00	; INT 16 handler address in BIOS segment
		
		CPU	186

		; This routine processes the int 16h function requests.
		; Code based on Art of Assembly: 20.5 The Keyboard Interrupt Service Routine
		;
		;  AH   Description
		;  --   ------------------------------------------------
		;  00h  Get a key from the keyboard, return code in AX.
		;  01h  Test for available key, ZF=1 if none, ZF=0 and
		;  		AX contains next key code if key available.
		;  02h  Get shift status. Returns shift key status in AL.
		;  03h  Set Autorepeat rate. BH=0,1,2,3 (delay time in
		;  		quarter seconds), BL=0..1Fh for 30 char/sec to
		;  		2 char/sec repeat rate.
		;  05h  Store scan code (in CX) in the type ahead buffer.
		;  10h  Get a key (same as 00h in this implementation).
		;  11h  Test for key (same as 01h).
		;  12h  Get extended key status. Returns status in AX.


		test    ah, 0xEF        ;Check for 0h and 10h
        je      GetKey
        cmp     ah, 0x02        ;Check for 01h and 02h
        jb      TestKey
		je      GetStatus
		cmp     ah, 0x03        ;Check for AutoRpt function.
		je      SetAutoRpt
		cmp     ah, 0x05        ;Check for StoreKey function.
		je      StoreKey
		cmp     ah, 0x11        ;Extended test key opcode.
		je      TestKey
		cmp     ah, 0x12        ;Extended status call
		je      ExtStatus

		; Well, it's a function we don't know about, so just return to the caller.

		iret

		; If the user specified ah=0 or ah=10h, come down here (we will not
		; differentiate between extended and original PC getc calls).

GetKey: mov     ah, 0x11
		int     0x16            ;See if key is available.
		je      GetKey          ;Wait for keystroke.

		push    ds
		push    bx
		mov     ax, 0x40
		mov     ds, ax
		cli                     ;Critical region! Ints off.
		mov     bx, [0x1A]      ;Ptr to next character.
		mov     ax, [bx]        ;Get the character.

		ADD     BX,0x02
		CMP     BX,0x3E
		JB      Skip1
		MOV     BX,0x1E
Skip1:	MOV     [0x1A],BX
		
		pop     bx
		pop     ds
		iret                    ;Restores interrupt flag.

		; TestKey-       Checks to see if a key is available in the keyboard buffer.
		;               We need to turn interrupts on here (so the kbd ISR can
		;               place a character in the buffer if one is pending).
		;               Generally, you would want to save the interrupt flag here.
		;               But BIOS always forces interrupts on, so there may be some
		;               programs out there that depend on this, so we won't "fix"
		;               this problem.
		;
		;               Returns key status in ZF and AX. If ZF=1 then no key is
		;               available and the value in AX is indeterminate. If ZF=0
		;               then a key is available and AX contains the scan/ASCII
		;               code of the next available key. This call does not remove
		;               the next character from the input buffer.

TestKey:
		sti                     ;Turn on the interrupts.
		push    ds
		push    bx
		mov     ax, 0x40
		mov     ds, ax
		cli                     ;Critical region, ints off!
		MOV     BX,[0x1A]
		mov     ax, [bx]        ;BIOS returns avail keycode.
		CMP     BX,[0x1C]		;ZF=1, if empty buffer
		pop     bx
		pop     ds
		sti                     ;Ints back on.
		retf    2               ;Pop flags (ZF is important!)


		; The GetStatus call simply returns the KbdFlags1 variable in AL.

GetStatus:
		push    ds
		mov     ax, 0x40
		mov     ds, ax
		MOV     AL,[0x17]		;Just return Std Status.
		pop     ds
		iret

		; StoreKey-     Inserts the value in CX into the type ahead buffer.

StoreKey:
		push    ds
		push    bx
		mov     ax, 0x40
		mov     ds, ax
		cli                     ;Ints off, critical region.
		MOV     BX,[0x1C]		;Address where we can put
		push    bx              ; next key code.
		mov     [bx], cx        ;Store the key code away.

		ADD     BX,2
		CMP     BX,0x3E
		JB      Skip2
		MOV     BX,0x1E
Skip2:	MOV     [0x1C],BX

		CMP     BX,[0x1A]		;Data overrun?
		jne     StoreOkay       ;If not, jump, if so
		POP     word [0x1C]    	; ignore key entry.
		sub     sp, 2           ;So stack matches alt path.
StoreOkay:
		add     sp, 2           ;Remove junk data from stk.
		pop     bx
		pop     ds
		iret                    ;Restores interrupts.

		; ExtStatus-    Retrieve the extended keyboard status and return it in
		;               AH, also returns the standard keyboard status in AL.

ExtStatus:
		push    ds
		mov     ax, 0x40
		mov     ds, ax

		MOV     AH,[0x18]
		and     ah, 0x7F        ;Clear final sysreq field.
		test    ah, 4        	;Test cur sysreq bit.
		je      NoSysReq        ;Skip if it's zero.
		or      ah, 0x80        ;Set final sysreq bit.
NoSysReq:
		and     ah, 0xF0        ;Clear alt/ctrl bits.
		MOV     AL,[0x96]
		and     al, 0x0C       	;Grab rt alt/ctrl bits.
		or      ah, al          ;Merge into AH.
		MOV     AL,[0x18]
		and     al, 3         	;Grab left alt/ctrl bits.
		or      ah, al          ;Merge into AH.
		MOV     AL,[0x17]		;AL contains normal flags.
		pop     ds
		iret

		; SetAutoRpt-   Sets the autorepeat rate. On entry, bh=0, 1, 2, or 3 (delay
		;               in 1/4 sec before autorepeat starts) and bl=0..1Fh (repeat
		;               rate, about 2:1 to 30:1 (chars:sec).

SetAutoRpt:
		push    cx
		push    bx

		mov     al, 0xAD		;Disable kbd for now.
		out     0x64, al		;Okay, send the command to the 8042

		and     bh, 3			;Force into proper range.
		mov     cl, 5
		shl     bh, cl          ;Move to final position.
		and     bl, 0x1F		;Force into proper range.
		or      bh, bl          ;8042 command data byte.
		mov     al, 0xF3		;8042 set repeat rate cmd.
		out     0x60, al        ;Send the command to 8042.
		mov     al, bh          ;Get parameter byte
		out     0x60, al        ;Send parameter to the 8042.

		mov     al, 0xAE		;Reenable keyboard.
		out     0x64, al		;Okay, send the command to the 8042
		mov     al, 0xF4		;Restart kbd scanning.
		out     0x60, al

		pop     bx
		pop     cx
		iret

		align	4
