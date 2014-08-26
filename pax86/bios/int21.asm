
		org 0xB60	; INT 21 handler address in BIOS segment
		
		CPU	186
		
		pushf
		sti
		cmp		ah,0x01			; DOS 1+ - READ CHARACTER FROM STANDARD INPUT, WITH ECHO
		jz		dos_01
		cmp		ah,0x02			; DOS 1+ - WRITE CHARACTER TO STANDARD OUTPUT
		jz		dos_02
		cmp		ah,0x06			; DOS 1+ - DIRECT CONSOLE I/O
		jz		dos_06
		cmp		ah,0x07			; DOS 1+ - DIRECT CHARACTER INPUT, WITHOUT ECHO
		jz		dos_07
		cmp		ah,0x08			; DOS 1+ - CHARACTER INPUT WITHOUT ECHO
		jz		dos_08
		cmp		ah,0x0A			; DOS 1+ - BUFFERED INPUT
		jz		dos_0A
		cmp		ah,0x0C			; DOS 1+ - FLUSH BUFFER AND READ STANDARD INPUT
		jz		dos_0C
		;-------
		; Go to the C handler routine
		;-------
		popf
		jmp		0xF000:0x0021
		;-------
		; DOS 1+ - READ CHARACTER FROM STANDARD INPUT, WITH ECHO
		; AH = 01h
		; Return:AL = character read
		;-------
dos_01:	call	dos_wait_char	; Return character in AL
		mov		ah,0x01			; Restore AH
out_AL:	push	bx
		push	ds
		mov		bx,0x40
		mov		ds,bx
		mov		bh,[0x62]		; BH = Active display page
		pop		ds
		push	ax
		mov		ah,0x0E
		int		0x10			; VIDEO - TELETYPE OUTPUT (AL=char, BH=page)
		pop		ax				; Restore AH
		pop		bx				; Restore BX
		popf
		iret
		;-------
		; DOS 1+ - WRITE CHARACTER TO STANDARD OUTPUT
		; AH = 02h
		; DL = character to write
		; Return:AL = last character output (despite the official docs which state nothing is returned) (at least DOS 2.1-7.0)
		;-------
dos_02:	mov		al,dl
		jmp		short	out_AL
		;-------
		; DOS 1+ - DIRECT CONSOLE I/O
		; AH = 06h
		; DL = character to output (except FFh)
		; Return:AL = character input (from function 07 if DL=0xFF) or output
		;-------
dos_06:	cmp		dl,0xFF
		jne		dos_02			; Go to output the char in DL if not 0xFF
		;-------
		; DOS 1+ - DIRECT CONSOLE INPUT
		; AH = 06h
		; DL = FFh
		; Return:ZF set if no character available
		; AL = 00h
		; ZF clear if character available
		; AL = character read
		;-------
		mov		ah,0x11			; KEYBOARD - CHECK FOR ENHANCED KEYSTROKE 
		int		0x16			; Do we have a key in buffer?
		jne		dos_06_cont		; Yes we do, go read it.
		;
		push	ds
		mov		ax,0x40
		mov		ds,ax
		cmp		byte [0x19],0	; Do we have a previous scancode saved to [0040:0019]?
		pop		ds
		jne		dos_06_cont		; Yes we do, go return it
		;
		popf
		int		0x28			; Call the idle interrupt
		mov		ah,0x06			; Restore AH
		xor		al,al
		retf	0x2				; Return with the Zero flag set
dos_06_cont:
		call	dos_wait_char	; Return character in AL
		mov		ah,0x06			; Restore AH
		popf
		and		ah,ah
		retf	0x2				; Return with the Zero flag clear (if character read)
		;-------
		; DOS 1+ - DIRECT CHARACTER INPUT, WITHOUT ECHO
		; AH = 07h
		; Return:AL = character read from standard input
		;-------
dos_07: call	dos_wait_char	; Return character in AL
		mov		ah,0x07			; Restore AH
		popf
		iret
		;-------
		; DOS 1+ - CHARACTER INPUT WITHOUT ECHO
		; AH = 08h
		; Return:AL = character read from standard input
		;-------
dos_08: call	dos_wait_char	; Return character in AL
		mov		ah,0x08			; Restore AH
		popf
		iret
		;-------
		; DOS 1+ - FLUSH BUFFER AND READ STANDARD INPUT
		; AH = 0Ch
		; AL = STDIN input function to execute after flushing buffer
		; other registers as appropriate for the input function
		; Return:As appropriate for the specified input function
		; Note: If AL is not one of 01h,06h,07h,08h, or 0Ah, the buffer is flushed but no input is attempted 
		;-------
dos_0C:	push	ds
		push	ax
		mov		ax,0x40
		mov		ds,ax
		mov		[0x19],ah		; Clear the possible previous scancode
		mov		ax,[0x1C]
		mov		[0x1A],ax		; Clear the keyboard buffer
		pop		ax
		pop		ds
		cmp		al,0x01
		je		dos_01
		cmp		al,0x06
		je		dos_06
		cmp		al,0x07
		je		dos_07
		cmp		al,0x08
		je		dos_08
		cmp		al,0x0A
		je		dos_0A
		xor		al,al
		popf
		iret
		;-------
		; DOS 1+ - BUFFERED INPUT
		; AH = 0Ah
		; DS:DX -> buffer (see #01344)
		; Return:Buffer filled with user input
		;
		; Notes: ^C/^Break are checked, and INT 23 is called if either detected.
		; Reads from standard input, which may be redirected under DOS 2+.
		; If the maximum buffer size (see #01344) is set to 00h, this call returns immediately without reading any input 
		;
		; Format of DOS input buffer:
		;
		; Offset  Size    Description     (Table 01344)
		; 00h    BYTE    maximum characters buffer can hold
		; 01h    BYTE    (call) number of chars from last input which may be recalled
		; 				 (ret) number of characters actually read, excluding CR
		; 02h  N BYTEs   actual characters read, including the final carriage return
		;-------
dos_0A:	push	ax
		push	bx
		push	cx
		add		dx,2			; Point DX to start of actual buffer
		mov		bx,dx
		mov		cx,[bx-2]		; CH=stored_size, CL=size of buffer
		;-------
		; If buffer size == 0, return immediately.
		;-------
		and		cl,cl
		jz		skip2
		;-------
		; The stored line is invalid unless it ends with a CR
		;-------
		mov		al,ch
		xor		ch,ch
		add		cx,bx
		sub		cx,1			; Now CX = buffer end offset (size-1)
		push	di
		mov		di,ax
		and		di,0xFF			; DI = stored_size
		cmp		byte [bx+di],0x0D	; Does the stored buffer end with a CR?
		je		loop0A			; Yep, OK
		xor		di,di			; Nope, stored_size = 0
		;-------
		; Loop here, filling the buffer
		;-------
loop0A:	call	dos_wait_char	; Return character in AL
		xor		ah,ah
		and		al,al			; Is it zero (extended keycode)?
		jnz		skip3			; Nope, an OK character
		call	dos_wait_char	; Return extended character in AL
		shl		ax,8			; AH = extended char, AL=0
		;-------
		; switch(c)
		;-------
skip3:
		;-------
		; 	case LF:	/* show LF if it's not the first character. Never store it */
		;		ignored
		;-------
		;-------
		;	case CTL_F:
        ;		break;
		;-------
		cmp		ax,0x06
		je		loop0A
		;-------
		;      case LEFT:
		;      case CTL_BS:
		;      case BS:
		;        if (count > 0)
		;        {
		;          unsigned new_pos;
		;          char c2 = local_buffer[--count];
		;          if (c2 == HT)
		;          {
		;            unsigned i;
		;            new_pos = cu_pos;
		;            for (i = 0; i < count; i++)
		;            {
		;              if (local_buffer[i] == HT)
		;                new_pos = (new_pos + 8) & ~7;
		;              else if (iscntrl(local_buffer[i]))
		;                new_pos += 2;
		;              else
		;                new_pos++;
		;            }
		;            do
		;              destr_bs(sft_out);
		;            while (scr_pos > new_pos);
		;          }
		;          else
		;          {
		;            if (iscntrl(c2))
		;              destr_bs(sft_out);
		;            destr_bs(sft_out);
		;          }
		;        }
		;        if (stored_pos > 0)
		;          stored_pos--;
		;        break;
		;-------
		cmp		ax,0x4B00		; LEFT
		je		skip7
		cmp		ax,0x7F			; CTL_BS
		je		skip7
		cmp		ax,0x08			; BS
		jne		skip8
skip7:	cmp		bx,dx			; if (count <= 0 )
		jbe		loop0A			;	ignore backspace
		dec		bx				; --count;
		;-------
		; destr_bs
		;-------
		push	bx
		push	ds
		mov		bx,0x40
		mov		ds,bx
		mov		bh,[0x62]		; BH = Active display page
		pop		ds
		mov		ax,0x0E08		; write_char(BS, sft_idx);
		int		0x10
		mov		ax,0x0E20		; write_char(' ', sft_idx);
		int		0x10
		mov		ax,0x0E08		; write_char(BS, sft_idx);
		int		0x10
		pop		bx
		jmp		short	loop0A
		;-------
		;	default:
        ;		if (count < size - 1 || c == CR)
        ;  			local_buffer[count++] = echo_char(c, sft_out);
        ;		else
        ;			write_char(BELL, sft_out);
        ;		if (stored_pos < stored_size && !insert)
        ;			stored_pos++;
        ;		break;
		;-------
skip8:
		cmp		ax,0x0D
		je		skip4
		cmp		bx,cx
		jae		skip5
skip4:	mov		[bx],al
		inc		bx
		push	bx
		push	ds
		mov		bx,0x40
		mov		ds,bx
		mov		bh,[0x62]		; BH = Active display page
		pop		ds
		mov		ah,0x0E
		int		0x10			; VIDEO - TELETYPE OUTPUT (AL=char, BH=page)
		pop		bx
		cmp		al,0x0D
		je		skip6			; All done if RET!
skip5:	jmp		loop0A			; else back to loop
		;-------
		; kb_count does not include the final CR
		; kp->kb_count = count - 1;
		;-------
skip6:	sub		bx,1
		sub		bx,dx			; Now BX = count-1
		mov		al,bl			; AL = count-1
		sub		dx,2			; Restore DX value to original value
		mov		bx,dx
		mov		[bx+1],al		; Save count of characters read
		pop		di
skip2:	pop		cx
		pop		bx
		pop		ax
		popf
		iret

		;-------
		; Subroutine to block until a key is pressed.
		; Returns pressed key from keyboard buffer in AL.
		; Can change AX, all other registers must be saved.
		;-------
dos_wait_char:
		push	ds
		mov		ax,0x40
		mov		ds,ax
		cmp		byte [0x19],0	; Do we have a previous scancode saved to [0040:0019]?
		jz		dos_wait_loop	; Nope, go to wait loop
		;-------
		; Return the previously saved extended scancode in AL
		;-------
		xor		al,al
		xchg	al,[0x19]		; AL = previous scancode saved to [0040:0019], clear the code
		pop		ds
		ret
dos_wait_loop:
		mov		ah,0x11			; KEYBOARD - CHECK FOR ENHANCED KEYSTROKE 
		int		0x16			; Do we have a key in buffer?
		jne		dos_have_key	; Yes we do, go read it.
		int		0x28			; Call the idle interrupt
		jmp		short dos_wait_loop	; And continue waiting
dos_have_key:
		mov		ah,0x10			; KEYBOARD - GET ENHANCED KEYSTROKE 
		int		0x16
		and		al,al			; if ( 0 == (ax&0xFF) )
		jnz		skip1
		mov		[0x19],ah		;	BIOSData[0x19] = (ax>>8);
skip1:	pop		ds
		ret

		align	4
