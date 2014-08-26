//=============================================================================
// ports_SB.s
//
// This file contains SoundBlaster (including AdLib) I/O port handlers.
//
// This file is part of the x86 emulation core written in ARM Assembly, originally
// from the DSx86 Nintendo DS DOS Emulator. See http://dsx86.patrickaalto.com
//
// Copyright (c) 2009-2013 Patrick "Pate" Aalto
//	
// Redistribution and use in source or binary form, with or without modifications,
// is NOT permitted without specific prior written permission from the author.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//=============================================================================

	.arch armv5te
	.file	"ports_SB.s"

	.global	in_226_SB_reset
	.global	in_22A_SB_read_data
	.global	in_22C_SB_DSP_status
	.global	in_22E_SB_data_avail
	.global	out_226_SB_reset
	.global	out_22C_SB_DSP
	.global	in_228_388_FM_status
	.global	out_228_388_FM_regsel
	.global	out_229_389_FM_data
	
	// ============= SOUNDBLASTER & ADLIB CODE AREA ====================

    .text
    .align  2
    .code   32
	.arm
	
#include "defines.inc"
#include "macros.inc"

#define	FIFO_SB		9
#define DSP_BUFSIZE 64

.macro DSP_FlushData_r2r1
	ldr		r2,=dsp_out_used
	mov		r1, #0
	str		r1,	[r2]					// sb.dsp.out.used=0;
	str		r1, [r2, #4]				// sb.dsp.out.pos=0;
.endm

.macro DSP_AddData_r0
	ldr		r2,=dsp_out_used
	ldr		r1,[r2], #8					// r1 = dsp_out_used, r2 points to dsp_out_data
	and		r0, #0xFF
	cmp		r1, #DSP_BUFSIZE			// if (sb.dsp.out.used<DSP_BUFSIZE)
	bge		1f							// {
	orr		r0, r1, lsl #16				// 	Save dps_out_used to r0 high halfword
	ldr		r1,[r2, #-4]				// 	r1 = dsp_out_pos
	add		r1, r0, lsr #16				//	r1 = Bitu start=sb.dsp.out.used+sb.dsp.out.pos;
	cmp		r1, #DSP_BUFSIZE			//	if (start>=DSP_BUFSIZE)
	subge	r1, #DSP_BUFSIZE			//		start-=DSP_BUFSIZE;
	strb	r0, [r2, r1]				//	sb.dsp.out.data[start]=val;
	lsr		r0, #16						//	r0 = dsp_out_used
	add		r0, #1						//	sb.dsp.out.used++;
	str		r0, [r2, #-8]				//	Save new dsp_out_used
1:										// }
.endm

.macro DSP_ReadData_r0					// static Bit8u DSP_ReadData(void) {
	ldr		r2,=dsp_out_used
	ldr		r1, [r2], #8				// 	r1 = dsp_out_used, r2 points to dsp_out_data
	subs	r1, #1						//	if (sb.dsp.out.used)
	ldrmib	r0, [r2, #-12]				//	Use the previous data if nothing new in the buffer
	bmi		1f							//	{
	str		r1, [r2, #-8]				//		sb.dsp.out.used--;
	ldr		r1, [r2, #-4]				// 		r1 = dsp_out_pos
	ldrb	r0, [r2, r1]				//		data=sb.dsp.out.data[sb.dsp.out.pos];
	add		r1, #1						//		sb.dsp.out.pos++;
	strb	r0, [r2, #-12]				//		Save the data value to the last data (for possible re-read)
	cmp		r1, #DSP_BUFSIZE			//		if (sb.dsp.out.pos>=DSP_BUFSIZE)
	subge	r1, #DSP_BUFSIZE			//			sb.dsp.out.pos-=DSP_BUFSIZE;
	str		r1, [r2, #-4]				//			Save new dsp_out_pos
1:										//	}
.endm									// }

.macro send_DSP_to_ARM7
	push	{lr}						// Push used registers
	bl		HandleSBCommand				// in "sb.S"
	pop		{lr}						// Pop used registers
.endm

	// ============= SOUNDBLASTER INPUT PORTS ==========================

	//-------
	// 0x22A = SoundBlaster READ DATA port.
	//-------
in_22A_SB_read_data:
	push	{r0}					// Save r0 = flags
	DSP_ReadData_r0
	bic		eax, #0xFF				// Clear current AL value
	orr		eax, r0					// AL = SB Data byte
	pop		{r0}					// Restore flags
	bx		lr

	//-------
	// 0x22C = SoundBlaster DSP WRITE STATUS port.
	//-------
in_22C_SB_DSP_status:
	ldr		r2,=dsp_write_busy
	ldr		r1, [r2]				// r1 = dsp_write_busy
	add		r1, #1
	tst		r1, #8
	str		r1, [r2]
	orr		eax, #0xFF				// if (sb.dsp.write_busy & 8) return 0xff;
	biceq	eax, #0x80				// else return 0x7f;
	bx		lr
	
	//-------
	// 0x22E = SoundBlaster DATA AVAILABLE port.
	//-------
in_22E_SB_data_avail:
	ldr		r2,=dsp_out_used
	ldr		r1, [r2]				// r1 = dsp_out_used
	cmp		r1, #0					// if (sb.dsp.out.used)
	orr		eax, #0xFF				//	return 0xff;
	biceq	eax, #0x80				// else return 0x7f;
#if 1
	mov		r1, #1
#ifdef IOS
	ldr		r2,=sb_irq_ack
	strb	r1, [r2]
#else
	strb	r1, [r2, #(sb_irq_ack - dsp_out_used)]
#endif
#endif
	bx		lr

	// ============= ADLIB/SB FM OUTPUT PORTS ==========================

	//-------
	// FM status port, hack for "Solar Winds" and "B.A.T. 2", need proper AdLib timer handling!
	//-------
in_228_388_FM_status:
	ldr		r2, =AdLibStatus
	bic		eax, #0xFF
	ldrb	r1, [r2]				// Get the current AdLib status byte
	tst		r1, #3					// Are either timers running?
	andeq	r1, #0xE0				// Nope, so just return the status bits ...
	orreq	eax, r1					// ... in AL register.
	bxeq	lr
	//-------
	// Timers are running, so check for overflows.
	//-------
#ifdef IOS
	push	{r9, r12, lr}
	bl		_CheckAdlibTimers		// Call the C routine in "timer.c"
	pop		{r9, r12, lr}
#else
	push	{r12, lr}
	bl		CheckAdlibTimers		// Call the C routine in "timer.c"
	pop		{r12, lr}
#endif
	//-------
	// Return the new status after the timer checks
	//-------
	and		r0, #0xE0				// Only return the status bits ...
	orr		eax, r0					// ... in AL register.
	bx		lr

	//-------
	// AdLib register selection (0x388)
	// SoundBlaster FM register selection (0x228)
	// On input:
	//	r1 = byte to output
	//	lr = return address
	//------
out_228_388_FM_regsel:	
	ldr		r2, =AdLibReg
	strb	r1, [r2]
	bx		lr						// Return to caller

	//-------
	// AdLib data port (0x389) 
	// SoundBlaster FM data port (0x229)
	//	- fifoSendValue32(FIFO_SB, (AdLibReg<<8)|AL);
	// On input:
	//	r1 = byte to output
	//	lr = return address
	//------
out_229_389_FM_data:	
	ldr		r2, =AdLibReg
	strb	r1, [r2, #1]			// Save the data byte to AdLibData
	ldrb	r0, [r2]				// Get the AdLib register number into r0
	cmp		r0, #4
	beq		4f						// Handle register 4 (Timer Control) locally 
	blt		2f						// Go test for register values < 4
1:	orr		r0, r1, r0, lsl #8		// parameter = reg<<8|data
	b		PutAdLibBuffer			// Send the command to ARM7, return to our caller
	//-------
	// Test for register values 1..3:
	//	- Register 1 needs to be sent to ARM7
	//	- Registers 2 and 3 need to be handled locally
	//-------
2:	cmp		r0, #2					// Is it 2?
	blt		1b						// It is less than 2, send that to ARM7
	bgt		3f
	//-------
	// Register 2 (Timer 1 Data)
	//-------
#ifdef IOS
	ldr		r2, =AdLibTimer1
	strb	r1, [r2]				// Store the data byte to the highest byte of AdLibTimer1
	bx		lr						// Return to caller
	//-------
	// Register 3 (Timer 2 Data)
	//-------
3:	ldr		r2, =AdLibTimer2
	strb	r1, [r2]				// Store the data byte to the highest byte of AdLibTimer2
	bx		lr						// Return to caller
	//-------
	// Register 4 (Timer Control Byte)
	//-------
4:	ldr		r2, =AdLibStatus
	ldrb	r0, [r2]
	tst		r1, #0x80				// Is the "Reset the flags for timers 1 & 2" bit set?
	bicne	r0, #0xE0				// Yes, clear the timer flags from AdLibStatus
	strneb	r0, [r2]
#else
	strb	r1, [r2, #(AdLibTimer1-AdLibReg)]		// Store the data byte to the highest byte of AdLibTimer1
	bx		lr						// Return to caller
	//-------
	// Register 3 (Timer 2 Data)
	//-------
3:	strb	r1, [r2, #(AdLibTimer2-AdLibReg)]		// Store the data byte to the highest byte of AdLibTimer2
	bx		lr						// Return to caller
	//-------
	// Register 4 (Timer Control Byte)
	//-------
4:	ldrb	r0, [r2, #(AdLibStatus-AdLibReg)]
	tst		r1, #0x80				// Is the "Reset the flags for timers 1 & 2" bit set?
	bicne	r0, #0xE0				// Yes, clear the timer flags from AdLibStatus
	strneb	r0, [r2, #(AdLibStatus-AdLibReg)]
#endif
	bxne	lr
	//-------
	// Start/Stop timers
	//-------
	mov		r0, r1					// Parameter = input value
#ifdef IOS
	push	{r9, r12, lr}
	bl		_StartAdlibTimers		// Call the C routine in "timer.c"
	pop		{r9, r12, pc}
#else
	push	{r12, lr}
	bl		StartAdlibTimers		// Call the C routine in "timer.c"
	pop		{r12, pc}
#endif

	// ============= SOUNDBLASTER OUTPUT PORTS =========================

	//-------
	// 0x226 = SoundBlaster RESET port
	// On input:
	//	r1 = byte to output
	//	lr = return address
	//-------
out_226_SB_reset:
	tst		r1, #1					// if ((val&1)!=0) {
	beq		1f
	ldr		r2, =dsp_cmd
	ldr		r0, =sb_mode
	mov		r1, #0
	str		r1, [r2]				// sb.dsp.cmd = DSP_NO_COMMAND;
	str		r1, [r2, #4]			// sb.dsp.cmd_len = 0;
#ifdef IOS
	ldr		r2, =dsp_e2_count
	str		r1, [r2]
#else
	str		r1, [r2, #(dsp_e2_count-dsp_cmd)]
#endif
	str		r1, [r0]				// sb_mode = SB_IDLE (in case we were playing something)
	mov		r1, #0xAA
#ifdef IOS
	ldr		r2, =dsp_e2_value
	str		r1, [r2]
#else
	str		r1, [r2, #(dsp_e2_value-dsp_cmd)]
#endif
	ldr		r1, =dsp_in_data
	ldr		r2, =sb_mode
	mov		r0, #(256-45)			// Reset the SB speed to 22050Hz
	strb	r0, [r1]
	b		set_time_constant		// Returns to our caller
	
1:	DSP_FlushData_r2r1				//	DSP_FlushData();
	mov		r0, #0xAA
	DSP_AddData_r0					//	DSP_AddData(0xaa);
	bx		lr

	//-------
	// 0x22C = SoundBlaster DSP COMMAND port
	//	- fifoSendValue32(FIFO_SB, (1<<16)|AL);
	//
	//	static void DSP_DoWrite(Bit8u val) {
	//		switch (sb.dsp.cmd) {
	//			case DSP_NO_COMMAND:
	//				sb.dsp.cmd=val;
	//				sb.dsp.cmd_len=DSP_cmd_len_sb[val];
	//				sb.dsp.in.pos=0;
	//				if (!sb.dsp.cmd_len)
	//					DSP_DoCommand();
	//				break;
	//			default:
	//				sb.dsp.in.data[sb.dsp.in.pos]=val;
	//				sb.dsp.in.pos++;
	//				if (sb.dsp.in.pos>=sb.dsp.cmd_len)
	//					DSP_DoCommand();
	//		}
	//	}
	// On input:
	//	r1 = byte to output
	//	lr = return address
	//-------
out_22C_SB_DSP:
	ldr		r2,=dsp_cmd
	ldrb	r0, [r2]
	cmp		r0, #0					// Do we already have a command?
	bne		.DSP_put_data			// Yep, put the value into input buffer
	//-------
	// A new DSP command is starting.
	// Store the command byte to dsp_cmd, and
	// the parameter count to cmd_len.
	//-------
	ldr		r0, =DSP_cmd_len_sb
	strb	r1, [r2]				// sb.dsp.cmd=val;
	ldrb	r0, [r0, r1]			// r0 = DSP_cmd_len_sb[val]
	cmp		r0, #0					// Internal command with no parameters?
	strb	r0, [r2, #1]			// dsp_cmd_len = r0
	beq		.DSP_Do_Command			// Yep, go handle this command immediately.
	//-------
	// Start of a new command, handled by ARM7.
	//-------
	ands	r0, #0x7F				// dsp_cmd_len == 0 ?
	beq		.DSP_ARM7_Cmd_Done		// Yep, nothing more needs to be done for this command
	//-------
	// New command with parameters,
	// init the "dsp_in_pos" to 0 and return.
	//-------
	mov		r1, #0
	str		r1, [r2, #-(DSP_BUFSIZE+4)] // dsp_in_pos = 0
.DSP_Done:	
	bx		lr
	//-------
	// New parameter for current command, put it to buffer
	//-------
.DSP_put_data:
	sub		r2, #DSP_BUFSIZE		// r2 = dsp_in_data
	ldr		r0, [r2, #-4]			// r0 = dps_in_pos
	strb	r1, [r2, r0]			// sb.dsp.in.data[sb.dsp.in.pos]=val;
	add		r0, #1
	str		r0, [r2, #-4]			// dsp_in_pos++;
	ldrb	r0, [r2, #(DSP_BUFSIZE+1)] // r0 = dsp_cmd_len
	tst		r0, #0x80				// Do we need to send this data byte to ARM7 ?
	beq		3f						// Nope, so skip sending
	//-------
	// Command that needs to be handled in HandleSBCommand
	//-------
	ldr		r1, [r2, #-4]			// r1 = dsp_in_pos
	and		r0, #0x7F
	cmp		r1, r0					// Have we sent all bytes that belong to this command?
	bxlt	lr						// Not yet, all done for this byte.
	//-------
	// All bytes received, start handling the DSP command
	//-------
.DSP_ARM7_Cmd_Done:	
	send_DSP_to_ARM7				// Send this command byte to ARM7
	//-------
	// All handling for this command finished, reset the command state.
	//-------
.DSP_Cmd_Done:
	ldr		r2, =dsp_cmd
	mov		r0, #0
	strb	r0, [r2]				// dsp_cmd = NO_COMMAND;
	strb	r0, [r2, #1]			// dsp_cmd_len = 0;
#ifdef IOS
	ldr		r2, =dsp_in_pos
	str		r0, [r2]				// dsp_in_pos = 0;
#else
	str		r0, [r2, #(dsp_in_pos-dsp_cmd)] // dsp_in_pos = 0;
#endif
	bx		lr
	//-------
	// Check if we need to start handling the actual command
	//-------
3:	ldr		r1, [r2, #-4]			// r1 = dsp_in_pos
	and		r0, #0x7F
	cmp		r1, r0					// Have we got all bytes that belong to this command?
	bxlt	lr						// Not yet, all done for this byte.
	//-------
	// Handle a command locally.
	// Now "dsp_cmd" contains the command byte, and
	// "dsp_in_data" the possible parameters for the command.
	//-------
.DSP_Do_Command:
	ldr		r2, =dsp_cmd
	ldrb	r0, [r2]				// r0 = dsp_cmd
	//-------
	// Registers now:
	//	r0 = the first byte of the DSP command
	//	r1 = The currently received byte of the DSP command
	//	r2 = address of the dsp_cmd save
	//-------
//	cmp		r0, #0x10				// Is the command "Direct DAC"?
//	beq		.DSP_10_DAC				// Yep, go handle it
	cmp		r0, #0xE0				// Is the command "DSP identification"?
	beq		.DSP_E0_ident			// Yep, go handle it
	cmp		r0, #0xE1				// Is the command "Get DSP Version Number"?
	beq		.DSP_E1_version			// Yep, go handle it
	cmp		r0, #0xE2				// Is the command "Weird DMA identification write routine"?
	beq		.DSP_E2_weird			// Yep, go handle it
	cmp		r0, #0xE4				// Is the command "Write Test Register"?
	beq		.DSP_E4_test			// Yep, go handle it
	cmp		r0, #0xE8				// Is the command "Read Test Register"?
	beq		.DSP_E8_test			// Yep, go handle it
	cmp		r0, #0xF2				// Is the command "IRQ Request, 8-bit"?
	beq		.DSP_F2_IRQ				// Yep, go handle it
	cmp		r0, #0x24				// Is the command "DMA ADC, 8-bit"?
	beq		.DSP_24_record			// Yep, go handle it
	cmp		r0, #0					// CMD 00 ??
	beq		.DSP_Cmd_Done			// Yep, ignore it
	cmp		r0, #0xE7				// CMD E7 (BC Racers)??
	beq		.DSP_Cmd_Done			// Yep, ignore it
	//-------
	// Unsupported SB command! Clear it.
	//-------
	mov		r1, #0
	strb	r1, [r2, #1]			// dsp_cmd_len = 0;
#ifdef IOS
	ldr		r0, =dsp_in_pos
	str		r1, [r0]				// dsp_in_pos = 0;
#else
	str		r1, [r2, #(dsp_in_pos-dsp_cmd)] // dsp_in_pos = 0;
#endif
	ldrb	r0, [r2]				// r0 = the unsupported SB Cmd value
	strb	r1, [r2]				// dsp_cmd = NO_COMMAND; 
	//-------
	// Store the Hex Cmd value to the BRUnsSBCmd string
	//-------
	ldr		r1, =BRUnsSBCmd
	mov		r2, r0, lsr #4
	add		r2, #48					// '0'
	cmp		r2, #57					// '9'
	addgt	r2, #(65-57-1)			// ('A'-'9'-1)
	strb	r2, [r1, #27]			// High Hex nibble of the SB DSP Cmd value
	and		r0, #0x0F
	add		r0, #48					// '0'
	cmp		r0, #57					// '9'
	addgt	r0, #(65-57-1)			// ('A'-'9'-1)
	strb	r0, [r1, #28]			// Low Hex nibble of the SB DSP Cmd value
	//-------
	// Go to debugger, telling the break reason.
	//-------
	pop		{r0,lr}
	msr		cpsr_f,r0				// Set the processor flags
	ldr		r0, =BreakReason
	str		r1, [r0]				// ... tell we break because of an unsupported port I/O, ...
	b		debug_trap_false		// Stop and go to the debugger

	//------
	// 0E0h		DSP Identification                                      SB2.0
    // 	 COMMAND->DATA <-NOT(DATA)
	//
    // 	 DESCRIPTION
    // 	  Returns bitwise NOT of data byte.
	//
    // 	 NOTES
    //	  * Results reliable only after DSP reset on early models.
	//------
.DSP_E0_ident:
	DSP_FlushData_r2r1
	ldr		r2, =dsp_in_data
	ldrb	r0, [r2]
	mvn		r0, r0
	DSP_AddData_r0					// DSP_AddData(~sb.dsp.in.data[0]);
	b		.DSP_Cmd_Done

	//------
	// 0E1h       DSP Version                                             SB
	//    COMMAND <-MAJORVERSIONBYTE<-MINORVERSIONBYTE
	//
	//    DESCRIPTION
	//     Determines DSP major and minor version.
	//
	//    MODEL                      VERSION
	//     SoundBlaster 1.0           1.xx  (1.05)
	//     SoundBlaster 1.5           1.xx  (1.05)
	//     SoundBlaster 2.0           2.xx  (2.01)
	//     SoundBlaster Pro           3.00  (?)
	//     SoundBlaster Pro 2         3.01+ (3.01, 3.02)
	//     SoundBlaster 16            4.0x  (4.04, 4.05)
	//     SoundBlaster 16 SCSI-2     4.11  (4.11)
	//     SoundBlaster AWE32         4.12+ (4.12)
	//
	//    NOTES
	//     * Ensure that no other card is mapped at the same port address by
	//         performing this command twice, checking for a consistent result.
	//     * BUG: Some SB1.x clones errantly return version 2.00.
	//     * BUG: Some SB16 SCSI-2s experience daughterboard communication errors.
	//------
.DSP_E1_version:
	DSP_FlushData_r2r1
	mov		r0, #2
	DSP_AddData_r0					// DSP_AddData(0x2);
	mov		r0, #1
	DSP_AddData_r0					// DSP_AddData(0x1);
	b		.DSP_Cmd_Done

	//------
	// 0E2h		Weird DMA identification write routine
	//
	//	 No info for this command, besides the DOSBox sources:
	//
	//		for (Bitu i = 0; i < 8; i++)
	//			if ((sb.dsp.in.data[0] >> i) & 0x01) sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	//		 sb.e2.value += E2_incr_table[sb.e2.count % 4][8];
	//		 sb.e2.count++;
	//		 GetDMAChannel(sb.hw.dma8)->Register_Callback(DSP_E2_DMA_CallBack);
	//		if (event==DMA_UNMASKED) {
	//			Bit8u val=sb.e2.value;
	//			DmaChannel * chan=GetDMAChannel(sb.hw.dma8);
	//			chan->Register_Callback(0);
	//			chan->Write(1,&val);
	//		}
	//------
.DSP_E2_weird:
	push	{r3}
	ldr		r2, =dsp_in_data
	ldrb	r0, [r2]
#ifdef IOS
	ldr		r2, =dsp_e2_count
	ldr		r1, [r2]								// r1 = dsp_e2_count value
#else
	ldr		r1, [r2, #(dsp_e2_count-dsp_in_data)]	// r1 = dsp_e2_count value
#endif
	and		r1, #3
	add		r1, r1, lsl #3							// r1 = 9*dsp_e2_count = table row
	ldr		r3, =E2_incr_table
	add		r3, r1, lsl #2							// r3 = &(E2_incr_table[sb.e2.count % 4])
	ldr		r2, =dsp_e2_value
	ldr		r2, [r2]								// r2 = dsp_e2_value
	//-------
	// Registers now:
	// r0 = sb.dsp.in.data[0]
	// r1 = free
	// r2 = sb.e2.value
	// r3 = &(E2_incr_table[sb.e2.count % 4])
	//-------
	// ----- i = 0
	tst		r0, #1									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 1
	tst		r0, #2									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 2
	tst		r0, #4									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 3
	tst		r0, #8									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 4
	tst		r0, #16									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 5
	tst		r0, #32									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 6
	tst		r0, #64									// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- i = 7
	tst		r0, #128								// if ((sb.dsp.in.data[0] >> i) & 0x01)
	ldrne	r1, [r3]
	addne	r2, r1									//	sb.e2.value += E2_incr_table[sb.e2.count % 4][i];
	add		r3, #4
	// ----- 
	ldr		r1, [r3]
	add		r1, r2									// sb.e2.value += E2_incr_table[sb.e2.count % 4][8];
	ldr		r2, =dsp_e2_value
	str		r1, [r2]
	// -----
	ldr		r0, [r2, #4]
	add		r0, #1									// sb.e2.count++;
	str		r0, [r2, #4]
	//-------
	// Write the calculated byte to DMAAddress location
	// Increment the DMAAddress after the write(?)
	//-------
	ldr		r2, =DMAAddress
	ldr		r3, =INTVectors
	ldr		r0, [r2]
	ldr		r3, [r3]
	add		r3, r0									// r3 = physical address where to write the calculated byte
	add		r0, #1
	strb	r1, [r3]								// Write the calculated byte to the DMAAddress location in RAM
	str		r0, [r2]								// Increment DMAAddress, prepare for the possible next byte
	pop		{r3}
	b		.DSP_Cmd_Done

	//------
	// 0E4h       Write Test Register                                     SB2.0
	//    COMMAND->TESTBYTE
	//
	//    DESCRIPTION
	//     Writes diagnostic register.
	//
	//    NOTES
	//     * DSP reset does not clear the test register.
	//------
.DSP_E4_test:
	ldr		r2, =dsp_in_data
	ldrb	r0, [r2]
#ifdef IOS
	ldr		r2, =dsp_test_reg
	strb	r0, [r2]								// sb.dsp.test_register=sb.dsp.in.data[0];
#else
	strb	r0, [r2, #(dsp_test_reg-dsp_in_data)]	// sb.dsp.test_register=sb.dsp.in.data[0];
#endif
	b		.DSP_Cmd_Done

	//------
	// 0E8h       Read Test Register                                      SB2.0
	//    COMMAND <-TESTBYTE
	//
	//    DESCRIPTION
	//     Reads diagnostic register.
	//
	//    NOTES
	//     * DSP reset does not clear the test register.
	//------
.DSP_E8_test:
	DSP_FlushData_r2r1
	ldr		r2, =dsp_test_reg
	ldrb	r0, [r2]
	DSP_AddData_r0					// DSP_AddData(dsp.test_reg);
	b		.DSP_Cmd_Done

	//------
	// 024h       DMA ADC, 8-bit                                          SB
	//	COMMAND->LENGTHLOBYTE->LENGTHHIBYTE
	//
	//	DESCRIPTION
	//	 Initiates 8-bit DMA transfer (record).
	//
	//	PROCEDURE
	//	  a) Install IRQ handler (hook vector, update PIC mask)
	//	  b) Perform Set Time Constant command (040h), or otherwise set sample rate
	//	  c) Perform Enable Speaker command (0D1h)
	//	  d) Setup DMA controller (mode = 044h + channel)
	//	  e) Perform DMA ADC, 8-bit command (024h)
	//	  f) IRQ: Acknowledge IRQ (input from IRQ Acknowledge, 8-bit port - 02x0Eh;
	//							   perform Generic EOI (020h) to appropriate PICs)
	//	  g) Perform Disable Speaker command (0D3h)
	//
	//	LENGTH = SAMPLES - 1
	//------
.DSP_24_record:
	//-------
	// Get r1 = the length value
	//-------
	ldr		r2, =dsp_in_data
	ldrh	r1, [r2]
	//-------
	// Get the DMAAddress pointer
	//-------
	ldr		r2, =DMAAddress
	ldr		r0, =INTVectors
	ldr		r2, [r2]
	ldr		r0, [r0]
	add		r1, #1
	add		r2, r0									// r2 = physical address where to write the recorded bytes
	//-------
	// Loop writing the recorded samples
	//-------
	mov		r0, #0x80
1:	strb	r0, [r2], #1							// Write the calculated byte to the DMAAddress location in RAM
	subs	r1, #1
	bne		1b
	//------
	// Flow-thru to sending an IRQ
	//------
	//------
	// 0F2h       IRQ Request, 8-bit                                      SB
	//    COMMAND
	//
	//    DESCRIPTION
	//     Triggers 8-bit interrupt.
	//
	//    PROCEDURE
	//      a) Install IRQ handler (hook vector, update PIC mask)
	//      b) Perform IRQ Request, 8-bit command (0F2h)
	//      c) IRQ: Acknowledge IRQ (input from IRQ Acknowledge, 8-bit port - 02x0Eh;
	//                               perform Generic EOI (020h) to appropriate PICs)
	//------
.DSP_F2_IRQ:
#ifdef IOS
	push	{r3, r9, r12, lr}
#else
	push	{r3, r12, lr}
#endif
	mov		r0, #0x10000			// This is a real SB IRQ init
#if defined(RPi) || defined(Roku)
	bl		SendSBIRQ
#else
	bl		SBIRQ
#endif
#ifdef IOS
	pop		{r3, r9, r12, lr}
#else
	pop		{r3, r12, lr}
#endif
	b		.DSP_Cmd_Done

	// ============= SOUNDBLASTER & ADLIB DATA AREA ====================

	.data
	.align 2
	
	// Number of bytes in input for commands, high bit set if need to send to ARM7
DSP_cmd_len_sb:
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0  // 0x00
	.byte	0x81,0,0,0, 0x82,2,0x82,0x82, 0,0,0,0, 0x80,0,0,0	// 0x10 Wari hack (14, 16, 17, 1c to ARM7)
	.byte	0,0,0,0, 2,0,0,0, 0,0,0,0, 0,0,0,0	// 0x20
	.byte	0,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0	// 0x30
	.byte	0x81,2,2,0, 0,0,0,0, 0x82,0,0,0, 0,0,0,0	// 0x40 (40, 48 to ARM7)
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0x50
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0x60
	.byte	0,0,0,0, 0x82,0x82,0x82,0x82, 0,0,0,0, 0,0,0,0	// 0x70
	.byte	0x82,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0x80
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0x90
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0xa0
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0xb0
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0xc0
	.byte	0x80,0x80,0,0x80, 0x80,0,0,0, 0,0,0x80,0, 0,0,0,0	// 0xd0 (d0, d1, d3, d4, da to ARM7)
	.byte	1,0,1,0, 1,0,0,0, 0,0,0,0, 0,0,0,0	// 0xe0
	.byte	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0	// 0xf0
	
E2_incr_table:	// indexed as [4][9]
	.word	0x01, -0x02, -0x04,  0x08, -0x10,  0x20,  0x40, -0x80, -106		// index [0, 0..8]
	.word	-0x01,  0x02, -0x04,  0x08,  0x10, -0x20,  0x40, -0x80,  165	// index [1, 0..8]
	.word	-0x01,  0x02,  0x04, -0x08,  0x10, -0x20, -0x40,  0x80, -151	// index [2, 0..8]
	.word	0x01, -0x02,  0x04, -0x08, -0x10,  0x20, -0x40,  0x80,   90		// index [3, 0..8]

BRUnsSBCmd:
	.ascii	"Unsupported SB DSP command XX!"
	.byte	0x0a, 0
	
	.data
	.align	2

	.global	sb_cmd_serialize_start			// For serialization!
sb_cmd_serialize_start:

dsp_write_busy:
	.space	4
dsp_in_used:
	.space	4
dsp_in_pos:
	.space	4
	.global	dsp_in_data
dsp_in_data:
	.space	DSP_BUFSIZE
	.global	dsp_cmd
dsp_cmd:
	.space	1
	.global	dsp_cmd_len
dsp_cmd_len:
	.space	1
dsp_test_reg:
	.space	1

	.align	2
dsp_out_last_data:
	.space	4
dsp_out_used:
	.space	4
dsp_out_pos:
	.space	4
dsp_out_data:
	.space	DSP_BUFSIZE
dsp_e2_value:
	.space	4
dsp_e2_count:
	.space	4
dsp_dac_sent:
	.space	4
dsp_dac_curr:
	.space	4

	.global	AdLibTimer1
AdLibTimer1:							// Timer 1 increments at 12500 Hz
	.space	1
	.global	AdLibTimer2
AdLibTimer2:							// Timer 2 increments at 3125 Hz
	.space	1
	.global	AdLibStatus
AdLibStatus:							// High 3 bits = Status returned by IN AL,388, low 2 bits = timer control byte
	.space	1
AdLibReg:
	.space	1
AdLibData:
	.space	1
	.global	AdLibMasked
AdLibMasked:							// Timer masked bits
	.space	1

	.global	sb_irq_ack
sb_irq_ack:
	.space	1
	
	.global	sb_cmd_serialize_end			// For serialization!
sb_cmd_serialize_end:
