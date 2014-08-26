;=============================================================================
; adlib.S
;
; This file contains routines to handle mono AdLib (OPL) 9-channel FM audio
; synthesis. The routines are heavily optimized so that running the emulation
; at 32kHz only consumes around 16MHz worth of ARMv5 processor CPU time.
; Note that changing the output frequency requires changing also the
; precalculated tl_tab.S and fn_tab.S values.
;
; This file is part of the x86 emulation core written in ARM Assembly, originally
; from the DSx86 Nintendo DS DOS Emulator. See http:;dsx86.patrickaalto.com
;
; Copyright (c) 2009-2013 Patrick "Pate" Aalto
;	
; Redistribution and use in source or binary form, with or without modifications,
; is NOT permitted without specific prior written permission from the author.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;=============================================================================

	AREA 	adlib_data, DATA, READWRITE
	ALIGN	4

	EXTERN	SBEmulator
	EXTERN	sin_tab
	EXTERN	tl_tab
	EXTERN	fn_tab

;========== AdLib emulation handling variables ========================

ADLIB_BUFFER_SAMPLES	EQU		(128)
ADLIB_BUFFER_SIZE		EQU		(2*ADLIB_BUFFER_SAMPLES)
R12_BUF_SHIFT			EQU		22

CPUCHECK				EQU		0
USEBUFFER				EQU		1
TL_TAB_VOL_ADJUST		EQU		0
SKIP_SILENT				EQU		1

	GLOBAL	adlib_serialize_start			; For serialization!
adlib_serialize_start

	IF USEBUFFER = 1
CMND_BUF_INCR			EQU		0x00400000
CMND_BUF_SHIFT			EQU		(22-2)
	GLOBAL	cmnd_buf
cmnd_buf									; Buffer for new AdLib commands
	SPACE	(4*1024)
	GLOBAL cmnd_buf_head
cmnd_buf_head								; Command buffer head (MUST BE IMMEDIATELY AFTER cmnd_buf!)
	SPACE	4
cmnd_buf_keycmnd							; Command buffer key command head
	SPACE	4
	GLOBAL	cmnd_buf_tail
cmnd_buf_tail								; Command buffer tail
	SPACE	4
cmnd_tick									; Command buffer tick compare value
	SPACE	4
	ENDIF	

	; ===== Main FM_OPL struct

	; ----- LFO handling, 8.24 fixed point (LFO calculations)
	
LFO_AM_TAB_ELEMENTS 	EQU		210
LFO_SH					EQU		24

lfo_pm_tbl
	SPACE	4	
lfo_am_cnt								; UINT32
	SPACE	4
lfo_pm_cnt								; UINT32	
	SPACE	4
	
lfo_am_depth							; UINT8	
	SPACE	1
lfo_pm_depth_range						; UINT8	
	SPACE	1
wavesel									; UINT8						/* waveform select enable flag	*/
	SPACE	1
rhythm									; UINT8						/* Rhythm mode					*/
	SPACE	1

	; ----- Two SLOTs for each of the 9 channels

TL_RES_LEN				EQU		(256)
TL_TAB_LEN 				EQU		(12*2*TL_RES_LEN)
ENV_QUIET				EQU		(TL_TAB_LEN>>4)
FREQ_SH					EQU		16
FREQ_MASK				EQU		((1<<FREQ_SH)-1)
SIN_BITS				EQU		10
SIN_LEN					EQU		(1<<SIN_BITS)
SIN_MASK				EQU		(SIN_LEN-1)

MAX_ATT_INDEX			EQU		512
MIN_ATT_INDEX			EQU		0

OP_PM_BIT				EQU		4
OP_AM_BIT				EQU		8
OP_CON_BIT				EQU		16
R12_AM_DEPTH_BIT		EQU		1
R12_AM_DIR_BIT			EQU		2
R12_RHYTHM_BIT			EQU		4

	; ----- Slot 1 -----
	GLOBAL SLOT1
SLOT1	
ch0_slot1_env_sustain					; UINT32;			/* r4 = envelope sustain level  */
	SPACE	4
ch0_slot1_env_incr						; UINT32;			/* r5 = envelope increment 		*/
	SPACE	4
ch0_slot1_Incr							; UINT32;			/* r6 = frequency counter step	*/
	SPACE	4
ch0_slot1_Cnt							; UINT32 Cnt;		/* r7 = frequency counter		*/
	SPACE	4
ch0_slot1_volume						; INT32	volume;		/* r8 = envelope counter		*/	
	SPACE	4
ch0_slot1_TLL							; INT32	TLL;		/* r9 = adjusted now TL			*/
	SPACE	4
	GLOBAL ch0_slot1_wavetable			; Needed for serialization
ch0_slot1_wavetable						; unsigned int wavetable;	/* r10= wavetable		*/
	SPACE	4
ch0_slot1_op1_out						; INT32 op1_out[2];	/* slot1 output for feedback	*/
ch0_block_fnum							; UINT32 block_fnum;/* block+fnum					*/
	SPACE	4
ch0_fc									; UINT32 fc;		/* Freq. Increment base			*/
	SPACE	4
ch0_ksl_base							; UINT32 ksl_base;	/* KeyScaleLevel Base step		*/
	SPACE	4
ch0_slot1_mul							; UINT8	mul;		/* multiple: mul_tab[ML]		*/
	SPACE	1
ch0_slot1_FB							; UINT8 FB;			/* feedback shift value			*/
	SPACE	1
ch0_slot1_bits							; UNIT8 bits;		/* AM, Vib, EG type, KSR 		*/
	SPACE	1
ch0_slot1_dummy
	SPACE	1
ch0_slot1_env_ar_incr					; UINT32;			/* current envelope attack incr	*/
	SPACE	4
ch0_slot1_env_dr_incr					; UINT32;			/* current envelope decay incr	*/
	SPACE	4
ch0_slot1_env_rr_incr					; UINT32;			/* current envelope release incr */
	SPACE	4
ch0_slot1_TL							; UINT32 TL;		/* total level: TL << 2			*/
	SPACE	4
ch0_slot1_ksl							; UINT8	ksl;		/* keyscale level				*/
	SPACE	1
ch0_slot1_ksr							; UINT8	ksr;		/* key scale rate: kcode>>KSR	*/
	SPACE	1
ch0_kcode								; UINT8 kcode;		/* key code (for key scaling)	*/
	SPACE	1
ch0_slot1_key							; UINT8	key;		/* 0 = KEY OFF, >0 = KEY ON		*/
	SPACE	1
ch0_slot1_ar							; UINT32 ar;		/* attack rate: AR<<2			*/
	SPACE	4
ch0_slot1_dr							; UINT32 dr;		/* decay rate:  DR<<2			*/
	SPACE	4
ch0_slot1_rr							; UINT32 rr;		/* release rate:RR<<2			*/
	SPACE	4
ch0_slot1_sl							; UINT32;			/* sustain level: sl_tab[SL]	*/
	SPACE	4
	
	; ----- Slot 2 (first 3 words are common to the whole channel) -----
	GLOBAL SLOT2
SLOT2	
SLOT_SIZE		EQU		(SLOT2-SLOT1)
	SPACE	SLOT_SIZE
CH_SIZE			EQU		(2*SLOT_SIZE)
	SPACE	8*(CH_SIZE)					; Space for 8 additional channels

SLOT_BITS				EQU		(ch0_slot1_bits-SLOT1)
SLOT2_BITS				EQU		(SLOT_SIZE+(ch0_slot1_bits-SLOT1))
SLOT_VOLUME				EQU		(ch0_slot1_volume-SLOT1)
SLOT_TLL				EQU		(ch0_slot1_TLL-SLOT1)
SLOT_WAVETABLE			EQU		(ch0_slot1_wavetable-SLOT1)
SLOT_CNT				EQU		(ch0_slot1_Cnt-SLOT1)
SLOT_INCR				EQU		(ch0_slot1_Incr-SLOT1)
SLOT_ENV_INCR			EQU		(ch0_slot1_env_incr-SLOT1)
SLOT_ENV_SUST			EQU		(ch0_slot1_env_sustain-SLOT1)
SLOT_MUL				EQU		(ch0_slot1_mul-SLOT1)
SLOT_OP1_OUT			EQU		(ch0_slot1_op1_out-SLOT1)
SLOT_KEY				EQU		(ch0_slot1_key-SLOT1)

SLOT1_BLOCK_FNUM		EQU		(SLOT_SIZE+(ch0_block_fnum-SLOT1))
SLOT2_BLOCK_FNUM		EQU		(ch0_block_fnum-SLOT1)
SLOT1_KSL_BASE			EQU		(SLOT_SIZE+(ch0_ksl_base-SLOT1))
SLOT2_KSL_BASE			EQU		(ch0_ksl_base-SLOT1)
SLOT1_FC				EQU		(SLOT_SIZE+(ch0_fc-SLOT1))
SLOT2_FC				EQU		(ch0_fc-SLOT1)
SLOT1_KCODE				EQU		(SLOT_SIZE+(ch0_kcode-SLOT1))
SLOT2_KCODE				EQU		(ch0_kcode-SLOT1)

	GLOBAL	adlib_serialize_end			; For serialization!
adlib_serialize_end

	;====================================

	AREA 	adlib, CODE, READONLY
	ALIGN	4

	;=======
	; Buffer new AdLib commands
	; Input: r0 = reg << 8 | value
	;=======
;-------------------------------------------------------------------------------------------------------------
	GLOBAL	PutAdLibBuffer
PutAdLibBuffer
;-------------------------------------------------------------------------------------------------------------
	IF USEBUFFER = 1
	ldr		r2, =cmnd_buf_head
	push	{r12, lr}
	ldr		r3, [r2]											; r3 = cmnd_buf_head value
	sub		r12, r2, #(cmnd_buf_head-cmnd_buf)					; r12 = cmnd_buf address
	add		r1, r3, #CMND_BUF_INCR
	lsr		r3, #CMND_BUF_SHIFT
	str		r0, [r12, r3]										; Write the value to the command buffer
	str		r1, [r2]											; Write new cmnd_buf_head value
	and		r0, #0xF000
	cmp		r0, #0xB000
	streq	r1, [r2, #(cmnd_buf_keycmnd-cmnd_buf_head)]			; If the command was B0..B8, make cmnd_buf_keycmnd = cmnd_buf_head
	pop		{r12, pc}
	ELSE
	push	{r4-r11}											; Push registers we are going to change
	ldr		r4, =cmnd_jump
	and		r1, r0, #0xE000										; switch(r&0xe0)
	ldr		pc, [r4, r1, lsr #11]								; Jump to the handler

	AREA 	jumptables, DATA, READONLY
	ALIGN	4

cmnd_jump
	DCD		cmnd_loop
	DCD		cmnd_20_30
	DCD		cmnd_40_50
	DCD		cmnd_60_70
	DCD		cmnd_80_90
	DCD		cmnd_A0_B0
	DCD		cmnd_C0_D0
	DCD		cmnd_E0_F0

	AREA 	adlib, CODE, READONLY
	ALIGN	4

cmnd_loop
	pop		{r4-r11}											; Pop changed registers
	ENDIF	
	bx		lr

	;=======
	; Handle AdLib emulation
	;=======
	
	MACRO
	LFO_AM_HANDLING
	;-------
	;	tmp = lfo_am_table[ OPL->lfo_am_cnt >> LFO_SH ];
	;	if (OPL->lfo_am_depth)
	;		LFO_AM = tmp;
	;	else
	;		LFO_AM = tmp>>2;
	;-------
	and		r1, r12, #0xFF										; r1 = LFO_AM and lfo_am_depth
	mov		r2, #3
	tst		r1, #R12_AM_DEPTH_BIT								; if (OPL->lfo_am_depth)
	moveq	r2, #5
	tst		r12, #(OP_AM_BIT<<8)								; AM bit set?
	movne	r1, r1, lsr r2
	addne	r9, r1												; Now r9 = SLOT volume base (0..511)
	MEND

	MACRO
	LFO_PM_HANDLING $fnum
	tst		r12, #(OP_PM_BIT<<8)								; if(op->vib)
	beq		%f1													; {
	ldr		r1, [r0, $fnum]										;	unsigned int block_fnum = CH->block_fnum;
	ldr		r3, [sp, #ADLIB_BUFFER_SIZE]						;	r3 = &lfo_pm_table[LFO_PM]
	and		r2, r1, #0x380										;	unsigned int fnum_lfo   = (block_fnum&0x0380) >> 7;
	lsr		r2, #(7-4)
	ldrsb	r2, [r3, r2]										;	signed int lfo_fn_table_index_offset = lfo_pm_table[LFO_PM + 16*fnum_lfo ];
	cmp		r2, #0												;	if (lfo_fn_table_index_offset)	/* LFO phase modulation active */
	beq		%f1													;	{
	add		r1, r2												;		block_fnum += lfo_fn_table_index_offset;
	and		r2, r1, #0x1C00
	lsr		r2, #10												;		block = (block_fnum&0x1c00) >> 10;
	ldr		r3, =fn_tab
	and		r1, r4, lsr #16										;		r1 = block_fnum&0x03ff (r4 high 16 bits contain 0x3FF)
	rsb		r2, #7												;		r2 = 7-block
	ldr		r1, [r3, r1, lsl #2]								;		r1 = OPL->fn_tab[block_fnum&0x03ff]
	ldrb	r3, [r0, #SLOT_MUL] 								; 		r3 = op->mul
	lsr		r1, r2												;		r1 = (OPL->fn_tab[block_fnum&0x03ff] >> (7-block))
	mul		r6, r1, r3											;		tmp = (OPL->fn_tab[block_fnum&0x03ff] >> (7-block)) * op->mul;
1																; } }
	MEND
	
	MACRO
	SpeakerFrequencyChange
	ldr		r5, =DOSRAMAddress
	ldr		r5, [r5]
	ldr		r6, =0x040004B8										; SCHANNEL_TIMER(11)
	sub		r5, #0x10000
	ldrh	r7, [r5, #-8]										; r7 = requested speaker frequency
	strh	r7, [r6]											; SCHANNEL_TIMER(11) = requested speaker frequency
	MEND

;-------------------------------------------------------------------------------------------------------------
	GLOBAL	AdlibInit
AdlibInit
;-------------------------------------------------------------------------------------------------------------
	push	{r4-r11}
	;-------
	; Set the LFO initial values
	;-------
	ldr		r0,=lfo_am_cnt
	mov		r1, #0												; lfo_am_cnt = 0, lfo_am_depth = False, lfo_am_direction = up
	str		r1, [r0] 
	str		r1, [r0, #(lfo_pm_cnt-lfo_am_cnt)] 					; lfo_pm_cnt = 0

	ldr		r1, =lfo_pm_table
	str		r1, [r0, #(lfo_pm_tbl-lfo_am_cnt)]					; Save the lfo_pm_table initial value

	;-------
	; Set the SLOT initial values
	;-------
	ldr		r0,=SLOT1
	mov		r1, #0
	mov		r2, #(MAX_ATT_INDEX<<16)
	mov		r3, #1
	mov		r4, #0x40
	ldr		r5, =sin_tab
	mvn		r6, #0xFC00
	lsl		r6, #16
	add		r6, #MAX_ATT_INDEX									; r6 = (1023<<16)|MAX_ATT_INDEX
	mov		r10, #0
reset_slot_loop
	str		r1, [r0]											; op->op1_out[0] = 0, op->op1_out[1] = 0
	str		r5, [r0, #SLOT_WAVETABLE]							; op->wavetable = sin_tab;
	str		r1, [r0, #(ch0_slot1_FB-SLOT1)]						; op->state = 0;
	str		r1, [r0, #SLOT_ENV_INCR]							; op->env_incr = 0;
	str		r2, [r0, #SLOT_VOLUME]								; op->volume = MAX_ATT_INDEX;
	str		r6, [r0, #SLOT_ENV_SUST]							; current sustain level = MAX_ATT_INDEX;
	strb	r3, [r0, #SLOT_MUL]									; op->mul = 1
	str		r4, [r0, #(ch0_slot1_TL-SLOT1)]						; op->TL = 0x40
	add		r0, #SLOT_SIZE
	add		r10, #1
	cmp		r10, #(2*9)
	blo		reset_slot_loop

	IF 0 = 1
	;-------
	; Output a sine tone immediately
	;-------
	ldr		r0,=SLOT1
	add		r0, #SLOT_SIZE						; r0 = slot 2
	mov		r1, #0
	mvn		r2, #0xFC00
	lsl		r2, #16
	str		r1, [r0, #SLOT_VOLUME]				; op->volume = MAX;
	str		r2, [r0, #SLOT_ENV_SUST]			; current sustain level = MAX;
	str		r1, [r0, #(ch0_slot1_TL-SLOT1)]		; op->TL = 0
	mov		r2, #(1024<<10)
	str		r2, [r0, #SLOT_INCR]				; r6 = SLOT1 Incr value
	ENDIF

	;-------
	; Reset the command buffer values
	;-------
	IF USEBUFFER = 1
	ldr		r2, =cmnd_buf_head
	str		r1, [r2]
	str		r1, [r2, #4]
	str		r1, [r2, #8]
	ENDIF	

	pop		{r4-r11}
	bx		lr

	MACRO
	CALC_ENVELOPE $op
	adds	r8, r5								; Adjust the volume by the envelope increment
	bmi		decay_$op							; Go to decay if we went over max volume
	rsbscc	r1, r8, r4, lsl #16					; Did we go under the SUSTAIN level?
	bcc		sustain_$op							; Yep, go adjust the volume
env_done_$op									; Continue
	MEND	
	
;-------------------------------------------------------------------------------------------------------------
	GLOBAL	AdlibRun
AdlibRun
;-------------------------------------------------------------------------------------------------------------
	push	{r4-r11, lr}
	mov		lr, r0								; lr = output buffer pointer
	ldr		r0, =lfo_pm_tbl
	ldr		r0, [r0]
	push	{r0}								; Save the lfo_pm_table address to stack
	ldr		r0, =cmnd_tick
	str		r1, [r0]							; Save second parameter = tick limit value
	;-------
	; Reserve the feedback buffer and clear the output buffer
	;-------
	sub		sp, #ADLIB_BUFFER_SIZE
	mov		r0, lr
	mov		r1, #0
	mov		r2, #0
	mov		r3, #0
	mov		r4, #0
	mov		r5, #0
	mov		r6, #0
	mov		r7, #0
	mov		r8, #0
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	stmia	r0!, {r1-r8}						; Clear 4*8 bytes = 32 bytes = 16 samples
	;-------
	; Register legend for slot calculation
	; r0  = SLOT data pointer
	; r1  =
	; r2  =
	; r3  = op1_out[0] (low 16 bits) and op1_out[1] (high 16 bits)
	; r4  = (1023<<16) | SLOT1->sustain level (or MAX_ATT_INDEX if envelope is in RELEASE or OFF mode)
	; r5  = SLOT->env_incr (negative if in attack mode)
	; r6  = SLOT->Incr (sound frequency increment)
	; r7  = SLOT->Cnt (sound frequency counter)
	; r8  = SLOT->volume << 16
	; r9  = SLOT volume base = ((SLOT1)->TLL + (LFO_AM & (SLOT1)->AMmask));
	; r10 = SLOT->wavetable
	; r11 = tl_tab
	; r12 = Bitmapped
	;		+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+---+---+---+---+---+---+---+---+---+---+
	;		| 31 | 30 | 29 | 28 | 27 | 26 | 25 | 24 | 23 | 22 | 21 | 20 | 19 | 18 | 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
	;		+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+---+---+---+---+---+---+---+---+---+---+
	;		|                  Buffer index (255..0)                    |   Channel index   |      SLOT-specific bit values       |   LFO AM value    |Rhy|AM |AM |
	;		|                                                           |      (0..8)       | Feedback Lvl |Con | AM |Vib |EGt|KSR|     (0...27)      |thm|dir|dep|
	;		+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+---+---+---+---+---+---+---+---+---+---+
	; lr  = Output Buffer Pointer
	;-------
	ldr		r0,  =SLOT1
	ldr		r12, [r0, #-(SLOT1-lfo_am_cnt)]
	ldr		r11, =tl_tab
	and		r12, #0xFF												; r12 = LFO_AM and lfo_am_depth
	
for_channel
	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	ldmia	r0, {r4-r10}										; r4..r10 = slot-specific data values

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte

	;-------
	; Amplitude modulation handling.
	; Output = r9 value adjusted.
	;-------
	LFO_AM_HANDLING

	;-------
	; Frequency modulation (vibrato) handling.
	; Output = r6 value adjusted.
	;-------
	LFO_PM_HANDLING #SLOT1_BLOCK_FNUM

	ldr		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value

	;-------
	; SLOT1 buffer fill loop handling
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

	IF SKIP_SILENT = 1
	;-------
	; Special code if the slot is silent
	;-------
	cmp		r8, r4, lsl #16										; Volume == silent ...
	ldrbeq	r1, [r0, #SLOT_KEY]
	cmpeq	r1, #0												; .. and not KEY_ON?
	beq		for_SLOT1_silent									; Yes, so handle it specially
	ENDIF
	
	;-------
	; Use different sample loops for SLOT1 depending on whether feedback is active
	;-------
	tst		r12, #(7<<(8+5))									; Feedback = 0?
	beq		for_SLOT1_no_FB										; Yep, use the no-feedback loop

	;=======
	; SLOT1 buffer fill loop when feedback is active
	; Output goes always to the feedback buffer,
	; SLOT2 uses it as phase_modulation or direct output depending on connect
	;
	;	FREQ_SH = 16
	;	FREQ_MASK = 65535
	;	SIN_MASK = 1023
	;	ENV_QUIET = 384 (= 6144>>4)
	;
	;	out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	;	SLOT->op1_out[0] = SLOT->op1_out[1];
	;	SLOT->op1_out[1] = 0;
	;	*SLOT->connect1 += SLOT->op1_out[0];
	;	env  = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	;	if( env < ENV_QUIET )
	;	{
	;		if (!SLOT->FB)
	;			out = 0;
	;		UINT32 p = (env<<4) + sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	;		if (p < TL_TAB_LEN)
	;			SLOT->op1_out[1] = tl_tab[p];
	;	}
	; On input, r3 low = older sample, r3 high = newer sample
	;=======
for_SLOT1_FB													; for( i=length-1; i >= 0 ; i-- ) {
	;-------
	; Feedback active, calculate r1 = ((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB)))
	;-------
	add		r2, r3, r3, lsl #16									; out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	mov		r1, r12, lsr #(8+5)									; r1 = feedback value, 1..7, 7 = smallest feedback, 1 = largest
	and		r1, #7
	add 	r1, #(1+TL_TAB_VOL_ADJUST)
	mov		r2, r2, asr r1
	add		r1, r7, r2											; r1 = SLOT1->Cnt + (out<<SLOT->FB)
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	CALC_ENVELOPE op1_FB
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	add		r2, r3, r3, lsl #16									; out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	mov		r1, r12, lsr #(8+5)									; r1 = feedback value, 1..7, 7 = smallest feedback, 1 = largest
	and		r1, #7
	add 	r1, #(1+TL_TAB_VOL_ADJUST)
	mov		r2, r2, asr r1
	add		r1, r7, r2											; r1 = SLOT1->Cnt + (out<<SLOT->FB)
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	;-------
	; Calculate phase generator values for SLOT 1
	;-------
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Loop to next sample
	;-------
	subs	r12, #(1<<R12_BUF_SHIFT)
	bge		for_SLOT1_FB										; }
	
for_SLOT1_done
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT1
	;-------
	str		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT1 Cnt value
	;-------
	; Calculate all the ADLIB_BUFFER_SAMPLES samples for SLOT 2
	;-------
skip_SLOT1
	add		r0, #SLOT_SIZE
	
	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback, Con, AM, Vib, EG type, KSR)
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte

	;-------
	; Amplitude modulation handling.
	; Output = r9 value adjusted.
	;-------
	LFO_AM_HANDLING

	;-------
	; Frequency modulation (vibrato) handling.
	; Output = r6 value adjusted.
	;-------
	LFO_PM_HANDLING #SLOT2_BLOCK_FNUM

	;-------
	; SLOT2 buffer fill loop handling.
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

	tst		r12, #(OP_CON_BIT<<8)								; If Con=1, op1 produces sound directly, else use it as phase modulation
	bne		for_SLOT2_add

	IF SKIP_SILENT = 1
	;-------
	; Skip the sample calculating if the slot is silent
	;-------
	cmp		r8, r4, lsl #16										; Volume == silent ...
	ldrbeq	r1, [r0, #SLOT_KEY]
	cmpeq	r1, #0												; .. and not KEY_ON?
	beq		skip_SLOT2											; Yes, so skip the slot handling
	ENDIF
	
	;=======
	; SLOT2 buffer fill loop when SLOT1 output goes to phase_modulation
	; 	env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	;	if( env < ENV_QUIET )
	;	{
	;		UINT32 p = (env<<4) + sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	;		if (p >= TL_TAB_LEN)
	;			output[0] = 0;
	;		else
	;			output[0] = tl_tab[p];
	;	}
	;=======
for_SLOT2_phase													; for( i=length-1; i >= 0 ; i-- ) {
	mov		r3, r12, lsr #R12_BUF_SHIFT
	ldr		r3, [sp, r3, lsl #2]								; r3 = phase_modulation[0] | phase_modulation[1] << 16
	mov		r1, r3, lsl #16
	add		r1, r7, r1, asr #TL_TAB_VOL_ADJUST					; r1 = ((SLOT->Cnt) + (phase_modulation<<16))
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										;	output[0] += tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, r3, lsl #16
	lslhs	r3, #16
	CALC_ENVELOPE op2_phase
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	add		r1, r7, r3, asr #TL_TAB_VOL_ADJUST					; r1 = ((SLOT->Cnt) + (phase_modulation<<16))
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										;	output[0] += tl_tab[p];
	;-------
	; Save the output
	;-------
	ldr		r2, [lr]											; Get current 2 samples from buffer
	lsl		r3, #16												; First output sample to high 16 bits
	add		r2, r3, r2, ror #16									; Add first output sample to buffer high 16 bits
	ror		r2, #16												; Swap the buffer samples
	addlo	r2, r1, lsl #16										; Add second sample to high 16 bits if needed
	str		r2, [lr], #4										; Save the result to buffer, increment pointer
	;-------
	; Loop to next sample
	;-------
	subs	r12, #(1<<R12_BUF_SHIFT)
	bge		for_SLOT2_phase										; }
for_SLOT2_done
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT2
	;-------
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT2 Cnt value
	sub		lr, #ADLIB_BUFFER_SIZE								; Rewind the buffer pointer
	;-------
	; Go handle the next channel unless this was already the last channel.
	;-------
skip_SLOT2
	add		r0, #SLOT_SIZE										; Go to SLOT1 of next channel (r0 points to SLOT2 now)
	add		r12, #0x00010000									; channel++
	and		r4, r12, #0x000F0000
	tst		r12, #R12_RHYTHM_BIT								; Do we have rhythm mode on?
	moveq	r5, #0x00090000										; Nope, handle 9 melodic channels
	movne	r5, #0x00060000										; Yep, handle 6 melodic channels
	cmp		r4, r5
	blt		for_channel
	cmp		r4, #0x00090000
	bhs		slots_done
	;-------
	; Bass Drum = CH6 OP1 & OP2
	; - If Con bit (of either slot) is on, we can ignore slot 1 completely! 
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	tst		r1, #0x10											; Test the Con bit
	bne		bd_slot2_solo										; Con=1, slot 1 is ignored!

	;-------
	; Slot 1 is used as a phase_modulation for slot 2. Calculate slot 1 first.
	;-------
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)

	ldr		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value

	;-------
	; SLOT1 buffer fill loop handling
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

for_bd_slot1													; for( i=length-1; i >= 0 ; i-- ) {
	;-------
	; Feedback active, calculate r1 = ((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB)))
	;-------
	add		r2, r3, r3, lsl #16									; out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	mov		r1, r12, lsr #(8+5)									; r1 = feedback value, 1..7, 7 = smallest feedback, 1 = largest
	ands	r1, #7
	moveq	r2, #0												; if (!SLOT->FB) out = 0;
	add 	r1, #(1+TL_TAB_VOL_ADJUST)
	mov		r2, r2, asr r1
	add		r1, r7, r2											; r1 = SLOT1->Cnt + (out<<SLOT->FB)
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	CALC_ENVELOPE bd_slot1
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Save the samples to feedback buffer
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	add		r2, r3, r3, lsl #16									; out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	mov		r1, r12, lsr #(8+5)									; r1 = feedback value, 1..7, 7 = smallest feedback, 1 = largest
	ands	r1, #7
	moveq	r2, #0												; if (!SLOT->FB) out = 0;
	add 	r1, #(1+TL_TAB_VOL_ADJUST)
	mov		r2, r2, asr r1
	add		r1, r7, r2											; r1 = SLOT1->Cnt + (out<<SLOT->FB)
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Loop to next sample
	;-------
	subs	r12, #(1<<R12_BUF_SHIFT)
	bge		for_bd_slot1										; }
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT1
	;-------
	str		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT1 Cnt value
	;-------
	; Calculate all the ADLIB_BUFFER_SAMPLES samples for SLOT 2
	;-------
	add		r0, #SLOT_SIZE
	
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback, Con, AM, Vib, EG type, KSR)
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)

	;-------
	; SLOT2 buffer fill loop handling
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

for_bd_slot2_phase												; for( i=length-1; i >= 0 ; i-- ) {
	mov		r3, r12, lsr #R12_BUF_SHIFT
	ldr		r3, [sp, r3, lsl #2]								; r3 = phase_modulation[0] | phase_modulation[1] << 16
	mov		r1, r3, lsl #16
	add		r1, r7, r1, asr #TL_TAB_VOL_ADJUST					; r1 = ((SLOT->Cnt) + (phase_modulation<<16))
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										;	output[0] += tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, r3, lsl #16									; Replace the low 16 bits of r13 with r1
	lslhs	r3, #16
	CALC_ENVELOPE bd_slot2_phase
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	add		r1, r7, r3, asr #TL_TAB_VOL_ADJUST					; r1 = ((SLOT->Cnt) + (phase_modulation<<16))
	ubfx	r1, r1, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										;	output[0] += tl_tab[p];
	;-------
	; Save the output
	;-------
	ldr		r2, [lr]											; Get current 2 samples from buffer
	lsl		r3, #(16+1)											; Drums have double volume
	qadd16	r2, r2, r3
	lsl		r1, #1
	qadd16lo r2, r2, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r2, [lr], #4										; Save the result to buffer, increment pointer
	;-------
	; Loop to next sample
	;-------
	bge		for_bd_slot2_phase									; }
	b		bd_slot2_done
	
	;-------
	; Bassdrum only uses slot2, calculate all the ADLIB_BUFFER_SAMPLES samples for SLOT 2
	;-------
bd_slot2_solo	
	add		r0, #SLOT_SIZE										; Jump over slot 1, make r0 point to slot 2

	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT into r12 second byte (needed in envelope calculations)

	;-------
	; Init the sample counter
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

for_bd_slot2													; for( i=length-1; i >= 0 ; i-- ) {
	ldr		r3, [lr]											; r3 = current 2 samples in the output buffer
	ubfx	r1, r7, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #1
	qadd16lo r3, r3, r1
	CALC_ENVELOPE bd_slot2
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	ubfx	r1, r7, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										;	output[0] = tl_tab[p];
	lsl		r1, #17
	qadd16lo r3, r3, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r3, [lr], #4										; Save the result to buffer, increment pointer
	;-------
	; Loop to next sample
	;-------
	bge		for_bd_slot2										; }
bd_slot2_done
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT2
	;-------
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT2 Cnt value
	
	sub		lr, #ADLIB_BUFFER_SIZE								; Rewind back to the buffer start
	add		r0, #SLOT_SIZE
	;-------
	; HiHat (CH7 OP1)
	;	/* high hat phase generation
	;		phase = d0 or 234 (based on frequency only)
	;		phase = 34 or 2d0 (based on noise)
	;	*/
	;
	;	/* base frequency derived from operator 1 in channel 7 */
	;	unsigned char bit7 = ((SLOT7_1->Cnt>>FREQ_SH)>>7)&1;
	;	unsigned char bit3 = ((SLOT7_1->Cnt>>FREQ_SH)>>3)&1;
	;	unsigned char bit2 = ((SLOT7_1->Cnt>>FREQ_SH)>>2)&1;
	;
	;	unsigned char res1 = (bit2 ^ bit7) | bit3;
	;
	;	/* when res1 = 0 phase = 0x000 | 0xd0; */
	;	/* when res1 = 1 phase = 0x200 | (0xd0>>2); */
	;	UINT32 phase = res1 ? (0x200|(0xd0>>2)) : 0xd0;
	;
	;	/* enable gate based on frequency of operator 2 in channel 8 */
	;	unsigned char bit5e= ((SLOT8_2->Cnt>>FREQ_SH)>>5)&1;
	;	unsigned char bit3e= ((SLOT8_2->Cnt>>FREQ_SH)>>3)&1;
	;
	;	unsigned char res2 = (bit3e ^ bit5e);
	;
	;	/* when res2 = 0 pass the phase from calculation above (res1); */
	;	/* when res2 = 1 phase = 0x200 | (0xd0>>2); */
	;	if (res2)
	;		phase = (0x200|(0xd0>>2));
	;
	;
	;	/* when phase & 0x200 is set and noise=1 then phase = 0x200|0xd0 */
	;	/* when phase & 0x200 is set and noise=0 then phase = 0x200|(0xd0>>2), ie no change */
	;	if (phase&0x200)
	;	{
	;		if (noise)
	;			phase = 0x200|0xd0;
	;	}
	;	else
	;	/* when phase & 0x200 is clear and noise=1 then phase = 0xd0>>2 */
	;	/* when phase & 0x200 is clear and noise=0 then phase = 0xd0, ie no change */
	;	{
	;		if (noise)
	;			phase = 0xd0>>2;
	;	}
	;
	;	output[0] += op_calc(phase<<FREQ_SH, env, 0, SLOT7_1->wavetable) * 2;
	;-------
	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)

	;-------
	; Init the sample counter
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)
	
for_hihat														; for( i=length-1; i >= 0 ; i-- ) {
	ldr		r3, [lr]											; r3 = current 2 samples in the output buffer
	eor		r1, r7, r7, lsr #5
	orr		r1, r7, lsr #1
	and		r1, #(1<<(16+2))									; r1 = res1 = (bit2 ^ bit7) | bit3; (== 0x200 shifted 9 bits left)
	movs	r2, r7, ror r7										; Carry flag = pseudo-random value
	orrcc	r1, #(0xD0<<(16+2-9))								; phase = 0x200|0xd0;
	orrcs	r1, #(0xD0<<(16+2-9-2))								; phase = 0xd0>>2;
	lsr		r1, #(16+2-9)
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + (phase & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #1
	qadd16lo r3, r3, r1
	CALC_ENVELOPE hihat
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	eor		r1, r7, r7, lsr #5
	orr		r1, r7, lsr #1
	and		r1, #(1<<(16+2))									; r1 = res1 = (bit2 ^ bit7) | bit3; (== 0x200 shifted 9 bits left)
	movs	r2, r7, ror r7										; Carry flag = pseudo-random value
	orrcc	r1, #(0xD0<<(16+2-9))								; phase = 0x200|0xd0;
	orrcs	r1, #(0xD0<<(16+2-9-2))								; phase = 0xd0>>2;
	lsr		r1, #(16+2-9)
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + (phase & SIN_MASK) ];
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsl		r1, #17
	qadd16lo r3, r3, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r3, [lr], #4										; Save the result to buffer, increment pointer
	;-------
	; Loop to next sample
	;-------
	bge		for_hihat											; }
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT1
	;-------
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT1 Cnt value
	;-------
	; Snare (CH7 OP2)
	;	/* Snare Drum (verified on real YM3812) */
	;	env = volume_calc(SLOT7_2);
	;	if( env < ENV_QUIET )
	;	{
	;		/* base frequency derived from operator 1 in channel 7 */
	;		unsigned char bit8 = ((SLOT7_1->Cnt>>FREQ_SH)>>8)&1;
	;
	;		/* when bit8 = 0 phase = 0x100; */
	;		/* when bit8 = 1 phase = 0x200; */
	;		UINT32 phase = bit8 ? 0x200 : 0x100;
	;
	;		/* Noise bit XORes phase by 0x100 */
	;		/* when noisebit = 0 pass the phase from calculation above */
	;		/* when noisebit = 1 phase ^= 0x100; */
	;		/* in other words: phase ^= (noisebit<<8); */
	;		if (noise)
	;			phase ^= 0x100;
	;
	;		output[0] += op_calc(phase<<FREQ_SH, env, 0, SLOT7_2->wavetable) * 2;
	;	}
	;-------
	sub		lr, #ADLIB_BUFFER_SIZE								; Rewind buffer pointer back to start
	add		r0, #SLOT_SIZE

	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	ldr		r4, [r0, #SLOT_ENV_SUST]							; r4 = (1023<<16) | SLOT1 sustain level (MAX_ATT_INDEX if release phase)
	ldr		r5, [r0, #SLOT_ENV_INCR]							; r5 = SLOT1 envelope increment value
	; r6 = SLOT Incr value not loaded, we use the value of the previous slot!
	; r7 = SLOT Cnt value not loaded, we use the value of the previous slot!
	ldr		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	ldr		r9, [r0, #SLOT_TLL]									; r9 = SLOT TLL value
	ldr		r10,[r0, #SLOT_WAVETABLE]							; r10 = SLOT wavetable value
	
	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)
	
	;-------
	; Init the sample counter
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

for_snare
	ldr		r3, [lr]											; r3 = current 2 samples in the output buffer
	mov		r1, r7, ror r7										; r1 = pseudo-random value
	and		r1, #0x100											; when noisebit = 1 phase ^= 0x100;
	tst		r7, #(1<<(16+8))									; bit8 = ((SLOT7_1->Cnt>>FREQ_SH)>>8)&1;
	orrne	r1, #0x200											; when bit8 = 1 phase = 0x200;
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + phase];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #1
	qadd16lo r3, r3, r1
	CALC_ENVELOPE snare
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	mov		r1, r7, ror r7										; r1 = pseudo-random value
	and		r1, #0x100											; when noisebit = 1 phase ^= 0x100;
	tst		r7, #(1<<(16+8))									; bit8 = ((SLOT7_1->Cnt>>FREQ_SH)>>8)&1;
	orrne	r1, #0x200											; when bit8 = 1 phase = 0x200;
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + phase];
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #17
	qadd16lo r3, r3, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r3, [lr], #4
	;-------
	; Loop to next sample
	;-------
	bge		for_snare											; }
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT2
	;-------
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	; r7 not saved!

	sub		lr, #ADLIB_BUFFER_SIZE								; Rewind back to the buffer start
	add		r0, #SLOT_SIZE
	;-------
	; Tom Tom (CH8 OP1)
	;	env = volume_calc(SLOT8_1);
	;	if( env < ENV_QUIET )
	;		output[0] += op_calc(SLOT8_1->Cnt, env, 0, SLOT8_1->wavetable) * 2;
	;-------
	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	ldr		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)

	;-------
	; Init the sample counter
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)
	
for_tomtom														; for( i=length-1; i >= 0 ; i-- ) {
	ldr		r3, [lr]											; r3 = current 2 samples in the output buffer
	ubfx	r1, r7, #16, #SIN_BITS								; Use only SLOT->Cnt
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #1
	qadd16lo r3, r3, r1
	CALC_ENVELOPE tomtom
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	ubfx	r1, r7, #16, #SIN_BITS								; Use only SLOT->Cnt
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsl		r1, #17
	qadd16lo r3, r3, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r3, [lr], #4
	;-------
	; Loop to next sample
	;-------
	bge		for_tomtom											; }
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT1
	;-------
	str		r3, [r0, #SLOT_OP1_OUT]								; r3 = SLOT1 op1_out value
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT1 Cnt value
	;-------
	; Cymbal (CH8 OP2)
	;	/* Top Cymbal (verified on real YM3812) */
	;	env = volume_calc(SLOT8_2);
	;	if( env < ENV_QUIET )
	;	{
	;		/* base frequency derived from operator 1 in channel 7 */
	;		unsigned char bit7 = ((SLOT7_1->Cnt>>FREQ_SH)>>7)&1;
	;		unsigned char bit3 = ((SLOT7_1->Cnt>>FREQ_SH)>>3)&1;
	;		unsigned char bit2 = ((SLOT7_1->Cnt>>FREQ_SH)>>2)&1;
	;
	;		unsigned char res1 = (bit2 ^ bit7) | bit3;
	;
	;		/* when res1 = 0 phase = 0x000 | 0x100; */
	;		/* when res1 = 1 phase = 0x200 | 0x100; */
	;		UINT32 phase = res1 ? 0x300 : 0x100;
	;
	;		/* enable gate based on frequency of operator 2 in channel 8 */
	;		unsigned char bit5e= ((SLOT8_2->Cnt>>FREQ_SH)>>5)&1;
	;		unsigned char bit3e= ((SLOT8_2->Cnt>>FREQ_SH)>>3)&1;
	;
	;		unsigned char res2 = (bit3e ^ bit5e);
	;		/* when res2 = 0 pass the phase from calculation above (res1); */
	;		/* when res2 = 1 phase = 0x200 | 0x100; */
	;		if (res2)
	;			phase = 0x300;
	;
	;		output[0] += op_calc(phase<<FREQ_SH, env, 0, SLOT8_2->wavetable) * 2;
	;	}
	;-------
	sub		lr, #ADLIB_BUFFER_SIZE								; Rewind buffer pointer back to start
	add		r0, #SLOT_SIZE

	;-------
	; Load the SLOT-specific data values
	;-------
	ldrb	r1, [r0, #SLOT_BITS]								; r1 = SLOT bits (Feedback(3 bits), Con, AM, Vib, EG type, KSR)
	ldmia	r0, {r4-r10}

	bic		r12, #0xFF00
	orr		r12, r1, lsl #8										; Put all bit values of SLOT1 into r12 second byte (needed in envelope calculations)

	;-------
	; Init the sample counter
	;-------
	orr		r12, #(((ADLIB_BUFFER_SAMPLES>>1)-1)<<R12_BUF_SHIFT)

for_cymbal
	ldr		r3, [lr]											; r3 = output[0] | output[1] << 16

	eor		r1, r7, r7, lsr #2
	and		r1, #(1<<(16+3))									; Zero flag = res2 = (bit3e ^ bit5e); (== 0x200 shifted 10 bits left)
	orr		r1, #(1<<(16+2))
	lsr		r1, #(16+2-10)
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + (phase & SIN_MASK) ];

	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #1
	qadd16lo r3, r3, r1
	CALC_ENVELOPE cymbal
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	eor		r1, r7, r7, lsr #2
	and		r1, #(1<<(16+3))									; Zero flag = res2 = (bit3e ^ bit5e); (== 0x200 shifted 10 bits left)
	orr		r1, #(1<<(16+2))
	lsr		r1, #(16+2-10)
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + (phase & SIN_MASK) ];
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	lsl		r1, #17
	qadd16lo r3, r3, r1
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r3, [lr], #4
	;-------
	; Loop to next sample
	;-------
	bge		for_cymbal											; }
	add		r12, #(1<<R12_BUF_SHIFT)
	;-------
	; Save the final values for SLOT2
	;-------
	str		r8, [r0, #SLOT_VOLUME]								; r8 = SLOT volume value << 16
	str		r7, [r0, #SLOT_CNT]									; r7 = SLOT Cnt value

	GLOBAL	slots_done
slots_done	
	;-------
	; Calculate new LFO_AM value. The value is a triangle waveform, values between 0 and 26.
	; Value should change after every 168 samples, but we change it after every ADLIB_BUFFER_SAMPLES samples.
	;-------
	ldr		r0,  =lfo_am_cnt
	ldr		r12, [r0]
;	adds	r12, #0x40000000									; Add the counter, if carry set we should change the LFO value.
;	bcc		1f													; Carry clear, just save the new counter value.
	; ----- We need to increase/decrease LFO_AM value.
	tst		r12, #R12_AM_DIR_BIT								; Are we incrementing or decrementing LFO_AM value?
	addeq	r12, #(1<<3)
	subne	r12, #(1<<3)
	tstne	r12, #(0x1F<<3)										; Is the new value zero?
	biceq	r12, #R12_AM_DIR_BIT								; Yes, so go upwards the next time
	cmp		r12, #(26<<3)										; Did we reach 26?
	orrge	r12, #R12_AM_DIR_BIT								; Yes, so go downwards the next time
;	add		r12, #0x40000000									; Counter starts from 1, not 0
1	str		r12, [r0]											; r12 = LFO_AM<<3 | rhythm<<2 | lfo_am_direction<<1 | lfo_am_depth
	;-------
	; Calculate new LFO_PM value. The value is a looping counter of values 0..7,
	; which adjusts the lfo_pm_table address in stack.
	; Value should change after every 674 samples, but we change it after 5*128 samples.
	;-------
	ldr		r1, [r0, #(lfo_pm_cnt-lfo_am_cnt)]
	ldr		r2, =0x33333333										; (65536*65536/5)
	adds	r1, r2												; Carry set if we need to adjust lfo_pm_table address
	bcc		%f1													; Carry clear, just save the new counter value.
	ldr		r2, [sp, #ADLIB_BUFFER_SIZE]
	add		r3, r2, #1
	and		r3, #7
	bic		r2, #7
	orr		r2, r3
	str		r2, [sp, #ADLIB_BUFFER_SIZE]
1	str		r1, [r0, #(lfo_pm_cnt-lfo_am_cnt)]
	IF USEBUFFER = 1
	;-------
	; Handle AdLib commands from the command buffer
	;-------
cmnd_loop
	ldr		r2, =cmnd_buf_head
	ldr		r1, [r2, #(cmnd_buf_tail-cmnd_buf_head)]
	ldr		r0, [r2, #(cmnd_buf_keycmnd-cmnd_buf_head)]
	cmp		r0, r1
	beq		cmnd_done											; No new commands in the buffer, go wait on the spin lock
	;-------
	; Make sure the command in the buffer is older than our tick limit
	;-------
	sub		r0, r2, #(cmnd_buf_head-cmnd_buf)					; r0 = cmnd_buf address 
	mov		r3, r1, lsr #CMND_BUF_SHIFT
	ldr		r0, [r0, r3]										; r0 = new command = tick<<16 | reg<<8 | value
	;-------
	; Ok, use this command, increment the tail pointer
	;-------
	add		r3, r1, #CMND_BUF_INCR
	str		r3, [r2, #(cmnd_buf_tail-cmnd_buf_head)]
	lsl		r0, #16
	lsr		r0, #16
	ldr		r4, =cmnd_jump
	ubfx	r1, r0, #13, #3										; switch(r&0xe0)
	ldr		pc, [r4, r1, lsl #2]								; Jump to the handler

	AREA 	jumptables, DATA, READONLY
	ALIGN	4

cmnd_jump
	DCD		cmnd_done
	DCD		cmnd_20_30
	DCD		cmnd_40_50
	DCD		cmnd_60_70
	DCD		cmnd_80_90
	DCD		cmnd_A0_B0
	DCD		cmnd_C0_D0
	DCD		cmnd_E0_F0

	AREA 	adlib, CODE, READONLY
	ALIGN	4

cmnd_done
	ENDIF
	ldr		r0, =lfo_pm_tbl
	add		sp, #ADLIB_BUFFER_SIZE
	pop		{r1}
	str		r1, [r0]											; Save the current lfo_pm_table address
	pop		{r4-r11, pc}										; Return to caller

	;=======

	LTORG
	
	;=======
	; SLOT1 buffer fill loop when feedback is inactive
	; Output goes always to the feedback buffer, 
	; SLOT2 uses it as phase_modulation or direct output depending on connect
	;
	;	FREQ_SH = 16
	;	FREQ_MASK = 65535
	;	SIN_MASK = 1023
	;	ENV_QUIET = 384 (= 6144>>4)
	;
	;	out  = SLOT->op1_out[0] + SLOT->op1_out[1];
	;	SLOT->op1_out[0] = SLOT->op1_out[1];
	;	SLOT->op1_out[1] = 0;
	;	*SLOT->connect1 += SLOT->op1_out[0];
	;	env  = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	;	if( env < ENV_QUIET )
	;	{
	;		if (!SLOT->FB)
	;			out = 0;
	;		UINT32 p = (env<<4) + sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	;		if (p < TL_TAB_LEN)
	;			SLOT->op1_out[1] = tl_tab[p];
	;	}
	;=======
for_SLOT1_no_FB													; for( i=length-1; i >= 0 ; i-- ) {
	ubfx	r1, r7, #16, #SIN_BITS								; Use only SLOT->Cnt
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	CALC_ENVELOPE op1_no_FB
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	ubfx	r1, r7, #16, #SIN_BITS								; Use only SLOT->Cnt
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (out<<SLOT->FB))) >> FREQ_SH ) & SIN_MASK) ];
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]										; SLOT->op1_out[1] = tl_tab[p];
	lsr		r3, #16
	orrlo	r3, r1, lsl #16
	add		r7, r6												; SLOT1->Cnt += SLOT1->Incr
	;-------
	; Loop to next sample
	;-------
	subs	r12, #(1<<R12_BUF_SHIFT)
	bge		for_SLOT1_no_FB										; }
	b		for_SLOT1_done
	;=======

	IF SKIP_SILENT = 1
	;=======
	; SLOT1 buffer fill loop when SLOT1 is silent.
	; Output goes always to the feedback buffer, 
	; SLOT2 uses it as phase_modulation or direct output depending on connect
	;=======
for_SLOT1_silent												; for( i=length-1; i >= 0 ; i-- ) {
	mov		r3, #0
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	subs	r12, #(1<<R12_BUF_SHIFT)
	blt		for_SLOT1_done
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	subs	r12, #(1<<R12_BUF_SHIFT)
	blt		for_SLOT1_done
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	subs	r12, #(1<<R12_BUF_SHIFT)
	blt		for_SLOT1_done
	mov		r1, r12, lsr #R12_BUF_SHIFT
	str		r3, [sp, r1, lsl #2]								; Write the value to the feedback buffer
	subs	r12, #(1<<R12_BUF_SHIFT)
	bge		for_SLOT1_silent									; }
	b		for_SLOT1_done
	;=======
	ENDIF

	;=======
	; SLOT2 buffer fill loop when SLOT1 produces output directly.
	; Need to mix the values to the output buffer.
	; This code is used much more rarely than the phase version.
	;
	; 	env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	;	if( env < ENV_QUIET )
	;	{
	;		UINT32 p = (env<<4) + sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	;		if (p >= TL_TAB_LEN)
	;			output[0] += 0;
	;		else
	;			output[0] += tl_tab[p];
	;	}
	;=======
for_SLOT2_add													; for( i=length-1; i >= 0 ; i-- ) {
	mov		r3, r12, lsr #R12_BUF_SHIFT
	ldr		r3, [sp, r3, lsl #2]								; r3 = 2 samples from SLOT1
	ubfx	r1, r7, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r2, r9, r8, lsr #16									; r2 = env = ((SLOT)->TLL + ((UINT32)(SLOT)->volume) + (LFO_AM & (SLOT)->AMmask));
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	qadd16lo r3, r3, r1											; Add the value to the feedback buffer value
	CALC_ENVELOPE op2_add
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	;-------
	; Unrolled loop, do the same thing again for the next sample
	;-------
	ubfx	r1, r7, #16, #SIN_BITS								; r1 &= SIN_MASK
	ldr		r1, [r10, r1, lsl #2]								; r1 = sin_tab[SLOT->wavetable + ((((signed int)((SLOT->Cnt & ~FREQ_MASK) + (phase_modulation<<16))) >> FREQ_SH ) & SIN_MASK) ];
	add		r1, r2, lsl #5										; r1 = env<<4 + sin_tab[..], extra << 1 for halfword accessing
	cmp		r1, #(2*TL_TAB_LEN)									; if (p < TL_TAB_LEN)
	ldrhlo	r1, [r11, r1]
	ldr		r2, [lr]											; Get current 2 samples from buffer
	add		r7, r6												; SLOT->Cnt += SLOT->Incr
	lsl		r1, #16
	qadd16lo r3, r3, r1											; Add the value to the feedback buffer value
	;-------
	; Save the output
	;-------
	qadd16	r2, r2, r3
	subs	r12, #(1<<R12_BUF_SHIFT)
	str		r2, [lr], #4										; Save the mixed samples to buffer, increment pointer
	;-------
	; Loop to next sample
	;-------
	bge		for_SLOT2_add										; }
	b		for_SLOT2_done
	;=======


	MACRO
	op_decay $item
	;-------
	; Operator went over max volume, go to decay phase
	;-------
decay_$item
	ldr		r5, [r0, #(ch0_slot1_env_dr_incr-SLOT1)]
	mov		r8, #0
	str		r5, [r0, #SLOT_ENV_INCR]				; r5 = SLOT1 envelope counter & mode value
	b		env_done_$item
	MEND

	MACRO
	op_sustain $item
	;-------
	; Operator went under the SUSTAIN volume level.
	; If the operator is in percussive mode, go to DECAY phase, else stay at this level (until KEY_OFF)
	;-------
sustain_$item
	mov		r8, r4, lsl #16							; Fix the volume to be exactly the SUSTAIN level ...
	IF USEBUFFER = 0
	ldr		r5, [r0, #SLOT_ENV_INCR]				; r5 = SLOT1 envelope counter & mode value
	tst		r5, #0x80000000							; Should we go to ATTACK phase?
	bne		env_done_$item							; Yes, so go there instead of sustain/release
	ENDIF	
	mov		r5, #0									; ... and stay at this volume level ...
	tst		r12, #(2<<8)							; ... unless EG type is clear (percussion mode) ...
	strne	r5, [r0, #SLOT_ENV_INCR]				; r5 = SLOT1 envelope counter & mode value
	bne		env_done_$item
	;-------
	; EG type = percussion: go to release mode instead of sustain mode.
	;-------
	mvn		r4, #0xFC00								; ... in which case we go to RELEASE phase.
	lsl		r4, #16
	orr		r4, #MAX_ATT_INDEX
	ldr		r5, [r0, #(ch0_slot1_env_rr_incr-SLOT1)]
	str		r4, [r0, #SLOT_ENV_SUST]				; r4 = (1023<<16) | SLOT1 sustain level (MAX_ATT_INDEX if release phase)
	str		r5, [r0, #SLOT_ENV_INCR]				; r5 = SLOT1 envelope counter & mode value
	b		env_done_$item
	MEND

	op_decay 	op1_FB
	op_sustain	op1_FB
	op_decay 	op1_no_FB
	op_sustain	op1_no_FB

	op_decay 	op2_add
	op_sustain	op2_add
	op_decay 	op2_phase
	op_sustain	op2_phase

	op_decay 	bd_slot1
	op_sustain	bd_slot1
	op_decay 	bd_slot2
	op_sustain	bd_slot2
	op_decay 	bd_slot2_phase
	op_sustain	bd_slot2_phase

	op_decay 	hihat
	op_sustain	hihat

	op_decay 	snare
	op_sustain	snare

	op_decay 	tomtom
	op_sustain	tomtom

	op_decay 	cymbal
	op_sustain	cymbal

	;=======
	; Handle 0xA0..0xA8, 0xB0..0xB8 and 0xBD commands.
	;=======
cmnd_A0_B0	
	and		r2, r0, #0xFF00
	cmp		r2, #0xBD00												; if (r == 0xbd)
	beq		cmnd_bd
	;-------
	; First calculate the channel (0..8) of the operation and make
	; r1 = channel SLOT1 pointer
	;-------
	and		r2, #0x0F00
	cmp		r2, #0x0800												; if( (r&0x0f) > 8)
	bgt		cmnd_loop												;	 return
	ldr		r1, =SLOT1
	lsr		r2, #8
	mov		r3, #(2*SLOT_SIZE)
	mul		r4, r3, r2
	add		r1, r4													; CH = &OPL->P_CH[r&0x0f];
	;-------
	; r2 = current block_fnum of this channel
	;-------
	ldr		r2,[r1, #SLOT1_BLOCK_FNUM] 								; r2 = CH->block_fnum
	tst		r0, #0x1000												; if(!(r&0x10))
	bne		cmnd_bX													; {	/* a0-a8 */
	;-------
	; A0..A8: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |      F-Number (least significant byte)        |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; r3 = new block_fnum
	;-------
	and		r3, r2, #0x1F00
	and		r0, #0xFF
	orr		r3, r0													;	block_fnum  = (CH->block_fnum&0x1f00) | v;
	b		cmnd_update												; } else
	;-------
	; B0..B8
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  Unused   | Key |     Octave      | F-Number  |
	; |           | On  |                 | high bits |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; r3 = new block_fnum
	;-------
cmnd_bX																; {	/* b0-b8 */
	and		r3, r2, #0xFF
	and		r4, r0, #0x1F
	lsl		r4, #8
	orr		r3, r4													;	block_fnum = ((v&0x1f)<<8) | (CH->block_fnum&0xff);
	ldrb	r4, [r1, #SLOT_KEY] 									; r4 = current KEY_ON value
	tst		r0, #0x20												; 	if(v&0x20)
	beq		keyoff													;	{
	;-------
	; Key On => Go to ATTACK state => sustain level = SLOT sustain level, envelope = attack
	;-------
	cmp		r4, #0
	orr		r4, #1
	strb	r4, [r1, #SLOT_KEY] 									; SLOT->key |= key_set;
	bne		%f1														; if( !SLOT->key ) {
	;-------
	; Key On: SLOT1 does not yet have key on, got to attack mode.
	;-------
	str		r4, [r1, #SLOT_CNT]										;	SLOT->Cnt = 0; /* restart Phase Generator */
	ldr		r5, [r1, #(ch0_slot1_sl-SLOT1)]
	ldr		r7, [r1, #(ch0_slot1_env_ar_incr-SLOT1)]
	mvn		r4, #0xFC00
	lsl		r4, #16
	orr		r5, r4
	str		r5, [r1, #SLOT_ENV_SUST] 								; SLOT1->env_sustain = SLOT1->sl;
	str		r7, [r1, #SLOT_ENV_INCR] 								; SLOT1->envelope = attack rate | ATTACK phase
1	ldrb	r4, [r1, #(SLOT_SIZE+SLOT_KEY)]							; r4 = current KEY_ON value
	cmp		r4, #0
	orr		r4, #1
	strb	r4, [r1, #(SLOT_SIZE+SLOT_KEY)] 						; SLOT->key |= key_set;
	bne		cmnd_update												; if( !SLOT->key ) {
	;-------
	; Key On: SLOT2 does not yet have key on, got to attack mode.
	;-------
	str		r4, [r1, #(SLOT_SIZE+SLOT_CNT)]							;	SLOT->Cnt = 0; /* restart Phase Generator */
	ldr		r5, [r1, #(SLOT_SIZE+(ch0_slot1_sl-SLOT1))]
	ldr		r7, [r1, #(SLOT_SIZE+(ch0_slot1_env_ar_incr-SLOT1))]
	mvn		r4, #0xFC00
	lsl		r4, #16
	orr		r5, r4
	str		r5, [r1, #(SLOT_SIZE+SLOT_ENV_SUST)] 					; SLOT2->env_sustain = SLOT2->sl;
	str		r7, [r1, #(SLOT_SIZE+SLOT_ENV_INCR)] 					; SLOT2->envelope = attack rate | ATTACK phase
	b		cmnd_update												;	}

	MACRO
	DRUM_KEY $bit
	ldrb	r4, [r1, #SLOT_KEY] 									; r4 = current KEY_ON value
	tst		r0, #$bit
	beq		%f1
	;-------
	; KEY_ON
	;-------
	cmp		r4, #0
	orr		r4, #2
	strb	r4, [r1, #SLOT_KEY] 									; SLOT->key |= key_set;
	bne		%f2														; if( !SLOT->key ) {
	str		r4, [r1, #SLOT_CNT]										;	SLOT->Cnt = 0; /* restart Phase Generator */
	ldr		r5, [r1, #(ch0_slot1_sl-SLOT1)]
	ldr		r7, [r1, #(ch0_slot1_env_ar_incr-SLOT1)]
	mvn		r4, #0xFC00
	lsl		r4, #16
	orr		r5, r4
	str		r5, [r1, #SLOT_ENV_SUST] 								; SLOT1->env_sustain = SLOT1->sl;
	str		r7, [r1, #SLOT_ENV_INCR] 								; SLOT1->envelope = attack rate | ATTACK phase
	b		%f2														; }
	;-------
	; KEY_OFF
	;-------
1	cmp		r4, #0													; if( SLOT->key )
	beq		%f2														; {
	bics	r4, #2													
	strb	r4, [r1, #SLOT_KEY] 									;	SLOT->key &= key_clr;
	bne		%f2														;	if( !SLOT->key ) {
	mvn		r5, #0xFC00
	lsl		r5, #16
	orr		r5, #MAX_ATT_INDEX
	ldr		r6,[r1, #(ch0_slot1_env_rr_incr-SLOT1)]
	str		r5,[r1, #SLOT_ENV_SUST]
	str		r6,[r1, #SLOT_ENV_INCR] 								; 		SLOT->state = EG_REL;
2																	; }
	MEND

	;-------
	; BD
	; +-----+-----+------+-----+-----+-----+------+-----+
	; |  7  |  6  |  5   |  4  |  3  |  2  |  1   |  0  |
	; +-----+-----+------+-----+-----+-----+------+-----+
	; | AM  | Vib |Rhythm|Bass |Snare| Tom | Top  |High |
	; |depth|depth|enable|Drum |Drum | Tom |Cymbal|Hat  |
	; +-----+-----+------+-----+-----+-----+------+-----+
	;-------
cmnd_bd
	;-------
	; Handle Vib depth bit (adjust address of lfo_pm_table in stack)
	;-------
	ldr		r3, [sp, #ADLIB_BUFFER_SIZE]							; Get lfo_pm_table start address
	tst		r0, #0x40												; Is "Vib depth" bit set?
	biceq	r3, #8
	orrne	r3, #8
	str		r3, [sp, #ADLIB_BUFFER_SIZE]							; Save lfo_pm_table start address
	;-------
	; Handle AM depth bit (stored in lfo_am_cnt)
	;-------
	ldr		r2,=lfo_am_cnt
	ldr		r3, [r2]
	tst		r0, #0x80												; Is "AM depth" bit set?
	biceq	r3, #1													; Nope, clear the bit in lfo_am_cnt
	orrne	r3, #1													; Yep, set the bit in lfo_am_cnt
	;-------
	; Handle Rhythm Enable bit (stored in lfo_am_cnt)
	;-------
	tst		r3, #4													; Do we currently have rhythm section on?
	bne		rhythm_on												; Yep, jump there
	;-------
	; Rhythm is currently off, just store the new bit.
	;-------
	tst		r0, #0x20												; Is "Rhythm enable" bit set?
	biceq	r3, #4													; Nope, clear the bit in lfo_am_cnt
	orrne	r3, #4													; Yep, set the bit in lfo_am_cnt
	str		r3, [r2]												; Save the changed lfo_am_cnt
	beq		cmnd_loop												; If rhythm bit stayed off, just back to loop.
	b		rhythm_started											; Rhythm section was just activated!
	;-------
	; Rhythm section is currently on, check if it was turned off.
	;-------
rhythm_on
	tst		r0, #0x20												; Is "Rhythm enable" bit set?
	biceq	r3, #4													; Nope, clear the bit in lfo_am_cnt
	orrne	r3, #4													; Yep, set the bit in lfo_am_cnt
	str		r3, [r2]												; Save the changed lfo_am_cnt
	beq		rhythm_stopped
	;-------
	; Rhythm section was just activated!
	;-------
rhythm_started
handle_rhythm
	ldr		r1,=SLOT1
	add		r1, #(6*CH_SIZE)
	;-------
	; Handle BassDrum on/off (Channel 6 slot 0 and slot 1)
	;-------
	DRUM_KEY 0x10
	add		r1, #SLOT_SIZE
	DRUM_KEY 0x10
	;-------
	; Handle HiHat on/off (Channel 7 slot 1)
	;-------
	add		r1, #SLOT_SIZE
	DRUM_KEY 1
	;-------
	; Handle Snare on/off (Channel 7 slot 2)
	;-------
	add		r1, #SLOT_SIZE
	DRUM_KEY 8
	;-------
	; Handle TomTom on/off (Channel 8 slot 1)
	;-------
	add		r1, #SLOT_SIZE
	DRUM_KEY 4
	;-------
	; Handle Cymbal on/off (Channel 8 slot 2)
	;-------
	add		r1, #SLOT_SIZE
	DRUM_KEY 2
	b		cmnd_loop
	;-------
	; Rhythm section was just deactivated! Stop all rhythm sounds.
	;-------
rhythm_stopped
	ldr		r1,=SLOT1
	mov		r4, #0
	add		r1, #(6*CH_SIZE)
	mvn		r5, #0xFC00
	lsl		r5, #16
	orr		r5, #MAX_ATT_INDEX
	ldr		r6,[r1, #(ch0_slot1_env_rr_incr-SLOT1)]
	ldr		r7,[r1, #(SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	str		r5,[r1, #SLOT_ENV_SUST] 				; SLOT1->env_sustain = MAX_ATT_INDEX;
	str		r5,[r1, #(SLOT_SIZE+SLOT_ENV_SUST)]		; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r6,[r1, #SLOT_ENV_INCR] 					; SLOT1->envelope = release rate
	str		r7,[r1, #(SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	ldr		r6,[r1, #(2*SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	ldr		r7,[r1, #(3*SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	str		r5,[r1, #(2*SLOT_SIZE+SLOT_ENV_SUST)]	; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r5,[r1, #(3*SLOT_SIZE+SLOT_ENV_SUST)]	; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r6,[r1, #(2*SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	str		r7,[r1, #(3*SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	ldr		r6,[r1, #(4*SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	ldr		r7,[r1, #(5*SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	str		r5,[r1, #(4*SLOT_SIZE+SLOT_ENV_SUST)]	; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r5,[r1, #(5*SLOT_SIZE+SLOT_ENV_SUST)]	; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r6,[r1, #(4*SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	str		r7,[r1, #(5*SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	strb	r4, [r1, #SLOT_KEY] 						; Tell the key is off
	strb	r4, [r1, #(SLOT_SIZE+SLOT_KEY)] 			; Tell the key is off
	strb	r4, [r1, #(2*SLOT_SIZE+SLOT_KEY)] 			; Tell the key is off
	strb	r4, [r1, #(3*SLOT_SIZE+SLOT_KEY)] 			; Tell the key is off
	strb	r4, [r1, #(4*SLOT_SIZE+SLOT_KEY)] 			; Tell the key is off
	strb	r4, [r1, #(5*SLOT_SIZE+SLOT_KEY)] 			; Tell the key is off
	b		cmnd_loop
	;-------
	; Key Off => Go to RELEASE state => sustain level = MAX_ATT_INDEX, envelope = release speed
	;-------
keyoff																;	else {
	;-------
	; Handle SLOT1
	;-------
	cmp		r4, #0													; if( SLOT->key )
	beq		%f2														; {
	bics	r4, #1
	strb	r4, [r1, #SLOT_KEY] 									;	SLOT->key &= key_clr;
	bne		%f2														;	if( !SLOT->key ) {
	mvn		r5, #0xFC00
	lsl		r5, #16
	orr		r5, #MAX_ATT_INDEX
	ldr		r6,[r1, #(ch0_slot1_env_rr_incr-SLOT1)]
	str		r5,[r1, #SLOT_ENV_SUST] 								; 		SLOT1->env_sustain = MAX_ATT_INDEX;
	str		r6,[r1, #SLOT_ENV_INCR] 								; 		SLOT1->envelope = release rate
	;-------
	; Handle SLOT2
	;-------
2	ldrb	r4, [r1, #(SLOT_SIZE+SLOT_KEY)]							; } r4 = current KEY_ON value
	cmp		r4, #0													; if( SLOT->key )
	beq		cmnd_update												; {
	bics	r4, #1
	strb	r4, [r1, #(SLOT_SIZE+SLOT_KEY)]							;	SLOT->key &= key_clr;
	bne		cmnd_update												;	if( !SLOT->key ) {
	mvn		r5, #0xFC00
	lsl		r5, #16
	orr		r5, #MAX_ATT_INDEX
	ldr		r7,[r1, #(SLOT_SIZE+(ch0_slot1_env_rr_incr-SLOT1))]
	str		r5,[r1, #(SLOT_SIZE+SLOT_ENV_SUST)]		; SLOT2->env_sustain = MAX_ATT_INDEX;
	str		r7,[r1, #(SLOT_SIZE+SLOT_ENV_INCR)]		; SLOT2->envelope = release rate
	;-------
	; Registers here
	;	r1 = pointer to SLOT1 of this channel (never points to SLOT2!)
	;	r2 = current "block_fnum" of this channel
	;	r3 = new "block_fnum" of this channel
	;-------
cmnd_update															; } /* update */
	cmp		r2, r3													; if(CH->block_fnum != block_fnum)
	beq		cmnd_loop												; {
	;-------
	; r3 = block_fnum (=sound frequency) has changed, update the sound variables.
	; First calculate and save the new ksl_base value.
	; r2 = CH->ksl_base = ksl_tab[block_fnum>>6];
	;-------
	ldr		r2, =ksl_tab
	str		r3, [r1, #SLOT1_BLOCK_FNUM] 							;   Save the new CH->block_fnum = block_fnum;
	mov		r6, r3, lsr #6
	ldrb	r2, [r2, r6]											;   r2 = ksl_tab[block_fnum>>6];
	mov		r6, r3, lsr #10											;	UINT8 block  = block_fnum >> 10;
	rsb		r6, #7													;	r6 = (7-block)
	str		r2, [r1, #SLOT1_KSL_BASE] 								; 	Save CH->ksl_base = ksl_tab[block_fnum>>6];
	;-------
	; Then get the new sound frequency.
	; r4 = CH->fc = OPL->fn_tab[block_fnum&0x03ff] >> (7-block);
	;-------
	mvn		r5, #0xFC000000											; 	r5 = 0x3FF<<16
	and		r4, r3, r5, lsr #16										;	r4 = block_fnum&0x03ff
	ldr		r5, =fn_tab
	ldr		r4, [r5, r4, lsl #2]									;	r4 = OPL->fn_tab[block_fnum&0x03ff]
	lsr		r4, r6													;	r4 = (OPL->fn_tab[block_fnum&0x03ff] >> (7-block))
	str		r4, [r1, #SLOT1_FC] 									; CH->fc       = OPL->fn_tab[block_fnum&0x03ff] >> (7-block);
	;-------
	; Then get the new kcode value.
	; r5 = CH->kcode = (CH->block_fnum&0x1c00)>>9;
	;-------
	and		r5, r3, #0x1C00											;	/* BLK 2,1,0 bits -> bits 3,2,1 of kcode */
	lsr		r5, #9													;	CH->kcode    = (CH->block_fnum&0x1c00)>>9;
	;-------
	; Change the kcode value by keyboard split point. TODO!
	;	if (OPL->mode&0x40)
	;		CH->kcode |= (CH->block_fnum&0x100)>>8;	/* notesel == 1 */
	;	else
	;		CH->kcode |= (CH->block_fnum&0x200)>>9;	/* notesel == 0 */
	; r5 = CH->kcode |= (CH->block_fnum&0x200)>>9;	/* notesel == 0 */
	;-------
	mov		r6, r3, lsr #9
	and		r6, #1
	orr		r5, r6													; CH->kcode |= (CH->block_fnum&0x200)>>9;	/* notesel == 0 */
	strb	r5, [r1, #SLOT1_KCODE]
	;=======
	; Handle SLOT1 of this channel
	;=======
	;-------
	; Calculate	SLOT->TLL = SLOT->TL + (CH->ksl_base>>SLOT->ksl);
	; Input
	;	r1 = SLOT pointer
	;	r2 = CH->ksl_base
	; Destroys
	;	r6, r7, r8
	;-------
	ldrb	r6, [r1, #(ch0_slot1_ksl-SLOT1)] 						; r6 = CH->SLOT[SLOT1].ksl
	ldr		r8, [r1, #(ch0_slot1_TL-SLOT1)] 						; r8 = CH->SLOT[SLOT1].TL
	mov		r7, r2, lsr r6											; r7 = (CH->ksl_base>>CH->SLOT[SLOT1].ksl)
	add		r7, r8
	str		r7, [r1, #SLOT_TLL] 									; CH->SLOT[SLOT1].TLL = CH->SLOT[SLOT1].TL + (CH->ksl_base>>CH->SLOT[SLOT1].ksl);
	;-------
	; Calculate	SLOT->Incr = CH->fc * SLOT->mul;
	; Input
	;	r4 = CH->fc
	;-------
	ldrb	r6, [r1, #SLOT_MUL] 									; r6 = CH->SLOT[SLOT1].mul
	mul		r7, r4, r6
	str		r7, [r1, #SLOT_INCR] 									; SLOT->Incr = CH->fc * SLOT->mul;
	;-------
	; Calculate	SLOT->ksr = CH->kcode >> SLOT->KSR;
	; Input
	;	r5 = CH->kcode
	;-------
	ldrb	r6, [r1, #SLOT_BITS] 									; r6 = bits (lowest bit = KSR value)
	and		r6, #1
	lsl		r6, #1													; r6 = 0 or 2
	ldrb	r7, [r1, #(ch0_slot1_ksr-SLOT1)] 						; r7 = CH->SLOT[SLOT1].ksr
	mov		r6, r5, lsr r6											; ksr = CH->kcode >> SLOT->KSR;
	cmp		r6, r7													; if( SLOT->ksr != ksr )
	beq		cmnd_slot1_done											; {
	strb	r6, [r1, #(ch0_slot1_ksr-SLOT1)] 						; 	SLOT->ksr = ksr;
	;-------
	; Calculate env_ar_incr, env_dr_incr and env_rr_incr
	;-------
	ldr		r8, =att_tab
	ldr		r7, [r1, #(ch0_slot1_ar-SLOT1)]							; r7 = SLOT->ar
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->ar + SLOT->ksr]
;	orr		r7, #1
	str		r7, [r1, #(ch0_slot1_env_ar_incr-SLOT1)]				; Save new env_ar_incr value
	ldr		r8, =env_tab
	ldr		r7, [r1, #(ch0_slot1_dr-SLOT1)]							; r7 = SLOT->dr
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->dr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_dr_incr-SLOT1)]				; Save new env_dr_incr value
	ldr		r7, [r1, #(ch0_slot1_rr-SLOT1)]							; r7 = SLOT->dr
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->rr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_rr_incr-SLOT1)]				; Save new env_rr_incr value
cmnd_slot1_done
	;=======
	; Handle SLOT2 of this channel
	;=======
	add		r1, #SLOT_SIZE
	;-------
	; Calculate	SLOT->TLL = SLOT->TL + (CH->ksl_base>>SLOT->ksl);
	; Input
	;	r1 = SLOT pointer
	;	r2 = CH->ksl_base
	; Destroys
	;	r6, r7, r8
	;-------
	ldrb	r6, [r1, #(ch0_slot1_ksl-SLOT1)] 						; r6 = CH->SLOT[SLOT1].ksl
	ldr		r8, [r1, #(ch0_slot1_TL-SLOT1)] 						; r8 = CH->SLOT[SLOT1].TL
	mov		r7, r2, lsr r6											; r7 = (CH->ksl_base>>CH->SLOT[SLOT1].ksl)
	add		r7, r8
	str		r7, [r1, #SLOT_TLL] 									; CH->SLOT[SLOT1].TLL = CH->SLOT[SLOT1].TL + (CH->ksl_base>>CH->SLOT[SLOT1].ksl);
calc_fcslot
	;-------
	; Calculate	SLOT->Incr = CH->fc * SLOT->mul;
	; Input
	;	r1 = SLOT pointer
	;	r4 = CH->fc
	; TODO! We are skipping the envelope KSR handling
	;-------
	ldrb	r6, [r1, #SLOT_MUL] 									; r6 = CH->SLOT[SLOT1].mul
	mul		r7, r4, r6
	str		r7, [r1, #SLOT_INCR] 									; SLOT->Incr = CH->fc * SLOT->mul;
	;-------
	; Calculate	SLOT->ksr = CH->kcode >> SLOT->KSR;
	; Input
	;	r1 = SLOT pointer
	;	r5 = CH->kcode
	;-------
	ldrb	r6, [r1, #SLOT_BITS] 									; r6 = CH->SLOT[SLOT1].KSR
	and		r6, #1
	lsl		r6, #1													; r6 = 0 or 2
	ldrb	r7, [r1, #(ch0_slot1_ksr-SLOT1)] 						; r7 = CH->SLOT[SLOT1].ksr
	mov		r6, r5, lsr r6											; ksr = CH->kcode >> SLOT->KSR;
	cmp		r6, r7													; if( SLOT->ksr != ksr )
	beq		cmnd_loop												; {
	strb	r6, [r1, #(ch0_slot1_ksr-SLOT1)] 						; 	SLOT->ksr = ksr;
	;-------
	; Calculate env_ar_incr, env_dr_incr and env_rr_incr
	;-------
	ldr		r8, =att_tab
	ldr		r7, [r1, #(ch0_slot1_ar-SLOT1)]							; r7 = SLOT->ar
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->ar + SLOT->ksr]
;	orr		r7, #1
	str		r7, [r1, #(ch0_slot1_env_ar_incr-SLOT1)]				; Save new env_ar_incr value
	ldr		r8, =env_tab
	ldr		r7, [r1, #(ch0_slot1_dr-SLOT1)]							; r7 = SLOT->dr
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->dr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_dr_incr-SLOT1)]				; Save new env_dr_incr value
	ldr		r7, [r1, #(ch0_slot1_rr-SLOT1)]							; r7 = SLOT->dr
	add		r7, r6
	ldr		r7, [r8, r7, lsl #2]									; r7 = env_tab[SLOT->rr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_rr_incr-SLOT1)]				; Save new env_rr_incr value
	b		cmnd_loop
	;=======
	
	MACRO
	r1r2_slot_from_r0
	ldr		r2, =slot_array
	and		r1, r0, #0x1F00
	lsr		r1, #8
	ldrb	r2, [r2, r1]											; slot = slot_array[r&0x1f];
	cmp		r2, #0xFF												; if(slot < 0)
	beq		cmnd_loop												;	return;
	ldr		r1, =SLOT1
	mov		r3, #SLOT_SIZE
	mul		r4, r3, r2
	add		r1, r4													; r1 = SLOT pointer, r2 = slot number
	MEND

	;=======
	; 80..95: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |     Sustain Level     |     Release Rate      |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	;=======
cmnd_80_90	
	r1r2_slot_from_r0
	;-------
	; First calculate and store the sustain level
	; SLOT->sl  = sl_tab[ v>>4 ];
	;-------
	ldr		r2, =sl_tab
	and		r3, r0, #0xF0
	lsr		r3, #3
	ldrh	r2, [r2, r3]											; r2 = sl_tab[ v>>4 ];
	str		r2, [r1, #(ch0_slot1_sl-SLOT1)]							; SLOT->sl  = sl_tab[ v>>4 ];
	;-------
	; Then calculate and store the release rate.
	; SLOT->rr  = (v&0x0f)? 16 + ((v&0x0f)<<2) : 0;
	;-------
	ands	r0, #0x0F
	lslne	r0, #2
	addne	r0, #16
	str		r0, [r1, #(ch0_slot1_rr-SLOT1)]							; Save SLOT->rr
	;-------
	; Also calculate env_rr_incr
	;-------
	ldrb	r6, [r1, #(ch0_slot1_ksr-SLOT1)] 						; Get SLOT->ksr
	ldr		r8, =env_tab
	add		r0, r6
	ldr		r7, [r8, r0, lsl #2]									; r7 = env_tab[SLOT->rr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_rr_incr-SLOT1)]				; Save new env_rr_incr value
	b		cmnd_loop


	;=======
	; 40..55: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |Key Scaling| -24 | -12 | -6  | -3  |-1.5 |-0.75|
	; |Level      |  dB |  dB | dB  | dB  | dB  | dB  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; Bits 7-6
	; 	00 = no change
	;	01 = 3dB/Octave
	;	10 = 1.5dB/Octave
	;	11 = 6dB/Octave
	;=======
cmnd_40_50	
	r1r2_slot_from_r0
	;-------
	; Calculate and store the ksl value
	;-------
	ldr		r4, =ksl_level
	mov		r3, r0, lsr #6
	and		r3, #3
	ldrb	r4, [r4, r3]						; r4 = ksl_level[(v>>6)&3]
	strb	r4, [r1, #(ch0_slot1_ksl-SLOT1)]	; SLOT->ksl = ksl_level[(v>>6)&3];      /* 0 / 3.0 / 1.5 / 6.0 dB/OCT */
	;-------
	; Calculate and store the total level
	;-------
	and		r3, r0, #0x3F
	lsl		r3, #(10-1-7)						; r3 = (v&0x3f)<<(ENV_BITS-1-7);
	str		r3, [r1, #(ch0_slot1_TL-SLOT1)]		; SLOT->TL  = (v&0x3f)<<(ENV_BITS-1-7); /* 7 bits TL (bit 6 = always 0) */
	;-------
	; Calculate and store the current total level, based on ksl_base
	;-------
	tst		r2, #1								; Is this even or odd slot number?
	ldrne	r2, [r1, #SLOT2_KSL_BASE]			; SLOT 2, so ksl_base is in this slot
	ldreq	r2, [r1, #SLOT1_KSL_BASE]			; SLOT 1, so ksl_base is in the next slot
	mov		r5, r2, lsr r4
	add		r3, r5
	str		r3, [r1, #SLOT_TLL]					; SLOT->TLL = SLOT->TL + (CH->ksl_base>>SLOT->ksl);
	b		cmnd_loop

	
	;=======
	; 60..75: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |     Attack Rate       |     Decay Rate        |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	;=======
cmnd_60_70	
	r1r2_slot_from_r0
	;-------
	; Calculate and store the attack rate.
	;-------
	mov		r2, #16
	ands	r4, r0, #0xF0
	lsr		r4, #2
	moveq	r2, #0
	add		r2, r4													; r2 = (v>>4)  ? 16 + ((v>>4)  <<2) : 0;
	str		r2, [r1, #(ch0_slot1_ar-SLOT1)]							; SLOT->ar = (v>>4)  ? 16 + ((v>>4)  <<2) : 0;
	;-------
	; Also calculate env_ar_incr
	;-------
	ldrb	r6, [r1, #(ch0_slot1_ksr-SLOT1)] 						; Get SLOT->ksr
	ldr		r8, =att_tab
	add		r2, r6
	ldr		r7, [r8, r2, lsl #2]									; r7 = env_tab[SLOT->ar + SLOT->ksr]
	orr		r7, #1
	str		r7, [r1, #(ch0_slot1_env_ar_incr-SLOT1)]				; Save new env_ar_incr value
	;-------
	; Then calculate and store the decay rate.
	; SLOT->dr    = (v&0x0f)? 16 + ((v&0x0f)<<2) : 0;
	;-------
	ands	r0, #0x0F
	lslne	r0, #2
	addne	r0, #16
	str		r0, [r1, #(ch0_slot1_dr-SLOT1)]							; Save SLOT->dr
	;-------
	; Also calculate env_dr_incr
	;-------
	ldr		r8, =env_tab
	add		r0, r6
	ldr		r7, [r8, r0, lsl #2]									; r7 = env_tab[SLOT->dr + SLOT->ksr]
	str		r7, [r1, #(ch0_slot1_env_dr_incr-SLOT1)]				; Save new env_dr_incr value
	b		cmnd_loop

	
	;=======
	; 20..35: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; | Amp | Vib | EG  | KSR |  Modulator Frequency  |
	; | Mod |     |type |     |       Multiple        |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; bit 7 - Apply amplitude modulation when set, AM depth is controlled by the AM-Depth flag in address BD.  
	; bit 6 - Apply vibrato when set, vibrato depth is controlled by the Vib-Depth flag in address BD.  
	; bit 5 - When set, the sustain level of the voice is maintained until released,
	;		  when clear, the sound begins to decay immediately after hitting the SUSTAIN phase.  
	; bit 4 - Keyboard scaling rate. If this bit is set, the envelope is foreshortened as it rises in pitch.
	; bits 3-0 - These bits indicate which harmonic the operator will produce sound (or modulation) in relation to the specified frequency
	;  0 - one octave below 
	;  1 - at the specified frequency 
	;  2 - one octave above 
	;  3 - an octave and a fifth above 
	;  4 - two octaves above 
	;  5 - two octaves and a major third above 
	;  6 - two octaves and a fifth above 
	;  7 - two octaves and a minor seventh above 
	;  8 - three octaves above 
	;  9 - three octaves and a major second above 
	;  A - three octaves and a major third above 
	;  B - three octaves and a major third above 
	;  C - three octaves and a fifth above 
	;  D - three octaves and a fifth above 
	;  E - three octaves and a major seventh above 
	;  F - three octaves and a major seventh above 
	;=======
cmnd_20_30	
	r1r2_slot_from_r0
	;-------
	; Calculate and store the Modulator Multiple
	;-------
	ldr		r4,=mul_tab
	and		r3, r0, #0x0F
	ldrb	r3, [r4, r3]											; r3 = mul_tab[v&0x0f];
	strb	r3, [r1, #SLOT_MUL]										; SLOT->mul = mul_tab[v&0x0f];
	;-------
	; Calculate and store the bits
	;-------
	ldrb	r3, [r1, #SLOT_BITS]									; Get current bits
	lsr		r0, #4
	and		r0, #0x0F
	eor		r0, #1													; Store the KSR bit opposite to its value
	bic		r3, #0x0F
	orr		r0, r3
	strb	r0, [r1, #SLOT_BITS]									; Store AM, Vib, EG type, KSR bits
	;-------
	; Go to calculating frequency etc.
	;-------
	tst		r2, #1													; Is this even or odd slot number?
	ldrne	r4, [r1, #SLOT2_FC]										; SLOT 2, so fc is in this slot
	ldreq	r4, [r1, #SLOT1_FC]										; SLOT 1, so fc is in the next slot
	ldrbne	r5, [r1, #SLOT2_KCODE]									; SLOT 2, so kcode is in this slot
	ldrbeq	r5, [r1, #SLOT1_KCODE]									; SLOT 1, so kcode is in the next slot
	b		calc_fcslot
	
	;=======
	; E0..F5: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |              Unused               | Waveform  |
	; |                                   |  select   |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	;=======
cmnd_E0_F0	
	r1r2_slot_from_r0
	;-------
	; Calculate and store the new waveform.
	;-------
	ldr		r2,=sin_tab
	and		r0, #3
	lsl		r0, #12													; Each wavetable is 1024 words = 4096 bytes in size
	add		r0, r2
	str		r0, [r1, #SLOT_WAVETABLE]								; SLOT->wavetable = sin_tab + (v&0x3)*SIN_LEN;
	b		cmnd_loop
	
	;=======
	; C0..C8: 
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; |        Unused         |    Feedback     |Conn.|
	; |                       |    strength     |     |
	; +-----+-----+-----+-----+-----+-----+-----+-----+
	; First calculate the channel (0..8) of the operation and make
	; r1 = channel SLOT1 pointer
	;=======
cmnd_C0_D0	
	and		r1, r0, #0x0F00
	cmp		r1, #0x0800							; if( (r&0x0f) > 8)
	bgt		cmnd_loop							;	 return;
	mov		r2, r1, lsr #8
	ldr		r1,=SLOT1
	mov		r3, #(2*SLOT_SIZE)
	mul		r4, r3, r2
	add		r1, r4								; CH = &OPL->P_CH[r&0x0f];
	;-------
	; Save the feedback (0=>0, 1..7 => 7..1) and connection bit
	;-------
	ldrb	r3, [r1, #SLOT_BITS]				; Get current bits of SLOT1
	ldrb	r4, [r1, #SLOT2_BITS]				; Get current bits of SLOT2
	and		r3, #0x0F
	and		r4, #0x0F
	tst		r0, #1								; Zero flag = "Conn" bit on/off?
	orrne	r3, #0x10
	orrne	r4, #0x10
	and		r0, #0x0E							; r0 = "Feedback strength"
	rsb		r0, #0x10
	and		r0, #0x0E
	orr		r3, r0, lsl #4
	orr		r4, r0, lsl #4
	strb	r3, [r1, #SLOT_BITS]				; Save new bits to SLOT1
	strb	r4, [r1, #SLOT2_BITS]				; Save new bits to SLOT2
	b		cmnd_loop


	AREA 	adlib_rodata, ALIGN=4, DATA, READONLY
	ALIGN	4
	
env_tab
	DCD	 0, 0, 0, 0, 0, 0, 0, 0
	DCD	 0, 0, 0, 0, 0, 0, 0, 0
	DCD	 24, 30, 36, 42, 48, 60, 72, 84
	DCD	 98, 122, 146, 170, 194, 242, 292, 340
	DCD	 388, 486, 582, 680, 776, 970, 1164, 1358
	DCD	 1554, 1942, 2330, 2718, 3108, 3884, 4662, 5438
	DCD	 6214, 7768, 9322, 10876, 12428, 15536, 18642, 21750
	DCD	 24858, 31072, 37286, 43500, 49716, 62144, 74574, 87002
	DCD	 99432, 124290, 149148, 174006, 198862, 248578, 298294, 348010
	DCD	 397724, 497156, 596588, 696020, 795448, 795448, 795448, 795448
	DCD	 795448, 795448, 795448, 795448, 795448, 795448, 795448, 795448
	DCD	 795448, 795448, 795448, 795448, 795448, 795448, 795448, 795448

att_tab
	DCD	 -1, -1, -1, -1, -1, -1, -1, -1
	DCD	 -1, -1, -1, -1, -1, -1, -1, -1
	DCD	 -823, -891, -939, -969, -989, -1013, -1079, -1273
	DCD	 -1439, -1805, -2157, -2545, -2877, -3609, -4315, -5089
	DCD	 -5753, -7217, -8631, -10179, -11507, -14435, -17263, -20357
	DCD	 -23017, -28869, -34525, -40715, -46033, -57739, -69049, -81481
	DCD	 -92129, -115479, -138099, -162963, -184511, -230957, -276767, -326721
	DCD	 -370043, -461915, -553535, -656645, -744197, -930247, -1116297, -1313291
	DCD	 -1488395, -1860495, -2232593, -2679111, -2912077, -3525147, -4465187, -5152137
	DCD	 -5581483, -7441977, -9568257, -11162965, -13395559, -13395559, -13395559, -13395559
	DCD	 -66977793, -66977793, -66977793, -66977793, -66977793, -66977793, -66977793, -66977793
	DCD	 -66977793, -66977793, -66977793, -66977793, -66977793, -66977793, -66977793, -66977793
	
ksl_tab
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	8
	DCB	12
	DCB	16
	DCB	20
	DCB	24
	DCB	28
	DCB	32
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	12
	DCB	20
	DCB	28
	DCB	32
	DCB	40
	DCB	44
	DCB	48
	DCB	52
	DCB	56
	DCB	60
	DCB	64
	DCB	0
	DCB	0
	DCB	0
	DCB	20
	DCB	32
	DCB	44
	DCB	52
	DCB	60
	DCB	64
	DCB	72
	DCB	76
	DCB	80
	DCB	84
	DCB	88
	DCB	92
	DCB	96
	DCB	0
	DCB	0
	DCB	32
	DCB	52
	DCB	64
	DCB	76
	DCB	84
	DCB	92
	DCB	96
	DCB	104
	DCB	108
	DCB	112
	DCB	116
	DCB	120
	DCB	124
	DCB	128
	DCB	0
	DCB	32
	DCB	64
	DCB	84
	DCB	96
	DCB	108
	DCB	116
	DCB	124
	DCB	128
	DCB	136
	DCB	140
	DCB	144
	DCB	148
	DCB	152
	DCB	156
	DCB	160
	DCB	0
	DCB	64
	DCB	96
	DCB	116
	DCB	128
	DCB	140
	DCB	148
	DCB	156
	DCB	160
	DCB	168
	DCB	172
	DCB	176
	DCB	180
	DCB	184
	DCB	188
	DCB	192
	DCB	0
	DCB	96
	DCB	128
	DCB	148
	DCB	160
	DCB	172
	DCB	180
	DCB	188
	DCB	192
	DCB	200
	DCB	204
	DCB	208
	DCB	212
	DCB	216
	DCB	220
	DCB	224

slot_array
	DCB	0
	DCB	2
	DCB	4
	DCB	1
	DCB	3
	DCB	5
	DCB	-1
	DCB	-1
	DCB	6
	DCB	8
	DCB	10
	DCB	7
	DCB	9
	DCB	11
	DCB	-1
	DCB	-1
	DCB	12
	DCB	14
	DCB	16
	DCB	13
	DCB	15
	DCB	17
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1
	DCB	-1

	ALIGN	2
sl_tab
	DCW	0
	DCW	16
	DCW	32
	DCW	48
	DCW	64
	DCW	80
	DCW	96
	DCW	112
	DCW	128
	DCW	144
	DCW	160
	DCW	176
	DCW	192
	DCW	208
	DCW	224
	DCW	496

ksl_level
	DCB	31
	DCB	1
	DCB	2
	DCB	0

mul_tab
	DCB	1
	DCB	2
	DCB	4
	DCB	6
	DCB	8
	DCB	10
	DCB	12
	DCB	14
	DCB	16
	DCB	18
	DCB	20
	DCB	20
	DCB	24
	DCB	24
	DCB	30
	DCB	30

cmnd_ch_tab															; This table maps commands to their channels
	DCB	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 0, 1, 2, 0, 1, 2, -1, -1, 3, 4, 5, 3, 4, 5, -1, -1			; 0x20-0x35 = AM, Vib, EG Type, KSR, Multiple
	DCB	 6, 7, 8, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 0, 1, 2, 0, 1, 2, -1, -1, 3, 4, 5, 3, 4, 5, -1, -1			; 0x40-0x55 = Key Scaling Level / Operator Output Level
	DCB	 6, 7, 8, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 0, 1, 2, 0, 1, 2, -1, -1, 3, 4, 5, 3, 4, 5, -1, -1			; 0x60-0x75 = Attack Rate / Decay Rate
	DCB	 6, 7, 8, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1

	DCB	 0, 1, 2, 0, 1, 2, -1, -1, 3, 4, 5, 3, 4, 5, -1, -1			; 0x80-0x95 = Sustain Level / Release Rate
	DCB	 6, 7, 8, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1		; 0xA0-0xA8 = F-Number LSB
	DCB	 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, 6, -1, -1		; 0xB0-0xB8 = KeyOn, Octave, F-Number MSB, 0xBD = Rhythm
	DCB	 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1		; 0xC0-0xC8 = Feedback/Algorithm
	DCB	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
	DCB	 0, 1, 2, 0, 1, 2, -1, -1, 3, 4, 5, 3, 4, 5, -1, -1			; 0xE0-0xF5 = Waveform
	DCB	 6, 7, 8, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1

	ALIGN	8	; 4 low-order bits of the address must be zero!
lfo_pm_table
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	0
	DCB	1
	DCB	0
	DCB	0
	DCB	0
	DCB	-1
	DCB	0
	DCB	0
	DCB	0
	DCB	1
	DCB	0
	DCB	0
	DCB	0
	DCB	-1
	DCB	0
	DCB	0
	DCB	0
	DCB	2
	DCB	1
	DCB	0
	DCB	-1
	DCB	-2
	DCB	-1
	DCB	0
	DCB	1
	DCB	1
	DCB	0
	DCB	0
	DCB	0
	DCB	-1
	DCB	0
	DCB	0
	DCB	0
	DCB	3
	DCB	1
	DCB	0
	DCB	-1
	DCB	-3
	DCB	-1
	DCB	0
	DCB	1
	DCB	2
	DCB	1
	DCB	0
	DCB	-1
	DCB	-2
	DCB	-1
	DCB	0
	DCB	1
	DCB	4
	DCB	2
	DCB	0
	DCB	-2
	DCB	-4
	DCB	-2
	DCB	0
	DCB	2
	DCB	2
	DCB	1
	DCB	0
	DCB	-1
	DCB	-2
	DCB	-1
	DCB	0
	DCB	1
	DCB	5
	DCB	2
	DCB	0
	DCB	-2
	DCB	-5
	DCB	-2
	DCB	0
	DCB	2
	DCB	3
	DCB	1
	DCB	0
	DCB	-1
	DCB	-3
	DCB	-1
	DCB	0
	DCB	1
	DCB	6
	DCB	3
	DCB	0
	DCB	-3
	DCB	-6
	DCB	-3
	DCB	0
	DCB	3
	DCB	3
	DCB	1
	DCB	0
	DCB	-1
	DCB	-3
	DCB	-1
	DCB	0
	DCB	1
	DCB	7
	DCB	3
	DCB	0
	DCB	-3
	DCB	-7
	DCB	-3
	DCB	0
	DCB	3

	END
