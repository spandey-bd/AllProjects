//=============================================================================
// fpu.s
//
// This file contains Floating Point Unit opcode emulation handlers.
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
// LOSS OF USE, DATA, OR PROFITS// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//=============================================================================

#if defined(RPi)
	.arch armv6
#elif defined(Roku)
	.cpu arm1176jzf-s
	.fpu vfp
#else
	.arch armv7-a
#endif

	.file	"fpu.s"

#include "defines.inc"
#include "macros.inc"
#include "gen_32.inc"

#define		LOG_FPU		0

	.text
	.align	2

#define	TAG_Valid 		0
#define	TAG_Zero 		1
#define	TAG_Weird 		2
#define	TAG_Empty 		3

#define	float_round_nearest_even	0
#define	float_round_down			1
#define	float_round_up				2
#define	float_round_to_zero			3

#define	float_flag_invalid   1
#define	float_flag_divbyzero 4
#define	float_flag_overflow  8
#define	float_flag_underflow 16
#define	float_flag_inexact   32

//---------------------------------------------------------------------------
// Control and status word macros
//---------------------------------------------------------------------------

#define	FPU_C0_FLAG		0x0100
#define	FPU_C1_FLAG		0x0200
#define	FPU_C2_FLAG		0x0400
#define	FPU_C3_FLAG		0x4000


.macro FPU_FPOP_r0r1r2
	//-------
	//		fpu.tags[TOP]=TAG_Empty;
	//		TOP = ((TOP+1)&7);
	//-------
	ldr		r2, =GP_fpu_tags
	ldrb	r1, [r2, #(GP_fpu_top - GP_fpu_tags)]
	mov		r0, #TAG_Empty				
	strb	r0, [r2, r1]				// fpu.tags[TOP]=TAG_Empty;
	add		r1, #1
	and		r1, #7
	strb	r1, [r2, #(GP_fpu_top - GP_fpu_tags)]
.endm

.macro FPU_PREP_PUSH_r0r1r3
	//-------
	//		TOP = (TOP - 1) & 7;
	//		fpu.tags[TOP]=TAG_Valid;
	//-------
	ldr		r3, =GP_fpu_tags
	ldrb	r1, [r3, #(GP_fpu_top - GP_fpu_tags)]
	mov		r0, #TAG_Valid
	sub		r1, #1
	and		r1, #7
	strb	r1, [r3, #(GP_fpu_top - GP_fpu_tags)]
	strb	r0, [r3, r1]				// fpu.tags[TOP]=TAG_Valid;
.endm

.macro FPU_SetCW_r3 reg
	//------
	//	fpu.cw = (Bit16u)word;
	//	fpu.cw_mask_all = (Bit16u)(word | 0x3f);
	//	fpu.round = (FPU_Round)((word >> 10) & 3);
	//------
	ldr		r3, =GP_fpu_regs
	strh	\reg, [r3, #(GP_fpu_cw - GP_fpu_regs)]
	orr		\reg, #0x3F
	strh	\reg, [r3, #(GP_fpu_cw_mask_all - GP_fpu_regs)]
#if defined(RPi) || defined(Roku)
	lsr		\reg, #10
	and		\reg, #3
#else
	ubfx	\reg, \reg, #10, #2
#endif
	strb	\reg, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
.endm

.macro FPU_GET_TOP_r0r3
	//------
	//	return (fpu.sw & 0x3800)>>11;
	//------
	ldr		r3, =GP_fpu_regs
	ldrh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
#if defined(RPi) || defined(Roku)
	lsr		r0, #11
	and		r0, #7
#else
	ubfx	r0, r0, #11, #3
#endif
	strb	r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
.endm

.macro FPU_SET_TOP_r0r1r3
	//------
	// fpu.sw &= ~0x3800;
	// fpu.sw |= (val&7)<<11;
	//------
	ldr		r3, =GP_fpu_regs
	ldrh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(RPi) || defined(Roku)
	bic		r0, #0x3800
	orr		r0, r1, lsl #11
#else
	bfi		r0, r1, #11, #3
#endif
	strh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
.endm

	
//---------------------------------------------------------------------------
// Opcode handlers
//---------------------------------------------------------------------------

// ------------------- D8 = ESC(0) ---------------------
// All modrm variations implemented!
//
// 0150:18B88A	D8C9	fmul	st(1)		(X-COM UFO)
// 0150:18B89C	D8C8	fmul	st(0)		(X-COM UFO)
// 0150:18B890	D84C24	fmul	[esp+14]	(X-COM UFO)

	.global	op_d8_USE32
op_d8_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, f32
	//0xc0 = mod = 11b => two register operands
	.word fadd_st0, fadd_st1, fadd_st2, fadd_st3, fadd_st4, fadd_st5, fadd_st6, fadd_st7
	.word fmul_st0, fmul_st1, fmul_st2, fmul_st3, fmul_st4, fmul_st5, fmul_st6, fmul_st7
	.word fcom_st0, fcom_st1, fcom_st2, fcom_st3, fcom_st4, fcom_st5, fcom_st6, fcom_st7
	.word fcomp_st0, fcomp_st1, fcomp_st2, fcomp_st3, fcomp_st4, fcomp_st5, fcomp_st6, fcomp_st7
	.word fsub_st0, fsub_st1, fsub_st2, fsub_st3, fsub_st4, fsub_st5, fsub_st6, fsub_st7
	.word fsubr_st0, fsubr_st1, fsubr_st2, fsubr_st3, fsubr_st4, fsubr_st5, fsubr_st6, fsubr_st7
	.word fdiv_st0, fdiv_st1, fdiv_st2, fdiv_st3, fdiv_st4, fdiv_st5, fdiv_st6, fdiv_st7
	.word fdivr_st0, fdivr_st1, fdivr_st2, fdivr_st3, fdivr_st4, fdivr_st5, fdivr_st6, fdivr_st7

	modrm_genall_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, _t0, f32

fadd_t0_bp_f32:
	mem_handler_bp
fadd_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FADD_EA(TOP);
	//		FPU_FADD(op1,8);
	//------
fadd_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vadd.f64    	d0, d0, d1
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fmul_t0_bp_f32:
	mem_handler_bp
fmul_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FMUL_EA(TOP);
	//		FPU_FMUL(op1,8);
	//------
fmul_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vmul.f64    	d0, d0, d1
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fcom_t0_bp_f32:
	mem_handler_bp
fcom_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FCOM_EA(TOP);
	//		FPU_FCOM(op1,8)//					// Compare TOP with the loaded value
	//------
fcom_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	mrs				r0,cpsr								// Save flags to r0
	vcmp.f64		d1, d0
	vmrs        	apsr_nzcv,fpscr						// Get the ARM CPU flags from FPU flags
	//------
	// if NaN (N=0,Z=0,C=1,V=1)
	//	FPU_SET_C3(1)//FPU_SET_C2(1)//FPU_SET_C0(1);
	//------
	mrs				r1,cpsr								// Get the FPU comparison flags to r1
	ldrh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	and				r1, #(ARM_NEG|ARM_ZERO|ARM_CARRY|ARM_OVER)
	cmp				r1, #(ARM_CARRY|ARM_OVER)
	bic				r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)
	orreq			r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)	// Mark for NaN!
	msr				cpsr_f, r1							// Restore the FPU comparison flags to ARM CPU flags
	//------
	// else if (st == other)
	//	FPU_SET_C3(1)//FPU_SET_C2(0)//FPU_SET_C0(0);
	//------
	orreq			r3, #(FPU_C3_FLAG)
	//------
	// else if (st < other)
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(1)//return;
	//------
	orrlt			r3, #(FPU_C0_FLAG)
	//------
	// else
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(0)//return;
	//------
	strh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	b				restore_flags_from_r0
	
fcomp_t0_bp_f32:
	mem_handler_bp
fcomp_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FCOM_EA(TOP);
	//		FPU_FCOM(op1,8)//					// Compare TOP with the loaded value
	//------
fcomp_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	mrs				r0,cpsr								// Save flags to r0
	vcmp.f64		d1, d0
	vmrs        	apsr_nzcv,fpscr						// Get the ARM CPU flags from FPU flags
	//------
	// if NaN (N=0,Z=0,C=1,V=1)
	//	FPU_SET_C3(1)//FPU_SET_C2(1)//FPU_SET_C0(1);
	//------
	mrs				r1,cpsr								// Get the FPU comparison flags to r1
	ldrh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	and				r1, #(ARM_NEG|ARM_ZERO|ARM_CARRY|ARM_OVER)
	cmp				r1, #(ARM_CARRY|ARM_OVER)
	bic				r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)
	orreq			r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)	// Mark for NaN!
	msr				cpsr_f, r1							// Restore the FPU comparison flags to ARM CPU flags
	//------
	// else if (st == other)
	//	FPU_SET_C3(1)//FPU_SET_C2(0)//FPU_SET_C0(0);
	//------
	orreq			r3, #(FPU_C3_FLAG)
	//------
	// else if (st < other)
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(1)//return;
	//------
	orrlt			r3, #(FPU_C0_FLAG)
	//------
	// else
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(0)//return;
	//------
	strh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	msr				cpsr_f, r0							// Restore the original ARM CPU flags
	//------
	// FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fsub_t0_bp_f32:
	mem_handler_bp
fsub_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FSUB_EA(TOP);
	//		FPU_FSUB(op1,8)//					// Subtract the loaded value from TOP, result into TOP
	//------
fsub_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vsub.f64    	d0,d1,d0							// d0 = st(TOP) - other
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fsubr_t0_bp_f32:
	mem_handler_bp
fsubr_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FSUBR_EA(TOP);
	//		FPU_FSUBR(op1,8)//					// Subtract TOP from the loaded value, result into TOP
	//------
fsubr_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vsub.f64    	d0,d0,d1							// d0 = other - st(TOP)
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fdiv_t0_bp_f32:
	mem_handler_bp
fdiv_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FDIV_EA(TOP);
	//		FPU_FDIV(op1,8)//					// Divide TOP by the loaded value, put result into TOP
	//------
fdiv_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vdiv.f64    	d0,d1,d0							// d0 = st(TOP) / other
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fdivr_t0_bp_f32:
	mem_handler_bp
fdivr_t0_f32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FLD_F32_EA(addr);
	//	FPU_FLD_F32(addr,8);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//------
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//------
	vcvt.f64.f32	d0, s0
	//------
	// EATREE(rm);
	//	FPU_FDIVR_EA(TOP);
	//		FPU_FDIVR(op1,8)//					// Divide the loaded value by TOP, put result into TOP
	//------
fdivr_TOP_d0_cont:
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	vdiv.f64    	d0,d0,d1							// d0 = other / st(TOP)
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fadd_st0:
	mov		r1, #0
	b		fadd_ST
fadd_st2:
	mov		r1, #2
	b		fadd_ST
fadd_st3:
	mov		r1, #3
	b		fadd_ST
fadd_st4:
	mov		r1, #4
	b		fadd_ST
fadd_st5:
	mov		r1, #5
	b		fadd_ST
fadd_st6:
	mov		r1, #6
	b		fadd_ST
fadd_st7:
	mov		r1, #7
	b		fadd_ST
fadd_st1:
	mov		r1, #1
fadd_ST:	
	//------
	// FPU_FADD(TOP,STV(sub));
	//	fpuregs[st]d += fpuregs[other]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fadd_TOP_d0_cont					// Add TOP with st(other) and store the result to TOP

fmul_st0:
	mov		r1, #0
	b		fmul_ST
fmul_st2:
	mov		r1, #2
	b		fmul_ST
fmul_st3:
	mov		r1, #3
	b		fmul_ST
fmul_st4:
	mov		r1, #4
	b		fmul_ST
fmul_st5:
	mov		r1, #5
	b		fmul_ST
fmul_st6:
	mov		r1, #6
	b		fmul_ST
fmul_st7:
	mov		r1, #7
	b		fmul_ST
fmul_st1:
	mov		r1, #1
fmul_ST:	
	//------
	// FPU_FMUL(TOP,STV(sub));
	//	fpuregs[st]d*=fpuregs[other]d;
	//------
	//------
	// FMUL (Multiply two floating point values) 
	// Syntax    fmul Src
	//            fmul Dest,Src
	// Note FMUL without operands can also be used with the MASM assembler
	//		 but such an instruction is coded as FMULP ST(1),ST(0)
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Overflow, Underflow, Precision
	// This instruction performs a signed multiplication of the floating point values
	// from the source (Src) and the destination (Dest) and overwrites the content of
	// the destination data register with the result// the value of the source remains unchanged
	// If only the source is specified, the TOP data register ST(0) is the implied
	// destination and the source must be the memory address of a REAL4 or REAL8 value
	// (see Chap2 for addressing modes of real numbers)
	// If both the source and destination are specified, they must both be FPU data
	// registers and one of them must be the TOP data register ST(0) 
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fmul_TOP_d0_cont					// Multiply TOP by st(other) and store the result to TOP

fcom_st0:
	mov		r1, #0
	b		fcom_ST
fcom_st2:
	mov		r1, #2
	b		fcom_ST
fcom_st3:
	mov		r1, #3
	b		fcom_ST
fcom_st4:
	mov		r1, #4
	b		fcom_ST
fcom_st5:
	mov		r1, #5
	b		fcom_ST
fcom_st6:
	mov		r1, #6
	b		fcom_ST
fcom_st7:
	mov		r1, #7
	b		fcom_ST
fcom_st1:
	mov		r1, #1
fcom_ST:	
	//------
	// FPU_FCOM(TOP,STV(sub));
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fcom_TOP_d0_cont

fcomp_st0:
	mov		r1, #0
	b		fcomp_ST
fcomp_st2:
	mov		r1, #2
	b		fcomp_ST
fcomp_st3:
	mov		r1, #3
	b		fcomp_ST
fcomp_st4:
	mov		r1, #4
	b		fcomp_ST
fcomp_st5:
	mov		r1, #5
	b		fcomp_ST
fcomp_st6:
	mov		r1, #6
	b		fcomp_ST
fcomp_st7:
	mov		r1, #7
	b		fcomp_ST
fcomp_st1:
	mov		r1, #1
fcomp_ST:	
	//------
	// FPU_FCOM(TOP,STV(sub));
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fcomp_TOP_d0_cont

fsub_st0:
	mov		r1, #0
	b		fsub_ST
fsub_st2:
	mov		r1, #2
	b		fsub_ST
fsub_st3:
	mov		r1, #3
	b		fsub_ST
fsub_st4:
	mov		r1, #4
	b		fsub_ST
fsub_st5:
	mov		r1, #5
	b		fsub_ST
fsub_st6:
	mov		r1, #6
	b		fsub_ST
fsub_st7:
	mov		r1, #7
	b		fsub_ST
fsub_st1:
	mov		r1, #1
fsub_ST:	
	//------
	// FPU_FSUB(TOP,STV(sub));
	//	fpuregs[st]d -= fpuregs[other]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fsub_TOP_d0_cont

fsubr_st0:
	mov		r1, #0
	b		fsubr_ST
fsubr_st2:
	mov		r1, #2
	b		fsubr_ST
fsubr_st3:
	mov		r1, #3
	b		fsubr_ST
fsubr_st4:
	mov		r1, #4
	b		fsubr_ST
fsubr_st5:
	mov		r1, #5
	b		fsubr_ST
fsubr_st6:
	mov		r1, #6
	b		fsubr_ST
fsubr_st7:
	mov		r1, #7
	b		fsubr_ST
fsubr_st1:
	mov		r1, #1
fsubr_ST:	
	//------
	// FPU_FSUBR(TOP,STV(sub));
	//	fpuregs[st]d = fpuregs[other]d - fpuregs[st]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fsubr_TOP_d0_cont

fdiv_st0:
	mov		r1, #0
	b		fdiv_ST
fdiv_st2:
	mov		r1, #2
	b		fdiv_ST
fdiv_st3:
	mov		r1, #3
	b		fdiv_ST
fdiv_st4:
	mov		r1, #4
	b		fdiv_ST
fdiv_st5:
	mov		r1, #5
	b		fdiv_ST
fdiv_st6:
	mov		r1, #6
	b		fdiv_ST
fdiv_st7:
	mov		r1, #7
	b		fdiv_ST
fdiv_st1:
	mov		r1, #1
fdiv_ST:	
	//------
	// FPU_FDIV(TOP,STV(sub));
	//	fpuregs[st]d /= fpuregs[other]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fdiv_TOP_d0_cont

fdivr_st0:
	mov		r1, #0
	b		fdivr_ST
fdivr_st2:
	mov		r1, #2
	b		fdivr_ST
fdivr_st3:
	mov		r1, #3
	b		fdivr_ST
fdivr_st4:
	mov		r1, #4
	b		fdivr_ST
fdivr_st5:
	mov		r1, #5
	b		fdivr_ST
fdivr_st6:
	mov		r1, #6
	b		fdivr_ST
fdivr_st7:
	mov		r1, #7
	b		fdivr_ST
fdivr_st1:
	mov		r1, #1
fdivr_ST:	
	//------
	// FPU_FDIVR(TOP,STV(sub));
	//	fpuregs[st]d = fpuregs[other]d / fpuregs[st]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*other
	vldr    		d0,[r0]								// d0 = st(other)
	b				fdivr_TOP_d0_cont

	.ltorg

// ------------------- D9 = ESC(1) ---------------------
// 0150:19837F	D93C24			fstcw	[esp]		(X-COM UFO)
// 0150:198396	D92D30B81A00	fldcw	[001AB830]	(X-COM UFO) (CW=127F)
// 0150:19D639	D9E8			fld1				(X-COM UFO)
// 0150:19839C	D9EE			fldz				(X-COM UFO)
//				D95C2420		fstp	[esp+20]	(X-COM UFO)
// 0180:21C4CB 	D9F1			fyl2x				(Abuse)
// 0180:2203A6	D9F0			f2xm1				(Abuse)
//
	.global	op_d9_USE32
op_d9_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fld, back1, fst, fstp, fldenv, fldcw, fstenv, fnstcw, "32"
	//0xc0 = mod = 11b => two register operands
	.word fld_st0, fld_st1, fld_st2, fld_st3, fld_st4, fld_st5, fld_st6, fld_st7
	.word fxch_st0, fxch_st1, fxch_st2, fxch_st3, fxch_st4, fxch_st5, fxch_st6, fxch_st7
	.word loop, loop, loop, loop, loop, loop, loop, loop		// FPU_FNOP
	.word fstp_st0, fstp_st1, fstp_st2, fstp_st3, fstp_st4, fstp_st5, fstp_st6, fstp_st7
	.word fchs, fabs, .unknown, .unknown, ftst, fxam, .unknown, .unknown
	.word fld1, fldl2t, fldl2e, fldpi, fldlg2, fldln2, fldz, .unknown
	.word f2xm1, fyl2x, fptan, fpatan, .unknown, .unknown, .unknown, .unknown
	.word fprem, .unknown, fsqrt, fsincos, frndint, fscale, fsin, fcos

	modrm_genall_1_oper fld, "skip", fst, fstp, fldenv, fldcw, fstenv, fnstcw, _t0, "32"

fld_t0_bp_32:
	mem_handler_bp
fld_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//-------
	//	FPU_PREP_PUSH();
	//-------
1:	FPU_PREP_PUSH_r0r1r3
	//-------
	//	FPU_FLD_F32(addr,TOP);
	//		union {
	//			float f;
	//			Bit32u l;
	//		}	blah;
	//		blah.l = mem_readd(addr);
	//-------
#if USE_UNALIGNED
	ldr				r0, [r2]
#else
	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//-------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.f);
	//-------
	ldr				r2, =GP_fpu_regs
	vcvt.f64.f32	d0, s0
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vstr        	d0,[r0]								// st(TOP) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fst_t0_bp_32:
	mem_handler_bp
fst_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//-------
	//	FPU_FST_F32(addr);
	//		blah.f = static_cast<float>(fpu.regs[TOP].d);
	//-------
1:	ldr				r0, =GP_fpu_regs
	ldrb			r1, [r0, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r1, lsl #3						// r0 = GP_fpu_regs + 8*TOP
	vldr        	d0,[r0]								// d0 = st(TOP)
	vcvt.f32.f64 	s0,d0
	//-------
	//		mem_writed(addr,blah.l);
	//-------
	vmov			r0, s0
#if USE_UNALIGNED
	str				r0, [r2]
#else
	strb			r0, [r2]
	lsr				r0, #8
	strb			r0, [r2, #1]
	lsr				r0, #8
	strb			r0, [r2, #2]
	lsr				r0, #8
	strb			r0, [r2, #3]
#endif
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fstp_t0_bp_32:
	mem_handler_bp
fstp_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//-------
	//	FPU_FST_F32(addr);
	//		blah.f = static_cast<float>(fpu.regs[TOP].d);
	//-------
1:	ldr				r0, =GP_fpu_regs
	ldrb			r1, [r0, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r1, lsl #3						// r0 = GP_fpu_regs + 8*TOP
	vldr        	d0,[r0]								// d0 = st(TOP)
	vcvt.f32.f64 	s0,d0
	//-------
	//		mem_writed(addr,blah.l);
	//-------
	vmov			r0, s0
#if USE_UNALIGNED
	str				r0, [r2]
#else
	strb			r0, [r2]
	lsr				r0, #8
	strb			r0, [r2, #1]
	lsr				r0, #8
	strb			r0, [r2, #2]
	lsr				r0, #8
	strb			r0, [r2, #3]
#endif
	//-------
	//	FPU_FPOP();
	//-------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldenv_t0_bp_32:
	mem_handler_bp
fldenv_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// Bit16u tag;
	// Bit32u tagbig;
	// Bitu cw;
	//	cw     = mem_readw(addr+0);
	//	FPU_SetCW(cw);
	//------
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	orr		r0, r1, lsl #8
	FPU_SetCW_r3 r0
	//------
	// if(!cpu.code.big) {
	//------
	ldrb	r1, [sp, #SP_CPU_BIG]
	mrs		r0,cpsr										// Save flags to r0
	cmp		r1, #0
	beq		fldenv_small								// Jump if we are not in cpu_code_big mode
	msr		cpsr_f, r0									// Restore flags
	//------
	// } else { 
	//	fpu.sw = (Bit16u)mem_readd(addr+4);
	//------
	ldrb	r0, [r2, #4]
	ldrb	r1, [r2, #5]
	orr		r0, r1, lsl #8
	strh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	//------
	//	tagbig = mem_readd(addr+8);
	//	tag    = static_cast<Bit16u>(tagbig);
	// }
	//------
	ldrb	r0, [r2, #8]
	ldrb	r1, [r2, #9]
fldenv_cont:
	orr		r0, r1, lsl #8
	//------
	// FPU_SetTag(tag);
	//	static INLINE void FPU_SetTag(Bit16u tag){
	//		for(Bitu i=0//i<8//i++)
	//			fpu.tags[i] = static_cast<FPU_Tag>((tag >>(2*i))&3);
	// }
	//------
	ldr		r3, =GP_fpu_tags
	and		r1, r0, #3
	strb	r1, [r3, #0]
	mov		r1, r0, lsr #2
	and		r1, #3
	strb	r1, [r3, #1]
	mov		r1, r0, lsr #4
	and		r1, #3
	strb	r1, [r3, #2]
	mov		r1, r0, lsr #6
	and		r1, #3
	strb	r1, [r3, #3]
	mov		r1, r0, lsr #8
	and		r1, #3
	strb	r1, [r3, #4]
	mov		r1, r0, lsr #10
	and		r1, #3
	strb	r1, [r3, #5]
	mov		r1, r0, lsr #12
	and		r1, #3
	strb	r1, [r3, #6]
	mov		r1, r0, lsr #14
	and		r1, #3
	strb	r1, [r3, #7]
	//------
	// TOP = FPU_GET_TOP();
	//------
	FPU_GET_TOP_r0r3
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
fldenv_small:
	msr		cpsr_f, r0									// Restore flags
	//------
	//	fpu.sw = mem_readw(addr+2);
	//------
	ldrb	r0, [r2, #2]
	ldrb	r1, [r2, #3]
	orr		r0, r1, lsl #8
	strh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	//------
	//	tag    = mem_readw(addr+4);
	//------
	ldrb	r0, [r2, #4]
	ldrb	r1, [r2, #5]
	b		fldenv_cont


fldcw_t0_bp_32:
	mem_handler_bp
fldcw_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// Bit16u temp = mem_readw(addr);
	// FPU_SetCW(temp);
	//------
1:	ldrb	r0, [r2]
	ldrb	r1, [r2, #1]
	orr		r0, r1, lsl #8
	FPU_SetCW_r3 r0
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fstenv_t0_bp_32:
	mem_handler_bp
fstenv_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_SET_TOP(TOP);
	//------
1:	FPU_SET_TOP_r0r1r3
	//------
	//#if(!cpu.code.big) {
	//	} else { 
	//		mem_writed(addr+0,static_cast<Bit32u>(fpu.cw));
	//------
	ldrb	r0, [r3, #(GP_fpu_cw - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_cw + 1 - GP_fpu_regs)]
	strb	r0, [r2]
	strb	r1, [r2, #1]
	ldrb	r1, [sp, #SP_CPU_BIG]
	mrs		r0,cpsr										// Save flags to r0
	cmp		r1, #0
	beq		fstenv_small								// Jump if we are not in cpu_code_big mode
	msr		cpsr_f, r0									// Restore flags
	mov		r0, #0
	strb	r0, [r2, #2]
	strb	r0, [r2, #3]
	//------
	//		mem_writed(addr+4,static_cast<Bit32u>(fpu.sw));
	//------
	ldrb	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_sw + 1 - GP_fpu_regs)]
	strb	r0, [r2, #4]
	strb	r1, [r2, #5]
	mov		r0, #0
	strb	r0, [r2, #6]
	strb	r0, [r2, #7]
	//------
	//		mem_writed(addr+8,static_cast<Bit32u>(FPU_GetTag()));
	//			Bit16u tag=0;
	//			for(Bitu i=0//i<8//i++)
	//				tag |= ( (fpu.tags[i]&3) <<(2*i));
	//			return tag;
	//------
	ldrb	r0, [r3, #(GP_fpu_tags - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_tags + 1 - GP_fpu_regs)]
	orr		r0, r1, lsl #2
	ldrb	r1, [r3, #(GP_fpu_tags + 2 - GP_fpu_regs)]
	orr		r0, r1, lsl #4
	ldrb	r1, [r3, #(GP_fpu_tags + 3 - GP_fpu_regs)]
	orr		r0, r1, lsl #6
	ldrb	r1, [r3, #(GP_fpu_tags + 4 - GP_fpu_regs)]
	orr		r0, r1, lsl #8
	ldrb	r1, [r3, #(GP_fpu_tags + 5 - GP_fpu_regs)]
	orr		r0, r1, lsl #10
	ldrb	r1, [r3, #(GP_fpu_tags + 6 - GP_fpu_regs)]
	orr		r0, r1, lsl #12
	ldrb	r1, [r3, #(GP_fpu_tags + 7 - GP_fpu_regs)]
	orr		r0, r1, lsl #14
	strb	r0, [r2, #8]
	lsr		r0, #8
	strb	r0, [r2, #9]
	mov		r0, #0
	strb	r0, [r2, #10]
	strb	r0, [r2, #11]
	//------
	//	}
	//------
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
fstenv_small:
	msr		cpsr_f, r0									// Restore flags
	//------
	//		mem_writew(addr+0,static_cast<Bit16u>(fpu.cw));
	//		mem_writew(addr+2,static_cast<Bit16u>(fpu.sw));
	//------
	ldrb	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_sw + 1 - GP_fpu_regs)]
	strb	r0, [r2, #2]
	strb	r1, [r2, #3]
	//------
	//		mem_writew(addr+4,static_cast<Bit16u>(FPU_GetTag()));
	//------
	ldrb	r0, [r3, #(GP_fpu_tags - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_tags + 1 - GP_fpu_regs)]
	orr		r0, r1, lsl #2
	ldrb	r1, [r3, #(GP_fpu_tags + 2 - GP_fpu_regs)]
	orr		r0, r1, lsl #4
	ldrb	r1, [r3, #(GP_fpu_tags + 3 - GP_fpu_regs)]
	orr		r0, r1, lsl #6
	ldrb	r1, [r3, #(GP_fpu_tags + 4 - GP_fpu_regs)]
	orr		r0, r1, lsl #8
	ldrb	r1, [r3, #(GP_fpu_tags + 5 - GP_fpu_regs)]
	orr		r0, r1, lsl #10
	ldrb	r1, [r3, #(GP_fpu_tags + 6 - GP_fpu_regs)]
	orr		r0, r1, lsl #12
	ldrb	r1, [r3, #(GP_fpu_tags + 7 - GP_fpu_regs)]
	orr		r0, r1, lsl #14
	strb	r0, [r2, #4]
	lsr		r0, #8
	strb	r0, [r2, #5]
	//------
	//	}
	//------
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fnstcw_t0_bp_32:
	mem_handler_bp
fnstcw_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	mem_writew(addr,fpu.cw);
	//------
1:	ldr		r3, =GP_fpu_regs
	ldrb	r0, [r3, #(GP_fpu_cw - GP_fpu_regs)]
	ldrb	r1, [r3, #(GP_fpu_cw + 1 - GP_fpu_regs)]
	strb	r0, [r2]
	strb	r1, [r2, #1]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fld_st0:
	mov		r1, #0
	b		fld_ST
fld_st2:
	mov		r1, #2
	b		fld_ST
fld_st3:
	mov		r1, #3
	b		fld_ST
fld_st4:
	mov		r1, #4
	b		fld_ST
fld_st5:
	mov		r1, #5
	b		fld_ST
fld_st6:
	mov		r1, #6
	b		fld_ST
fld_st7:
	mov		r1, #7
	b		fld_ST
fld_st1:
	mov		r1, #1
fld_ST:	
	//------
	// Bitu reg_from=STV(sub);
	//------
	ldr		r3, =GP_fpu_regs
	ldrb	r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add		r1, r0
	and		r1, #7								// r1 = st(i) index
	add		r2, r3, r1, lsl #3
	vldr	d0, [r2]							// d0 = st(i) value
	add		r2, r3, #(GP_fpu_tags - GP_fpu_regs)
	ldrb	r1, [r2, r1]						// r1 = st(i) tag
	//------
	// FPU_PREP_PUSH();
	//		TOP = (TOP - 1) & 7;
	//-------
	sub		r0, #1
	and		r0, #7
	strb	r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	//------
	// FPU_FST(from, TOP);
	//		fputags[top] = fputags[from];
	//------
	strb	r1, [r2, r0]						// st(top) tag = r1
	//------
	//		fpup_regs[top]m1 = fpup_regs[from]m1;
	//		fpup_regs[top]m2 = fpup_regs[from]m2;
	//		fpup_regs[top]m3 = fpup_regs[from]m3;
	//------
	add		r2, r3, r0, lsl #3
	vstr	d0, [r2]							// st(top) value = d0
	//------
	//		FPU_SET_C1(0);
	//------
	ldrh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic		r0, #FPU_C1_FLAG
	strh	r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fxch_st0:
	mov		r1, #0
	b		fxch_ST
fxch_st2:
	mov		r1, #2
	b		fxch_ST
fxch_st3:
	mov		r1, #3
	b		fxch_ST
fxch_st4:
	mov		r1, #4
	b		fxch_ST
fxch_st5:
	mov		r1, #5
	b		fxch_ST
fxch_st6:
	mov		r1, #6
	b		fxch_ST
fxch_st7:
	mov		r1, #7
	b		fxch_ST
fxch_st1:
	mov		r1, #1
fxch_ST:
	//------
	// FPU_FXCH(TOP,STV(sub));
	//	Bit32u m1s = fpup_regs[other]m1;
	//	Bit32u m2s = fpup_regs[other]m2;
	//	Bit16u m3s = fpup_regs[other]m3;
	//	fpup_regs[other]m1 = fpup_regs[stv]m1;
	//	fpup_regs[other]m2 = fpup_regs[stv]m2;
	//	fpup_regs[other]m3 = fpup_regs[stv]m3;
	//	fpup_regs[stv]m1 = m1s;
	//	fpup_regs[stv]m2 = m2s;
	//	fpup_regs[stv]m3 = m3s;
	//------
	ldr		r3, =GP_fpu_regs
	ldrb	r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add		r1, r0
	and		r1, #7								// r1 = st(i) index
	add		r2, r3, r1, lsl #3
	vldr	d0, [r2]							// d0 = st(i) value
	add		r2, r3, r0, lsl #3
	vldr	d1, [r2]							// d1 = st(top) value
	vstr	d0, [r2]							// st(top) value = d0 = original st(i) value
	add		r2, r3, r1, lsl #3
	vstr	d1, [r2]							// st(i) value = d1 = original st(top) value
	//------
	//	FPU_SET_C1(0);
	//------
	ldrh	r2, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic		r2, #FPU_C1_FLAG
	strh	r2, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	//------
	//	FPU_Tag tag = fputags[other];
	//	fputags[other] = fputags[stv];
	//	fputags[stv] = tag;
	//------
	add		r3, #(GP_fpu_tags - GP_fpu_regs)
	add		r2, r3, r0							// r2 = fpu_tags[top] address
	add		r3, r3, r1							// r3 = fpu_tags[i] address
	ldrb	r0, [r2]
	ldrb	r1, [r3]
	strb	r0, [r3]
	strb	r1, [r2]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fstp_st0:
	mov		r1, #0
	b		fstp_ST
fstp_st2:
	mov		r1, #2
	b		fstp_ST
fstp_st3:
	mov		r1, #3
	b		fstp_ST
fstp_st4:
	mov		r1, #4
	b		fstp_ST
fstp_st5:
	mov		r1, #5
	b		fstp_ST
fstp_st6:
	mov		r1, #6
	b		fstp_ST
fstp_st7:
	mov		r1, #7
	b		fstp_ST
fstp_st1:
	mov		r1, #1
fstp_ST:	
	//------
	//	FPU_FST(TOP,STV(sub));
	//		fputags[other] = fputags[st];
	//		fpuregs[other] = fpuregs[st];
	//------
	ldr		r3, =GP_fpu_regs
	ldrb	r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add		r1, r0
	and		r1, #7								// r1 = st(i) index
	add		r2, r3, r0, lsl #3
	vldr	d0, [r2]							// d0 = st(top) value
	add		r2, r3, r1, lsl #3
	vstr	d0, [r2]							// st(i) value = d0
	add		r3, #(GP_fpu_tags - GP_fpu_regs)
	ldrb	r2, [r3, r0]						// r2 = fpu_tags[top]
	strb	r2, [r3, r1]						// fpu_tags[i] = r2
	//------
	//	FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fchs:
	//------
	//	fpuregs[TOP]d = -1*(fpuregs[TOP]d);
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	vldr        d0,[r2]
	vneg.f64    d0,d0
	vstr        d0,[r2]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fabs:
	//------
	//	fpuregs[TOP]d = fabs(fpuregs[TOP]d);
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	vldr       	d0,[r2]
	vabs.f64    d0,d0
	vstr        d0,[r2]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

ftst:
	//------
	//	fpuregs[8]d = 00;
	//	FPU_FCOM(TOP,8);
	//------
	ldr				r0, =fpu_lit_0
	vldr			d0, [r0]
	b				fcom_TOP_d0_cont

fxam:
	//------
	// This instruction examines the content of ST(0) and the type of value is reported
	// in the condition codes C3, C2 and C0 of the Status Word as indicated below
	// In addition, the C1 bit would be the same as the sign bit of the value in ST(0) 
    // Content of ST(0)        C3        C2        C0
    //    Unsupported           0         0         0
    //        NAN               0         0         1
    // Normal finite number     0         1         0
    //      Infinity            0         1         1
    //        Zero              1         0         0
    //       Empty              1         0         1
    // Denormalized number      1         1         0
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	add			r1, #(GP_fpu_tags - GP_fpu_regs)
	ldrb		r1, [r1, r0]							// r1 = fputags[top]
	//------
	// Test for Empty tag
	//------
	mrs			r0,cpsr									// Save flags to r0
	cmp			r1, #TAG_Empty
	ldrh		lr, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic			lr, #(FPU_C0_FLAG|FPU_C1_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)
	orreq		lr, #(FPU_C0_FLAG|FPU_C3_FLAG)
	streqh		lr, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	beq			restore_flags_from_r0
	//------
	// Set C1 based on the sign of the value
	//------
	ldr			r1, [r2, #4]							// r1 = high word of the double value
	tst			r1, #0x80000000
	orrne		lr, #FPU_C1_FLAG
	//-------
	// Test for NaN and Infinity
	//-------
#if defined(RPi) || defined(Roku)
	mov			r1, r2, lsr #20
	bic			r1, #0x800
#else
	ubfx		r1, r1, #20, #11						// r1 = exponent of the double value
#endif
	add			r1, #1
	cmp			r1, #0x800								// Was the exponent 0x7FF (NaN or infinity)?
	bne			fxam_exp_not_7FF
	//-------
	// Exponent is 0x7FF, so either NaN or Infinity
	//-------
	orr			lr, #FPU_C0_FLAG						// NaN and Infinity both have C0 set
	ldr			r1, [r2]								// r1 = low word of the double value
	ldr			r2, [r2, #4]							// r2 = high word of the double value
#if defined(RPi) || defined(Roku)
	lsl			r2, #12
	lsr			r2, #12
#else
	ubfx		r2, r2, #0, #20
#endif
	orrs		r1, r2									// Is the Sig part zero?
	orreq		lr, #FPU_C2_FLAG						// Set C2 if Infinity (Sig part == 0)
	strh		lr, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0
fxam_exp_not_7FF:
	subs		r1, #1									// Is the exponent zero?
	bne			fxam_normal
	//------
	// Exponent is zero, so either Zero or Denormalized number
	//------
	orr			lr, #FPU_C3_FLAG						// Zero and Denormalized both have C3 flag set
	ldr			r1, [r2]								// r1 = low word of the double value
	ldr			r2, [r2, #4]							// r2 = high word of the double value
#if defined(RPi) || defined(Roku)
	lsl			r2, #12
	lsr			r2, #12
#else
	ubfx		r2, r2, #0, #20
#endif
	orrs		r1, r2									// Is the Sig part zero?
	orrne		lr, #FPU_C2_FLAG						// Set also C2 if Denormalized number
	strh		lr, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0
fxam_normal:
	orr			lr, #FPU_C2_FLAG						// Normal number, so set C2 flag
	strh		lr, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0

	
fldz:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// fputags[TOP] = TAG_Zero;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, #(GP_fpu_tags - GP_fpu_regs)
	mov			r1, #TAG_Zero
	strb		r1, [r2, r0]							// r1 = fputags[top]
	//------
	// fpuregs[TOP]d = 0.0;
	//------
	add			r2, r3, r0, lsl #3
	mov			r0, #0
	str			r0, [r2]
	str			r0, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldl2t:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// #define L2T		3.3219280948873623 = 400A934F0979A371
	// fpuregs[TOP]d = L2T;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	ldr			r0, =0x0979A371
	ldr			r1, =0x400A934F
	str			r0, [r2]
	str			r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldl2e:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// #define L2E		1.4426950408889634 = 3FF71547652B82FE
	// fpuregs[TOP]d = L2E;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	ldr			r0, =0x652B82FE
	ldr			r1, =0x3FF71547
	str			r0, [r2]
	str			r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldpi:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// #define PI		3.14159265358979323846 = 400921FB54442D18
	// fpuregs[TOP]d = PI;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	ldr			r0, =0x54442D18
	ldr			r1, =0x400921FB
	str			r0, [r2]
	str			r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldlg2:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// #define LG2		0.3010299956639812 = 3FD34413509F79FF
	// fpuregs[TOP]d = LG2;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	ldr			r0, =0x509F79FF
	ldr			r1, =0x3FD34413
	str			r0, [r2]
	str			r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fldln2:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// #define LN2		0.69314718055994531 = 3FE62E42FEFA39EF
	// fpuregs[TOP]d = LN2;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3
	ldr			r0, =0xFEFA39EF
	ldr			r1, =0x3FE62E42
	str			r0, [r2]
	str			r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fld1:
	//------
	// FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	// 	fpuregs[TOP]d = 1.0;
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r2, [r3, #(GP_fpu_top - GP_fpu_regs)]
	mov			r0, #0
	mov			r1, #0x3F000000
	orr			r1, #0x00F00000									// r0:r1 = 1.0 = 0x3FF0000000000000
	add			r2, r3, r2, lsl #3
	strd		r0, r1, [r2]
#if LOG_FPU
	b			loop_log
#else
	b			loop
#endif

f2xm1:
	//------
	// Syntax    f2xm1 (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow, Precision
	// This instruction computes the exponential value of 2 to the power of the value of ST(0) and subtracts 1
	// The content of ST(0) is overwritten with the result The content of ST(0) must be within the range of
	// -1.0 to +1.0// if it is outside the acceptable range, the result is undefined but no exception is reported 
	//
	// fpuregs[TOP].d = pow(2.0,fpuregs[TOP].d) - 1;
	//------
	mrs			r0,cpsr									// Save flags to r0
	ldr			r3, =GP_fpu_regs
	push		{r0, r4, r9, r12}
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
#if defined(RPi) || defined(Roku)
	ldr			r1,=fpu_lit_2
	vldr		d0, [r1]
#else
	vmov.f64    d0,#2.0
#endif
	add			r4, r3, r0, lsl #3
	vldr        d1,[r4]
	bl          pow
#if defined(RPi) || defined(Roku)
	ldr			r0,=fpu_lit_1
	vldr		d1, [r0]
#else
	vmov.f64    d1,#1.0
#endif
	vsub.f64    d0,d0,d1
	vstr        d0,[r4]
#else
	mov     	r1, #0x40000000
	add			r4, r3, r0, lsl #3
	mov     	r0, #0									// r0:r1 = 2.0
	ldrd    	r2, [r4]								// r2:r3 = fpuregs[TOP].d
	bl      	pow
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x300000						// r2:r3 = 1.0
	bl      	__aeabi_dsub
	strd    	r0, [r4]
#endif
	pop			{r0, r4, r9, r12}
	b			restore_flags_from_r0

fyl2x:
	//------
	// Syntax    fyl2x (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow, Overflow, Zero divide, Precision
	// This instruction computes the logarithm base 2 of the value in the TOP
	// data register ST(0) and multiplies it by the value in the ST(1) register
	// The content of ST(0) must be a non-zero positive number The content of
	// ST(1) is overwritten with the result and the TOP data register is POPed 
	// The following tabulation gives the resulting values based on the contents of ST(0) and ST(1)
	// "F" means a finite value between 0 and INFINITY, and "Inv" means an Invalid operation 
	//
	// ST(0)	+?	F>+1	+1	+1>F>0	±0	-F	-? 
	// ST(1)
	// +?		+?	 +?		Inv   -?	-?	Inv Inv 
	// +F		+?	 +F		+0	  -F	-?	Inv Inv 
	// +0		Inv  +0		+0	  -0	Inv Inv Inv 
	// -0		Inv  -0		-0	  +0	Inv Inv Inv 
	// -F		-?	 -F		-0	  +F	+?	Inv Inv 
	// -?		-?	 -?		Inv   +?	+?	Inv Inv 
	//
	// To explain the Invalid results in the above tabulation, 
	// log(+?)=+?, therefore ?*0 is Invalid 
	// log(+1)=0, therefore 0*? is Invalid 
	// log(+0)=-?, therefore -?*0 is Invalid 
	// log(negative number) is always Invalid (except -0 which is treated as +0) 
	//
	// An Invalid operation exception is detected if either ST(0) or ST(1) is empty,
	// or is a NAN, or the combination of operands is indicated as "Inv" in the above tabulation,
	// setting the related flag in the Status Word The TOP data register would be POPed
	// and the content of ST(0) (formerly ST(1)) would be overwritten with the INDEFINITE value 
	//
	// fpuregs[STV(1)].d*=log(fpuregs[TOP].d)/log(static_cast<Real64>(2.0));
	//------
	mrs			r0,cpsr									// Save flags to r0
	ldr			r3, =GP_fpu_regs
	push		{r0, r4, r9, r12}
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       {d8}
	add			r2, r3, r0, lsl #3						// r2 = st(top) address
	add			r0, #1
	and			r0, #7
	add			r4, r3, r0, lsl #3						// r4 = st(1) address
	vldr        d0,[r2]									// d0 = fpuregs[TOP].d
	bl          log
	ldr			r0,=fpu_log_2
	vmov.f64    d8,d0									// d8 = log(fpuregs[TOP].d)
	vldr		d0,[r0]									// d0 = log(2.0)
	vldr        d2,[r4]									// d2 = fpuregs[STV(1)].d
	vdiv.f64    d0,d8,d0								// d0 = log(fpuregs[TOP].d) / log(2.0)
	vmul.f64    d0,d0,d2								// d0 = fpuregs[STV(1)].d * log(fpuregs[TOP].d) / log(2.0)
	vstr        d0,[r4]									// Save the result
	vpop        {d8}
#else
	add			r2, r3, r0, lsl #3						// r2 = st(top) address
	add			r0, #1
	and			r0, #7
	add			r4, r3, r0, lsl #3						// r4 = st(1) address
	ldrd        r0,[r2]									// r0:r1 = fpuregs[TOP].d
	bl          log
	ldr			r2,=0xfefa39ef
	ldr			r3,=0x3fe62e42							// r2:r3 = log(double(2.0))
	bl      	__aeabi_ddiv
	ldrd		r2, [r4]								// r2:r3 = fpuregs[STV(1)].d
	bl      	__aeabi_dmul
	strd    	r0, [r4]
#endif
	FPU_FPOP_r0r1r2
	pop			{r0, r4, r9, r12}
	b			restore_flags_from_r0

fptan:
	//------
	// Syntax    fptan (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow, Precision
	// This instruction computes the tangent of the source angle value in ST(0)
	// The tangent value replaces the content of ST(0), the TOP register field
	// of the Status Word is decremented, and a value of 1.0 is inserted into the new ST(0)
	// The angle must be expressed in radians and be within the -2^63 to +2^63 range 
	// The extra value of 1.0 is primarily for compatibility with the early co-processors prior to the 387
	// The FSIN and FCOS instructions were not then available and had to be computed from
	// the tangent value (and the acceptable range for the angle was only 0 to +?/4)
	// It is more a nuisance than a feature with the more modern FPUs, requiring the
	// need for two registers instead of one and an extra instruction to discard it
	// If the source angle value is outside the acceptable range (but not INFINITY),
	// the C2 flag of the Status Word is set to 1 and the content of all data registers
	// remains unchanged// the TOP register field of the Status Word is not modified
	// and no exception is detected (The source value can be reduced to within the
	// acceptable range with the FPREM instruction using a divisor of 2?) 
	//
	// fpuregs[TOP]d = tan(fpuregs[TOP]d);
	//------
	mrs			r0,cpsr									// Save flags to r0
	ldr			r3, =GP_fpu_regs
	push		{r0, r4, r9, r12}
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r3, r0, lsl #3						// r4 = st(top) address
#if defined(__ARM_PCS_VFP)
	vldr		d0, [r4]
	bl          tan
	vstr        d0, [r4]
#else
	ldrd		r0, [r4]
	bl			tan
	strd		r0, [r4]
#endif
	pop			{r0, r4, r9, r12}
	msr			cpsr_f, r0								// Restore flags
	//------
	// FPU_SET_C2(0);
	//------
	ldr			r3, =GP_fpu_regs
	ldrh		r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic			r0, #FPU_C2_FLAG
	strh		r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	//------
	// FPU_PUSH(1.0);
	//------
	b			fld1									// Go to pushing 1.0 to the top of stack
	
fpatan:
	//------
	// fpuregs[STV(1)]d = atan2(fpuregs[STV(1)]d,fpuregs[TOP]d);
	//------
	mrs			r0,cpsr									// Save flags to r0
	ldr			r3, =GP_fpu_regs
	push		{r0, r4, r9, r12}
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3						// r2 = st(top) address
	add			r0, #1
	and			r0, #7
	add			r4, r3, r0, lsl #3						// r4 = st(1) address
#if defined(__ARM_PCS_VFP)
	vldr        d0,[r4]
	vldr        d1,[r2]
	bl          atan2
	vstr        d0,[r4]
#else
	ldrd        r0,[r4]
	ldrd        r2,[r2]
	bl          atan2
	strd        r0,[r4]
#endif
	pop			{r0, r4, r9, r12}
	msr			cpsr_f, r0								// Restore flags
	//------
	// FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fprem:
	//------
	// Syntax    fprem (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow
	// This instruction performs the equivalent of a ST(0) modulo ST(1) with REAL numbers,
	// overwriting the content of ST(0) with the remainder which will have the same sign
	// as the original value in ST(0) The content of ST(1) remains unchanged 
	// The remainder represents the following value 
	//
	// Remainder = ST(0) - (Q * ST(1)) 
	//
	// where Q is an integer obtained by truncating the quotient of ST(0)÷ST(1) 
	//
	// However, the exponent in ST(0) cannot be reduced by more than 63 with each execution
	// of the FPREM instruction If the difference between the exponents of ST(0) and ST(1)
	// is larger than 63, ST(0) will then be only partially reduced and that partial remainder
	// will replace the content of ST(0) The software will have to repeat the FPREM instruction
	// as often as necessary to obtain a final remainder which will be smaller than the modulus ST(1)
	// When the reduction is complete, the C2 bit of the Status Word is cleared, otherwise it is set to 1 
	//
	// An Invalid operation exception is detected if ST(0) or ST(1) is empty, or is a NAN,
	// or if ST(0) is INFINITY, or if ST(1) is zero, setting the related flag in the Status Word
	// The content of ST(0) would be overwritten with the INDEFINITE value 
	//
	// A Stack Fault exception is also detected if either ST(0) or ST(1) is empty,
	// setting the related flag in the Status Word 
	//
	// A Denormal exception is detected when the content of either ST(0) or ST(1)
	// is a denormalized number or a result is a denormalized number,
	// setting the related flag in the Status Word 
	//
	// An Underflow exception will be detected if the result exceeds the range
	// limit of REAL10 numbers, setting the related flag in the Status Word 
	//
	// When the operation is successful, the 3 least significant bits of the quotient (Q)
	// are inserted in the C0, C1 and C3 bit fields of the Status Word in the following manner 
	//
	// C0 = bit2 of the quotient (Q2) 
	// C1 = bit0 of the quotient (Q0) 
	// C3 = bit1 of the quotient (Q1) 
	//
	//	double tmp = st/st1;
	//	long long ressaved = (long long)tmp;
	//	fpu_sw &= ~(FPU_C0_FLAG|FPU_C1_FLAG|FPU_C2_FLAG|FPU_C3_FLAG);
	//#if (ressaved&4) fpu_sw |= FPU_C0_FLAG;
	//#if (ressaved&2) fpu_sw |= FPU_C3_FLAG;
	//#if (ressaved&1) fpu_sw |= FPU_C1_FLAG;
	//#if (fabs(tmp) > (double)(9223372036854775807LL)) fpu_sw |= FPU_C2_FLAG;
	//	return st - ressaved*st1;
	//
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r5, r6, r12}
	ldr			r6, =GP_fpu_regs
	vpush       {d8-d10}
	ldrb		r0, [r6, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r6, r0, lsl #3						// r4 = st(top) address
	add			r0, #1
	and			r0, #7
	add			r5, r6, r0, lsl #3						// r5 = st(1) address
	//-------
	// double tmp = st/st1;
	//-------
	vldr        d9, [r4]								// d9 = st(top)
	vldr        d10, [r5]								// d10 = st(1)
	vdiv.f64    d8,d9,d10								// d8 = tmp = st(top) / st(1)
	//-------
	// long long ressaved = (long long)tmp;
	//-------
#if defined(__ARM_PCS_VFP)
	vmov    	r0, r1, d8
	bl      	__aeabi_d2lz							// r0,r1 = long long ressaved
#else
	ldr			r2, =GP_fpu_temp
	vstr		d8, [r2]
	ldrd		r0, [r2]
	bl			__aeabi_d2lz
#endif
	//-------
	// fpu_sw &= ~(FPU_C0_FLAG|FPU_C1_FLAG|FPU_C2_FLAG|FPU_C3_FLAG);
	//-------
	ldrh		r2, [r6, #(GP_fpu_sw - GP_fpu_regs)]
	bic			r2, #(FPU_C0_FLAG|FPU_C1_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)
	//-------
	// if (ressaved&1) fpu_sw |= FPU_C1_FLAG;
	// if (ressaved&2) fpu_sw |= FPU_C3_FLAG;
	// if (ressaved&4) fpu_sw |= FPU_C0_FLAG;
	//-------
	tst			r0, #1
	orrne		r2, #FPU_C1_FLAG
	tst			r0, #2
	orrne		r2, #FPU_C3_FLAG
	tst			r0, #4
	orrne		r2, #FPU_C0_FLAG
	strh		r2, [r6, #(GP_fpu_sw - GP_fpu_regs)]
	//-------
	// if (fabs(tmp) > (double)(9223372036854775807LL)) fpu_sw |= FPU_C2_FLAG;
	//-------
	ldr			r3, =fpu_fprem_test
	vldr        d0,[r3]									// d0 = 9.22337e+018
	vabs.f64    d1,d8									// d1 = fabs(tmp)
	vcmp.f64   	d1,d0
	vmrs        apsr_nzcv,fpscr
	ble         1f
	orr        	r2, #FPU_C2_FLAG
	strh		r2, [r6, #(GP_fpu_sw - GP_fpu_regs)]
	//-------
	// st(top) = st - ressaved*st1;
	//-------
#if defined(__ARM_PCS_VFP)
1:	bl      	__aeabi_l2d							// d0 = (double)r0:r1
    vmov    	d0, r0, r1
#else
1:	bl			__aeabi_l2d
	strd		r0, [r4]
	vldr		d0, [r4]
#endif
	vmul.f64    d0,d0,d10								// d0 = ressaved * st1
	vsub.f64    d0,d9,d0								// d0 = st - ressaved * st1
	vstr        d0,[r4]									// st(top) = st - ressaved * st1
	vpop       	{d8-d10}
	pop			{r0, r4, r5, r6, r12}
	b			restore_flags_from_r0

	
fsqrt:
	//------
	// FPU_FSQRT()
	//	fpuregs[TOP].d = sqrt(fpuregs[TOP].d);
	//------
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r3, r0, lsl #3						// r2 = st(top) address
	vldr        d0,[r2]
	vsqrt.f64   d0,d0
	vstr        d0,[r2]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fsincos:
	//------
	// Syntax    fsincos (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow, Precision
	// This instruction computes the sine and the cosine of the source angle value in ST(0)
	// The sine value replaces the content of ST(0), the TOP register field of the
	// Status Word is decremented, and the cosine value is inserted into the new ST(0)
	// The angle must be expressed in radians and be within the -2^63 to +2^63 range
	// (This instruction is faster than computing the sine and cosine separately with the FSIN and FCOS instructions) 
	// If the source angle value is outside the acceptable range (but not INFINITY),
	// the C2 flag of the Status Word is set to 1 and the content of all data registers remains unchanged;
	// the TOP register field of the Status Word is not modified and no exception is detected
	// (The source value can be reduced to within the acceptable range with the FPREM instruction using a divisor of 2?) 
	//
	// Real64 temp = fpuregs[TOP]d;
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r9, r12}
	ldr			r4, =GP_fpu_regs
#if defined(__ARM_PCS_VFP)
	vpush       {d8}
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
	vldr        d8,[r4]									// d8 = temp = st(top) value
	vmov.f64    d0,d8
	bl          sin										// d0 = sin(tmp)
	vstr        d0,[r4]									// st(top) = sin(tmp)
	vmov.f64    d0,d8
	bl          cos										// d0 = cos(tmp)
	FPU_PREP_PUSH_r0r1r3
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r3 = new st(top) address
	vstr        d0,[r3]									// st(top) = cos(tmp)
	vpop        {d8}
#else
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
	ldrd       	r0,[r4]									// r0 = temp = st(top) value
	bl          sin										// d0 = sin(tmp)
	ldrd       	r2,[r4]									// r0 = temp = st(top) value
	strd        r0,[r4]									// st(top) = sin(tmp)
	mov			r0, r2
	mov			r1, r3
	bl          cos										// d0 = cos(tmp)
	mov			r2, r0
	mov			r4, r1
	FPU_PREP_PUSH_r0r1r3
	ldr			r1, =GP_fpu_regs
	ldrb		r0, [r1, #(GP_fpu_top - GP_fpu_regs)]
	add			r1, r0, lsl #3							// r1 = new st(top) address
	mov			r3, r4
	strd        r2,[r1]									// st(top) = cos(tmp)
#endif
	pop			{r0, r4, r9, r12}
	//------
	// FPU_SET_C2(0);
	//------
	ldrh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic			r1, #FPU_C2_FLAG
	strh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0

frndint:
	//------
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	// Bit64s temp= static_cast<Bit64s>(FROUND(fpuregs[TOP]d));
	// fpu.regs[TOP].d=static_cast<double>(temp);
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r9, r12}
	ldr			r4, =GP_fpu_regs
#if defined(__ARM_PCS_VFP)
	vpush       {d8}
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	ldrb		r1, [r4, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
	vldr        d0,[r4]									// d0 = st(top) value = in
	cmp			r1, #float_round_nearest_even
	bne			1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    d8,d0									// d8 = in
	bl          floor
#if defined(RPi) || defined(Roku)
	ldr			r0,=fpu_lit_05
	vldr		d1, [r0]
#else
	vmov.f64    d1,#0.5
#endif
	vmov.f64    d2,d0									// d2 = floor(in)
	vsub.f64    d0,d8,d0
	vcmp.f64   	d0,d1
	vmrs        apsr_nzcv,fpscr
	beq         4f
	blt			3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr			r0,=fpu_lit_1
	vldr		d1, [r0]
#else
	vmov.f64    d1,#1.0
#endif
	vadd.f64    d0,d2,d1								// d0 = floor(in) + 1
	b           2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    d0,d2									// d0 = floor(in)
	b           2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vmov    	r0, r1, d2
	bl      	__aeabi_d2lz							// r0,r1 = long long ressaved
	tst         r0,#1
	beq         5f										// Use r0:r1 as-is
#if defined(RPi) || defined(Roku)
	ldr			r0,=fpu_lit_1
	vldr		d1, [r0]
#else
	vmov.f64    d1,#1.0
#endif
	vadd.f64    d0,d2,d1								// d0 = floor(in) + 1
	b           2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r1, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r1, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit64s temp= static_cast<Bit64s>(FROUND(fpuregs[TOP]d));
	//------
2:	vmov    	r0, r1, d0
	bl      	__aeabi_d2lz							// r0,r1 = long long ressaved
	//------
	// fpu.regs[TOP].d=static_cast<double>(temp);
	//------
5:	bl      	__aeabi_l2d
    vmov    	d0, r0, r1
	vstr        d0, [r4]
	vpop        {d8}
#else
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	ldrb		r2, [r4, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
	ldrd        r0,[r4]									// d0 = st(top) value = in
	cmp			r2, #float_round_nearest_even
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2lz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit64s temp= static_cast<Bit64s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2lz
	//------
	// fpu.regs[TOP].d=static_cast<double>(temp);
	//------
5:	bl			__aeabi_l2d
	strd		r0, [r4]
#endif	
	pop			{r0, r4, r9, r12}
	b			restore_flags_from_r0


fscale:
	//------
	// Syntax    fscale (no operand)
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Underflow, Overflow, Precision
	// This instruction adds the truncated integral value of ST(1) to the exponent
	// portion of the value in ST(0) The content of ST(1) remains unchanged
	// This is effectively a multiplication (or division) of the value in ST(0) by an integral power of 2 	
	//
	// fpuregs[TOP].d *= pow(2.0,static_cast<Real64>(static_cast<Bit64s>(fpuregs[STV(1)].d)));
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r9, r12}
	ldr			r3, =GP_fpu_regs
	ldrb		r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r3, r0, lsl #3						// r4 = st(top) address
	add			r0, #1
	and			r0, #7
	add			r3, r0, lsl #8							// r3 = st(1) address
#if defined(__ARM_PCS_VFP)
	ldrd        r0, [r3]								// r0:r1 = st(1) value
	bl      	__aeabi_d2lz							// r0,r1 = long long ressaved
	bl      	__aeabi_l2d
    vmov    	d1, r0, r1
#if defined(RPi) || defined(Roku)
	ldr			r0,=fpu_lit_2
	vldr		d0, [r0]
#else
	vmov.f64    d0,#2.0
#endif
	bl          pow
	vldr        d1,[r4]									// d1 = st(top) value
	vmul.f64    d0,d0,d1
	vstr        d0,[r4]									// st(top) = result
#else
	ldrd		r0, [r3]
	bl			__aeabi_d2lz
	bl			__aeabi_l2d
	mov     	r2, r0
	mov     	r3, r1
	mov     	r0, #0
	mov     	r1, #0x40000000							// r0:r1 = 2.0
	bl      	pow
	ldrd        r2,[r4]									// r2:r3 = st(top) value
    bl      	__aeabi_dmul
	strd        r0,[r4]									// st(top) = result
#endif
	pop			{r0, r4, r9, r12}
	b			restore_flags_from_r0


fsin:
	//------
	// fpuregs[TOP]d = sin(fpuregs[TOP]d);
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r9, r12}
	ldr			r4, =GP_fpu_regs
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
#if defined(__ARM_PCS_VFP)
	vldr        d0,[r4]									// d0 = st(top) value
	bl          sin										// d0 = sin(tmp)
	vstr        d0,[r4]									// st(top) = sin(tmp)
#else
	ldrd        r0,[r4]									// r0:r1 = st(top) value
	bl          sin										// r0:r1 = sin(tmp)
	strd        r0,[r4]									// st(top) = sin(tmp)
#endif
	pop			{r0, r4, r9, r12}
	//------
	// FPU_SET_C2(0);
	//------
	ldr			r3, =GP_fpu_regs
	ldrh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic			r1, #FPU_C2_FLAG
	strh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0


fcos:
	//------
	// fpuregs[TOP]d = cos(fpuregs[TOP]d);
	//------
	mrs			r0,cpsr									// Save flags to r0
	push		{r0, r4, r9, r12}
	ldr			r4, =GP_fpu_regs
	ldrb		r0, [r4, #(GP_fpu_top - GP_fpu_regs)]
	add			r4, r0, lsl #3							// r4 = st(top) address
#if defined(__ARM_PCS_VFP)
	vldr        d0,[r4]									// d0 = st(top) value
	bl          cos										// d0 = cos(tmp)
	vstr        d0,[r4]									// st(top) = cos(tmp)
#else
	ldrd        r0,[r4]									// r0:r1 = st(top) value
	bl          cos										// r0:r1 = cos(tmp)
	strd        r0,[r4]									// st(top) = cos(tmp)
#endif
	pop			{r0, r4, r9, r12}
	//------
	// FPU_SET_C2(0);
	//------
	ldr			r3, =GP_fpu_regs
	ldrh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic			r1, #FPU_C2_FLAG
	strh		r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	b			restore_flags_from_r0

	.ltorg

// ------------------- DA = ESC(2) ---------------------
// All modrm variations supported!
//
// Note: fcompp should be fucompp, but DOSBox does not differentiate them either.
//
	.global	op_da_USE32
op_da_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, i32
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, fcompp_st1, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	
	modrm_genall_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, _t0, i32

fadd_t0_bp_i32:
	mem_handler_bp
fadd_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fadd_TOP_d0_cont

	
fmul_t0_bp_i32:
	mem_handler_bp
fmul_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fmul_TOP_d0_cont


fcom_t0_bp_i32:
	mem_handler_bp
fcom_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fcom_TOP_d0_cont


fcomp_t0_bp_i32:
	mem_handler_bp
fcomp_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fcomp_TOP_d0_cont


fsub_t0_bp_i32:
	mem_handler_bp
fsub_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fsub_TOP_d0_cont


fsubr_t0_bp_i32:
	mem_handler_bp
fsubr_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fsubr_TOP_d0_cont


fdiv_t0_bp_i32:
	mem_handler_bp
fdiv_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fdiv_TOP_d0_cont


fdivr_t0_bp_i32:
	mem_handler_bp
fdivr_t0_i32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
	vmov			s0, r0
	//------
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.d);
	//------
	vcvt.f64.s32 	d0,s0
	b				fdivr_TOP_d0_cont


// ------------------- DB = ESC(3) ---------------------
// All modrm variations supported!
//
// 0150:19837F	DB842494000000	fild	[esp+00000094]		(X-COM UFO)
// 0150:196480	2EDB2D			fldt	cs:[0019645C]		(X-COM UFO)
// Note: DB E8+i (FUCOMI) instruction is valid only for the Pentium Pro and subsequent processors.
//		 DB F0+i (FCOMI) instruction is valid only for the Pentium Pro and subsequent processors.
//
	.global	op_db_USE32
op_db_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fild, back1, fist, fistp, back1, fld_80, back1, fstp_80, "32"
	//0xc0 = mod = 11b => two register operands
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, fclex, finit, loop, loop, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_1_oper fild, "skip", fist, fistp, "skip", fld_80, "skip", fstp_80, _t0, "32"

fild_t0_bp_32:
	mem_handler_bp
fild_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			r2, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, r2, lsl #24
#endif
fild_r0_cont:
	vmov			s0, r0
	vcvt.f64.s32 	d0,s0
	//-------
	//	FPU_PREP_PUSH();
	//-------
	FPU_PREP_PUSH_r0r1r3
	//-------
	//	FPU_FLD_I32(addr,TOP);
	//		Bit32s blah = mem_readd(addr);
	//		fpu.regs[store_to].d = static_cast<Real64>(blah);
	//-------
	ldr			r2, =GP_fpu_regs
	ldrb		r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add			r2, r0, lsl #3							// r2 = st(top) address
	vstr		d0, [r2]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fist_t0_bp_32:
	mem_handler_bp
fist_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_FST_I32(addr);
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	//	mem_writed(addr,static_cast<Bit32s>(FROUND(fpu.regs[TOP].d)));
	//------
1:	mrs				r0,cpsr								// Save flags to r0
	ldr				r3, =GP_fpu_regs
	push			{r0, r2, r9, r12}					// Save flags, store address, r12
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       	{d8}
	ldrb			r1, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add				r3, r0, lsl #3						// r3 = st(top) address
	vldr        	d0,[r3]								// d0 = st(top) value = in
	cmp				r1, #float_round_nearest_even
	bne				1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    	d8,d0								// d8 = in
	bl          	floor
#if defined(RPi) || defined(Roku)
	ldr				r0,=fpu_lit_05
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#0.5
#endif
	vmov.f64    	d2,d0								// d2 = floor(in)
	vsub.f64    	d0,d8,d0
	vcmp.f64   		d0,d1
	vmrs        	apsr_nzcv,fpscr
	beq         	4f
	blt				3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr				r0,=fpu_lit_1
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#1.0
#endif
	vadd.f64    	d0,d2,d1							// d0 = floor(in) + 1
	b           	2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    	d0,d2								// d0 = floor(in)
	b           	2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vcvt.s32.f64 	s0,d2
	vmov        	r1,s0								// r1 = returned integer value
	tst         	r1,#1
	addne			r1,#1								// r1 = floor(in) + 1
	b           	5f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp				r1, #float_round_down
	bne				1f
	bl          	floor
	b				2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp				r1, #float_round_up
	bne				2f
	bl          	ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
2:	vcvt.s32.f64 	s0,d0
	vmov        	r1,s0									// r1 = returned integer value
5:	vpop			{d8}
#else
	ldrb		r2, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r4 = st(top) address
	cmp			r2, #float_round_nearest_even
	ldrd        r0,[r3]									// d0 = st(top) value = in
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2iz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit32s temp= static_cast<Bit32s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2iz
	//------
	// Return r1 = Bit32s value
	//------
5:	mov			r1, r0
#endif
	pop				{r0, r2, r9, r12}						// Pop flags, store address, r12
	//------
	//	mem_writed(addr,static_cast<Bit32s>(FROUND(fpu.regs[TOP].d)));
	//------
#if USE_UNALIGNED
	str				r1, [r2]
#else
	strb			r1, [r2]
	lsr				r1, #8
	strb			r1, [r2, #1]
	lsr				r1, #8
	strb			r1, [r2, #2]
	lsr				r1, #8
	strb			r1, [r2, #3]
#endif
	b				restore_flags_from_r0


fistp_t0_bp_32:
	mem_handler_bp
fistp_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_FST_I32(addr);
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	//	mem_writed(addr,static_cast<Bit32s>(FROUND(fpu.regs[TOP].d)));
	//------
1:	mrs				r0,cpsr								// Save flags to r0
	ldr				r3, =GP_fpu_regs
	push			{r0, r2, r9, r12}					// Save flags, store address, r12
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       	{d8}
	ldrb			r1, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add				r3, r0, lsl #3						// r3 = st(top) address
	vldr        	d0,[r3]								// d0 = st(top) value = in
	cmp				r1, #float_round_nearest_even
	bne				1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    	d8,d0								// d8 = in
	bl          	floor
#if defined(RPi) || defined(Roku)
	ldr				r0,=fpu_lit_05
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#0.5
#endif
	vmov.f64    	d2,d0								// d2 = floor(in)
	vsub.f64    	d0,d8,d0
	vcmp.f64   		d0,d1
	vmrs        	apsr_nzcv,fpscr
	beq         	4f
	blt				3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr				r0,=fpu_lit_1
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#1.0
#endif
	vadd.f64    	d0,d2,d1							// d0 = floor(in) + 1
	b           	2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    	d0,d2								// d0 = floor(in)
	b           	2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vcvt.s32.f64 	s0,d2
	vmov        	r1,s0								// r1 = returned integer value
	tst         	r1,#1
	addne			r1,#1								// r1 = floor(in) + 1
	b           	5f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp				r1, #float_round_down
	bne				1f
	bl          	floor
	b				2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp				r1, #float_round_up
	bne				2f
	bl          	ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
2:	vcvt.s32.f64 	s0,d0
	vmov        	r1,s0									// r1 = returned integer value
5:	vpop			{d8}
#else
	ldrb		r2, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r4 = st(top) address
	cmp			r2, #float_round_nearest_even
	ldrd        r0,[r3]									// d0 = st(top) value = in
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2iz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit32s temp= static_cast<Bit32s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2iz
	//------
	// Return r1 = Bit32s value
	//------
5:	mov			r1, r0
#endif
	pop				{r0, r2, r9, r12}						// Pop flags, store address, r12
	//------
	//	mem_writed(addr,static_cast<Bit32s>(FROUND(fpu.regs[TOP].d)));
	//------
#if USE_UNALIGNED
	str				r1, [r2]
#else
	strb			r1, [r2]
	lsr				r1, #8
	strb			r1, [r2, #1]
	lsr				r1, #8
	strb			r1, [r2, #2]
	lsr				r1, #8
	strb			r1, [r2, #3]
#endif
	msr				cpsr_f, r0								// Restore flags from r0
	//------
	//	FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fld_80_t0_bp_32:
	mem_handler_bp
fld_80_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
1:	mrs				r0,cpsr								// Save flags to r0
	push			{r0}								// Save flags
	//-------
	//	FPU_PREP_PUSH();
	//-------
	FPU_PREP_PUSH_r0r1r3
	//------
	// fpu.regs[TOP].d = FPU_FLD80(addr);
	//    bits64 aSig = extractFloatx80Frac( a );
	//------
#if USE_UNALIGNED
	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
	ldrsh			r2, [r2, #8]					// r2 = aExp and aSign
#else
	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
	ldrb			r3, [r2, #8]
	ldrsb			r2, [r2, #9]
	orr				r2, r3, r2, lsl #8
#endif
	//------
	//    int32 aExp = extractFloatx80Exp( a );
	//    flag aSign = extractFloatx80Sign( a );
	//------
	mov				r3, r2, asr #15					// r3 = aSign
#if defined(RPi) || defined(Roku)
	bic				r2, r3, lsl #15					// Clear out the sign from r2 = aExp
#else
	ubfx			r2, r2, #0, #15					// r2 = aExp
#endif
	//------
	//    if ( aExp == 0x7FFF ) {
	//------
	add				r2, #1
	cmp				r2, #0x8000
	sub				r2, #1
	bne				fld_exp_not_7FFF
	//------
	//        if ( !(bits64) ( aSig<<1 ) ) {
	//------
	mov				lr, r1, lsl #1
	orrs			lr, r0
	bne				1f
	//------
	//			  return packFloat64( aSign, 0x7FF, 0 );
	//------
	orr				r1, r3, lsl #31					// Set the sign bit from r3 to r1 highest bit
	orr				r1, #0x7F000000
	orr				r1, #0x00F00000					// Set the exponent to be 0x7FF
	b				fld_80_done
	//------
	//		   else
	//            return commonNaNToFloat64( floatx80ToCommonNaN( a ) );
	//------
1:	lsr				r1, #12							// Set the bits from the aSig high word to the 64bit result aSig
	orr				r1, r3, lsl #31					// Set the sign bit from r3 to r1 highest bit
#if defined(RPi) || defined(Roku)
	mov				r3, #0x8000
	sub				r3, #8
#else
	movw			r3, #0x7FF8
#endif
	lsl				r3, #16
	orr				r1, r3							// Set the exponent to be 0x7FF and Sig to have highest bit set
	b				fld_80_done
	//------
	//    }
	//    shift64RightJamming( aSig, 1, &zSig );
	//------
fld_exp_not_7FFF:
	//------
	//    if ( aExp || aSig ) aExp -= 0x3C00;
	//------
	orr				lr, r0, r1
	orrs			lr, r2
	mov				lr, #0x3C00
	subne			r2, lr
	//------
	//    Bit64s mant64 = (test.eind.ll >> 11) & LONGTYPE(0xfffffffffffff);
	//------
	lsr				r0, #11
	lsl				lr, r1, #(32-11)
	orr				r0, lr
	bic				r1, #0x80000000
	lsr				r1, #11
	//------
	//    result.ll = (sign <<63)|(exp64final << 52)| mant64;
	//------
	orr				r1, r3, lsl #31							// Set the sign bit from r3 to r1 highest bit
	orr				r1, r2, lsl #20							// Set the exponent from r2
	//------
	// Save the Float64 value to fpu_regs[TOP]
	//------
fld_80_done:
	ldr				r3, =GP_fpu_regs
	ldrb			r2, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r2, r3, r2, lsl #3						// r2 = st(top) address
	str				r0, [r2]
	str				r1, [r2, #4]
	//------
	//	FPU_SET_C1(0);
	//------
	pop				{r0}									// Restore flags
	ldrh			r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	bic				r1, #FPU_C1_FLAG
	strh			r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
#if LOG_FPU
	msr				cpsr_f, r0
	b				loop_log
#else
	b				restore_flags_from_r0
#endif

fstp_80_t0_bp_32:
	mem_handler_bp
fstp_80_t0_32:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
1:	mrs				r0,cpsr									// Save flags to r0
	push			{r0, r2}								// Save flags and store address
	//------
	//	FPU_FST_F80(addr);
	//	 FPU_ST80(addr,TOP);
	//------
	ldr				r3, =GP_fpu_regs
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r3, r0, lsl #3							// r3 = st(top) address
	ldr				r0, [r3]
	ldr				r1, [r3, #4]
	//------
	//    bits64 aSig = extractFloat64Frac( a );
	//    int16 aExp = extractFloat64Exp( a );
	//    flag aSign = extractFloat64Sign( a );
	//    if ( aExp == 0x7FF ) {
	//------
#if defined(RPi) || defined(Roku)
	bic				r2, r1, #0x80000000
	lsr				r2, #20
	mov				r3, #0x800
	sub				r3, #1
#else
	ubfx			r2, r1, #20, #11						// r2 = aExp
	movw			r3, #0x7FF
#endif
	cmp				r2, r3
	bne				fstp_80_exp_not_7FF
	//------
	//        if ( aSig )
	//------
#if defined(RPi) || defined(Roku)
	mov				r3, r1, lsl #12
	lsr				r3, #12
#else
	ubfx			r3, r1, #0, #20
#endif
	orrs			r3, r0
	beq				fstp_80_aSig_0
	//------
	//			  return commonNaNToFloatx80( float64ToCommonNaN( a ) );
    // a.sign = a>>63;
    // a.low = 0;
    // a.high = a<<12;
    // z.low = LIT64( 0xC000000000000000 ) | ( a.high>>1 );
    // z.high = ( ( (bits16) a.sign )<<15 ) | 0x7FFF;
    // return z;
	//------
	mov				r2, r1, asr #31
#if defined(RPi) || defined(Roku)
	mov				r3, #0x8000
	sub				r3, #1
#else
	movw			r3, #0x7FFF
#endif
	orr				r2, r3						// r2 = z.high = ( ( (bits16) a.sign )<<15 ) | 0x7FFF;
	lsl				r1, #11
	mov				r3, r0, lsr #(32-11)
	orr				r1, r0
	lsl				r0, #11
	orr				r1, #0xC0000000				// r0,r1 = z.low = LIT64( 0xC000000000000000 ) | ( a.high>>1 );
	b				fstp_80_save						// Go save
	//------
	//        return packFloatx80( aSign, 0x7FFF, LIT64( 0x8000000000000000 ) );
	//------
fstp_80_aSig_0:
	mov				r2, r1, asr #31
#if defined(RPi) || defined(Roku)
	mov				r3, #0x8000
	sub				r3, #1
#else
	movw			r3, #0x7FFF
#endif
	orr				r2, r3						// r2 = z.high = ( ( (bits16) a.sign )<<15 ) | 0x7FFF;
	mov				r1, #0x80000000
	mov				r0, #0
	b				fstp_80_save						// Go save
	//------
	//    }
	//    if ( aExp == 0 ) {
	//------
fstp_80_exp_not_7FF:
	cmp				r2, #0
	bne				fstp_80_exp_not_0
	//------
	//        if ( aSig == 0 )
	//			  return packFloatx80( aSign, 0, 0 );
	//------
#if defined(RPi) || defined(Roku)
	mov				r3, r1, lsl #12
	lsr				r3, #12
#else
	ubfx			r3, r1, #0, #20
#endif
	orrs			r3, r0
	bne				fstp_80_aSig_not_0
	mov				r2, r1, asr #31
	mov				r1, #0								// r0 = r1 = 0
	lsl				r2, #15
	b				fstp_80_save						// Go save +0.0 or -0.0
fstp_80_aSig_not_0:
	//------
	//        normalizeFloat64Subnormal( aSig, &aExp, &aSig );
	//    }
	// TODO!
	//------
fstp_80_exp_not_0:
	//------
	//    return packFloatx80(aSign, aExp + 0x3C00, ( aSig | LIT64( 0x0010000000000000 ) )<<11 );
	//------
	mov				r3, r1, asr #31				// r3 = aSign
	add				lr, r2, #0x3C00				// lr = aExp + 0x3C00
#if defined(RPi) || defined(Roku)
	orr				lr, r3, lsl #15
#else
	bfi				lr, r3, #15, #1				// lr = aSign, aExp + 0x3C00
#endif
	orr				r1, #0x00100000				// t0,t1 = ( aSig | LIT64( 0x0010000000000000 ) )
	lsl				r1, #11
	mov				r3, r0, lsr #(32-11)
	orr				r1, r3
	mov				r3, r0, lsl #11				// r3,r1 = ( aSig | LIT64( 0x0010000000000000 ) )<<11
fstp_80_save:
	pop				{r0, r2}
#if USE_UNALIGNED
	str				r3, [r2]
	str				r1, [r2, #4]
	strh			lr, [r2, #8]
#else
	strb			r3, [r2]
	lsr				r3, #8
	strb			r3, [r2, #1]
	lsr				r3, #8
	strb			r3, [r2, #2]
	lsr				r3, #8
	strb			r3, [r2, #3]
	strb			r1, [r2, #4]
	lsr				r1, #8
	strb			r1, [r2, #5]
	lsr				r1, #8
	strb			r1, [r2, #6]
	lsr				r1, #8
	strb			r1, [r2, #7]
	strb			lr, [r2, #8]
	lsr				lr, #8
	strb			lr, [r2, #9]
#endif
	msr				cpsr_f, r0								// Restore flags from r0
	//------
	//	FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fclex:
	ldr				r3, =GP_fpu_regs
	ldrh			r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	and				r1, #0x7F00				// Clear exceptions
	strh			r1, [r3, #(GP_fpu_sw - GP_fpu_regs)]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

	.global	finit
finit:
	//------
	//	FPU_SetCW(0x37F);
	//------
#if defined(RPi) || defined(Roku)
	mov				r0, #0x7F
	orr				r0, #0x300
#else
	movw			r0, #0x37F
#endif
	FPU_SetCW_r3 	r0
	//------
	//	fpu.sw=0;
	//------
	mov				r0, #0
	strh			r0, [r3, #(GP_fpu_sw - GP_fpu_regs)]
	//------
	//	TOP=FPU_GET_TOP();
	//------
	strb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	//------
	//	fpu.tags[0]=TAG_Empty;
	//	fpu.tags[1]=TAG_Empty;
	//	fpu.tags[2]=TAG_Empty;
	//	fpu.tags[3]=TAG_Empty;
	//	fpu.tags[4]=TAG_Empty;
	//	fpu.tags[5]=TAG_Empty;
	//	fpu.tags[6]=TAG_Empty;
	//	fpu.tags[7]=TAG_Empty;
	//	fpu.tags[8]=TAG_Valid// // is only used by us
	//------
	mov				r0, #TAG_Empty
	strb			r0, [r3, #(GP_fpu_tags - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 1 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 2 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 3 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 4 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 5 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 6 - GP_fpu_regs)]
	strb			r0, [r3, #(GP_fpu_tags + 7 - GP_fpu_regs)]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

	.ltorg
	
// ------------------- DC = ESC(4) ---------------------
// All modrm variations supported!
//
// 0160:194CE9	DC0D65561D00	fmulq	[001D5665]		(Destruction Derby)
//
	.global	op_dc_USE32
op_dc_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, f64
	//0xc0 = mod = 11b => two register operands
	.word fadd_st0_st, fadd_st1_st, fadd_st2_st, fadd_st3_st, fadd_st4_st, fadd_st5_st, fadd_st6_st, fadd_st7_st
	.word fmul_st0_st, fmul_st1_st, fmul_st2_st, fmul_st3_st, fmul_st4_st, fmul_st5_st, fmul_st6_st, fmul_st7_st
	.word fcom_st0, fcom_st1, fcom_st2, fcom_st3, fcom_st4, fcom_st5, fcom_st6, fcom_st7
	.word fcomp_st0, fcomp_st1, fcomp_st2, fcomp_st3, fcomp_st4, fcomp_st5, fcomp_st6, fcomp_st7
	.word fsubr_st0_st, fsubr_st1_st, fsubr_st2_st, fsubr_st3_st, fsubr_st4_st, fsubr_st5_st, fsubr_st6_st, fsubr_st7_st
	.word fsub_st0_st, fsub_st1_st, fsub_st2_st, fsub_st3_st, fsub_st4_st, fsub_st5_st, fsub_st6_st, fsub_st7_st
	.word fdivr_st0_st, fdivr_st1_st, fdivr_st2_st, fdivr_st3_st, fdivr_st4_st, fdivr_st5_st, fdivr_st6_st, fdivr_st7_st
	.word fdiv_st0_st, fdiv_st1_st, fdiv_st2_st, fdiv_st3_st, fdiv_st4_st, fdiv_st5_st, fdiv_st6_st, fdiv_st7_st

	modrm_genall_1_oper fadd, fmul, fcom, fcomp, fsub, fsubr, fdiv, fdivr, _t0, f64

fadd_t0_bp_f64:
	mem_handler_bp
fadd_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fadd_TOP_d0_cont

	
fmul_t0_bp_f64:
	mem_handler_bp
fmul_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fmul_TOP_d0_cont


fcom_t0_bp_f64:
	mem_handler_bp
fcom_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fcom_TOP_d0_cont


fcomp_t0_bp_f64:
	mem_handler_bp
fcomp_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fcomp_TOP_d0_cont


fsub_t0_bp_f64:
	mem_handler_bp
fsub_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fsub_TOP_d0_cont


fsubr_t0_bp_f64:
	mem_handler_bp
fsubr_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fsubr_TOP_d0_cont


fdiv_t0_bp_f64:
	mem_handler_bp
fdiv_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fdiv_TOP_d0_cont


fdivr_t0_bp_f64:
	mem_handler_bp
fdivr_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
#if USE_UNALIGNED
1:	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
1:	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_temp
	str				r0, [r3]
	str				r1, [r3, #4]
	vldr			d0, [r3]
	b				fdivr_TOP_d0_cont


fadd_st0_st:
	mov		r1, #0
	b		fadd_sti_st
fadd_st2_st:
	mov		r1, #2
	b		fadd_sti_st
fadd_st3_st:
	mov		r1, #3
	b		fadd_sti_st
fadd_st4_st:
	mov		r1, #4
	b		fadd_sti_st
fadd_st5_st:
	mov		r1, #5
	b		fadd_sti_st
fadd_st6_st:
	mov		r1, #6
	b		fadd_sti_st
fadd_st7_st:
	mov		r1, #7
	b		fadd_sti_st
fadd_st1_st:
	mov		r1, #1
fadd_sti_st:
	//------
	// fadd
	// Syntax    FADD st(i),st
	// FPU_FADD(TOP,STV(sub));
	//	fpuregs[st]d += fpuregs[other]d;
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vadd.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fmul_st0_st:
	mov		r1, #0
	b		fmul_sti_st
fmul_st2_st:
	mov		r1, #2
	b		fmul_sti_st
fmul_st3_st:
	mov		r1, #3
	b		fmul_sti_st
fmul_st4_st:
	mov		r1, #4
	b		fmul_sti_st
fmul_st5_st:
	mov		r1, #5
	b		fmul_sti_st
fmul_st6_st:
	mov		r1, #6
	b		fmul_sti_st
fmul_st7_st:
	mov		r1, #7
	b		fmul_sti_st
fmul_st1_st:
	mov		r1, #1
fmul_sti_st:
	//------
	// FMUL
	// Syntax    FMUL st(i),st
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vmul.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fsubr_st0_st:
	mov		r1, #0
	b		fsubr_sti_st
fsubr_st2_st:
	mov		r1, #2
	b		fsubr_sti_st
fsubr_st3_st:
	mov		r1, #3
	b		fsubr_sti_st
fsubr_st4_st:
	mov		r1, #4
	b		fsubr_sti_st
fsubr_st5_st:
	mov		r1, #5
	b		fsubr_sti_st
fsubr_st6_st:
	mov		r1, #6
	b		fsubr_sti_st
fsubr_st7_st:
	mov		r1, #7
	b		fsubr_sti_st
fsubr_st1_st:
	mov		r1, #1
fsubr_sti_st:
	//------
	// FSUBR (Subtract in reverse two floating point values) 
	// Syntax    fsubr st(i),st
	// 
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Overflow, Underflow, Precision
	// This instruction performs a signed subtraction of the content of the ST(i)
	// data register from the content of ST(0) and overwrites the content of
	// the ST(i) register with the result
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vsub.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	

fsub_st0_st:
	mov		r1, #0
	b		fsub_sti_st
fsub_st2_st:
	mov		r1, #2
	b		fsub_sti_st
fsub_st3_st:
	mov		r1, #3
	b		fsub_sti_st
fsub_st4_st:
	mov		r1, #4
	b		fsub_sti_st
fsub_st5_st:
	mov		r1, #5
	b		fsub_sti_st
fsub_st6_st:
	mov		r1, #6
	b		fsub_sti_st
fsub_st7_st:
	mov		r1, #7
	b		fsub_sti_st
fsub_st1_st:
	mov		r1, #1
fsub_sti_st:
	//------
	// FSUB (Subtract two floating point values) 
	// Syntax    fsub st(i),st
	// 
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Overflow, Underflow, Precision
	// This instruction performs a signed subtraction of the content of the ST(0)
	// data register from the content of ST(i) and overwrites the content of
	// the ST(i) register with the result
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vsub.f64    	d0, d1, d0
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fdivr_st0_st:
	mov		r1, #0
	b		fdivr_sti_st
fdivr_st2_st:
	mov		r1, #2
	b		fdivr_sti_st
fdivr_st3_st:
	mov		r1, #3
	b		fdivr_sti_st
fdivr_st4_st:
	mov		r1, #4
	b		fdivr_sti_st
fdivr_st5_st:
	mov		r1, #5
	b		fdivr_sti_st
fdivr_st6_st:
	mov		r1, #6
	b		fdivr_sti_st
fdivr_st7_st:
	mov		r1, #7
	b		fdivr_sti_st
fdivr_st1_st:
	mov		r1, #1
fdivr_sti_st:
	//------
	// fdivr (Divide two floating point values) 
	// Syntax    fdivr st(i),st
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//					Overflow, Underflow, Precision, Zero divide
	// This instruction performs a signed division of the content of the ST(0) data
	// register by the content of the ST(i) data register The result then overwrites
	// the content of the ST(i) register 
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vdiv.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fdiv_st0_st:
	mov		r1, #0
	b		fdiv_sti_st
fdiv_st2_st:
	mov		r1, #2
	b		fdiv_sti_st
fdiv_st3_st:
	mov		r1, #3
	b		fdiv_sti_st
fdiv_st4_st:
	mov		r1, #4
	b		fdiv_sti_st
fdiv_st5_st:
	mov		r1, #5
	b		fdiv_sti_st
fdiv_st6_st:
	mov		r1, #6
	b		fdiv_sti_st
fdiv_st7_st:
	mov		r1, #7
	b		fdiv_sti_st
fdiv_st1_st:
	mov		r1, #1
fdiv_sti_st:
	//------
	// FDIV (Divide two floating point values) 
	// Syntax    fdiv st(i),st
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//					Overflow, Underflow, Precision, Zero divide
	// This instruction performs a signed division of the content of the ST(i) data
	// register by the content of the ST(0) data register The result then overwrites
	// the content of the ST(i) register and the TOP data register is popped
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr    		d1,[r1]								// d1 = st(i)
	vdiv.f64    	d0, d1, d0
	vstr        	d0,[r1]								// st(i) = result
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

	.ltorg
	
// ------------------- DD = ESC(5) ---------------------
// Mostly operations with "double" values
// 0150:198A47	DD7DF8		fstsw	[ebp-08]	(X-COM UFO)
// 0150:198A52	DD5DF0		fstpq	[ebp-10]	(X-COM UFO)
// 0160:1CE1A2	DD4424		fldq	[esp+4]		(Destruction Derby)

	.global	op_dd_USE32
op_dd_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fld, back1, fst, fstp, back1, back1, back1, fnstsw, f64
	//0xc0 = mod = 11b => two register operands
	.word ffree_st0, ffree_st1, ffree_st2, ffree_st3, ffree_st4, ffree_st5, ffree_st6, ffree_st7
	.word fxch_st0, fxch_st1, fxch_st2, fxch_st3, fxch_st4, fxch_st5, fxch_st6, fxch_st7
	.word fst_st0, fst_st1, fst_st2, fst_st3, fst_st4, fst_st5, fst_st6, fst_st7
	.word fstp_st0, fstp_st1, fstp_st2, fstp_st3, fstp_st4, fstp_st5, fstp_st6, fstp_st7
	.word fcom_st0, fcom_st1, fcom_st2, fcom_st3, fcom_st4, fcom_st5, fcom_st6, fcom_st7
	.word fcomp_st0, fcomp_st1, fcomp_st2, fcomp_st3, fcomp_st4, fcomp_st5, fcomp_st6, fcomp_st7
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_1_oper fld, "skip", fst, fstp, "skip", "skip", "skip", fnstsw, _t0, f64

fld_t0_bp_f64:
	mem_handler_bp
fld_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_PREP_PUSH();
	//------
1:	FPU_PREP_PUSH_r0r1r3
	//------
	//	FPU_FLD_F64(addr,TOP);
	//------
#if USE_UNALIGNED
	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	ldr				r3, =GP_fpu_regs
	ldrb			r2, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r2, r3, r2, lsl #3					// r2 = st(top) address
	str				r0, [r2]
	str				r1, [r2, #4]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fst_t0_bp_f64:
	mem_handler_bp
fst_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//-------
	//	FPU_FST_F64(addr);
	//-------
1:	ldr				r3, =GP_fpu_regs
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r3, r0, lsl #3				// r3 = st(top) address
	ldr				r0, [r3]
	ldr				r1, [r3, #4]
#if USE_UNALIGNED
	str				r0, [r2]
	str				r1, [r2, #4]
#else
	strb			r0, [r2]
	lsr				r0, #8
	strb			r0, [r2, #1]
	lsr				r0, #8
	strb			r0, [r2, #2]
	lsr				r0, #8
	strb			r0, [r2, #3]
	strb			r1, [r2, #4]
	lsr				r1, #8
	strb			r1, [r2, #5]
	lsr				r1, #8
	strb			r1, [r2, #6]
	lsr				r1, #8
	strb			r1, [r2, #7]
#endif
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

	
fstp_t0_bp_f64:
	mem_handler_bp
fstp_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//-------
	//	FPU_FST_F64(addr);
	//-------
1:	ldr				r3, =GP_fpu_regs
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r3, r0, lsl #3				// r3 = st(top) address
	ldr				r0, [r3]
	ldr				r1, [r3, #4]
#if USE_UNALIGNED
	str				r0, [r2]
	str				r1, [r2, #4]
#else
	strb			r0, [r2]
	lsr				r0, #8
	strb			r0, [r2, #1]
	lsr				r0, #8
	strb			r0, [r2, #2]
	lsr				r0, #8
	strb			r0, [r2, #3]
	strb			r1, [r2, #4]
	lsr				r1, #8
	strb			r1, [r2, #5]
	lsr				r1, #8
	strb			r1, [r2, #6]
	lsr				r1, #8
	strb			r1, [r2, #7]
#endif
	//-------
	//	FPU_FPOP();
	//-------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fnstsw_t0_bp_f64:
	mem_handler_bp
fnstsw_t0_f64:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_SET_TOP(TOP);
	//------
1:	FPU_SET_TOP_r0r1r3				// Leaves current fpu_sw into r0 register
	//------
	//	mem_writew(addr,fpu.sw);
	//	//seems to break all dos4gw games :)
	//------
	strb			r0, [r2]
	lsr				r0, #8
	strb			r0, [r2, #1]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


ffree_st0:
	mov		r1, #0
	b		ffree
ffree_st1:
	mov		r1, #1
	b		ffree
ffree_st2:
	mov		r1, #2
	b		ffree
ffree_st3:
	mov		r1, #3
	b		ffree
ffree_st4:
	mov		r1, #4
	b		ffree
ffree_st5:
	mov		r1, #5
	b		ffree
ffree_st6:
	mov		r1, #6
	b		ffree
ffree_st7:
	mov		r1, #7
ffree:
	//------
	// FFREE
	// Syntax    ffree st(i)
	//------
	ldr				r3, =GP_fpu_regs
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r2, r3, #(GP_fpu_tags - GP_fpu_regs)
	add				r1, r0
	and				r1, #7
	mov				r0, #TAG_Empty
	strb			r0, [r2, r1]
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fst_st0:
	mov		r1, #0
	b		fst_ST
fst_st2:
	mov		r1, #2
	b		fst_ST
fst_st3:
	mov		r1, #3
	b		fst_ST
fst_st4:
	mov		r1, #4
	b		fst_ST
fst_st5:
	mov		r1, #5
	b		fst_ST
fst_st6:
	mov		r1, #6
	b		fst_ST
fst_st7:
	mov		r1, #7
	b		fst_ST
fst_st1:
	mov		r1, #1
fst_ST:
	//------
	//	FPU_FST(TOP,STV(sub));
	//		fputags[other] = fputags[st];
	//		fpuregs[other] = fpuregs[st];
	//------
	ldr				r3, =GP_fpu_regs
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r2, r3, #(GP_fpu_tags - GP_fpu_regs)
	add				r1, r0
	ldrb			lr, [r2, r0]				// lr = tag(top)
	and				r1, #7
	strb			lr, [r2, r1]				// tag(i) = tag(top)
	add				r2, r3, r1, lsl #3
	add				r3, r0, lsl #3
	ldr				r0, [r3]
	ldr				r1, [r3, #4]				// r0:r1 = value from top
	str				r0, [r2]
	str				r1, [r2, #4]				// Save to st(i)
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


// ------------------- DE = ESC(6) ---------------------
// 0150:19D63D	DEF9		fdivp	st(1), st	(X-COM UFO)
// 0150:18B884	DEF1		fdivrp	st(1), st	(X-COM UFO)
//
	.global	op_de_USE32
op_de_USE32:
	modrm_jump_32
	modrm_tbl_1_oper back1, back1, back1, back1, back1, back1, back1, back1, i16
	//0xc0 = mod = 11b => two register operands
	.word faddp_st0, faddp_st1, faddp_st2, faddp_st3, faddp_st4, faddp_st5, faddp_st6, faddp_st7
	.word fmulp_st0, fmulp_st1, fmulp_st2, fmulp_st3, fmulp_st4, fmulp_st5, fmulp_st6, fmulp_st7
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, fcompp_st1, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word fsubrp_st0, fsubrp_st1, fsubrp_st2, fsubrp_st3, fsubrp_st4, fsubrp_st5, fsubrp_st6, fsubrp_st7
	.word fsubp_st0, fsubp_st1, fsubp_st2, fsubp_st3, fsubp_st4, fsubp_st5, fsubp_st6, fsubp_st7
	.word fdivrp_st0, fdivrp_st1, fdivrp_st2, fdivrp_st3, fdivrp_st4, fdivrp_st5, fdivrp_st6, fdivrp_st7
	.word fdivp_st0, fdivp_st1, fdivp_st2, fdivp_st3, fdivp_st4, fdivp_st5, fdivp_st6, fdivp_st7

faddp_st0:
	mov		r1, #0
	b		faddp
faddp_st2:
	mov		r1, #2
	b		faddp
faddp_st3:
	mov		r1, #3
	b		faddp
faddp_st4:
	mov		r1, #4
	b		faddp
faddp_st5:
	mov		r1, #5
	b		faddp
faddp_st6:
	mov		r1, #6
	b		faddp
faddp_st7:
	mov		r1, #7
	b		faddp
faddp_st1:
	mov		r1, #1
faddp:
	//------
	// FADDP
	// Syntax    faddp st(i),st
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vadd.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	

fmulp_st0:
	mov		r1, #0
	b		fmulp
fmulp_st2:
	mov		r1, #2
	b		fmulp
fmulp_st3:
	mov		r1, #3
	b		fmulp
fmulp_st4:
	mov		r1, #4
	b		fmulp
fmulp_st5:
	mov		r1, #5
	b		fmulp
fmulp_st6:
	mov		r1, #6
	b		fmulp
fmulp_st7:
	mov		r1, #7
	b		fmulp
fmulp_st1:
	mov		r1, #1
fmulp:
	//------
	// FMULP
	// Syntax    fmulp st(i),st
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vmul.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fcompp_st1:
	//------
	// FCOMPP (Compare ST(0) to ST(1) and POP both registers) 
	//
	// Syntax:    fcompp (no operand)
	//
	// Exception flags: Stack Fault, Invalid operation, Denormalized value
	// This instruction is the same as the FCOM and FCOMP instructions with the following two differences: 
	// - ST(0) can only be compared to ST(1) and 
	// - both ST(0) and ST(1) are popped after the comparison is completed. 
	// This instruction is used when both the values in ST(0) and in ST(1)
	// would no longer be needed in FPU registers for further computation. 
	//
	//	FPU_FCOM(TOP,STV(1))//
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r1, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*TOP
	vldr    		d1,[r0]								// d1 = st(TOP)
	add				r1, #1
	and				r1, #7
	add				r0, r2, r1, lsl #3					// r0 = GP_fpu_regs + 8*(TOP+1)
	vldr    		d0,[r0]								// d0 = st(1)
	mrs				r0,cpsr								// Save flags to r0
	vcmp.f64		d1, d0
	vmrs        	apsr_nzcv,fpscr						// Get the ARM CPU flags from FPU flags
	//------
	// if NaN (N=0,Z=0,C=1,V=1)
	//	FPU_SET_C3(1)//FPU_SET_C2(1)//FPU_SET_C0(1);
	//------
	mrs				r1,cpsr								// Get the FPU comparison flags to r1
	ldrh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	and				r1, #(ARM_NEG|ARM_ZERO|ARM_CARRY|ARM_OVER)
	cmp				r1, #(ARM_CARRY|ARM_OVER)
	bic				r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)
	orreq			r3, #(FPU_C0_FLAG|FPU_C2_FLAG|FPU_C3_FLAG)	// Mark for NaN!
	msr				cpsr_f, r1							// Restore the FPU comparison flags to ARM CPU flags
	//------
	// else if (st == other)
	//	FPU_SET_C3(1)//FPU_SET_C2(0)//FPU_SET_C0(0);
	//------
	orreq			r3, #(FPU_C3_FLAG)
	//------
	// else if (st < other)
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(1)//return;
	//------
	orrlt			r3, #(FPU_C0_FLAG)
	//------
	// else
	//	FPU_SET_C3(0)//FPU_SET_C2(0)//FPU_SET_C0(0)//return;
	//------
	strh			r3, [r2, #(GP_fpu_sw - GP_fpu_regs)]// r3 = FPU status word
	msr				cpsr_f, r0							// Restore the original ARM CPU flags
	//------
	//	FPU_FPOP()// /* extra pop at the bottom*/
	//------
	FPU_FPOP_r0r1r2
	//------
	// FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
	
fsubrp_st0:
	mov		r1, #0
	b		fsubrp
fsubrp_st2:
	mov		r1, #2
	b		fsubrp
fsubrp_st3:
	mov		r1, #3
	b		fsubrp
fsubrp_st4:
	mov		r1, #4
	b		fsubrp
fsubrp_st5:
	mov		r1, #5
	b		fsubrp
fsubrp_st6:
	mov		r1, #6
	b		fsubrp
fsubrp_st7:
	mov		r1, #7
	b		fsubrp
fsubrp_st1:
	mov		r1, #1
fsubrp:
	//------
	// FSUBRP (Subtract in reverse two floating point values and pop ST(0)) 
	// Syntax    fsubrp st(i),st
	// 
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Overflow, Underflow, Precision
	// This instruction performs a signed subtraction of the content of the ST(i)
	// data register from the content of ST(0) and overwrites the content of
	// the ST(i) register with the result The TOP data register is then popped
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vsub.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fsubp_st0:
	mov		r1, #0
	b		fsubp
fsubp_st2:
	mov		r1, #2
	b		fsubp
fsubp_st3:
	mov		r1, #3
	b		fsubp
fsubp_st4:
	mov		r1, #4
	b		fsubp
fsubp_st5:
	mov		r1, #5
	b		fsubp
fsubp_st6:
	mov		r1, #6
	b		fsubp
fsubp_st7:
	mov		r1, #7
	b		fsubp
fsubp_st1:
	mov		r1, #1
fsubp:
	//------
	// FSUBP (Subtract two floating point values and pop ST(0)) 
	// Syntax    fsubp st(i),st
	// 
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//                  Overflow, Underflow, Precision
	// This instruction performs a signed subtraction of the content of the ST(0)
	// data register from the content of ST(i) and overwrites the content of
	// the ST(i) register with the result The TOP data register is then popped
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vsub.f64    	d0, d1, d0
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fdivrp_st0:
	mov		r1, #0
	b		fdivrp
fdivrp_st2:
	mov		r1, #2
	b		fdivrp
fdivrp_st3:
	mov		r1, #3
	b		fdivrp
fdivrp_st4:
	mov		r1, #4
	b		fdivrp
fdivrp_st5:
	mov		r1, #5
	b		fdivrp
fdivrp_st6:
	mov		r1, #6
	b		fdivrp
fdivrp_st7:
	mov		r1, #7
	b		fdivrp
fdivrp_st1:
	mov		r1, #1
fdivrp:
	//------
	// fdivrp (Divide two floating point values and pop ST(0)) 
	// Syntax    fdivrp st(i),st
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//					Overflow, Underflow, Precision, Zero divide
	// This instruction performs a signed division of the content of the ST(0) data
	// register by the content of the ST(i) data register The result then overwrites
	// the content of the ST(i) register and the TOP data register is popped 
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vdiv.f64    	d0, d0, d1
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

fdivp_st0:
	mov		r1, #0
	b		fdivp
fdivp_st2:
	mov		r1, #2
	b		fdivp
fdivp_st3:
	mov		r1, #3
	b		fdivp
fdivp_st4:
	mov		r1, #4
	b		fdivp
fdivp_st5:
	mov		r1, #5
	b		fdivp
fdivp_st6:
	mov		r1, #6
	b		fdivp
fdivp_st7:
	mov		r1, #7
	b		fdivp
fdivp_st1:
	mov		r1, #1
fdivp:
	//------
	// FDIVP (Divide two floating point values and pop ST(0)) 
	// Syntax    fdivp st(i),st
	//
	// Exception flags Stack Fault, Invalid operation, Denormalized value,
	//					Overflow, Underflow, Precision, Zero divide
	// This instruction performs a signed division of the content of the ST(i) data
	// register by the content of the ST(0) data register The result then overwrites
	// the content of the ST(i) register and the TOP data register is popped
	//------
	ldr				r2, =GP_fpu_regs
	ldrb			r0, [r2, #(GP_fpu_top - GP_fpu_regs)]
	add				r1, r0
	and				r1, #7
	add				r0, r2, r0, lsl #3					// r0 = st(top) address
	add				r1, r2, r1, lsl #3					// r1 = st(i) address
	vldr    		d0,[r0]								// d0 = st(top)
	vldr			d1,[r1]								// d1 = st(i)
	vdiv.f64    	d0, d1, d0
	vstr        	d0,[r1]								// st(i) = result
	//------
	// FPU_FPOP()//		
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif

	.ltorg
	
// ------------------- DF = ESC(7) ---------------------
// All modrm variations except FBLD/FBSTP supported!
//
// 0150:19D646	DFE0	fstsw	ax				(X-COM UFO)
// 0150:18B87C	DF0528	fildw	[001A8428]		(X-COM UFO)
// Note: DF F0+i (FCOMIP) instruction is valid only for the Pentium Pro and subsequent processors
//
	.global	op_df_USE32
op_df_USE32:
	modrm_jump_32
	modrm_tbl_1_oper fild, back1, fist, fistp, back1, fild_64, back1, fistp_64, i16
	//0xc0 = mod = 11b => two register operands
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word fxch_st0, fxch_st1, fxch_st2, fxch_st3, fxch_st4, fxch_st5, fxch_st6, fxch_st7
	.word fstp_st0, fstp_st1, fstp_st2, fstp_st3, fstp_st4, fstp_st5, fstp_st6, fstp_st7
	.word fstp_st0, fstp_st1, fstp_st2, fstp_st3, fstp_st4, fstp_st5, fstp_st6, fstp_st7
	.word fnstsw_ax, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown
	.word .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown, .unknown

	modrm_genall_1_oper fild, "skip", fist, fistp, "skip", fild_64, "skip", fistp_64, _t0, i16

fild_t0_bp_i16:
	mem_handler_bp
fild_t0_i16:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
1:	ldrb		r0, [r2]
	ldrsb		r1, [r2, #1]
	orr			r0, r1, lsl #8
	b			fild_r0_cont


fist_t0_bp_i16:
	mem_handler_bp
fist_t0_i16:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_FST_I16(addr);
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	//	mem_writew(addr,static_cast<Bit16s>(FROUND(fpu.regs[TOP].d)));
	//------
1:	mrs				r0,cpsr								// Save flags to r0
	ldr				r3, =GP_fpu_regs
	push			{r0, r2, r9, r12}					// Save flags, store address, r12
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       	{d8}
	ldrb			r1, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add				r3, r0, lsl #3						// r3 = st(top) address
	vldr        	d0,[r3]								// d0 = st(top) value = in
	cmp				r1, #float_round_nearest_even
	bne				1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    	d8,d0								// d8 = in
	bl          	floor
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_05
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#0.5
#endif
	vmov.f64    	d2,d0								// d2 = floor(in)
	vsub.f64    	d0,d8,d0
	vcmp.f64   		d0,d1
	vmrs        	apsr_nzcv,fpscr
	beq         	4f
	blt				3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_1
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#1.0
#endif
	vadd.f64    	d0,d2,d1							// d0 = floor(in) + 1
	b           	2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    	d0,d2								// d0 = floor(in)
	b           	2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vcvt.s32.f64 	s0,d2
	vmov        	r1,s0								// r1 = returned integer value
	tst         	r1,#1
	addne			r1,#1								// r1 = floor(in) + 1
	b           	5f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp				r1, #float_round_down
	bne				1f
	bl          	floor
	b				2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp				r1, #float_round_up
	bne				2f
	bl          	ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
2:	vcvt.s32.f64 	s0,d0
	vmov        	r1,s0									// r1 = returned integer value
5:	vpop			{d8}
#else
	ldrb		r2, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r4 = st(top) address
	cmp			r2, #float_round_nearest_even
	ldrd        r0,[r3]									// d0 = st(top) value = in
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2iz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit32s temp= static_cast<Bit32s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2iz
	//------
	// Return r1 = Bit32s value
	//------
5:	mov			r1, r0
#endif
	pop				{r0, r2, r9, r12}							// Pop flags, store address, r12
	//------
	//	mem_writew(addr,static_cast<Bit16s>(FROUND(fpu.regs[TOP].d)));
	//------
	strb			r1, [r2]
	lsr				r1, #8
	strb			r1, [r2, #1]
	b				restore_flags_from_r0


fistp_t0_bp_i16:
	mem_handler_bp
fistp_t0_i16:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	//	FPU_FST_I16(addr);
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	//	mem_writew(addr,static_cast<Bit16s>(FROUND(fpu.regs[TOP].d)));
	//------
1:	mrs				r0,cpsr								// Save flags to r0
	ldr				r3, =GP_fpu_regs
	push			{r0, r2, r9, r12}					// Save flags, store address, r12
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       	{d8}
	ldrb			r1, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add				r3, r0, lsl #3						// r3 = st(top) address
	vldr        	d0,[r3]								// d0 = st(top) value = in
	cmp				r1, #float_round_nearest_even
	bne				1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    	d8,d0								// d8 = in
	bl          	floor
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_05
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#0.5
#endif
	vmov.f64    	d2,d0								// d2 = floor(in)
	vsub.f64    	d0,d8,d0
	vcmp.f64   		d0,d1
	vmrs        	apsr_nzcv,fpscr
	beq         	4f
	blt				3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_1
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#1.0
#endif
	vadd.f64    	d0,d2,d1							// d0 = floor(in) + 1
	b           	2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    	d0,d2								// d0 = floor(in)
	b           	2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vcvt.s32.f64 	s0,d2
	vmov        	r1,s0								// r1 = returned integer value
	tst         	r1,#1
	addne			r1,#1								// r1 = floor(in) + 1
	b           	5f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp				r1, #float_round_down
	bne				1f
	bl          	floor
	b				2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp				r1, #float_round_up
	bne				2f
	bl          	ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
2:	vcvt.s32.f64 	s0,d0
	vmov        	r1,s0									// r1 = returned integer value
5:	vpop			{d8}
#else
	ldrb		r2, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r4 = st(top) address
	cmp			r2, #float_round_nearest_even
	ldrd        r0,[r3]									// d0 = st(top) value = in
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2iz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit32s temp= static_cast<Bit32s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2iz
	//------
	// Return r1 = Bit32s value
	//------
5:	mov			r1, r0
#endif
	pop				{r0, r2, r9, r12}						// Pop flags, store address, r12
	//------
	//	mem_writew(addr,static_cast<Bit16s>(FROUND(fpu.regs[TOP].d)));
	//------
	strb			r1, [r2]
	lsr				r1, #8
	strb			r1, [r2, #1]
	msr				cpsr_f,r0
	//------
	//	FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif


fild_64_t0_bp_i16:
	mem_handler_bp
fild_64_t0_i16:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
1:	mrs				r0,cpsr								// Save flags to r0
	push			{r0, r9, r12}						// Save flags and r12
	//------
	//	FPU_PREP_PUSH();
	//------
	FPU_PREP_PUSH_r0r1r3
	//------
	//	FPU_FLD_I64(addr,TOP);
	//		FPU_Reg blah;
	//		blah.l.lower = mem_readd(addr);
	//		blah.l.upper = mem_readd(addr+4);
	//		fpu.regs[store_to].d = static_cast<Real64>(blah.ll);
	//------
#if USE_UNALIGNED
	ldr				r0, [r2]						// r0 = aSig low word
	ldr				r1, [r2, #4]					// r1 = aSig high word
#else
	ldrb			r0, [r2]
	ldrb			r1, [r2, #1]
	ldrb			r3, [r2, #2]
	ldrb			lr, [r2, #3]
	orr				r0, r1, lsl #8
	orr				r0, r3, lsl #16
	orr				r0, lr, lsl #24
	ldrb			r1, [r2, #4]
	ldrb			r3, [r2, #5]
	ldrb			lr, [r2, #6]
	orr				r1, r3, lsl #8
	ldrb			r3, [r2, #7]
	orr				r1, lr, lsl #16
	orr				r1, r3, lsl #24
#endif
	bl				__aeabi_l2d
	ldr				r3, =GP_fpu_regs
	ldrb			r2, [r3, #(GP_fpu_top - GP_fpu_regs)]
	add				r2, r3, r2, lsl #3					// r2 = st(top) address
	strd			r0, [r2]
	pop				{r0, r9, r12}						// Restore flags and r12
	b				restore_flags_from_r0


fistp_64_t0_bp_i16:
	mem_handler_bp
fistp_64_t0_i16:
	mem_handler_jump_r0r3 1f, bad_EGA_opcode, bad_EGA_opcode
	//------
	// FPU_FST_I64(addr)
	//	static double FROUND(double in){
	//		switch(fpu.round){
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//		default:
	//			return in;
	//			break;
	//		}
	//	}
	//  FPU_Reg blah;
	//  blah.ll = static_cast<Bit64s>(FROUND(fpu.regs[TOP].d));
	//  mem_writed(addr,blah.l.lower);
	//  mem_writed(addr+4,blah.l.upper);
	//------
1:	mrs				r0,cpsr								// Save flags to r0
	ldr				r3, =GP_fpu_regs
	push			{r0, r2, r9, r12}					// Save flags, store address, r12
	ldrb			r0, [r3, #(GP_fpu_top - GP_fpu_regs)]
#if defined(__ARM_PCS_VFP)
	vpush       	{d8}
	ldrb			r1, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add				r3, r0, lsl #3						// r3 = st(top) address
	vldr        	d0,[r3]								// d0 = st(top) value = in
	cmp				r1, #float_round_nearest_even
	bne				1f
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	vmov.f64    	d8,d0								// d8 = in
	bl          	floor
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_05
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#0.5
#endif
	vmov.f64    	d2,d0								// d2 = floor(in)
	vsub.f64    	d0,d8,d0
	vcmp.f64   		d0,d1
	vmrs        	apsr_nzcv,fpscr
	beq         	4f
	blt				3f
	//------
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//------
#if defined(RPi) || defined(Roku)
	ldr				r0, =fpu_lit_1
	vldr			d1, [r0]
#else
	vmov.f64    	d1,#1.0
#endif
	vadd.f64    	d0,d2,d1							// d0 = floor(in) + 1
	b           	2f
	//------
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//------
3:	vmov.f64    	d0,d2								// d0 = floor(in)
	b           	2f
	//------
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//------
4:	vmov    		r0, r1, d2
	bl      		__aeabi_d2lz							// r0,r1 = long long ressaved
	tst         	r0,#1
	beq				5f
	adds			r0, #1									// r0,r1 = floor(in) + 1
	adcs			r1, #0
	b           	5f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp				r1, #float_round_down
	bne				1f
	bl          	floor
	b				2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp				r1, #float_round_up
	bne				2f
	bl          	ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
2:	vmov    		r0, r1, d0
	bl      		__aeabi_d2lz						// r0,r1 = long long ressaved
5:	vpop			{d8}
#else
	ldrb		r2, [r3, #(GP_float_rounding_mode - GP_fpu_regs)]
	add			r3, r0, lsl #3							// r4 = st(top) address
	cmp			r2, #float_round_nearest_even
	ldrd        r0,[r3]									// d0 = st(top) value = in
	bne			1f
	push		{r4, r5, r6, r7}
	//------
	//		case ROUND_Nearest:	
	//		#if (in-floor(in)>0.5) return (floor(in)+1);
	//		#else if (in-floor(in)<0.5) return (floor(in));
	//		#else return (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//			break;
	//------
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in
	bl      	floor
	mov			r2, r0
	mov			r3, r1									// r2:r3 = floor(in)
	mov     	r4, r0
	mov     	r5, r1									// r4:r5 = floor(in)
	mov     	r0, r6
	mov     	r1, r7									// r0:r1 = in
	bl      	__aeabi_dsub
	mov     	r3, #0x3fc00000
	mov     	r2, #0
	add     	r3, r3, #0x200000						// r2:r3 = 0.5
	mov     	r6, r0
	mov     	r7, r1									// r6:r7 = in - floor(in)
	bl      	__aeabi_dcmpgt
	cmp     	r0, #0
	bne     	3f										// Jump if (in-floor(in)>0.5)
	mov     	r3, #0x3fc00000
	mov     	r0, r6
	mov     	r1, r7
	mov     	r2, #0
	add     	r3, r3, #0x200000
	bl      	__aeabi_dcmplt
	cmp     	r0, #0
	mov     	r0, r4
	mov     	r1, r5									// r0:r1 = floor(in)
	bne			4f										// Jump if (in-floor(in)<0.5)
	//----------
	//	else 
	//		return r0:r1 = (((static_cast<Bit64s>(floor(in)))&1)!=0)?(floor(in)+1):(floor(in));
	//----------
	bl      	__aeabi_d2iz
	tst     	r0, #1
	popeq		{r4, r5, r6, r7}
	beq			5f										// Jump if (((static_cast<Bit64s>(floor(in)))&1)==0)
	//----------
	// 	if (in-floor(in)>0.5)
	//		return r0:r1 = floor(in)+1
	//----------
3:	mov     	r3, #0x3fc00000
	mov     	r0, r4
	mov     	r1, r5
	mov     	r2, #0
	add     	r3, r3, #0x300000
	bl      	__aeabi_dadd
4:	pop			{r4, r5, r6, r7}
	b			2f
	//------
	//		case ROUND_Down:
	//			return (floor(in));
	//			break;
	//------
1:	cmp			r2, #float_round_down
	bne			1f
	bl          floor
	b			2f
	//------
	//		case ROUND_Up:
	//			return (ceil(in));
	//			break;
	//------
1:	cmp			r2, #float_round_up
	bne			2f
	bl          ceil
	//------
	//		case ROUND_Chop:
	//			return in// //the cast afterwards will do it right maybe cast here
	//			break;
	//------
	// Bit64s temp= static_cast<Bit64s>(FROUND(fpuregs[TOP]d));
	//------
2:	bl      	__aeabi_d2lz
	//------
	// Return r1 = Bit32s value
	//------
5:
#endif
	pop				{r2, r3, r9, r12}						// Pop flags, store address, r12
	//------
	//	mem_writed(addr,static_cast<Bit64s>(FROUND(fpu.regs[TOP].d)));
	//------
#if USE_UNALIGNED
	str				r0, [r3]						// r0 = aSig low word
	str				r1, [r3, #4]					// r1 = aSig high word
#else
	strb			r0, [r3]
	lsr				r0, #8
	strb			r0, [r3, #1]
	lsr				r0, #8
	strb			r0, [r3, #2]
	lsr				r0, #8
	strb			r0, [r3, #3]
	strb			r1, [r3, #4]
	lsr				r1, #8
	strb			r1, [r3, #5]
	lsr				r1, #8
	strb			r1, [r3, #6]
	lsr				r1, #8
	strb			r1, [r3, #7]
#endif
	msr				cpsr_f,r2								// Restore flags
	//------
	//	FPU_FPOP();
	//------
	FPU_FPOP_r0r1r2
#if LOG_FPU
	b				loop_log
#else
	b				loop
#endif
	
fnstsw_ax:
	//------
	// FPU_SET_TOP(TOP);
	//------
	FPU_SET_TOP_r0r1r3				// Leaves current fpu_sw into r0 register
	//------
	// reg_ax = fpu.sw;
	//------
#if defined(RPi) || defined(Roku)
	lsr			eax, #16
	orr			eax, r0, eax, lsl #16
#else
	bfi			eax, r0, #0, #16
#endif
#if LOG_FPU
loop_log:
	mrs			r1, cpsr
	ldr			r0, [sp, #SP_EX_CSIP]
	push		{r1, r9, r12}
	sub			r0, #1
	bl			log_fpu_c
	pop			{r0, r9, r12}
	b			restore_flags_from_r0
#else
	b				loop
#endif

	
	.data
	.align 3

fpu_lit_0:
	.word		0, 0
#if defined(RPi) || defined(Roku)
fpu_lit_05:
	.word		0, 0x3FE00000
fpu_lit_1:
	.word		0, 0x3FF00000
fpu_lit_2:
	.word		0, 0x40000000
#endif
fpu_log_2:
	.word		0xfefa39ef, 0x3fe62e42
fpu_fprem_test:
	.word		0, 0x43E00000		// 9.22337e+018

	.global	GP_fpu_regs
GP_fpu_regs:											// 9*2*4 bytes!
	.word		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
GP_fpu_temp:
	.word		0, 0
GP_fpu_cw:		
	.hword		0
GP_fpu_cw_mask_all:
	.hword		0
	.global	GP_fpu_sw
GP_fpu_sw:
	.hword		0
	.global	GP_fpu_top
GP_fpu_top:
	.hword		0
GP_fpu_tags:
	.byte		0, 0, 0, 0, 0, 0, 0, 0, 0 				// 9 bytes!
GP_float_rounding_mode:
	.byte		0
	
