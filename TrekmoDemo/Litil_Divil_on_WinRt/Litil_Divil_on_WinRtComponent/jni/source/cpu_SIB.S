//=============================================================================
// cpu_SIB.s
//
// This file contains the SIB table generator macros and SIB byte effective
// address calculation routines needed in 32-bit address decoding.
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
	.file	"cpu_SIB.s"

#include "defines.inc"
#include "macros.inc"

	.text
	.align	2

.macro sib_item base idx scale disp
	.word	sib_\base\()_\idx\()_\scale\()_\disp
.endm
.macro sib_row reg d 
	sib_item eax \reg, \d, ""
	sib_item ecx \reg, \d, ""
	sib_item edx \reg, \d, ""
	sib_item ebx \reg, \d, ""
	sib_item esp \reg, \d, ""
	sib_item disp32 \reg, \d, ""
	sib_item esi \reg, \d, ""
	sib_item edi \reg, \d, ""
.endm
.macro sib_block d
	sib_row eax, \d
	sib_row ecx, \d
	sib_row edx, \d
	sib_row ebx, \d
	sib_row "_", \d
	sib_row ebp, \d
	sib_row esi, \d
	sib_row edi, \d
.endm

.macro handle_item base idx scale
sib_\base\()_\idx\()_\scale\()_:
	.ifc \base, ebp
		mem_handler_bp
	.endif
	.ifc \base, esp
		mem_handler_bp
	.endif
	.ifc \scale, 0
		//------
		// SIB, no scale
		//------
		.ifc \base, disp32	
			//------
			// This is disp32 with no base if MOD = 00
			//------
			mov		lr, r1
			r0_from_disp32
			.ifc \idx, _
			.else	
				add		r0, \idx
			.endif	
			bx		lr
		.else
			.ifc \idx, _
				mov		r0, \base
			.else	
				add		r0, \base, \idx
			.endif	
		.endif	
	.else
		//------
		// SIB, scale
		//------
		.ifc \base, disp32	
			//------
			// This is disp32 with no base if MOD = 00
			//------
			mov		lr, r1
			r0_from_disp32
			.ifc \idx, _
			.else	
				add		r0, \idx, lsl #\scale
			.endif	
			bx		lr			// Jump to the original r1 address
		.else
			.ifc \idx, _
				mov		r0, \base
			.else	
				add		r0, \base, \idx, lsl #\scale
			.endif	
		.endif	
	.endif
	bx		r1
.endm
.macro handle_row reg d
	handle_item eax \reg \d
	handle_item ecx \reg \d
	handle_item edx \reg \d
	handle_item ebx \reg \d
	handle_item esp \reg \d
	handle_item "disp32" \reg \d
	handle_item esi \reg \d
	handle_item edi \reg \d
.endm
.macro handle_block d
	handle_row eax \d
	handle_row ecx \d
	handle_row edx \d
	handle_row ebx \d
	handle_row "_" \d
	handle_row ebp \d
	handle_row esi \d
	handle_row edi \d
.endm

	.global	sib_table
sib_table:
	sib_block 0
	sib_block 1
	sib_block 2
	sib_block 3

	handle_block 0
	handle_block 1
	handle_block 2
	handle_block 3

jump_back:
	b		.unknown

.macro sib_row_d reg d x
	sib_item eax \reg, \d, \x
	sib_item ecx \reg, \d, \x
	sib_item edx \reg, \d, \x
	sib_item ebx \reg, \d, \x
	sib_item esp \reg, \d, \x
	sib_item ebp \reg, \d, \x
	sib_item esi \reg, \d, \x
	sib_item edi \reg, \d, \x
.endm
.macro sib_block_d d x
	sib_row_d eax, \d, \x
	sib_row_d ecx, \d, \x
	sib_row_d edx, \d, \x
	sib_row_d ebx, \d, \x
	sib_row_d "_", \d, \x
	sib_row_d ebp, \d, \x
	sib_row_d esi, \d, \x
	sib_row_d edi, \d, \x
.endm

.macro handle_item_d8 base idx scale
sib_\base\()_\idx\()_\scale\()_d8:
	.ifc \base, ebp
		mem_handler_bp
	.endif
	.ifc \base, esp
		mem_handler_bp
	.endif
	.ifc \scale, 0
		//------
		// SIB, no scale
		//------
		.ifc \idx, _
			r0_from_idx_disp8 \base
		.else	
			r0_from_idx_disp8 \base
			add		r0, \idx
		.endif	
	.else
		//------
		// SIB, scale
		//------
		.ifc \idx, _
			r0_from_idx_disp8 \base
		.else	
			r0_from_idx_disp8 \base
			add		r0, \idx, lsl #\scale
		.endif	
	.endif
	bx		r1
.endm
.macro handle_row_d8 reg d
	handle_item_d8 eax \reg \d
	handle_item_d8 ecx \reg \d
	handle_item_d8 edx \reg \d
	handle_item_d8 ebx \reg \d
	handle_item_d8 esp \reg \d
	handle_item_d8 ebp \reg \d
	handle_item_d8 esi \reg \d
	handle_item_d8 edi \reg \d
.endm
.macro handle_block_d8 d
	handle_row_d8 eax \d
	handle_row_d8 ecx \d
	handle_row_d8 edx \d
	handle_row_d8 ebx \d
	handle_row_d8 "_" \d
	handle_row_d8 ebp \d
	handle_row_d8 esi \d
	handle_row_d8 edi \d
.endm

	.global	sib_disp8_table
sib_disp8_table:
	sib_block_d 0, d8
	sib_block_d 1, d8
	sib_block_d 2, d8
	sib_block_d 3, d8

	handle_block_d8 0
	handle_block_d8 1
	handle_block_d8 2
	handle_block_d8 3

.macro handle_item_d32 base idx scale
sib_\base\()_\idx\()_\scale\()_d32:
	.ifc \base, ebp
		mem_handler_bp
	.endif
	.ifc \base, esp
		mem_handler_bp
	.endif
	mov		lr, r1
	.ifc \scale, 0
		//------
		// SIB, no scale
		//------
		.ifc \idx, _
			r0_from_idx_disp32 \base
		.else	
			r0_from_idx_disp32 \base
			add		r0, \idx
		.endif	
	.else
		//------
		// SIB, scale
		//------
		.ifc \idx, _
			r0_from_idx_disp32 \base
		.else	
			r0_from_idx_disp32 \base
			add		r0, \idx, lsl #\scale
		.endif	
	.endif
	bx		lr			// Jump to original r1 value
.endm
.macro handle_row_d32 reg d
	handle_item_d32 eax \reg \d
	handle_item_d32 ecx \reg \d
	handle_item_d32 edx \reg \d
	handle_item_d32 ebx \reg \d
	handle_item_d32 esp \reg \d
	handle_item_d32 ebp \reg \d
	handle_item_d32 esi \reg \d
	handle_item_d32 edi \reg \d
.endm
.macro handle_block_d32 d
	handle_row_d32 eax \d
	handle_row_d32 ecx \d
	handle_row_d32 edx \d
	handle_row_d32 ebx \d
	handle_row_d32 "_" \d
	handle_row_d32 ebp \d
	handle_row_d32 esi \d
	handle_row_d32 edi \d
.endm


	.global	sib_disp32_table
sib_disp32_table:
	sib_block_d 0, d32
	sib_block_d 1, d32
	sib_block_d 2, d32
	sib_block_d 3, d32

	handle_block_d32 0
	handle_block_d32 1
	handle_block_d32 2
	handle_block_d32 3

