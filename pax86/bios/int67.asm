
		org 	0xA40	; INT67 (EMS) device header
		
		CPU		186

		JMP 	0F000h:0067h
		
		resb	5

		db		'EMMXXXX0',0

		align	4
		