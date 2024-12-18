
StartBlock0:
	; Starting new memory block at 0
.p4510   ; 65816 processor
;.i16    ; X/Y are 16 bits
;.a8     ; A is 8 bits
;.segment "CODE"
EndBlock0:
StartBlock2001:
	; Starting new memory block at $2001
.p4510   ; 65816 processor
	 .org $2001
	 .byte $09,$20 ;End of command marker (first byte after the 00 terminator)
	 .byte $0a,$00 ;10
	 .byte $fe,$02,$30,$00 ;BANK 0
	 .byte $13, $20 
	 .byte $14,$00 ;20
	 .byte $9e ;SYS
	 .byte $38,$32,$32,$34
	  .byte $00
endd_s:
	  .byte $00,$00    ;End of basic terminators
	  .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF    ;extra
	; Ending memory block at $2001
	; Resuming memory block at $2001
demo:
	; LineNumber: 246
	jmp block1
	; LineNumber: 3
graph_s	= $02
	; LineNumber: 5
graph_LogicalRowSize:	.word	0
	; LineNumber: 7
graph_RowSize:	.word	0
	; LineNumber: 4
fileio_s	=  $04
	; LineNumber: 5
joystick_Joy1Button:	.byte	$00
	; LineNumber: 6
joystick_Joy1Up:	.byte	$00
	; LineNumber: 7
joystick_Joy1Down:	.byte	$00
	; LineNumber: 8
joystick_Joy1Left:	.byte	$00
	; LineNumber: 9
joystick_Joy1Right:	.byte	$00
	; LineNumber: 11
joystick_Joy2Button:	.byte	$00
	; LineNumber: 12
joystick_Joy2Up:	.byte	$00
	; LineNumber: 13
joystick_Joy2Down:	.byte	$00
	; LineNumber: 14
joystick_Joy2Left:	.byte	$00
	; LineNumber: 15
joystick_Joy2Right:	.byte	$00
	; LineNumber: 23
str_p1	=  $08
	; LineNumber: 23
str_p2	=  $16
	; LineNumber: 23
str_p3	=  $0B
	; LineNumber: 24
str_i:	.byte	0
	; LineNumber: 24
str_j:	.byte	0
	; LineNumber: 25
str_li:	.byte	0
	; LineNumber: 26
str_b:	.byte	0
	; LineNumber: 26
str_c:	.byte	0
	; LineNumber: 27
str_num:	.word	0
	; LineNumber: 28
str_chars:		.asciiz	"0123456789ABCDEF"

	; LineNumber: 10
mapx:	.word	0
	; LineNumber: 10
mapy:	.word	0
	; LineNumber: 13
PALFILENAME:		.asciiz	"AURORA.BIN"

	; LineNumber: 15
TILESFILENAME:		.asciiz	"TILES.BIN"

	; LineNumber: 17
UIFILENAME:		.asciiz	"UI.BIN"

	; LineNumber: 19
MAPFILENAME:		.asciiz	"MAP.BIN"

	; LineNumber: 21
s_dex:	
	.byte	$04, $05, $18, 0
	; LineNumber: 22
s_str:	
	.byte	$13, $14, $12, 0
	; LineNumber: 23
s_int:	
	.byte	$09, $0e, $14, 0
	; LineNumber: 24
s_mov:	
	.byte	$0d, $0f, $16, 0
	; LineNumber: 26
i_dex:	.word	$00
	; LineNumber: 27
i_str:	.word	$3e7
	; LineNumber: 28
i_int:	.word	$00
	; LineNumber: 29
i_mov:	.word	$3e7
	; LineNumber: 31
varPrefixed_y:	.byte	0
	; LineNumber: 32
addr:	.word	0
	; LineNumber: 33
maddr:	.word	0
	; LineNumber: 35
lestring:		.asciiz	"      "

	; LineNumber: 41
playerX:	.word	$32
	; LineNumber: 42
playerY:	.byte	$32
	; LineNumber: 44
RRB_playerTilePosition:	.res	50,0
	; LineNumber: 45
RRB_playerXPosition:	.res	50,0
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4c     ;$59 used for hi-byte
initdiv16x8_dividend = $4e	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4e ;save memory by reusing divident to store the result
divide16x8:
	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16:	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16:
	dex
	bne divloop16
	rts
end_procedure_init16x8div:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4c
mul16x8_num1 = $4e
mul16x8_num2 = $50
mul16x8_procedure:
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd:
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop:
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop:
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
end_procedure_init16x8mul:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4c
div8x8_d = $4e
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure:
	lda #$00
	ldx #$07
	clc
div8x8_loop1:
	rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2:
	dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end:
	rts
end_procedure_init8x8div:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4c
multiplier_a = $4e
multiply_eightbit:
	cpx #$00
	beq mul_end
	dex
	stx $4e
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop:
	bcc mul_skip
mul_mod:
	adc multiplier_a
mul_skip:
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end:
	txa
	rts
initeightbitmul_multiply_eightbit2:
	rts
end_procedure_initeightbitmul:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initjoystick
	;    Procedure type : Built-in function
	;    Requires initialization : no
joystickup: .byte 0
joystickdown: .byte 0
joystickleft: .byte 0
joystickright: .byte 0
joystickbutton: .byte 0
callJoystick:
	lda #0
	sta joystickup
	sta joystickdown
	sta joystickleft
	sta joystickright
	sta joystickbutton
	lda #%00000001 ; mask joystick up mment
	bit $50      ; bitwise AND with address 56320
	bne joystick_down       ; zero flag is not set -> skip to down
	lda #1
	sta joystickup
joystick_down:
	lda #%00000010 ; mask joystick down movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_left       ; zero flag is not set -> skip to down
	lda #1
	sta joystickdown
joystick_left:
	lda #%00000100 ; mask joystick left movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_right       ; zero flag is not set -> skip to down
	lda #1
	sta joystickleft
joystick_right:
	lda #%00001000 ; mask joystick up movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_button       ; zero flag is not set -> skip to down
	lda #1
	sta joystickright
joystick_button:
	lda #%00010000 ; mask joystick up movement
	bit $50      ; bitwise AND with address 56320
	bne callJoystick_end       ; zero flag is not set -> skip to down
	lda #1
	sta joystickbutton
callJoystick_end:
	rts
	rts
end_procedure_initjoystick:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
colormemory =  $fb
screen_x = $4c
screen_y = $4e
SetScreenPosition:
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop:
	clc
	adc #80
	bcc sskip
	inc screenmemory+1
sskip:
	dey
	bne syloop
sydone:
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone:
	sta screenmemory
	rts
initmoveto_moveto3:
	rts
end_procedure_initmoveto:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_Enable40Mhz
	;    Procedure type : User-defined procedure
	; LineNumber: 11
graph_Enable40Mhz:
	; LineNumber: 12
			lda #$41
			sta $00
		
	; LineNumber: 16
	rts
end_procedure_graph_Enable40Mhz:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_EnableVic4
	;    Procedure type : User-defined procedure
	; LineNumber: 18
graph_EnableVic4:
	; LineNumber: 20
			lda #$00
			tax
			tay
			taz
			map
			eom
			
			lda #$47
			sta $d02f
			lda #$53
			sta $d02f		
		
	; LineNumber: 33
	rts
end_procedure_graph_EnableVic4:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_DisableC65Rom
	;    Procedure type : User-defined procedure
	; LineNumber: 36
graph_DisableC65Rom:
	; LineNumber: 37
			lda #$70
			sta $d640
			eom		
		
	; LineNumber: 42
	rts
end_procedure_graph_DisableC65Rom:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_EnableFastRRB
	;    Procedure type : User-defined procedure
	; LineNumber: 45
graph_EnableFastRRB:
	; LineNumber: 46
			lda #$08
			tsb $d031
			lda #$40
			tsb $d051
			lda #$00
			sta $d05b
		
	; LineNumber: 54
	rts
end_procedure_graph_EnableFastRRB:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_Initialize
	;    Procedure type : User-defined procedure
	; LineNumber: 59
graph_Initialize:
	; LineNumber: 60
			sei
			lda #$35
			sta $01
			; Disable CIA Interrupts
			lda #$7f
			sta $dc0d
			sta $dd0d
					
		
	; LineNumber: 70
	jsr graph_Enable40Mhz
	; LineNumber: 71
	jsr graph_EnableVic4
	; LineNumber: 72
	jsr graph_DisableC65Rom
	; LineNumber: 73
			; Disable IRQ raster interrupts
			; because C65 uses raster interrupts in the ROM
			lda #$00
			sta $d01a
			
			; Unmap C64 Roms $d030 by clearing bits 3-7
			lda #%11111000
			trb $d030
			cli
		
			; Disable Hot register to VIC2 register
			lda #$80
			trb $d05d
			
			; Disable Vic3 ATTR register to enable 8bit color
			lda #$20
			trb $d031
			
			; Disable H640
			lda #%10000000
			trb $d031
			
			; Disable V400
			lda #%00001000
			trb $d031
			
			; Enable RAM palettes
			lda #$04
			tsb $d030
			
			; Turm on FCM mode and 16bit per char num
			lda #$05
			sta $d054
			
			; border and back color
			lda #0
			sta $d020
			sta $d021
			
			; exclude Kernel+Basic rom
			lda    $00
        		ora    #$07
        		sta    $00
        
			;lda #$1D
			;sta $01
		
			; enable fine raster compare
			lda #%10000111
            	trb $d07a
			lda #%10000000
			trb $d053
			lda #$68
            	sta $d079
		
	; LineNumber: 129
	rts
end_procedure_graph_Initialize:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_Poke32
	;    Procedure type : User-defined procedure
	; LineNumber: 132
	; LineNumber: 131
localVariable_graph_Poke32_graph_mb:	.byte	0
	; LineNumber: 131
localVariable_graph_Poke32_graph_b:	.byte	0
	; LineNumber: 131
localVariable_graph_Poke32_graph_ad:	.word	0
	; LineNumber: 131
localVariable_graph_Poke32_graph_val:	.byte	0
graph_Poke32_block9:
graph_Poke32:
	; LineNumber: 133
			lda localVariable_graph_Poke32_graph_mb
			sta $43
			lda localVariable_graph_Poke32_graph_b
			sta $42
			lda localVariable_graph_Poke32_graph_ad+1
			sta $41
			lda	localVariable_graph_Poke32_graph_ad
			sta $40
			lda localVariable_graph_Poke32_graph_val
			ldz #0
			nop
			sta ($40),z
		
	; LineNumber: 148
	rts
end_procedure_graph_Poke32:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_WPoke32
	;    Procedure type : User-defined procedure
	; LineNumber: 151
	; LineNumber: 150
localVariable_graph_WPoke32_graph_mb:	.byte	0
	; LineNumber: 150
localVariable_graph_WPoke32_graph_b:	.byte	0
	; LineNumber: 150
localVariable_graph_WPoke32_graph_ad:	.word	0
	; LineNumber: 150
localVariable_graph_WPoke32_graph_val:	.word	0
graph_WPoke32_block10:
graph_WPoke32:
	; LineNumber: 152
			lda localVariable_graph_WPoke32_graph_mb
			sta $43
			lda localVariable_graph_WPoke32_graph_b
			sta $42
			lda localVariable_graph_WPoke32_graph_ad+1
			sta $41
			lda	localVariable_graph_WPoke32_graph_ad
			sta $40
			
			lda localVariable_graph_WPoke32_graph_val
			ldz #0
			nop
			sta ($40),z
			
			lda localVariable_graph_WPoke32_graph_val+1
			ldz #1
			nop
			sta ($40),z
		
	; LineNumber: 173
	rts
end_procedure_graph_WPoke32:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_SetScreenLocation
	;    Procedure type : User-defined procedure
	; LineNumber: 186
	; LineNumber: 185
localVariable_graph_SetScreenLocation_graph_b:	.byte	0
	; LineNumber: 185
localVariable_graph_SetScreenLocation_graph_addr:	.word	0
graph_SetScreenLocation_block11:
graph_SetScreenLocation:
	; LineNumber: 187
			lda localVariable_graph_SetScreenLocation_graph_b
			sta $d062
			lda localVariable_graph_SetScreenLocation_graph_addr
			sta $d061
			lda localVariable_graph_SetScreenLocation_graph_addr+1
			sta $d060		
		
	; LineNumber: 195
	rts
end_procedure_graph_SetScreenLocation:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_SetScreenWidth
	;    Procedure type : User-defined procedure
	; LineNumber: 198
	; LineNumber: 197
localVariable_graph_SetScreenWidth_graph_newRowSize:	.word	0
graph_SetScreenWidth_block12:
graph_SetScreenWidth:
	; LineNumber: 199
	ldy localVariable_graph_SetScreenWidth_graph_newRowSize+1 ;keep
	lda localVariable_graph_SetScreenWidth_graph_newRowSize
	; Calling storevariable on generic assign expression
	sta graph_RowSize
	sty graph_RowSize+1
	; LineNumber: 200
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : graph_RowSize
	ldy graph_RowSize+1
	lda graph_RowSize
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta graph_LogicalRowSize
	sty graph_LogicalRowSize+1
	; LineNumber: 202
		lda graph_RowSize
		sta $D05E
		lda graph_LogicalRowSize
		sta $D058
		lda graph_LogicalRowSize+1
		sta $D059
		lda #25
		sta $d07b
	
	
	; LineNumber: 213
	rts
end_procedure_graph_SetScreenWidth:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_vsync
	;    Procedure type : User-defined procedure
	; LineNumber: 217
	; LineNumber: 216
localVariable_graph_vsync_graph_rline:	.word	0
graph_vsync_block13:
graph_vsync:
	; LineNumber: 218
	@vsync_wait1:
		lda $d053
		and #$07
		cmp localVariable_graph_vsync_graph_rline+1
		bne @vsync_wait1
		lda localVariable_graph_vsync_graph_rline
		cmp $d052
		bne @vsync_wait1
	@vsync_wait2:
		cmp $d052
		beq @vsync_wait2
	
	; LineNumber: 231
	rts
end_procedure_graph_vsync:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_writeChar
	;    Procedure type : User-defined procedure
	; LineNumber: 235
	; LineNumber: 234
localVariable_graph_writeChar_graph_x:	.word	0
	; LineNumber: 234
localVariable_graph_writeChar_graph_y:	.word	0
	; LineNumber: 234
localVariable_graph_writeChar_graph_char:	.byte	0
	; LineNumber: 234
localVariable_graph_writeChar_graph_col:	.byte	0
graph_writeChar_block14:
graph_writeChar:
	; LineNumber: 236
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_b
	; Generic 16 bit op
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_graph_writeChar_graph_y
	ldy localVariable_graph_writeChar_graph_y+1
	lda localVariable_graph_writeChar_graph_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
graph_writeChar_rightvarInteger_var17 = $54
	sta graph_writeChar_rightvarInteger_var17
	sty graph_writeChar_rightvarInteger_var17+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_graph_writeChar_graph_x
	ldy localVariable_graph_writeChar_graph_x+1
	lda localVariable_graph_writeChar_graph_x
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc graph_writeChar_rightvarInteger_var17
graph_writeChar_wordAdd15:
	sta graph_writeChar_rightvarInteger_var17
	; High-bit binop
	tya
	adc graph_writeChar_rightvarInteger_var17+1
	tay
	lda graph_writeChar_rightvarInteger_var17
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_ad
	sty localVariable_graph_Poke32_graph_ad+1
	lda localVariable_graph_writeChar_graph_char
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_val
	jsr graph_Poke32
	; LineNumber: 237
	lda #$f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_mb
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_b
	; Generic 16 bit op
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_graph_writeChar_graph_y
	ldy localVariable_graph_writeChar_graph_y+1
	lda localVariable_graph_writeChar_graph_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
graph_writeChar_rightvarInteger_var20 = $54
	sta graph_writeChar_rightvarInteger_var20
	sty graph_writeChar_rightvarInteger_var20+1
	; Generic 16 bit op
	ldy #0
	lda #$1
graph_writeChar_rightvarInteger_var23 =  $56
	sta graph_writeChar_rightvarInteger_var23
	sty graph_writeChar_rightvarInteger_var23+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_graph_writeChar_graph_x
	ldy localVariable_graph_writeChar_graph_x+1
	lda localVariable_graph_writeChar_graph_x
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc graph_writeChar_rightvarInteger_var23
graph_writeChar_wordAdd21:
	sta graph_writeChar_rightvarInteger_var23
	; High-bit binop
	tya
	adc graph_writeChar_rightvarInteger_var23+1
	tay
	lda graph_writeChar_rightvarInteger_var23
	; Low bit binop:
	clc
	adc graph_writeChar_rightvarInteger_var20
graph_writeChar_wordAdd18:
	sta graph_writeChar_rightvarInteger_var20
	; High-bit binop
	tya
	adc graph_writeChar_rightvarInteger_var20+1
	tay
	lda graph_writeChar_rightvarInteger_var20
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_ad
	sty localVariable_graph_Poke32_graph_ad+1
	lda localVariable_graph_writeChar_graph_col
	; Calling storevariable on generic assign expression
	sta localVariable_graph_Poke32_graph_val
	jsr graph_Poke32
	; LineNumber: 238
	rts
end_procedure_graph_writeChar:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : graph_writeFCMString
	;    Procedure type : User-defined procedure
	; LineNumber: 248
	; LineNumber: 247
localVariable_graph_writeFCMString_graph_i:	.byte	0
	; LineNumber: 246
localVariable_graph_writeFCMString_graph_x:	.word	0
	; LineNumber: 246
localVariable_graph_writeFCMString_graph_y:	.word	0
	; LineNumber: 246
localVariable_graph_writeFCMString_graph_s	=  $0D
	; LineNumber: 246
localVariable_graph_writeFCMString_graph_col:	.byte	0
graph_writeFCMString_block24:
graph_writeFCMString:
	; LineNumber: 249
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_i
	; LineNumber: 250
graph_writeFCMString_while25:
graph_writeFCMString_loopstart29:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy localVariable_graph_writeFCMString_graph_i
	lda (localVariable_graph_writeFCMString_graph_s),y
	; cmp #$00 ignored
	beq graph_writeFCMString_edblock28
graph_writeFCMString_ctb26: ;Main true block ;keep:
	; LineNumber: 251
	; LineNumber: 252
	; INTEGER optimization: a=b+c 
	lda localVariable_graph_writeFCMString_graph_x
	clc
	adc localVariable_graph_writeFCMString_graph_i
	sta localVariable_graph_writeChar_graph_x+0
	lda localVariable_graph_writeFCMString_graph_x+1
	adc #0
	sta localVariable_graph_writeChar_graph_x+1
	ldy localVariable_graph_writeFCMString_graph_y+1 ;keep
	lda localVariable_graph_writeFCMString_graph_y
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeChar_graph_y
	sty localVariable_graph_writeChar_graph_y+1
	; Load pointer array
	ldy localVariable_graph_writeFCMString_graph_i
	lda (localVariable_graph_writeFCMString_graph_s),y
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeChar_graph_char
	lda localVariable_graph_writeFCMString_graph_col
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeChar_graph_col
	jsr graph_writeChar
	; LineNumber: 253
	; Test Inc dec D
	inc localVariable_graph_writeFCMString_graph_i
	; LineNumber: 254
	jmp graph_writeFCMString_while25
graph_writeFCMString_edblock28:
graph_writeFCMString_loopend30:
	; LineNumber: 255
	rts
end_procedure_graph_writeFCMString:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DMA_dmaFill
	;    Procedure type : User-defined procedure
	; LineNumber: 8
	; LineNumber: 7
localVariable_DMA_dmaFill_DMA_dstmb:	.byte	0
	; LineNumber: 7
localVariable_DMA_dmaFill_DMA_dstbank:	.byte	0
	; LineNumber: 7
localVariable_DMA_dmaFill_DMA_dst:	.word	0
	; LineNumber: 7
localVariable_DMA_dmaFill_DMA_countx:	.word	0
	; LineNumber: 7
localVariable_DMA_dmaFill_DMA_value:	.word	0
DMA_dmaFill_block35:
DMA_dmaFill:
	; LineNumber: 9
		lda localVariable_DMA_dmaFill_DMA_countx
		ldy localVariable_DMA_dmaFill_DMA_countx+1
		sta @dmaFillCount
		sty @dmaFillCount+1
		
		lda localVariable_DMA_dmaFill_DMA_value
		ldy localVariable_DMA_dmaFill_DMA_value+1
		sta @dmaFillValue
		sty @dmaFillValue+1
		
		lda localVariable_DMA_dmaFill_DMA_dst
		ldy localVariable_DMA_dmaFill_DMA_dst+1
		sta @dmaFillDst
		sty @dmaFillDst+1
	
		lda localVariable_DMA_dmaFill_DMA_dstbank
		sta @dmaFillDstBank
		
		lda localVariable_DMA_dmaFill_DMA_dstmb
		sta @dmaDstMB
		
		
		sta $d707												; execute DMA
		.byte $0B												; F011B DMA format
		.byte $81
		@dmaDstMB:
		.byte $00												; set MB of destination
		.byte $00												; end of job options
		.byte $03												; fill
		@dmaFillCount: 		.word $0028							; count
		@dmaFillValue: 		.word $00							; value
		.byte $00												; src bank
		@dmaFillDst: 		.word $f800							; dst
		@dmaFillDstBank: 	.byte $01							; dst bank
		.byte $00												; cmd hi
		.word $00												; modulo / ignored
	
	; LineNumber: 48
	rts
end_procedure_DMA_dmaFill:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DMA_dmaCopy
	;    Procedure type : User-defined procedure
	; LineNumber: 52
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_srcmb:	.byte	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_srcbank:	.byte	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_src:	.word	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_dstmb:	.byte	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_dstbank:	.byte	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_dst:	.word	0
	; LineNumber: 51
localVariable_DMA_dmaCopy_DMA_countx:	.word	0
DMA_dmaCopy_block36:
DMA_dmaCopy:
	; LineNumber: 53
		lda localVariable_DMA_dmaCopy_DMA_countx
		ldy localVariable_DMA_dmaCopy_DMA_countx+1
		sta @dmaCopyCount
		sty @dmaCopyCount+1
				
		lda localVariable_DMA_dmaCopy_DMA_dst
		ldy localVariable_DMA_dmaCopy_DMA_dst+1
		sta @dmaFillDst
		sty @dmaFillDst+1
	
		lda localVariable_DMA_dmaCopy_DMA_dstbank
		sta @dmaFillDstBank
		
		lda localVariable_DMA_dmaCopy_DMA_dstmb
		sta @dmaDstMB
		
		lda localVariable_DMA_dmaCopy_DMA_src
		ldy localVariable_DMA_dmaCopy_DMA_src+1
		sta @dmaSrc
		sty @dmaSrc+1
		
		lda localVariable_DMA_dmaCopy_DMA_srcbank
		sta @dmaSrcBank
		lda localVariable_DMA_dmaCopy_DMA_srcmb
		sta @dmaSrcMB
		
		sta $d707												; execute DMA
		.byte $0B												; F011B DMA format
		.byte $81
		@dmaDstMB:			.byte $00							; set MB of destination
		.byte $80
		@dmaSrcMB:			.byte $00
		.byte $00												; end of job options
		.byte $00												; copy
		@dmaCopyCount: 		.word $0028							; count
		@dmaSrc: 			.word $00							; source
		@dmaSrcBank:     	.byte $00							; src bank
		@dmaFillDst: 		.word $f800							; dst
		@dmaFillDstBank: 	.byte $01							; dst bank
		.byte $00												; cmd hi
		.word $00												; modulo / ignored
	
	; LineNumber: 98
	rts
end_procedure_DMA_dmaCopy:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fileio_LoadFileHigh
	;    Procedure type : User-defined procedure
	; LineNumber: 7
	; LineNumber: 6
localVariable_fileio_LoadFileHigh_fileio_b:	.word	0
	; LineNumber: 6
localVariable_fileio_LoadFileHigh_fileio_addr:	.word	0
	; LineNumber: 6
localVariable_fileio_LoadFileHigh_fileio_l:	.byte	0
fileio_LoadFileHigh_block37:
fileio_LoadFileHigh:
	; LineNumber: 8
		sei		
	
		lda localVariable_fileio_LoadFileHigh_fileio_b+1	; for load address
		ora #$80
		ldy localVariable_fileio_LoadFileHigh_fileio_b
		ldx #$00	; bank for filename
		jsr $ff6b
		lda #$00
		ldx #$08
		ldy #$00
		jsr $FFBA
					
		lda localVariable_fileio_LoadFileHigh_fileio_l
		ldx fileio_s
		ldy fileio_s+1
		jsr $FFBD
	
		lda #$00
		ldx localVariable_fileio_LoadFileHigh_fileio_addr
		ldy localVariable_fileio_LoadFileHigh_fileio_addr+1
		jsr $FFD5
		bcs @derror
		jmp @goexit
	@derror:
		inc $d020
		jmp @derror
		
	@goexit:
		cli
	
	; LineNumber: 42
	rts
end_procedure_fileio_LoadFileHigh:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : fileio_LoadFile
	;    Procedure type : User-defined procedure
	; LineNumber: 45
	; LineNumber: 44
localVariable_fileio_LoadFile_fileio_b:	.byte	0
	; LineNumber: 44
localVariable_fileio_LoadFile_fileio_addr:	.word	0
	; LineNumber: 44
localVariable_fileio_LoadFile_fileio_l:	.byte	0
fileio_LoadFile_block38:
fileio_LoadFile:
	; LineNumber: 46
		sei
	
	
		lda localVariable_fileio_LoadFile_fileio_b	; for load address
		ldx #$00
		jsr $ff6b
		
		lda #$00
		ldx #$08
		ldy #$00
		jsr $FFBA
		
		lda localVariable_fileio_LoadFile_fileio_l
		ldx fileio_s
		ldy fileio_s+1
		jsr $FFBD
	
		lda #$00
		ldx localVariable_fileio_LoadFile_fileio_addr
		ldy localVariable_fileio_LoadFile_fileio_addr+1
		jsr $FFD5
		bcs @derror
		jmp @goexit
	@derror:
		inc $d020
		jmp @derror
		
	@goexit:
		cli
	
	; LineNumber: 80
	rts
end_procedure_fileio_LoadFile:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : joystick_GetJoyStick
	;    Procedure type : User-defined procedure
	; LineNumber: 19
joystick_GetJoyStick:
	; LineNumber: 20
	lda #$0
	; Calling storevariable on generic assign expression
	sta joystick_Joy2Button
	; LineNumber: 21
	; Calling storevariable on generic assign expression
	sta joystick_Joy2Up
	; LineNumber: 22
	; Calling storevariable on generic assign expression
	sta joystick_Joy2Down
	; LineNumber: 23
	; Calling storevariable on generic assign expression
	sta joystick_Joy2Left
	; LineNumber: 24
	; Calling storevariable on generic assign expression
	sta joystick_Joy2Right
	; LineNumber: 26
	; Calling storevariable on generic assign expression
	sta joystick_Joy1Button
	; LineNumber: 27
	; Calling storevariable on generic assign expression
	sta joystick_Joy1Up
	; LineNumber: 28
	; Calling storevariable on generic assign expression
	sta joystick_Joy1Down
	; LineNumber: 29
	; Calling storevariable on generic assign expression
	sta joystick_Joy1Left
	; LineNumber: 30
	; Calling storevariable on generic assign expression
	sta joystick_Joy1Right
	; LineNumber: 33
	
	@j2btn:
		lda $DC00
		and #$10
		bne @j2up
		lda #$01
		sta joystick_Joy2Button
		jmp @exit
	@j2up:
		lda $DC00
		and #$01
		bne @j2down
		lda #$01
		sta joystick_Joy2Up
		jmp @exit
	@j2down:
		lda $DC00
		and #$02
		bne @j2left
		lda #$01
		sta joystick_Joy2Down
		jmp @exit
	@j2left:
		lda $DC00
		and #$04
		bne @j2right
		lda #$01
		sta joystick_Joy2Left
		jmp @exit
	@j2right:
		lda $DC00
		and #$08
		bne @j1btn
		lda #$01
		sta joystick_Joy2Right
		jmp @exit
	@j1btn:
		lda $DC01
		and #$10
		bne @j1up
		lda #$01
		sta joystick_Joy1Button
		jmp @exit
	@j1up:
		lda $DC01
		and #$01
		bne @j1down
		lda #$01
		sta joystick_Joy1Up
		jmp @exit
	@j1down:
		lda $DC01
		and #$02
		bne @j1left
		lda #$01
		sta joystick_Joy1Down
		jmp @exit
	@j1left:
		lda $DC01
		and #$04
		bne @j1right
		lda #$01
		sta joystick_Joy1Left
		jmp @exit
	@j1right:
		lda $DC01
		and #$08
		bne @exit
		lda #$01
		sta joystick_Joy1Right
	@exit:
	; LineNumber: 106
	rts
end_procedure_joystick_GetJoyStick:
	;*
; //
; //	Returns the length of a string. Note that
; //	this will only work for strings <256 bytes. 
; //
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_strlen
	;    Procedure type : User-defined procedure
	; LineNumber: 41
str_strlen_block40:
str_strlen:
	; LineNumber: 42
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_li
	; LineNumber: 43
str_strlen_while41:
str_strlen_loopstart45:
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy str_li
	lda (str_p3),y
	; cmp #$00 ignored
	beq str_strlen_edblock44
str_strlen_ctb42: ;Main true block ;keep:
	; LineNumber: 43
	; Test Inc dec D
	inc str_li
	jmp str_strlen_while41
str_strlen_edblock44:
str_strlen_loopend46:
	; LineNumber: 46
	; LineNumber: 47
	lda str_li
	rts
end_procedure_str_strlen:
	;*
; // Reverses a string
; // 
; // *
	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_reverse
	;    Procedure type : User-defined procedure
	; LineNumber: 55
str_reverse_block49:
str_reverse:
	; LineNumber: 56
	lda str_p2
	ldx str_p2+1
	sta str_p3
	stx str_p3+1
	jsr str_strlen
	; Calling storevariable on generic assign expression
	sta str_c
	; LineNumber: 57
	; 8 bit binop
	; Add/sub where right value is constant number
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta str_j
	; LineNumber: 58
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_i
	; LineNumber: 59
str_reverse_while50:
str_reverse_loopstart54:
	; Binary clause Simplified: LESS
	lda str_i
	; Compare with pure num / var optimization
	cmp str_j;keep
	bcs str_reverse_edblock53
str_reverse_ctb51: ;Main true block ;keep:
	; LineNumber: 60
	; LineNumber: 61
	; Load pointer array
	ldy str_i
	lda (str_p2),y
	; Calling storevariable on generic assign expression
	sta str_b
	; LineNumber: 62
	; Load pointer array
	ldy str_j
	lda (str_p2),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p2),y
	; LineNumber: 63
	lda str_b
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_j ; optimized, look out for bugs
	sta (str_p2),y
	; LineNumber: 64
	; Test Inc dec D
	dec str_j
	; LineNumber: 65
	; Test Inc dec D
	inc str_i
	; LineNumber: 66
	jmp str_reverse_while50
str_reverse_edblock53:
str_reverse_loopend55:
	; LineNumber: 67
	rts
end_procedure_str_reverse:
	;*
; //	Converts a number to a string in base b
; //	example:
; //	<code>
; //itoa(1234, p1, 16); 
; // coverts "1234" to a hexadecimal string stored in p1
; //	</code>
; //
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_itoa
	;    Procedure type : User-defined procedure
	; LineNumber: 80
str_itoa_block58:
str_itoa:
	; LineNumber: 81
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_i
	; LineNumber: 82
	; Binary clause INTEGER: EQUALS
	lda str_num+1   ; compare high bytes
	cmp #$00 ;keep
	bne str_itoa_edblock62
	lda str_num
	cmp #$00 ;keep
	bne str_itoa_edblock62
	jmp str_itoa_ctb60
str_itoa_ctb60: ;Main true block ;keep:
	; LineNumber: 83
	; LineNumber: 84
	lda #$30
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (str_p1),y
	; LineNumber: 85
	
; // Simply 0
	lda #$0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$1
	sta (str_p1),y
	; LineNumber: 86
	rts
	; LineNumber: 87
str_itoa_edblock62:
	; LineNumber: 88
str_itoa_while65:
str_itoa_loopstart69:
	; Binary clause INTEGER: NOTEQUALS
	lda str_num+1   ; compare high bytes
	cmp #$00 ;keep
	beq str_itoa_pass174
	jmp str_itoa_ctb66
str_itoa_pass174:
	lda str_num
	cmp #$00 ;keep
	beq str_itoa_edblock68
	jmp str_itoa_ctb66
str_itoa_ctb66: ;Main true block ;keep:
	; LineNumber: 89
	; LineNumber: 90
	; Load Byte array
	; CAST type NADA
	; Modulo
	lda str_b
str_itoa_val_var76 = $54
	sta str_itoa_val_var76
	ldy str_num+1 ;keep
	lda str_num
	sec
str_itoa_modulo77:
	sbc str_itoa_val_var76
	bcs str_itoa_modulo77
	adc str_itoa_val_var76
	tax
	lda str_chars,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p1),y
	; LineNumber: 91
	; Test Inc dec D
	inc str_i
	; LineNumber: 92
	; 16x8 div
	ldy str_num+1 ;keep
	lda str_num
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	lda str_b
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	ldy initdiv16x8_dividend+1
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	; LineNumber: 93
	jmp str_itoa_while65
str_itoa_edblock68:
str_itoa_loopend70:
	; LineNumber: 94
	lda #$0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p1),y
	; LineNumber: 95
	
; // null-term string    
	lda str_p1
	ldx str_p1+1
	sta str_p2
	stx str_p2+1
	jsr str_reverse
	; LineNumber: 97
	rts
end_procedure_str_itoa:
	
; // 100 Tiles 
	; NodeProcedureDecl -1
	; ***********  Defining procedure : loadData
	;    Procedure type : User-defined procedure
	; LineNumber: 49
loadData:
	; LineNumber: 53
	
; // use file loading before eliminating the kernel rom!!!!!
; // loading palette data
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_b
	; Integer constant assigning
	; Load16bitvariable : #$4000
	ldy #$40
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_addr
	sty localVariable_fileio_LoadFile_fileio_addr+1
	lda #$a
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_l
	lda #<PALFILENAME
	ldx #>PALFILENAME
	sta fileio_s
	stx fileio_s+1
	jsr fileio_LoadFile
	; LineNumber: 56
		ldx #$ff
	@cploop:
		lda $4000,x
		sta $d100,x
		lda $4100,x
		sta $d200,x
		lda $4200,x
		sta $d300,x
		dex
		bne @cploop
	
	; LineNumber: 70
	
; // copy palette data
; // load tile data
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_b
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_addr
	sty localVariable_fileio_LoadFile_fileio_addr+1
	lda #$9
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFile_fileio_l
	lda #<TILESFILENAME
	ldx #>TILESFILENAME
	sta fileio_s
	stx fileio_s+1
	jsr fileio_LoadFile
	; LineNumber: 72
	
; // load ui overlay
	; Integer constant assigning
	; Load16bitvariable : #$800
	ldy #$08
	lda #$00
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_b
	sty localVariable_fileio_LoadFileHigh_fileio_b+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_addr
	sty localVariable_fileio_LoadFileHigh_fileio_addr+1
	lda #$6
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_l
	lda #<UIFILENAME
	ldx #>UIFILENAME
	sta fileio_s
	stx fileio_s+1
	jsr fileio_LoadFileHigh
	; LineNumber: 74
	
; // load map data
	; Integer constant assigning
	; Load16bitvariable : #$801
	ldy #$08
	lda #$01
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_b
	sty localVariable_fileio_LoadFileHigh_fileio_b+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_addr
	sty localVariable_fileio_LoadFileHigh_fileio_addr+1
	lda #$7
	; Calling storevariable on generic assign expression
	sta localVariable_fileio_LoadFileHigh_fileio_l
	lda #<MAPFILENAME
	ldx #>MAPFILENAME
	sta fileio_s
	stx fileio_s+1
	jsr fileio_LoadFileHigh
	; LineNumber: 75
	rts
end_procedure_loadData:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : DrawMap
	;    Procedure type : User-defined procedure
	; LineNumber: 83
	; LineNumber: 79
localVariable_DrawMap_i:	.byte	0
	; LineNumber: 80
localVariable_DrawMap_varPrefixed_x:	.byte	0
	; LineNumber: 80
localVariable_DrawMap_varPrefixed_y:	.byte	0
	; LineNumber: 81
localVariable_DrawMap_addr:	.word	0
	; LineNumber: 82
localVariable_DrawMap_maddr:	.word	0
DrawMap_block79:
DrawMap:
	; LineNumber: 91
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_DrawMap_varPrefixed_y
DrawMap_forloop80:
	; LineNumber: 86
	; LineNumber: 87
	
; // draw the cut of the map into the map window
	; Generic 16 bit op
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy mapy+1 ;keep
	lda mapy
	clc
	adc localVariable_DrawMap_varPrefixed_y
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcc DrawMap_skip96
	iny
DrawMap_skip96:
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$c8
	sta mul16x8_num2
	jsr mul16x8_procedure
DrawMap_rightvarInteger_var97 = $54
	sta DrawMap_rightvarInteger_var97
	sty DrawMap_rightvarInteger_var97+1
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : mapx
	ldy mapx+1
	lda mapx
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc DrawMap_rightvarInteger_var97
DrawMap_wordAdd93:
	sta DrawMap_rightvarInteger_var97
	; High-bit binop
	tya
	adc DrawMap_rightvarInteger_var97+1
	tay
	lda DrawMap_rightvarInteger_var97
	; Calling storevariable on generic assign expression
	sta localVariable_DrawMap_maddr
	sty localVariable_DrawMap_maddr+1
	; LineNumber: 88
	; Generic 16 bit op
	ldy #0
	lda #$2
DrawMap_rightvarInteger_var100 = $54
	sta DrawMap_rightvarInteger_var100
	sty DrawMap_rightvarInteger_var100+1
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_DrawMap_varPrefixed_y
	ldy #0
	lda localVariable_DrawMap_varPrefixed_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc DrawMap_rightvarInteger_var100
DrawMap_wordAdd98:
	sta DrawMap_rightvarInteger_var100
	; High-bit binop
	tya
	adc DrawMap_rightvarInteger_var100+1
	tay
	lda DrawMap_rightvarInteger_var100
	; Calling storevariable on generic assign expression
	sta localVariable_DrawMap_addr
	sty localVariable_DrawMap_addr+1
	; LineNumber: 89
	lda #$80
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_srcmb
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_srcbank
	ldy localVariable_DrawMap_maddr+1 ;keep
	lda localVariable_DrawMap_maddr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_src
	sty localVariable_DMA_dmaCopy_DMA_src+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dstmb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dstbank
	ldy localVariable_DrawMap_addr+1 ;keep
	lda localVariable_DrawMap_addr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dst
	sty localVariable_DMA_dmaCopy_DMA_dst+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$38
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_countx
	sty localVariable_DMA_dmaCopy_DMA_countx+1
	jsr DMA_dmaCopy
	; LineNumber: 90
DrawMap_loopstart81:
	; Test Inc dec D
	inc localVariable_DrawMap_varPrefixed_y
	lda #$18
	cmp localVariable_DrawMap_varPrefixed_y ;keep
	beq DrawMap_loopdone101
DrawMap_loopnotdone102:
	jmp DrawMap_forloop80
DrawMap_loopdone101:
DrawMap_loopend82:
	; LineNumber: 91
	rts
end_procedure_DrawMap:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : UpdateUi
	;    Procedure type : User-defined procedure
	; LineNumber: 94
UpdateUi:
	; LineNumber: 97
	
; // update the values and print into stats section	
	lda i_dex
	clc
	adc #$01
	sta i_dex+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdateUi_WordAdd104
	inc i_dex+1
UpdateUi_WordAdd104:
	; LineNumber: 98
	; Binary clause INTEGER: GREATER
	lda i_dex+1   ; compare high bytes
	cmp #$03 ;keep
	bcc UpdateUi_edblock108
	bne UpdateUi_ctb106
	lda i_dex
	cmp #$84 ;keep
	bcc UpdateUi_edblock108
	beq UpdateUi_edblock108
UpdateUi_ctb106: ;Main true block ;keep:
	; LineNumber: 97
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta i_dex
	sty i_dex+1
UpdateUi_edblock108:
	; LineNumber: 100
	lda i_str
	sec
	sbc #$01
	sta i_str+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdateUi_WordAdd111
	dec i_str+1
UpdateUi_WordAdd111:
	; LineNumber: 101
	; Binary clause INTEGER: LESSEQUAL
	lda i_str+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdateUi_ctb113
	bne UpdateUi_edblock115
	lda i_str
	cmp #$01 ;keep
	beq UpdateUi_ctb113
	bcs UpdateUi_edblock115
UpdateUi_ctb113: ;Main true block ;keep:
	; LineNumber: 100
	; Integer constant assigning
	; Load16bitvariable : #$3e7
	ldy #$03
	lda #$e7
	; Calling storevariable on generic assign expression
	sta i_str
	sty i_str+1
UpdateUi_edblock115:
	; LineNumber: 103
	lda i_int
	clc
	adc #$01
	sta i_int+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdateUi_WordAdd118
	inc i_int+1
UpdateUi_WordAdd118:
	; LineNumber: 104
	; Binary clause INTEGER: GREATER
	lda i_int+1   ; compare high bytes
	cmp #$03 ;keep
	bcc UpdateUi_edblock122
	bne UpdateUi_ctb120
	lda i_int
	cmp #$84 ;keep
	bcc UpdateUi_edblock122
	beq UpdateUi_edblock122
UpdateUi_ctb120: ;Main true block ;keep:
	; LineNumber: 103
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta i_int
	sty i_int+1
UpdateUi_edblock122:
	; LineNumber: 106
	lda i_mov
	sec
	sbc #$01
	sta i_mov+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdateUi_WordAdd125
	dec i_mov+1
UpdateUi_WordAdd125:
	; LineNumber: 107
	; Binary clause INTEGER: LESSEQUAL
	lda i_mov+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdateUi_ctb127
	bne UpdateUi_edblock129
	lda i_mov
	cmp #$01 ;keep
	beq UpdateUi_ctb127
	bcs UpdateUi_edblock129
UpdateUi_ctb127: ;Main true block ;keep:
	; LineNumber: 106
	; Integer constant assigning
	; Load16bitvariable : #$3e7
	ldy #$03
	lda #$e7
	; Calling storevariable on generic assign expression
	sta i_mov
	sty i_mov+1
UpdateUi_edblock129:
	; LineNumber: 109
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<s_dex
	ldx #>s_dex
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 110
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<s_str
	ldx #>s_str
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 111
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<s_int
	ldx #>s_int
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 112
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<s_mov
	ldx #>s_mov
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 116
	ldy i_dex+1 ;keep
	lda i_dex
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	lda #<lestring
	ldx #>lestring
	sta str_p1
	stx str_p1+1
	lda #$a
	; Calling storevariable on generic assign expression
	sta str_b
	jsr str_itoa
	; LineNumber: 117
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$23
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<lestring
	ldx #>lestring
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 118
	ldy i_str+1 ;keep
	lda i_str
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	lda #<lestring
	ldx #>lestring
	sta str_p1
	stx str_p1+1
	lda #$a
	; Calling storevariable on generic assign expression
	sta str_b
	jsr str_itoa
	; LineNumber: 119
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$23
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<lestring
	ldx #>lestring
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 120
	ldy i_int+1 ;keep
	lda i_int
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	lda #<lestring
	ldx #>lestring
	sta str_p1
	stx str_p1+1
	lda #$a
	; Calling storevariable on generic assign expression
	sta str_b
	jsr str_itoa
	; LineNumber: 121
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$23
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$4
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<lestring
	ldx #>lestring
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$3
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 122
	ldy i_mov+1 ;keep
	lda i_mov
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	lda #<lestring
	ldx #>lestring
	sta str_p1
	stx str_p1+1
	lda #$a
	; Calling storevariable on generic assign expression
	sta str_b
	jsr str_itoa
	; LineNumber: 123
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$23
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_x
	sty localVariable_graph_writeFCMString_graph_x+1
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_y
	sty localVariable_graph_writeFCMString_graph_y+1
	lda #<lestring
	ldx #>lestring
	sta localVariable_graph_writeFCMString_graph_s
	stx localVariable_graph_writeFCMString_graph_s+1
	lda #$2
	; Calling storevariable on generic assign expression
	sta localVariable_graph_writeFCMString_graph_col
	jsr graph_writeFCMString
	; LineNumber: 124
	rts
end_procedure_UpdateUi:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitRRB
	;    Procedure type : User-defined procedure
	; LineNumber: 129
	; LineNumber: 128
localVariable_InitRRB_i:	.byte	0
	; LineNumber: 129
localVariable_InitRRB_varPrefixed_a:	.word	0
InitRRB_block132:
InitRRB:
	; LineNumber: 154
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_InitRRB_i
InitRRB_forloop133:
	; LineNumber: 132
	; LineNumber: 136
	
; // calculate position in attrib/screen ram 
; // 40 chars visible on screen
	; Generic 16 bit op
	ldy #0
	lda #$50
InitRRB_rightvarInteger_var146 = $54
	sta InitRRB_rightvarInteger_var146
	sty InitRRB_rightvarInteger_var146+1
	; Mul 16x8 setup
	; Load16bitvariable : localVariable_InitRRB_i
	ldy #0
	lda localVariable_InitRRB_i
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc InitRRB_rightvarInteger_var146
InitRRB_wordAdd144:
	sta InitRRB_rightvarInteger_var146
	; High-bit binop
	tya
	adc InitRRB_rightvarInteger_var146+1
	tay
	lda InitRRB_rightvarInteger_var146
	; Calling storevariable on generic assign expression
	sta localVariable_InitRRB_varPrefixed_a
	sty localVariable_InitRRB_varPrefixed_a+1
	; LineNumber: 138
	lda #$f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$90
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 139
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 140
	
; // Player X Position
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	pha
	lda localVariable_InitRRB_i
	asl
	tax
	pla
	sta RRB_playerXPosition,x
	tya
	sta RRB_playerXPosition+1,x
	; LineNumber: 141
	
; // save address
	lda localVariable_InitRRB_varPrefixed_a
	clc
	adc #$02
	sta localVariable_InitRRB_varPrefixed_a+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc InitRRB_WordAdd147
	inc localVariable_InitRRB_varPrefixed_a+1
InitRRB_WordAdd147:
	; LineNumber: 143
	lda #$f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 144
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; Integer constant assigning
	; Load16bitvariable : #$100c
	ldy #$10
	lda #$0c
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 145
	
; // Player Tile
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	pha
	lda localVariable_InitRRB_i
	asl
	tax
	pla
	sta RRB_playerTilePosition,x
	tya
	sta RRB_playerTilePosition+1,x
	; LineNumber: 146
	
; // save address
	lda localVariable_InitRRB_varPrefixed_a
	clc
	adc #$02
	sta localVariable_InitRRB_varPrefixed_a+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc InitRRB_WordAdd148
	inc localVariable_InitRRB_varPrefixed_a+1
InitRRB_WordAdd148:
	; LineNumber: 148
	lda #$f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$10
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 149
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; Integer constant assigning
	; Load16bitvariable : #$140
	ldy #$01
	lda #$40
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 150
	
; // goto end of screen
	lda localVariable_InitRRB_varPrefixed_a
	clc
	adc #$02
	sta localVariable_InitRRB_varPrefixed_a+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc InitRRB_WordAdd149
	inc localVariable_InitRRB_varPrefixed_a+1
InitRRB_WordAdd149:
	; LineNumber: 152
	lda #$f
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$f8
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 153
	
; // additional char to end row
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	ldy localVariable_InitRRB_varPrefixed_a+1 ;keep
	lda localVariable_InitRRB_varPrefixed_a
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 154
InitRRB_loopstart134:
	; Test Inc dec D
	inc localVariable_InitRRB_i
	lda #$19
	cmp localVariable_InitRRB_i ;keep
	beq InitRRB_loopdone150
InitRRB_loopnotdone151:
	jmp InitRRB_forloop133
InitRRB_loopdone150:
InitRRB_loopend135:
	; LineNumber: 155
	rts
end_procedure_InitRRB:
	; NodeProcedureDecl -1
	; ***********  Defining procedure : UpdatePlayer
	;    Procedure type : User-defined procedure
	; LineNumber: 161
	; LineNumber: 158
localVariable_UpdatePlayer_i:	.byte	0
	; LineNumber: 159
localVariable_UpdatePlayer_ly:	.byte	0
	; LineNumber: 160
localVariable_UpdatePlayer_dy:	.word	0
UpdatePlayer_block152:
UpdatePlayer:
	; LineNumber: 163
	
; // Input Player
	jsr joystick_GetJoyStick
	; LineNumber: 165
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda joystick_Joy2Up
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdatePlayer_edblock156
UpdatePlayer_localsuccess182: ;keep:
	; ; logical AND, second requirement
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda playerY
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdatePlayer_edblock156
UpdatePlayer_ctb154: ;Main true block ;keep:
	; LineNumber: 166
	; LineNumber: 167
	; Binary clause Simplified: GREATEREQUAL
	lda playerY
	; Compare with pure num / var optimization
	cmp #$15;keep
	bcc UpdatePlayer_eblock186
UpdatePlayer_ctb185: ;Main true block ;keep:
	; LineNumber: 167
	; Test Inc dec D
	dec playerY
	jmp UpdatePlayer_edblock187
UpdatePlayer_eblock186:
	; LineNumber: 169
	; Binary clause INTEGER: GREATER
	lda mapy+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock203
	bne UpdatePlayer_ctb201
	lda mapy
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock203
	beq UpdatePlayer_edblock203
UpdatePlayer_ctb201: ;Main true block ;keep:
	; LineNumber: 171
	; LineNumber: 172
	; Optimizer: a = a +/- b
	; Load16bitvariable : playerY
	lda playerY
	clc
	adc #$20
	sta playerY
	; LineNumber: 173
	lda mapy
	sec
	sbc #$04
	sta mapy+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdatePlayer_WordAdd207
	dec mapy+1
UpdatePlayer_WordAdd207:
	; LineNumber: 174
UpdatePlayer_edblock203:
UpdatePlayer_edblock187:
	; LineNumber: 176
UpdatePlayer_edblock156:
	; LineNumber: 177
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda joystick_Joy2Down
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdatePlayer_edblock211
UpdatePlayer_localsuccess237: ;keep:
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda playerY
	; Compare with pure num / var optimization
	cmp #$c8;keep
	bcs UpdatePlayer_edblock211
UpdatePlayer_ctb209: ;Main true block ;keep:
	; LineNumber: 178
	; LineNumber: 179
	; Binary clause Simplified: LESS
	lda playerY
	; Compare with pure num / var optimization
	cmp #$b4;keep
	bcs UpdatePlayer_eblock241
UpdatePlayer_ctb240: ;Main true block ;keep:
	; LineNumber: 179
	; Test Inc dec D
	inc playerY
	jmp UpdatePlayer_edblock242
UpdatePlayer_eblock241:
	; LineNumber: 181
	; Binary clause INTEGER: LESS
	lda mapy+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_ctb256
	bne UpdatePlayer_edblock258
	lda mapy
	cmp #$4c ;keep
	bcs UpdatePlayer_edblock258
UpdatePlayer_ctb256: ;Main true block ;keep:
	; LineNumber: 183
	; LineNumber: 184
	; Optimizer: a = a +/- b
	; Load16bitvariable : playerY
	lda playerY
	sec
	sbc #$20
	sta playerY
	; LineNumber: 185
	lda mapy
	clc
	adc #$04
	sta mapy+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdatePlayer_WordAdd262
	inc mapy+1
UpdatePlayer_WordAdd262:
	; LineNumber: 186
UpdatePlayer_edblock258:
UpdatePlayer_edblock242:
	; LineNumber: 187
UpdatePlayer_edblock211:
	; LineNumber: 188
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda joystick_Joy2Left
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdatePlayer_edblock266
UpdatePlayer_localsuccess298: ;keep:
	; ; logical AND, second requirement
	; Binary clause INTEGER: GREATER
	lda playerX+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock266
	bne UpdatePlayer_ctb264
	lda playerX
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock266
	beq UpdatePlayer_edblock266
UpdatePlayer_ctb264: ;Main true block ;keep:
	; LineNumber: 189
	; LineNumber: 190
	; Binary clause INTEGER: GREATER
	lda playerX+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_eblock302
	bne UpdatePlayer_ctb301
	lda playerX
	cmp #$14 ;keep
	bcc UpdatePlayer_eblock302
	beq UpdatePlayer_eblock302
UpdatePlayer_ctb301: ;Main true block ;keep:
	; LineNumber: 190
	lda playerX
	sec
	sbc #$01
	sta playerX+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdatePlayer_WordAdd318
	dec playerX+1
UpdatePlayer_WordAdd318:
	jmp UpdatePlayer_edblock303
UpdatePlayer_eblock302:
	; LineNumber: 192
	; Binary clause INTEGER: GREATER
	lda mapx+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock323
	bne UpdatePlayer_ctb321
	lda mapx
	cmp #$00 ;keep
	bcc UpdatePlayer_edblock323
	beq UpdatePlayer_edblock323
UpdatePlayer_ctb321: ;Main true block ;keep:
	; LineNumber: 194
	; LineNumber: 195
	lda playerX
	clc
	adc #$20
	sta playerX+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdatePlayer_WordAdd328
	inc playerX+1
UpdatePlayer_WordAdd328:
	; LineNumber: 196
	lda mapx
	sec
	sbc #$04
	sta mapx+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdatePlayer_WordAdd329
	dec mapx+1
UpdatePlayer_WordAdd329:
	; LineNumber: 197
UpdatePlayer_edblock323:
UpdatePlayer_edblock303:
	; LineNumber: 198
UpdatePlayer_edblock266:
	; LineNumber: 199
	; Optimization: replacing a > N with a >= N+1
	; Binary clause Simplified: GREATEREQUAL
	lda joystick_Joy2Right
	; Compare with pure num / var optimization
	cmp #$1;keep
	bcc UpdatePlayer_edblock333
UpdatePlayer_localsuccess365: ;keep:
	; ; logical AND, second requirement
	; Binary clause INTEGER: LESS
	lda playerX+1   ; compare high bytes
	cmp #$01 ;keep
	bcc UpdatePlayer_ctb331
	bne UpdatePlayer_edblock333
	lda playerX
	cmp #$40 ;keep
	bcs UpdatePlayer_edblock333
UpdatePlayer_ctb331: ;Main true block ;keep:
	; LineNumber: 200
	; LineNumber: 201
	; Binary clause INTEGER: LESS
	lda playerX+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_ctb368
	bne UpdatePlayer_eblock369
	lda playerX
	cmp #$dc ;keep
	bcs UpdatePlayer_eblock369
UpdatePlayer_ctb368: ;Main true block ;keep:
	; LineNumber: 201
	lda playerX
	clc
	adc #$01
	sta playerX+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdatePlayer_WordAdd385
	inc playerX+1
UpdatePlayer_WordAdd385:
	jmp UpdatePlayer_edblock370
UpdatePlayer_eblock369:
	; LineNumber: 203
	; Binary clause INTEGER: LESS
	lda mapx+1   ; compare high bytes
	cmp #$00 ;keep
	bcc UpdatePlayer_ctb388
	bne UpdatePlayer_edblock390
	lda mapx
	cmp #$4d ;keep
	bcs UpdatePlayer_edblock390
UpdatePlayer_ctb388: ;Main true block ;keep:
	; LineNumber: 205
	; LineNumber: 206
	lda playerX
	sec
	sbc #$20
	sta playerX+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcs UpdatePlayer_WordAdd395
	dec playerX+1
UpdatePlayer_WordAdd395:
	; LineNumber: 207
	lda mapx
	clc
	adc #$04
	sta mapx+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc UpdatePlayer_WordAdd396
	inc mapx+1
UpdatePlayer_WordAdd396:
	; LineNumber: 208
UpdatePlayer_edblock390:
UpdatePlayer_edblock370:
	; LineNumber: 209
UpdatePlayer_edblock333:
	; LineNumber: 212
	; Right is PURE NUMERIC : Is word =0
	; 8 bit mul of power 2
	; Load16bitvariable : playerY
	lda playerY
	lsr
	lsr
	lsr
	; Calling storevariable on generic assign expression
	sta localVariable_UpdatePlayer_ly
	; LineNumber: 215
		lda playerY
		and #$07
		eor #$07
		asl
		asl
		asl
		asl
		asl
	
		sta localVariable_UpdatePlayer_dy	+1		
	
	; LineNumber: 231
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_UpdatePlayer_i
UpdatePlayer_forloop397:
	; LineNumber: 229
	
; // textrow of player
; // y fine value dy = y % 8 shifted to high 3 bits and stored in highyte of word (2 byte per char!)
; // clear all tiles - Screen Memory
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	; Load Integer array
	; CAST type INTEGER
	lda localVariable_UpdatePlayer_i
	asl
	tax
	lda RRB_playerTilePosition,x 
	ldy RRB_playerTilePosition+1,x 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; Integer constant assigning
	; Load16bitvariable : #$100c
	ldy #$10
	lda #$0c
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
UpdatePlayer_loopstart398:
	; Compare is onpage
	; Test Inc dec D
	inc localVariable_UpdatePlayer_i
	lda #$19
	cmp localVariable_UpdatePlayer_i ;keep
	bne UpdatePlayer_forloop397
UpdatePlayer_loopdone402: ;keep:
UpdatePlayer_loopend399:
	; LineNumber: 236
	
; // Set Player X Position -  Screen Memory || row before for fine y
; // Set Player Tile - Screen Memory || row before for fine y
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	; Load Integer array
	; CAST type INTEGER
	; 8 bit binop
	; Add/sub where right value is constant number
	lda localVariable_UpdatePlayer_ly
	sec
	sbc #$1
	 ; end add / sub var with constant
	asl
	tax
	lda RRB_playerTilePosition,x 
	ldy RRB_playerTilePosition+1,x 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; Integer constant assigning
	; Load16bitvariable : #$1020
	ldy #$10
	lda #$20
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 237
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	; Load Integer array
	; CAST type INTEGER
	; 8 bit binop
	; Add/sub where right value is constant number
	lda localVariable_UpdatePlayer_ly
	sec
	sbc #$1
	 ; end add / sub var with constant
	asl
	tax
	lda RRB_playerXPosition,x 
	ldy RRB_playerXPosition+1,x 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy playerX+1 ;keep
	lda playerX
	ora localVariable_UpdatePlayer_dy
	; Testing for byte:  localVariable_UpdatePlayer_dy+1
	; RHS is word, no optimization
	pha 
	tya 
	ora localVariable_UpdatePlayer_dy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 241
	
; // X Position & fine Y Position
; // Set Player X Position -  Screen Memory
; // Set Player Tile - Screen Memory
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	; Load Integer array
	; CAST type INTEGER
	lda localVariable_UpdatePlayer_ly
	asl
	tax
	lda RRB_playerTilePosition,x 
	ldy RRB_playerTilePosition+1,x 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; Integer constant assigning
	; Load16bitvariable : #$1021
	ldy #$10
	lda #$21
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 242
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_mb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_b
	; Load Integer array
	; CAST type INTEGER
	lda localVariable_UpdatePlayer_ly
	asl
	tax
	lda RRB_playerXPosition,x 
	ldy RRB_playerXPosition+1,x 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_ad
	sty localVariable_graph_WPoke32_graph_ad+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	ldy playerX+1 ;keep
	lda playerX
	ora localVariable_UpdatePlayer_dy
	; Testing for byte:  localVariable_UpdatePlayer_dy+1
	; RHS is word, no optimization
	pha 
	tya 
	ora localVariable_UpdatePlayer_dy+1
	tay 
	pla 
	; Calling storevariable on generic assign expression
	sta localVariable_graph_WPoke32_graph_val
	sty localVariable_graph_WPoke32_graph_val+1
	jsr graph_WPoke32
	; LineNumber: 244
	rts
end_procedure_UpdatePlayer:
block1:
main_block_begin_:
	; LineNumber: 247
	
; // X Position & fine Y Position
	jsr loadData
	; LineNumber: 248
	jsr graph_Initialize
	; LineNumber: 249
	jsr graph_EnableFastRRB
	; LineNumber: 251
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_graph_SetScreenLocation_graph_b
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_graph_SetScreenLocation_graph_addr
	sty localVariable_graph_SetScreenLocation_graph_addr+1
	jsr graph_SetScreenLocation
	; LineNumber: 252
	
; // Screen at $50000
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$2d
	; Calling storevariable on generic assign expression
	sta localVariable_graph_SetScreenWidth_graph_newRowSize
	sty localVariable_graph_SetScreenWidth_graph_newRowSize+1
	jsr graph_SetScreenWidth
	; LineNumber: 255
	
; // screen width with rrb in mind -> 1 single bob = 40->45 change with rrb!
; // clear out screen and attrib
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstmb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstbank
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dst
	sty localVariable_DMA_dmaFill_DMA_dst+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$19
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_countx
	sty localVariable_DMA_dmaFill_DMA_countx+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_value
	sty localVariable_DMA_dmaFill_DMA_value+1
	jsr DMA_dmaFill
	; LineNumber: 256
	lda #$ff
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstmb
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstbank
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dst
	sty localVariable_DMA_dmaFill_DMA_dst+1
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num1
	sty mul16x8_num1Hi
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$19
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_countx
	sty localVariable_DMA_dmaFill_DMA_countx+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_value
	sty localVariable_DMA_dmaFill_DMA_value+1
	jsr DMA_dmaFill
	; LineNumber: 265
	lda #$0
	; Calling storevariable on generic assign expression
	sta varPrefixed_y
MainProgram_forloop405:
	; LineNumber: 260
	; LineNumber: 261
	
; // copy ui frame
	; Mul 16x8 setup
	; Load16bitvariable : varPrefixed_y
	ldy #0
	lda varPrefixed_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta addr
	sty addr+1
	; LineNumber: 262
	; Swapping nodes :  num * expr -> exp*num (mul only)
	; Right is PURE NUMERIC : Is word =1
	; 16 bit mul or div
	; Mul 16x8 setup
	; Load16bitvariable : varPrefixed_y
	ldy #0
	lda varPrefixed_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	lda #$50
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Calling storevariable on generic assign expression
	sta maddr
	sty maddr+1
	; LineNumber: 263
	lda #$80
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_srcmb
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_srcbank
	ldy maddr+1 ;keep
	lda maddr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_src
	sty localVariable_DMA_dmaCopy_DMA_src+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dstmb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dstbank
	ldy addr+1 ;keep
	lda addr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_dst
	sty localVariable_DMA_dmaCopy_DMA_dst+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$50
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaCopy_DMA_countx
	sty localVariable_DMA_dmaCopy_DMA_countx+1
	jsr DMA_dmaCopy
	; LineNumber: 264
MainProgram_loopstart406:
	; Compare is onpage
	; Test Inc dec D
	inc varPrefixed_y
	lda #$19
	cmp varPrefixed_y ;keep
	bne MainProgram_forloop405
MainProgram_loopdone410: ;keep:
MainProgram_loopend407:
	; LineNumber: 273
	lda #$1
	; Calling storevariable on generic assign expression
	sta varPrefixed_y
MainProgram_forloop411:
	; LineNumber: 268
	; LineNumber: 269
	
; // zero out stats area
	; Generic 16 bit op
	ldy #0
	lda #60
MainProgram_rightvarInteger_var421 = $54
	sta MainProgram_rightvarInteger_var421
	sty MainProgram_rightvarInteger_var421+1
	; Mul 16x8 setup
	; Load16bitvariable : varPrefixed_y
	ldy #0
	lda varPrefixed_y
	sta mul16x8_num1
	sty mul16x8_num1Hi
	; Load16bitvariable : graph_LogicalRowSize
	ldy graph_LogicalRowSize+1
	lda graph_LogicalRowSize
	sta mul16x8_num2
	jsr mul16x8_procedure
	; Low bit binop:
	clc
	adc MainProgram_rightvarInteger_var421
MainProgram_wordAdd419:
	sta MainProgram_rightvarInteger_var421
	; High-bit binop
	tya
	adc MainProgram_rightvarInteger_var421+1
	tay
	lda MainProgram_rightvarInteger_var421
	; Calling storevariable on generic assign expression
	sta addr
	sty addr+1
	; LineNumber: 270
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstmb
	lda #$5
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstbank
	ldy addr+1 ;keep
	lda addr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dst
	sty localVariable_DMA_dmaFill_DMA_dst+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$12
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_countx
	sty localVariable_DMA_dmaFill_DMA_countx+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_value
	sty localVariable_DMA_dmaFill_DMA_value+1
	jsr DMA_dmaFill
	; LineNumber: 271
	lda #$ff
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstmb
	lda #$8
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dstbank
	ldy addr+1 ;keep
	lda addr
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_dst
	sty localVariable_DMA_dmaFill_DMA_dst+1
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$12
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_countx
	sty localVariable_DMA_dmaFill_DMA_countx+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta localVariable_DMA_dmaFill_DMA_value
	sty localVariable_DMA_dmaFill_DMA_value+1
	jsr DMA_dmaFill
	; LineNumber: 272
MainProgram_loopstart412:
	; Test Inc dec D
	inc varPrefixed_y
	lda #$18
	cmp varPrefixed_y ;keep
	beq MainProgram_loopdone422
MainProgram_loopnotdone423:
	jmp MainProgram_forloop411
MainProgram_loopdone422:
MainProgram_loopend413:
	; LineNumber: 274
	jsr InitRRB
	; LineNumber: 276
MainProgram_while424:
MainProgram_loopstart428:
	; Binary clause Simplified: NOTEQUALS
	clc
	lda #$1
	; cmp #$00 ignored
	beq MainProgram_edblock427
MainProgram_ctb425: ;Main true block ;keep:
	; LineNumber: 277
	; LineNumber: 278
	ldy #0   ; Force integer assignment, set y = 0 for values lower than 255
	lda #$1
	; Calling storevariable on generic assign expression
	sta localVariable_graph_vsync_graph_rline
	sty localVariable_graph_vsync_graph_rline+1
	jsr graph_vsync
	; LineNumber: 279
	; Poke
	; Optimization: both storage and shift are constant
	lda #$5a
	sta $d021
	; LineNumber: 281
	jsr DrawMap
	; LineNumber: 282
	jsr UpdateUi
	; LineNumber: 283
	jsr UpdatePlayer
	; LineNumber: 284
	; Poke
	; Optimization: both storage and shift are constant
	lda #$0
	sta $d021
	; LineNumber: 285
	jmp MainProgram_while424
MainProgram_edblock427:
MainProgram_loopend429:
	; LineNumber: 286
main_block_end_:
	; End of program
	; Ending memory block at $2001
	; Ending memory block at $2001
EndBlock2001:

