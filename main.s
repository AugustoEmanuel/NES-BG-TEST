.include "nes_addresses.s"

.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 0               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
  nmi_lock:       .res 1 ; prevents NMI re-entry
  nmi_count:      .res 1 ; is incremented every NMI
  nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
  nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
  scroll_x:       .res 1 ; x scroll position
  scroll_y:       .res 1 ; y scroll position
  scroll_nmt:     .res 1 ; nametable select (0-3 = PPU_CTRL,$2400,$2800,$2C00)
  temp:           .res 1 ; temporary variable
  buttons:        .res 1 ; joy1 buttons state variable
  pointer:        .res 2 ; general purpose pointer

; Main code segement for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx PPU_CTRL	; disable NMI
  stx PPU_MASK 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit PPU_STATUS
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit PPU_STATUS
  bpl vblankwait2

main:
load_palettes:
  lda PPU_STATUS
  lda #$3f
  sta PPU_ADDR
  lda #$00
  sta PPU_ADDR
  ldx #$00
@loop:
  lda palettes, x
  sta PPU_DATA
  inx
  cpx #$04
  bne @loop

@update_bg:
  lda PPU_STATUS
  jsr clear_chram
  jsr load_tiles
  lda #$20  ;;Set PPU address to Nametable start
  sta PPU_ADDR
  lda #$00
  sta PPU_ADDR
  ldy #$00
  jsr draw_bg
@scroll:
  lda #$00
  sta PPU_SCROLL
  sta PPU_SCROLL

enable_rendering:
  
  lda #%01000000
  sta $4017
  lda #%00001111
  sta $4015

  lda #%10000000	; Enable NMI
  sta PPU_CTRL
  lda #%00001000	; Enable Sprites
  sta PPU_MASK

mainLoop:
  jsr update_controller
  lda buttons
  cmp #%00000001
  beq move_right
  cmp #%00000010
  beq move_left
  cmp #%00000100
  beq move_down
  cmp #%00001000
  beq move_up
draw:
  jsr ppu_update
  jmp mainLoop

move_right:
  dec scroll_x
  cmp #$FF
  beq @move_right2
  lda scroll_nmt
  eor #%00000001
  sta scroll_nmt
@move_right2:
  jmp draw
move_left:
  inc scroll_x
  bne @move_left2
  lda scroll_nmt
  eor #%00000001
  sta scroll_nmt
@move_left2:
  jmp draw

move_up:
  inc scroll_y
  jmp draw

move_down:
  dec scroll_y
  jmp draw



ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

nmi:
; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta PPU_MASK
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
@scroll:
  lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta PPU_CTRL
  lda scroll_x
  sta PPU_SCROLL
  lda scroll_y
  sta PPU_SCROLL
	; enable rendering
	lda #%00011110
	sta PPU_MASK
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	lda #0
	sta nmi_lock
@nmi_end:
	pla
	tay
	pla
	tax
	pla
	rti

draw_bg:
  ptr = $00 ; zero page pointer at $00, $01

  lda #<megumin_nametable
  sta ptr+0
  lda #>megumin_nametable
  sta ptr+1
  ldx #4 ; do this loop 4 times
  ldy #0
  :
    lda (ptr), Y
    sta PPU_DATA
    iny
    bne :-
    dex
    beq :+ ; finished if X = 0
    inc ptr+1 ; ptr = ptr + 256
    jmp :- ; loop again: Y = 0, X -= 1, ptr += 256
  :
  rts


update_controller:
  lda #$01
  sta IO_JOY1
  sta buttons
  lsr a      
  sta IO_JOY1
loop:
  lda IO_JOY1
  lsr a	      
  rol buttons
  bcc loop
  rts

.include "tile_loader.s"

palettes:
  ; Background Palette
  .byte $20, $15, $07, $26

megumin_nametable:
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07,$08,$00,$00,$00,$00,$09,$0a,$0b,$0c,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0d,$0e,$00,$00,$0f,$10,$11,$12,$13,$14,$15,$16,$00,$00,$17,$0c,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$18,$19,$00,$00,$00,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$24,$25,$00,$26,$27,$28,$29,$2a,$2b,$00,$00,$2c,$2d,$2e,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$2f,$00,$00,$30,$31,$32,$33,$34,$35,$36,$00,$00,$00,$37,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$00,$00,$42,$43,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$44,$00,$45,$46,$47,$48,$49,$4a,$4b,$4c,$00,$4d,$4e,$0c,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$00,$59,$0c,$0c,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$0c,$5a,$5b,$00,$5c,$5d,$55,$5e,$5f,$60,$00,$61,$38,$62,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$0c,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$7e,$00,$00,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$00,$88,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$0c,$0c,$89,$00,$8a,$8b,$8c,$8d,$8e,$8f,$90,$91,$92,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$00,$9c,$9d,$9e,$9f,$a0,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


