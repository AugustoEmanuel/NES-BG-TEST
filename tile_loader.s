.segment "CODE"

clear_chram:
    lda PPU_STATUS
    lda #$00
    sta PPU_ADDR
    sta PPU_ADDR
    lda #$00
    ldx #32 ; do this loop 16 times
    ldy #$00
    :
        sta PPU_DATA
        iny
        bne :-
        dex
        beq :+ ; finished if X = 0
        jmp :- ; loop again: Y = 0, X -= 1, ptr += 256
    :
    rts

load_tiles:
    lda PPU_STATUS
    lda #$00
    sta PPU_ADDR
    sta PPU_ADDR
    lda #<megumin_tiles
    sta pointer+0
    lda #>megumin_tiles
    sta pointer+1
    ldx #16 ; do this loop 16 times
    ldy #0
    :
        lda (pointer), Y
        sta PPU_DATA
        iny
        bne :-
        dex
        beq :+ ; finished if X = 0
        inc pointer+1 ; ptr = ptr + 256
        jmp :- ; loop again: Y = 0, X -= 1, ptr += 256
    :
    rts

megumin_tiles:
.incbin "megumin.chr"