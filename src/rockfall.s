          *= $4000

v       = 53248 ;
clear   = $e544 ; clear screen       
enable  = $d015 ; 53269 enable sprites
enablem = $d01C ; 53276 enable multi-color sprites
sprite0 = $7f8  ; 2040
color0  = $d027 ; 53287
sp0x    = $d000 ; 53248
sp0y    = $d001 ; 53249
msbx    = $d010 ; 53264 horizontal high bit
jstick  = $dc00 ; 56320

shouse  = $0340 ; 832


setup   jsr clear
        lda #$0e
        sta 53280
        sta 53281
        lda #$0d    ; block 13
        sta sprite0
        lda #$ff
        sta enable
        lda #$ff
        sta enablem
        lda #$00     ; black
        sta color0
        lda #$02    ; sprite multicolor 1 (red)
        sta $d025
        lda #$0f    ; sprite multicolor 2 (light grey)
        sta $d026
        ldx #0

build   lda databoyr,x
        sta shouse,x
        inx
        cpx #63
        bne build
        lda #0
        sta msbx
        ldx #100
        lda #70
        stx sp0x
        sta sp0y
        
main    lda jstick
        and #15
        cmp #7
        beq right
        cmp #11
        beq left
        cmp #14
        beq up
        cmp #13
        beq down
        jmp main
        
right   ldx sp0x
        inx
        cpx #255
        bne movex
        lda #1
        sta msbx
        jmp movex
        
left    ldx sp0x
        dex
        cpx #255
        bne movex
        lda #0
        sta msbx
        jmp movex
        
up      ldx sp0y
        dex
        jmp movey
        
down    ldx sp0y
        inx
        jmp movey
        
movex   stx sp0x
        ldy #0
        jmp pause
        
movey   stx sp0y
        ldy #0
        jmp pause
        
pause   iny
        cpy #255
        bne pause
        jmp main        
        
        rts                    


databoyr .byte $00,$aa,$00,$02,$be,$80,$0a,$bf
         .byte $c0,$0b,$be,$c0,$0b,$be,$f0,$0b
         .byte $ff,$c0,$0b,$ff,$c0,$02,$fa,$00
         .byte $00,$3f,$00,$01,$be,$40,$01,$69
         .byte $40,$03,$55,$c0,$03,$55,$c0,$03
         .byte $55,$c0,$00,$55,$00,$01,$69,$40
         .byte $01,$41,$40,$05,$41,$50,$0f,$c3
         .byte $f0,$aa,$aa,$aa,$28,$00,$28,$80

databoyl .byte $00,$aa,$00,$02,$be,$80,$03,$fe
         .byte $a0,$03,$be,$e0,$0f,$be,$e0,$03
         .byte $ff,$e0,$03,$ff,$e0,$00,$af,$80
         .byte $00,$fc,$00,$01,$be,$40,$01,$69
         .byte $40,$03,$55,$c0,$03,$55,$c0,$03
         .byte $55,$c0,$00,$55,$00,$01,$69,$40
         .byte $01,$41,$40,$05,$41,$50,$0f,$c3
         .byte $f0,$aa,$aa,$aa,$28,$00,$28,$80        

