            *= $4000

v           = 53248 ;
leftedge    = $4001 ; detect left edge
rightedge   = $4002 ; detect right edge
topedge     = $4003 ; detect top edge
bottomedge  = $4004 ; detect bottom edge
clear       = $e544 ; clear screen       
enable      = $d015 ; 53269 enable sprites
enablem     = $d01C ; 53276 enable multi-color sprites
sprite0     = $7f8  ; 2040
color0      = $d027 ; 53287
sp0x        = $d000 ; 53248
sp0y        = $d001 ; 53249
msbx        = $d010 ; 53264 most significant high bit x-coord
jstick      = $dc01 ; 56321 joystick 1

shouse      = $0340 ; 832


setup   jsr clear
        lda #$00
        sta 53280
        lda #$06
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
        ldx #25
        stx sp0x
        ldy #51
        sty sp0y
        
main    lda jstick
        eor #255
        cmp #8
        beq right
        cmp #4
        beq left
        cmp #1
        beq up
        cmp #2
        beq down
        jmp main
        
right   jsr rightedgefunc ; detect if hit right edge
        lda rightedge
        cmp #1
        beq main

        ldx sp0x
        inx
        cpx #255
        bne movex
        lda #1
        sta msbx
        jmp movex
        
left    jsr leftedgefunc ; detect if hit left edge
        lda leftedge
        cmp #1
        beq main
        
        ldx sp0x
        dex
        cpx #255
        bne movex
        lda #0
        sta msbx
        jmp movex
        
up      jsr topedgefunc ; detect if hit top edge
        lda topedge
        cmp #1
        beq main

        ldx sp0y
        dex
        jmp movey
        
down    jsr bottomedgefunc ; detect if hit bottom edge
        lda bottomedge
        cmp #1
        beq main

        ldx sp0y
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
        
; -------- function to detect left edge --------
        
leftedgefunc lda #0
             sta leftedge
             lda msbx
             cmp #0
             beq leftedge0
             rts
           
leftedge0    ldx sp0x
             cpx #25
             beq hitleftedge
             rts
                  
hitleftedge  lda #1
             sta leftedge
             rts                      
        
; -------- function to detect right edge --------
        
rightedgefunc lda #0
              sta rightedge
              lda msbx
              cmp #1
              beq rightedge1
              rts
           
rightedge1    ldx sp0x
              cpx #63
              beq hitrightedge
              rts
                  
hitrightedge  lda #1
              sta rightedge
              rts                      
        
; -------- function to detect top edge --------
        
topedgefunc lda #0
            sta topedge
            ldx sp0y
            cpx #51
            beq hittopedge
            rts           
                  
hittopedge  lda #1
            sta topedge
            rts                      

; -------- function to detect bottom edge --------
        
bottomedgefunc lda #0
               sta bottomedge
               ldx sp0y
               cpx #228
               beq hitbottomedge
               rts           
                  
hitbottomedge  lda #1
               sta bottomedge
               rts                      
        
; -------- end game --------
                
end     rts                    


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

