            *= $4000

v           = 53248 ;
leftEdge    = $4001 ; detect left edge
rightEdge   = $4002 ; detect right edge
topEdge     = $4003 ; detect top edge
bottomEdge  = $4004 ; detect bottom edge
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
        
right   jsr rightEdgeFunc ; detect if hit right edge
        lda rightEdge
        cmp #1
        beq main

        ldx sp0x
        inx
        stx sp0x
        
        jsr pause
        jsr testScreenX1
        jmp main
        
left    jsr leftEdgeFunc ; detect if hit left edge
        lda leftEdge
        cmp #1
        beq main
        
        ldx sp0x
        dex
        stx sp0x
        
        jsr pause
        jsr testScreenX0
        jmp main
        
up      jsr topEdgeFunc ; detect if hit top edge
        lda topEdge
        cmp #1
        beq main

        ldx sp0y
        dex
        stx sp0y
        
        jsr pause
        jmp main
        
down    jsr bottomEdgeFunc ; detect if hit bottom edge
        lda bottomEdge
        cmp #1
        beq main

        ldx sp0y
        inx
        stx sp0y
        
        jsr pause
        jmp main
        
; -------- subroutine to set the high bit to 1 if needed --------

testScreenX1 ldx sp0x
             cpx #255
             beq moveHighBit1
             rts
             
moveHighBit1 lda #1
             sta msbx
             rts

; -------- subroutine to set the high bit to 0 if needed --------

testScreenX0 ldx sp0x
             cpx #255
             beq moveHighBit0
             rts
             
moveHighBit0 lda #0
             sta msbx
             rts
        
; -------- subroutine to pause motion --------
        
pause    ldy #0
         jsr pause255

pause255 iny
         cpy #255
         bne pause255
         rts
        
; -------- function to detect left edge --------
        
leftEdgeFunc lda #0
             sta leftEdge
             lda msbx
             cmp #0
             beq leftEdge0
             rts
           
leftEdge0    ldx sp0x
             cpx #25
             beq hitLeftEdge
             rts
                  
hitLeftEdge  lda #1
             sta leftEdge
             rts                      
        
; -------- function to detect right edge --------
        
rightEdgeFunc lda #0
              sta rightEdge
              lda msbx
              cmp #1
              beq rightEdge1
              rts
           
rightEdge1    ldx sp0x
              cpx #63
              beq hitRightEdge
              rts
                  
hitRightEdge  lda #1
              sta rightEdge
              rts                      
        
; -------- function to detect top edge --------
        
topEdgeFunc lda #0
            sta topEdge
            ldx sp0y
            cpx #51
            beq hitTopEdge
            rts           
                  
hitTopEdge  lda #1
            sta topEdge
            rts                      

; -------- function to detect bottom edge --------
        
bottomEdgeFunc lda #0
               sta bottomEdge
               ldx sp0y
               cpx #228
               beq hitBottomEdge
               rts           
                  
hitBottomEdge  lda #1
               sta bottomEdge
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

