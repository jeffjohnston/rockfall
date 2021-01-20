                   *= $4000

v                  = 53248 ;
leftEdge           = $4001 ; detect left edge
rightEdge          = $4002 ; detect right edge
topEdge            = $4003 ; detect top edge
bottomEdge         = $4004 ; detect bottom edge
bulletX            = $4005
bulletY            = $4006
bullet             = $4007
space              = $4008

chrout             = $ffd2
textColor          = $0286
plot               = $fff0
clearScreen        = $e544 ; clear screen       
enableSprites      = $d015 ; 53269 enable sprites
enableMultiSprites = $d01C ; 53276 enable multi-color sprites
sprite0            = $7f8  ; 2040
color0             = $d027 ; 53287
sprite0X           = $d000 ; 53248
sprite0Y           = $d001 ; 53249
mostSigBitX        = $d010 ; 53264
joyStick1          = $dc01 ; 56321

hoverRightImg      = $2c0 ; 704 block 11
hoverLeftImg       = $340 ; 832 block 13


; -------- setup --------

    jsr clearScreen
    lda #$00
    sta 53280 ; border color
    lda #$06
    sta 53281 ; background color

    lda #11 ; block 11
    sta sprite0

    lda #$ff
    sta enableSprites
    lda #$ff
    sta enableMultiSprites
    lda #$00 ; black
    sta color0
    lda #$02 ; sprite multicolor 1 (red)
    sta $d025
    lda #$0f ; sprite multicolor 2 (light grey)
    sta $d026

    lda #0 ; begin high bit
    sta mostSigBitX
    ldx #75 ; begin x pos
    stx sprite0X
    ldy #100 ; begin y pos
    sty sprite0Y

; -------- build images --------

                   ldx #0

buildHoverRightImg lda hoverRightImgData,x
                   sta hoverRightImg,x
                   inx
                   cpx #63
                   bne buildHoverRightImg
        
                   ldx #0
        
buildHoverLeftImg lda hoverLeftImgData,x
                  sta hoverLeftImg,x
                  inx
                  cpx #63
                  bne buildHoverLeftImg
                   

; -------- test bullet --------                   

lda #62
sta bullet
lda #32
sta space

ldx #$02	
stx textColor


start lda sprite0X
      sec
      sbc #24
      lsr
      lsr
      lsr
      adc #3
      sta bulletY     

      lda sprite0Y
      sec
      sbc #50
      lsr
      lsr
      lsr
      adc #1
      sta bulletX     

loop  ldx bulletX
      ldy bulletY         
      clc
      jsr plot  
      lda bullet     
      jsr chrout
      
      jsr longPause
      jsr longPause
      
      ldx bulletX
      ldy bulletY         
      clc
      jsr plot  
      lda space     
      jsr chrout
      
      inc bulletY
      ldy bulletY
      
      cpy #38
      bne loop
     
      
      



        
; -------- game loop --------
        
gameloop lda joyStick1
         eor #255
         cmp #8
         beq jmpMoveRight
         cmp #4
         beq jmpMoveLeft
         cmp #1
         beq jmpMoveUp
         cmp #2
         beq jmpMoveDown
         cmp #9
         beq jmpMoveUpRight
         cmp #5
         beq jmpMoveUpLeft
         cmp #10
         beq jmpMoveDownRight
         cmp #6
         beq jmpMoveDownLeft
         ;jmp floatDown
         jmp gameloop
    
    
jmpMoveRight     jmp moveRight
jmpMoveLeft      jmp moveLeft
jmpMoveUp        jmp moveUp
jmpMoveDown      jmp moveDown
jmpMoveUpRight   jmp moveUpRight
jmpMoveUpLeft    jmp moveUpLeft
jmpMoveDownRight jmp moveDownRight
jmpMoveDownLeft  jmp moveDownLeft

; -------- gameloop character floats down --------               
                
floatDown jsr bottomEdgeFunc
          lda bottomEdge
          cmp #1
          beq floatDownJmpGameLoop

          ldx sprite0Y
          inx
          stx sprite0Y
         
          jsr longPause
                
floatDownJmpGameLoop jmp gameloop
        
; -------- move gameloop character moveRight --------
        
moveRight jsr rightEdgeFunc
          lda rightEdge
          cmp #1
          beq rightJmpGameLoop

          jsr hoverImgFaceRight

          ldx sprite0X
          inx
          stx sprite0X
        
          jsr shortPause
          jsr testScreenX1
        
rightJmpGameLoop jmp gameloop
        
; -------- move gameloop character left --------               
        
moveLeft jsr leftEdgeFunc
         lda leftEdge
         cmp #1
         beq leftJmpGameLoop
        
         jsr hoverImgFaceLeft
        
         ldx sprite0X
         dex
         stx sprite0X
        
         jsr shortPause
         jsr testScreenX0
        
leftJmpGameLoop jmp gameloop

; -------- move gameloop character up --------               
                
moveUp jsr topEdgeFunc
       lda topEdge
       cmp #1
       beq upJmpGameLoop

       ldx sprite0Y
       dex
       stx sprite0Y
    
       jsr mediumPause
        
upJmpGameLoop jmp gameloop        
        
; -------- move gameloop character down --------               
                
moveDown jsr bottomEdgeFunc
         lda bottomEdge
         cmp #1
         beq downJmpGameLoop

         ldx sprite0Y
         inx
         stx sprite0Y

         jsr shortPause
                
downJmpGameLoop jmp gameloop

; -------- move gameloop character up and to the right --------  

moveUpRight jsr topEdgeFunc
            lda topEdge
            cmp #1
            beq upRightJmpGameLoop

            jsr rightEdgeFunc
            lda rightEdge
            cmp #1
            beq upRightJmpGameLoop
      
            jsr hoverImgFaceRight

            ldx sprite0X
            inx
            stx sprite0X

            ldx sprite0Y
            dex
            stx sprite0Y
    
            jsr mediumPause
            jsr testScreenX1
                
upRightJmpGameLoop jmp gameloop        

; -------- move gameloop character up and to the left --------  

moveUpLeft jsr topEdgeFunc
           lda topEdge
           cmp #1
           beq upLeftJmpGameLoop

           jsr leftEdgeFunc
           lda leftEdge
           cmp #1
           beq upLeftJmpGameLoop
      
           jsr hoverImgFaceLeft

           ldx sprite0X
           dex
           stx sprite0X

           ldx sprite0Y
           dex
           stx sprite0Y
    
           jsr mediumPause
           jsr testScreenX0
                
upLeftJmpGameLoop jmp gameloop     

; -------- move gameloop character down and to the right --------  

moveDownRight jsr bottomEdgeFunc
              lda bottomEdge
              cmp #1
              beq downRightJmpGameLoop
             
              jsr rightEdgeFunc
              lda rightEdge
              cmp #1
              beq downRightJmpGameLoop
             
              jsr hoverImgFaceRight

              ldx sprite0X
              inx
              stx sprite0X

              ldx sprite0Y
              inx
              stx sprite0Y
           
              jsr shortPause
              jsr testScreenX1
                
downRightJmpGameLoop jmp gameloop        

; -------- move gameloop character down and to the left --------  

moveDownLeft jsr bottomEdgeFunc
             lda bottomEdge
             cmp #1
             beq downLeftJmpGameLoop
             
             jsr leftEdgeFunc
             lda leftEdge
             cmp #1
             beq downLeftJmpGameLoop
             
             jsr hoverImgFaceLeft
             
             ldx sprite0X
             dex
             stx sprite0X

             ldx sprite0Y
             inx
             stx sprite0Y
           
             jsr shortPause
             jsr testScreenX0
                
downLeftJmpGameLoop jmp gameloop        
   
        
; -------- subroutine to set the high bit to 1 if needed --------

testScreenX1 ldx sprite0X
             cpx #255
             beq moveHighBit1
             rts
             
moveHighBit1 lda #1
             sta mostSigBitX
             rts

; -------- subroutine to set the high bit to 0 if needed --------

testScreenX0 ldx sprite0X
             cpx #255
             beq moveHighBit0
             rts
             
moveHighBit0 lda #0
             sta mostSigBitX
             rts             
             
; -------- long pause --------
        
longPause jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          jsr pause
          rts

; -------- medium pause --------
        
mediumPause jsr pause
            jsr pause
            jsr pause
            rts

; -------- short pause --------
        
shortPause jsr pause
           jsr pause
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
             lda mostSigBitX
             cmp #0
             beq leftEdge0
             rts
           
leftEdge0    ldx sprite0X
             cpx #25
             beq hitLeftEdge
             rts
                  
hitLeftEdge  lda #1
             sta leftEdge
             rts                      
        
; -------- function to detect right edge --------
        
rightEdgeFunc lda #0
              sta rightEdge
              lda mostSigBitX
              cmp #1
              beq rightEdge1
              rts
           
rightEdge1    ldx sprite0X
              cpx #63
              beq hitRightEdge
              rts
                  
hitRightEdge  lda #1
              sta rightEdge
              rts                      
        
; -------- function to detect top edge --------
        
topEdgeFunc lda #0
            sta topEdge
            ldx sprite0Y
            cpx #51
            beq hitTopEdge
            rts           
                  
hitTopEdge  lda #1
            sta topEdge
            rts                      

; -------- function to detect bottom edge --------
        
bottomEdgeFunc lda #0
               sta bottomEdge
               ldx sprite0Y
               cpx #228
               beq hitBottomEdge
               rts           
                  
hitBottomEdge  lda #1
               sta bottomEdge
               rts     
               
; -------- turn gameloop character right --------
               
hoverImgFaceRight lda #11
                  sta sprite0
                  rts

; -------- turn gameloop character left --------
               
hoverImgFaceLeft lda #13
                 sta sprite0
                 rts
                                        
; -------- end game --------
                
end     rts                    


hoverRightImgData .byte $00,$aa,$00,$02,$be,$80,$0a,$bf
                  .byte $c0,$0b,$be,$c0,$0b,$be,$f0,$0b
                  .byte $ff,$c0,$0b,$ff,$c0,$02,$fa,$00
                  .byte $00,$3f,$00,$01,$be,$40,$01,$69
                  .byte $40,$03,$55,$c0,$03,$55,$c0,$03
                  .byte $55,$c0,$00,$55,$00,$01,$69,$40
                  .byte $01,$41,$40,$05,$41,$50,$0f,$c3
                  .byte $f0,$aa,$aa,$aa,$28,$00,$28,$80

hoverLeftImgData .byte $00,$aa,$00,$02,$be,$80,$03,$fe
                 .byte $a0,$03,$be,$e0,$0f,$be,$e0,$03
                 .byte $ff,$e0,$03,$ff,$e0,$00,$af,$80
                 .byte $00,$fc,$00,$01,$be,$40,$01,$69
                 .byte $40,$03,$55,$c0,$03,$55,$c0,$03
                 .byte $55,$c0,$00,$55,$00,$01,$69,$40
                 .byte $01,$41,$40,$05,$41,$50,$0f,$c3
                 .byte $f0,$aa,$aa,$aa,$28,$00,$28,$80        

