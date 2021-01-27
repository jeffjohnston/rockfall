                   *= $4000

v                  = 53248 ;
leftEdge           = $4001 ; detect left edge
rightEdge          = $4002 ; detect right edge
topEdge            = $4003 ; detect top edge
bottomEdge         = $4004 ; detect bottom edge

chrout             = $ffd2
textColor          = $0286
plot               = $fff0
clearScreen        = $e544 ; clear screen       
enableSprites      = $d015 ; 53269 enable sprites
enableMultiSprites = $d01C ; 53276 enable multi-color sprites

hoverSprite        = $7f8  ; 2040
hoverColor         = $d027 ; 53287
hoverSpriteX       = $d000 ; 53248
hoverSpriteY       = $d001 ; 53249
hoverRightImg      = $2c0  ; 704 block 11
hoverLeftImg       = $340  ; 832 block 13

rocketSprite       = $7f9  ; 2041
rocketColor        = $d028 ; 53288
rocketSpriteX      = $d002 ; 53250
rocketSpriteY      = $d003 ; 53251
rocketRightImg     = $380  ; 869 block 14
rocketLeftImg      = $3c0  ; 960 block 15

mostSigBitX        = $d010 ; 53264
joyStick1          = $dc01 ; 56321

; -------- setup --------

    jsr clearScreen
    lda #$00
    sta 53280 ; border color
    lda #$06
    sta 53281 ; background color
    
    lda #11 ; block 11
    sta hoverSprite
    lda #14 ; block 14
    sta rocketSprite

    lda #1
    sta enableSprites
    lda #$ff
    sta enableMultiSprites
    lda #$00 ; black
    sta hoverColor
    sta rocketColor
    lda #$02 ; sprite multicolor 1 (red)
    sta $d025
    lda #$0f ; sprite multicolor 2 (light grey)
    sta $d026
    
    lda #0 ; begin high bit
    sta mostSigBitX
    ldx #75 ; begin x pos
    stx hoverSpriteX
    ldy #126 ; begin y pos
    sty hoverSpriteY

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

                    ldx #0

buildRocketRightImg lda rocketRightImgData,x
                    sta rocketRightImg,x
                    inx
                    cpx #63
                    bne buildRocketRightImg
        
                    ldx #0
        
buildRocketLeftImg lda rocketLeftImgData,x
                   sta rocketLeftImg,x
                   inx
                   cpx #63
                   bne buildRocketLeftImg
        
; -------- game loop --------
        
gameloop jsr pause
         jsr moveRocket

checkFireButton lda joyStick1
                eor #255
                cmp #16
                beq jmpShootRocket
                cmp #24 ; with fire button
                beq jmpShootRocket
                cmp #20 ; with fire button
                beq jmpShootRocket
                cmp #17 ; with fire button
                beq jmpShootRocket
                cmp #18 ; with fire button
                beq jmpShootRocket
                cmp #25 ; with fire button
                beq jmpShootRocket
                cmp #21 ; with fire button
                beq jmpShootRocket
                cmp #26 ; with fire button
                beq jmpShootRocket
                cmp #22 ; with fire button
                beq jmpShootRocket
                jmp checkMovement
         
jmpShootRocket   jmp shootRocket
         
checkMovement lda joyStick1
              eor #255
              cmp #8
              beq jmpMoveRight
              cmp #24 ; with fire button
              beq jmpMoveRight
              cmp #4
              beq jmpMoveLeft
              cmp #20 ; with fire button
              beq jmpMoveLeft
              cmp #1
              beq jmpMoveUp
              cmp #17 ; with fire button
              beq jmpMoveUp
              cmp #2
              beq jmpMoveDown
              cmp #18 ; with fire button
              beq jmpMoveDown
              cmp #9
              beq jmpMoveUpRight
              cmp #25 ; with fire button
              beq jmpMoveUpRight
              cmp #5
              beq jmpMoveUpLeft
              cmp #21 ; with fire button
              beq jmpMoveUpLeft
              cmp #10
              beq jmpMoveDownRight
              cmp #26 ; with fire button
              beq jmpMoveDownRight
              cmp #6
              beq jmpMoveDownLeft
              cmp #22 ; with fire button
              beq jmpMoveDownLeft
              jmp gameloop
    
    
jmpMoveRight     jmp moveRight
jmpMoveLeft      jmp moveLeft
jmpMoveUp        jmp moveUp
jmpMoveDown      jmp moveDown
jmpMoveUpRight   jmp moveUpRight
jmpMoveUpLeft    jmp moveUpLeft
jmpMoveDownRight jmp moveDownRight
jmpMoveDownLeft  jmp moveDownLeft
jmpFloatDown     jmp floatDown

; -------- shoot rocket --------  

shootRocket ldx hoverSprite
            cpx #11
            beq shootRocketRight
            bne shootRocketLeft             

shootRocketRight jsr rocketImgFaceRight

                 lda hoverSpriteX
                 cmp #234
                 bcs shootRocketJmpCheckMovement
                 
                 adc #21
                 sta rocketSpriteX

                 lda hoverSpriteY
                 sta rocketSpriteY
                 
                 jmp checkMovement  

shootRocketLeft jsr rocketImgFaceLeft

                lda hoverSpriteX
                cmp #47
                bcc shootRocketJmpCheckMovement
                
                sbc #22
                sta rocketSpriteX

                lda hoverSpriteY
                sta rocketSpriteY
                 
                jmp checkMovement
                
shootRocketJmpCheckMovement jmp checkMovement                

; -------- move rocket --------     

moveRocket ldx rocketSprite
           cpx #14
           beq moveRocketRight
           bne moveRocketLeft     
           rts

moveRocketRight lda #3
                sta enableSprites
                
                ldx rocketSpriteX
                inx
                stx rocketSpriteX                

                lda rocketSpriteX
                cmp #255
                beq stopRocket                
                rts

moveRocketLeft lda #3
               sta enableSprites

               ldx rocketSpriteX
               dex
               stx rocketSpriteX                

               lda rocketSpriteX
               cmp #25
               beq stopRocket                
               rts
                
stopRocket lda #1
           sta enableSprites

           lda #0
           sta rocketSpriteX
           sta rocketSpriteY
           rts

; -------- gameloop character floats down --------               
                
floatDown jsr bottomEdgeFunc
          lda bottomEdge
          cmp #1
          beq floatDownJmpGameLoop

          ldx hoverSpriteY
          inx
          stx hoverSpriteY
                
floatDownJmpGameLoop jmp gameloop
        
; -------- move gameloop character moveRight --------
        
moveRight jsr rightEdgeFunc
          lda rightEdge
          cmp #1
          beq rightJmpGameLoop

          jsr hoverImgFaceRight

          ldx hoverSpriteX
          inx
          stx hoverSpriteX
        
rightJmpGameLoop jmp gameloop
        
; -------- move gameloop character left --------               
        
moveLeft jsr leftEdgeFunc
         lda leftEdge
         cmp #1
         beq leftJmpGameLoop
        
         jsr hoverImgFaceLeft
        
         ldx hoverSpriteX
         dex
         stx hoverSpriteX
        
leftJmpGameLoop jmp gameloop

; -------- move gameloop character up --------               
                
moveUp jsr topEdgeFunc
       lda topEdge
       cmp #1
       beq upJmpGameLoop

       ldx hoverSpriteY
       dex
       stx hoverSpriteY
    
upJmpGameLoop jmp gameloop        
        
; -------- move gameloop character down --------               
                
moveDown jsr bottomEdgeFunc
         lda bottomEdge
         cmp #1
         beq downJmpGameLoop

         ldx hoverSpriteY
         inx
         stx hoverSpriteY
                
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

            ldx hoverSpriteX
            inx
            stx hoverSpriteX

            ldx hoverSpriteY
            dex
            stx hoverSpriteY
                
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

           ldx hoverSpriteX
           dex
           stx hoverSpriteX

           ldx hoverSpriteY
           dex
           stx hoverSpriteY
                
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

              ldx hoverSpriteX
              inx
              stx hoverSpriteX

              ldx hoverSpriteY
              inx
              stx hoverSpriteY
                
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
             
             ldx hoverSpriteX
             dex
             stx hoverSpriteX

             ldx hoverSpriteY
             inx
             stx hoverSpriteY
                
downLeftJmpGameLoop jmp gameloop        
   
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
             ldx hoverSpriteX
             cpx #25
             beq hitLeftEdge
             rts
           
hitLeftEdge  lda #1
             sta leftEdge
             rts                      
        
; -------- function to detect right edge --------
        
rightEdgeFunc lda #0
              sta rightEdge
              ldx hoverSpriteX
              cpx #255
              beq hitRightEdge
              rts
           
hitRightEdge  lda #1
              sta rightEdge
              rts                      
        
; -------- function to detect top edge --------
        
topEdgeFunc lda #0
            sta topEdge
            ldx hoverSpriteY
            cpx #51
            beq hitTopEdge
            rts           
                  
hitTopEdge  lda #1
            sta topEdge
            rts                      

; -------- function to detect bottom edge --------
        
bottomEdgeFunc lda #0
               sta bottomEdge
               ldx hoverSpriteY
               cpx #228
               beq hitBottomEdge
               rts           
                  
hitBottomEdge  lda #1
               sta bottomEdge
               rts     
               
; -------- turn gameloop character right --------
               
hoverImgFaceRight lda #11
                  sta hoverSprite
                  rts

; -------- turn gameloop character left --------
               
hoverImgFaceLeft lda #13
                 sta hoverSprite
                 rts

; -------- turn gameloop character right --------
               
rocketImgFaceRight lda #14
                   sta rocketSprite
                   rts

; -------- turn gameloop character left --------
               
rocketImgFaceLeft lda #15
                  sta rocketSprite
                  rts
                 
; -------- print hex value --------          
                 
printHexValue  pha
               lsr
               lsr
               lsr
               lsr
               jsr printHexNybble
               pla
               and #$0f
printHexNybble cmp #$0a
               bcs phnIsLetter
phnIsDigit     ora #$30
               bne phnPrint
phnIsLetter    sbc #$09
phnPrint       sta $0400, x
               inx               
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

rocketRightImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$0e,$00,$00
                   .byte $02,$80,$00,$06,$aa,$b0,$1a,$aa
                   .byte $ac,$06,$aa,$b0,$02,$80,$00,$0e
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$80

rocketLeftImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                  .byte $00,$00,$00,$00,$00,$00,$00,$00
                  .byte $00,$00,$00,$00,$00,$00,$00,$b0
                  .byte $00,$02,$80,$0e,$aa,$90,$3a,$aa
                  .byte $a4,$0e,$aa,$90,$00,$02,$80,$00
                  .byte $00,$b0,$00,$00,$00,$00,$00,$00
                  .byte $00,$00,$00,$00,$00,$00,$00,$00
                  .byte $00,$00,$00,$00,$00,$00,$00,$80

