                   *= $4000

v                  = 53248
irqvec             = $314 ; 788
irqnor             = $ea31 ; 59953 standard irq routine

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
hoverRightImg      = $3000 ; 12288 block 192 (64*192=12288)
hoverLeftImg       = $3040 ; 12352 block 193
 
rocketSprite       = $7f9  ; 2041
rocketColor        = $d028 ; 53288
rocketSpriteX      = $d002 ; 53250
rocketSpriteY      = $d003 ; 53251
rocketRightImg     = $3080 ; 12416 block 194
rocketLeftImg      = $30C0 ; 12480 block 195

rockSprite         = $7fa  ; 2042
rockColor          = $d029 ; 53289
rockSpriteX        = $d004 ; 53252
rockSpriteY        = $d005 ; 53253
rockImg            = $3100 ; 12544 block 196

mostSigBitX        = $d010 ; 53264
joyStick1          = $dc01 ; 56321

; -------- setup --------

    jsr clearScreen
    jsr setupCustomIrq
    
    lda #$00
    sta 53280 ; border color
    lda #$06
    sta 53281 ; background color
    
    lda #192 ; block 192
    sta hoverSprite
    lda #194 ; block 194
    sta rocketSprite
    lda #196 ; block 196
    sta rockSprite

    lda #1
    sta enableSprites
    lda #$ff
    sta enableMultiSprites
    
    lda #$02 ; red (individual color)
    sta hoverColor
    sta rocketColor
    
    lda #$08 ; brown (individual color)
    sta rockColor
    
    lda #$00 ; sprite multicolor 1 (black)
    sta $d025
    lda #$0f ; sprite multicolor 2 (light grey)
    sta $d026
    
    lda #0 ; begin high bit
    sta mostSigBitX
    
    ldx #75 ; begin x pos
    stx hoverSpriteX
    ldy #126 ; begin y pos
    sty hoverSpriteY
    
;    lda enableSprites
;    eor #4
;    sta enableSprites

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
                    
             ldx #0
        
buildRockImg lda rockImgData,x
             sta rockImg,x
             inx
             cpx #63
             bne buildRockImg
        
; -------- game loop --------
        
gameloop jsr moveRocket

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
                jmp checkJoystick
         
jmpShootRocket  jmp shootRocket
         
checkJoystick lda joyStick1
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

shootRocket lda enableSprites
            and #2
            cmp #2 
            beq shootRocketJmpCheckJoystick

            ldx hoverSprite
            cpx #192
            beq shootRocketRight
            bne shootRocketLeft             

shootRocketRight jsr rocketImgFaceRight

                 lda hoverSpriteX
                 cmp #234
                 bcs shootRocketJmpCheckJoystick
                 
                 adc #21
                 sta rocketSpriteX

                 lda hoverSpriteY
                 sta rocketSpriteY
                 
                 lda enableSprites
                 eor #2
                 sta enableSprites               
                 
                 jmp checkJoystick  

shootRocketLeft jsr rocketImgFaceLeft

                lda hoverSpriteX
                cmp #47
                bcc shootRocketJmpCheckJoystick
                
                sbc #22
                sta rocketSpriteX

                lda hoverSpriteY
                sta rocketSpriteY
                 
                lda enableSprites
                eor #2
                sta enableSprites               
                 
                jmp checkJoystick
                
shootRocketJmpCheckJoystick jmp checkJoystick                

; -------- move rocket --------     

moveRocket ldx rocketSprite
           cpx #194
           beq moveRocketRight
           bne moveRocketLeft     
           rts

moveRocketRight ldx rocketCanMove
                cpx #0
                beq moveRocketRight
          
                ldx #0
                stx rocketCanMove
                
                ldx rocketSpriteX
                inx
                inx
                stx rocketSpriteX                

                lda rocketSpriteX
                cmp #254
                bcs stopRocket                
                rts

moveRocketLeft ldx rocketCanMove
               cpx #0
               beq moveRocketLeft
          
               ldx #0
               stx rocketCanMove

               ldx rocketSpriteX
               dex
               dex
               stx rocketSpriteX                

               lda rocketSpriteX
               cmp #24
               bcc stopRocket                
               rts
                
stopRocket lda enableSprites
           eor #2
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
          jsr checkIfHoverCanMove

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
         jsr checkIfHoverCanMove
        
         ldx hoverSpriteX
         dex
         stx hoverSpriteX
        
leftJmpGameLoop jmp gameloop

; -------- move gameloop character up --------               
                
moveUp jsr topEdgeFunc
       lda topEdge
       cmp #1
       beq upJmpGameLoop

       jsr checkIfHoverCanMove

       ldx hoverSpriteY
       dex
       stx hoverSpriteY
    
upJmpGameLoop jmp gameloop        
        
; -------- move gameloop character down --------               
                
moveDown jsr bottomEdgeFunc
         lda bottomEdge
         cmp #1
         beq downJmpGameLoop

         jsr checkIfHoverCanMove

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
            jsr checkIfHoverCanMove

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
           jsr checkIfHoverCanMove

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
              jsr checkIfHoverCanMove

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
             jsr checkIfHoverCanMove
             
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
               
hoverImgFaceRight lda #192
                  sta hoverSprite
                  rts

; -------- turn gameloop character left --------
               
hoverImgFaceLeft lda #193
                 sta hoverSprite
                 rts

; -------- turn gameloop character right --------
               
rocketImgFaceRight lda #194
                   sta rocketSprite
                   rts

; -------- turn gameloop character left --------
               
rocketImgFaceLeft lda #195
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
             
; -------- check to see if can move hover --------
             
checkIfHoverCanMove ldx hoverCanMove
                    cpx #0
                    beq checkIfHoverCanMoveJmpGameLoop
          
                    ldx #0
                    stx hoverCanMove
                    rts

checkIfHoverCanMoveJmpGameLoop jmp gameloop
             
; -------- setup irq delay --------

setupCustomIrq ldx #0
               stx hoverCanMove
               stx rocketCanMove

               sei
               lda #<customIrq
               sta irqvec
               lda #>customIrq
               sta irqvec+1
               
;               lda #$00
;               sta $d012
;               lda #$36
;               sta $d012
;               lda #$7f
;               sta $dc0d
;               lda #$1b
;               sta $d011
;               lda #7
;               sta $d01a

               cli
               rts
           
; -------- irq delay --------
        
customIrq ldx #1
          stx hoverCanMove
          stx rocketCanMove
          jmp standardIrq
         
standardIrq jmp irqnor             
                                        
; -------- end game --------
                
end     rts                    

; -------- custom variables --------

leftEdge        .byte 0 ; detect left edge
rightEdge       .byte 0 ; detect right edge
topEdge         .byte 0 ; detect top edge
bottomEdge      .byte 0 ; detect bottom edge
hoverCanMove    .byte 0 ; check to see if hover can move
rocketCanMove   .byte 0 ; check to see if rocket can move

hoverRightImgData .byte $00,$55,$00,$01,$7d,$40,$05,$7f
                  .byte $c0,$07,$7d,$c0,$07,$7d,$f0,$07
                  .byte $ff,$c0,$07,$ff,$c0,$01,$f5,$00
                  .byte $00,$3f,$00,$02,$7d,$80,$02,$96
                  .byte $80,$03,$aa,$c0,$03,$aa,$c0,$03
                  .byte $aa,$c0,$00,$aa,$00,$02,$96,$80
                  .byte $02,$82,$80,$0a,$82,$a0,$0f,$c3
                  .byte $f0,$55,$55,$55,$14,$00,$14,$82

hoverLeftImgData .byte $00,$55,$00,$01,$7d,$40,$03,$fd
                 .byte $50,$03,$7d,$d0,$0f,$7d,$d0,$03
                 .byte $ff,$d0,$03,$ff,$d0,$00,$5f,$40
                 .byte $00,$fc,$00,$02,$7d,$80,$02,$96
                 .byte $80,$03,$aa,$c0,$03,$aa,$c0,$03
                 .byte $aa,$c0,$00,$aa,$00,$02,$96,$80
                 .byte $02,$82,$80,$0a,$82,$a0,$0f,$c3
                 .byte $f0,$55,$55,$55,$14,$00,$14,$82   

rocketRightImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$0d,$00,$00
                   .byte $01,$40,$00,$09,$55,$70,$25,$55
                   .byte $5c,$09,$55,$70,$01,$40,$00,$0d
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$82

rocketLeftImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$70
                   .byte $00,$01,$40,$0d,$55,$60,$35,$55
                   .byte $58,$0d,$55,$60,$00,$01,$40,$00
                   .byte $00,$70,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$82
                  
rockImgData .byte $00,$00,$00,$00,$55,$00,$01,$aa
            .byte $40,$01,$aa,$40,$06,$aa,$90,$1a
            .byte $aa,$a4,$1a,$aa,$a4,$1a,$aa,$a4
            .byte $6a,$aa,$a9,$6a,$aa,$a9,$6a,$aa
            .byte $a9,$6a,$aa,$a9,$6a,$aa,$a9,$1a
            .byte $aa,$a4,$1a,$aa,$a4,$1a,$aa,$a4
            .byte $06,$aa,$90,$01,$aa,$40,$01,$aa
            .byte $40,$00,$55,$00,$00,$00,$00,$88

