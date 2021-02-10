                   *= $4000

v                  = 53248
irqvec             = $314 ; 788
irqnor             = $ea31 ; 59953 standard irq routine

chrout             = $ffd2
textColor          = $0286
plot               = $fff0
random             = $d41b ; 53299
clearScreen        = $e544 ; clear screen       
enableSprites      = $d015 ; 53269 enable sprites
enableMultiSprites = $d01C ; 53276 enable multi-color sprites
spriteCollision    = $d01e ; 53278

hoverSprite        = $7f8  ; 2040
hoverColor         = $d027 ; 53287
hoverSpriteX       = $d000 ; 53248
hoverSpriteY       = $d001 ; 53249
hoverRightImg      = $3000 ; 12288 block 192 (64*192=12288)
hoverLeftImg       = $3040 ; 12352 block 193
 
missleSprite       = $7f9  ; 2041
missleColor        = $d028 ; 53288
missleSpriteX      = $d002 ; 53250
missleSpriteY      = $d003 ; 53251
missleRightImg     = $3080 ; 12416 block 194
missleLeftImg      = $30C0 ; 12480 block 195

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
    sta missleSprite
    lda #196 ; block 196
    sta rockSprite

    lda #1
    sta enableSprites
    lda #$ff
    sta enableMultiSprites
    
    lda #$02 ; red (individual color)
    sta hoverColor
    sta missleColor
    
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
    
    ; turn on the randomizer
    lda #$ff  ; maximum frequency value
    sta $d40e ; voice 3 frequency low byte
    sta $d40f ; voice 3 frequency high byte
    lda #$80  ; noise waveform, gate bit off
    sta $d412 ; voice 3 control register

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
buildMissleRightImg lda missleRightImgData,x
                    sta missleRightImg,x
                    inx
                    cpx #63
                    bne buildMissleRightImg
        
                   ldx #0        
buildMissleLeftImg lda missleLeftImgData,x
                   sta missleLeftImg,x
                   inx
                   cpx #63
                   bne buildMissleLeftImg
                    
             ldx #0        
buildRockImg lda rockImgData,x
             sta rockImg,x
             inx
             cpx #63
             bne buildRockImg
        
; -------- game loop --------
        
gameloop jsr moveMissle
         jsr moveRock

checkFireButton lda joyStick1
                eor #255
                cmp #16
                beq jmpShootMissle
                cmp #24 ; with fire button
                beq jmpShootMissle
                cmp #20 ; with fire button
                beq jmpShootMissle
                cmp #17 ; with fire button
                beq jmpShootMissle
                cmp #18 ; with fire button
                beq jmpShootMissle
                cmp #25 ; with fire button
                beq jmpShootMissle
                cmp #21 ; with fire button
                beq jmpShootMissle
                cmp #26 ; with fire button
                beq jmpShootMissle
                cmp #22 ; with fire button
                beq jmpShootMissle
                jmp checkJoystick
         
jmpShootMissle  jmp shootMissle
         
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

; -------- shoot missle --------  

shootMissle lda enableSprites
            and #2
            cmp #2 
            beq shootMissleJmpCheckJoystick

            ldx hoverSprite
            cpx #192
            beq shootMissleRight
            bne shootMissleLeft             

shootMissleRight jsr missleImgFaceRight

                 lda hoverSpriteX
                 cmp #234
                 bcs shootMissleJmpCheckJoystick
                 
                 adc #21
                 sta missleSpriteX

                 lda hoverSpriteY
                 sta missleSpriteY
                 
                 lda enableSprites
                 eor #2
                 sta enableSprites               
                 
                 jmp shootMissleJmpCheckJoystick  

shootMissleLeft jsr missleImgFaceLeft

                lda hoverSpriteX
                cmp #47
                bcc shootMissleJmpCheckJoystick
                
                sbc #22
                sta missleSpriteX

                lda hoverSpriteY
                sta missleSpriteY
                 
                lda enableSprites
                eor #2
                sta enableSprites               
                 
                jmp shootMissleJmpCheckJoystick
                
shootMissleJmpCheckJoystick jmp checkJoystick                

; -------- move missle --------     

moveMissle lda enableSprites
           and #2
           cmp #2 
           beq moveMissleLR
           rts

moveMissleLR ldx missleSprite
             cpx #194
             beq moveMissleRight
             bne moveMissleLeft     
             rts
           
moveMissleRight ldx missleCanMove
                cpx #0
                beq moveMissleRight
          
                ldx #0
                stx missleCanMove
                
                ldx missleSpriteX
                inx
                inx
                stx missleSpriteX                

                lda missleSpriteX
                cmp #254
                bcs stopMissle                
                rts

moveMissleLeft ldx missleCanMove
               cpx #0
               beq moveMissleLeft
          
               ldx #0
               stx missleCanMove

               ldx missleSpriteX
               dex
               dex
               stx missleSpriteX                

               lda missleSpriteX
               cmp #24
               bcc stopMissle                
               rts
                
stopMissle lda enableSprites
           eor #2
           sta enableSprites

           lda #0
           sta missleSpriteX
           sta missleSpriteY
           rts

; -------- move rock --------               

moveRock lda rockCanMove
         cmp #1
         beq moveRockYN

         jmp finishRock
         
moveRockYN lda #0 ; reset timer
           sta rockDelayTimer
           sta rockCanMove

           lda enableSprites ; see if rock is moving
           and #4
           cmp #4
           beq checkRockForCollision ; rock is already moving
           
startRockMove lda random
              and #127
              cmp #7
              bne finishRock

              lda enableSprites
              eor #4
              sta enableSprites
               
              lda #36 ; begin x pos
              sta rockSpriteX
              lda #50 ; begin y pos
              sta rockSpriteY           
               
              jmp finishRock
  
moveRockDown ldy rockSpriteY
             iny
             sty rockSpriteY
             
             lda rockSpriteY
             cmp #228
             beq rockHitBottom

             jmp finishRock
             
rockHitBottom lda enableSprites ; remove rock
              eor #4
              sta enableSprites

              jmp finishRock
                        
checkRockForCollision lda spriteCollision
                      and #4
                      cmp #4
                      bne moveRockDown
                      
                      lda enableSprites ; remove rock and missle
                      eor #6
                      sta enableSprites
                      
                      jmp finishRock

finishRock rts                      

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
               
missleImgFaceRight lda #194
                   sta missleSprite
                   rts

; -------- turn gameloop character left --------
               
missleImgFaceLeft lda #195
                  sta missleSprite
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
               stx missleCanMove
               stx rockCanMove
               stx rockDelayTimer

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
        
customIrq lda #1
          sta hoverCanMove
          sta missleCanMove

          ldx rockDelayTimer
          inx
          stx rockDelayTimer
          cpx #2
          beq setRockCanMove
          jmp standardIrq
          
setRockCanMove lda #1
               sta rockCanMove
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
missleCanMove   .byte 0 ; check to see if missle can move
rockCanMove     .byte 0 ; check to see if rock can move
rockDelayTimer  .byte 0 ; check to see if rock can move

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

missleRightImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$0d,$00,$00
                   .byte $01,$40,$00,$09,$55,$70,$25,$55
                   .byte $5c,$09,$55,$70,$01,$40,$00,$0d
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$82

missleLeftImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
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

