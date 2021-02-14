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
 
missileSprite       = $7f9  ; 2041
missileColor        = $d028 ; 53288
missileSpriteX      = $d002 ; 53250
missileSpriteY      = $d003 ; 53251
missileRightImg     = $3080 ; 12416 block 194
missileLeftImg      = $30C0 ; 12480 block 195

rockSprite1        = $7fa  ; 2042
rockSprite1X       = $d004 ; 53252
rockSprite1Y       = $d005 ; 53253
rockColor1         = $d029 ; 53289

rockSprite2        = $7fb  ; 2043
rockSprite2X       = $d006 ; 53254
rockSprite2Y       = $d007 ; 53255
rockColor2         = $d02a ; 53290

rockSprite3        = $7fc  ; 2044
rockSprite3X       = $d008 ; 53256
rockSprite3Y       = $d009 ; 53257
rockColor3         = $d02b ; 53291

rockSprite4        = $7fd  ; 2045
rockSprite4X       = $d00a ; 53258
rockSprite4Y       = $d00b ; 53259
rockColor4         = $d02c ; 53292

rockSprite5        = $7fe  ; 2046
rockSprite5X       = $d00c ; 53260
rockSprite5Y       = $d00d ; 53261
rockColor5         = $d02d ; 53293

rockImg            = $3100 ; 12544 block 196

mostSigBitX        = $d010 ; 53264
joyStick1          = $dc01 ; 56321

; -------- setup --------

setup jsr clearScreen
      jsr setupCustomIrq
    
      lda #$00
      sta 53280 ; border color
      lda #$06
      sta 53281 ; background color
    
      lda #192 ; block 192
      sta hoverSprite
      lda #194 ; block 194
      sta missileSprite
      lda #196 ; block 196
      sta rockSprite1
      sta rockSprite2
      sta rockSprite3
      sta rockSprite4
      sta rockSprite5

      lda #1
      sta enableSprites
      lda #$ff
      sta enableMultiSprites
    
      lda #$02 ; red (individual color)
      sta hoverColor
      sta missileColor
    
      lda #$08 ; brown (individual color)
      sta rockColor1
      sta rockColor2
      sta rockColor3
      sta rockColor4
      sta rockColor5
    
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
buildMissileRightImg lda missileRightImgData,x
                    sta missileRightImg,x
                    inx
                    cpx #63
                    bne buildMissileRightImg
        
                   ldx #0        
buildMissileLeftImg lda missileLeftImgData,x
                   sta missileLeftImg,x
                   inx
                   cpx #63
                   bne buildMissileLeftImg
                    
             ldx #0        
buildRockImg lda rockImgData,x
             sta rockImg,x
             inx
             cpx #63
             bne buildRockImg
        
; -------- game loop --------
        
gameloop lda refreshScreen
         cmp #1
         bne gameloop
         
         lda #0
         sta refreshScreen

         lda spriteCollision 
         sta spriteCollisionDetector ; copy collision

         jsr moveMissile
         jsr moveRocks
         
;         jsr moveRock2
;         jsr moveRock3
;         jsr moveRock4
;         jsr moveRock5         
         jsr checkFireButton
         jsr checkJoystick
         jmp gameloop
         
; -------- move missile --------

moveMissile lda enableSprites
            and #2
            cmp #2 
            beq moveMissileLR
            rts

moveMissileLR ldx missileSprite
              cpx #194
              beq moveMissileRight
              bne moveMissileLeft
              rts
           
moveMissileRight ldx missileSpriteX
                 inx
                 inx
                 stx missileSpriteX

                 lda missileSpriteX
                 cmp #254
                 bcs stopMissile
                 rts

moveMissileLeft ldx missileSpriteX
                dex
                dex
                stx missileSpriteX

                lda missileSpriteX
                cmp #24
                bcc stopMissile
                rts
                
stopMissile lda enableSprites
            eor #2
            sta enableSprites

            lda #0
            sta missileSpriteX
            sta missileSpriteY
            rts

; -------- move rocks --------


moveRocks lda #4
          sta rockSpriteBit
          lda #6
          sta rockSpriteBitPlus2
          lda #0
          sta rockSpriteOffset
          lda #36
          sta rockSpriteXPos         

          jsr moveRock
          
          lda #8
          sta rockSpriteBit
          lda #10
          sta rockSpriteBitPlus2
          lda #2
          sta rockSpriteOffset
          lda #82
          sta rockSpriteXPos
          
          jsr moveRock

          lda #16
          sta rockSpriteBit
          lda #18
          sta rockSpriteBitPlus2
          lda #4
          sta rockSpriteOffset
          lda #128
          sta rockSpriteXPos
          
          jsr moveRock

          lda #32
          sta rockSpriteBit
          lda #34
          sta rockSpriteBitPlus2
          lda #6
          sta rockSpriteOffset
          lda #174
          sta rockSpriteXPos
          
          jsr moveRock

          lda #64
          sta rockSpriteBit
          lda #66
          sta rockSpriteBitPlus2
          lda #8
          sta rockSpriteOffset
          lda #220
          sta rockSpriteXPos
          
          jsr moveRock

          rts
            
; -------- move rock --------

moveRock lda enableSprites ; see if rock is moving
         and rockSpriteBit
         cmp rockSpriteBit
         beq checkRockForCollision
           
startRockMove lda random
              and #63
              cmp #7
              bne finishRock

              lda enableSprites
              eor rockSpriteBit
              sta enableSprites
               
              ldx rockSpriteOffset
              lda rockSpriteXPos ; begin x pos
              sta rockSprite1X, x
              lda #50 ; begin y pos
              sta rockSprite1Y, x
               
              rts
  
moveRockDown ldx rockSpriteOffset
             ldy rockSprite1Y, x
             iny
             tya
             sta rockSprite1Y, x
             
             lda rockSprite1Y, x
             cmp #228
             beq rockHitBottom

             rts
             
rockHitBottom lda enableSprites ; remove rock
              eor rockSpriteBit
              sta enableSprites

              rts
                        
checkRockForCollision lda spriteCollisionDetector
                      and rockSpriteBitPlus2
                      cmp rockSpriteBitPlus2
                      bne moveRockDown
                      
                      lda enableSprites ; remove rock and missile
                      eor rockSpriteBitPlus2
                      sta enableSprites

finishRock rts    

; -------- check fire button --------         

checkFireButton lda joyStick1
                eor #255
                cmp #16
                beq shootMissile
                cmp #24 ; with fire button
                beq shootMissile
                cmp #20 ; with fire button
                beq shootMissile
                cmp #17 ; with fire button
                beq shootMissile
                cmp #18 ; with fire button
                beq shootMissile
                cmp #25 ; with fire button
                beq shootMissile
                cmp #21 ; with fire button
                beq shootMissile
                cmp #26 ; with fire button
                beq shootMissile
                cmp #22 ; with fire button
                beq shootMissile
                rts

; -------- shoot missile --------  

shootMissile lda enableSprites
             and #2
             cmp #2 
             beq finishShootMissile

             ldx hoverSprite
             cpx #192
             beq shootMissileRight
             bne shootMissileLeft

shootMissileRight jsr missileImgFaceRight

                  lda hoverSpriteX
                  cmp #234
                  bcs finishShootMissile
                 
                  adc #21
                  sta missileSpriteX

                  lda hoverSpriteY
                  sta missileSpriteY
                 
                  lda enableSprites
                  eor #2
                  sta enableSprites
                 
                  rts

shootMissileLeft jsr missileImgFaceLeft

                 lda hoverSpriteX
                 cmp #47
                 bcc finishShootMissile
                
                 sbc #22
                 sta missileSpriteX

                 lda hoverSpriteY
                 sta missileSpriteY
                 
                 lda enableSprites
                 eor #2
                 sta enableSprites
                
finishShootMissile rts

; -------- check joystick --------
         
checkJoystick lda joyStick1
              eor #255
              cmp #8
              beq jumpMoveMoverRight
              cmp #24 ; with fire button
              beq jumpMoveMoverRight
              cmp #4
              beq jumpMoveHoverLeft
              cmp #20 ; with fire button
              beq jumpMoveHoverLeft
              cmp #1
              beq jumpMoveHoverUp
              cmp #17 ; with fire button
              beq jumpMoveHoverUp
              cmp #2
              beq jumpMoveHoverDown
              cmp #18 ; with fire button
              beq jumpMoveHoverDown
              cmp #9
              beq jumpMoveHoverUpRight
              cmp #25 ; with fire button
              beq jumpMoveHoverUpRight
              cmp #5
              beq jumpMoveHoverUpLeft
              cmp #21 ; with fire button
              beq jumpMoveHoverUpLeft
              cmp #10
              beq jumpMoveHoverDownRight
              cmp #26 ; with fire button
              beq jumpMoveHoverDownRight
              cmp #6
              beq jumpMoveHoverDownLeft
              cmp #22 ; with fire button
              beq jumpMoveHoverDownLeft
              jmp floatHoverDown
    
jumpMoveMoverRight     jmp moveHoverRight
jumpMoveHoverLeft      jmp moveHoverLeft
jumpMoveHoverUp        jmp moveHoverUp
jumpMoveHoverDown      jmp moveHoverDown
jumpMoveHoverUpRight   jmp moveHoverUpRight
jumpMoveHoverUpLeft    jmp moveHoverUpLeft
jumpMoveHoverDownRight jmp moveHoverDownRight
jumpMoveHoverDownLeft  jmp moveHoverDownLeft   

; -------- hover floats down --------
                
floatHoverDown jsr detectHoverHitBottomEdge
               lda hoverHitBottomEdge
               cmp #1
               beq finishFloatHoverDown

               ldx hoverSpriteY
               inx
               stx hoverSpriteY
                
finishFloatHoverDown rts
        
; -------- move hover moveHoverRight --------
        
moveHoverRight jsr detectHoverHitRightEdge
               lda hoverHitRightEdge
               cmp #1
               beq finishMoveHoverRight

               jsr hoverImgFaceRight

               ldx hoverSpriteX
               inx
               stx hoverSpriteX
        
finishMoveHoverRight rts
        
; -------- move hover left --------
        
moveHoverLeft jsr detectHoverHitLeftEdge
              lda hoverHitLeftEdge
              cmp #1
              beq finishHoveHoverLeft
            
              jsr hoverImgFaceLeft
            
              ldx hoverSpriteX
              dex
              stx hoverSpriteX
        
finishHoveHoverLeft rts

; -------- move hover up --------
                
moveHoverUp jsr detectHoverHitTopEdge
            lda hoverHitTopEdge
            cmp #1
            beq finishMoveHoverUp

            ldx hoverSpriteY
            dex
            stx hoverSpriteY
    
finishMoveHoverUp rts
        
; -------- move hover down --------
                
moveHoverDown jsr detectHoverHitBottomEdge
              lda hoverHitBottomEdge
              cmp #1
              beq finishMoveHoverDown

              ldx hoverSpriteY
              inx
              stx hoverSpriteY
                
finishMoveHoverDown rts

; -------- move hover up and to the right --------

moveHoverUpRight jsr detectHoverHitTopEdge
                 lda hoverHitTopEdge
                 cmp #1
                 beq finishMoveHoverUpRight

                 jsr detectHoverHitRightEdge
                 lda hoverHitRightEdge
                 cmp #1
                 beq finishMoveHoverUpRight
          
                 jsr hoverImgFaceRight
                 
                 ldx hoverSpriteX
                 inx
                 stx hoverSpriteX

                 ldx hoverSpriteY
                 dex
                 stx hoverSpriteY
                
finishMoveHoverUpRight rts

; -------- move hover up and to the left --------

moveHoverUpLeft jsr detectHoverHitTopEdge
                lda hoverHitTopEdge
                cmp #1
                beq finishMoveHoverUpLeft

                jsr detectHoverHitLeftEdge
                lda hoverHitLeftEdge
                cmp #1
                beq finishMoveHoverUpLeft
          
                jsr hoverImgFaceLeft
               
                ldx hoverSpriteX
                dex
                stx hoverSpriteX

                ldx hoverSpriteY
                dex
                stx hoverSpriteY
                
finishMoveHoverUpLeft rts

; -------- move hover down and to the right --------

moveHoverDownRight jsr detectHoverHitBottomEdge
                   lda hoverHitBottomEdge
                   cmp #1
                   beq finishMoveHoverDownRight
                 
                   jsr detectHoverHitRightEdge
                   lda hoverHitRightEdge
                   cmp #1
                   beq finishMoveHoverDownRight
                 
                   jsr hoverImgFaceRight

                   ldx hoverSpriteX
                   inx
                   stx hoverSpriteX

                   ldx hoverSpriteY
                   inx
                   stx hoverSpriteY
                
finishMoveHoverDownRight rts

; -------- move hover down and to the left --------

moveHoverDownLeft jsr detectHoverHitBottomEdge
                  lda hoverHitBottomEdge
                  cmp #1
                  beq finishMoveHoverDownLeft
                 
                  jsr detectHoverHitLeftEdge
                  lda hoverHitLeftEdge
                  cmp #1
                  beq finishMoveHoverDownLeft
                 
                  jsr hoverImgFaceLeft
                 
                  ldx hoverSpriteX
                  dex
                  stx hoverSpriteX

                  ldx hoverSpriteY
                  inx
                  stx hoverSpriteY
                
finishMoveHoverDownLeft rts
   
; -------- function to detect hover hit left edge --------
        
detectHoverHitLeftEdge lda #0
                       sta hoverHitLeftEdge
                       ldx hoverSpriteX
                       cpx #25
                       beq setHoverHitLeftEdge
                       rts
           
setHoverHitLeftEdge lda #1
                    sta hoverHitLeftEdge
                    rts 
        
; -------- function to detect hover hit right edge --------
        
detectHoverHitRightEdge lda #0
                        sta hoverHitRightEdge
                        ldx hoverSpriteX
                        cpx #255
                        beq setHoverHitRightEdge
                        rts
           
setHoverHitRightEdge lda #1
                     sta hoverHitRightEdge
                     rts
        
; -------- function to detect hover hit top edge --------
        
detectHoverHitTopEdge lda #0
                      sta hoverHitTopEdge
                      ldx hoverSpriteY
                      cpx #51
                      beq setHoverHitTopEdge
                      rts
                  
setHoverHitTopEdge lda #1
                   sta hoverHitTopEdge
                   rts

; -------- function to detect hover hit bottom edge --------
        
detectHoverHitBottomEdge lda #0
                         sta hoverHitBottomEdge
                         ldx hoverSpriteY
                         cpx #228
                         beq setHoverHitBottomEdge
                         rts
                  
setHoverHitBottomEdge lda #1
                      sta hoverHitBottomEdge
                      rts
               
; -------- turn hover right --------
               
hoverImgFaceRight lda #192
                  sta hoverSprite
                  rts

; -------- turn hover left --------
               
hoverImgFaceLeft lda #193
                 sta hoverSprite
                 rts

; -------- turn missile right --------
               
missileImgFaceRight lda #194
                   sta missileSprite
                   rts

; -------- turn missile left --------
               
missileImgFaceLeft lda #195
                  sta missileSprite
                  rts
      
; -------- subroutine to pause motion --------
        
pause    ldy #0
         jsr pause255

pause255 iny
         cpy #255
         bne pause255
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
             
; -------- setup irq delay --------

setupCustomIrq ldx #0
               stx refreshScreen

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
          sta refreshScreen
          jmp irqnor
                                        
; -------- end game --------
                
end     rts

; -------- custom variables --------

hoverHitLeftEdge          .byte 0
hoverHitRightEdge         .byte 0
hoverHitTopEdge           .byte 0
hoverHitBottomEdge        .byte 0
refreshScreen             .byte 0
spriteCollisionDetector   .byte 0
rockSpriteBit             .byte 0 ; bit number
rockSpriteBitPlus2        .byte 0
rockSpriteOffset          .byte 0
rockSpriteXPos            .byte 0

hoverRightImgData .byte $00,$55,$00,$01,$7d,$40,$05,$7f
                  .byte $c0,$07,$7d,$c0,$07,$7d,$f0,$07
                  .byte $ff,$c0,$07,$ff,$c0,$01,$f5,$00
                  .byte $00,$3f,$00,$02,$7d,$80,$02,$96
                  .byte $80,$03,$aa,$c0,$03,$aa,$c0,$03
                  .byte $aa,$c0,$00,$aa,$00,$02,$96,$80
                  .byte $02,$82,$80,$0a,$82,$a0,$0f,$c3
                  .byte $f0,$55,$55,$55,$14,$00,$14,$82
                                    
hoverRightUpsideDownImgData .byte $14,$00,$14,$55,$55,$55,$0f,$c3
                            .byte $f0,$0a,$82,$a0,$02,$82,$80,$02
                            .byte $96,$80,$00,$aa,$00,$03,$aa,$c0
                            .byte $03,$aa,$c0,$03,$aa,$c0,$02,$96
                            .byte $80,$02,$7d,$80,$00,$3f,$00,$01
                            .byte $f5,$00,$07,$ff,$c0,$07,$ff,$c0
                            .byte $07,$7d,$f0,$07,$7d,$c0,$05,$7f
                            .byte $c0,$01,$7d,$40,$00,$55,$00,$82

hoverLeftImgData .byte $00,$55,$00,$01,$7d,$40,$03,$fd
                 .byte $50,$03,$7d,$d0,$0f,$7d,$d0,$03
                 .byte $ff,$d0,$03,$ff,$d0,$00,$5f,$40
                 .byte $00,$fc,$00,$02,$7d,$80,$02,$96
                 .byte $80,$03,$aa,$c0,$03,$aa,$c0,$03
                 .byte $aa,$c0,$00,$aa,$00,$02,$96,$80
                 .byte $02,$82,$80,$0a,$82,$a0,$0f,$c3
                 .byte $f0,$55,$55,$55,$14,$00,$14,$82 
                 
hoverLeftUpsideDownImgData .byte $14,$00,$14,$55,$55,$55,$0f,$c3
                           .byte $f0,$0a,$82,$a0,$02,$82,$80,$02
                           .byte $96,$80,$00,$aa,$00,$03,$aa,$c0
                           .byte $03,$aa,$c0,$03,$aa,$c0,$02,$96
                           .byte $80,$02,$7d,$80,$00,$fc,$00,$00
                           .byte $5f,$40,$03,$ff,$d0,$03,$ff,$d0
                           .byte $0f,$7d,$d0,$03,$7d,$d0,$03,$fd
                           .byte $50,$01,$7d,$40,$00,$55,$00,$82

missileRightImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$0d,$00,$00
                   .byte $01,$40,$00,$09,$55,$70,$25,$55
                   .byte $5c,$09,$55,$70,$01,$40,$00,$0d
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$00
                   .byte $00,$00,$00,$00,$00,$00,$00,$82

missileLeftImgData .byte $00,$00,$00,$00,$00,$00,$00,$00
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

