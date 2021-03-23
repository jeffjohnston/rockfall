                     *= $4000

v                     = 53248
irqvec                = $314 ; 788
irqnor                = $ea31 ; 59953 standard irq routine

chrout                = $ffd2
textColor             = $0286
plot                  = $fff0
linprt                = $bdcd
random                = $d41b ; 53299
clearScreen           = $e544 ; clear screen       
enableSprites         = $d015 ; 53269 enable sprites
enableMultiSprites    = $d01C ; 53276 enable multi-color sprites
spriteCollision       = $d01e ; 53278
graphics              = $d018 ; 53272
zeroPageScreenLoByte  = $fb
zeroPageScreenHiByte  = $fc
zeroPageColourLoByte  = $fd
zeroPageColourHiByte  = $fe
zeroPageLineLoByte    = $b2
zeroPageLineHiByte    = $b3

screenMountainLine1   = 1104
screenMountainLine2   = 1144
screenMountainLine3   = 1184
screenMountainLine4   = 1224
screenMountainLine5   = 1264
screenMountainLine6   = 1304
screenMountainLine7   = 1344
screenMountainLine8   = 1384
screenMountainLine9   = 1424
screenMountainLine10  = 1464
screenMountainLine11  = 1504

colourMountainLine1   = 55376
colourMountainLine2   = 55416
colourMountainLine3   = 55456
colourMountainLine4   = 55496
colourMountainLine5   = 55536
colourMountainLine6   = 55576
colourMountainLine7   = 55616
colourMountainLine8   = 55656
colourMountainLine9   = 55696
colourMountainLine10  = 55736
colourMountainLine11  = 55776

screenHouseLine1      = 1784
screenHouseLine2      = 1824
screenHouseLine3      = 1864
screenHouseLine4      = 1904
screenHouseLine5      = 1944

colourHouseLine1      = 56056
colourHouseLine2      = 56096
colourHouseLine3      = 56136
colourHouseLine4      = 56176
colourHouseLine5      = 56216

screenDivideLine1     = 1704
colourDivideLine1     = 55976

screenLifeMeterLine1  = 1259
screenLifeMeterLine2  = 1299
screenLifeMeterLine3  = 1339
screenLifeMeterLine4  = 1379
screenLifeMeterLine5  = 1419
screenLifeMeterLine6  = 1459
screenLifeMeterLine7  = 1499
screenLifeMeterLine8  = 1539
screenLifeMeterLine9  = 1579
screenLifeMeterLine10 = 1619
screenLifeMeterLine11 = 1659
screenLifeMeterLine12 = 1699

colourLifeMeterLine1  = 55531
colourLifeMeterLine2  = 55571
colourLifeMeterLine3  = 55611
colourLifeMeterLine4  = 55651
colourLifeMeterLine5  = 55691
colourLifeMeterLine6  = 55731
colourLifeMeterLine7  = 55771
colourLifeMeterLine8  = 55811
colourLifeMeterLine9  = 55851
colourLifeMeterLine10 = 55891
colourLifeMeterLine11 = 55931
colourLifeMeterLine12 = 55971

characterSet0         = $d000 ; 53248
characterSet1         = $d100 ; 53504
characterSet2         = $d200 ; 53760
characterSet3         = $d300 ; 54016
characterSet4         = $d400 ; 54272 ; custom characters in reverse character section

characterMem0         = $2000 ; 8192
characterMem1         = $2100 ; 8448
characterMem2         = $2200 ; 8704
characterMem3         = $2300 ; 8960
characterMem4         = $2400 ; 9216

hoverSprite           = $7f8  ; 2040
hoverColor            = $d027 ; 53287
hoverSpriteX          = $d000 ; 53248
hoverSpriteY          = $d001 ; 53249
hoverRightImg         = $3000 ; 12288 block 192 (64*192=12288)
hoverLeftImg          = $3040 ; 12352 block 193
 
missileSprite         = $7f9  ; 2041
missileColor          = $d028 ; 53288
missileSpriteX        = $d002 ; 53250
missileSpriteY        = $d003 ; 53251
missileRightImg       = $3080 ; 12416 block 194
missileLeftImg        = $30C0 ; 12480 block 195

rockSprite1           = $7fa  ; 2042
rockSprite1X          = $d004 ; 53252
rockSprite1Y          = $d005 ; 53253
rockColor1            = $d029 ; 53289

rockSprite2           = $7fb  ; 2043
rockSprite2X          = $d006 ; 53254
rockSprite2Y          = $d007 ; 53255
rockColor2            = $d02a ; 53290

rockSprite3           = $7fc  ; 2044
rockSprite3X          = $d008 ; 53256
rockSprite3Y          = $d009 ; 53257
rockColor3            = $d02b ; 53291

rockSprite4           = $7fd  ; 2045
rockSprite4X          = $d00a ; 53258
rockSprite4Y          = $d00b ; 53259
rockColor4            = $d02c ; 53292

rockSprite5           = $7fe  ; 2046
rockSprite5X          = $d00c ; 53260
rockSprite5Y          = $d00d ; 53261
rockColor5            = $d02d ; 53293

rockImg               = $3100 ; 12544 block 196

mostSigBitX           = $d010 ; 53264
joyStick1             = $dc01 ; 56321

; -------- setup --------

setup jsr clearScreen
      jsr setupCustomIrq
      jsr setupCharacterSet
    
      lda #$00
      sta 53280 ; border color
      lda #$06
      sta 53281 ; background color
    
      lda #0
      sta textColor

      lda #0
      sta populationSaved
      sta populationSaved+1
    
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
      
      lda #80
      sta lifeMeterCounter    
      
      lda #0
      sta lifeMeterSubCounter  

      lda #0
      sta lastRockHoverHit  

      jsr setupImages
      jsr drawMountains
      jsr drawHouses
      jsr drawDivide
      jsr drawLifeMeter

; -------- game loop --------
        
gameloop lda refreshScreen
         cmp #1
         bne gameloop
         
         lda #0
         sta refreshScreen
         
         ; will not have to do this with game over routine
         lda lifeMeterCounter
         beq gameloop        

         lda spriteCollision 
         sta spriteCollisionDetector ; copy collision

         jsr moveMissile
         jsr moveRocks
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
          lda #5
          sta rockSpriteBitPlus1
          lda #6
          sta rockSpriteBitPlus2
          lda #0
          sta rockSpriteOffset
          lda #36
          sta rockSpriteXPos         

          jsr moveRock
          
          lda #8
          sta rockSpriteBit
          lda #9
          sta rockSpriteBitPlus1
          lda #10
          sta rockSpriteBitPlus2
          lda #2
          sta rockSpriteOffset
          lda #82
          sta rockSpriteXPos
          
          jsr moveRock

          lda #16
          sta rockSpriteBit
          lda #17
          sta rockSpriteBitPlus1
          lda #18
          sta rockSpriteBitPlus2
          lda #4
          sta rockSpriteOffset
          lda #128
          sta rockSpriteXPos
          
          jsr moveRock

          lda #32
          sta rockSpriteBit
          lda #33
          sta rockSpriteBitPlus1
          lda #34
          sta rockSpriteBitPlus2
          lda #6
          sta rockSpriteOffset
          lda #174
          sta rockSpriteXPos
          
          jsr moveRock

          lda #64
          sta rockSpriteBit
          lda #65
          sta rockSpriteBitPlus1
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
               
finishRock rts
              
rockHitHover lda lastRockHoverHit
             cmp rockSpriteBit
             beq moveRockDown

             lda rockSpriteBit
             sta lastRockHoverHit
             jsr decrementLifeMeter
             jsr decrementLifeMeter
             jsr decrementLifeMeter
             jsr decrementLifeMeter
             jsr decrementLifeMeter

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
              
              jsr decrementLifeMeter

              lda lastRockHoverHit
              cmp rockSpriteBit
              bne clearLastRockHitHover

              rts
                        
checkRockForCollision lda spriteCollisionDetector
                      and rockSpriteBitPlus1
                      cmp rockSpriteBitPlus1
                      beq rockHitHover

                      lda spriteCollisionDetector
                      and rockSpriteBitPlus2
                      cmp rockSpriteBitPlus2
                      bne moveRockDown
                      
                      lda enableSprites ; remove rock and missile
                      eor rockSpriteBitPlus2
                      sta enableSprites
                      
                      jsr incrementPopulationSaved

                      rts

clearLastRockHitHover lda #0
                      sta lastRockHoverHit

                      rts

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
      
; -------- increment population saved --------
      
incrementPopulationSaved inc populationSaved
                         beq incrementPopulationSavedHiByte

finishPopulationSaved clc
                      jmp printPopulationSaved

incrementPopulationSavedHiByte inc populationSaved+1
                               bne finishPopulationSaved
                               lda #$ff
                               sta populationSaved
                               sta populationSaved+1
                               sec
                               jmp printPopulationSaved 

printPopulationSaved ldx #2
                     ldy #32
                     jsr $e50c
      
                     ldx populationSaved
                     lda populationSaved+1
                     jsr linprt
                     rts
             
; -------- decrement life meter --------
               
decrementLifeMeter lda lifeMeterCounter
                   cmp #09
                   bcc jumpLifeMeterSection10
                   cmp #17
                   bcc jumpLifeMeterSection9
                   cmp #25
                   bcc jumpLifeMeterSection8
                   cmp #33
                   bcc jumpLifeMeterSection7
                   cmp #41
                   bcc jumpLifeMeterSection6
                   cmp #49
                   bcc jumpLifeMeterSection5
                   cmp #57
                   bcc jumpLifeMeterSection4
                   cmp #65
                   bcc jumpLifeMeterSection3
                   cmp #73
                   bcc jumpLifeMeterSection2
                   cmp #81
                   bcc jumpLifeMeterSection1
                   rts

jumpLifeMeterSection1  jmp lifeMeterSection1
jumpLifeMeterSection2  jmp lifeMeterSection2
jumpLifeMeterSection3  jmp lifeMeterSection3
jumpLifeMeterSection4  jmp lifeMeterSection4
jumpLifeMeterSection5  jmp lifeMeterSection5
jumpLifeMeterSection6  jmp lifeMeterSection6
jumpLifeMeterSection7  jmp lifeMeterSection7
jumpLifeMeterSection8  jmp lifeMeterSection8
jumpLifeMeterSection9  jmp lifeMeterSection9
jumpLifeMeterSection10 jmp lifeMeterSection10

lifeMeterSection1  lda #<screenLifeMeterLine2+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine2+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine2+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine2+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection2  lda #<screenLifeMeterLine3+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine3+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine3+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine3+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection3  lda #<screenLifeMeterLine4+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine4+1
                   sta zeroPageScreenHiByte
 
                   lda #<colourLifeMeterLine4+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine4+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection4  lda #<screenLifeMeterLine5+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine5+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine5+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine5+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection5  lda #<screenLifeMeterLine6+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine6+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine6+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine6+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection6  lda #<screenLifeMeterLine7+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine7+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine7+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine7+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection7  lda #<screenLifeMeterLine8+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine8+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine8+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine8+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection8  lda #<screenLifeMeterLine9+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine9+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine9+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine9+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection9  lda #<screenLifeMeterLine10+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine10+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine10+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine10+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

lifeMeterSection10 lda #<screenLifeMeterLine11+1
                   sta zeroPageScreenLoByte
                   lda #>screenLifeMeterLine11+1
                   sta zeroPageScreenHiByte

                   lda #<colourLifeMeterLine11+1
                   sta zeroPageColourLoByte
                   lda #>colourLifeMeterLine11+1
                   sta zeroPageColourHiByte
                  
                   jsr drawLifeMeterSection
                   jmp finishLifeMeter

drawLifeMeterSection ldx lifeMeterSubCounter                  
                     ldy #0
                     lda lifeMeterCharacterList,x
                     sta (zeroPageScreenLoByte),y
    
                     lda lifeMeterSubCounter
                     cmp #7
                     beq lifeMeterBackgroundColor
               
                     lda #08
                     sta (zeroPageColourLoByte),y
                     rts
                     
lifeMeterBackgroundColor lda #06
                         sta (zeroPageColourLoByte),y
                  
                         rts

finishLifeMeter lda lifeMeterCounter
                beq gameOver

                sec                
                sbc #1
                sta lifeMeterCounter
                
                lda lifeMeterCounter
                beq gameOver
                
                clc
                lda lifeMeterSubCounter
                adc #1
                sta lifeMeterSubCounter

                lda lifeMeterSubCounter
                cmp #8
                beq resetLifeMeterSubCounter
                rts

resetLifeMeterSubCounter lda #0
                         sta lifeMeterSubCounter

                         rts

; -------- game over --------

gameOver rts ; this is good enought for now

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
                 
; -------- build images --------

setupImages        ldx #0
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
             
             rts
                                                               
; -------- load custom graphics characters --------

setupCharacterSet   sei
              
                    lda $01
                    and #251
                    sta $01

                    ldx #0
defaultCharacterSet lda characterSet0,x ; get char data
                    sta characterMem0,x ; store in characterMem

                    lda characterSet1,x
                    sta characterMem1,x

                    lda characterSet2,x
                    sta characterMem2,x

                    lda characterSet3,x
                    sta characterMem3,x

                    inx
                    bne defaultCharacterSet

                    ldx #0
customCharacterSet lda customCharacterSetData,x
                   sta characterMem4,x
                   inx
                   cpx #184
                   bne customCharacterSet

                   lda $01
                   ora #04
                   sta $01

                   lda graphics
                   and #240
                   ora #8
                   sta graphics

                   cli
                   
                   rts

; -------- draw mountains --------

drawMountains    ; --- draw mountain line 1 ---
                
                 lda #<screenMountainLine1
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine1
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine1
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine1
                 sta zeroPageColourHiByte

                 lda #<mountainLine1
                 sta zeroPageLineLoByte
                 lda #>mountainLine1
                 sta zeroPageLineHiByte
                 
                 jsr drawMountainLine;

                 ; --- draw mountain line 2 ---
                
                 lda #<screenMountainLine2
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine2
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine2
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine2
                 sta zeroPageColourHiByte

                 lda #<mountainLine2
                 sta zeroPageLineLoByte
                 lda #>mountainLine2
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 3 ---
                
                 lda #<screenMountainLine3
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine3
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine3
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine3
                 sta zeroPageColourHiByte

                 lda #<mountainLine3
                 sta zeroPageLineLoByte
                 lda #>mountainLine3
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 4 ---
                
                 lda #<screenMountainLine4
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine4
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine4
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine4
                 sta zeroPageColourHiByte

                 lda #<mountainLine4
                 sta zeroPageLineLoByte
                 lda #>mountainLine4
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 5 ---
                
                 lda #<screenMountainLine5
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine5
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine5
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine5
                 sta zeroPageColourHiByte

                 lda #<mountainLine5
                 sta zeroPageLineLoByte
                 lda #>mountainLine5
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 6 ---
                
                 lda #<screenMountainLine6
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine6
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine6
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine6
                 sta zeroPageColourHiByte

                 lda #<mountainLine6
                 sta zeroPageLineLoByte
                 lda #>mountainLine6
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 7 ---
                
                 lda #<screenMountainLine7
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine7
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine7
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine7
                 sta zeroPageColourHiByte

                 lda #<mountainLine7
                 sta zeroPageLineLoByte
                 lda #>mountainLine7
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 8 ---
                
                 lda #<screenMountainLine8
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine8
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine8
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine8
                 sta zeroPageColourHiByte

                 lda #<mountainLine8
                 sta zeroPageLineLoByte
                 lda #>mountainLine8
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 9 ---
                
                 lda #<screenMountainLine9
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine9
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine9
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine9
                 sta zeroPageColourHiByte

                 lda #<mountainLine9
                 sta zeroPageLineLoByte
                 lda #>mountainLine9
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 ; --- draw mountain line 10 ---
                
                 lda #<screenMountainLine10
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine10
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine10
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine10
                 sta zeroPageColourHiByte

                 lda #<mountainLine10
                 sta zeroPageLineLoByte
                 lda #>mountainLine10
                 sta zeroPageLineHiByte
                 
                 jsr drawMountainLine;

                 ; --- draw mountain line 11 ---
                
                 lda #<screenMountainLine11
                 sta zeroPageScreenLoByte
                 lda #>screenMountainLine11
                 sta zeroPageScreenHiByte

                 lda #<colourMountainLine11
                 sta zeroPageColourLoByte
                 lda #>colourMountainLine11
                 sta zeroPageColourHiByte

                 lda #<mountainLine11
                 sta zeroPageLineLoByte
                 lda #>mountainLine11
                 sta zeroPageLineHiByte
                
                 jsr drawMountainLine;

                 rts

                
drawMountainLine ldy #0
mountainLine     lda (zeroPageLineLoByte),y
                 sta (zeroPageScreenLoByte),y
               
                 lda #00
                 sta (zeroPageColourLoByte),y                  
                   
                 iny
                 cpy #32
                 bne mountainLine

                 rts

; -------- draw houses --------
            
drawHouses    ; --- draw house line 1 ---
                
              lda #<screenHouseLine1
              sta zeroPageScreenLoByte
              lda #>screenHouseLine1
              sta zeroPageScreenHiByte

              lda #<colourHouseLine1
              sta zeroPageColourLoByte
              lda #>colourHouseLine1
              sta zeroPageColourHiByte

              lda #<houseLine1
              sta zeroPageLineLoByte
              lda #>houseLine1
              sta zeroPageLineHiByte
                
              jsr drawHouseLine;

              ; --- draw house line 2 ---
                
              lda #<screenHouseLine2
              sta zeroPageScreenLoByte
              lda #>screenHouseLine2
              sta zeroPageScreenHiByte

              lda #<colourHouseLine2
              sta zeroPageColourLoByte
              lda #>colourHouseLine2
              sta zeroPageColourHiByte

              lda #<houseLine2
              sta zeroPageLineLoByte
              lda #>houseLine2
              sta zeroPageLineHiByte
                
              jsr drawHouseLine;

              ; --- draw house line 3 ---
                
              lda #<screenHouseLine3
              sta zeroPageScreenLoByte
              lda #>screenHouseLine3
              sta zeroPageScreenHiByte

              lda #<colourHouseLine3
              sta zeroPageColourLoByte
              lda #>colourHouseLine3
              sta zeroPageColourHiByte

              lda #<houseLine3
              sta zeroPageLineLoByte
              lda #>houseLine3
              sta zeroPageLineHiByte
                
              jsr drawHouseLine;

              ; --- draw house line 4 ---
                
              lda #<screenHouseLine4
              sta zeroPageScreenLoByte
              lda #>screenHouseLine4
              sta zeroPageScreenHiByte

              lda #<colourHouseLine4
              sta zeroPageColourLoByte
              lda #>colourHouseLine4
              sta zeroPageColourHiByte

              lda #<houseLine4
              sta zeroPageLineLoByte
              lda #>houseLine4
              sta zeroPageLineHiByte
                
              jsr drawHouseLine;

              ; --- draw house line 5 ---
                
              lda #<screenHouseLine5
              sta zeroPageScreenLoByte
              lda #>screenHouseLine5
              sta zeroPageScreenHiByte

              lda #<colourHouseLine5
              sta zeroPageColourLoByte
              lda #>colourHouseLine5
              sta zeroPageColourHiByte

              lda #<houseLine5
              sta zeroPageLineLoByte
              lda #>houseLine5
              sta zeroPageLineHiByte
                
              jsr drawHouseLine;

              rts


drawHouseLine ldy #0
houseLine     lda (zeroPageLineLoByte),y
              sta (zeroPageScreenLoByte),y
               
              lda #00
              sta (zeroPageColourLoByte),y                  
                   
              iny
              cpy #32
              bne houseLine

              rts
               
; -------- draw divide --------
            
drawDivide      ldx #0
drawDivideLine1 lda divideLine1,x
                sta screenDivideLine1,x
               
                lda #00
                sta colourDivideLine1,x
               
                inx
                cpx #32
                bne drawDivideLine1
                
                rts               

; -------- draw life meter --------
            
drawLifeMeter     ; --- draw life meter line 1 ---
                
                  lda #<screenLifeMeterLine1
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine1
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine1
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine1
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine1
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine1
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 2 ---
                
                  lda #<screenLifeMeterLine2
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine2
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine2
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine2
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine2
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine2
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 3 ---
                
                  lda #<screenLifeMeterLine3
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine3
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine3
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine3
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine3
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine3
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 4 ---
                
                  lda #<screenLifeMeterLine4
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine4
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine4
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine4
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine4
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine4
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 5 ---
                
                  lda #<screenLifeMeterLine5
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine5
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine5
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine5
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine5
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine5
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 6 ---
                 
                  lda #<screenLifeMeterLine6
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine6
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine6
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine6
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine6
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine6
                  sta zeroPageLineHiByte
                  
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 7 ---
                
                  lda #<screenLifeMeterLine7
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine7
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine7
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine7
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine7
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine7
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 8 ---
                
                  lda #<screenLifeMeterLine8
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine8
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine8
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine8
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine8
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine8
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 9 ---
                
                  lda #<screenLifeMeterLine9
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine9
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine9
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine9
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine9
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine9
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 10 ---
                
                  lda #<screenLifeMeterLine10
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine10
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine10
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine10
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine10
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine10
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 11 ---
                
                  lda #<screenLifeMeterLine11
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine11
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine11
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine11
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine11
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine11
                  sta zeroPageLineHiByte
                 
                  jsr drawLifeMeterLine;

                  ; --- draw life meter line 12 ---
                
                  lda #<screenLifeMeterLine12
                  sta zeroPageScreenLoByte
                  lda #>screenLifeMeterLine12
                  sta zeroPageScreenHiByte

                  lda #<colourLifeMeterLine12
                  sta zeroPageColourLoByte
                  lda #>colourLifeMeterLine12
                  sta zeroPageColourHiByte

                  lda #<lifeMeterLine12
                  sta zeroPageLineLoByte
                  lda #>lifeMeterLine12
                  sta zeroPageLineHiByte
                
                  jsr drawLifeMeterLine;

                  rts

                
drawLifeMeterLine ldy #0
lifeMeterLine     lda (zeroPageLineLoByte),y
                  sta (zeroPageScreenLoByte),y
               
                  lda #00
                  sta (zeroPageColourLoByte),y     
                  
                  lda (zeroPageLineLoByte),y
                  cmp #$96
                  bne lifeMeterLineInc
                  
                  lda #08
                  sta (zeroPageColourLoByte),y
                   
lifeMeterLineInc  iny
                  cpy #3
                  bne lifeMeterLine

                  rts

; -------- end game --------
                
end     rts

; -------- custom variables --------

hoverHitLeftEdge        .byte 0
hoverHitRightEdge       .byte 0
hoverHitTopEdge         .byte 0
hoverHitBottomEdge      .byte 0
refreshScreen           .byte 0
spriteCollisionDetector .byte 0
rockSpriteBit           .byte 0 ; bit number
rockSpriteBitPlus1      .byte 0
rockSpriteBitPlus2      .byte 0
rockSpriteOffset        .byte 0
rockSpriteXPos          .byte 0
populationSaved         .byte 0,0
lastRockHoverHit        .byte 0

lifeMeterCounter        .byte 0
lifeMeterSubCounter     .byte 0

mountainLine1  .byte $20,$20,$20,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$20
               .byte $81,$20,$20,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$20

mountainLine2  .byte $20,$20,$20,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$80
               .byte $20,$82,$20,$20,$20,$20,$20,$81
               .byte $20,$20,$20,$20,$20,$20,$20,$20

mountainLine3  .byte $20,$20,$20,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$80,$20
               .byte $20,$20,$82,$20,$20,$20,$80,$20
               .byte $82,$20,$20,$20,$20,$20,$20,$20

mountainLine4  .byte $20,$20,$20,$81,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$80,$20,$20
               .byte $20,$20,$20,$82,$20,$80,$20,$20
               .byte $20,$82,$20,$20,$20,$20,$20,$20

mountainLine5  .byte $20,$20,$80,$20,$82,$20,$20,$20
               .byte $81,$20,$20,$20,$80,$20,$20,$20
               .byte $20,$20,$20,$20,$82,$20,$20,$20
               .byte $20,$20,$81,$20,$20,$20,$20,$20

mountainLine6  .byte $20,$80,$20,$20,$20,$82,$20,$80
               .byte $20,$82,$20,$80,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$82,$20,$20
               .byte $20,$80,$20,$82,$20,$20,$20,$20

mountainLine7  .byte $20,$20,$20,$20,$20,$20,$80,$20
               .byte $20,$20,$82,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$82,$20
               .byte $80,$20,$20,$20,$82,$20,$20,$20

mountainLine8  .byte $20,$20,$20,$20,$20,$80,$20,$20
               .byte $20,$20,$20,$82,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$80
               .byte $20,$20,$20,$20,$20,$82,$20,$20

mountainLine9  .byte $20,$20,$20,$20,$80,$20,$20,$20
               .byte $20,$20,$20,$20,$82,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$80,$20
               .byte $20,$20,$20,$20,$20,$20,$82,$20

mountainLine10 .byte $20,$20,$20,$80,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$82,$20,$20
               .byte $20,$20,$20,$20,$20,$80,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$82

mountainLine11 .byte $20,$20,$80,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$20
               .byte $20,$20,$20,$20,$80,$20,$20,$20
               .byte $20,$20,$20,$20,$20,$20,$20,$20

houseLine1     .byte $20,$20,$20,$20,$20,$20,$85,$86
               .byte $20,$20,$20,$20,$85,$86,$20,$20
               .byte $20,$20,$85,$86,$20,$20,$20,$20
               .byte $85,$86,$20,$20,$20,$20,$20,$20

houseLine2     .byte $20,$20,$20,$20,$20,$20,$83,$84
               .byte $20,$20,$20,$20,$83,$84,$20,$20
               .byte $20,$20,$83,$84,$20,$20,$20,$20
               .byte $83,$84,$20,$20,$20,$20,$20,$20

houseLine3     .byte $20,$20,$20,$85,$86,$20,$20,$20
               .byte $20,$85,$86,$20,$20,$20,$20,$85
               .byte $86,$20,$20,$20,$20,$85,$86,$20
               .byte $20,$20,$20,$85,$86,$20,$20,$20

houseLine4     .byte $20,$20,$20,$83,$84,$20,$85,$86
               .byte $20,$83,$84,$20,$85,$86,$20,$83
               .byte $84,$20,$85,$86,$20,$83,$84,$20
               .byte $85,$86,$20,$83,$84,$20,$20,$20

houseLine5     .byte $20,$20,$20,$20,$20,$20,$83,$84
               .byte $20,$20,$20,$20,$83,$84,$20,$20
               .byte $20,$20,$83,$84,$20,$20,$20,$20
               .byte $83,$84,$20,$20,$20,$20,$20,$20

divideLine1    .byte $20,$64,$64,$64,$64,$64,$64,$64
               .byte $64,$64,$64,$64,$64,$64,$64,$64
               .byte $64,$64,$64,$64,$64,$64,$64,$64
               .byte $64,$64,$64,$64,$64,$64,$64,$20
               
lifeMeterLine1  .byte $88,$87,$8e
lifeMeterLine2  .byte $89,$96,$8d
lifeMeterLine3  .byte $89,$96,$8d
lifeMeterLine4  .byte $89,$96,$8d
lifeMeterLine5  .byte $89,$96,$8d
lifeMeterLine6  .byte $89,$96,$8d
lifeMeterLine7  .byte $89,$96,$8d
lifeMeterLine8  .byte $89,$96,$8d
lifeMeterLine9  .byte $89,$96,$8d
lifeMeterLine10 .byte $89,$96,$8d
lifeMeterLine11 .byte $89,$96,$8d
lifeMeterLine12 .byte $8a,$8b,$8c

lifeMeterCharacterList .byte $95,$94,$93,$92,$91,$90,$8f,$96

customCharacterSetData .byte $01,$02,$06,$0a,$18,$2a,$40,$a0 ; character number 128 $80 left of mountain
                       .byte $00,$00,$00,$00,$18,$3c,$5a,$ff ; character number 129 $81 top of mountain
                       .byte $80,$40,$60,$50,$18,$54,$02,$05 ; character number 130 $82 right of mountain
                       .byte $80,$80,$80,$83,$82,$82,$82,$ff ; character number 131 $83 bottom left side of house
                       .byte $01,$01,$1d,$95,$9d,$81,$81,$ff ; character number 132 $84 bottom right side of house
                       .byte $01,$02,$04,$08,$10,$20,$40,$ff ; character number 133 $85 top left side of house
                       .byte $82,$4c,$2c,$1c,$0c,$04,$02,$ff ; character number 134 $86 top right side of house

                       .byte $00,$00,$00,$00,$00,$00,$00,$ff ; character number 135 $87 top life meter                        
                       .byte $00,$00,$00,$00,$00,$00,$00,$01 ; character number 136 $88 top left life meter
                       .byte $01,$01,$01,$01,$01,$01,$01,$01 ; character number 137 $89 left life meter
                       .byte $01,$00,$00,$00,$00,$00,$00,$00 ; character number 138 $8a bottom left life meter
                       .byte $ff,$00,$00,$00,$00,$00,$00,$00 ; character number 139 $8b bottom life meter                        
                       .byte $80,$00,$00,$00,$00,$00,$00,$00 ; character number 140 $8c bottom right life meter
                       .byte $80,$80,$80,$80,$80,$80,$80,$80 ; character number 141 $8d right life meter
                       .byte $00,$00,$00,$00,$00,$00,$00,$80 ; character number 142 $8e right top life meter

                       .byte $00,$00,$00,$00,$00,$00,$00,$ff ; character number 143 $8f 1 life meter
                       .byte $00,$00,$00,$00,$00,$00,$ff,$ff ; character number 144 $90 2 life meter
                       .byte $00,$00,$00,$00,$00,$ff,$ff,$ff ; character number 145 $91 3 life meter
                       .byte $00,$00,$00,$00,$ff,$ff,$ff,$ff ; character number 146 $92 4 life meter
                       .byte $00,$00,$00,$ff,$ff,$ff,$ff,$ff ; character number 147 $93 5 life meter
                       .byte $00,$00,$ff,$ff,$ff,$ff,$ff,$ff ; character number 148 $94 6 life meter
                       .byte $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; character number 149 $95 7 life meter
                       .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; character number 150 $96 8 life meter

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

