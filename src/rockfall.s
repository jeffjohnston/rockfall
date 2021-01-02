          *= $4000
        
sprite0 = $7f8
enable  = $d015
color0  = $d027
sp0x    = $d000
sp0y    = $d001
msbx    = $d010
jstick  = $dc00
shouse  = $0340

        jsr $e544
        lda #$0d
        sta sprite0
        lda #1
        sta enable
        lda #2
        sta color0
        ldx #0
        lda #0
        
cleanup sta shouse,x
        inx
        cpx #63
        bne cleanup
        ldx #0
        lda #$ff

build   sta shouse,x
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
                
                 
        
    
    


