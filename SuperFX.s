.include "libSFX.i"

_vram_tile_addr = $0000
_vram_map_addr  = $1000
_max_test_no    = 1
_ypos_start     = 2 
_ypos_start2    = 3 

.macro mWriteText string, xpos, ypos
  .local stra, skip, address
  address = (_vram_map_addr + xpos + (32 * ypos))
    
    ldx #address
    jsr vram_set_address
    
    ldx #.strlen(string)
    ldy #.loword(stra)
    jsr vram_write
  
    bra skip
  stra: .byte string
  skip:
.endmacro

.macro mWriteTextHex xpos, ypos
  .local address
  address = (_vram_map_addr + xpos + (32 * ypos))
    
    ldx #address
    jsr vram_write_text_byte

.endmacro

.macro set_DBR_IO
    phb
    pha
    
    lda #00
    pha
    plb
    pla
.endmacro

.macro restore_DBR
    plb
.endmacro

.macro runTest test, name, xpos, ypos
    .if (.strlen(name) > 0)
        mWriteText name, xpos, ypos
    .endif
    
    set_DBR_IO
    
    jsr resetMC
    ldx #.loword(test)
    jsl start_gsu
    
    restore_DBR
    
    phy
           
    pla
    mWriteTextHex xpos + .strlen(name) + 2, ypos
    
    pla
    mWriteTextHex xpos + .strlen(name), ypos
  
.endmacro



Main:
        ;Copy GSU code to HIRAM
        memcpy HIRAM, __GSUCODE_LOAD__, __GSUCODE_SIZE__

        ;Copy Main code to WRAM
        memcpy EXRAM, __MAINCODE_LOAD__, __MAINCODE_SIZE__
        
        VRAM_memcpy _vram_tile_addr*2, Font, sizeof_Font
        
        ;Copy fast code to DMA regs
        memcpy __DMACODE_RUN__, __DMACODE_LOAD__, $0C ; $4300-$430B
        memcpy __DMACODE_RUN__ + $10, __DMACODE_LOAD__ + $10, __DMACODE_SIZE__ - $10; $4310-$431B
        
        lda #^maincode
        pha
        plb          ; set Data bank
        jsl maincode ; jump to WRAM

.segment "MAINCODE"

maincode:
        
        lda #$01
        sta f:NMITIMEN ; Disable NMI, Enable Joypad read
        
        set_DBR_IO

        ;Screen color
        CGRAM_setcolor_rgb 0, 7,15,31
        CGRAM_setcolor_rgb 1, 31,31,31
        CGRAM_setcolor_rgb 2, 31,31,31
        CGRAM_setcolor_rgb 3, 31,31,31

        restore_DBR
        
        ; Enable BG1 Main layer
        lda #$01
        sta f:TM
        
        ; BG1 Map address
        lda #bgsc(_vram_map_addr*2, SC_SIZE_32X32)
        sta f:BG1SC
        
        RW a16
        lda #$0000
        sta f:_ShowTests
        sta f:_updateTest
        sta f:_testNumber
        sta f:_CacheEn
        sta f:_GSU_CLSR
        sta f:_GSU_MS0
        sta f:_JoyLast
        sta f:_JoyNew
        RW a8
        
        mWriteText "SFX TEST", 0, 0
        mWriteText "VC", 28, 0
        
        mWriteText "PRESS ANY BUTTON", 8, 14

        jsr enableDisplay 
        
        nop
        
_waitVBlank:
        jsr waitVBlank
        jsr readJoypad
        
        lda f:_updateTest
        beq _waitVBlank
        
        jsr runGSUTest
 
        bra _waitVBlank

        
        
; ----------------
vram_set_address:
    pha
    
    set_DBR_IO
    
    lda #$80
    sta VMAINC

    stx VMADDL
    
    restore_DBR
    
    pla
    rts

vram_write:
    lda a:$0,y
    jsr vram_write_val
    iny
    dex
    bne vram_write
    rts
    
vram_write_val:
    set_DBR_IO
    sta VMDATAL
    stz VMDATAH
    restore_DBR
    rts
    
vram_memset:
    jsr vram_set_address
:   sta f:VMDATAL
    sta f:VMDATAH
    dey
    bne :-
    rts

hex_to_char:
    cmp #$0A
    bcc dc ;0-9 check
    add #$41 - $0A ; A-F
    bra hx
dc: add #$30   ; 0-9
hx: 
    rts

vram_write_text_byte:
    jsr vram_set_address
    
    pha
    lsr
    lsr
    lsr
    lsr ; high nibble

    jsr hex_to_char
    jsr vram_write_val
    
    pla
    and #$0F ; low nibble

    jsr hex_to_char
    jsr vram_write_val
    rts

waitVBlank:
:   lda     f:HVBJOY          ;If currently in vblank, wait until flag is down
    bmi     :-
:   lda     f:HVBJOY          ;Wait until vblank flag is up
    bpl     :-
    rts


enableDisplay:
    lda     #inidisp(ON, DISP_BRIGHTNESS_MAX)
    sta     f:INIDISP
    rts


disableDisplay:
    lda     #inidisp(OFF, DISP_BRIGHTNESS_MAX)
    sta     f:INIDISP
    rts

resetMC:
    ; Ver 1 chip freezes with some tests. Reset before every test 
    ; With GSU1+ (ver 4) this also resets the Cache so skip it for those
    lda     GSU_VCR
    cmp     #$01
    bne     :+
    lda     #$00
    sta     GSU_SFR
:
    rts
    
readJoypad:
:   lda f:HVBJOY
    and #$01
    beq :-         ; wait until read start
    
:   lda f:HVBJOY
    and #$01
    bne :-         ; wait until read done

    RW a16

    lda f:JOY1L
    tax
    
    ; newly pressed
    eor f:_JoyLast
    and f:JOY1L
    sta f:_JoyNew
    tay 
    
    txa
    sta f:_JoyLast
    
    lda #$0000
    sta f:_updateTest
    
    lda f:_ShowTests
    bne _checkButtons

    tya
    and #$F0F0     ;any button
    beq :+
    lda #$1
    sta f:_ShowTests
    sta f:_updateTest
:   jmp _joy_done


   
_checkButtons:    
    tya
    and #$2000     ; Select button
    beq _checkLeft
    lda f:_GSU_CLSR
    eor #$1
    sta f:_GSU_CLSR
    lda #$1
    sta f:_updateTest

_checkLeft:
    tya    
    and #$0200    ; Dpad left
    beq _checkRight
    lda f:_testNumber
    cmp #$0000
    bne :+
    lda #_max_test_no ; wrap
    bra :++
:   dec
:   sta f:_testNumber
    lda #$1
    sta f:_updateTest
    
_checkRight:
    tya    
    and #$0100    ; Dpad right
    beq _checkBtnY
    lda f:_testNumber
    cmp #$0001
    bne :+
    lda #$0000  ; wrap
    bra :++
:   inc
:   sta f:_testNumber
    lda #$1
    sta f:_updateTest
    
_checkBtnY:
    tya    
    and #$4000    ; Y button
    beq _checkBtnB
    lda f:_GSU_MS0
    eor #$1
    sta f:_GSU_MS0
    lda #$1
    sta f:_updateTest
    
_checkBtnB:
    tya    
    and #$8000    ; B button
    beq _joy_done
    lda f:_CacheEn
    eor #$1
    sta f:_CacheEn
    lda #$1
    sta f:_updateTest
    
_joy_done:
    RW a8
    rts

runGSUTest:
    ; Copy GSU code to Cart RAM
    memcpy __GSUCODE_RUN__, HIRAM, __GSUCODE_SIZE__
    
    jsr disableDisplay
    
    set_DBR_IO

    ; clear screen
    lda #$00
    ldx #_vram_map_addr+32  ; skip first row
    ldy #(32*32-32)
    jsr vram_memset
    
    restore_DBR
    
    lda     f:_GSU_CLSR
    beq     :+
    mWriteText "21MHZ", 9, 0 
    bra     :++
:   mWriteText "10MHZ", 9, 0 
:

    lda     f:_GSU_MS0
    beq     :+
    mWriteText "MS1", 15, 0 
    bra     :++
:   mWriteText "MS0", 15, 0 
:

    lda     f:_CacheEn
    beq     :+
    mWriteText "CACHE ON", 19, 0 
    bra     :++
:   mWriteText "NO CACHE", 19, 0 
:

    lda     f:GSU_VCR
    mWriteTextHex 30, 0

    ;Configure GSU
    
    lda     #$00
    sta     f:GSU_SFR  ; Stop GSU. Also resets Cache base register (Not on v1 chip)
    
    lda     #$70
    sta     f:GSU_PBR   ; program bank
    
    lda     #$04       ; 70:1000-7FFFF
    sta     f:GSU_SCBR ; Screen base register
    
    lda     #%00011000
    sta     f:GSU_SCMR ; Screen mode register
    
    lda     f:_GSU_MS0
    beq     :+
    lda     #%10100000 ; High speed Multiplier 
    bra     :++
:   lda     #%10000000
:   sta     f:GSU_CFGR ; Config register
    
    lda     f:_GSU_CLSR
    sta     f:GSU_CLSR ; Clock register
    
    jsr     waitVBlank
    
    lda     f:_testNumber
    cmp     #$01
    bne     :+
    jsr     runGSUTests2
    bra     _testDone
;:   cmp     #$02
;    bne     :+
;    jsr     runGSUTests3
;    bra     _testDone

:
    jsr     runGSUTests1 ; else run test 1

_testDone:
    jsr     enableDisplay
    rts

runGSUTests1:
    lda     f:_CacheEn
    beq     :+
    runTest GSU_cache1, "", 0, 60 ; Set Cache base register for code below
:  

    runTest GSU_adc,   "ADC R ", 0, (_ypos_start+0)
    runTest GSU_adci,  "ADC # ", 0, (_ypos_start+1)
    runTest GSU_add,   "ADD R ", 0, (_ypos_start+2)
    runTest GSU_addi,  "ADD # ", 0, (_ypos_start+3)
    runTest GSU_and,   "AND R ", 0, (_ypos_start+4)
    runTest GSU_andi,  "AND # ", 0, (_ypos_start+5)
    runTest GSU_alt1,  "ALT1  ", 0, (_ypos_start+6)
    runTest GSU_alt2,  "ALT2  ", 0, (_ypos_start+7)
    runTest GSU_alt3,  "ALT3  ", 0, (_ypos_start+8)
    runTest GSU_asr,   "ASR   ", 0, (_ypos_start+9)
    runTest GSU_bcc,   "BCC   ", 0, (_ypos_start+10)
    runTest GSU_bcs,   "BCS   ", 0, (_ypos_start+11)
    runTest GSU_beq,   "BEQ   ", 0, (_ypos_start+12)
    runTest GSU_bge,   "BGE   ", 0, (_ypos_start+13)
    runTest GSU_bic,   "BIC R ", 0, (_ypos_start+14)
    runTest GSU_bici,  "BIC # ", 0, (_ypos_start+15)
    runTest GSU_blt,   "BLT   ", 0, (_ypos_start+16)
    runTest GSU_bmi,   "BMI   ", 0, (_ypos_start+17)
    runTest GSU_bne,   "BNE   ", 0, (_ypos_start+18)
    runTest GSU_bpl,   "BPL   ", 0, (_ypos_start+19)
    runTest GSU_bra,   "BRA   ", 0, (_ypos_start+20)
    runTest GSU_bvc,   "BVC   ", 0, (_ypos_start+21)
    
    
    runTest GSU_cmode, "CMODE ", 0, (_ypos_start+23)
    runTest GSU_cmp,   "CMP   ", 0, (_ypos_start+24)
    runTest GSU_color, "COLOR ", 0, (_ypos_start+25)
    
    runTest GSU_dec,   "DEC   ", 11, (_ypos_start+0)
    runTest GSU_div,   "DIV   ", 11, (_ypos_start+1)
    runTest GSU_fmult, "FMULT ", 11, (_ypos_start+2)
    runTest GSU_from,  "FROM  ", 11, (_ypos_start+3)
    
    lda     f:_CacheEn
    beq     :+
    runTest GSU_cache2, "", 0, 60
:    
    runTest GSU_getb1, "GETB1 ", 11, (_ypos_start+4)
    runTest GSU_getb2, "GETB2 ", 11, (_ypos_start+5)
    runTest GSU_getbh, "GETBH ", 11, (_ypos_start+6)
    runTest GSU_getbl, "GETBL ", 11, (_ypos_start+7)
    runTest GSU_getbs, "GETBS ", 11, (_ypos_start+8)
    runTest GSU_getc,  "GETC  ", 11, (_ypos_start+9)
    runTest GSU_hib,   "HIB   ", 11, (_ypos_start+10)
    runTest GSU_ibt,   "IBT   ", 11, (_ypos_start+11)
    runTest GSU_inc,   "INC   ", 11, (_ypos_start+12)
    runTest GSU_iwt,   "IWT   ", 11, (_ypos_start+13)
    runTest GSU_jmp,   "JMP   ", 11, (_ypos_start+14)
    runTest GSU_ldb,   "LDB   ", 11, (_ypos_start+15)
    runTest GSU_ldw,   "LDW   ", 11, (_ypos_start+16)
    runTest GSU_link,  "LINK  ", 11, (_ypos_start+17)
    runTest GSU_lm,    "LM    ", 11, (_ypos_start+18)
    runTest GSU_lms,   "LMS   ", 11, (_ypos_start+19)
    runTest GSU_lmult, "LMULT ", 11, (_ypos_start+20)
    runTest GSU_lob,   "LOB   ", 11, (_ypos_start+21)
    runTest GSU_lsr,   "LSR   ", 11, (_ypos_start+22)
    runTest GSU_merge, "MERGE ", 11, (_ypos_start+23)
    runTest GSU_move,  "MOVE  ", 11, (_ypos_start+24)
    runTest GSU_moves, "MOVES ", 11, (_ypos_start+25)
    
    runTest GSU_mult,  "MULT R", 22, (_ypos_start+0)
    runTest GSU_multi, "MULT #", 22, (_ypos_start+1)
    runTest GSU_nop,   "NOP   ", 22, (_ypos_start+2)
    runTest GSU_not,   "NOT   ", 22, (_ypos_start+3)

    lda     f:_CacheEn
    beq     :+
    runTest GSU_cache3, "", 0, 60
:

    runTest GSU_or,    "OR RN ", 22, (_ypos_start+4)
    runTest GSU_ori,   "OR #  ", 22, (_ypos_start+5)

    lda     f:_CacheEn
    beq     :+
    runTest GSU_cache4, "", 0, 60
:
    runTest GSU_ramb,  "RAMB  ", 22, (_ypos_start+6)
    runTest GSU_rol,   "ROL   ", 22, (_ypos_start+7)
    runTest GSU_romb,  "ROMB  ", 22, (_ypos_start+8)
    runTest GSU_ror,   "ROR   ", 22, (_ypos_start+9)
    runTest GSU_sbc,   "SBC   ", 22, (_ypos_start+10)
    runTest GSU_sbk,   "SBK   ", 22, (_ypos_start+11)
    runTest GSU_sex,   "SEX   ", 22, (_ypos_start+12)
    runTest GSU_sm,    "SM    ", 22, (_ypos_start+13)
    runTest GSU_sms,   "SMS   ", 22, (_ypos_start+14)
    runTest GSU_stb,   "STB   ", 22, (_ypos_start+15)
    runTest GSU_stw,   "STW   ", 22, (_ypos_start+16)
    runTest GSU_sub,   "SUB R ", 22, (_ypos_start+17)
    runTest GSU_subi,  "SUB # ", 22, (_ypos_start+18)
    runTest GSU_swap,  "SWAP  ", 22, (_ypos_start+19)
    runTest GSU_to,    "TO    ", 22, (_ypos_start+20)
    runTest GSU_umult, "UMULT ", 22, (_ypos_start+21)
    runTest GSU_umulti,"UMULT#", 22, (_ypos_start+22)
    runTest GSU_with,  "WITH  ", 22, (_ypos_start+23)
    runTest GSU_xor,   "XOR R ", 22, (_ypos_start+24)
    runTest GSU_xori,  "XOR # ", 22, (_ypos_start+25)
    
    runTest GSU_cache, "CACHE ", 0, (_ypos_start+22)
    
    rts

runGSUTests2:
    mWriteText "4-C  16-C 256-C", 14, 2

    lda     f:_CacheEn
    beq     :+
    runTest GSU_cache3, "", 0, 60
:
    
    ; 4-color
    runTest GSU_plot1,  "PLOT #00      ", 0, (_ypos_start2+0)
    runTest GSU_plot2,  "PLOT #A1      ", 0, (_ypos_start2+1)
    runTest GSU_plot3,  "PLOT #F0      ", 0, (_ypos_start2+2)
    runTest GSU_plot4,  "PLOT #FC      ", 0, (_ypos_start2+3)
    runTest GSU_plot5,  "PLOT #F0 B3   ", 0, (_ypos_start2+4)
    runTest GSU_plot6,  "PLOT #00 B0   ", 0, (_ypos_start2+5)
    runTest GSU_plot7,  "PLOT #A2 B0   ", 0, (_ypos_start2+6)
    runTest GSU_plot8,  "PLOT #A3 B1   ", 0, (_ypos_start2+7)
    runTest GSU_plot9,  "PLOT #A4 B4   ", 0, (_ypos_start2+8)
    runTest GSU_plot10, "PLOT #A5 B4+1 ", 0, (_ypos_start2+9)
    runTest GSU_plot11, "PLOT #00 TILE ", 0, (_ypos_start2+10)
    runTest GSU_plot12, "PLOT #A6 TILE ", 0, (_ypos_start2+11)
    runTest GSU_plot13, "PLOT #00 RPIX ", 0, (_ypos_start2+12)
    runTest GSU_plot14, "PLOT #A7 RPIX ", 0, (_ypos_start2+13)
    runTest GSU_plot15, "PLOT8#00 RPIX ", 0, (_ypos_start2+14)
    runTest GSU_plot16, "PLOT8#A8 RPIX ", 0, (_ypos_start2+15)
    runTest GSU_plot17, "PLOT #A9 NOPS ", 0, (_ypos_start2+16)
    runTest GSU_rpix1,  "RPIX          ", 0, (_ypos_start2+17)
    runTest GSU_rpix2,  "RPIX INC R1   ", 0, (_ypos_start2+18)
   
    
    ; 16-color
    lda     #%00011001
    sta     f:GSU_SCMR
    runTest GSU_plot1,  "", 19, (_ypos_start2+0)
    runTest GSU_plot2,  "", 19, (_ypos_start2+1)
    runTest GSU_plot3,  "", 19, (_ypos_start2+2)
    runTest GSU_plot4,  "", 19, (_ypos_start2+3)
    runTest GSU_plot5,  "", 19, (_ypos_start2+4)
    runTest GSU_plot6,  "", 19, (_ypos_start2+5)
    runTest GSU_plot7,  "", 19, (_ypos_start2+6)
    runTest GSU_plot8,  "", 19, (_ypos_start2+7)
    runTest GSU_plot9,  "", 19, (_ypos_start2+8)
    runTest GSU_plot10, "", 19, (_ypos_start2+9)
    runTest GSU_plot11, "", 19, (_ypos_start2+10)
    runTest GSU_plot12, "", 19, (_ypos_start2+11)
    runTest GSU_plot13, "", 19, (_ypos_start2+12)
    runTest GSU_plot14, "", 19, (_ypos_start2+13)
    runTest GSU_plot15, "", 19, (_ypos_start2+14)
    runTest GSU_plot16, "", 19, (_ypos_start2+15)
    runTest GSU_plot17, "", 19, (_ypos_start2+16)
    runTest GSU_rpix1,  "", 19, (_ypos_start2+17)
    runTest GSU_rpix2,  "", 19, (_ypos_start2+18)
    
     ; 256-color
    lda     #%00011011
    sta     f:GSU_SCMR
    runTest GSU_plot1,  "", 24, (_ypos_start2+0)
    runTest GSU_plot2,  "", 24, (_ypos_start2+1)
    runTest GSU_plot3,  "", 24, (_ypos_start2+2)
    runTest GSU_plot4,  "", 24, (_ypos_start2+3)
    runTest GSU_plot5,  "", 24, (_ypos_start2+4)
    runTest GSU_plot6,  "", 24, (_ypos_start2+5)
    runTest GSU_plot7,  "", 24, (_ypos_start2+6)
    runTest GSU_plot8,  "", 24, (_ypos_start2+7)
    runTest GSU_plot9,  "", 24, (_ypos_start2+8)
    runTest GSU_plot10, "", 24, (_ypos_start2+9)
    runTest GSU_plot11, "", 24, (_ypos_start2+10)
    runTest GSU_plot12, "", 24, (_ypos_start2+11)
    runTest GSU_plot13, "", 24, (_ypos_start2+12)
    runTest GSU_plot14, "", 24, (_ypos_start2+13)
    runTest GSU_plot15, "", 24, (_ypos_start2+14)
    runTest GSU_plot16, "", 24, (_ypos_start2+15)
    runTest GSU_plot17, "", 24, (_ypos_start2+16)
    runTest GSU_rpix1,  "", 24, (_ypos_start2+17)
    runTest GSU_rpix2,  "", 24, (_ypos_start2+18)
    
    runTest GSU_ljmp,  "LJMP ", 0, (_ypos_start+21)
    
    runTest GSU_cache_off, "", 0, 60
    
    rts
    
; Run code fast in DMA regs
.segment "DMACODE"
start_gsu:
        ldy #$0000      ; 3
        
        stx     GSU_R15 ; 3 - GSU start
        bra     _loop   ; 2
        
_dey:   dey             ; 1 - Timeout: back to $FFFF
_end:   rtl             ; 1 - return with cycle count in Y

.res 16-3-3-2-1-1, $00  ; pad to $430F

;$4310
_loop:  iny             ; 1
        beq     _dey    ; 2

        lda     GSU_SFR ; 3
        and     #$20    ; 2
        bne     _loop   ; 2
        bra     _end    ; 2

.segment "LORAM"
_ShowTests: .res 2
_updateTest: .res 2
_testNumber: .res 2
_CacheEn:   .res 2
_GSU_CLSR:  .res 2
_GSU_MS0:   .res 2
_JoyLast:   .res 2
_JoyNew :   .res 2

;Import graphics
.segment "RODATA"
incbin  Font,        "font.bin"