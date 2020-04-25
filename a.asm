SCREENLEFT = $18
SCREENRIGHT = $158
SCREENWIDTH = SCREENRIGHT-SCREENLEFT
SCREENTOP = $33
SCREENBOTTOM = $fb
SCREENHEIGHT = SCREENBOTTOM-SCREENTOP

SPRITEXSIZE = 24
SPRITEYSIZE = 21


MODE_TITLE = 1
MODE_GAME = 3
MODE_GAMEOVER = 5
MODE_EXPLODE = 7 ;on time out
            
            * = 2
gamemode    .byte ?
initflag    .byte ?
scrbufloc   .byte ?
sampflag    .byte ?
sampnib     .byte ?

joy         .byte ?
rawjoy      .byte ?

seed        .byte ?
rept        .byte ?
val         .byte ?
            
            ;title vars
titlex      .byte ?
titley      .byte ?
titledir    .byte ?
titleirqcnt .byte ?
titlescroll .fill 40
titlescrollx
            .byte ?
titlescrollptr
            .word ?
titlescrolldelay
            .byte ?
            
            ;game vars
level       .byte ?
nextlevelcnt
            .byte ?
health      .byte ?
score       .word ?
time        .byte ?
timefrac    .byte ?

cursorxlo   .byte ?
cursorxhi   .byte ?
cursory     .byte ?
cursorcol   .byte ?

enemystat   .fill 3 ;0=inactive, 1=alive, 2=dead
enemytimer  .fill 3
enemyxlo    .fill 3
enemyxhi    .fill 3
enemyy      .fill 3
            
            
            
            
scratch .fill 8
            
lastvar 
            
            .enc "text"
            .cdef "09",0
            .cdef "AZ",$a
            .cdef "az",$24
            .cdef "  ",$3e
            .cdef "..",$3f
            .cdef ",,",$40
            .cdef "!!",$41
            .cdef "??",$42
            .cdef "--",$43
            .cdef "++",$44
            .cdef """""",$45
            .cdef "((",$46
            .cdef "))",$47
            .cdef "[[",$48
            .cdef "]]",$49
            .cdef "//",$4a
            .cdef "''",$4b
            .cdef "::",$4c
            
            * = $1f00
            lda #$7f
            sta $dc0d
            sta $dd0d
            ldx #$ff
            txs
            stx $dc00
            inx
            stx $d01a
            lda #<nmi
            sta $fffa
            lda #>nmi
            sta $fffb
            lda #$35
            sta $01
            lda $dc04
            sta val
            lda $d012
            and #$0f
            sta seed
            stx rept
            stx rawjoy
            
            stx $d011
            bit $d011
            bpl *-3
            bit $d011
            bmi *-3
            stx $d020
            stx $d015
            stx $d01b
            stx $d01d
            stx $d017
            lda $dd00
            and #$fc
            ora #2
            sta $dd00
-           lda bitmapscr,x
            sta screen,x
            lda bitmapscr+$100,x
            sta screen+$100,x
            lda bitmapscr+$200,x
            sta screen+$200,x
            lda bitmapscr+$2e8,x
            sta screen+$2e8,x
            lda bitmapcol,x
            sta $d800,x
            lda bitmapcol+$100,x
            sta $d900,x
            lda bitmapcol+$200,x
            sta $da00,x
            lda bitmapcol+$300,x
            sta $db00,x
            inx
            bne -
            lda #$55
            sta sampnib
            lda #123
            sta $dc04
            stx $dc05
            lda #$11
            sta $dc0e
            lda #SAMP_RIFLSHTS
            sta sampflag
            lda #$0a
            sta cursorcol
            lda #$f0
            sta titlescrolldelay
            lda #$ff
            sta $d012
            lda #<mainirq
            sta $fffe
            lda #>mainirq
            sta $ffff
            lda #1
            sta $d019
            sta $d01a
            cli
            jsr rng
            pha
            lsr
            adc #1
            sta titlex
            pla
            and #$1f
            adc #1
            sta titley
inittitle   ldx #39
            lda #' '
-           sta titlescroll,x
            dex
            bpl -
            jsr testsound+0
            lda #$00
            sta titlescrollx
            lda #<titletext
            sta titlescrollptr
            lda #>titletext
            sta titlescrollptr+1
            lda #$80
            sta initflag
            lda #MODE_TITLE
            sta gamemode
            
gameloop    ldy sampflag
            bmi +
            lda samplist,y
            sta sampptr+1
            lda samplist+1,y
            sta sampptr+2
            lda samplist+2,y
            sta sampendlo+1
            lda samplist+3,y
            sta sampendhi+1
            lda #$ff
            sta sampflag
+           ldx waitflag
waitflag = *+1
-           cpx #$00
            bne dogame
            lda $dc0d
            lsr
            bcc -
            lda sampptr+1
sampendlo   cmp #0
            lda sampptr+2
sampendhi   sbc #0
            bcs -
            sei
            dec $01
sampptr     lda @w0
            inc $01
            cli
            asl sampnib
            bcs _lo
            lsr
            lsr
            lsr
            lsr
            sta $d418
            bpl -
_lo         inc sampnib
            and #$0f
            sta $d418
            inc sampptr+1
            bne -
            inc sampptr+2
            bne -
            
dogame      ldy rawjoy
            lda $dc00
            and $dc01
            eor #$1f
            sta rawjoy
            tya
            eor #$1f
            and rawjoy
            sta joy
            ldy gamemode
            jsr jump
            .word title,game,gameover,explode
            
mainirq     sta irqa+1
            stx irqx+1
            sty irqy+1
            lda #$18
            sta $d016
            lda #$3b
            sta $d011
            lda #$a0
            sta $d018
            lda #$05
            sta $d021
            ldy gamemode
            jsr jump
            .word titleirq,gameirq,gameoverirq,explodeirq
            
jump        pla
            sta scratch
            pla
            sta scratch+1
            lda (scratch),y
            sta (+)+1
            iny
            lda (scratch),y
            sta (+)+2
+           jmp *
            
rng     ldy seed
        dec rept
        bne ++
        dey
        bpl +
        ldy #len(seedtbl)-1
+       sty seed
+       lda val
        beq +
        asl
        beq ++
        bcc ++
+       eor seedtbl,y
+       sta val
        rts
seedtbl .byte $f5,$e7,$cf,$c3,$a9,$8d,$87,$71,$69,$65,$63,$5f,$4d,$2f,$2b,$1d
            
            ;
            ;
            ;
            ; ---------------- TITLE MODE
            ;
            ;
            ;
            
title       lda waitflag
            and #3
            bne _nomove
            ldx titley
            lda titledir
            and #1
            beq _down
            inx
            stx titley
            cpx #SCREENHEIGHT - (6*21)-10
            beq +
            bne ++
_down       dex
            stx titley
            bne ++
+           lda titledir
            eor #1
            sta titledir
+           ldx titlex
            lda titledir
            and #2
            beq _left
            inx
            stx titlex
            cpx #SCREENWIDTH - (4*24)
            beq +
            bne ++
_left       dex
            stx titlex
            bne ++
+           lda titledir
            eor #2
            sta titledir
+           
_nomove     
            lda titlescrolldelay
            beq +
            dec titlescrolldelay
+           lda titlescrollx
            sec
            sbc #2
            and #7
            sta titlescrollx
            bcs _noscroll
            ldx #38
-           lda titlescroll,x
            sta titlescroll+1,x
            dex
            bpl -
            ldy titlescrolldelay
            bne _noscroll
-           lda (titlescrollptr),y
            bpl +
            jsr testsound+0
            lda #$b0
            sta titlescrolldelay
            lda #<titletext
            sta titlescrollptr
            lda #>titletext
            sta titlescrollptr+1
            bne _noscroll
+           sta titlescroll
            inc titlescrollptr
            bne _noscroll
            inc titlescrollptr+1
_noscroll   
            
            jsr testsound+3
            
            lda joy
            and #$10
            bne +
            jmp gameloop
+           jmp initgame


            
titleirq    lda #$0c
            sta $d027
            sta $d027+1
            sta $d027+2
            sta $d027+3
            sta $d027+4
            sta $d027+5
            sta $d027+6
            sta $d027+7
            lda #$0f
            sta $d025
            lda #$01
            sta $d026
            lda #$ff
            sta $d01c
            ;x-wise
            ldx #$00
            stx $d010
            lda titlex
            clc
            adc #SCREENLEFT
            bcc +
            inx
            rol $d010
+           sta $d00e
            sta $d006
            adc #SPRITEXSIZE
            bcc +
            inx
+           cpx #1
            rol $d010
            sta $d00c
            sta $d004
            adc #SPRITEXSIZE
            bcc +
            inx
+           cpx #1
            rol $d010
            sta $d00a
            sta $d002
            adc #SPRITEXSIZE
            bcc +
            inx
+           cpx #1
            rol $d010
            sta $d008
            sta $d000
            lda $d010
            asl
            asl
            asl
            asl
            ora $d010
            sta $d010
            ;y-wise
            lda titley
            clc
            adc #SCREENTOP-1
            sta $d001
            sta $d003
            sta $d005
            sta $d007
            adc #SPRITEYSIZE-3
            sta $d012
            
            lda #$0f
            sta $d015
            lda #5
            sta titleirqcnt
            ldx #(logosprites&$3fff) / $40
            stx screen+$3fb
            inx
            stx screen+$3fa
            inx
            stx screen+$3f9
            inx
            stx screen+$3f8
            
            asl initflag
            bcc +
            jsr undogameover
            ldx #39
-           lda #1
            sta $d800 + (24*40),x
            dex
            bpl -
+           .for i = 0, i < 40, i=i+1
                lda titlescroll+(39-i)
                sta screen+(24*40)+i
            .next
            lda titlescrollx
            sta titlescrollxirq+1
            
            lda #<titlespriteirq
            sta $fffe
            lda #>titlespriteirq
            sta $ffff
mainirqend  inc waitflag
irqend      inc $d019
irqa        lda #$00
irqx        ldx #$00
irqy        ldy #$00
nmi         rti
            
titlespriteirq
            sta irqa+1
            stx irqx+1
            sty irqy+1
            lda titleirqcnt
            lsr
            bcc +
            ldx #$00 ;x=previous sprite set, y=new sprite set
            ldy #$08
            bne ++
+           ldx #$08
            ldy #$00
+           lda $d001,x
            clc
            adc #SPRITEYSIZE
            sta $d001,y
            sta $d003,y
            sta $d005,y
            sta $d007,y
            adc #SPRITEYSIZE-3
            sta $d012
            txa
            lsr
            tax
            tya
            lsr
            tay
            lda screen+$3fb,x
            adc #4
            sta screen+$3fb,y
            adc #1
            sta screen+$3fa,y
            adc #1
            sta screen+$3f9,y
            adc #1
            sta screen+$3f8,y
            lda $d015
            eor #$ff
            sta $d015
            dec titleirqcnt
            bne +
            lda #$f0
            sta $d012
            lda #<titlescrollirq
            sta $fffe
            lda #>titlescrollirq
            sta $ffff
+           jmp irqend
            
titlescrollirq
            sta irqa+1
            stx irqx+1
            sty irqy+1
            
            ldx #6
            dex
            bne *-1
            lda #$7b
            sta $d011
            lda #0
            sta $d021
titlescrollxirq
            lda #0
            sta $d016
            lda #$a8
            sta $d018
            ldx $d012
            lda #$1b
            cpx $d012
            beq *-3
            sta $d011
irqreturn   lda #$ff
            sta $d012
            lda #<mainirq
            sta $fffe
            lda #>mainirq
            sta $ffff
            jmp irqend
            
            
undogameover
            ldx #39
-           lda bitmapscr + (40*8),x
            sta screen + (40*8),x
            lda bitmapscr + (40*9),x
            sta screen + (40*9),x
            lda bitmapscr + (40*10),x
            sta screen + (40*10),x
            lda bitmapscr + (40*11),x
            sta screen + (40*11),x
            lda bitmapscr + (40*12),x
            sta screen + (40*12),x
            lda bitmapscr + (40*13),x
            sta screen + (40*13),x
            lda bitmapscr + (40*14),x
            sta screen + (40*14),x
            lda bitmapcol + (40*8),x
            sta $d800 + (40*8),x
            lda bitmapcol + (40*9),x
            sta $d800 + (40*9),x
            lda bitmapcol + (40*10),x
            sta $d800 + (40*10),x
            lda bitmapcol + (40*11),x
            sta $d800 + (40*11),x
            lda bitmapcol + (40*12),x
            sta $d800 + (40*12),x
            lda bitmapcol + (40*13),x
            sta $d800 + (40*13),x
            lda bitmapcol + (40*14),x
            sta $d800 + (40*14),x
            dex
            bpl -
            rts
            
titletext   .text "You are the last person in your platoon and the enemy forces are advancing rapidly.  It is now your job to protect the secret military instalation.  The situation is dire and if the enemy gains precious United States military secrets, we would lose our tactical advantage in the war.       The enemy will jump out of the underbrush of the jungle.  You must use the joystick sight to kill them.  If you let them get back into the underbrush, they will take a shot at you and take some of your health.  The soldiers will keep coming at you until you are dead, or their invasion force has been depleted.       For every soldier that you kill by firing on them, you get 5 points.  Every time a soldier escapes, it takes some of your health.  The difficulty level increases with every level that is completed. ",$ff

            
            
            
            ;
            ;
            ;
            ; ---------------- GAME MODE
            ;
            ;
            ;
            
            
            
initgame    lda #0
            sta level
            sta score
            sta score+1
            sta cursorxhi
            ldx #2
-           lda #0
            sta enemystat,x
            lda enemytimer,x
            and #$0f
            adc #$01
            sta enemytimer,x
            dex
            bpl -
            lda #14
            sta nextlevelcnt
            lda #$a0
            sta health
            lda #$99
            sta time
            lda #55
            sta timefrac
            lda #SCREENWIDTH/2 - 12
            sta cursorxlo
            lda #SCREENHEIGHT/2 - 10
            sta cursory
            lda #SAMP_BLANK
            sta sampflag
            lda #$80
            sta initflag
            lda #MODE_GAME
            sta gamemode
            jmp gameloop
            
game        
            lda joy
            and #$10
            beq noshot
            lda cursorxlo ;get target in zp
            clc
            adc #SPRITEXSIZE/2
            sta scratch
            lda cursorxhi
            adc #$00
            sta scratch+1
            lda cursory
            adc #SPRITEYSIZE/2
            sta scratch+2
            ldx #2
shotloop    ldy enemystat,x
            dey
            bne _next
            lda scratch
            sec
            sbc enemyxlo,x
            sta scratch+3
            lda scratch+1
            sbc enemyxhi,x
            bne _next
            lda scratch+2
            ;sec
            sbc enemyy,x
            bcc _next
            cmp #SPRITEYSIZE*2
            bcs _next
            lda scratch+3
            cmp #SPRITEXSIZE
            bcs _next
            dec nextlevelcnt
            bne +
            inc level
            lda level
            asl
            asl
            adc #14
            sta nextlevelcnt
+           lda #SAMP_SHOT
            sta sampflag
            lda #2
            sta enemystat,x
            lda #$17
            sta enemytimer,x
            sei
            sed
            lda score
            clc
            adc #5
            sta score
            lda score+1
            adc #0
            sta score+1
            cld
            cli
            bcc noshot
_next       dex
            bpl shotloop
noshot      
            
            
            lda rawjoy
            sta scratch
            
            lda cursory
            lsr scratch
            bcc _noup
            sbc #4
            bcs _noup
            lda #0
_noup       lsr scratch
            bcc _nodown
            adc #3
            cmp #SCREENHEIGHT-21-10
            bcc _nodown
            lda #SCREENHEIGHT-21-10
_nodown     sta cursory
            
            lda cursorxlo
            ldx cursorxhi
            lsr scratch
            bcc _noleft
            sbc #5
            bcs _noleft
            dex
            bpl _noleft
            inx
            txa
_noleft     lsr scratch
            bcc _noright
            adc #4
            bcc +
            inx
+           cmp #<SCREENWIDTH-24
            bcc _noright
            cpx #>SCREENWIDTH-24
            bcc _noright
            lda #<SCREENWIDTH-24
_noright    sta cursorxlo
            stx cursorxhi
            
            
            
            ldx #2
enemyloop   ldy enemystat,x
            beq _inactive
            dey
            beq _active
            dec enemytimer,x
            bne _next
            beq _remove
            
_active     dec enemytimer,x
            bne _next
            lda health
            beq _remove
            sei
            sed
            sec
            sbc #5
            sta health
            cld
            cli
_remove     jsr rng
            and #$1f
            sta enemytimer,x
            lda #0
            sta enemystat,x
            bpl _next
            
_inactive   dec enemytimer,x
            bne _next
            jsr rng
            pha
            jsr rng
            tay
            asl
            bcc _xok
            pla
            cmp #<(SCREENWIDTH-24)
            bcc +
            and #$1f
+           pha
            sec
_xok        lda #$00
            rol
            sta enemyxhi,x
            pla
            sta enemyxlo,x
            tya
            and #$7f
            adc #$10
            sta enemyy,x
            jsr rng
            and #$f
            sta scratch
            lda level
            asl
            asl
            asl
            eor #$ff
            adc #$90
            sbc scratch
            sta enemytimer,x
            inc enemystat,x
_next       dex
            bpl enemyloop
            
            
            dec timefrac
            bne +
            lda #55
            sta timefrac
            lda time
            beq initexplode
            sei
            sed
            sec
            sbc #1
            sta time
            cld
            cli
+           
            
            lda health
            beq initgameover
            jmp gameloop
initexplode lda #$c8
            sta timefrac
            lda #SAMP_RIFLSHTS
            sta sampflag
            lda #MODE_EXPLODE
            sta gamemode
            jmp gameloop
            
initgameover
            lda #0
            sta timefrac
            lda #6
            sta time
            lda #SAMP_MALEYELL
            sta sampflag
            lda #$80
            sta initflag
            lda #MODE_GAMEOVER
            sta gamemode
            jmp gameloop
            
gameirq     lda #$01
            sta $d015
            lda #$fc
            sta $d01c
            lda cursorcol
            sta $d027
            eor #(7 ^ $a)
            sta cursorcol
            lda #2
            sta $d025
            lda #8
            sta $d026
            lda cursory
            clc
            adc #SCREENTOP-1
            sta $d001
            lda cursorxlo
            adc #SCREENLEFT
            sta $d000
            bcs +
            lda cursorxhi
            cmp #$01
+           lda #$00
            rol
            sta $d010
            lda #(cursorsprite&$3fff) / $40
            sta screen+$3f8
            
            ldx #2
-           lda enemystat,x
            beq _noenemy
            cmp #2
            beq _dead
            txa
            asl
            tay
            lda #(enemysprites&$3fff) / $40
            sta screen+$3fa,y
            lda #((enemysprites&$3fff) / $40) + 1
            sta screen+$3fb,y
            lda enemytimer,x
            lsr
            lda #5
            bcc +
            lda #$d
+           sta $d029,y
            sta $d02a,y
            bne +
_dead       txa
            asl
            tay
            lda #(deadsprites&$3fff) / $40
            sta screen+$3fa,y
            lda #((deadsprites&$3fff) / $40) + 1
            sta screen+$3fb,y
            lda #$07
            sta $d029,y
            sta $d02a,y
+           tya
            asl
            tay
            lda enemyy,x
            clc
            adc #SCREENTOP-1
            sta $d005,y
            adc #SPRITEYSIZE
            sta $d007,y
            lda enemyxlo,x
            adc #SCREENLEFT
            sta $d004,y
            sta $d006,y
            lda enemyxhi,x
            bne +
            bcc ++
+           lda $d010
            ora enemyactive,x
            sta $d010
+           lda $d015
            ora enemyactive,x
            sta $d015
_noenemy    dex
            bpl -
            
            
            asl initflag
            bcc +
            jsr undogameover
            ldx #39
-           lda statusbar,x
            sta screen+(24*40),x
            dex
            bpl -
+           
            jsr writestatus
            lda time
            lsr
            lsr
            lsr
            lsr
            sta screen+(24*40)+38
            lda time
            and #$0f
            sta screen+(24*40)+39
            
            lda #8
            sta titlescrollxirq+1
            lda #$f0
            sta $d012
            lda #<titlescrollirq
            sta $fffe
            lda #>titlescrollirq
            sta $ffff
            jmp mainirqend
            
            
writestatus ldx level
            inx
            stx screen+(24*40)+6
            lda score+1
            lsr
            lsr
            lsr
            lsr
            sta screen+(24*40)+15
            lda score+1
            and #$0f
            sta screen+(24*40)+16
            lda score
            lsr
            lsr
            lsr
            lsr
            sta screen+(24*40)+17
            lda score
            and #$0f
            sta screen+(24*40)+18
            lda health
            cmp #$a0
            bcc +
            ldx #1
            stx screen+(24*40)+28
            dex
            stx screen+(24*40)+29
            stx screen+(24*40)+30
            bcs ++
+           and #$0f
            sta screen+(24*40)+30
            lda health
            lsr
            lsr
            lsr
            lsr
            sta screen+(24*40)+29
            lda #0
            sta screen+(24*40)+28
+           
            rts
            
enemyactive .byte $c,$30,$c0


statusbar   .text "LEVEL 1  SCORE 0000  HEALTH 100  TIME 99"
            
            
            ;
            ;
            ;
            ; --------------- GAME OVER MODE
            ;
            ;
            ;
            
            
            
gameover    lda sampptr+2
            cmp sampendhi+1
            bcc +
            lda joy
            and #$10
            bne _igame
            dec timefrac
            bne +
            dec time
            beq _ititle
+           jmp gameloop
_ititle     jmp inittitle
_igame      jmp initgame
            
            
            
gameoverirq lda #$0f
            sta $d022
            lda #$0c
            sta $d023
            lda #$00
            sta $d015
            
            asl initflag
            bcs +
            jmp _noinit
+           ldx #39
            lda #' '
-           sta screen + (40*8),x
            sta screen + (40*9),x
            sta screen + (40*10),x
            sta screen + (40*11),x
            sta screen + (40*12),x
            sta screen + (40*13),x
            sta screen + (40*14),x
            dex
            bpl -
            ldx #4
-           .for i = 0, i < 7, i=i+1
                lda skullscr + (5*i),x
                sta screen + (40*(i+8))+2,x
            .next
            lda #8
            .for i = 0, i < 7, i=i+1
                sta $d800 + (40*(i+8))+2,x
            .next
            dex
            bpl -
            ldx #27
-           lda gameovertext,x
            sta screen + (40*9) + 10,x
            lda gameovertext+28,x
            sta screen + (40*11) + 10,x
            lda gameovertext+56,x
            sta screen + (40*13) + 10,x
            lda #0
            sta $d800 + (40*9) + 10,x
            sta $d800 + (40*11) + 10,x
            sta $d800 + (40*13) + 10,x
            dex
            bpl -
            ldx level
            inx
            stx screen + (40*13) + 23
_noinit     
            jsr writestatus
            
            
            lda #$70
            sta $d012
            lda #<gameoverirq1
            sta $fffe
            lda #>gameoverirq1
            sta $ffff
            jmp mainirqend
            
            
gameoverirq1
            sta irqa+1
            stx irqx+1
            sty irqy+1
            
            ldx #6
            dex
            bne *-1
            lda #$7b
            sta $d011
            lda #1
            sta $d021
            lda #$a8
            sta $d018
            ldx $d012
            lda #$1b
            cpx $d012
            beq *-3
            sta $d011
            lda #$aa
            sta $d012
            lda #<gameoverirq2
            sta $fffe
            lda #>gameoverirq2
            sta $ffff
            jmp irqend
            
            
gameoverirq2
            sta irqa+1
            stx irqx+1
            sty irqy+1
            
            lda #$7b
            ldx #$aa
            cpx $d012
            bne +
            cpx $d012
            bne +
            cpx $d012
            bne +
            cpx $d012
            bne +
            nop
            nop
+           sta $d011
            ldx #5
            ldy #$a0
            stx $d021
            sty $d018
            lda #$3b
            nop
            nop
            sta $d011
            
            lda #8
            sta titlescrollxirq+1
            lda #$f0
            sta $d012
            lda #<titlescrollirq
            sta $fffe
            lda #>titlescrollirq
            sta $ffff
            jmp irqend
            
            
            
gameovertext
            .text "Dead...  You fought well and"
            .text "you will be remembered.  You"
            .text "got to level 0.             "
            
skulls  = binary("skull.iscr")
skullscr    .for i = 0, i < len(skulls), i=i+1
                .if skulls[i]
                    .byte skulls[i] + (skullchars&$7ff)/8 - 1
                .else
                    .byte ' '
                .endif
            .next
            
            
            
            ;
            ;
            ;
            ; --------------- EXPLOSION
            ;
            ;
            ;
explode     dec timefrac
            beq +
            jmp gameloop
+           jmp initgameover
            
explodeirq  lda #$00
            sta $d015
            sta $d011
            lda #13
            sta $d012
            lda #<explodecolirq
            sta $fffe
            lda #>explodecolirq
            sta $ffff
            jmp mainirqend
            
explodecolirq
            sta irqa+1
            stx irqx+1
            sty irqy+1
            lda timefrac
            lsr
            lsr
            lda #0
            bcc +
            lda #2
+           sta $d020
            jmp irqreturn
            
explodecol  .byte $2,$a,$8,$7
            
            
            
            
            * = $2d00
testsound   .binary "testsound.sid",$7e
            
            * = $3800
bitmapscr   .binary "junglebitmap.scr"
            * = $3c00
bitmapcol   .binary "junglebitmap.col"
            * = $4000
bitmap      .binary "junglebitmap.map"
            * = $6000
textchars   .binary "chars.chr"
skullchars  .binary "skull.imap",8
            * = $6800
screen      .fill $400

logospr = binary("logospritescolor.map")
logosprites .for i = 0, i < len(logospr), i=i+1
                v .var 0
                .if logospr[i] & $c0
                    v |= logospr[i] & $c0
                .else
                    v |= $80
                .endif
                .if logospr[i] & $30
                    v |= logospr[i] & $30
                .else
                    v |= $20
                .endif
                .if logospr[i] & $0c
                    v |= logospr[i] & $0c
                .else
                    v |= $08
                .endif
                .if logospr[i] & $03
                    v |= logospr[i] & $03
                .else
                    v |= $02
                .endif
                .byte v
            .next
            
cursorspr = binary("crosshair.map")
cursorsprite
            .for i = 0, i < len(cursorspr), i=i+1
                .byte cursorspr[i] ^ $ff
            .next
            
enemyspr = binary("enemysprite.map")
enemysprites
            .for i = 0, i < len(enemyspr), i=i+1
                v .var 0
                .if enemyspr[i] & $80
                    v |= (enemyspr[i] & $c0) ^ $40
                .else
                    v |= enemyspr[i] & $c0
                .endif
                .if enemyspr[i] & $20
                    v |= (enemyspr[i] & $30) ^ $10
                .else
                    v |= enemyspr[i] & $30
                .endif
                .if enemyspr[i] & $08
                    v |= (enemyspr[i] & $0c) ^ $04
                .else
                    v |= enemyspr[i] & $0c
                .endif
                .if enemyspr[i] & $02
                    v |= (enemyspr[i] & $03) ^ $01
                .else
                    v |= enemyspr[i] & $03
                .endif
                .byte v
            .next
deadsprites .binary "explosion.map"
            
sample  .macro
        .for i = 0, i < len(\1) & $ffffffe, i=i+2
            .byte (\1[i] & $f0) | (\1[i+1] >> 4)
        .next
        .endm
      
SAMP_BLANK = 0
SAMP_MALEYELL = 4
SAMP_RIFLSHTS = 8
SAMP_SHOT = $c

samplist    .word 0,0
            .word maleyell, maleyell+size(maleyell)
            .word riflshts, riflshts+size(riflshts)
            .word shotsamp, shotsamp+size(shotsamp)

maleyell    #sample binary("MALEYELL.raw")
riflshts    #sample binary("RIFLSHTS.raw")
shotsamp    #sample binary("SHOT.raw")
            
            