; bg-ophis.asm - Avik Das
;
; A very simple demo, based on the NES101 tutorial by Michael Martin.
; This demo can be assembled using Ophis 1.0, and runs on FCEUX.
; 
; This demo serves two purposes. Firstly, my goal is to learn NES
; development, which I can only achieve by creating a program.
; Secondly, the entirety of the program will be in one file, in order
; to show at a glance all the different parts of the program.

  ; iNES header

  ; iNES identifier
  .byte "NES",$1A

  .byte $01 ; 1 PRG-ROM page
  .byte $01 ; 1 CHR-ROM page

  .byte $00, $00 ; horizontal mirroring, mapper #0
  .byte $00,$00,$00,$00,$00,$00,$00,$00 ; reserved/filler

  ; PRG-ROM

  .alias sprite $200   ; use page 2 for sprite data
  .alias player sprite ; the first sprite is the player

  .text zp ; zero page
  .org $0000

  .space ab   1 ; whether the A button was pressed before
  .space bb   1 ; whether the B button was pressed before
  .space sel  1 ; whether the B button was pressed before

  .space snd  1 ; whether a low or a high note should be played
  .space ani  1 ; the current frame of the animation
  .space temp 1 ; a temporary variable in an undefined state

  ; Actual program code. We only have one PRG-ROM chip here, so the
  ; origin is $C000.
  .text
  .org $C000

reset:
  sei ; disable interrupts
  cld ; ensure binary mode

  ; wait two VBlanks
* lda $2002
  bpl -
* lda $2002
  bpl -

  ; clear out the RAM
  lda #$00
  ldx #$00
* sta $000,x
  sta $100,x
  sta $200,x
  sta $300,x
  sta $400,x
  sta $500,x
  sta $600,x
  sta $700,x
  inx
  bne -

  ; reset the stack pointer
  ldx #$FF
  txs

  lda #$00
  sta $2000
  sta $2001

  jsr init_graphics
  jsr init_sound
  jsr init_variables

  ; set PPU registers
  lda #%10001000 ; enable NMI on VBlank
                 ; 8x8 sprites
                 ; background pattern table at $0000
                 ;     sprite pattern table at $1000
                 ; name table at $2000
  sta $2000
  lda #%00011110 ; unmodified color intensity
                 ; sprites and backgrounds visible
                 ; sprites and backgrounds not clipped
                 ; color display
  sta $2001

  cli ; enable interrupts

loop: jmp loop ; transfer control to VBlank routines

init_graphics:
  jsr init_sprites
  jsr load_palette
  jsr load_name_tables
  rts

init_sprites:
  ; Clear page 2, used to hold sprite data. If we zeroed out this page,
  ; all the unused sprites would end up in the upper left corner of the
  ; screen. To prevent that, we make sure the Y coordinate of all the
  ; unused sprites are set to $FF, which puts them off screen. For
  ; simplicity, all the data on this page is set to $FF, then
  ; overwritten for the sprites we want to use.
  lda #$FF
  ldx #$00
* sta sprite,x
  inx
  bne -

  ; initialize sprite 0
  lda #$70
  sta player   ; Y coordinate
  lda #$00
  sta player+1 ; tile index
  sta player+2 ; no flip, in front, first palette
  sta player+3 ; X coordinate
  ; initialize sprite 1
  lda #$70
  sta player+4 ; Y coordinate
  lda #$01
  sta player+5 ; tile index
  lda #$00
  sta player+6 ; no flip, in front, first palette
  lda #$08
  sta player+7 ; X coordinate
  ; initialize sprite 2
  lda #$78
  sta player+8  ; Y coordinate
  lda #$02
  sta player+9  ; tile index
  lda #$01
  sta player+10 ; no flip, in front, second palette
  lda #$00
  sta player+11 ; X coordinate
  ; initialize sprite 3
  lda #$78
  sta player+12 ; Y coordinate
  lda #$03
  sta player+13 ; tile index
  lda #$01
  sta player+14 ; no flip, in front, second palette
  lda #$08
  sta player+15 ; X coordinate

  rts

load_palette:
  lda #$3F  ; write the address $3F00 to PPU address port
  sta $2006 ; write the high byte
  lda #$00
  sta $2006 ; write the low byte

  ldx #$00
* lda palette, x ; load data from address (palette_data + x)
  sta $2007           ; write data to PPU
  inx
  cpx #$20            ; loop if x != $20
  bne -

  rts

load_name_tables:
  ; load 1KB of data into the first name table
  ; $2000 and $2400 mirrored, so we can fill $2400 and $2800
  ldy #$00
  ldx #$04
  lda #<bg
  sta $10
  lda #>bg
  sta $11
  lda #$24
  sta $2006
  lda #$00
  sta $2006
* lda ($10),y
  sta $2007
  iny
  bne -
  inc $11
  dex
  bne -

  ; clear out $2800
  ; we're already at $2800
  ldy #$00
  ldx #$04
  lda #$00
* sta $2007
  iny
  bne -
  dex
  bne -

  rts

init_sound:
  lda #%00000011 ; length ctr not enabled
                 ; no delta modulation
                 ; no noise
                 ; no triangle
                 ; yes pulse #2
                 ; yes pulse #1
  sta $4015
  lda #0         ; sweep not enabled
                 ; period = 0
                 ; not negative
                 ; no shift
  sta $4001
  sta $4005
  lda #%01000000 ; 4-frame cycle
                 ; disable frame interrupt
  sta $4017
  rts

init_variables:
  lda #0
  sta ab
  sta bb
  sta sel
  sta snd
  sta ani
  rts

react_to_input:
.scope
  ; reset joypads
  lda #$01
  ldx #$00
  sta $4016
  stx $4016

  lda $4016 ; don't ignore A
  and #1
  beq _not_a

  lda ab
  and #1
  bne +  ; don't switch colors if A was pressed before
  lda #1
  sta ab ; A is now pressed

  lda player+2   ; sprite attributes
  and #%11       ; isolate palette portion
  clc
  adc #2         ; switch to next palette (2 palettes were used)
  and #%11       ; cycle back to 0th palette if necessary
  sta temp       ; store new palette portion
  ; All four sprites need to be changed
  ; TODO: refactor
  lda player+2   ; sprite attributes
  and #%11111100 ; remove palette portion
  ora temp       ; store new palette portion
  sta player+2   ; update sprite
  lda player+6   ; sprite attributes
  and #%11111100 ; remove palette portion
  ora temp       ; store new palette portion
  sta player+6   ; update sprite

  lda temp
  sec
  sbc #1
  and #%1
  pha
  lda temp
  and #%10
  sta temp
  pla
  ora temp
  sta temp

  lda player+10  ; sprite attributes
  and #%11111100 ; remove palette portion
  ora temp       ; store new palette portion
  sta player+10  ; update sprite
  lda player+14  ; sprite attributes
  and #%11111100 ; remove palette portion
  ora temp       ; store new palette portion
  sta player+14  ; update sprite
  jmp +

_not_a:
  lda #0
  sta ab ; A is no longer pressed

* lda $4016 ; don't ignore B
  and #1
  beq _not_b

  lda bb
  and #1
  bne +  ; don't flip if B was pressed before
  lda #1
  sta bb ; B is now pressed

  lda player+2   ; sprite attributes
  clc
  adc #%10000001 ; effectively flip MSB, vertical flip
  and #%11111101
  sta player+2
  lda player+6
  clc
  adc #%10000001
  and #%11111101
  sta player+6
  lda player+10
  clc
  adc #%10000001
  and #%11111101
  sta player+10
  lda player+14
  clc
  adc #%10000001
  and #%11111101
  sta player+14
  lda player+1   ; tile index
  clc
  adc #2
  and #%00000011
  sta player+1
  adc #1
  sta player+5
  adc #1
  and #%00000011
  sta player+9
  adc #1
  sta player+13
  jmp +

_not_b:
  lda #0
  sta bb ; B is no longer pressed

* lda $4016 ; don't ignore SELECT
  and #1
  beq _not_sel

  lda sel
  and #1
  bne +   ; don't flip if SELECT was pressed before
  lda #1
  sta sel ; SELECT is now pressed

  lda player+2   ; sprite attributes
  clc
  adc #%00100000 ; effectively flip bit 2, background priority
  and #%00100000 ; isolate new background priority
  sta temp
  lda player+2
  and #%11011111 ; clear old background priority
  ora temp       ; update new background priority
  sta player+2
  lda player+6
  and #%11011111
  ora temp
  sta player+6
  lda player+10
  and #%11011111
  ora temp
  sta player+10
  lda player+14
  and #%11011111
  ora temp
  sta player+14
  jmp +

_not_sel:
  lda #0
  sta sel ; SELECT is no longer pressed

* lda #0
  sta temp  ; hasn't moved

  lda $4016 ; ignore START
  
  lda $4016  ; don't ignore UP
  and #1
  beq +
  lda player
  cmp #8     ; can't go past top of screen
  beq +
  dec player ; update Y-coordinate
  dec player
  dec player+4
  dec player+4
  dec player+8
  dec player+8
  dec player+12
  dec player+12

  lda #1     ; has moved
  sta temp

* lda $4016  ; don't ignore DOWN
  and #1
  beq +
  lda player
  cmp #222-8 ; can't go past bottom of screen
  beq +
  inc player ; update Y-coordinate
  inc player
  inc player+4
  inc player+4
  inc player+8
  inc player+8
  inc player+12
  inc player+12

  lda #1     ; has moved
  sta temp

* lda $4016    ; don't ignore LEFT
  and #1
  beq +
  lda player+3
  beq +        ; can't past left of screen
  dec player+3 ; update X-coordinate
  dec player+3
  dec player+7
  dec player+7
  dec player+11
  dec player+11
  dec player+15
  dec player+15

  lda #1     ; has moved
  sta temp

* lda $4016    ; don't ignore RIGHT
  and #$01
  beq +
  lda player+3
  cmp #255-7-8
  beq +        ; can't past right of screen
  inc player+3 ; update X-coordinate
  inc player+3
  inc player+7
  inc player+7
  inc player+11
  inc player+11
  inc player+15
  inc player+15

  lda #1     ; has moved
  sta temp

* jsr _update_frame
  rts

_update_frame:
  lda ani
  beq _rest_frame
  and #%1111
  bne _inc_ani
  lda ani
  and #%110000
  bne _change_frame
_rest_frame:
  lda temp
  beq _done
_change_frame:
  lda player+1   ; tile index
  clc
  adc #$4
  and #$F
  sta player+1
  adc #1
  and #$F
  sta player+5
  adc #1
  and #$F
  sta player+9
  adc #1
  and #$F
  sta player+13
_inc_ani:
  inc ani
  lda ani
  and #%111111
  sta ani
_done:
  rts
.scend

snd_low_c:
  pha
  lda #%10000100 ; duty = 2
                 ; loop env/disable length = 0
                 ; env not disabled
                 ; vol/env period = 4
  sta $4000
                 ; middle C has a frequency of about 523 Hz, so the square
                 ; wave needed has a frequency of 261.5 Hz, which
                 ; corresponds to $1AA
  lda #%10101010 ; upper two nibbles of $1AA
  sta $4002
  lda #%00001001 ; length index = 0b00001, corresponds to 127 frames
                 ; upper three bits of $1AA
  sta $4003
  pla
  rts

snd_high_c:
  pha
  lda #%10000110 ; duty = 2
                 ; loop env/disable length = 0
                 ; env not disabled
                 ; vol/env period = 6
  sta $4004
                 ; high C has a frequency of about 2092 Hz, so the square
                 ; wave needed has a frequency of 1046 Hz, which
                 ; corresponds to $06A
  lda #%01101010 ; upper two nibbles of $06A
  sta $4006
  lda #%00001000 ; length index = 0b00001, corresponds to 127 frames
                 ; upper three bits of $06A
  sta $4007
  pla
  rts

play_snd:
.scope
  lda #%00000
  cmp snd
  beq _play_low_c
  lda #%10000
  cmp snd
  beq _play_high_c
  jmp _done_playing
_play_low_c:
  jsr snd_low_c
  jmp _done_playing
_play_high_c:
  jsr snd_high_c
_done_playing:
  inc snd
  lda #%11111
  and snd
  sta snd
  rts
.scend

vblank:
  jsr react_to_input

  ldx #$00  ; Reset VRAM
  stx $2006
  stx $2006

  lda #$00
  sta $2005 ; Write 0 for Horiz. Scroll value
  sta $2005 ; Write 0 for  Vert. Scroll value

  lda #>sprite
  sta $4014    ; move page $200-$2FF into SPR-RAM via DMA
  
  jsr play_snd

  rti
irq   : rti

  ; Palettes
palette:
  ; Background palette, a wide variety of colors
  .byte $0F,$10,$00,$30 ; grays
  .byte $2D,$15,$2A,$22 ; slightly lighter
  .byte $00,$35,$39,$32 ; even lighter
  .byte $30,$30,$30,$30 ; whites
  ; Sprite palette, background is dark blue
  .byte $0C,$31,$1C,$0F ; top of ghost, normal
  .byte $0C,$31,$1C,$16 ; bottom of ghost, normal
  .byte $0F,$26,$08,$30 ; top of ghost, inverted
  .byte $0F,$26,$08,$2C ; bottom of ghost, inverted

bg:
  ; 32x30 (16 bytes per line, 60 lines)
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05
  .byte $01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01
  .byte $02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02,$03,$01,$02
  .byte $04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04
  .byte $05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05,$06,$04,$05

  ; attribute table
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

  .advance $FFFA
  .word vblank, reset, irq

  ; CHR-ROM

  ; This is a Mapper #0 cartridge, so the CHR-ROM is an 8K block of
  ; data mapped directly into the first $2000 bytes of the PPU's
  ; address space. Correspondingly, we begin by setting the origin to
  ; $0000.
  .org $0000

  ; Pattern Table #0: Background

  ; A single, transparent 8x8 tile
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00

  ; a 24x16 cloud, composed of a 3x2 tile grid
  .byte $00,$00,$00,$00,$03,$04,$08,$08
  .byte $00,$00,$00,$03,$07,$0F,$1F,$1F
  .byte $00,$00,$1C,$36,$23,$D2,$6F,$BF
  .byte $00,$1C,$3E,$7F,$FF,$EF,$91,$40
  .byte $00,$00,$00,$00,$00,$E0,$F0,$F0
  .byte $00,$00,$00,$00,$E0,$F0,$78,$B8
  .byte $05,$09,$12,$11,$18,$0F,$00,$00
  .byte $0E,$1E,$3D,$3E,$3F,$1F,$0F,$00
  .byte $7F,$FF,$FF,$FF,$FF,$FF,$00,$00
  .byte $85,$0A,$05,$0A,$05,$FF,$FF,$00
  .byte $F0,$F0,$E8,$F8,$F8,$F0,$00,$00
  .byte $78,$F8,$7C,$BC,$5C,$F8,$F0,$00

  .advance $1000 ; The rest of Pattern Table #0 is blank

  ; Pattern Table #1: Sprites

  ; A ghost, with four frames of walking animation. Each frame is
  ; 16x16, so there are four sprites per frame.
  ;.byte $00,$61,$F0,$F4,$F4,$F0,$F0,$80 ; frame 0
  ;.byte $18,$1E,$0F,$2F,$2F,$0F,$0F,$19
  ;.byte $00,$61,$F0,$D0,$F4,$F0,$70,$00 ; frame 1
  ;.byte $18,$1E,$0F,$0B,$2F,$0F,$0F,$19
  ;.byte $00,$61,$F0,$F4,$F4,$F0,$F0,$80 ; frame 2
  ;.byte $18,$1E,$0F,$2F,$2F,$0F,$0F,$19
  ;.byte $00,$61,$F0,$F4,$D0,$F0,$F0,$80 ; frame 3
  ;.byte $18,$1E,$0F,$2F,$0B,$0F,$0E,$18

  .byte $00,$03,$0F,$0F,$1F,$1F,$1F,$1F ; frame 0
  .byte $03,$0C,$10,$10,$24,$26,$26,$60
  .byte $00,$C0,$F0,$F0,$F8,$F8,$F8,$F8
  .byte $C0,$30,$08,$08,$44,$64,$64,$06
  .byte $5F,$5F,$5F,$1F,$1F,$1F,$19,$00
  .byte $A5,$AA,$A0,$60,$20,$20,$26,$19
  .byte $FA,$FA,$FA,$F8,$F8,$F8,$98,$00
  .byte $55,$A5,$05,$06,$04,$04,$64,$98
  
  .byte $00,$03,$0F,$0F,$1F,$1F,$5F,$5F ; frame 1
  .byte $03,$0C,$10,$10,$24,$66,$A6,$A0
  .byte $00,$C0,$F0,$F0,$F8,$F8,$F8,$F8
  .byte $C0,$30,$08,$08,$44,$64,$64,$04
  .byte $5F,$1F,$1F,$1F,$1B,$01,$00,$00
  .byte $A5,$6A,$20,$20,$24,$1A,$01,$00
  .byte $F8,$F8,$FA,$FA,$FA,$B8,$18,$00
  .byte $54,$A6,$05,$05,$05,$46,$A4,$18

  .byte $00,$03,$0F,$0F,$1F,$1F,$1F,$1F ; frame 2
  .byte $03,$0C,$10,$10,$24,$26,$26,$60
  .byte $00,$C0,$F0,$F0,$F8,$F8,$F8,$F8
  .byte $C0,$30,$08,$08,$44,$64,$64,$06
  .byte $5F,$5F,$5F,$1F,$1F,$1F,$19,$00
  .byte $A5,$AA,$A0,$60,$20,$20,$26,$19
  .byte $FA,$FA,$FA,$F8,$F8,$F8,$98,$00
  .byte $55,$A5,$05,$06,$04,$04,$64,$98

  .byte $00,$03,$0F,$0F,$1F,$1F,$1F,$1F ; frame 3
  .byte $03,$0C,$10,$10,$24,$26,$26,$20
  .byte $00,$C0,$F0,$F0,$F8,$F8,$FA,$FA
  .byte $C0,$30,$08,$08,$44,$66,$65,$05
  .byte $1F,$1F,$5F,$5F,$5F,$1D,$18,$00
  .byte $25,$6A,$A0,$A0,$A0,$62,$25,$18
  .byte $FC,$F8,$F8,$F8,$D8,$80,$00,$00
  .byte $55,$A6,$04,$04,$24,$58,$80,$00
  
  .advance $2000 ; The rest of Pattern Table #1 is blank

; vim: ft=asmM6502:tw=72:sw=2
