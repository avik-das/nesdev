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
  .space a    1 ; whether the A button was pressed before
  .space snd  1 ; whether a low or a high note should be played
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
  ; clear page 2, used to hold sprite data
  lda #$00
  ldx #$00
* sta sprite,x
  inx
  bne -

  ; initialize sprite 0
  lda #$70
  sta player   ; Y coordinate
  lda #$01
  sta player+1 ; tile index
  lda #$00
  sta player+2 ; no flip, in front, first palette
  sta player+3 ; X coordinate

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
  sta a
  sta snd
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

  lda a
  and #1
  bne +  ; don't switch colors if A was pressed before
  lda #1
  sta a  ; A is now pressed

  lda player+2   ; sprite attributes
  and #%11       ; isolate palette portion
  clc
  adc #1         ; switch to next palette
  and #%11       ; cycle back to 0th palette if necessary
  sta temp       ; store new palette portion
  lda player+2   ; sprite attributes
  and #%11111100 ; remove palette portion
  ora temp       ; store new palette portion
  sta player+2   ; update sprite
  jmp +

_not_a:
  lda #0
  sta a  ; A is no longer pressed

* lda $4016 ; ignore B
  lda $4016 ; ignore SELECT
  lda $4016 ; ignore START
  
  lda $4016  ; don't ignore UP
  and #1
  beq +
  lda player
  cmp #8     ; can't go past top of screen
  beq +
  dec player ; update Y-coordinate
  dec player

* lda $4016  ; don't ignore DOWN
  and #1
  beq +
  lda player
  cmp #$DE   ; can't go past bottom of screen
  beq +
  inc player ; update Y-coordinate
  inc player

* lda $4016    ; don't ignore LEFT
  and #1
  beq +
  lda player+3
  beq +        ; can't past left of screen
  dec player+3 ; update X-coordinate
  dec player+3

* lda $4016    ; don't ignore RIGHT
  and #$01
  beq +
  lda player+3
  cmp #255-9
  beq +        ; can't past right of screen
  inc player+3 ; update X-coordinate
  inc player+3

* rts
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
  .byte $0F,$16,$1A,$12 ; primaries
  .byte $2D,$15,$2A,$22 ; slightly lighter
  .byte $00,$35,$39,$32 ; even lighter
  .byte $30,$30,$30,$30 ; whites
  ; Sprite palette
  .byte $0F,$06,$26,$30 ; red, dark
  .byte $0F,$05,$24,$30 ; red, light
  .byte $0F,$02,$22,$30 ; blue, dark
  .byte $0F,$11,$31,$30 ; blue, light

bg:
  ; 32x30 (16 bytes per line, 60 lines)
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

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

  ; A single, simple 8x8 tile
  .byte $00,$00,$18,$3C,$3C,$18,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00

  .advance $1000 ; The rest of Pattern Table #0 is blank

  ; Pattern Table #1: Sprites

  ; A single, transparent 8x8 tile
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00

  ; A ghost, with four frames of walking animation. Each frame is 8x8.
  .byte $00,$61,$F0,$F4,$F4,$F0,$F0,$80 ; frame 0
  .byte $18,$1E,$0F,$2F,$2F,$0F,$0F,$19
  .byte $00,$61,$F0,$F4,$F4,$F0,$70,$00 ; frame 1
  .byte $18,$1E,$0F,$2F,$2F,$0F,$0F,$19
  .byte $00,$61,$F0,$F4,$F4,$F0,$F0,$80 ; frame 2
  .byte $18,$1E,$0F,$2F,$2F,$0F,$0F,$19
  .byte $00,$61,$F0,$F4,$F4,$F0,$F0,$80 ; frame 3
  .byte $18,$1E,$0F,$2F,$2F,$0F,$0E,$18
  
  .advance $2000 ; The rest of Pattern Table #1 is blank
