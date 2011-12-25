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

  ; XXX Global variables go here
  ; .space name size ; documentation

  ; Global variables related to a level map
  .alias MAX_HEIGHT 8 ; the maximum height any platform can be
  .alias heights $300 ; leave an entire page for the height map
  .org $0400          ; used for caching the entire level

  .alias LEVELW 256 ; the width of level, in columns. Each column is
                    ; 16 pixels wide, for a total of 16 columns per
                    ; screen.
  .space hindex 1   ; the current entry in the height map used to
                    ; populate the next column

  .space leftcol  1 ; the decoded height of the left column of the two
                    ; columns stored in a single height map entry
  .space rghtcol   1 ; same, but for the right column
  .space vramaddrh 1 ; the upper byte of the VRAM address being written
  .space vramaddrl 1 ; the lower byte of the VRAM address being written

  ; lookup tables used by PRNG routines, one per page
  .alias T0 $500
  .alias T1 $600
  .alias T2 $700
  .alias T3 $800

  .org $0900
  ; Global variables for PRNG routines
  .space SEED0 1 ; byte 0 of the seed
  .space SEED1 1 ; byte 1 of the seed
  .space SEED2 1 ; byte 2 of the seed
  .space SEED3 1 ; byte 3 of the seed
  .space TMP   3 ; temporary used by PRNG routines
  .space MOD   1 ; temporary used by PRNG routines

  ; Actual program code. We only have one PRG-ROM chip here, so the
  ; origin is $8000. Mapper #0, mirrors the 16KiB code from $8000-$BFFF
  ; and $C000-$FFFF because only one PRG-ROM page.
  .text
  .org $C000

  .include "../lib/rand-table.asm"

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

  jsr init_random
  jsr init_heights
  jsr init_graphics
  jsr init_sound
  jsr init_variables

  ; set PPU registers
  lda #%10110000 ; enable NMI on VBlank
                 ; 8x16 sprites
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

init_random:
  lda #$00
  sta SEED0
  sta SEED1
  sta SEED2
  sta SEED3
  jsr RNDGENTBLS
  rts

init_heights:
  ; for x in 0 to N/2: # generate two heights at a time
  ;    h1 = rand(MAX_HEIGHT)
  ;    h2 = rand(MAX_HEIGHT)
  ;    h = (h1 << 4) | h2
  ;    store h at (heights+x)
  ldx LEVELW/2
* lda MAX_HEIGHT
  jsr RANDOM8
  asl
  asl
  asl
  asl
  sta heights,x
  lda MAX_HEIGHT
  jsr RANDOM8
  ora heights,x
  sta heights,x

  dex
  bne -

  rts

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

  ; initialize sprite 0 (left half of player)
  lda #$80
  sta player   ; Y coordinate
  lda #$00
  sta player+2 ; no flip, in front, first palette
  sta player+3 ; X coordinate
  lda #$02
  sta player+1 ; tile index

  ; initialize sprite 1 (right half of player)
  lda #$80
  sta player+4 ; Y coordinate
  lda #$00
  sta player+6 ; no flip, in front, first palette
  lda #$08
  sta player+7 ; X coordinate
  lda #$04
  sta player+5 ; tile index

  ; Due to the clearing of page 2, all the other sprites will be
  ; positioned at (0,0), with all flags set to 0. In particular, the
  ; tile index will be 0, so if it's easier to work with sprites where
  ; the tile index is 0-indexed, then the rest of page 2 needs to be set
  ; so that all other tiles at (0,0) are transparent.

  rts

load_palette:
  lda #$3F  ; write the address $3F00 to PPU address port
  sta $2006 ; write the high byte
  lda #$00
  sta $2006 ; write the low byte

  ldx #$00
* lda palette, x ; load data from address (palette_data + x)
  sta $2007      ; write data to PPU
  inx
  cpx #$20       ; loop if x != $20
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
  ; XXX initialize global variables here, usually to 0
  lda #$00
  sta hindex
  rts

decode_heights:
  lda heights,x
  and #%1111
  sta rghtcol
  lda heights,x
  lsr
  lsr
  lsr
  lsr
  sta leftcol
  rts

draw_next_cols:
.scope
  ldx #0
  ; TODO: doesn't work
  jsr decode_heights

  ldy #15
  lda #$20
  sta vramaddrh
  lda #$00
  sta vramaddrl

_yloop_start:
  cpy #0
  beq _yloop_end

  lda vramaddrh
  sta $2006
  lda vramaddrl
  sta $2006

  cpy leftcol
  bpl _leftcol_clear
  lda #$01
  sta $2007
  lda #$02
  sta $2007
  jmp _leftcol_done
_leftcol_clear:
  lda #$00
  sta $2007
  sta $2007
_leftcol_done:

  cpy rghtcol
  bpl _rghtcol_clear
  lda #$01
  sta $2007
  lda #$02
  sta $2007
  jmp _rghtcol_done
_rghtcol_clear:
  lda #$00
  sta $2007
  sta $2007
_rghtcol_done:
  
  lda vramaddrl
  clc
  adc #$40
  sta vramaddrl
  bne _no_incr_vramaddrh
  inc vramaddrh

_no_incr_vramaddrh:
  dey
  jmp _yloop_start

_yloop_end:
  rts
.scend

react_to_input:
  ; reset joypads
  lda #$01
  ldx #$00
  sta $4016
  stx $4016

  ; XXX the key to the entire project. Implement this!

  lda $4016 ; ignore A
  lda $4016 ; ignore B
  lda $4016 ; ignore SELECT
  lda $4016 ; ignore START
  lda $4016 ; ignore UP
  lda $4016 ; ignore DOWN
  lda $4016 ; ignore LEFT
  lda $4016 ; ignore RIGHT

  rts

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

vblank:
  jsr react_to_input
  jsr draw_next_cols

  ldx #$00  ; Reset VRAM
  stx $2006
  stx $2006

  lda #$00
  sta $2005 ; Write 0 for Horiz. Scroll value
  sta $2005 ; Write 0 for  Vert. Scroll value

  lda #>sprite
  sta $4014 ; move page $200-$2FF into SPR-RAM via DMA

irq   : rti ; vblank falls through to here

  ; Palettes
palette:
  ; Background palette, a wide variety of colors
  .byte $0F,$18,$28,$38 ; light crate, yellow
  .byte $0F,$08,$18,$28 ; darker crate, yellow
  .byte $0F,$0B,$19,$3A ; dollar sign, green
  .byte $30,$30,$30,$30 ; whites
  ; Sprite palette, background is dark blue
  .byte $0F,$02,$11,$21 ; player, blue
  .byte $0F,$31,$1C,$16
  .byte $0F,$26,$08,$30
  .byte $0F,$26,$08,$2C

bg:
  ; 32x30 (16 bytes = 16 tiles per line)
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06
  .byte $05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06,$05,$06

  ; attribute table
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA

  .advance $FFFA
  .word vblank, reset, irq

  ; CHR-ROM

  ; This is a Mapper #0 cartridge, so the CHR-ROM is an 8K block of
  ; data mapped directly into the first $2000 bytes of the PPU's
  ; address space. Correspondingly, we begin by setting the origin to
  ; $0000.
  .org $0000

  ; Pattern Table #0: Sprites

  ; Two transparent 8x8 tiles
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00

  ; 16x16 walking person built from 4 8x8 tiles
  .byte $03,$07,$06,$04,$0F,$17,$30,$28
  .byte $00,$03,$03,$03,$00,$0F,$1F,$17
  .byte $2C,$3C,$0C,$0C,$0D,$1A,$1C,$1C ; left-bottom
  .byte $17,$07,$07,$07,$06,$0C,$08,$00
  .byte $80,$C0,$C0,$40,$C0,$E0,$7C,$3C ; right-top
  .byte $00,$80,$80,$80,$00,$C0,$E0,$F8
  .byte $7C,$E0,$60,$60,$30,$B0,$50,$70 ; right-bottom
  .byte $80,$C0,$C0,$C0,$E0,$60,$20,$00
  
  .advance $1000 ; The rest of Pattern Table #0 is blank

  ; Pattern Table #1: Background

  ; A single, transparent 8x8 tile
  .byte $00,$00,$00,$00,$00,$00,$00,$00
  .byte $00,$00,$00,$00,$00,$00,$00,$00

  ; 16x16 crate built from 4 8x8 tiles
  .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE
  .byte $00,$7F,$40,$5F,$5F,$40,$5E,$5D
  .byte $FF,$FF,$FF,$E7,$C7,$8F,$1F,$3F
  .byte $00,$FE,$02,$DA,$BA,$72,$EA,$DA
  .byte $FC,$F8,$F1,$E3,$E7,$FF,$FF,$FF
  .byte $43,$57,$4E,$5C,$5B,$40,$7F,$00
  .byte $7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF
  .byte $82,$7A,$FA,$02,$FA,$02,$FE,$00

  ; 16x16 dollar sign built from 4 8x8 tiles
  .byte $01,$01,$03,$07,$0A,$04,$00,$00
  .byte $01,$01,$03,$07,$0F,$0E,$0F,$07
  .byte $80,$80,$E0,$F0,$70,$00,$00,$00
  .byte $80,$80,$E0,$F0,$70,$00,$00,$C0
  .byte $01,$00,$00,$0E,$0F,$07,$01,$01
  .byte $02,$00,$00,$00,$00,$00,$00,$00
  .byte $40,$A0,$70,$F0,$E0,$C0,$80,$80
  .byte $A0,$50,$00,$00,$00,$00,$00,$00

  .advance $2000 ; The rest of Pattern Table #1 is blank

; vim: ft=asmM6502:tw=72:sw=2
