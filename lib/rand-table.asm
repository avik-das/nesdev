; rand-table.asm - Bruce Clark, Avik Das
; 
; Linear congruential pseudo-random number generator, by Bruce Clark,
; ported to the Ophis assembler. This file contains the PRNG that uses a
; generated multiplication lookup table to significantly speed up number
; generation at cost of space and initial set up time.
; 
; All credit goes to Bruce Clark's article:
;     http://www.6502.org/source/integers/random/random.html
; 
; Usage of this functionality works as follows:
; 
; 1. Reserve space for all the variables listed below.
; 2. Include this file.
; 3. Initialize all four bytes of the seed (see below).
; 4. Call RNDGENTBLS to initialize the multiplication lookup tables.
; 5. Call RANDOM8 to generate an eight-bit random number between 0
;    (inclusive) and the accumulator value (exclusive).
; 
; The following variables are expected to be available:
; 
; * SEED0, SEED1, SEED2, SEED3
;     - SEEDn is byte n of the seed
;     - these should be initialized before the first call to RAND
; * TMP, TMP+1, TMP+2
;     - three bytes
;     - need not be initialized
; * MOD
;     - need not be initialized
; * T0, T1, T2, T3
;     - four lookup tables. Each will take up one page (256 bytes) of
;       space, and it is recommended they start on a page boundary.
; 
; It is recommended, for speed, that all these be stored in page zero,
; but this is not necessary for correct operation.
; 
; TODO: Bruce Clark's page provides a number of other functions, in
;       particular ones to generate 16-bit random numbers, and functions
;       to generate random numbers more uniformly. These should be
;       ported over as well.

; == START CODE ========================================================
; 
; The code style has been retained, and any changes have been
; documented. Changes related to scoping, such as using Ophis's
; anonymous labels ("*") or the use of Ophis directives ".scope" and
; ".scend" have not been noted.

; Calculate SEED = 1664525 * SEED + 1
; 
; Enter with:
;
;   SEED0 = byte 0 of seed
;   SEED1 = byte 1 of seed
;   SEED2 = byte 2 of seed
;   SEED3 = byte 3 of seed
;
; Returns:
;
;   SEED0 = byte 0 of seed
;   SEED1 = byte 1 of seed
;   SEED2 = byte 2 of seed
;   SEED3 = byte 3 of seed
;
; TMP is overwritten
;
; For maximum speed, locate each table on a page boundary
;
; Assuming that (a) SEED0 to SEED3 and TMP are located on page zero, and
; (b) all four tables start on a page boundary:
;
;   Space: 58 bytes for the routine
;          1024 bytes for the tables
;   Speed: JSR RAND takes 94 cycles

RAND:    CLC       ; compute lower 32 bits of:
         LDX SEED0 ; 1664525 * ($100 * SEED1 + SEED0) + 1
         LDY SEED1
         LDA T0,X
         ADC #1
         STA SEED0
         LDA T1,X
         ADC T0,Y
         STA SEED1
         LDA T2,X
         ADC T1,Y
         STA TMP
         LDA T3,X
         ADC T2,Y
         TAY       ; keep byte 3 in Y for now (for speed)
         CLC       ; add lower 32 bits of:
         LDX SEED2 ; 1664525 * ($10000 * SEED2)
         LDA TMP
         ADC T0,X
         STA SEED2
         TYA
         ADC T1,X
         CLC
         LDX SEED3 ; add lower 32 bits of:
         ADC T0,X  ; 1664525 * ($1000000 * SEED3)
         STA SEED3
         RTS

; Generate T0, T1, T2 and T3 tables
;
; A different multiplier can be used by simply replacing the four bytes
; that are commented below
; 
; NOTE: The original version was named GENTBLS. This has been renamed
;       for namespacing reasons.
; NOTE: The original version mentions a modification that speeds up the
;       routine at the cost of one byte. This modification has been
;       applied.

RNDGENTBLS: LDX #0      ; 1664525 * 0 = 0
            STX T0
            STX T1
            STX T2
            STX T3
            CLC
          * LDA T0,X    ; add 1664525 to previous entry to get next entry
            ADC #$0D    ; byte 0 of multiplier
            STA T0+1,X
            LDA T1,X
            ADC #$66    ; byte 1 of multiplier
            STA T1+1,X
            LDA T2,X
            ADC #$19    ; byte 2 of multiplier
            STA T2+1,X
            LDA T3,X
            ADC #$00    ; byte 3 of multiplier
            STA T3+1,X
            INX         ; note: carry will be clear here
            CPX #$FF
            BNE -
            RTS

; Get the next SEED and obtain an 8-bit random number from it
;
; Requires the RAND subroutine
;
; Enter with:
;
;   accumulator = modulus
;
; Exit with:
;
;   accumulator = random number, 0 <= accumulator < modulus
;
; MOD, TMP, TMP+1, and TMP+2 are overwritten
;
; Note that TMP to TMP+2 are only used after RAND is called.
;
.scope
RANDOM8: STA MOD    ; store modulus in MOD
         JSR RAND   ; get next seed
         LDA #0     ; multiply SEED by MOD
         STA TMP+2
         STA TMP+1
         STA TMP
         SEC
         ROR MOD    ; shift out modulus, shifting in a 1
                    ;     (will loop 8 times)
_R8A:    BCC _R8B   ; branch if a zero was shifted out
         CLC        ; add SEED, keep upper 8 bits of product in
                    ;     accumulator
         TAX
         LDA TMP
         ADC SEED0
         STA TMP
         LDA TMP+1
         ADC SEED1
         STA TMP+1
         LDA TMP+2
         ADC SEED2
         STA TMP+2
         TXA
         ADC SEED3
_R8B:    ROR        ; shift product right
         ROR TMP+2
         ROR TMP+1
         ROR TMP
         LSR MOD    ; loop until all 8 bits of MOD have been shifted out
         BNE _R8A
         RTS
.scend
