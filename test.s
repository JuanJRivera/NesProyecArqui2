; Joel André Alvarado Morales 802-20-8724

.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
; Address trackers
current_oam_address: .res 1

; Args for render_sprite subroutine
render_x: .res 1
render_y: .res 1
render_tile: .res 1

; Animation vars
direction: .res 1
animState: .res 1


; Main code segment for the program
.segment "CODE"

.include "constants.inc"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx PPUCTRL	; disable NMI
  stx PPUMASK 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit PPUSTATUS
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory
  
;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit PPUSTATUS
  bpl vblankwait2

main:
;Se utiliza para llenar el OAM con valores de 0xFF y asi no incurrir en bugs
  clear_oam_memory:
    ldx #0
    loop_init_oamdata:
      lda #$ff ; load byte x of sprite list
      sta SPRITE_BUFFER, x  
      inx
      cpx #255
      bne loop_init_oamdata

  ; load_sprites:
  ;   lda PPUSTATUS ; Check PPUSTATUS
  ;   lda #$00 ; Start @ OAMADDR 0
  ;   sta OAMADDR

  ;   ldx #0
  ;   loop_load_sprites:
  ;     lda sprites, X ; load byte x of sprite list
  ;     sta OAMDATA ; 
  ;     inx
  ;     cpx #64
  ;     bne loop_load_sprites

  load_palettes:
    lda PPUSTATUS
    lda #$3f
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
    @loop:
      lda palettes, x
      sta PPUDATA
      inx
      cpx #$20
      bne @loop
  
  initial_sprite:
  lda #$00
  sta render_tile
  lda #$10
  sta render_x
  sta render_y

  jsr render_sprite

enable_rendering:
  lda #%10000000	; Enable NMI
  sta PPUCTRL
  lda #%00010110; Enable background and sprite rendering in PPUMASK.
  sta PPUMASK

forever:
  jmp forever

nmi:
  lda #$00
  sta PPUSCROLL
  lda #$00
  sta PPUSCROLL
  rti

render_sprite:
  lda PPUSTATUS

  ; Load current oam address
  lda current_oam_address
  sta OAMADDR


  ; Write first tile of selected sprite
  lda render_y
  sta OAMDATA
  lda render_tile
  sta OAMDATA
  lda #$00
  sta OAMDATA
  lda render_x
  sta OAMDATA

  ; Write second tile of selected sprite
  ; Increase render_y by 8
  lda render_y
  clc
  adc #8
  sta OAMDATA
  ; Increase render_tile by 16 (next tile down) 
  lda render_tile
  clc
  adc #16
  sta OAMDATA
  ; Default palette
  lda $00
  sta OAMDATA
  ; Leave x untouched, tile directly under 1st tile has same x coordinate
  lda render_x
  sta OAMDATA

  ; Write third tile of selected sprite (directly right of 1st tile)
  ; render_y is the same as the first tile
  lda render_y
  sta OAMDATA
  ; Increase render_tile by 1 (next tile to the right)
  lda render_tile
  clc
  adc #1
  sta OAMDATA
  ; Default palette
  lda $00
  sta OAMDATA
  ; Increase x by 8
  lda render_x
  clc
  adc #8
  sta OAMDATA

  ; Write fourth tile of selected sprite (directly right of 2nd tile)
  ; render_y is increased by 8
  lda render_y
  clc
  adc #8
  sta OAMDATA
  ; Increase render_tile by 16 + 1 (next tile to the right)
  lda render_tile
  clc
  adc #17
  sta OAMDATA
  ; Default palette
  lda $00
  sta OAMDATA
  ; Increase x by 8
  lda render_x
  clc
  adc #8
  sta OAMDATA

  ; Save new oam address
  lda current_oam_address
  clc
  adc #4
  sta current_oam_address
  rts

palettes:
; background palette
.byte $0F, $16, $13, $37
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00

; sprite palette
.byte $0F, $16, $13, $37
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00

sprites:
; up arrow big
.byte $00, $00, $00, $00
.byte $00, $01, $00, $08
.byte $08, $10, $00, $00
.byte $08, $11, $00, $08

; up arrow small
.byte $00, $02, $00, $10
.byte $00, $03, $00, $18
.byte $08, $12, $00, $10
.byte $08, $13, $00, $18

; down arrow big
.byte $00, $04, $00, $20
.byte $00, $05, $00, $28
.byte $08, $14, $00, $20
.byte $08, $15, $00, $28

; down arrow small
.byte $00, $06, $00, $30
.byte $00, $07, $00, $38
.byte $08, $16, $00, $30
.byte $08, $17, $00, $38

; Character memory
.segment "CHARS"
.incbin "tiles2.chr"