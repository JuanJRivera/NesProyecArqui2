
;Juan J Rivera Mercado 802-20-8179

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

SPRITE_BUFFER = $0200
available_oam = $0201

OAMADDR   = $2003
OAMDATA   = $2004
OAMDMA    = $4014

CONTROLLER1 = $4016

BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000


BASE_ADDR_Y = $08
TILE_BASE_ADDR = $01
ATTR_BASE_ADDR = $02
BASE_ADDR_X = $0B

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

; Args for update_sprite subroutine
animState: .res 1 ; 0 = Neutral, 1 = Move front, 2 = Move back,
timer: .res 1 ; Timer to control sprite animation
vblank_flag: .res 1 ; Flag to signal when vblank has started
skip_animation_flag: .res 1 ; Flag to skip animation
isMoving: .res 1

; Args for controllers 
pad1: .res 1 ; Controller 1 input 
direction: .res 1
directionChanged: .res 1
offsetFirstsprite: .res 1

; Args for Background subrutine
; These are used for nametable subroutines
NT_ptr: .res 2 ; Pointer to the current nametable
Specific_NT: .res 2 ; Choose the nametable used
Specific_Atributes: .res 2 ; Choose the atributes for the nametable used
MT_PTR: .res 2 ; Pointer to the metatile
Specific_Tile_Write: .res 1 ; Tile to write to the nametable
Byte_To_Extraction: .res 1 ; Byte to extract bits
Bits_From_Byte: .res 1 ; Number of bits to extract from the byte
need_update_nametable: .res 1 ; Flag to signal if the nametable needs to be updated
SCROLL_POSITION_X: .res 1 ; Scroll position for nametable
SCROLL_POSITION_Y: .res 1 ; Scroll position for nametable
Curr_stage: .res 1 ; Current stage of the game

; Main code segment for the program
.segment "CODE"

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
lda #$00
sta current_oam_address

  init_oamdata:
    ldx #0
    loop_init_oamdata:
      lda #$ff ; load byte x of sprite list
      sta SPRITE_BUFFER, x ; store current byte in sprite buffer
      inx
      cpx #255
      bne loop_init_oamdata

    load_null_sprites:
        ldx #0
        loop_load_null_sprites:
            lda #$00
            sta SPRITE_BUFFER, x
            inx
            cpx #8
            bne loop_load_null_sprites
        stx available_oam ; Set available_oam to 8

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


    load_sprites:
      ldy #0
      loop_load_sprites:
          lda ant_sprites, y
          sta render_y
          iny
          lda ant_sprites, y
          sta render_tile
          iny
          iny
          lda ant_sprites, y
          sta render_x
          iny
          jsr render_tile_subroutine
          cpy #(16)
          bne loop_load_sprites

    ; subrutina para cargar los tiles al nametable
    load_nametable:
    ;Load stage 1 for the game
  	  lda #1
      sta Curr_stage
    
;en esta parte del codigo deberia ir el codigo para cargar el primer stage del juego
      ; Select first nametable
        lda #<stage_one_nametable1_packaged  ; aqui se cargan los 60 paquetes de datos referente al primer stage
        sta Specific_NT
        lda #>stage_one_nametable1_packaged
        sta Specific_NT+1

; Choose the atribute table for the specific_nametable
      ; Select first attribute table
        lda #<stage_one_nametable1_attributes ; aqui se cargan los atributos del primer stage
        sta Specific_Atributes
        lda #>stage_one_nametable1_attributes
        sta Specific_Atributes+1


        ; $2000 for first nametable
        lda #$20  ; aqui se carga la direccion de memoria donde se va a escribir el primer stage
        sta NT_ptr ; pointer que nos permitira hacer el offset dentro de memoria
        lda #$00
        sta NT_ptr+1
        jsr write_nametable ; se llama la subrutina para escribir el primer stage

        ; $23C0 for first attribute table
        lda #$23
        sta NT_ptr
        lda #$C0   ; aqui se carga la direccion de memoria donde se va a escribir los atributos del primer stage
        sta NT_ptr+1
        jsr load_attributes

; Aqui deberia ir el codigo para cargar el segundo stage del juego
      ; Select second nametable
        lda #<stage_one_nametable2_packaged
        sta Specific_NT
        lda #>stage_one_nametable2_packaged  ; lo mismo que con el primer nametable pero con el segundo nametable que seria
        sta Specific_NT+1                   ; la parte derecha de la pantalla

        ; Select second attribute table
        lda #<stage_one_nametable2_attributes
        sta Specific_Atributes
        lda #>stage_one_nametable2_attributes
        sta Specific_Atributes+1

        ; $2400 for second nametable
        lda #$24
        sta NT_ptr
        lda #$00                    ;lugar en memoria donde se cargara el segundo nametable
        sta NT_ptr+1
        jsr write_nametable

        ; $27C0 for second attribute table
        lda #$27
        sta NT_ptr                  ; lugar en memoria donde se cargara el segundo atributo
        lda #$C0
        sta NT_ptr+1
        jsr load_attributes

      ; lda PPUSTATUS
      ; lda #>NT_ptr
      ; sta PPUADDR
      ; lda #<NT_ptr
      ; sta PPUADDR

enable_rendering:
  ;scroll position to 0
  lda #$00
  sta PPUSCROLL
  lda #$00
  sta PPUSCROLL

  lda #%10001000	; Enable NMI
  sta PPUCTRL
  lda #%00011110; Enable background and sprite rendering in PPUMASK.
  sta PPUMASK

forever:
  lda vblank_flag
  cmp #1
  bne NMIunsync
    jsr handle_input                    ; se llama a la subrutina para manejar la entrada del jugador que 
    jsr update_player                   ; se encarga de cambiar los stages
    jsr Update_Sprite_logic
    jsr swap_nametable                  ; se llama a la subrutina para cambiar de nametable
  NMIunsync:
    jmp forever

nmi:
  pha
  txa
  pha
  tya
  pha


  lda #1
  sta vblank_flag

  lda #$02 ;load 0200 que es donde empieza el SPRITE_BUFFER
  sta OAMDMA ;store en OAMDATA

  lda timer ; este temporizador nos dira cuando puedo cambiar de animacion
  cmp #30 ; comparo timer con 30
  bne skip_timer_reset; si no es 30 entonces no reseteo el timer

  lda #$00
  sta timer ; reseteo el timer
  jmp scroll_screen_check ; salto a la subrutina de scroll
skip_timer_reset:
  inc timer

scroll_screen_check:
    lda SCROLL_POSITION_X
    cmp #255
    beq skip_scroll_increment             ; si ya llegue a 255 no incremento el scroll porque es el limite en la pantalla

; Increment PPUSCROLL to scroll the screen 
    inc SCROLL_POSITION_X

    skip_scroll_increment:
    lda SCROLL_POSITION_X
    sta PPUSCROLL
    lda SCROLL_POSITION_Y
    sta PPUSCROLL

  pla 
  tay
  pla
  tax
  pla
  
  rti

render_tile_subroutine:
    ldx available_oam ; Offset for OAM buffer

    lda render_y
    sta SPRITE_BUFFER, x
    inx

    lda render_tile
    sta SPRITE_BUFFER, x
    inx

    lda #$20
    sta SPRITE_BUFFER, x
    inx

    lda render_x
    sta SPRITE_BUFFER, x
    inx

    stx available_oam ; Update available_oam to the next available OAM buffer index`

    rts

handle_input:
    lda #$01
    sta CONTROLLER1  ; Latch the controller state
    lda #$00
    sta CONTROLLER1  ; Complete the latch process

    lda #$00
    sta pad1    ; Initialize 'pad' to 0

    ldx #$08   ; Prepare to read 8 buttons
    read_button_loop:
        lda CONTROLLER1       ; Read a button state
        lsr             ; Shift right, moving the button state into the carry
        rol pad1         ; Rotate left through carry, moving the carry into 'pad'
        dex             ; Decrement the count
        bne read_button_loop  ; Continue until all 8 buttons are read

    rts


 ; update sprite debe entrar al oam data y cambiar los valores de los tiles para que cambie el tile dibujado cada cierto tiempo
Update_Sprite_logic:
  jsr check_update_condition ; jump to check_update_condition subroutine
  lda skip_animation_flag
  cmp #1 ; Check if skip_animation_flag is set
  beq end_update_sprite_logic


  lda isMoving
  cmp #0
  beq reset_animation_state_and_sprites ; If isMoving is 0, reset animState and sprites
  jmp skip_reset_animState

  reset_animation_state_and_sprites:
    ; Reset animState to 0
    lda #$00
    sta animState
    jmp end_update_sprite_logic


  ;   ; Reset sprites to first frame (assuming the reset logic is corrected from subtraction to addition)
  ;   ldx #9 ; Start offset in sprite buffer
  ;   ldy #0
  ;   reset_sprites_loop:
  ;     lda SPRITE_BUFFER, x
  ;     clc
  ;     sbc #3 ; Adjust according to the desired frame change
  ;     sta SPRITE_BUFFER, x
  ;     inx
  ;     inx
  ;     inx
  ;     inx
  ;     iny
  ;     cpy #16
  ;     bne reset_sprites_loop
  skip_reset_animState:
    lda animState
    clc
    adc #1
    sta animState

    cmp #2
    bcc update_sprites
    lda #0
    sta animState
    jsr NOAnimated_sprite
    jmp end_update_sprite_logic

    ; Update animation state
  update_sprites:
  ; pha
  ; txa
  ; pha
  ; tya
  ; pha

    

    ldx #9 ; Start offset in sprite buffer
    ldy #0
    update_sprites_loop:
      lda SPRITE_BUFFER, x
      clc
      adc #2 ; Adjust to change the sprite to the next frame
      sta SPRITE_BUFFER, x
      inx
      inx
      inx
      inx
      iny
      cpy #16
      bne update_sprites_loop
    end_update_sprite_logic:
      lda #$00 ; Reset vblank_flag
      sta vblank_flag
      sta skip_animation_flag
  rts

  ; pla
  ; tay
  ; pla
  ; tax
  ; pla
    ;rts


check_update_condition:
    lda timer
    cmp #29
    bne set_skip_flag  ; Si frameCounter no es 29, se establece la bandera para saltar

    lda vblank_flag
    cmp #1
    beq clear_skip_flag ; Si vblank_flag está establecido, limpiamos la bandera para no saltar

set_skip_flag:
    lda #1
    sta skip_animation_flag
    rts

clear_skip_flag:
    lda #0
    sta skip_animation_flag
    rts



update_player:

    ; Assume no movement initially
    lda #0
    sta isMoving

    ; Check each direction
    lda pad1
    and #BTN_UP
    beq check_down  ; If not pressed, check next button
    lda #0          ; Direction for up
    sta direction
    lda #1          ; Indicate walking
    sta isMoving
    jsr move_player_up
    jmp end_update ; Skip further checks

    check_down:
    lda pad1
    and #BTN_DOWN
    beq check_left
    lda #1
    sta direction
    lda #1
    sta isMoving
    jsr move_player_down
    jmp end_update

    check_left:
    lda pad1
    and #BTN_LEFT
    beq check_right
    lda #2
    sta direction
    lda #1
    sta isMoving
    jsr move_player_left
    jmp end_update

    check_right:
    lda pad1
    and #BTN_RIGHT
    beq end_update
    lda #3
    sta direction
    lda #1
    jsr move_player_right
    sta isMoving


    end_update:
    lda direction
    cmp directionChanged ; Check if the direction has changed
    beq no_change_direction ; If the direction has not changed, skip changing the sprite
    lda direction 
    sta directionChanged ; Update directionChanged to the new direction
    jsr NOAnimated_sprite 
    no_change_direction:
    rts

NOAnimated_sprite:
  ; Get the offset for the sprite
  jsr get_offset_for_direction_sprite

    ldx #9 ; offset for buffer, where the tile data for tile 1 is stored
    ldy #0 ; offset for firstSpritesTiles and 4 count
    reset_sprites_loop:
    tya ; Load y to a
    pha ; Push y to the stack

    ldy offsetFirstsprite ; Load offsetFirstsprite to x
    lda firstSpritesTiles, y ; Load tile data for tile y
    sta SPRITE_BUFFER, x ; Store the tile data in the buffer
    
    lda offsetFirstsprite ; Load offsetFirstsprite to a
    clc
    adc #1
    sta offsetFirstsprite ; Store the updated offsetFirstsprite back to offset_static_spri
    pla
    tay
    ; ; pop in stack variables
    txa ; Load x to a
    clc
    adc #4 ; Add 4 to x to move to the next tile data
    tax ; Store the updated x back to x
    
    iny
    cpy #4 ; Check if y is 4
    bne reset_sprites_loop

  jmp end_update


get_offset_for_direction_sprite:
  ; i will traverse through firstSpritesTiles to get the offset of the sprite
  LDA direction     
  CMP #3         ; Compare offsetFirstsprite with 3
  BEQ SetValue3  
  CMP #2
  BEQ SetValue2  ; If offsetFirstsprite is 2, branch to code that sets Y to the desired value for this case
  CMP #1
  BEQ SetValue1 

  ; If none of the above, we assume offsetFirstsprite is 0 and fall through to SetValue0
  SetValue0:
      LDA #0         ; Set offsetFirstsprite to the value corresponding to offsetFirstsprite being 0
      STA offsetFirstsprite
      JMP Continue   ; Jump to the rest of the code
  SetValue1:
      LDA #4       ; Set offsetFirstsprite to the value corresponding to offsetFirstsprite being 1
      STA offsetFirstsprite
      JMP Continue
  SetValue2:
      LDA #8        ; Set offsetFirstsprite to the value corresponding to offsetFirstsprite being 2
      STA offsetFirstsprite
      JMP Continue
  SetValue3:
      LDA #12         
      STA offsetFirstsprite
      ; here
  Continue:
      rts

move_player_up:
    ldx #BASE_ADDR_Y
    ldy #0
    move_player_up_loop:
        lda SPRITE_BUFFER, x
        clc
        sbc #1
        sta SPRITE_BUFFER, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_up_loop
    rts

move_player_down:
    ldx #BASE_ADDR_Y
    ldy #0
    move_player_down_loop:
        lda SPRITE_BUFFER, x
        clc
        adc #2
        sta SPRITE_BUFFER, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_down_loop
    rts

move_player_left:
    ldx #BASE_ADDR_X
    ldy #0
    move_player_left_loop:
        lda SPRITE_BUFFER, x
        clc
        sbc #1
        sta SPRITE_BUFFER, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_left_loop
    rts

move_player_right:
    ldx #BASE_ADDR_X
    ldy #0
    move_player_right_loop:
        lda SPRITE_BUFFER, x
        clc
        adc #2
        sta SPRITE_BUFFER, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_right_loop
    rts

write_nametable_in_2x2_regions:
    ; Save registers to stack
    pha
    txa
    pha    ; esta es la subrutina que se encarga de dibujar tile por tile en la pantalla mediante la utilizacion de 
    tya    ; de sumas prestablecidas para saber en que region de memoria se va a escribir y en que tile especifico
    pha    ; se va a escribir
    

    ; Write the first tile of 4 tiles in 2x2 region
    lda NT_ptr
    sta PPUADDR
    lda NT_ptr+1
    sta PPUADDR
    lda Specific_Tile_Write
    sta PPUDATA

    ; Write the second tile of 4 tiles in 2x2 region
    lda NT_ptr
    sta PPUADDR
    lda NT_ptr+1
    clc
    adc #1
    sta PPUADDR
    lda Specific_Tile_Write
    clc
    adc #1
    sta PPUDATA

    ; Write the third tile of 4 tiles in 2x2 region
    lda NT_ptr
    sta PPUADDR
    lda NT_ptr+1
    clc
    adc #32
    sta PPUADDR
    lda Specific_Tile_Write
    clc
    adc #16
    sta PPUDATA

    ; Write the fourth tile of 4 tiles in 2x2 region
    lda NT_ptr
    sta PPUADDR
    lda NT_ptr+1
    clc
    adc #33
    sta PPUADDR
    lda Specific_Tile_Write
    clc
    adc #17
    sta PPUDATA

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts   ;regresa al read_bits_loop para confirmar si ya acabo o se vuelve a llamar


extraction_and_write_byte:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Loop through 2-bit pairs of the byte
    ; Each 2-bit pair corresponds to the top left tile of a 2x2 megatile, 
    ; can be used to index megatile array
    ldx #0
    read_bits_loop:
        lda #$00
        sta Bits_From_Byte ; Clear Bits_From_Byte
        
        lda Byte_To_Extraction ; Load byte to decode    ; para leer el byte que se va a decodificar se utiliza asl y rol
        clc                                             ; dos veces para saber cual es nuestro tile de arriba a la izquierda
        asl ; Sift to read 1 bit into carry              ; luego utlizarmos el MT_pointer para saber cual tile del mega tile debemos escribir
        rol Bits_From_Byte ; Rotate carry into Bits_From_Byte ; y luego se llama a la subrutina write_nametable_in_2x2_regions para escribir el tile
        asl ; Sift to read 1 bit into carry
        rol Bits_From_Byte ; Rotate carry into Bits_From_Byte
        sta Byte_To_Extraction ; Save byte back to Byte_To_Extraction

        ldy Bits_From_Byte ; Save the 2-bit pair to y register
        lda (MT_PTR), y ; Load tile from megatiles based on 2-bit pair 
        sta Specific_Tile_Write ; Save specific tile to Specific_Tile_Write

        ; From Specific_Tile_Write, call write_region_2x2_nametable 
        ; subroutine to write 2x2 region of nametable
        ; based on the top left tile of the mega tile selected
        jsr write_nametable_in_2x2_regions 

        ; Move NT_PTR to next 2x2 region
        lda NT_ptr+1     ;esta suma se encarga de mantenerlo moviendose 2 para la derecha buscando el siguiente tile
        clc
        adc #2
        sta NT_ptr+1

        ; Increment x to move to next 2-bit pair  
        inx                 ;luego de salir de la subrutina se incrementa x para saber si ya se escribieron los 4 megatiles
        cpx #4              ; y si no es asi sigue el loop porque aun no se ha dibujado el byte completo
        bne read_bits_loop
    
    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts 

; Loads, decodes and writes a nametable at NAME_TABLE_PTR 
; from a packaged nametable in ROM
write_nametable:  ; aqui se carga el nametable

    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Based on Curr_stage, use the megatiles correcponding to the stage
    lda Curr_stage
    cmp #1
    beq use_first_stage_megatiles ; segun el current stage se carga el megatile correspondiente
    cmp #2
    beq use_second_stage_megatiles

    use_first_stage_megatiles:
        lda #<megatiles_stage_one
        sta MT_PTR
        lda #>megatiles_stage_one
        sta MT_PTR+1
        jmp extract_and_write_nametable
    
    use_second_stage_megatiles:
        lda #<megatiles_stage_two
        sta MT_PTR
        lda #>megatiles_stage_two
        sta MT_PTR+1
        jmp extract_and_write_nametable

    extract_and_write_nametable: ; con el nametable que estamos utlizando se extraen los 60 bytes de datos en un forloop
    ldx #0                       ;llamando a extraction_and_write_byte que se encarga de desempaquetar los datos
    read_nametable_loop:
        txa
        tay
        lda (Specific_NT), y
        sta Byte_To_Extraction
        jsr extraction_and_write_byte

        ; Check if x+1 % 4 == 0, means we read 4 bytes, increment NAMETABLE_PTR by 32
        txa
        clc
        adc #1
        and #%00000011
        beq increment_nametable_ptr       ; se hace un mod para saber si los 4 bytes ya fueron leidos
        jmp skip_increment_nametable_ptr  ; con el objetivo de aumentar 32 al puntero de la memoria para pasar a la 
                                          ; siguiente region de 2x2 tiles
        increment_nametable_ptr:
            lda NT_ptr+1
            clc
            adc #32
            sta NT_ptr+1
        
            ; Check if carry, need to increment high byte
            bcc skip_increment_nametable_ptr  ; si hay un carry se incrementa el puntero de la memoria
            inc NT_ptr                        ; debido a que ya se termino de leer los 4 bytes
        
        skip_increment_nametable_ptr:
            inx 
            cpx #60
            bne read_nametable_loop
    
    ; Done with subroutine, pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts


load_attributes:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ldx #0
    read_attribute_loop:
        txa
        tay
        lda (Specific_Atributes), y
        sta PPUDATA
        inx
        cpx #64
        bne read_attribute_loop
    ; Done writing attributes

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts

swap_nametable:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; If A was not pressed, skip to end
    lda pad1
    and #BTN_A                       ; se comprueba con el pad si el boton A fue presionado utlizando los bits de BTN_A
    beq skip_nametable_change
    
    ; Disable disable NMI and screen
    lda PPUCTRL
    and #%01111111
    sta PPUCTRL
    lda PPUMASK                     ; para la pantalla para poder cambiar de nametable sin que se vea el cambio
    and #%11100000
    sta PPUMASK

    vblankwait3:
        bit PPUSTATUS
        bpl vblankwait3         ; se espera a que el vblank este listo para poder cambiar de nametable


    ; If in stage one, set to stage two
    ; If in stage two, set to stage one
    lda Curr_stage
    cmp #1
    beq set_second_stage        ; se compara el current stage para saber a que stage se va a cambiar
    cmp #2
    beq set_first_stage

    set_second_stage:
        lda #1
        sta need_update_nametable
        lda #2
        sta Curr_stage                         ; se cambia el current stage y se resetea el scroll para inicial en el
        jmp reset_scroll_update_nametable      ; nuevo stage
    
    set_first_stage:
        lda #1
        sta need_update_nametable
        lda #1
        sta Curr_stage
        jmp reset_scroll_update_nametable
    
    reset_scroll_update_nametable:
        ; Set scroll position to 0,0
        lda #$00
        sta SCROLL_POSITION_X                ; el label que permite resetear el scroll para que se inicie en la posicion 0,0
        sta SCROLL_POSITION_Y
        jsr update_nametable

    skip_nametable_change:

    ; Restore NMI and screen
    lda #%10010000
    sta PPUCTRL
    lda #$1e                      ; reactivamos el screen para que se vea el cambio de nametable
    sta PPUMASK

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla
    
    rts

update_nametable:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Check if need_update_nametable is set
    lda need_update_nametable                          ; se comprueba si se necesita actualizar la nametable con el flag
    cmp #1                                             ; need_update_nametable
    bne skip_load_second_stage_nametable

    ; Select nametable based on CURRENT_STAGE
    lda Curr_stage
    cmp #1
    beq select_stage_one

    lda Curr_stage
    cmp #2
    beq select_stage_two

    select_stage_one:
        ; Load stage one nametable1 nametable
        lda #<stage_one_nametable1_packaged             ; se carga el primer stage de la nametable
        sta Specific_NT
        lda #>stage_one_nametable1_packaged
        sta Specific_NT+1

        lda #$20
        sta NT_ptr
        lda #$00
        sta NT_ptr+1
        jsr write_nametable

        ; Load stage one nametable1 attributes
        lda #<stage_one_nametable1_attributes
        sta Specific_Atributes                          ; se cargan los atributos del primer stage
        lda #>stage_one_nametable1_attributes
        sta Specific_Atributes+1

        lda #$23
        sta NT_ptr
        lda #$C0
        sta NT_ptr+1
        jsr load_attributes

        ; Load stage one nametable2 nametable
        lda #<stage_one_nametable1_packaged
        sta Specific_Atributes
        lda #>stage_one_nametable1_packaged              ; lo mismo para el segundo nametable del primer stage
        sta Specific_Atributes+1

        lda #$24
        sta NT_ptr
        lda #$00
        sta NT_ptr+1
        jsr write_nametable

        ; Load stage one nametable attributes
        lda #<stage_one_nametable1_attributes
        sta Specific_Atributes
        lda #>stage_one_nametable1_attributes
        sta Specific_Atributes+1

        lda #$27
        sta NT_ptr
        lda #$C0
        sta NT_ptr+1
        jsr load_attributes

        jmp skip_update_nametable


    skip_load_second_stage_nametable:
        jmp skip_update_nametable      ; si no se necesita actualizar la nametable se salta a la siguiente subrutina
    
    select_stage_two:
        ; Load stage two nametable3 nametable
        lda #<stage_two_nametable3_packaged
        sta Specific_NT                               ; lo mismo que el primero pero con el segundo stage nametable y atributes
        lda #>stage_two_nametable3_packaged
        sta Specific_NT+1

        lda #$20
        sta NT_ptr
        lda #$00
        sta NT_ptr+1
        jsr write_nametable

        ; Load stage two nametable3 attributes
        lda #<stage_two_nametable3_attributes
        sta Specific_Atributes
        lda #>stage_two_nametable3_attributes
        sta Specific_Atributes+1

        lda #$23
        sta NT_ptr
        lda #$C0
        sta NT_ptr+1
        jsr load_attributes

        ; Load stage two nametable4 nametable
        lda #<stage_two_nametable4_packaged
        sta Specific_NT
        lda #>stage_two_nametable4_packaged
        sta Specific_NT+1

        lda #$24
        sta NT_ptr
        lda #$00
        sta NT_ptr+1
        jsr write_nametable

        ; Load stage two nametable4 attributes
        lda #<stage_two_nametable4_attributes
        sta Specific_Atributes
        lda #>stage_two_nametable4_attributes
        sta Specific_Atributes+1

        lda #$27
        sta NT_ptr
        lda #$C0
        sta NT_ptr+1
        jsr load_attributes

        jmp skip_update_nametable

    skip_update_nametable:
    ; Set need_update_nametable to 0
    lda #0
    sta need_update_nametable

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts

palettes:
.byte $0f, $10, $07, $2d ;pallete 0
.byte $0f, $00, $11, $30 ;pallete 1	
.byte $0f, $00, $2a, $30 ; pallete 2
.byte $0f, $00, $2a, $30 ; pallete 3

.byte $0F, $16, $13, $37
.byte $0f, $00, $00, $00
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00

ant_sprites:
;primer sprite hacia arriba 1ra animacion
.byte $54, $01, $00, $64
.byte $54, $02, $00, $6C
.byte $5C, $11, $00, $64
.byte $5C, $12, $00, $6C

firstSpritesTiles:
      ; 0   1     2   3     4   5     6    7   8     9   A   B     C    D     E   F
.byte $01, $02, $11, $12, $41, $42, $51, $52, $61, $62, $71, $72, $21, $22, $31, $32

; name_table:
; .byte $01, $02, $03, $04, $05, $06, $07, $08
; .byte $11, $12, $13, $14, $15, $16, $17, $18
; .byte $21, $22, $23, $24, $25, $26, $27, $28
; .byte $31, $32, $33, $34, $35, $36, $37, $38

stage_one_nametable1_packaged:
.incbin "ScriptEmpaquetamiento/nametable1_packaged.bin"
stage_one_nametable1_attributes:
.incbin "ScriptEmpaquetamiento/nametable1_attributes.bin"
stage_one_nametable2_packaged:
.incbin "ScriptEmpaquetamiento/nametable2_packaged.bin"
stage_one_nametable2_attributes:
.incbin "ScriptEmpaquetamiento/nametable2_attributes.bin"
stage_two_nametable3_packaged:
.incbin "ScriptEmpaquetamiento/nametable3_packaged.bin"
stage_two_nametable3_attributes:
.incbin "ScriptEmpaquetamiento/nametable3_attributes.bin"
stage_two_nametable4_packaged:
.incbin "ScriptEmpaquetamiento/nametable4_packaged.bin"
stage_two_nametable4_attributes:
.incbin "ScriptEmpaquetamiento/nametable4_attributes.bin"

; Megatiles
megatiles_stage_one:
.byte $01, $03, $05, $07
megatiles_stage_two:
.byte $21, $23, $25,$27 

; Character memory
.segment "CHARS"
.incbin "tiles2.chr"