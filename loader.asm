tape_leader_length = 32
irq_break_delay = @(half 3)
irq_delay = 7
irq_handler_delay = 29
restart_delay = @(+ irq_break_delay irq_delay irq_handler_delay)
tape_map_length = 1024
tape_map_end = @(+ tape_map tape_map_length)

timer = @(- (* 8 *pulse-long*) restart_delay)

main:
    sei
    lda #$7f
    sta $911e
    sta $912e
    sta $912d

    ldx #5
l:  lda cfg,x
    sta tape_ptr,x
    dex
    bpl -l

    lda #tape_leader_length
    sta tape_leader_countdown

    lda #<tape_map
    sta s
    sta d
    lda #>tape_map
    sta @(++ s)
    sta @(++ d)
    ldy #0
l:  lda #$ff
    sta (d),y
    iny
    bne -l
    inc @(++ d)
    lda @(++ d)
    cmp #>tape_map_end
    bne -l
    lda #0
    sta tape_map
    lda #3
    sta @(+ tape_map tape_map_length -1)

    ; Start tape motor.
    lda $911c
    and #$fd
    sta $911c

    ; Set IRQ vector.
    lda #<tape_leader1
    sta $314
    lda #>tape_leader1
    sta $315

    ; Initialise VIA2 Timer 1 (cassette tape read).
    ldx #@(low *tape-pulse*) ; Restart timer.
    sta $9124
    ldx #@(high *tape-pulse*) ; Restart timer.
    sta $9125
    lda #%00000000  ; One-shot mode.
    sta $912b
    lda #%10000010  ; CA1 IRQ enable (tape pulse)
    sta $912e

    ; Let the IRQ handler do everything.
    cli
    clc
w:  bcc -w

tape_get_bit:
    lda $912d               ; Get timer underflow bit.
    ldx #@(high *tape-pulse*) ; Restart timer.
    stx $9125
    ldx $9121
    asl     ; Move underflow bit into carry.
    asl
    sta $900f
    rts

tape_leader1:
    jsr tape_get_bit
    bcc +n
    lda tape_leader_countdown
    bmi +j
    lda #tape_leader_length
    sta tape_leader_countdown
    jmp intret
j:  ldx #<tape_leader1end
    ldy #>tape_leader1end
    jmp intretn
n:  dec tape_leader_countdown
    jmp intret

tape_leader1end:
    lda #<timer
    sta $9124
    lda #>timer
    sta $9125
    lda #0
    sta tape_bit_counter
    lda #32
    sta tape_leader_countdown
    ldx #<tape_sync
    ldy #>tape_sync
    jmp intretn

tape_sync:
    lda $9124       ; Read the timer's low byte which is your sample.
    ldy #>timer
    ldx $9125
    sty $9125       ; Write high byte to restart the timer.

    ; Plot nibble into map of pulse lengths.
    sta s
    stx @(++ s)
    lda s
    clc
    adc #<tape_map
    sta s
    lda @(++ s)
    adc #>tape_map
    sta @(++ s)
    ldy #0
    lda tape_bit_counter
    sta (s),y
    clc
    adc #1
    and #3
    sta tape_bit_counter
    sta $900f

    dec tape_leader_countdown
    beq fill_map
    bne intret

intretn:
    stx $314
    sty $315
intret:
    lda #$7f
    sta $912d
    jmp $eb18

fill_map:
    lda #<tape_map
    sta s
    lda #>tape_map
    sta @(++ s)

    ldy #0
l:  lda (s),y
    bmi +f
    tax
    jsr inc_s
    lda @(++ s)
    cmp #>tape_map_end
    beq +r
    bne -l
f:  lda s
    sta d
    lda @(++ s)
    sta @(++ d)
    lda #0
    sta c
    sta @(++ c)
m:  jsr inc_d
    jsr inc_c
    lda (d),y
    bmi -m
n:  txa
    sta (s),y
    jsr inc_s
    sta (s),y
    lda (d),y
    jsr dec_d
    sta (d),y
    jsr dec_c
    beq -l
    jsr dec_c
    bne -n
    beq -l

r:  lda #tape_leader_length
    sta tape_leader_countdown
    ldx #@(low *tape-pulse*)
    sta $9124
    ldx #@(high *tape-pulse*)
    sta $9125
    ldx #<tape_leader2
    ldy #>tape_leader2
    jmp intretn

tape_leader2:
    jsr tape_get_bit
    bcc +n
    lda tape_leader_countdown
    bmi +j
    lda #tape_leader_length
    sta tape_leader_countdown
    jmp intret
j:  ldx #<tape_leader2end
    ldy #>tape_leader2end
    jmp intretn
n:  dec tape_leader_countdown
    jmp intret

tape_leader2end:
    lda #<timer
    sta $9124
    lda #>timer
    sta $9125
    lda #4
    sta tape_bit_counter
    ldx #<tape_loader_data
    ldy #>tape_loader_data
    jmp intretn

tape_loader_data:
    lda $9124       ; Read the timer's low byte which is your sample.
    ldy #>timer
    ldx $9125
    sty $9125       ; Write high byte to restart the timer.

    sta s
    stx @(++ s)
    lda s
    clc
    adc #<tape_map
    sta s
    lda @(++ s)
    adc #>tape_map
    sta @(++ s)
    ldy #0
    lda (s),y
 
    asl tape_current_byte
    asl tape_current_byte
    ora tape_current_byte
    sta tape_current_byte
    sta $900f
    dec tape_bit_counter
    beq byte_complete
r:  jmp intret

byte_complete:
    lda #4                  ; Reset bit count.
    sta tape_bit_counter
    lda tape_current_byte   ; Save byte to its destination.
    ldy #0
    sta (tape_ptr),y
    inc tape_ptr            ; Advance destination address.
    bne +n
    inc @(++ tape_ptr)
n:  dec tape_counter        ; All bytes loaded?
    bne -r                  ; No...
    dec @(++ tape_counter)
    bne -r                  ; No...

    sei
    lda #$7f                ; Turn off tape pulse interrupt.
    sta $912e
    sta $912d

    lda tape_old_irq
    sta $314
    lda @(++ tape_old_irq)
    sta $315

    jmp (tape_callback)

start_game:
    ldx #@(- copy_forwards_end copy_forwards 1)
l:  lda copy_forwards,x
    sta $1000,x
    dex
    bpl -l
    jmp $1000

inc_s:
    inc s
    bne +n
    inc @(+ 1 s)
n:  rts

inc_d:
    inc d
    bne +n
    inc @(+ 1 d)
n:  rts

inc_c:
    inc c
    bne +n
    inc @(+ 1 c)
n:  rts

dec_d:
    pha
    dec d
    lda d
    cmp #$ff
    bne +n
    dec @(+ 1 d)
n:  pla
    rts

dec_c:
    dec c
    lda c
    cmp #$ff
    bne +n
    dec @(+ 1 c)
n:  lda c
    ora @(+ 1 c)
    rts

copy_forwards:
    ldy #@(low binary_size)
    lda #0
    sta c
    lda #@(++ (high binary_size))
    sta @(++ c)
    lda #<target
    sta s
    lda #>target
    sta @(++ s)
    lda #$ff
    sta d
    lda #$11
    sta @(++ d)

    ldy #0
l:  lda (s),y
    sta (d),y
    inc s
    beq +k
n:  inc d
    beq +m
q:  dex
    bne -l
    dec @(++ c)
    bne -l
    jmp $120d

k:  inc @(++ s)
    clc
    bcc -n

m:  inc @(++ d)
    clc
    bcc -q
copy_forwards_end:

binary_size = @(length (fetch-file "sssa.exo.prg"))
cfg:
    <target >target
    <binary_size @(++ (high binary_size))
    <start_game >start_game

target:
