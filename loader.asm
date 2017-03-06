tape_leader_length = 48
irq_break_delay = @(half 3)
irq_delay = 7
irq_handler_delay = 29
restart_delay = @(+ irq_break_delay irq_delay irq_handler_delay)

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
    lda #4
    sta tape_bit_counter

    lda #<tape_loader
    sta @(+ 1 mod_handler)
    lda #>tape_loader
    sta @(+ 2 mod_handler)

    ; Start tape motor.
    lda $911c
    and #$fd
    sta $911c

    ; Set IRQ vector.
    lda #<irq
    sta $314
    lda #>irq
    sta $315

    ; Initialise VIA2 Timer 1 (cassette tape read).
    lda #<timer
    sta current_low
    sta $9124
    lda #>timer
    sta $9125

    lda #%00000000  ; One-shot mode.
    sta $912b
    lda #%10000010  ; CA1 IRQ enable (tape pulse)
    sta $912e

    ; Let the IRQ handler do everthing.
    cli
w:  jmp -w

irq:
    lda $9124       ; Read the timer's low byte which is your sample.
    ldy #>timer
    ldx $9125
    stx $1000
    sty $9125       ; Write high byte to restart the timer.

    tax
    lsr             ; Reduce sample from 7 to 2 bits.
    lsr
    lsr
    lsr
    lsr
    lsr
    sta $900f       ; Something for the eye.
mod_handler:
    jsr $0000
jmp +done

    ; Make sum of samples.
    txa
    clc
    adc average
    sta average
    bcc +n
    inc @(++ average)

n:  dec tleft
    bne +done

    ; Correct time if average pulse length doesn't match our desired value.
    lda @(++ average)   ; average / 256
    cmp #$7f
    beq +done           ; It's already what we want.
    bcc +n
    dec current_low
    bne +m
n:  inc current_low
m:  lda current_low
    sta $9124
    lda #0
    sta average
    sta @(++ average)

done:
    lda #$7f
    sta $912d
    jmp $eb18

tape_loader:
    cmp #0
    beq +n
    ldx #<tape_loader
    ldy #>tape_loader
    lda tape_leader_countdown
    bpl restart_loader
    ldx #<tape_loader_data
    ldy #>tape_loader_data
restart_loader:
    lda #tape_leader_length
    sta tape_leader_countdown
    stx @(+ 1 mod_handler)
    sty @(+ 2 mod_handler)
    bne +r
n:  dec tape_leader_countdown
r:  rts

tape_loader_data:
    asl tape_current_byte
    asl tape_current_byte
    ora tape_current_byte
    sta tape_current_byte
    dec tape_bit_counter
    beq byte_complete
    rts

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
