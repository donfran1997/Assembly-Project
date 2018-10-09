;
; Comp2121 project
;
; Created: 26/5/2017
; Author : Kristen & Francis
;

.include "m2560def.inc"

;###################################
;Defines
;###################################

.def temp = r16
.def keyRead = r17
.def temp1 = r18
.def screen = r19
.def coin = r20
.def item = r21
.def temp2 = r24

.equ quarterSec = 1953/2  ;7812/4/2 for time3
.equ threeSec = 12
.equ fiveSec = 5*4
.equ fiveSecH = quarterSec*4*5
.equ PBdebounceTime = quarterSec / 2; debouncing time is 1/8 second
.equ soundMask = 0x01
.equ FreqSpeaker = 15; pitch at C
.equ PORTADIR = 0xF0 ; PD7-4: output-col, PD3-0, input-row
.equ INITCOLMASK = 0xEF ; scan from the rightmost column,
.equ INITROWMASK = 0x01 ; scan from the top row
.equ ROWMASK = 0x0F ; for obtaining input from Port D
.equ AdcStable = 13  ;check 13 times analog digital conveter




;###################################
;Macros
;###################################

.macro incCounter
    lds temp, @0
    inc temp
    sts @0, temp
.endmacro

.macro setValue
    ldi temp, @1
    sts @0, temp
.endmacro

.macro clear
	clr temp
	sts @0,temp
	sts @0+1, temp
.endmacro

.macro clear2
    clr temp
    sts @0, temp
.endmacro

.macro setFlag
    ldi temp, 1
    sts @0, temp
.endmacro

;turn on led high
.macro turnOn
    ldi temp, 0b00000100
    out portg, temp
.endmacro

.macro PB0_debounce
    lds temp, PB0Mark
    cpi temp, 0
    breq PB0_End  ;it has debounced

    ;if has not debounced yet
    ;load PB0counter
    lds r24, PB0Counter
    lds r25, PB0Counter+1
    ;add 1
    adiw r24:r25, 1

    cpi r24, low(PBdebounceTime)
    ldi temp, high(PBdebounceTime)
    cpc r25, temp
    brne PB0_notDebounced ;if not yet

    ;others, clear counter and PB debounce flag
    clear PB0Counter
    clear2 PB0Mark
    rjmp PB0_End

    PB0_notDebounced:
    ;store number of PB0counter back
        sts PB0Counter, r24
        sts PB0Counter+1, r25

    PB0_End:
.endmacro

.macro PB1_debounce
    lds temp, PB1Mark
    cpi temp, 0
    breq PB1_End  ;it has debounced

    ;if has not debounced yet
    ;load PB0counter
    lds r24, PB1Counter
    lds r25, PB1Counter+1
    adiw r24:r25, 1

    cpi r24, low(PBdebounceTime)
    ldi temp, high(PBdebounceTime)
    cpc r25, temp
    brne PB1_notDebounced ;if not yet

    ;others, clear counter and PB debounce flag
    clear PB1Counter
    clear2 PB1Mark
    rjmp PB1_End

    PB1_notDebounced:
    ;store number of PB1counter back
        sts PB1Counter, r24
        sts PB1Counter+1, r25

    PB1_End:
.endmacro

.macro MotorOn
    ;toggle motor
    lds temp, MotorSwitch
    com temp  ;Reverse
    sts MotorSwitch, temp ; write back
    andi temp, 0x08 ;take the PE5 equal 0b00001000
    out porte, temp ;write to motor
.endmacro

.macro turnOnMotor
    ldi temp, 0x08 ;0b00001000
    out porte, temp
.endmacro

.macro PBenable
    ldi temp, (1<<INT0)|(1<<INT1)
    out EIMSK, temp
    clear2 PB0Mark
    clear2 PB1Mark
.endmacro

.macro PBdisable
	clr temp
	out EIMSK, temp
    clear2 PB0Mark
    clear2 PB1Mark
.endmacro

.macro do_lcd_data_r
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro LEDShowNum
	mov temp, @0
	call LEDShowFunction
.endmacro

.macro ShowNum
	in temp1, SREG
	push temp1
	push temp  ;use temp to check if the hundred position is 0

		mov temp1, @0
		clr temp2
		clr temp

	hundred_start:
		cpi temp1, 100
		brlo hundred_end
		inc temp2
		subi temp1, 100
		rjmp hundred_start

	hundred_end:
		cpi temp2, 0
		breq ten_init

		ldi temp, 1 ; set temp to notice tens to display num even if it is 0

		; not zero, then display hundred
		subi temp2, -'0'
		do_lcd_data_r temp2

	ten_init:
		clr temp2

	ten_start:
		cpi temp1, 10
		brlo ten_end
		inc temp2
		subi temp1, 10
		; when it is higher than 10
		rjmp ten_start

	ten_end:
		; when ten's flag is on, always show ten's no matter what
		cpi temp, 0
		brne ten_display

		cpi temp2, 0
		breq single_start
	ten_display:
		; not zero, then display tens
		subi temp2, -'0'
		do_lcd_data_r temp2

	single_start:
		; now temp1 is left with single digits
		subi temp1, -'0'
		do_lcd_data_r temp1

	pop temp
	pop temp1
	out SREG, temp1
.endmacro

.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro readPot
	ldi temp, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp
	ldi temp, 1<<MUX5
	sts ADCSRB, temp
	ldi temp, (1<<ADEN)|(1<<ADSC)|(1<<ADIE)|(5<<ADPS0)
	sts ADCSRA, temp
.endmacro



;###################################
;Dseg
;###################################

.dseg
    PB0Counter: .byte 2
    PB1Counter: .byte 2
    PB0Mark: .byte 1
    PB1Mark: .byte 1

    Stock: .byte 9
    Price: .byte 9
    currStock: .byte 2
    currPrice: .byte 2

    ADC_max_Counter: .byte 1
    ADC_min_Counter: .byte 1
    ADC_min: .byte 1
    ADC_max: .byte 1

    Bright: .byte 1
    BacklightCounter: .byte 2
    pressKey: .byte 1
    BrightCounter: .byte 1
    BacklightOn: .byte 1

    Sound: .byte 1
    SoundCounter: .byte 1
    SoundState: .byte 1
    SoundStart: .byte 1

    QsecCounter: .byte 2
    StartCounter: .byte 2

    MotorSwitch: .byte 1
    MotorMode: .byte 1
    MotorCounter: .byte 1

    InsertCoin: .byte 1
    insertedCoins_live: .byte 1

    LEDBlinkMode: .byte 1
    BlinkMark: .byte 1
    ledBlinkCounter: .byte 1
    currPattern: .byte 1

    KeypadFree: .byte 1
    StarCounter: .byte 1

;###################################
;Cseg
;###################################

.cseg
.org 0
	jmp RESET

.org INT0addr ;interrupt, jump to pb0
	jmp PB0int

.org INT1addr
	jmp PB1int

.org  OVF3addr
	jmp Timer3Ovf

.org 0x003A ;ADC complete
	jmp ADC_Method

.org 0x0070
	reti



;############### PB0 PB1 Int ####################

;screen = 0, means it is start screen
;screen = 2, means it is admin screen
;others screen = 1

PB0int:
    push temp
    in temp, SREG
    push temp

    ;if has not debounce yet
    lds temp, PB0Mark
    cpi temp, 0
    brne PB0_debounce_End

    ;if has debounced
    ;mark debounce
    incCounter PB0Mark
    clear PB0Counter  ;Clear counter

    ;screen is 1
    cpi screen, 1
    brne PB0_Screen2

    PB0_Screen1:
        ;disable the led
        setValue ledBlinkCounter, threeSec    ;turn off led
        rjmp PB0_debounce_End

    PB0_Screen2:
        cpi screen, 2
        brne PB0_debounce_End

        ;when the screen is admin screen
        ;update number of stock
        lds YL, currStock
        lds YH, currStock+1

        ;only increase to 255
        ld temp, Y
        cpi temp, 0xFF
        breq PB0_debounce_End

        ;increase the number of stock
        inc temp
        st Y, temp

    PB0_debounce_End:

    pop temp
	out SREG, temp
	pop temp
	reti

PB1int:
    push temp
    in temp, SREG
    push temp

    ;if has not debounce yet
    lds temp, PB1Mark
    cpi temp, 0
    brne PB1_debounce_End

    ;if has debounced
    ;mark debounce
    incCounter PB1Mark
    clear PB1Counter  ;Clear counter

    ;screen is 1
    cpi screen, 1
    brne PB1_Screen2

    PB1_Screen1:
        ;disable the led
        setValue ledBlinkCounter, threeSec    ;turn off led
        rjmp PB1_debounce_End

    PB1_Screen2:
        cpi screen, 2
        brne PB1_debounce_End

        ;when the screen is admin screen
        ;update number of stock
        lds YL, currStock
        lds YH, currStock+1

        ;only decrease when the number over 0
        ld temp, Y
        cpi temp, 0x00
        breq PB0_debounce_End

        ;decrease the number of stock
        dec temp
        st Y, temp

    PB1_debounce_End:

    pop temp
    out SREG, temp
    pop temp
    reti

ADC_Method:
    push temp
    in temp, SREG
    push temp
    push r24
    push r25

    ;load adc value
    lds r24, ADCL
    lds r25, ADCH

    ;compare with min value
    cpi r24, 0x00
	clr temp
	cpc r25, temp
	breq ADCMin

    ;compare with max value
    cpi r24, 0xFF
	ldi temp, 0x03
	cpc r25, temp
	breq ADCMax

    ;others, clear them
    clear2 ADC_min_Counter
    clear2 ADC_max_Counter

    rjmp endADC

    ADCMin:
        clear2 ADC_max_Counter
        incCounter ADC_min_Counter
        cpi temp, ADCstable
        brne endADC
        clear2 ADC_min_Counter  ;clear min counter
        setFlag ADC_min
        rjmp endADC


    ADCMax:
        clear2 ADC_min_Counter
        incCounter ADC_max_Counter
        cpi temp, ADCstable
        brne endADC
        clear2 ADC_max_Counter  ;clear max counter

        lds temp, ADC_min
        cpi temp, 1
        brne endADC

        turnOn
        setFlag ADC_max
        clear2 ADC_min
        rjmp endADC

    endADC:

	pop r25
	pop r24
	pop temp
	out SREG, temp
	pop temp
	reti

Timer3Ovf:
    ;PROLOGUE
    push r24
    push r25
    push YL
    push YH
    push temp1
    push temp
    in temp, SREG
    push temp

    ;check debounce PB1 and PB0
    PB0_debounce
    PB1_debounce

    ;############## LCD Backlight #############
    ;if it is admin mode
    cpi screen, 2
    brne lcd_Backlight

    ;it is, max Bright
    setValue Bright, 255
    ldi temp, 0xFF
    sts OCR3BL, temp ; output compare register B with low byte
    rjmp lcdBacklight_End

    lcd_Backlight:
        ;if key is pressed
        lds temp, pressKey
        cpi temp, 0
        brne keyPressed

        ;if lcd counter = 5 sec, turn off
        lds r24, BacklightCounter
        lds r25, BacklightCounter+1
        adiw r24:r25, 1

        cpi r24, low(fiveSecH)
        ldi temp, high(fiveSecH)
        cpc r25, temp
        breq BacklightOff

        ;write back counter
        sts BacklightCounter, r24
        sts BacklightCounter+1, r25
        rjmp lcdBacklight_End

        keyPressed:
            ;if pressed, clear counter
            clear BacklightCounter
            ldi temp, 0xFF
	        sts OCR3BL, temp
	        sts Bright, temp
	        rjmp lcdBacklight_End

        BacklightOff:
            ;128 level of brightness
            incCounter BrightCounter
            cpi temp, 15   ;15 overflow for each brightness  500ms turn off smoothly
            brne lcdBacklight_End

            ;clear brightCounter
            clear BrightCounter

            ;decrease brightness
            lds temp, Bright
            cpi temp, 2
            brsh Backlight_Dec

            ;when brightness over 2
            ;set to 0
            clr temp
            rjmp newBacklight

            Backlight_Dec:
                subi temp, 2

            newBacklight:
                ;write back
                sts Bright, temp
                sts OCR3BL, temp
                rjmp lcdBacklight_End

        lcdBacklight_End:

    ;############## Speaker sound #############
    ; if sound is on
	lds temp, Sound
	cpi temp, 0  ;not 0, play sound
	breq SoundEnd

    lds temp, SoundCounter
    cpi temp, FreqSpeaker
    brne SoundNot

    clear2 SoundCounter

    ;toggle sound
    ;reverse
    lds temp, soundState
	com temp
    ;write back
	sts SoundState, temp

    ;mask and write back to portB
	andi temp, soundMask
	out portb, temp
	rjmp SoundEnd

    SoundNot:
        ;increase counter
        incCounter SoundCounter

    SoundEnd:

    ;############## Compare quarter second #############
    QSec:

        ; load addr
        lds r24, QsecCounter
        lds r25, QsecCounter + 1
        ; add 1
        adiw r24:r25, 1

        ; compare 1/4 sec
        cpi r24, low(quarterSec)
        ldi temp, high(quarterSec)
        cpc r25, temp

        breq IsQuarterSec
        jmp notQsec

    ;############## Quarter second #############
    IsQuarterSec:
        clear QsecCounter
        ; if screen is 0, it is start mode
        ; count to 3 sec, so that into select screen
        cpi screen, 0
        breq Screen0
        rjmp ScreenEnd

        ;it shows the start screen
        Screen0:
            lds r24, StartCounter
			inc r24
            cpi r24, threeSec
            brne Screen0End ;if not yet

            ;if it is 3 sec, clear counter
            inc screen   ;shows select screen
            clear StartCounter
            rjmp ScreenEnd

        Screen0End:
            ;write back
            sts StartCounter, r24
            rjmp ScreenEnd
    ScreenEnd:

;############## Motor Mode #############
    lds temp, MotorMode
	cpi temp, 0  ; DEFAULT MODE
	breq MotorEnd

	cpi temp,  1 ; 1 means pulsing
	breq pulseMotor

	cpi temp, 2 ; 2 means turn on for three sec
	breq TurnOn3sec

	rjmp MotorEnd

    pulseMotor:
        ;coins return
        lds temp, InsertCoin ; 2 * num of real coins inserted
		cpi temp, 0
		breq MotorDefault

        ; decrease coins and store back
		dec temp
		sts InsertCoin, temp
		MotorOn

        rjmp MotorEnd

    TurnOn3sec:
        lds temp, MotorCounter
        inc temp
        cpi temp, threeSec
		brne MotorOnKeep

        ; turn off motor then clear counter
		; set default
		clear2 MotorCounter
		rjmp MotorDefault

		MotorOnKeep:
			sts MotorCounter, temp
			turnOnMotor
			rjmp MotorEnd

    MotorDefault:
        clr temp
        sts MotorMode, temp
        out porte, temp ; turn off
        rjmp MotorEnd

    MotorEnd:

;############## LED Mode #############
    lds temp, LEDBlinkMode
    cpi temp, 0 ;  DEFAULT MODE
    breq LEDend

    cpi temp, 1 ; 1/2 sec blinking mode
    breq BkMode

    BkMode:
        lds temp, ledBlinkCounter
        cpi temp, threeSec
        breq BlinkFinish
        incCounter ledBlinkCounter  ; increment LED count

        ; check blink (keep/toggle)
        lds temp, BlinkMark
        cpi temp, 1 ;equal 1, clear blinkMark and skip
        breq LED_Keep

        ; mark it so that it holds next time
        incCounter BlinkMark

        ;toggle light
        lds temp, currPattern
        ;reverse
        com temp
        out portc, temp

        ; write to portG
        mov temp1, temp
        andi temp1, 0x03 ; led0 and 1 is connected to portg pin0,1
        out portg, temp1

        sts currPattern, temp  ; store curr pattern back
        rjmp LEDend

        LED_Keep:
            clear2 BlinkMark
            rjmp LEDend

        BlinkFinish:
            clear2 ledBlinkCounter
            clear2 BlinkMark
            clr temp
            out portc, temp
            out portg, temp
            rjmp LED_default

        LED_default:
            setValue LEDBlinkMode, 0

    LEDend:

;################## Pressed "*" ##################
    ;* is pressed which hold 5 seconds
    ;so it should go into select mode

    lds temp, KeypadFree
    cpi temp, 0 ; 0 means the keyboard is released this moment
    breq notStar

	; check if the pressed key is star, when it isnt released
	lds temp, pressKey
	cpi temp, '*'
	brne notStar

    ; it is star and pressed, increase the counter
	incCounter StarCounter

    ; compare if it is 5 seconds
	cpi temp, fiveSec
	brne StarEnd

    ; set up screen to enter admin mode
	clear2 StarCounter
	ldi screen, 2
	rjmp StarEnd

    notStar:
        clear2 StarCounter

    StarEnd:

        ; Decline speaker count if it is not zero
        ; check if key is pressed
        lds temp, SoundStart
        cpi temp, 0
        breq speakerDecrease

        ; if soundStart is marked
        ; copy to sound flag and clear it
        sts Sound, temp
        clear2 SoundStart
        rjmp speakerDecreaseEnd

    speakerDecrease:
        lds temp, Sound
        cpi temp, 0
        breq speakerDecreaseEnd
        ; decrease 1
        dec temp
        sts Sound, temp

    speakerDecreaseEnd:

    rjmp endOvf

;############## Not equal Quarter second #############
    NotQsec:
        ; write back
        sts QsecCounter, r24
        sts QsecCounter + 1, r25


;################## EPILOGUE #################
    endOvf:

    pop temp
	out SREG, temp
	pop temp
	pop temp1
	pop YH
	pop YL
	pop r25
	pop r24
	reti

;################## RESET #####################
RESET:
    ;########### Stack Pointer Initial ###########
    ldi r16, low(RAMEND)
    out SPL, r16
    ldi r16, high(RAMEND)
    out SPH, r16

    ;########### LCD Initial ###########
    ser r16
    out DDRF, r16 ;set data port
    out DDRA, r16 ;set rs/rw etc
    clr r16
    out PORTF, r16  ; clear port
    out PORTA, r16	; clear port

    do_lcd_command 0b00111000 ; 2x5x7
    rcall sleep_5ms
    do_lcd_command 0b00111000 ; 2x5x7
    rcall sleep_1ms
    do_lcd_command 0b00111000 ; 2x5x7
    do_lcd_command 0b00111000 ; 2x5x7
    do_lcd_command 0b00001000 ; display off?
    do_lcd_command 0b00000001 ; clear display
    do_lcd_command 0b00000110 ; increment, no display shift
    do_lcd_command 0b00001100 ; Cursor on, bar, no blink

    ;########### LED Initial ###########
    ser temp
	out DDRG, temp
	out DDRC, temp
	clr temp
	out PORTC, temp
	out PORTG, temp

    ;########### Motor Initial ###########
    ; use portE E4 for motor
	ser temp
	out DDRE, temp

    ;########### Time Initial ###########
    ldi temp, (2<<CS30) ; prescaler of 8
	sts TCCR3B, temp     ;prescaler store into Timer counter interrupt control register

    ;Waveform generate mode, use phase correct mode
    ;compare output mode, enable chanel B
    ;up, turn off OC3B, down->turn on OC3B
	ldi temp, (1<<WGM30) | (2<<COM3B0)
	sts TCCR3A, temp
    ;set 1
	ser temp
    ;output compare register
	sts OCR3BL, temp
	clr temp
	sts OCR3BH, temp

    ;Timer Overflow Interrupt Enable
	ldi temp, 1<<TOIE3
    ;Timer Interrupt Mask Register
	sts TIMSK3, temp

    ;########### KetPad Initial ###########
    ldi temp, PORTADIR ; PA7:4/PA3:0, out/in
	sts DDRL, temp

    ;########### Set falling edge for PBs ###########
    ;Interrupt Sense Control
    ldi temp, (2<<ISC00)|(2<<ISC10)
	sts EICRA, temp

    ;########### Sound Initial ###########
    ldi temp, 0x01
	out DDRB, temp
	clr temp
	out PORTB, temp

    ;########### Clear Register ###########
    clr temp2
	clr temp
	clr screen
    clear2 Bright
    setValue Bright,0xFF

    clear PB0Counter
    clear PB1Counter
    clear2 PB0Mark
    clear2 PB1Mark

    clear currStock
    clear currPrice

    clear2 ADC_max_Counter
    clear2 ADC_min_Counter
    clear2 ADC_min
    clear2 ADC_max


    clear BacklightCounter
    clear2 pressKey
    clear2 BrightCounter
    clear2 BacklightOn
    incCounter BacklightOn

    clear2 Sound
    clear2 SoundCounter
    clear2 SoundState
    clear2 SoundStart

    clear QsecCounter
    clear2 StartCounter

    clear2 MotorSwitch
    clear2 MotorMode
    clear2 MotorCounter

    clear2 InsertCoin

    clear2 LEDBlinkMode
    clear2 BlinkMark
    clear2 ledBlinkCounter
    clear2 currPattern

    clear2 KeypadFree
    clear2 StarCounter

    ;########### Price and Stock Initial ###########
    rcall initPrice
	rcall initStock

	sei
	jmp main

;###################################
;          Main Program            #
;###################################
main:
    ;########### Start screen ###########
    StartScreen:
        do_lcd_data '2'
        do_lcd_data '1'
        do_lcd_data '2'
        do_lcd_data '1'
        do_lcd_data ' '
        do_lcd_data '1'
        do_lcd_data '7'
        do_lcd_data 'S'
        do_lcd_data '1'
        do_lcd_command 0b10001110 ; move cursor the 2nd last place
        do_lcd_data 'A'
        do_lcd_data '1'
        do_lcd_command 0b11000000 ; move cursor to the 2nd row
        do_lcd_data 'V'
        do_lcd_data 'e'
        do_lcd_data 'n'
        do_lcd_data 'd'
        do_lcd_data 'i'
        do_lcd_data 'n'
        do_lcd_data 'g'
        do_lcd_data ' '
        do_lcd_data 'M'
        do_lcd_data 'a'
        do_lcd_data 'c'
        do_lcd_data 'h'
        do_lcd_data 'i'
        do_lcd_data 'n'
        do_lcd_data 'e'

        StartLoop:
            rcall InputKey
            cpi keyRead, 0 ; equal 0, no valid key found
            brne selectScreen

            cpi screen, 0 ; checck if screen is updated
            breq StartLoop
            rjmp selectScreen

    ;########### Select screen ###########
    selectScreen:
    	do_lcd_command 0b00000001 ;clear all display
    	do_lcd_data 'S'
    	do_lcd_data 'e'
    	do_lcd_data 'l'
    	do_lcd_data 'e'
    	do_lcd_data 'c'
    	do_lcd_data 't'
    	do_lcd_data ' '
    	do_lcd_data 'i'
    	do_lcd_data 't'
    	do_lcd_data 'e'
    	do_lcd_data 'm'

        ldi screen, 1
    	ldi temp, 1
    	sts MotorMode, temp

        SelectWaitKey:
            ; if screen is changed to 2, go to admin screen
            cpi screen, 2
            breq jumpAdmin

            ; get key
            rcall InputKey

            cpi keyRead, 0
            breq SelectWaitKey ; when no input

            ; if it is less than 10
            ; then it is number

            cpi keyRead, 10
            brlo checkStock

            rjmp SelectWaitKey

        jumpAdmin:
            jmp adminScreen

        checkStock:
            ldi YL, low(Stock)
            ldi YH, high(Stock)

            ; get right index
            mov item, keyRead
            subi item, 1
            add YL, item
            clr temp
            adc YH, temp

            ; read the stock
            ld temp, Y

            ; if equal to zero, jmp empty screen
            cpi temp, 0
            breq emptyScreen

            ; when not equal, update the curr selected item's addr
    		sts currStock, YL
    		sts currStock + 1, YH

            rjmp coinScreen

    ;########### Empty screen ###########
    emptyScreen:
		PBenable
		; clear display first
		do_lcd_command 0b00000001
		do_lcd_data 'O'
		do_lcd_data 'u'
		do_lcd_data 't'
		do_lcd_data ' '
		do_lcd_data 'o'
		do_lcd_data 'f'
		do_lcd_data ' '
		do_lcd_data 's'
		do_lcd_data 't'
		do_lcd_data 'o'
		do_lcd_data 'c'
		do_lcd_data 'k'

		do_lcd_command 0b11000000 ;move cursor to the bottom right

		; set speaker to 1 sec
		setValue SoundStart, 4

		; convert back the item number
		mov temp, item
		inc temp
		ShowNum temp; show the currently selected item

		; start turn on LED
		setValue LEDBlinkMode, 1
		; clear number of blinks
		clear2 ledBlinkCounter

		; loop until led mode is back
		emptyStart:
			lds temp, LEDBlinkMode
			cpi temp, 0 ; check if it is default
			brne emptyStart

		emptyEnd:
			PBdisable
			; turn off sound
			clear2 Sound
			rjmp selectScreen

	rjmp loop

    ;########### Coin screen ###########
    coinScreen:
		; clear display first
		do_lcd_command 0b00000001
		do_lcd_data 'I'
		do_lcd_data 'n'
		do_lcd_data 's'
		do_lcd_data 'e'
		do_lcd_data 'r'
		do_lcd_data 't'
		do_lcd_data ' '
		do_lcd_data 'c'
		do_lcd_data 'o'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data 's'

		; find price of item
		ldi YH, high(Price)
		ldi YL, low(Price)
		; item is already sub by 1
		add YL, item
		clr temp
		adc YH, temp
		ld coin, Y

		coinKey:

			; set cursor to bottom
			do_lcd_command 0b11000000
			; show number of coins left
			lds temp, insertedCoins_live
			lsr temp ; divide it by two

			LEDShowNum temp ;show number of coins inserted
			ShowNum coin

			ldi temp, 0xFF
			delayCoin:
				dec temp
				brne delayCoin

			readPot

			; check pot flag
			; only if both are set, go next
			lds temp, ADC_min
			cpi temp, 1
			brne CancelKey

			lds temp, ADC_max
			cpi temp, 1
			brne CancelKey

			; now clear both flags
			clear2 ADC_min
			clear2 ADC_max

			; decrease number of coins needed
			; when it's more than 1
			subi coin, 1
			incCounter insertedCoins_live
			incCounter insertedCoins_live

			cpi coin, 0
			breq deliverScreen

			CancelKey:
				rcall InputKey
				cpi keyRead, '#'
				brne jumpCoinKey

				; now clear both flags before go back
				clear2 ADC_min
				clear2 ADC_max

				; use a backup value so that they don't interrupt
				lds temp, insertedCoins_live
				sts InsertCoin, temp
				clear2 insertedCoins_live

				; clear LED
				clr temp
				out portc, temp

				rjmp selectScreen

				jumpCoinKey:
					jmp coinKey

    ;########### Deliver screen ###########

    deliverScreen:
		; set speaker to one sec
		setValue SoundStart, 4

		; Update delievered item count
		; load selected item into Y register
		lds YL, currStock
		lds YH, currStock+1

		; decrease the number of item in the stock
		ld temp, Y
		dec temp
		; write back
		st Y, temp

		do_lcd_command 0b00000001 ;clear all display
		do_lcd_data 'D'
		do_lcd_data 'e'
		do_lcd_data 'l'
		do_lcd_data 'i'
		do_lcd_data 'v'
		do_lcd_data 'e'
		do_lcd_data 'r'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data 'g'
		do_lcd_data ' '
		do_lcd_data 'i'
		do_lcd_data 't'
		do_lcd_data 'e'
		do_lcd_data 'm'

		; clear all inserted coins
		clear2 insertedCoins_live

		; set motor to on mode
		setValue MotorMode, 2
		; LED to blink mode
		setValue LEDBlinkMode ,1

		; keeps turning motor until its mode is changed
		deliverStart:
			lds temp, MotorMode
			cpi temp, 2
			brne deliverEnd
			rjmp deliverStart

		deliverEnd:

		jmp selectScreen

    ;############## Admin Mode #####################
    adminScreen:
		; set speaker to one sec
		setValue SoundStart, 4

		do_lcd_command 0b00000001 ;clear all display
		do_lcd_data 'A'
		do_lcd_data 'd'
		do_lcd_data 'm'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data ' '
		do_lcd_data 'm'
		do_lcd_data 'o'
		do_lcd_data 'd'
		do_lcd_data 'e'

		ldi temp, 0xFF
		adminDelay:
			dec temp
			brne adminDelay

		; set default item to be item 1
		ldi item, 1
		PBenable

		adminStart:

			; show number selected
			do_lcd_command 0b10001011 ;set cursor to item place
			ShowNum item

			; show stock value
			do_lcd_command 0b11000000 ; set cursor to bottom left
			do_lcd_data '#'

			; fetch stock value
			ldi YL, low(Stock)
			ldi YH, high(Stock)

			; find stock addr
			mov temp, item
			subi temp, 1

			clr temp1
			add YL, temp
			adc YH, temp1
			; store addr
			sts currStock, YL
			sts currStock+1, YH

			; load stock number
			ld temp, Y
			; show stock num
			ledShowNum temp
			ShowNum temp

			do_lcd_data ' '
			do_lcd_data ' '

			; show price
			do_lcd_command 0b11001110 ; move cursor to second line 2nd right
			; show dollar sign
			do_lcd_data '$'

			; fetch price value
			ldi YL, low(Price)
			ldi YH, high(Price)

			; find price addr
			mov temp, item
			subi temp, 1

			clr temp1
			add YL, temp
			adc YH, temp1

			; store addr
			sts currPrice, YL
			sts currPrice+1, YH

			; load price number
			ld temp, Y
			; show price num
			ShowNum temp

			; try get new item value

			adminNewInput:
				rcall InputKey
				cpi keyRead, 0
				breq jumpAdminStart;if no input, go back

				cpi keyRead, 10 ; check if input is number
				brlo adminChange

				; check if input is A
				cpi keyRead, 'A'
				breq adminA

				cpi keyRead, 'B'
				breq adminB

				cpi keyRead, 'C'
				breq adminC

				cpi keyRead, '#'
				breq adminHash

				rjmp adminStart


				adminA:
					; fetch current price
					ld temp, Y
					cpi temp, 3
					breq jumpAdminStart

					; write back to price array
					inc temp
					st Y, temp
					rjmp adminStart

				adminB:
					; fetch current price
					ld temp, Y
					cpi temp, 1
					breq jumpAdminStart

					; write back to price array
					dec temp
					st Y, temp
					rjmp adminStart

				adminC:
					; load stock addr to Y
					lds YL, currStock
					lds YH, currStock+1

					; write empty value back
					clr temp
					st Y, temp
					rjmp adminStart

				adminHash:
					rjmp adminEnd

				adminChange:
					; if get input, move to item
					mov item, keyRead

			jumpAdminStart:
				jmp adminStart

		adminEnd:
		PBdisable

		; turn off LED
		clr temp
		out portc, temp
        out portg, temp
		jmp selectScreen


	rjmp loop

loop:
	rjmp loop




;##################################################
; FUNCTIONS
;##################################################

;##################################################
; LEDs
;##################################################

LEDShowFunction:
	push temp2
	push temp
	push r17
	clr r17

	cpi temp, 0
	breq LEDshow

	; check if number is 9
	cpi temp, 9
	breq show_9

	cpi temp, 10 ; when it is more or equal to 10, show 10
	brsh show_10

	;show number
		ldi temp2, 1
		ldi r17, 1

	ledShift:
		cp temp2, temp
		breq LEDshow
		lsl r17
		inc r17
		inc temp2
		rjmp ledShift

	LEDshow:
		out PORTC, r17
		clr temp
		out portg, temp
		rjmp LEDshowEnd

	show_9:
		ser temp
		out portc, temp
		ldi temp, 0x01
		out portg, temp
		rjmp LEDshowEnd

	show_10:
		ser temp
		out portc, temp
		ldi temp, 0x03
		out portg, temp
		rjmp LEDshowEnd

	LEDshowEnd:
		pop r17
		pop temp
		pop temp2
		ret


;##################################################
; LCD
;##################################################
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

lcd_command:
	out PORTF, r16
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret


;##################################################
; INITS Price and Stock
;##################################################
initPrice:
	push YL
	push YH

	ldi YL, low(Price)
	ldi YH, high(Price)

	clr temp
	clr temp2

	PriceLoop:
		ldi temp2, 1
		st Y+, temp2
		ldi temp2, 2
		st Y+, temp2

		inc temp
		cpi temp, 4
		brne PriceLoop

	ldi temp2, 1
	st Y+, temp2

	pop YH
	pop YL
	ret

initStock:
	push YL
	push YH
	ldi YL, low(Stock)
	ldi YH, high(Stock)

	clr temp
	ldi temp2, 1

	StockLoop:
		st Y+, temp2
		inc temp2
		inc temp
		cpi temp, 9
		brne StockLoop

	pop YH
	pop YL
	ret



;##################################################
; KEYBOARD
;##################################################
InputKey:
	push r20
	push r21
	push r22
	push r23
	clr r20
	clr r21
	clr r22
	clr r23

	getKey:
		rcall sleep_1ms
		ldi r21, INITCOLMASK ; initial column mask
		clr r23 ; initial

	colLoop:
		cpi r23, 4
		breq notFound; If all keys are scanned, no input detected, go to not found
		sts PORTL, r21 ; or scan a column.
		rcall sleep_1ms

		lds temp, PINL ; Read PORTL
		andi temp, ROWMASK ; Get the output value
		cpi temp2, 0xF ; if any row is low
		breq nextCol

		; find the row which is low
		ldi r20, INITROWMASK ; Initialize for row check
		clr r22 ;

	rowLoop:
		cpi r22, 4
		breq nextCol ; the row scan is over.
		mov temp2, temp ;temp kept input from pinL
		and temp2, r20 ; check unmasked bit

		breq convert ; if bit is clear, the key is pressed
		inc r22 ; move to the next row
		lsl r20
		andi r20, 0x0F ;

		jmp rowLoop

	nextCol:
		; if row scan is over
		lsl r21
		inc r21 ; in order to make bit0 1
		inc r23 ; increase column value
		jmp colLoop ; go to the next column

	convert:
		cpi r23, 3 ; If the pressed key is in col.3
		breq letters ; got a letter
		; If the key is not in col.3
		cpi r22, 3 ; If the key is in row3,
		breq symbols ; we have a symbol or 0

		mov keyRead, r22 ; Otherwise we have a number in 1-9
		lsl keyRead
		add keyRead, r22
		add keyRead, r23 ; temp = row*3 + col
		subi keyRead, -1 ; Add the value of character ??1??
		jmp convertEnd

	letters:
		ldi keyRead, 'A'
		add keyRead, r22 ; Get the ASCII value for the key
		jmp convertEnd

	symbols:
		cpi r23, 0 ; Check if we have a star
		breq star
		cpi r23, 1 ; or if we have zero
		breq zero

		ldi keyRead, '#' ; if not we have hash
		jmp convertEnd
	star:
		ldi keyRead, '*' ; Set to star
		jmp convertEnd

	zero:
		ldi keyRead, '0' ; Set to zero
		jmp convertEnd

	notFound:
		; turn down the released flag
		clear2 pressKey ; no keys are pressed at current moment
		clear2 KeypadFree ; 0: already released, 1: not yet released

	failGet:
		ldi keyRead, 0
		jmp keyboardEnd


	convertEnd:
		; store pressed key
		sts pressKey, keyRead

		lds temp, KeypadFree
		cpi temp, 0
		brne failGet

		; set 1 qsec speaker sound
		setValue SoundStart, 1

		clr temp
		; delay first before return when found value
		; in order to debounce (200ms)
		delayLoop:
		    cpi temp, 40
		    breq checkPressed
		    rcall sleep_5ms
		    inc temp
		    rjmp delayLoop

	checkPressed:
		lds temp, PINL
		andi temp, ROWMASK
		cpi temp, 0x0F
		; if equal, means released
		breq released

		; not released yet
		ldi temp, 1
		sts KeypadFree, temp
		rjmp keyboardEnd

	released:
		clear2 KeypadFree

;################ EPILOGUE ##################
	keyboardEnd:
		pop r23
		pop r22
		pop r21
		pop r20
		ret
