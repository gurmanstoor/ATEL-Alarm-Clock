; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0
BUTTON0		  equ P0.6
BUTTON1		  equ P0.4
BUTTON2       equ P0.0
BUTTON3		  equ P0.1
BUTTON4		  equ P0.2
BUTTON5       equ P0.5

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
A1_hr:		  ds 1 ; ds reserves 1 byte in memory
A1_min:		  ds 1
A1_sec:		  ds 1
A2_hr:		  ds 1
A2_min:		  ds 1
A2_sec:		  ds 1
Mode:		  ds 1
Temp:		  ds 1
Days:		  ds 1
Hours:		  ds 1
Minutes: 	  ds 1
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
al1_am_flag: dbit 1
al2_am_flag: dbit 1
am_flag: dbit 1
pause: dbit 1
weekday_flag: dbit 1

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'xx:xx:xx xx xxx', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    clr ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already. 
	
	cpl SOUND_OUT
	reti
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Go_To_Timer2 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Go_To_Timer2
	sjmp Cont_To_Timer2

Go_To_Timer2:
	ljmp Timer2_ISR_done

; change this to just add into INC_Done
Cont_To_Timer2:   
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	;jnb UPDOWN, Timer2_ISR_decrement <---------------------------------------------------------
	
Alarm_2:
	mov a, BCD_counter
	cjne a, A2_sec, Alarm_1
	mov a, Minutes
	cjne a, A2_min, Alarm_1
	mov a, Hours
	cjne a, A2_hr, Alarm_1
	mov a, Days
	cjne a, #0x06, Alarm_1
	cjne a, #0x07, Alarm_1
	sjmp Alarm_Trigger

Alarm_1:
	mov a, BCD_counter
	cjne a, A1_sec, Normal
	mov a, Minutes
	cjne a, A1_min, Normal
	mov a, Hours
	cjne a, A1_hr, Normal
	mov a, Days
	sjmp Alarm_Trigger
	
Alarm_Trigger:
	setb ET0

Normal:
	mov a, BCD_counter
	; check if at 59 s
	cjne a, #0x59, Inc_Second

Inc_Minute:
	clr a
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
	mov a, Minutes	
	cjne a, #0x59, Cont1
	
Inc_Hour:
	clr a
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Minutes, a

	mov a, Hours
	cjne a, #0x11, Next
	cpl am_flag
	jb am_flag, New_Day
	sjmp Next
	
New_Day:
	mov Temp, a
	
	mov a, Days
	cjne a, #0x07, Inc_Day
	mov a, #0x00
	sjmp Save_Day
	
Inc_Day:
	add a, #0x01

Save_Day:
	mov Days, a
	subb a, #0x05
	jc Weekday

Weekend: 
	setb weekday_flag
	cpl weekday_flag
	sjmp Next

Weekday: 
	setb weekday_flag
	
Next:	
	mov a, Temp
	cjne a, #0x12, Cont2

To_One:
	mov a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Hours, a
	sjmp Timer2_ISR_done

Cont2:	
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Hours, a
	sjmp Timer2_ISR_done

Cont1:		
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Minutes, a
	sjmp Timer2_ISR_done
	
Inc_Second:
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	sjmp Timer2_ISR_done	
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.

;Timer2_ISR_da:
;	da a ; Decimal adjust instruction.  Check datasheet for more details!
;	mov BCD_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti ;end ISR 

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
    setb am_flag
    setb al1_am_flag
    setb al2_am_flag
    setb pause
    cpl am_flag
	mov BCD_counter, #0x30
	mov Minutes, #0x59
	mov Hours, #0x11
	mov Days, #0x03
	mov Mode, #0x01
	mov A1_hr, #0x12
	mov A1_min, #0x00
	mov A1_sec, #0x00
	mov A2_hr, #0x12
	mov A2_min, #0x00
	mov A2_sec, #0x00
	mov weekday_flag, #0x00
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb BOOT_BUTTON, Change_Mode  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, Change_Mode  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	cpl pause
	jb pause, Stop
	setb TR2
	sjmp Change_Mode
	
Stop:
	clr ET0
	clr TR2                 ; Stop timer 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Change_Mode:
	mov a, Mode
	jb BUTTON3, Change_Day
	Wait_Milli_Seconds(#50)
	jb BUTTON3, Change_Day
	jb BUTTON3, $
	
	cjne a, #0x03, Add_Mode
	mov a, #0x01
	sjmp Change_Day

Add_Mode:
	add a, #0x01
	da a
	
Change_Day:
	mov Mode, a
	mov r0, a
	mov a, Days
	jb BUTTON5, Change_am_pm
	Wait_Milli_Seconds(#50)
	jb BUTTON5, Change_am_pm
	jb BUTTON5, $
	
	cjne a, #0x07, Add_Day
	mov a, #0x01
	sjmp Change_am_pm
	
Add_Day:
	add a, #0x01
	da a

Change_am_pm:
	mov Days, a
	
	jb BUTTON4, Change_Seconds
	Wait_Milli_Seconds(#50)
	jb BUTTON4, Change_Seconds
	jb BUTTON4, $
	
	cjne r0, #1, amcase2
amcase1:
	cpl am_flag
	sjmp Change_Seconds
amcase2:
	cjne r0, #2, amcase3
	cpl al1_am_flag
	sjmp Change_Seconds
amcase3:
	cpl al2_am_flag		
	
Change_Seconds:	

	cjne r0, #1, seccase2
seccase1:
	mov a, BCD_counter
	sjmp Change_Seconds_cont
seccase2:
	cjne r0, #2, seccase3
	mov a, A1_sec
	sjmp Change_Seconds_cont
seccase3:
	mov a, A2_sec

Change_Seconds_cont:
	jb BUTTON2, Change_Minutes
	Wait_Milli_Seconds(#50)
	jb BUTTON2, Change_Minutes
	jb BUTTON2, $
	
	cjne a, #0x59, Add_Sec
	mov a, #0x00
	sjmp Change_Minutes
	
Add_Sec:
	add a, #0x01
	da a
	
Change_Minutes:
	
	cjne r0, #1, seccase2b
seccase1b:
	mov BCD_counter, a
	sjmp Change_Minutes_cont
seccase2b:
	cjne r0, #2, seccase3b
	mov A1_sec, a
	sjmp Change_Minutes_cont
seccase3b:
	mov A2_sec, a
	
Change_Minutes_cont:
	cjne r0, #1, mincase2
mincase1:
	mov a, Minutes
	sjmp Change_Minutes_cont2
mincase2:
	cjne r0, #2, mincase3
	mov a, A1_min
	sjmp Change_Minutes_cont2
mincase3:
	mov a, A2_min
	
Change_Minutes_cont2:
	
	jb BUTTON1, Change_Hours
	Wait_Milli_Seconds(#50)
	jb BUTTON1, Change_Hours
	jb BUTTON1, $
	
	cjne a, #0x59, Add_Min
	mov a, #0x00
	sjmp Change_Hours
	
Add_Min:
	add a, #0x01
	da a

Change_Hours:

	cjne r0, #1, mincase2b
mincase1b:
	mov Minutes, a
	sjmp Change_Hours_cont
mincase2b:
	cjne r0, #2, mincase3b
	mov A1_min, a
	sjmp Change_Hours_cont
mincase3b:
	mov A2_min, a
	
Change_Hours_cont:

	cjne r0, #1, hrcase2
hrcase1:
	mov a, Hours
	sjmp Change_Hours_cont2
hrcase2:
	cjne r0, #2, hrcase3
	mov a, A1_hr
	sjmp Change_Hours_cont2
hrcase3:
	mov a, A2_hr

Change_Hours_cont2:

	jb BUTTON0, End_Change
	Wait_Milli_Seconds(#50)
	jb BUTTON0, End_Change
	jb BUTTON0, $
	
	cjne a, #0x12, Add_Hr
	mov a, #0x01
	sjmp End_Change
	
Add_Hr:
	add a, #0x01
	da a

End_Change:	
	cjne r0, #1, hrcase2b
hrcase1b:
	mov Hours, a
	sjmp LoopBack
hrcase2b:
	cjne r0, #2, hrcase3b
	mov A1_hr, a
	sjmp LoopBack
hrcase3b:
	mov A2_hr, a

LoopBack:
	ljmp loop_b

loop_a:
	jnb half_seconds_flag, Go_To_Loop

Go_To_Loop:
	ljmp loop

loop_b:
    mov a, Mode
    cjne a, #0x01, go_to_alarm1
	sjmp Time
	
go_to_alarm1:
	ljmp Alarm1
    
Time:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 4)
	Display_BCD(Minutes)
	Set_Cursor(1, 1)
	Display_BCD(Hours)
	
	mov a, Days
	cjne a, #0x01, Mon
	
Sun:
	Set_Cursor(1, 13)
	Display_char(#'S')
	Set_Cursor(1, 14)
	Display_char(#'U')
	Set_Cursor(1, 15)
	Display_char(#'N')
	ljmp disp_mode1

Mon:
	cjne a, #0x02, Tue
	Set_Cursor(1, 13)
	Display_char(#'M')
	Set_Cursor(1, 14)
	Display_char(#'O')
	Set_Cursor(1, 15)
	Display_char(#'N')
	ljmp disp_mode1

Tue:
	cjne a, #0x03, Wed
	Set_Cursor(1, 13)
	Display_char(#'T')
	Set_Cursor(1, 14)
	Display_char(#'U')
	Set_Cursor(1, 15)
	Display_char(#'E')
	ljmp disp_mode1

Wed:
	cjne a, #0x04, Thu
	Set_Cursor(1, 13)
	Display_char(#'W')
	Set_Cursor(1, 14)
	Display_char(#'E')
	Set_Cursor(1, 15)
	Display_char(#'D')
	ljmp disp_mode1

Thu:
	cjne a, #0x05, Fri
	Set_Cursor(1, 13)
	Display_char(#'T')
	Set_Cursor(1, 14)
	Display_char(#'H')
	Set_Cursor(1, 15)
	Display_char(#'U')
	ljmp disp_mode1

Fri:
	cjne a, #0x06, Sat
	Set_Cursor(1, 13)
	Display_char(#'F')
	Set_Cursor(1, 14)
	Display_char(#'R')
	Set_Cursor(1, 15)
	Display_char(#'I')
	ljmp disp_mode1

Sat:
	Set_Cursor(1, 13)
	Display_char(#'S')
	Set_Cursor(1, 14)
	Display_char(#'A')
	Set_Cursor(1, 15)
	Display_char(#'T')
		
disp_mode1:
	Set_Cursor(2, 2)
	Display_char(#'C')
	Set_Cursor(2, 3)
	Display_char(#'L')
	Set_Cursor(2, 4)
	Display_char(#'K')
	
am_or_pm:
	Set_Cursor(1, 10)
	jb am_flag, disp_am
	Display_char(#'P')
	Set_Cursor(1, 11)
	Display_char(#'M')
	
	ljmp loop
	
disp_am:
	Display_char(#'A')
	Set_Cursor(1, 11)
	Display_char(#'M')	
    ljmp loop
    
Alarm1: 
	cjne a, #0x02, second_half
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(A1_sec) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 4)
	Display_BCD(A1_min)
	Set_Cursor(1, 1)
	Display_BCD(A1_hr)
	Set_Cursor(1, 10)

second_half:
	cjne a, #0x02, disp_al1

	Set_Cursor(1, 13)
	Display_char(#'W')
	Set_Cursor(1, 14)
	Display_char(#'D')
	Set_Cursor(1, 15)
	Display_char(#'Y')

disp_al1:
	cjne a, #0x02, al1_am_or_pm
	Set_Cursor(2, 2)
	Display_char(#'A')
	Set_Cursor(2, 3)
	Display_char(#'L')
	Set_Cursor(2, 4)
	Display_char(#'1')

al1_am_or_pm:
	cjne a, #0x02, Alarm2
	Set_Cursor(1, 10)
	jb al1_am_flag, al1_disp_am
	Display_char(#'P')
	Set_Cursor(1, 11)
	Display_char(#'M')
	ljmp loop
	
al1_disp_am:
	Display_char(#'A')
	Set_Cursor(1, 11)
	Display_char(#'M')	
    ljmp loop
		
Alarm2:
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(A2_sec) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 4)
	Display_BCD(A2_min)
	Set_Cursor(1, 1)
	Display_BCD(A2_hr)
	Set_Cursor(1, 10)

	Set_Cursor(1, 13)
	Display_char(#'W')
	Set_Cursor(1, 14)
	Display_char(#'N')
	Set_Cursor(1, 15)
	Display_char(#'D')
	
disp_al2:
	Set_Cursor(2, 2)
	Display_char(#'A')
	Set_Cursor(2, 3)
	Display_char(#'L')
	Set_Cursor(2, 4)
	Display_char(#'2')

al2_am_or_pm:
	Set_Cursor(1, 10)
	jb al1_am_flag, al2_disp_am
	Display_char(#'P')
	Set_Cursor(1, 11)
	Display_char(#'M')
	ljmp loop
	
al2_disp_am:
	Display_char(#'A')
	Set_Cursor(1, 11)
	Display_char(#'M')	
    ljmp loop
		
END
