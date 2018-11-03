;* ALARM.ASM - A simple memory-resident program that beeps the speaker
;* at a prearranged time.  Can be loaded more than once for multiple
;* alarm settings.  During installation, ALARM establishes a handler
;* for the timer interrupt (interrupt 08).  It then terminates through
;* the Terminate-and-Stay-Resident function (function 31h).  After the
;* alarm sounds, the resident portion of the program retires by setting
;* a flag that prevents further processing in the handler.

        .MODEL tiny                     ; Create ALARM.COM
        .STACK
;;; The model directive determins how the program is to be arranged in
;;; memory, and specifically how to arrange the segments.  In the 'tiny'
;;; model the CS, DS and SS should be the same value.
;;;
;;; Since tiny is used here, masm assumes you want a .com file.


        .CODE

        ORG     5Dh                     ; Location of time argument in PSP,
CountDown       LABEL   WORD            ;   converted to number of 5-second
                                        ;   intervals to elapse
;;; The PSP is the "Program Segment Prefix," an area of memory loaded just
;;; prior to your program in memory containing information about the program
;;; as DOS sees it.  The offset 5Dh here is referring to an area known as a
;;; FCB or "File Control Block."  However, these are no longer used and DOS
;;; stores the first few bytes of the command line here, all this is doing
;;; is looking at the first command line arguments.


        .STARTUP
        jmp     Install                 ; Jump over data and resident code
;;; Since this is a TSR and DOS is so limited in memory, we want to unload
;;; our program along with any of the code and data we're no longer using.
;;; To do this, we put our setup routines at the end of the program, so we
;;; can tell DOS to discard them and reuse that memory for the next program
;;; loaded.


; Data must be in code segment so it won't be thrown away with Install code.

OldTimer        DWORD   ?               ; Address of original timer routine
tick_91         BYTE    91              ; Counts 91 clock ticks (5 seconds)
TimerActiveFlag BYTE    0               ; Active flag for timer handler
Error_msg	BYTE	0dh,0ah
		BYTE	'The time must be between 0000 and 2359 !'
		BYTE	0dh,0ah,'$'
;;; We make sure the data we need is in the code segment.  Obviously we're
;;; doing this already since we're making a COM file, but here we know the
;;; linker won't put it after the code segment or anything.  When writing a
;;; TSR you have to be very careful of where you place code and data.


;* NewTimer - Handler routine for timer interrupt (interrupt 08).
;* Decrements CountDown every 5 seconds.  No other action is taken
;* until CountDown reaches 0, at which time the speaker sounds.

;;; This is the interrupt handler for the timer.  We'll be modifying the
;;; interrupt vector table to install this, and storing the old handler in
;;; OldTimer in the Install procedure.


NewTimer PROC   FAR

        .IF     cs:TimerActiveFlag != 0 ; If timer busy or retired,
        jmp     cs:OldTimer             ;   jump to original timer routine
        .ENDIF
;;; The .IF and .ENDIF emit the appropriate cmp and jmp instructions, as well
;;; as the label to jump to.  x86 has tens of jmp instructions, this
;;; simplifies using them considerably.


        inc     cs:TimerActiveFlag      ; Set active flag
        pushf                           ; Simulate interrupt by pushing flags,
        call    cs:OldTimer             ;   then far-calling original routine
;;; We hijacked the old timer interrupt service, here we call it before we
;;; execute our own code.  We have to emulate the INT instruction to call it
;;; correctly which pushes flags then does a far call, so that's what we're
;;; doing here.
;;;
;;; We also increment TimerActiveFlag which prevents multiple copies of this
;;; procedure from being run at the same time.


        sti                             ; Enable interrupts
;;; We're done doing interrupt tasks, the remaining code in the function
;;; doesn't modifying the internal state of DOS, the BIOS or anything else.
;;; From this point on it might as well be a normal function so turn
;;; interrupts back on.  It's best to turn interrupts back on as quickly
;;; as possible so no interrupts are missed.


        push    ds                      ; Preserve DS register
        push    cs                      ; Point DS to current segment for
        pop     ds                      ;   further memory access
        dec     tick_91                 ; Count down for 91 ticks
;;; This timer fires at 18.2Hz and we want to check every 5 seconds.
;;; 5 seconds * 18.2Hz = 91 ticks


        .IF     zero?                   ; If 91 ticks have elapsed,
        mov     tick_91, 91             ;   reset secondary counter and
        dec     CountDown               ;   subtract one 5-second interval
        .IF     zero?                   ; If CountDown drained,
        call    Sound                   ;   sound speaker
        inc     TimerActiveFlag         ; Alarm has sounded, set flag
        .ENDIF
        .ENDIF
;;; Note that TimerActiveFlag is incremented twice (here and at the top of
;;; the procedure), but decremented once.  After the beep plays it will
;;; never play again.


        dec     TimerActiveFlag         ; Decrement active flag
        pop     ds                      ; Recover DS
        iret                            ; Return from interrupt handler

NewTimer ENDP


;* Sound - Sounds speaker with the following tone and duration:

BEEP_TONE       EQU     440             ; Beep tone in hertz
BEEP_DURATION   EQU     6               ; Number of clocks during beep,
                                        ;   where 18 clocks = approx 1 second

Sound   PROC    USES ax bx cx dx es     ; Save registers used in this routine
        mov     al, 0B6h                ; Initialize channel 2 of
        out     43h, al                 ;   timer chip
;;; The timer chip is used not only to fire the timer interrupt, but also to
;;; drive the PC speaker.  The 43h IO port controls the modes of operation for
;;; the timer chip.  Channel 0 is what is used to generate the int 8 timer
;;; so don't touch that.  Channel 1 is used to refresh DRAM memory, don't
;;; touch that either.  Channel 2 is used to control the PC speaker.
;;;
;;; The byte being written here ins 0B6h.
;;;   10------  Select timer channel 2
;;;   --11----  Load LSB, then MSB in future writes
;;;   ----101-  Send output to PC speaker
;;;   -------0  Process data as binary
;;;
;;; So all together we're selecting channel 2, telling it we're about to send
;;; it 2 bytes for the frequency, send the output of the timer channel 2 to
;;; the speaker and the data we're sending is binary, not BCD.


        mov     dx, 12h                 ; Divide 1,193,180 hertz
        mov     ax, 34DCh               ;   (clock frequency) by
        mov     bx, BEEP_TONE           ;   desired frequency
        div     bx                      ; Result is timer clock count
;;; In x86 there is no way to load ax:dx with a 32-bit value directly.  Instead
;;; you have to load it manually and that's what's being done here: loading
;;; 1,192,180 into ax:dx.  However, I don't see why it's really being done,
;;; can't all that be done with the MASM preprocessor or as a constant?


        out     42h, al                 ; Low byte of count to timer
        mov     al, ah
        out     42h, al                 ; High byte of count to timer
;;; And here we write low and high bytes to the timer, as we configured above.


        in      al, 61h                 ; Read value from port 61h
        or      al, 3                   ; Set first two bits
        out     61h, al                 ; Turn speaker on
;;; Now we need to talk to the Programmable Peripheral Interface (PPI) chip 
;;; to actually allow the timer signals to get to the speaker.


; Pause for specified number of clock ticks

        mov     dx, BEEP_DURATION       ; Beep duration in clock ticks
        sub     cx, cx                  ; CX:DX = tick count for pause
        mov     es, cx                  ; Point ES to low memory data
        add     dx, es:[46Ch]           ; Add current tick count to CX:DX
        adc     cx, es:[46Eh]           ; Result is target count in CX:DX
;;; The address 0000:045C contains a tick counter since CPU reset incremented
;;; once every 18.2Hz as a double word.  We're loading it into CX:DX here and
;;; adding BEEP_DURATION.


        .REPEAT
        mov     bx, es:[46Ch]           ; Now repeatedly poll clock
        mov     ax, es:[46Eh]           ;   count until the target
        sub     bx, dx                  ;   time is reached
        sbb     ax, cx
        .UNTIL  !carry?

        in      al, 61h                 ; When time elapses, get port value
        xor     al, 3                   ; Kill bits 0-1 to turn
        out     61h, al                 ;   speaker off
;;; Doing the opposite, turning off the bits that send the PC timer to the
;;; speaker.  I don't know why they're using xor al, 3 here though, as if
;;; something else touched the speaker in the meantime then this will turn
;;; it back on and leave it on.  Instead, and al, 11111100b would be a
;;; better option.


        ret

Sound   ENDP



;* Install - Converts ASCII argument to valid binary number, replaces
;* NewTimer as the interrupt handler for the timer, then makes program
;* memory resident by exiting through function 31h.
;*
;* This procedure marks the end of the TSR's resident section and the
;* beginning of the installation section.  When ALARM terminates through
;* function 31h, the above code and data remain resident in memory.  The
;* memory occupied by the following code is returned to DOS.


Install PROC

; Time argument is in hhmm military format.  Convert ASCII digits to
; number of minutes since midnight, then convert current time to number
; of minutes since midnight.  Difference is number of minutes to elapse
; until alarm sounds.  Convert to seconds-to-elapse, divide by 5 seconds,
; and store result in word CountDown.

DEFAULT_TIME    EQU     3600            ; Default alarm setting = 1 hour
                                        ;   (in seconds) from present time
        mov     ax, DEFAULT_TIME
        cwd                             ; DX:AX = default time in seconds


	sub	bx,bx			; Check if there is only numbers
	.REPEAT
	cmp	BYTE PTR CountDown[bx],'0'
	jb	time_err
	cmp	BYTE PTR CountDown[bx],'9'
	ja	time_err
	inc	bx
	.UNTIL	(bx == 4)
;;; Look at the command-line arguments and ensure that they're all digits
;;; from '0' to '9'.
;;;
;;; The .REPEAT and .UNTIL macros generate labels and cmp/jmp pairs
;;; respectively.


        xor     CountDown[0], '00'      ;   convert 4 bytes of ASCII
        xor     CountDown[2], '00'      ;   argument to binary
;;; This is a bit clever.  Normally to convert an ASCII '0' to 0 you would
;;; subtract '0', but here it's using xor to do 2 digits at a time.  This
;;; works because '0' is 00110000b, '1' is 00110001b, etc.  All you need to
;;; do is clear those top 2 bits and we know those are there because of the
;;; code just above.

                                        
        mov     al, 10                  ; Multiply 1st hour digit by 10
        mul     BYTE PTR CountDown[0]   ;   and add to 2nd hour digit
        add     al, BYTE PTR CountDown[1]
        mov     bh, al                  ; BH = hour for alarm to go off
        mov     al, 10                  ; Repeat procedure for minutes
        mul     BYTE PTR CountDown[2]   ; Multiply 1st minute digit by 10
        add     al, BYTE PTR CountDown[3] ;   and add to 2nd minute digit
        mov     bl, al                  ; BL = minute for alarm to go off
;;; Now you just need to combine the 2 digits to get the actual number for
;;; hours and minutes.


	cmp 	bh,23  			; Check if hour <=23
	jg	time_err
	cmp	bl,59  			; Check if minute <=59
	jnge	time_ok

time_err:
	mov	dx,offset Error_msg	; Print Error_msg
	mov	ah,9
	int	21h
	.exit				; exit and don't install
;;; The .exit macro generates a typical 4Ch call to DOS to terminate the
;;; program.


time_ok:
        mov     ah, 2Ch                 ; Request function 2Ch
        int     21h                     ; Get Time (CX = current hour/min)
;;; Function 2Ch of DOS gets the current system time with CH=hour, CL=minute,
;;; DH=seconds, DL=1/100 seconds


        mov     dl, dh
        sub     dh, dh
;;; DX=current seconds


        push    dx                      ; Save DX = current seconds

        mov     al, 60                  ; Multiply current hour by 60
        mul     ch                      ;   to convert to minutes
        sub     ch, ch
        add     cx, ax                  ; Add current minutes to result
                                        ; CX = minutes since midnight
;;; CX=minutes since midnight


        mov     al, 60                  ; Multiply alarm hour by 60
        mul     bh                      ;   to convert to minutes
        sub     bh, bh
        add     ax, bx                  ; AX = number of minutes since
                                        ;   midnight for alarm setting
;;; AX=alarm minutes since midnight


        sub     ax, cx                  ; AX = time in minutes to elapse
                                        ;   before alarm sounds
;;; AX=minutes until alarm
        .IF     carry?                  ; If alarm time is tomorrow,
        add     ax, 24 * 60             ;   add minutes in a day
        .ENDIF
;;; If the alarm time has already passed, add a full day

        mov     bx, 60
        mul     bx                      ; DX:AX = minutes-to-elapse-times-60
;;; DX:AX=seconds until alarm


        pop     bx                      ; Recover current seconds
        sub     ax, bx                  ; DX:AX = seconds to elapse before
        sbb     dx, 0                   ;   alarm activates
;;; Subtract seconds of current time from seconds to alarm
        .IF     carry?                  ; If negative,
        mov     ax, 5                   ;   assume 5 seconds
        cwd
        .ENDIF


        mov     bx, 5                   ; Divide result by 5 seconds
        div     bx                      ; AX = number of 5-second intervals
        mov     CountDown, ax           ;   to elapse before alarm sounds
;;; We're only running the check once every 5 seconds, so divide by 5
;;; and store in CountDown.  Finally, everything is set up and it's time to
;;; install the interrupt handler.


        mov     ax, 3508h               ; Request function 35h
        int     21h                     ; Get vector for timer (interrupt 08)
;;; DOS function 35h gets an interrupt vector.  This is preferable to reading
;;; the interrupt vector table directly to allow DOS to provide some
;;; future-proofing.  If for some reason interrupt 08h is moved in future PC
;;; models then DOS can help you out, but if you write directly to the
;;; interrupt vector table then things could break.  As I'm writing this in
;;; late 2018 this is a bit of a moot point, but writing to the IVT can be
;;; tricky so we'll let DOS handle it.


        mov     WORD PTR OldTimer[0], bx; Store address of original
        mov     WORD PTR OldTimer[2], es;   timer interrupt
        mov     ax, 2508h               ; Request function 25h
        mov     dx, OFFSET NewTimer     ; DS:DX points to new timer handler
        int     21h                     ; Set vector with address of NewTimer
;;; Store the old interrupt vector and install ours using function 25h.


        mov     dx, OFFSET Install      ; DX = bytes in resident section
        mov     cl, 4
        shr     dx, cl                  ; Convert to number of paragraphs
        inc     dx                      ;   plus one
;;; DOS wants memory sizes in paragraphs, not bytes.  Here we're calculating
;;; how much of the program to cut off.  We don't need the Install procedure
;;; anymore so we get the offset to it, divide by 16 and add one for any bytes
;;; that happened to be in the last paragraph we cut off when dividing by 16.


        mov     ax, 3100h               ; Request function 31h, error code=0
        int     21h                     ; Terminate-and-Stay-Resident
;;; And then this is where the magic happens.  Instead of exiting with 4Ch we
;;; exit with 31h which tells DOS to terminate the program but keep it in memory,
;;; keeping our interrupt handlers in memory.


Install ENDP

        END
