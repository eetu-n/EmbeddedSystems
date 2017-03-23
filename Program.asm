@DATA

	ldisk		DS	1
	cdisk		DS	2
	switch		DS	3
	error		DS	4
	emergency	DS	5
	dcount		DS	6
	state		DS	7
	tmr			DS	8
	cerror		DS	9
	echeck		DS	10

@CODE

	IOAREA	   EQU	-16				; address of the I/O-Area, modulo 2^18
	INPUT	   EQU	7				; position of the input buttons (relative to IOAREA)
	OUTPUT	   EQU	11				; relative position of the power outputs
	DSPDIG	   EQU	9				; relative position of the 7-segment display's digit selector
	DSPSEG	   EQU	8				; relative position of the 7-segment display's segments
	TIMER	   EQU	13
	ADCONVS	   EQU	6

	begin:	   BRA	main			; skip subroutine Hex7Seg
;
Hex7Seg:	   BRS	Hex7Seg_bgn		; push address(tbl) onto stack and proceed at "bgn"
Hex7Seg_tbl:  CONS	%01111110		; 7-segment pattern for '0'
			  CONS	%00110000		; 7-segment pattern for '1'
			  CONS	%01101101		; 7-segment pattern for '2'
			  CONS	%01111001		; 7-segment pattern for '3'
			  CONS	%00110011		; 7-segment pattern for '4'
			  CONS	%01011011		; 7-segment pattern for '5'
			  CONS	%01011111		; 7-segment pattern for '6'
			  CONS	%01110000		; 7-segment pattern for '7'
			  CONS	%01111111		; 7-segment pattern for '8'
			  CONS	%01111011		; 7-segment pattern for '9'
			  CONS	%01110111		; 7-segment pattern for 'A'
			  CONS	%00011111		; 7-segment pattern for 'b'
			  CONS	%01001110		; 7-segment pattern for 'C'
			  CONS	%00111101		; 7-segment pattern for 'd'
			  CONS	%01001111		; 7-segment pattern for 'E'
			  CONS	%01000111		; 7-segment pattern for 'F'
Hex7Seg_bgn:   AND	R0	%01111		; R3 := R0 MOD 16 , just to be safe...
			  LOAD	R1	[SP++]		; R4 := address(tbl) (retrieve from stack)
			  LOAD	R1	[R1+R0]		; R4 := tbl[R0]
			   RTS
;
;      The body of the main program
;
	main:	  LOAD	R0	tmrisr
			   ADD	R0	R5
			  LOAD	R1	16
			  STOR	R0	[R1]
			  LOAD	R0	0
			  STOR	R0	[GB + state]
			  LOAD	R0	0
			  STOR	R0	[GB + dcount]
			  LOAD	R5	IOAREA			; R5 := "address of the area with the I/O-registers"
			  LOAD	R0	0
			   BRS	Hex7Seg
			  STOR	R1	[R5 + DSPSEG]
			  LOAD	R1	%0100000
			  STOR	R1	[R5	+ DSPDIG]
			   BRA	start
;
;	Initial state
;
	start:	  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01000000
			   CMP	R1	%01000000
			   BEQ	recover
			  LOAD	R1	[R5	+ INPUT]
			   AND	R1	%010000000
			   CMP	R1	%010000000
			   BNE	start
;
			  LOAD	R4	0
			  STOR	R4	[GB + cdisk]
			  STOR	R4	[GB + ldisk]
			  LOAD	R3	%0100			; Set LED on
			  STOR	R3	[R5 + OUTPUT]
			  LOAD	R2	999999999
	loop5:	   BRS	wait1
			   SUB	R2	1
			   CMP	R2	0
			   BNE	loop5
			  LOAD	R1	[R5 + ADCONVS]	; Load sensor reading to R1
			   CMP	R1	%011100110		; Check if R1 is less than this value; disk is white
			   BMI	skip3
			  LOAD	R4	1
			  STOR	R4	[GB + cdisk]
			  STOR	R4	[GB + ldisk]
			  
	skip3:	  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
			   BRA	fcheck
;
;	End of initial state
;

;
;	Pushing state
;
	push:	  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	10000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	4
			  STOR	R4	[GB + cerror]
			  LOAD	R4	1
			  STOR	R4	[GB + state]
			  LOAD	R4	[GB + dcount]
			   ADD	R4	1
			  STOR	R4	[GB + dcount]
			   BRS	sod
			   BRS	Hex7Seg
			  STOR	R1	[R5 + DSPSEG]
			  LOAD	R1	%0100000
			  STOR	R1	[R5 + DSPDIG]
	ploop:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R3	%01000
			   XOR	R3	%01				; Load 1 to R3
			  STOR	R3	[R5 + OUTPUT]	; Store R3 in output, turn on output1
			  LOAD	R2	4				; Loop 4 times
	loop:	   BRS	wait1				; Wait 1 tick
			   SUB	R2 1
			   CMP	R2 0
			   BNE	loop
			   XOR	R3	%01				; Load 0 to R3
			  STOR	R3	[R5 + OUTPUT]	; Store R3 in output, turn off output1
			   BRS	wait1				; wait for 1 tick
;
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01				; Only consider switch 1
			  LOAD	R4	[GB + switch]
			   CMP	R4	1				; Check if switch has been on
			   BNE	skip				; If not equal, skip this step
			   CMP	R1	0				; Check if switch 1 is pressed
			   BEQ	fcheck				; If not, branch to precheck
			   BRA  ploop				; If yes, loop to ploop
			   
	skip:	   BRS	oncheck				; BRS to oncheck
			   BRA	ploop				; Loop back to ploop
;
	oncheck:   CMP	R1 %01				; If R1 is 1
			   BNE	return				; NOT return
			  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	10000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	5
			  STOR	R4	[GB + cerror]
;
	floop:	  LOAD	R1	[R5 + INPUT]
			   AND	R1	%0100
			  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			   CMP	R1	%0100
			   BEQ	floop
			  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	10000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	4
			  STOR	R4	[GB + cerror]
			  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
			  LOAD	R4	1				; DO set switch to 1
			  STOR	R4	[GB + switch]
			   RTS						; return
;
;	End of pushing state
;

;
;	Turning state
;
	turn:	  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	20000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	3
			  STOR	R4	[GB + cerror]
			  LOAD	R4	0				; Set switch to 0
			  STOR	R4	[GB + switch]
			  LOAD	R0	2
			  STOR	R0	[GB + state]
			   BRS	sod
			   BRS	Hex7Seg
			  STOR	R1	[R5 + DSPSEG]
			  LOAD	R1	%0100000
			  STOR	R1	[R5 + DSPDIG]
;
	turn1:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	ch2
			   XOR	R3	%010			; Set motor 2 on
			  STOR	R3	[R5 + OUTPUT]	
			  LOAD	R2	8				; loop 4 times
	loop2:	   BRS	wait1				; Wait 1 tick
			   SUB	R2	1
			   CMP	R2	0
			   BNE	loop2
			   XOR	R3	%010			; Turn off motor 2
			  STOR	R3	[R5 + OUTPUT]
			   BRS	wait1				; Wait 1 tick
;
			  LOAD	R1	[R5 + INPUT]	; Load input to R1
			   AND	R1	%01010			; Consider only bucket switches
			  LOAD	R4	[GB + switch]
			   CMP	R4	1
			   BNE	skip2
			   CMP	R1	0
			   BEQ	turn3
			   BRA	turn1
			   
	skip2:	   BRS	oncheck2
			   BRA	turn1				; If not, loop to turn2
;
	turn3:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			   XOR	R3	%010			; Set motor 2 on
			  STOR	R3	[R5 + OUTPUT]	
			  LOAD	R2	4				; loop 4 times
	loop3:	   BRS	wait1				; Wait 1 tick
			   SUB	R2	1
			   CMP	R2	0
			   BNE	loop3
			   XOR	R3	%010			; Turn off motor 2
			  STOR	R3	[R5 + OUTPUT]
			   BRS	wait1				; Wait 1 tick
			  
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%010
			   CMP	R1	%010
			   BEQ	scheck
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01000
			   CMP	R1	%01000
			   BEQ	scheck2
			   BRA	turn3
;
	oncheck2: LOAD	R1	[R5 + INPUT]
			   AND	R1	%010
			   CMP	R1	%010			; If switch 2 is pressed
			   BNE	scheck3				; NOT return
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01000
			   CMP	R1	%01000
			   BNE	scheck4
			  LOAD	R4	1				; DO set switch to 1
			  STOR	R4	[GB + switch]
			   RTS						; return
;
	scheck:	  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	1000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	2
			  STOR	R4	[GB + cerror]
	sloop:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01000
			   CMP	R1	%01000
			   BEQ	reset
			   BRA	sloop
;
	scheck2:  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	1000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	6
			  STOR	R4	[GB + cerror]
	sloop2:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%010
			   CMP	R1	%010
			   BEQ	reset	
			   BRA	sloop2
;
	scheck3:  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	1000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	7
			  STOR	R4	[GB + cerror]
	sloop3:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01000
			   CMP	R1	%01000
			   BNE	return
			   BRA	sloop3
;
	scheck4:  CLRI	8
			  LOAD	R4	0
			   SUB	R4	[R5 + TIMER]
			  STOR	R4	[R5 + TIMER]
			  LOAD	R4	1000
			  STOR	R4	[R5 + TIMER]
			  SETI	8
			  LOAD	R4	8
			  STOR	R4	[GB + cerror]
	sloop4:	  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%010
			   CMP	R1	%010
			   BNE	return
			   BRA	sloop4
;
;	End of turning state
;

;
;	Disk detection state
;
	fcheck:	  CLRI	8
			  LOAD	R4	3
			  STOR	R4	[GB + state]
			   BRS	sod
			   BRS	Hex7Seg
			  STOR	R1	[R5 + DSPSEG]
			  LOAD	R1	%0100000
			  STOR	R1	[R5 + DSPDIG]
			  LOAD	R4	[GB + echeck]
			   CMP	R4	1
			   BEQ	error
			  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
;
			  LOAD	R3	%010000
			  STOR	R3	[R5 + OUTPUT]
			  LOAD	R2	999999999
	loop6:	   BRS	wait1
			   SUB	R2	1
			   CMP	R2	0
			   BNE	loop6
			  LOAD	R1	[R5 + ADCONVS]
			  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
			   CMP	R1	%000010000
			   BMI	rset
;
	buck:	  LOAD	R3	%0100			; Set LED on
			  STOR	R3	[R5 + OUTPUT]
			  LOAD	R2	999999999
	loop4:	   BRS	wait1
			   SUB	R2	1
			   CMP	R2	0
			   BNE	loop4
			  LOAD	R1	[R5 + ADCONVS]	; Load sensor reading to R1
			   CMP	R1	%011100110		; Check if R1 is less than this value
			   BMI	buck2				; If yes, branch to buck2
			  LOAD	R4	1
			  STOR	R4	[GB + cdisk]
			  LOAD	R0	[GB + ldisk]
			   CMP	R4	R0				; Check if bucket is already 0
			   BEQ	reset				; If yes, reset
			  STOR	R4	[GB + ldisk]
			   BRA	treset
;
	buck2:	  LOAD	R4	0
			  STOR	R4	[GB + cdisk]
			  LOAD	R0	[GB + ldisk]
			   CMP	R4	R0				; If R4 is already 1
			   BEQ	reset				; reset
			  STOR	R4	[GB + ldisk]
			   BRA	treset				; turn reset
;
;	End of disk detection state
;

;
;	Misc. subroutines
;
	return:	   RTS
;
	reset:	  CLRI	8
			  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
			  STOR	R3	[GB + switch]
			   BRA	push
;
	treset:	  CLRI	8
			  LOAD	R3	0
			  STOR	R3	[R5 + OUTPUT]
			  STOR	R3	[GB + switch]
			   BRA	turn
;
	sod:	  LOAD	R0	[R5]
			   AND	R0	%01
			   CMP	R0	%01
			   BEQ	st
			  LOAD	R0	[GB + dcount]
			   RTS
	st:		  LOAD	R0	[GB + state]
			   RTS
;
	wait1:	  LOAD	R1	[R5+TIMER]		; Load timer into R1
	waitloop:  BRS	ch1
			   CMP	R1	[R5+TIMER]		; Compare R1 to timer
			   BPL	return				; If 
			   BRA	waitloop
;
;	End of misc. subroutines
;

;
;	Error reporting subroutines
;
	error:	  LOAD	R3	0
			  STOR	R3	[GB + ldisk]
			  STOR	R3	[GB + cdisk]
			  STOR	R3	[GB + switch]
			  STOR	R3	[GB + emergency]
			  STOR	R3	[GB + dcount]
			  STOR	R3	[GB + state]
			  STOR	R3	[GB + tmr]
			  STOR	R3	[GB + echeck]
			  STOR	R3	[GB + cerror]
			  STOR	R3	[R5 + OUTPUT]
			  LOAD	R0	[GB + error]
			  STOR	R3	[GB + error]
			   BRS	Hex7Seg
			  STOR	R1	[R5 + DSPSEG]
			  LOAD	R1	%01
			  STOR	R1	[R5 + DSPDIG]
	erlo:	  LOAD	R1	[R5 + INPUT]
			   AND	R1	%010000000
			   CMP	R1	%010000000
			   BEQ	erlo
			   BRA	start
;
;	Error 1: emergency switch is pressed.
;
	er1:	  LOAD	R3	1
			  STOR	R3	[GB + error]
			   BRA	error
;
	ch1:	  LOAD	R4	[GB + emergency]
			   CMP	R4	1
			   BEQ	skip4
			  LOAD	R4	[R5 + INPUT]
			   AND	R4	%010000000
			   CMP	R4	%010000000
			   BEQ	return
			  LOAD	R4	1
			  STOR	R4	[GB + emergency]
			   RTS
			   
	skip4:	  LOAD	R4	[R5 + INPUT]
			   AND	R4	%010000000
			   CMP	R4	%010000000
			   BEQ	er1
			   RTS
;
;	Error 2: Bucket switches out of sync.
;
	er2:	  LOAD	R3	2
			  STOR	R3	[GB + error]
			   BRA	error
;
	ch2:	  LOAD	R4	[R5	+ INPUT]
			  LOAD	R0	R4
			   AND	R4	%010
			   DIV	R4	%010
			   AND	R0	%01000
			   DIV	R0	%01000
			   CMP	R4	R0
			   BNE	er2
			   BRA	error
;
;
;
  
;
;	End of errors
;

;
;	Recover the machine to the resting state.
;
recover:	   XOR	R3	%01				; Load 1 to R3
			  STOR	R3	[R5 + OUTPUT]	; Store R3 in output, turn on output1
			  LOAD	R2	4				; Loop 4 times
	rloop:	   BRS	wait1				; Wait 1 tick
			   SUB	R2 1
			   CMP	R2 0
			   BNE	rloop
			   XOR	R3	%01				; Load 0 to R3
			  STOR	R3	[R5 + OUTPUT]	; Store R3 in output, turn off output1
			   BRS	wait1				; wait for 1 tick
;
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%01				; Only consider switch 1
			  LOAD	R4	[GB + switch]
			   CMP	R4	1				; Check if switch has been on
			   BNE	rskip				; If not equal, skip this step
			   CMP	R1	0				; Check if switch 1 is pressed
			   BEQ	rturn				; If not, branch to precheck
			   BRA  recover				; If yes, loop to push
;
	rskip:	   BRS	roncheck				; BRS to oncheck
			   BRA	recover				; Loop back to push
;
	rturn:	  LOAD	R4	0				; Set switch to 0
			  STOR	R4	[GB + switch]
;
	rturn1:	   XOR	R3	%010			; Set motor 2 on
			  STOR	R3	[R5 + OUTPUT]	
			  LOAD	R2	8				; loop 4 times
	rloop2:	   BRS	wait1				; Wait 1 tick
			   SUB	R2	1
			   CMP	R2	0
			   BNE	rloop2
			   XOR	R3	%010			; Turn off motor 2
			  STOR	R3	[R5 + OUTPUT]
			   BRS	wait1				; Wait 1 tick
;
			  LOAD	R1	[R5 + INPUT]	; Load input to R1
			   AND	R1	%01010			; Consider only bucket switches
			  LOAD	R4	[GB + switch]
			   CMP	R4	1
			   BNE	rskip2
			   CMP	R1	0
			   BEQ	rturn3
			   BRA	rturn1
			   
	rskip2:	   BRS	oncheck2
			   BRA	rturn1				; If not, loop to turn2
;
	rturn3:	   XOR	R3	%010			; Set motor 2 on
			  STOR	R3	[R5 + OUTPUT]	
			  LOAD	R2	4				; loop 4 times
	rloop3:	   BRS	wait1				; Wait 1 tick
			   SUB	R2	1
			   CMP	R2	0
			   BNE	rloop3
			   XOR	R3	%010			; Turn off motor 2
			  STOR	R3	[R5 + OUTPUT]
			   BRS	wait1				; Wait 1 tick
			  LOAD	R1	[R5 + INPUT]
			   AND	R1	%010
			   CMP	R1	%010
			   BEQ	rset
			   BRA	rturn3
;
	rset:	  LOAD	R0	0
			  STOR	R0	[GB + ldisk]
			  STOR	R0	[GB + cdisk]
			  STOR	R0	[GB + switch]
			  STOR	R0	[GB + error]
			  STOR	R0	[GB + emergency]
			  STOR	R0	[GB + dcount]
			  STOR	R0	[GB + state]
			  STOR	R0	[GB + tmr]
			  STOR	R0	[GB + echeck]
			  STOR	R0	[GB + cerror]
			  STOR	R0	[R5 + OUTPUT]
			  LOAD	R0	%01111110
			  STOR	R0	[R5 + DSPSEG]
			  LOAD	R0	%0100000
			  STOR	R0	[R5	+ DSPDIG]
			  CLRI	8
			   BRA	start
;
	roncheck:  CMP	R1 %01				; If R1 is 1
			   BNE	return				; NOT return
			  LOAD	R4	1				; DO set switch to 1
			  STOR	R4	[GB + switch]
			   RTS						; return			  
;
;
;	ISRs
;
	tmrisr:	  LOAD	R0	[GB + cerror]
			  STOR	R0	[GB + error]
			  LOAD	R0	1
			  STOR	R0	[GB + echeck]
			   RTE
;
@END