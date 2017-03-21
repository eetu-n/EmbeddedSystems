@CODE

   IOAREA	   EQU	-16				; address of the I/O-Area, modulo 2^18
    INPUT	   EQU	7				; position of the input buttons (relative to IOAREA)
   OUTPUT	   EQU	11				; relative position of the power outputs
   DSPDIG	   EQU 	9				; relative position of the 7-segment display's digit selector
   DSPSEG	   EQU	8				; relative position of the 7-segment display's segments
    TIMER	   EQU	13
  ADCONVS	   EQU	6

  begin :      BRA  main			; skip subroutine Hex7Seg
;
Hex7Seg     :  BRS  Hex7Seg_bgn		; push address(tbl) onto stack and proceed at "bgn"
Hex7Seg_tbl : CONS  %01111110		; 7-segment pattern for '0'
              CONS  %00110000		; 7-segment pattern for '1'
              CONS  %01101101		; 7-segment pattern for '2'
              CONS  %01111001		; 7-segment pattern for '3'
              CONS  %00110011		; 7-segment pattern for '4'
              CONS  %01011011		; 7-segment pattern for '5'
              CONS  %01011111		; 7-segment pattern for '6'
              CONS  %01110000		; 7-segment pattern for '7'
              CONS  %01111111		; 7-segment pattern for '8'
              CONS  %01111011		; 7-segment pattern for '9'
              CONS  %01110111		; 7-segment pattern for 'A'	
              CONS  %00011111		; 7-segment pattern for 'b'
              CONS  %01001110		; 7-segment pattern for 'C'
              CONS  %00111101		; 7-segment pattern for 'd'
              CONS  %01001111		; 7-segment pattern for 'E'
              CONS  %01000111		; 7-segment pattern for 'F'
Hex7Seg_bgn:   AND  R0  %01111		; R0 := R0 MOD 16 , just to be safe...
              LOAD  R1  [SP++]		; R1 := address(tbl) (retrieve from stack)
              LOAD  R1  [R1+R0]		; R1 := tbl[R0]
               RTS
;
;      The body of the main program
;
	main:	  LOAD	R5	IOAREA		; R5 := "address of the area with the I/O-registers"
			  LOAD	R1	%01000
			  STOR	R1	[R5 + OUTPUT]
			   BRA	test
;
	loop:	  LOAD	R3	[R5 + ADCONVS]
			   BRS	check
			   BRS	Hex7Seg
			  STOR	R1	[R5+DSPSEG]
			  LOAD	R1	%00001
			  STOR	R1	[R5+DSPDIG]
			   BRA	loop
;
	check:	  CMP	R3	%011100110
			  BMI	check2
			 LOAD	R0	1
			  BRA	return
;
	check2:	  CMP	R3	%010011100
			  BMI	nada
			 LOAD	R0	2
			  BRA	return
;
	test:	 LOAD	R0	[R5]
			  AND	R0	%01
			  BRS	Hex7Seg
			 STOR	R1	[R5 + DSPSEG]
			 LOAD	R1	1
			 STOR	R1	[R5 + DSPDIG]
			  BRA	test
;
	pooploop: BRA	pooploop
;
	nada:	 LOAD	R0	3
			  RTS
;
	return:	  RTS
@END