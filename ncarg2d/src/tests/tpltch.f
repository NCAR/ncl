C
C $Id: tpltch.f,v 1.5 1993-03-05 00:12:28 haley Exp $
C
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL TPLTCH(IERR)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE TPLTCH (IERR)
C
C LATEST REVISION        November, 1992.
C
C PURPOSE                To provide a minimal demo and test of routines
C                        in the package PLOTCHAR.
C
C USAGE                  CALL TPLTCH (IERR)
C
C ARGUMENTS
C
C ON OUTPUT              IERR
C                          An integer variable
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                        PLOTCHAR TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C                        In addition, one frame is produced.  To
C                        determine if the test is successful, it is
C                        necessary to examine that frame.
C
C PRECISION              Single
C
C REQUIRED PACKAGES      PLOTCHAR, SPPS
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C ALGORITHM              TPLTCH just writes a simple message on a
C                        single frame.  For a more complex example
C                        of the use of PLOTCHAR, see the example
C                        named "epltch".
C
      CHARACTER*53 MSG(5)
C
      DATA MSG /'This is the graphical output from the example named'  ,
     +          '"tpltch".  It demonstrates minimal functioning of the',
     +          'package PLOTCHAR.  For a more complete demonstration' ,
     +          'of the capabilities of PLOTCHAR, run the example'     ,
     +          'named "epltch".'                                      /
C
C Do a call to SET which allows the use of fractional coordinates.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Write a single frame, using calls to PLCHHQ, PLCHMQ, and PLCHLQ.
C
      CALL PLCHMQ (.5,.95,'PLOTCHAR DEMONSTRATION',16.,0.,0.)
C
      CALL PLCHHQ (.5,.77,'DEMONSTRATING THE USE OF CALLS TO PLCHHQ',
     +                                                     16.,0.,0.)
C
      DO 101 I=1,5
	CALL PLCHHQ (.1025,.75-.03*REAL(I),MSG(I),.015,0.,-1.)
  101 CONTINUE
C
      CALL PLCHMQ (.5,.52,'DEMONSTRATING THE USE OF CALLS TO PLCHMQ',
     +                                                     16.,0.,0.)
C
      DO 102 I=1,5
	CALL PLCHMQ (.1025,.50-.03*REAL(I),MSG(I),.015,0.,-1.)
  102 CONTINUE
C
      CALL PLCHLQ (.5,.27,'DEMONSTRATING THE USE OF CALLS TO PLCHLQ',
     +                                                     16.,0.,0.)
C
      DO 103 I=1,5
	CALL PLCHLQ (.1025,.25-.03*REAL(I),MSG(I),.015,0.,-1.)
  103 CONTINUE
C
C Advance the frame.
C
      CALL FRAME
C
C Zero the error flag.
C
      IERR=0
C
C Write the log message.
C
      WRITE (6,1001)
C
C Quit.
C
      RETURN
C
C Format.
C
 1001 FORMAT (' PLOTCHAR TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
