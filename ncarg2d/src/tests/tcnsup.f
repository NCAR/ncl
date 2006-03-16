
      PROGRAM TCNSUP
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL CNSUP(IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE CNSUP (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        the CONRECSPR package.
C
C USAGE                  CALL CNSUP (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C               CONRECSPR TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 2 output
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      CONRECSPR, DASHSUPR
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              The function
C
C                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09)
C
C                        for X = -1. to +1. in increments of .1, and
C                            Y = -1.2 to +1.2 in increments of .1,
C                        is computed.  Then, entries EZCNTR and CONREC
C                        are called to generate contour plots of Z.
C
C                        This is the highest quality entry of the
C                        CONREC family of utilities.
C
C
C Z contains the values to be plotted.
C
      REAL            Z(21,25)
C
C Define the position of the title.
C
      DATA TX/.4267/, TY/.9765/
C
C
C Initialize the error parameter.
C
      IERROR = 0
C
C Fill a 2-D array to be plotted.
C
      DO  20 I=1,21
         X = .1*REAL(I-11)
         DO  10 J=1,25
            Y = .1*REAL(J-13)
            Z(I,J) = X+Y+1./((X-.10)**2+Y**2+.09)-
     1               1./((X+.10)**2+Y**2+.09)
   10    CONTINUE
   20 CONTINUE
C
C Select normalization transformation number 0.
C
      CALL GSELNT (0)
C
C
C     Frame 1 -- EZCNTR entry of CONRECSPR.
C
C Entry EZCNTR requires only the array name and dimensions.
C
      CALL PLCHLQ (TX, TY,
     1   'DEMONSTRATION PLOT FOR EZCNTR ENTRY OF CONRECSPR',16.,0.,0.)
      CALL EZCNTR (Z,21,25)
C
C
C     Frame 2 -- CONREC entry of CONRECSPR.
C
C Entry CONREC allows user definition of various plot parameters.
C
C In this example, the lowest contour level (-4.5), the highest contour
C level (4.5), and the increment between contour levels (0.3) are set.
C
      CALL PLCHLQ (TX,TY,
     1           'DEMONSTRATION PLOT FOR CONREC ENTRY OF CONRECSPR',
     2           16.,0.,0.)
      CALL CONREC (Z,21,21,25,-4.5,4.5,.3,0,-1,0)
      CALL FRAME
C
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' CONRECSPR TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
