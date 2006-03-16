
      PROGRAM TPWRZS
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
      CALL TPWRZS1(IERR)
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
      SUBROUTINE TPWRZS1 (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        entry PWRZS with the SRFACE utility.
C
C USAGE                  CALL TPWRZS1 (IERROR)
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
C               PWRZS TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C REQUIRED ROUTINES      PWRZS, SRFACE
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C ALGORITHM              A function of 2 variables is defined and the
C                        values of the function on a 2-D rectangular
C                        grid are stored in an array.  This routine
C                        calls SRFACE to draw a surface representation
C                        of the array values.  PWRZS is then called 3
C                        times to label the front, side, and back of
C                        the picture.
C
      DIMENSION       Z(20,30)   ,X(20)      ,Y(30)      ,MM(20,30,2),
     1                S(6)
C
C Load the SRFACE common block needed to supress a NEWFM call.
C
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
C
C Define the center of a plot title string on a square grid of size
C 0. to 1.
C
      DATA TX/0.4375/, TY/0.9667/
C
C Specify grid loop indices and a line of sight.
C
      DATA M/20/, N/30/
      DATA S/4.,5.,3.,0.,0.,0./
C
C Initial the error parameter.
C
      IERROR = 1
C
C Define the function values and store them in the Z array.
C
      DO  10 I=1,M
         X(I) = -1.+REAL(I-1)/REAL(M-1)*2.
   10 CONTINUE
      DO  20 J=1,N
         Y(J) = -1.+REAL(J-1)/REAL(N-1)*2.
   20 CONTINUE
      DO  40 J=1,N
         DO  30 I=1,M
            Z(I,J) = EXP(-2.*SQRT(X(I)**2+Y(J)**2))
   30    CONTINUE
   40 CONTINUE
C
C Set SRFACE parameters to supress the FRAME call and draw contours.
C
      IFR = 0
      IDRZ = 1
C
C Select normalization trans number 0.
C
      CALL GSELNT (0)
C
C Label the plot.
C
      CALL PLCHLQ (TX,TY,'DEMONSTRATION PLOT FOR PWRZS',16.,0.,0.)
C
C Draw the surface plot.
C
      CALL SRFACE (X,Y,Z,MM,M,M,N,S,0.)
C
C Put the PWRZS labels on the picture.
C
      ISIZE = 35
      CALL PWRZS (0.,1.1,0.,'FRONT',5,ISIZE,-1,3,0)
      CALL PWRZS (1.1,0.,0.,'SIDE',4,ISIZE,2,-1,0)
      CALL PWRZS (0.,-1.1,.2,' BACK BACK BACK BACK BACK',25,ISIZE,-1,
     1            3,0)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
C
C Restore the SRFACE parameters to their default values.
C
      IFR = 1
      IDRZ = 0
C
      RETURN
C
 1001 FORMAT (' PWRZS TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
