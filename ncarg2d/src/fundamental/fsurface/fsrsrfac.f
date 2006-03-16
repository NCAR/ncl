
      PROGRAM FSRSRFAC
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL TSRFAC(IWKID,IERR)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE TSRFAC (IWKID,IERROR)
C
C PURPOSE                To provide a simple demonstration of SRFACE.
C
C USAGE                  CALL TSRFAC (IWKID,IERROR)
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          A workstation id number
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C
C               SRFACE TEST EXECUTED--SEE PLOT TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      SRFACE
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              The function
C
C                          Z(X,Y) = .25*(X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09)
C
C                        for X = -1. to +1. in increments of .1, and
C                            Y = -1.2 to +1.2 in increments of .1,
C                        is computed.  Then, entry SURFACE
C                        is called to a generate surface plot of Z.
C
C HISTORY                SURFACE was first written in April 1979 and
C                        converted to FORTRAN 77 and GKS in March 1984.
C
C XX contains the X-direction coordinate values for Z(X,Y);  YY contains
C the Y-direction coordinate values for Z(X,Y);  Z contains the function
C values;  S contains values for the line of sight for entry SRFACE;
C WORK is a work array;  ANGH contains the angle in degrees in the X-Y
C plane to the line of sight;  and ANGV contains the angle in degrees
C from the X-Y plane to the line of sight.
C
      REAL            XX(21)     ,YY(25)     ,Z(21,25)   ,S(6)       ,
     1                WORK(1096)
C
      DATA  S(1), S(2), S(3), S(4), S(5), S(6)/
     1      -8.0, -6.0,  3.0,  0.0,  0.0,  0.0/
C
      DATA  ANGH/45./, ANGV/15./
C
C Specify coordinates for plot titles.  The values CX and CY
C define the center of the title string in a 0. to 1. range.
C
      DATA CX/.5/, CY/.9/
C
C Initialize the error parameter.
C
      IERROR = 0
C
C Set up a the background and foreground colors
C
C     White background
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
C     Blue foreground
C
      CALL GSCR (IWKID,1,0.,0.,1.)
C
C Fill the XX and YY coordinate arrays as well as the Z function array.
C
      DO  20 I=1,21
         X = .1*REAL(I-11)
         XX(I) = X
         DO  10 J=1,25
            Y = .1*REAL(J-13)
            YY(J) = Y
            Z(I,J) = (X+Y+1./((X-.1)**2+Y**2+.09)-
     1           1./((X+.1)**2+Y**2+.09))*.25
 10      CONTINUE
 20   CONTINUE
C
C Select the normalization transformation 0.
C
      CALL GSELNT(0)
C
C Add the plot title.
C
C Set the text alignment to center the string in horizontal and vertical
C
      CALL GSTXAL(2,3)
C
C Set the character height.
C
      CALL GSCHH(.016)
C
C Write the text.
C
      CALL GTX(CX,CY,'DEMONSTRATION PLOT FOR SRFACE ENTRY OF SRFACE')
C
      CALL SRFACE (XX,YY,Z,WORK,21,15,12,S,0.)
C
C This routine automatically generates frame advances.
C
C
      RETURN
C
      END
