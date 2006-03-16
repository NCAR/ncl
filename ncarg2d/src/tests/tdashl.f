
      PROGRAM TDASHL
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
      CALL DASHL(IERR)
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
      SUBROUTINE DASHL (IERROR)
C
C PURPOSE                To provide a simple demonstration of DASHLINE
C
C USAGE                  CALL DASHL (IERROR)
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
C               DASHLINE TEST EXECUTED--SEE PLOT TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C REQUIRED ROUTINES      DASHLINE
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C ALGORITHM              TDASHL utilizes the software DASHLINE
C                        routines DASHDB, DASHDC, FRSTD, VECTD,
C                        LINED, and CURVED to draw 5 curves on 1
C                        picture using 5 different DASHLINE patterns.
C                        Each curve is centered about solid axis
C                        lines and labelled with the binary
C                        representation of the DASHLINE pattern used.
C
C X contains the abscissae and Y the ordinates of the curve to be plotted.
C
      DIMENSION       X(31)      ,Y(31)      ,IPAT(5)
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Set a solid dash pattern,  1111111111111111 (BINARY).
C Boolean operations (using locally-implemented support
C routines) are used to support porting to hosts with 16
C bit integers.
C
      ISOLID = IOR (ISHIFT (32767,1), 1)
C
C Array IPAT contains 5 different 16-BIT dash patterns.  The patterns
C are constructed with boolean operations as shown above.
C The binary representations of the patterns are
C        0001110001111111
C        1111000011110000
C        1111110011111100
C        1111111100000000
C        1111111111111100
C
      IPAT(1) = IOR (ISHIFT ( 3647,1), 1)
      IPAT(2) = ISHIFT (30840,1)
      IPAT(3) = ISHIFT (32382,1)
      IPAT(4) = ISHIFT (32640,1)
      IPAT(5) = ISHIFT (32766,1)
C
      DO  70 K=1,5
         CALL DASHDB (ISOLID)
         ORG =1.07-0.195*K
C
C Draw the central axis for each curve.
C
         CALL FRSTD (.50,ORG-0.03)
         CALL VECTD (.50,ORG+0.03)
         CALL LINED (.109,ORG,.891,ORG)
         CALL DASHDB (IPAT(K))
C
C Compute the curve coordinates and draw the curve.
C
         DO  10 I=1,31
            THETA = REAL(I-1)*3.1415926535897932/15.
            X(I) = 0.5+.4*COS(THETA)
            Y(I) = ORG+.075*SIN(REAL(K)*THETA)
   10    CONTINUE
         CALL CURVED (X,Y,31)
C
C Label each curve with the appropriate binary representation of the
C DASHLINE pattern.
C
C Locate the string at the left end, but vertically centered.
C
      CALL GSTXAL(1,3)
C
C Set the character height.
C
      CALL GSCHH(.012)
C
         ORY = ORG+.09
         GO TO ( 20, 30, 40, 50, 60),K
   20    CALL GTX (.1,ORY,'IPAT=0001110001111111')
         GO TO  70
   30    CALL GTX (.1,ORY,'IPAT=1111000011110000')
         GO TO  70
   40    CALL GTX (.1,ORY,'IPAT=1111110011111100')
         GO TO  70
   50    CALL GTX (.1,ORY,'IPAT=1111111100000000')
         GO TO  70
   60    CALL GTX (.1,ORY,'IPAT=1111111111111100')
C
   70 CONTINUE
C
      CALL GSTXAL(2,3)
      CALL GTX (.5,.985,'DEMONSTRATION PLOT FOR DASHLINE')
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
 1001 FORMAT (' DASHLINE TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
