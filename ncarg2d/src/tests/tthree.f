
      PROGRAM TTHREE
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
      CALL THREE(IERR)
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
      SUBROUTINE THREE (IERROR)
C
C PURPOSE                To provide a simple demonstration of THREED.
C
C USAGE                  CALL THREE (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C
C               THREED TEST EXECUTED--SEE PLOT TO CERTIFY
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
C REQUIRED ROUTINES      THREED
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              Routine TTHREE calls SET3 to establish a
C                        mapping between the plotter addresses and
C                        the user's volume, and to indicate the
C                        coordinates of the eye position from which
C                        the lines to be drawn are viewed.  Next,
C                        the volume perimeters and associated tick
C                        marks are drawn by calls to PERIM3.  The
C                        selected latitude and longitude lines of
C                        a sphere are then drawn.
C
C HISTORY                THREED was originally written in November
C                        1976 and converted to FORTRAN 77 and GKS
C                        in July 1984.
C
      REAL EYE(3),X(31),Y(31),Z(31)
C
C Specify the arguments to be used by routine SET3 on a plot
C grid in the address range of 0. to 1.  In each coordinate direction,
C the values  RXA, RXB, RYA, and RYB define the portion of the address
C space to be used in making the plot.  UC, UD, VC, VD, WC, and WD
C define a volume in user coordinates which is to be mapped onto the
C portion of the viewing surface as specified by RXA, RXB, RYA, and RYB.
C
      DATA RXA/0.097656/, RXB/0.90236/, RYA/0.097656/, RYB/0.90236/
      DATA UC/-1./, UD/1./, VC/-1./, VD/1./, WC/-1./, WD/1./
      DATA EYE(1),EYE(2),EYE(3)/10.,6.,3./
      DATA TX/0.4374/, TY/0.9570/
      DATA PI/3.1415926535898/
C
C Select normalization transformation 0.
C
      CALL GSELNT (0)
C
C Call SET3 to establish a mapping between the plotter addresses
C and the user's volume, and to indicate the coordinates of the
C eye position from which the lines to be drawn are viewed.
C
      CALL SET3(RXA,RXB,RYA,RYB,UC,UD,VC,VD,WC,WD,EYE)
C
C Call PERIM3 to draw perimeter lines and tick marks.
C
      CALL PERIM3(2,5,1,10,1,-1.)
      CALL PERIM3(4,2,1,1,2,-1.)
      CALL PERIM3(2,10,4,5,3,-1.)
C
C Define and draw latitudinal lines on the sphere of radius one
C having its center at (0.,0.,0.)
C
      DO 10 J=1,18
      THETA = REAL(J)*PI/9.
      CT = COS(THETA)
      ST = SIN(THETA)
      DO 20 K=1,31
      PHI = REAL(K-16)*PI/30.
      Z(K) = SIN(PHI)
      CP = COS(PHI)
      X(K) = CT*CP
      Y(K) = ST*CP
   20 CONTINUE
      CALL CURVE3(X,Y,Z,31)
   10 CONTINUE
C
C Define and draw longitudinal lines on the sphere of radius one
C having its center at (0.,0.,0.)
C
      DO 30 K=1,5
      PHI = REAL(K-3)*PI/6.
      SP = SIN(PHI)
      CP = COS(PHI)
      DO 40 J=1,31
      TUETA = REAL(J-1)*PI/15.
      X(J) = COS(TUETA)*CP
      Y(J) = SIN(TUETA)*CP
      Z(J) = SP
   40 CONTINUE
      CALL CURVE3(X,Y,Z,31)
   30 CONTINUE
C
C Add a plot title.
C
      CALL PLCHLQ(TX,TY,'DEMONSTRATION PLOT FOR ROUTINE THREED',
     +             16.,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
C
      IERROR = 0
      WRITE(6,1001)
      RETURN
C
 1001 FORMAT (' THREED TEST EXECUTED--SEE PLOT TO CERTIFY')
      END
