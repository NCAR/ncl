
      PROGRAM TGRIDA
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
      CALL GRIDA(IERR)
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
      SUBROUTINE GRIDA(IER)
C
C PURPOSE                To provide a simple demonstration of
C                        all of the entry points of the GRIDAL
C                        package.  Eight plots are produced.
C
C USAGE                  CALL GRIDA (IERROR)
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
C              GRIDAL TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 8
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        these plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C ALGORITHM              All of the entries of the GRIDAL package
C                        are invoked (GRID, GRIDL, PERIM, PERIML,
C                        HALFAX, TICK4, LABMOD, and GRIDAL) to
C                        produce plots.  The GRIDAL entry is called
C                        ten times.  The first call is to demonstate
C                        a full frame grid.  The next nine calls
C                        create a single frame that contains the
C                        nine legal IGPH grid options to show how
C                        up to nine plots can be placed on a single
C                        frame.
C
      CHARACTER*2 BUFF
C
C Define normalization transformation 1.
C
      CALL GSWN(1,0.,1.,0.,1.)
      CALL GSVP(1,.2,.8,.2,.8)
C
C GRID
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR GRID',16.,0.,0.)
      CALL GSELNT(1)
      CALL GRID(5,2,6,3)
      CALL FRAME
C
C GRIDL
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR GRIDL',16.,0.,0.)
      CALL GSELNT(1)
      CALL GRIDL(5,2,6,3)
      CALL FRAME
C
C PERIM
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR PERIM',16.,0.,0.)
      CALL GSELNT(1)
      CALL PERIM(5,2,6,3)
      CALL FRAME
C
C PERIML
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR PERIML',16.,0.,0.)
      CALL GSELNT(1)
      CALL PERIML(5,2,6,3)
      CALL FRAME
C
C HALFAX
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR HALFAX',16.,0.,0.)
      CALL GSELNT(1)
      CALL HALFAX(5,2,6,3,.3,.5,0,0)
      CALL FRAME
C
C TICK4
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR TICK4',16.,0.,0.)
      CALL GSELNT(1)
      CALL TICK4(150,50,150,50)
      CALL PERIM(5,2,6,3)
      CALL FRAME
      CALL TICK4(12,8,12,8)
C
C LABMOD
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR LABMOD',16.,0.,0.)
      CALL GSELNT(1)
      CALL LABMOD('(E10.2)','(F4.2)',10,4,15,15,0,0,0)
      CALL HALFAX(2,1,10,1,0.,0.,1,1)
      CALL FRAME
C
C Use LABMOD to reduce the number of digits in the grid scales
C
C
      CALL LABMOD('(F4.2)','(F4.2)',4,4,0,0,0,0,0)
C
C GRIDAL  -  A single grid on a frame.
C
      IGPH = 0
      WRITE(BUFF,1001)IGPH
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.85,'IGPH = ',16.,0.,1.)
      CALL PLCHLQ(.5,.85,BUFF,16.,0.,-1.)
      CALL PLCHLQ(.5,.9,'DEMONSTRATION PLOT FOR GRIDAL',16.,0.,0.)
      CALL GSELNT(1)
      CALL GRIDAL(5,2,6,3,1,1,IGPH,.3,.13)
      CALL FRAME
C
C GRIDAL  -  All 9 legal grids on a single frame.
C
      CALL GSELNT(0)
      CALL PLCHLQ(.5,.98,'TEST IGPH OPTIONS OF GRIDAL',16.,0.,0.)
      KNT = 0
      DO 100 I=0,10
      IF (I .EQ. 3 .OR. I .EQ. 7) GOTO 100
      IGPH = I
      WRITE(BUFF,1001)IGPH
      KNT = KNT + 1
C
C Compute the X and Y grid boundaries for the 9 plots.
C
      Y1 = .42
      IF(KNT.LT.4) Y1 = .74
      IF(KNT.GT.6) Y1 = .10
      X1 = .10
      IF(KNT.EQ.2.OR.KNT.EQ.5.OR.KNT.EQ.8) X1 = .42
      IF(KNT.EQ.3.OR.KNT.EQ.6.OR.KNT.EQ.9) X1 = .74
      X2 = X1 + .18
      Y2 = Y1 + .18
C
C Specify some user coordinates.
C
      H1 = X1*10.
      H2 = X2*10.
      V1 = Y1*10.
      V2 = Y2*10.
C
C Locate the IGPH legend above the grid.
C
      XG = X1 + .13
      YG = Y2 + .03
      CALL GSELNT(0)
      CALL PLCHLQ(XG,YG,'IGPH = ',16.,0.,1.)
      CALL PLCHLQ(XG,YG,BUFF,16.,0.,-1.)
      CALL GSELNT(1)
      CALL SET(X1,X2,Y1,Y2,H1,H2,V1,V2,1)
      CALL GRIDAL(3,3,4,2,1,1,IGPH,H1,V1)
  100 CONTINUE
C
C Advance the frame.
C
      CALL FRAME
 1001 FORMAT(I2)
C
      WRITE(6,600)
  600 FORMAT(' GRIDAL TEST EXECUTED--SEE PLOTS TO CERTIFY')
      RETURN
      END
