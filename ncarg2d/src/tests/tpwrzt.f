
      PROGRAM TPWRZT
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
      CALL TPWRZT1(IERR)
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
      SUBROUTINE TPWRZT1 (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        entry PWRZT with the THREED utility.
C
C USAGE                  CALL TPWRZT1 (IERROR)
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
C               PWRZT TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C REQUIRED ROUTINES      PWRZT, THREED
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C ALGORITHM              The THREED package is called to establish a
C                        3-D projection onto 2 space, and to draw the
C                        axis lines.  PWRZT is then called to label
C                        the axes for a 3 space plot.
C
C EYE contains the (U,V,Z) coordinate of the eye position.
C
      REAL            EYE(3)
      DATA EYE(1), EYE(2), EYE(3) /3.5, 3.0, 5.0/
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Select normalization transformation number 0.
C
      CALL GSELNT (0)
C
C A call to SET3 establishes the mapping of 3 space coordinates onto
C the coordinate system of the graphics device.
C
      CALL SET3 (.1,.9,.1,.9,0.,1.,0.,1.,0.,1.,EYE)
C
C Draw the 3 space axes.
C
      CALL LINE3 (0.,0.,0.,0.,0.,1.)
      CALL LINE3 (0.,0.,0.,0.,1.,0.)
      CALL LINE3 (0.,0.,0.,1.,0.,0.)
C
C PWRZT is used to label each of the axes and the plot
C
      ICNT = 0
      ISIZE = 30
      LINE = 2
      ITOP = 3
      CALL PWRZT (0.,.5,.1,'V-AXIS',6,ISIZE,LINE,ITOP,ICNT)
C
      LINE = -1
      ITOP = 3
      CALL PWRZT (.5,0.,.1,'U-AXIS',6,ISIZE,LINE,ITOP,ICNT)
C
      LINE = 3
      ITOP = -2
      CALL PWRZT (0.,.1,.5,'Z-AXIS',6,ISIZE,LINE,ITOP,ICNT)
C
      LINE = 2
      ITOP = -1
      ISIZE = 30
      ICNT = -1
      CALL PWRZT (.5,.2,0.,'DEMONSTRATION OF PWRZT WITH THREED',
     1            34,ISIZE,LINE,ITOP,ICNT)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
 1001 FORMAT (' PWRZT TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
