
      PROGRAM TGFLAS
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
      CALL GFLAS(IERR)
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
      SUBROUTINE GFLAS (IERROR)
C
C PURPOSE                To provide a simple demonstration of the
C                        GFLASH package.
C
C USAGE                  CALL GFLAS (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                            = 0  If there is a normal exit from GFLASH,
C                            = 1  Otherwise
C
C I/O                    If there is a normal exit from GFLASH,
C                        the message
C
C                          GFLASH TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        is written on unit 6
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       GFLASH
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written  by members of the
C                        Scientific Computing Division of NCAR,
C                        Boulder Colorado
C
C PORTABILITY            FORTRAN 77
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C
C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
      DATA    X0C, Y0C, RC, NPTC/ 0.325, 0.5, 0.10 , 210/
      DATA    X0P, Y0P, RP, NPTP/ 0.500, 0.5, 0.35 ,  16/
      DATA    X0S, Y0S, RS      / 0.675, 0.5, 0.10      /
C
C Establish the viewport and window.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C  Initialize the GFLASH package.  If using a non-NCAR GKS package
C  the final argument in the following call should be replaced with
C  the workstation type for WISS.
C
      CALL GOPWK(9,1,3)
C
C  Put a circle in a buffer with identifier 1.
C
      CALL GFLAS1(1)
      DTHETA = TWOPI/NPTC
      CALL FRSTPT(X0C+RC,Y0C)
      DO 20 I=1,NPTC
      ANG = DTHETA*REAL(I)
      XC = RC*COS(ANG)
      YC = RC*SIN(ANG)
      CALL VECTOR(X0C+XC,Y0C+YC)
   20 CONTINUE
      CALL GFLAS2
C
C  Put a polygonal fan in a buffer with identifier 2.
C
      CALL GFLAS1(2)
      DTHETA = TWOPI/NPTP
      DO 10 I=1,NPTP
      ANG = DTHETA*REAL(I)
      XC = RP*COS(ANG)
      YC = RP*SIN(ANG)
      CALL LINE(X0P,Y0P,X0P+XC,Y0P+YC)
   10 CONTINUE
      CALL GFLAS2
C
C  Put a square in a buffer with identifier 3.
C
      CALL GFLAS1(3)
      CALL FRSTPT(X0S+RS,Y0S+RS)
      CALL VECTOR(X0S-RS,Y0S+RS)
      CALL VECTOR(X0S-RS,Y0S-RS)
      CALL VECTOR(X0S+RS,Y0S-RS)
      CALL VECTOR(X0S+RS,Y0S+RS)
      CALL GFLAS2
C
C  Put a background perimeter in a buffer with identifier 4.
C
      CALL GFLAS1(4)
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
      CALL GFLAS2
C
C  Create plots.
C
C  Frame 1 -- title, circle, and fan.
C
      CALL PLCHLQ(0.5,0.91,'FRAME 1',25.,0.,0.)
      CALL GFLAS3(1)
      CALL GFLAS3(2)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Frame 2 -- fan, title, and square.
C
      CALL GFLAS3(2)
      CALL PLCHLQ(0.5,0.91,'FRAME 2',25.,0.,0.)
      CALL GFLAS3(3)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Frame 3 -- circle, square, fan, and title (note that the change
C             in window affects the PLCHLQ call, but not the elements
C             in the buffers -- this illustrates the independent
C             nature of the FLASH buffers).
C
      CALL SET (0.,1.,0.,1.,0.,10.,0.,10.,1)
      CALL GFLAS3(1)
      CALL GFLAS3(3)
      CALL GFLAS3(2)
      CALL PLCHLQ(5.0,9.1,'FRAME 3',25.,0.,0.)
      CALL GFLAS3(4)
      CALL FRAME
C
C  Close the GFLASH package.
C
      CALL GCLWK(9)
C
      END
