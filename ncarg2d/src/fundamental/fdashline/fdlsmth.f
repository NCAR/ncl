
      PROGRAM FDLSMTH
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
      CALL EXLINE (IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END

      SUBROUTINE EXLINE (IWKID) 
C
C PURPOSE                To provide a simple demonstration of the
C                        smoothing and crowded line removal techniques.
C
C USAGE                  CALL EXLINE (IWKID)
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          Workstation id
C
C LANGUAGE               FORTRAN
C
C PORTABILITY            FORTRAN 77
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C
      REAL XCOORD(10), YCOORD(10)
C
C Establish the viewport and window.
C
      CALL SET(.01,.99,0.01,.99,0.,1.,0.,1.,1)
C
C Initialize the crowded line removal buffer
C
      CALL RESET
C
C Set up a color table
C
C     White background
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
C Black foreground
C
      CALL GSCR (IWKID,1,0.,0.,0.)
C
C Red
C
      CALL GSCR (IWKID,2,1.,0.,0.)
C
C Green
C
      CALL GSCR (IWKID,3,0.,1.,0.)
C
C Blue
C
      CALL GSCR (IWKID,4,0.,0.,1.)
C
C Set line color
C
      CALL GSPLCI(4)
C
C Set line width
C
      CALL GSLWSC(1.)
        
      DO 10 I=1,50
C
C Get the coordinates for a circle in the center of the frame
C
         CALL GNCRCL(REAL(I)/50.,.5,.333,10,XCOORD, YCOORD)
C
C Draw it
C
         CALL CURVED(XCOORD,YCOORD,10)
 10   CONTINUE
C
C Draw a label in the center
C
      CALL PLCHLQ(.5,.95,'Smoothing and Crowded Line Removal',.02,0.,0.)
C
C Advance the Frame
C
      CALL FRAME
C
      END

      SUBROUTINE GNCRCL(XCNTR, YCNTR, RAD, NPTS, XCOORD, YCOORD)
      INTEGER NPTS
      REAL XCNTR, YCNTR, RAD, XCOORD(NPTS), YCOORD(NPTS)

C
C This function generates the coordinates for a circle with
C center at XCNTR, YCNTR, and a radius of RAD.  There are
C NPTS in the circle and the coordinates are returned in the
C arrays XCOORD and YCOORD.
C
C Compute number of radians per degree
C
      RADPDG = 2.*3.14159/360.
C
C Initialize the angle
C
       ANGLE = 0.
C
C Calculate the change in angle (360./number of points in circle)
C
       DELTA = 360./(NPTS-1)
C
C Convert to radians
C
       DELTA = DELTA * RADPDG
C
C Calculate each coordinate
C
       DO 10 I=1,NPTS
          XCOORD(I) = RAD * (COS(ANGLE)) + XCNTR
          YCOORD(I) = RAD * (SIN(ANGLE)) + YCNTR
          ANGLE = ANGLE + DELTA
  10   CONTINUE

       RETURN
       END
