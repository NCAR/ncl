
      PROGRAM FDLCURVD
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
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
C
      SUBROUTINE EXLINE (IWKID)
C
C PURPOSE                To provide a simple demonstration of the
C                        CURVED line drawing techniques.
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
      REAL XCOORD(120), YCOORD(120)
      REAL XCOOR2(120), YCOOR2(120)
      REAL XCOOR3(120), YCOOR3(120)
C
C Establish the viewport and window.
C
      CALL SET(.01,.99,0.01,.99,0.,1.,0.,1.,1)
C
C Turn buffering off
C
      CALL SETUSV('PB',2)
C
C Set up a color table
C
C     White background
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
      CALL GSLWSC(2.)
C
C Draw a line of circles around the plotter frame
C
      DO 20 I=0,23
         CALL GNCRCL(I/25.+.025, 0.+.025, .025, 25, XCOORD, YCOORD)
         CALL CURVED(XCOORD,YCOORD,25)
         CALL GNCRCL(I/25.+.025, 1.-.025, .025, 25, XCOORD, YCOORD)
         CALL CURVED(XCOORD,YCOORD,25)
         CALL GNCRCL(0.+.025,I/25.+.025,.025, 25, XCOORD, YCOORD)
         CALL CURVED(XCOORD,YCOORD,25)
         CALL GNCRCL(1.-.025,I/25.+.025,.025, 25, XCOORD, YCOORD)
         CALL CURVED(XCOORD,YCOORD,25)
  20  CONTINUE
      CALL GNCRCL(1.-.025,1.-.025,.025, 25, XCOORD, YCOORD)
      CALL CURVED(XCOORD,YCOORD,25)
C
C Get the coordinates for a circle in the center of the frame
C
      CALL GNCRCL(.5,.5,.333,30,XCOOR2, YCOOR2)
C
C Increase Line width
C
      CALL GSLWSC(5.)
C
C Set the Line Color
C     
      CALL GSPLCI(2)
C
C Draw it
C
      CALL CURVED(XCOOR2,YCOOR2,30)
C
C Using these coordinates, plot 30 smaller circles on the circle
C        Decrease Line Width
C
      CALL GSLWSC(3.)
C
C Set the Line Color
C
      CALL GSPLCI(3)
      DO 30 I = 1,30
         CALL GNCRCL(XCOOR2(I),YCOOR2(I),.07,30,XCOORD, YCOORD)
         CALL CURVED(XCOORD,YCOORD,30)
C
C Using these coordinates, plot 30 smaller circles on the circle
C
C Decrease Line Width
C
         CALL GSLWSC(1.)
C
C Set the Line Color
C
         CALL GSPLCI(4)
         DO 40 J = 1,30
            CALL GNCRCL(XCOORD(J),YCOORD(J),.01,30,XCOOR3, YCOOR3)
            CALL CURVED(XCOOR3,YCOOR3,30)
 40      CONTINUE
C
C Increase Line Width
C
         CALL GSLWSC(3.)
C
C Set the Line Color
C
         CALL GSPLCI(3)
 30   CONTINUE
C
C Draw a label in the center
C
      CALL PLCHLQ(.5,.7,'Circles',.03,0.,0.)
      CALL PLCHLQ(.5,.6,'of',.03,0.,0.)
      CALL PLCHLQ(.5,.5,'Circles',.03,0.,0.)
      CALL PLCHLQ(.5,.4,'of',.03,0.,0.)
      CALL PLCHLQ(.5,.3,'Circles',.03,0.,0.)
C
C Advance the Frame
C
      CALL FRAME

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
 10    CONTINUE

      RETURN
      END
