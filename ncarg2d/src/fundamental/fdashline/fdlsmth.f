C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL EXLINE
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
C
C
      SUBROUTINE EXLINE 
C
C PURPOSE                To provide a simple demonstration of the
C                        smoothing and crowded line removal techniques.
C
C USAGE                  CALL EXLINE 
C
C ARGUMENTS
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
      CALL GSCR (1,0,1.,1.,1.)
C     Black foreground
      CALL GSCR (1,1,0.,0.,0.)
C     Red
      CALL GSCR (1,2,1.,0.,0.)
C     Green
      CALL GSCR (1,3,0.,1.,0.)
C     Blue
      CALL GSCR (1,4,0.,0.,1.)

C Set line color
      CALL GSPLCI(4)
C Set line width
      CALL GSLWSC(1.)

        
      DO 10 I=1,50
C Get the coordinates for a circle in the center of the frame
         CALL GNCRCL(REAL(I)/50.,.5,.333,10,XCOORD, YCOORD)
C        Draw it
         CALL CURVED(XCOORD,YCOORD,10)
 10   CONTINUE

C Draw a label in the center
      CALL PLCHLQ(.5,.95,'Smoothing and Crowded Line Removal',.02,0.,0.)

C Advance the Frame
      CALL FRAME
C
      END

      SUBROUTINE GNCRCL(XCNTR, YCNTR, RAD, NPTS, XCOORD, YCOORD)
      REAL XCNTR, YCNTR, RAD, XCOORD(NPTS), YCOORD(NPTS)
      INTEGER NPTS

C
C This function generates the coordinates for a circle with
C center at XCNTR, YCNTR, and a radius of RAD.  There are
C NPTS in the circle and the coordinates are returned in the
C arrays XCOORD and YCOORD.
C

C
C Compute number of radians per degree
C
      RADPDG = 2.*3.14159/360.

C Initialize the angle
       ANGLE = 0.
C Calculate the change in angle (360./number of points in circle)
       DELTA = 360./(NPTS-1)
C Convert to radians
       DELTA = DELTA * RADPDG

C Calculate each coordinate
       DO 10 I=1,NPTS
	  XCOORD(I) = RAD * (COS(ANGLE)) + XCNTR
	  YCOORD(I) = RAD * (SIN(ANGLE)) + YCNTR
          ANGLE = ANGLE + DELTA
  10   CONTINUE

      RETURN
      END
