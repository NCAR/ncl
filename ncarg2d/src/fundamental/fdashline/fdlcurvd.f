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
C                        CURVED line drawing techniques.
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
      CALL GSLWSC(2.)

C Draw a line of circles around the plotter frame
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
        
C Get the coordinates for a circle in the center of the frame
         CALL GNCRCL(.5,.5,.333,30,XCOOR2, YCOOR2)
C        Increase Line width
	 CALL GSLWSC(5.)
C        Set the Line Color
	 CALL GSPLCI(2)
C        Draw it
         CALL CURVED(XCOOR2,YCOOR2,30)
	

C Using these coordinates, plot 30 smaller circles on the circle
C        Decrease Line Width
         CALL GSLWSC(3.)
C        Set the Line Color
	 CALL GSPLCI(3)
         DO 30 I = 1,30
           CALL GNCRCL(XCOOR2(I),YCOOR2(I),.07,30,XCOORD, YCOORD)
           CALL CURVED(XCOORD,YCOORD,30)
C Using these coordinates, plot 30 smaller circles on the circle
C          Decrease Line Width
           CALL GSLWSC(1.)
C          Set the Line Color
	   CALL GSPLCI(4)
           DO 40 J = 1,30
             CALL GNCRCL(XCOORD(J),YCOORD(J),.01,30,XCOOR3, YCOOR3)
             CALL CURVED(XCOOR3,YCOOR3,30)
  40       CONTINUE
C          Increase Line Width
           CALL GSLWSC(3.)
C          Set the Line Color
	   CALL GSPLCI(3)
  30     CONTINUE
           
C Draw a label in the center
      CALL PLCHLQ(.5,.7,'Circles',.03,0.,0.)
      CALL PLCHLQ(.5,.6,'of',.03,0.,0.)
      CALL PLCHLQ(.5,.5,'Circles',.03,0.,0.)
      CALL PLCHLQ(.5,.4,'of',.03,0.,0.)
      CALL PLCHLQ(.5,.3,'Circles',.03,0.,0.)

C Advance the Frame
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
