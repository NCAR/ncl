C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL LINEEX
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
      SUBROUTINE LINEEX 
C
C PURPOSE                To provide a simple demonstration of 
C                        how to set line dash patterns. 
C
C USAGE                  CALL LINEEX 
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
      CHARACTER STR*60
      REAL XCOORD(360), YCOORD(360)

C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/

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

C
C Create and Plot 2 Sine Curves
C
C       Set the dash pattern
        STR = '$''$''Line''drawn''with''VECTD$''$''$''$'''
	CALL DASHDC(STR,15,20)
C       Move plotter pen
        CALL FRSTD(0.,.60)
	DO 22 I=1,360
	  Y = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .60
          CALL VECTD(REAL(I)/360.,Y)
   22   CONTINUE
C Set the Dash pattern
        STR = '$$$$$$Line''drawn''with''CURVED$$$$$$$$$$$$'
	CALL DASHDC(STR,15,20)
        CALL GSLWSC(3.)
	DO 23 I=1,360
          XCOORD(I) = REAL(I)/360.
	  YCOORD(I) = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .45
   23   CONTINUE
C Draw the second curve
        CALL CURVED(XCOORD, YCOORD, 360)

C Draw a straight line
        CALL GSLWSC(4.)
C 1111100110011111 binary  = 63903 decimal
        CALL DASHDB(63903)
        CALL LINED(0.1,.15, .9,.15)
C Label the line
        CALL PLCHLQ(0.5,0.10,'Line drawn with LINED',20.,0.,0.)


C  Create a background perimeter 
C
      CALL GSPLCI(1)
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
C
C  Label the plot
C
      CALL PLCHLQ(0.7,0.90,'Setting Dash Patterns',25.,0.,0.)
      CALL PLCHLQ(0.7,0.81,'with DASHDB and DASHDC',25.,0.,0.)
      CALL FRAME
C
      END
