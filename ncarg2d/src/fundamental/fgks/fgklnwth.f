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
C                        how to change line width.
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

C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
      DATA    X0P, Y0P, RP, NPTP/ 0.500, 0.5, 0.40 ,  16/
C
C Declare the constant for converting from degrees to radians.
C
      DATA DTR / .017453292519943 /


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
C  Create a polygonal fan 
C
      DTHETA = TWOPI/NPTP
      DO 10 I=1,16
      	ANG = DTHETA*REAL(I) + .19625
      	XC = RP*COS(ANG)
      	YC = RP*SIN(ANG)
C
C Set the line color
C 
	CALL GSPLCI (1)
C
C Set line width 
C 
	CALL GSLWSC(REAL(I)) 

C
C Draw a line
C
      	CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
  10  CONTINUE
      
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C     Set the line color to black
      CALL GSPLCI (1)
C
C  Create a background perimeter 
C
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
C
C  Label the plot
C
      CALL PLCHLQ(0.5,0.91,'Changing Line Width',25.,0.,0.)
C
C Advance the frame
C
      CALL FRAME
C
      END
