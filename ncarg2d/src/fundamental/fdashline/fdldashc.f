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
C PURPOSE                To provide a simple demonstration of the
C                        different line drawing techniques.
C
C USAGE                  CALL LINEEX 
C
C ARGUMENTS
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
      CHARACTER STR*60

C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
      DATA    X0P, Y0P, RP, NPTP/ 0.500, 0.5, 0.45 ,  16/
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
      DO 10 I=1,6
      	ANG = DTHETA*REAL(I) + .19625
      	XC = RP*COS(ANG)
      	YC = RP*SIN(ANG)
C
C Set the line color
C 
	CALL GSPLCI (MOD(I,4)+1)
C
C Set line width 
C 
	CALL GSLWSC(2.) 

	IF (I .EQ. 1) THEN
C         Solid Line (the default)
          STR = '$$$$Solid$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
	  CALL GSLN(1)
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 2) THEN
C         Dashed line
          STR = '$$$$Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
	  CALL GSLN(2)
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 3) THEN
C         Dotted line
          STR = '$$$$Dotted$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
	  CALL GSLN(3)
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 4) THEN
C         Dashed dotted line
          STR = '$$$$Dotted''Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
	  CALL GSLN(4)
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 5) THEN
C	  Don't do anything different here
C	  Color is changed at beginning of loop
          STR = '$$$$Color''Any$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 6) THEN
C	  Increase the line width 
          STR = '$$$$Width''Any$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
	  CALL DASHDC(STR,20,20)
	  CALL GSLWSC(10.) 
      	  CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
        ELSEIF (I .EQ. 7) THEN
        ELSEIF (I .EQ. 8) THEN
	ENDIF
      
C
C Reset the line type to a thin black solid line
C
        CALL GSLN(1)
	CALL GSLWSC(2.) 
	CALL GSPLCI (1)
   10 CONTINUE
C
C Create and Plot 2 Sine Curves
C

C	Set line width
	CALL GSLWSC(4.) 
C       Set the dash pattern
        STR = '$''$Draw''Any''Curve$''$''$''$''$''$''$'''
	CALL DASHDC(STR,15,20)
C       Move plotter pen
        CALL FRSTD(0.,.25)
C       Compute the curve coordinates
	DO 20 I=1,360
	  Y = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .25
          CALL VECTD(REAL(I)/360.,Y)
   20   CONTINUE

C	Set line width
	CALL GSLWSC(2.) 
C       Set the dash pattern
        STR = '$''$$''$$$''$$$$ Any''Pattern'
	CALL DASHDC(STR,20,20)
C       Set the line color to green
	CALL GSPLCI (3)
C       Move plotter pen
        CALL FRSTD(0.,.125)
C       Compute the curve coordinates
	DO 30 I=1,360
	  Y = (SIN(REAL(I)*(TWOPI/360.)) * .125) + .125
          CALL VECTD(REAL(I)/720.,Y)
   30   CONTINUE

C
C Create and plot a spiral curve
C
        CALL SET (.4,.9,.0,.55,-1.,1.,-1.,1.,1)
        STR = '$$$$$$$$$$$$$$$$$$$$$$$$$$Any''Shape'
	CALL DASHDC(STR,80,20)
C       Move plotter pen
        RAD=.001
        XCD=.25+.5*RAD*COS(0.)
        YCD=.25+.5*RAD*SIN(0.)
        CALL FRSTD(XCD,YCD)
C       Set the line color to red
	CALL GSPLCI (2)
	DO 40 ING=1,1500
          RAD=REAL(ING)/1000.
          ANG=DTR*REAL(ING-1)
          XCD=.25+.5*RAD*COS(ANG)
          YCD=.25+.5*RAD*SIN(ANG)
 	  CALL VECTD(XCD, YCD)
   40   CONTINUE


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
C     CALL PLCHLQ(0.5,0.91,'',25.,0.,0.)
      CALL FRAME
C
      END
