
      PROGRAM FDLDASHC
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
      CALL LINEEX (IWKID)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE LINEEX (IWKID)
C
C PURPOSE                To provide a simple demonstration of the
C                        different line drawing techniques.
C
C USAGE                  CALL LINEEX (IWKID)
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          Workstation id
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
C
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
C White background
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
C
C  Solid Line (the default)
C
            STR = '$$$$Solid$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            CALL DASHDC(STR,20,20)
            CALL GSLN(1)
            CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
         ELSEIF (I .EQ. 2) THEN
C
C Dashed line
C
            STR = '$$$$Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            CALL DASHDC(STR,20,20)
            CALL GSLN(2)
            CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
         ELSEIF (I .EQ. 3) THEN
C
C Dotted line
C
            STR = '$$$$Dotted$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            CALL DASHDC(STR,20,20)
            CALL GSLN(3)
            CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
         ELSEIF (I .EQ. 4) THEN
C
C Dashed dotted line
C
            STR = '$$$$Dotted''Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            CALL DASHDC(STR,20,20)
            CALL GSLN(4)
            CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
         ELSEIF (I .EQ. 5) THEN
C
C Don't do anything different here
C Color is changed at beginning of loop
C
            STR = '$$$$Color''Any$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            CALL DASHDC(STR,20,20)
            CALL LINED(X0P,Y0P,X0P+XC,Y0P+YC)
         ELSEIF (I .EQ. 6) THEN
C
C Increase the line width 
C
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
 10   CONTINUE
C
C Create and Plot 2 Sine Curves
C
C Set line width
C
      CALL GSLWSC(4.) 
C
C Set the dash pattern
C
      STR = '$''$Draw''Any''Curve$''$''$''$''$''$''$'''
      CALL DASHDC(STR,15,20)
C
C Move plotter pen
C
      CALL FRSTD(0.,.25)
C
C Compute the curve coordinates
C
      DO 20 I=1,360
         Y = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .25
         CALL VECTD(REAL(I)/360.,Y)
 20   CONTINUE
C
C Set line width
C
      CALL GSLWSC(2.) 
C
C  Set the dash pattern
C
      STR = '$''$$''$$$''$$$$ Any''Pattern'
      CALL DASHDC(STR,20,20)
C
C Set the line color to green
C
      CALL GSPLCI (3)
C
C Move plotter pen
C
      CALL FRSTD(0.,.125)
C
C Compute the curve coordinates
C
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
C
C Move plotter pen
C
      RAD=.001
      XCD=.25+.5*RAD*COS(0.)
      YCD=.25+.5*RAD*SIN(0.)
      CALL FRSTD(XCD,YCD)
C
C Set the line color to red
C
      CALL GSPLCI (2)
      DO 40 ING=1,1500
         RAD=REAL(ING)/1000.
         ANG=DTR*REAL(ING-1)
         XCD=.25+.5*RAD*COS(ANG)
         YCD=.25+.5*RAD*SIN(ANG)
         CALL VECTD(XCD, YCD)
 40   CONTINUE
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Set the line color to black
C
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
