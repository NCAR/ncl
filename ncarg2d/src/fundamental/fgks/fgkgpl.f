
      PROGRAM FGKGPL
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
      CALL GPLXPL (IWKID)
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
      SUBROUTINE GPLXPL (IWKID)
C
C PURPOSE                To provide a simple demonstration of the
C                        GKS line drawing techniques.
C
C USAGE                  CALL GPLXPL (IWKID)
C
C Coordinate arrays
C
      REAL    XCD(1500), YCD(1500)
C
C Declare the constant for converting from degrees to radians.
C
      DATA DTR / .017453292519943 /
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
C Draw a world map
C
C Position the map on the plotter frame
C
      CALL MAPPOS(.01,.99,.01,.99)
C
C Define a map projection
C
      CALL MAPROJ('CE',0., -150., 0.)
C
C Choose map limits 
C
      CALL MAPSET('CO', 60., 30., -60., -30.)
C
C Choose continental outlines
C
      CALL MAPSTC ('OU', 'CO')
C
C Turn off grid lines
C
      CALL MAPSTR ('GR',0.)      
C
C Draw it
C
      CALL MAPDRW
C
C Create a spiral curve
C
C  Set the line color to red
C
      CALL GSPLCI (2)
      DO 40 ING=1,1500
         RAD=REAL(ING)/1000.
         ANG=DTR*REAL(ING-1)
         XCD(ING)=.25+.5*RAD*COS(ANG)
         YCD(ING)=.25+.5*RAD*SIN(ANG)
 40   CONTINUE
C
C Draw 3 spirals on the map over areas of high hurricane
C probability.  Draw 2 additional spirals at the bottom
C of the plot to use as a key.  Dashed spirals indicate
C relatively low areas of hurricane probability, while solid
C spirals indicate higher probability.
C
C Set the line type to solid (the default)
C
      CALL GSLN(1)
C
C Set the position of the spiral
C
      CALL SET (.24,.37,.48,.61,-1.,1.,-1.,1.,1)
C
C Draw the line
C
      CALL GPL(1500, XCD, YCD)
C
C Set the position of the spiral
C
      CALL SET (.03,.10,.43,.50,-1.,1.,-1.,1.,1)
C
C  Draw the line
C
      CALL GPL(1500, XCD, YCD)
C
C Set the position of the spiral
C
      CALL SET (.62,.75,.47,.60,-1.,1.,-1.,1.,1)
C
C Draw the line
C
 	CALL GPL(1500, XCD, YCD)
C
C Set the position of the spiral
C
      CALL SET (.25,.38,.10,.23,-1.,1.,-1.,1.,1)
C
C Draw the line
C
      CALL GPL(1500, XCD, YCD)
C
C Set the position of the spiral
C
      CALL SET (.65,.72,.10,.17,-1.,1.,-1.,1.,1)
C
C Draw the line
C
      CALL GPL(1500, XCD, YCD)
C
C Reset the plot window
C
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
      CALL PLCHLQ(0.5,0.80,'Areas of High Hurricane Probability'
     +     ,25.,0.,0.)

      CALL PLCHLQ(0.5,0.25,'Average number of tropical cyclones per 5 de
     +gree square per year',15.,0.,0.)

      CALL PLCHLQ(0.33,0.10,'> 3',15.,0.,0.)

      CALL PLCHLQ(0.70,0.10,'2<n<3',15.,0.,0.)

      CALL FRAME

      END
