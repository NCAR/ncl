C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL GPLXPL
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
      SUBROUTINE GPLXPL
C
C PURPOSE                To provide a simple demonstration of the
C                        GKS line drawing techniques.
C
C USAGE                  CALL GPLXPL
C

C Coordinate arrays
      REAL    XCD(1500), YCD(1500)
C
C Declare the constant for converting from degrees to radians.
C
      DATA DTR / .017453292519943 /
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
C Draw a world map
C
C     Position the map on the plotter frame
      CALL MAPPOS(.01,.99,.01,.99)

C     Define a map projection
      CALL MAPROJ('CE',0., -150., 0.)

C     Choose map limits 
      CALL MAPSET('CO', 60., 30., -60., -30.)

C     Choose continental outlines
      CALL MAPSTC ('OU', 'CO')

C     Turn off grid lines
      CALL MAPSTR ('GR',0.)      

C     Draw it
      CALL MAPDRW

C
C Create a spiral curve
C
C       Set the line color to red
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

C       Set the line type to solid (the default)
        CALL GSLN(1)
C       Set the position of the spiral
        CALL SET (.24,.37,.48,.61,-1.,1.,-1.,1.,1)
C       Draw the line
 	CALL GPL(1500, XCD, YCD)

C       Set the position of the spiral
        CALL SET (.03,.10,.43,.50,-1.,1.,-1.,1.,1)
C       Draw the line
 	CALL GPL(1500, XCD, YCD)

C       Set the position of the spiral
        CALL SET (.62,.75,.47,.60,-1.,1.,-1.,1.,1)
C       Draw the line
 	CALL GPL(1500, XCD, YCD)

C       Set the position of the spiral
        CALL SET (.25,.38,.10,.23,-1.,1.,-1.,1.,1)
C       Draw the line
 	CALL GPL(1500, XCD, YCD)

C       Set the position of the spiral
        CALL SET (.65,.72,.10,.17,-1.,1.,-1.,1.,1)
C       Draw the line
 	CALL GPL(1500, XCD, YCD)


C     Reset the plot window
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
      CALL PLCHLQ(0.5,0.80,'Areas of High Hurricane Probability'
     +    ,25.,0.,0.)

      CALL PLCHLQ(0.5,0.25,'Average number of tropical cyclones per 5 de
     +gree square per year',15.,0.,0.)

      CALL PLCHLQ(0.33,0.10,'> 3',15.,0.,0.)

      CALL PLCHLQ(0.70,0.10,'2<n<3',15.,0.,0.)

      CALL FRAME
C
      END
