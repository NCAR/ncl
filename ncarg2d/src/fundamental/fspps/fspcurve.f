      PROGRAM CRVEXP

C This program demonstrates how to use the SPPS CURVE routine and
C the GKS GPL routine.  Both routine are given the same set of
C coordinates to draw, however, because of the SET call the x axis
C is reversed.  GPL ignores this, and CURVE obeys it.

      REAL XCOORD(10), YCOORD(10)
      REAL X2(2), X3(2)
      REAL Y2(2), Y3(2)

C Coordinates for plot key
      DATA X2/.2, .3/
      DATA Y2/.1, .1/
      DATA X3/.7, .8/
      DATA Y3/.1, .1/

C Coordinates for line (used by both GPL and CURVE)
      DATA XCOORD/1.,2.,3.,5.,7.,9.,13.,16.,19.,23./
      DATA YCOORD/1.,3.,5.,6.,7.,10.,13.,16.,14.,17./


C
C Open GKS
C
      CALL OPNGKS

C
C Map the plotter frame to user coordinates.  Notice that
C we have reversed the X axis here.
C
      CALL SET (0.,1., 0., 1., 25., 0., 0., 20., 1)

C
C Set the line type to dashed.
C
      CALL GSLN(2)
C
C Draw the line with GPL. It will ignore the axis reversal.
C
      CALL GPL(10,XCOORD,YCOORD)

C
C Set the line type to solid
C
      CALL GSLN(1)
C
C Draw the line with CURVE.  It will observe the axis reversal
C
      CALL CURVE(XCOORD,YCOORD,10)

C
C Reset the plotter to user coordinate mapping to make it easier
C to plot the text and key.  (Axis reversal is turned off)
C
      CALL SET (0.,1., 0., 1., 0., 1., 0., 1., 1)

C
C Draw the text
C
      CALL PLCHLQ(.25, .15, 'GKS GPL Routine', 15., 0., 0.)
C Set line type to dashed.
      CALL GSLN(2)
C Draw a dashed line under the previous text
      CALL GPL(2,X2,Y2)

C Draw more text
      CALL PLCHLQ(.75, .15, 'SPPS CURVE Routine', 15., 0., 0.)
C Set line type to solid.
      CALL GSLN(1)
C Draw a solid line under the previous text
      CALL GPL(2,X3,Y3)

C
C Draw a main title
C
      CALL PLCHLQ(.5, .9, 'Drawing lines with GPL and CURVE', 20., 
     +     0., 0.)

C
C Draw a border around the plot
C
      CALL LINE(0.,0.,1.,0.)
      CALL LINE(1.,0.,1.,1.)
      CALL LINE(1.,1.,0.,1.)
      CALL LINE(0.,1.,0.,0.)

C
C Advance the frame 
C
      CALL FRAME

C
C Close GKS
C
      CALL CLSGKS

      STOP
      END
