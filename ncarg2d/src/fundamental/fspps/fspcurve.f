
      PROGRAM FSPCURVE
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
C This program demonstrates how to use the SPPS CURVE routine and
C the GKS GPL routine.  Both routine are given the same set of
C coordinates to draw, however, because of the SET call the x axis
C is reversed.  GPL ignores this, and CURVE obeys it.
C
      REAL XCOORD(10), YCOORD(10)
      REAL X2(2), X3(2)
      REAL Y2(2), Y3(2)
C
C Coordinates for plot key
C
      DATA X2/.2, .3/
      DATA Y2/.1, .1/
      DATA X3/.7, .8/
      DATA Y3/.1, .1/
C
C Coordinates for line (used by both GPL and CURVE)
C
      DATA XCOORD/1.,2.,3.,5.,7.,9.,13.,16.,19.,23./
      DATA YCOORD/1.,3.,5.,6.,7.,10.,13.,16.,14.,17./
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
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
C
C Set line type to dashed.
C
      CALL GSLN(2)
C
C Draw a dashed line under the previous text
C
      CALL GPL(2,X2,Y2)
C
C Draw more text
C
      CALL PLCHLQ(.75, .15, 'SPPS CURVE Routine', 15., 0., 0.)
C
C Set line type to solid.
C
      CALL GSLN(1)
C
C Draw a solid line under the previous text
C
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
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END
