
      PROGRAM FSPLINE
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
C This is an example program which demonstrates how to use
C the NCAR Graphics LINE routine.  The program draws a
C simple flow diagram and labels the diagram objects.
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up a color table
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
C Set the line color
C 
      CALL GSPLCI(2)
C
C Set the plotter mapping space
C
      CALL SET (0.,1.,0.,1.,0.,20., 0.,20.,1)
C
C Set the line width
C
      CALL GSLWSC(4.)
C
C
C Draw a box
C 
      CALL LINE(2.,8.,2.,10.)
      CALL LINE(2.,10.,5.,10.)
      CALL LINE(5.,10.,5.,8.)
      CALL LINE(5.,8.,2.,8.)
C
C Add text
C
      CALL PLCHLQ(3.5,9.,'Read I',15.,0.,0.)
C
C Draw a diamond
C
      CALL LINE(8.,9.,9.5,11.)
      CALL LINE(9.5,11.,11.,9.)
      CALL LINE(11.,9.,9.5,7.)
      CALL LINE(9.5,7.,8.,9.)
C
C Add text in Diamond
C
      CALL PLCHLQ(9.5,9.,'Is I<3?',15.,0.,0.)
      CALL PLCHLQ(10.,11.5,'yes',15.,0.,0.)
      CALL PLCHLQ(9.9,6.5,'no',15.,0.,0.)
C
C Draw a box
C 
      CALL LINE(15.,13.,18.,13.)
      CALL LINE(18.,13.,18.,11.)
      CALL LINE(18.,11.,15.,11.)
      CALL LINE(15.,11.,15.,13.)
C
C Add text in box
C
      CALL PLCHLQ(16.5,12.,'I = I+1',15.,0.,0.)
C
C Draw a box
C 
      CALL LINE(15.,7.,18.,7.)
      CALL LINE(18.,7.,18.,5.)
      CALL LINE(18.,5.,15.,5.)
      CALL LINE(15.,5.,15.,7.)
C
C Add text in box
C
      CALL PLCHLQ(16.5,6.,'I = I-1',15.,0.,0.)
C
C Set the line width
C
      CALL GSLWSC(2.)
C
C Set the line color
C
      CALL GSPLCI(4)
C
C Connect the objects
C
      CALL LINE(5.,9.,8.,9.)
      CALL LINE(9.5,11.,9.5,12.)
      CALL LINE(9.5,12.,15.,12.)
      CALL LINE(9.5,7.,9.5,6.)
      CALL LINE(9.5,6.,15.,6.)
C
C Label top of plot
C
      CALL PLCHHQ(10.,15.,'Decision Flow Chart',25.,0.,0.)
C
C Draw a boundary around plotter frame
C
      CALL LINE(0.,0.,20.,0.)
      CALL LINE(20.,0.,20.,20.)
      CALL LINE(20.,20.,0.,20.)
      CALL LINE(0.,20.,0.,0.)

      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP

      END
