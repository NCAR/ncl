
      PROGRAM PGKEX01
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
C  Define two triangles.
C
      DIMENSION XT1(3),YT1(3),XT2(3),YT2(3)
      DATA XT1 /10.0, 90.0, 50.0/
      DATA YT1 / 0.2,  0.2,  0.3/
      DATA XT2 /10.0, 90.0, 50.0/
      DATA YT2 / 0.5,  0.5,  0.4/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a normalization transformation and select it.
C
      CALL GSWN (1, 0.00, 100.00, 0.10, 0.50)
      CALL GSVP (1, 0.05,   0.95, 0.05, 0.95)
      CALL GSELNT (1)
C
C  Set up a color table.
C
      CALL GSCR (IWKID, 0, 1., 1., 1.)
      CALL GSCR (IWKID, 1, 1., 0., 0.)
      CALL GSCR (IWKID, 2, 0., 0., 1.)
      CALL GSCR (IWKID, 3, 0., 0., 0.)
C
C  Set fill area interior style to solid.
C
      CALL GSFAIS (1)
C
C  Fill triangle 1 with red and triangle 2 with blue.
C
      CALL GSFACI (1)
      CALL GFA (3, XT1, YT1)
      CALL GSFACI (2)
      CALL GFA (3, XT2, YT2)
C
C  Select normalization transformation 0 for drawing the text
C  to avoid effects of the non-square aspect ratio of the 
C  normalizartion transformation on the plotted characters.
C
      CALL GSELNT(0)
C
C  Set text color to red; align the text as (center, half); 
C  specify the text size; and draw it.
C
      CALL GSTXCI (3)
      CALL GSTXAL (2,3)
      CALL GSCHH  (.025)
      CALL GTX (.5,.125,'Output from a GKS program')
C
C  Advance the frame to ensure all output is plotted.
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
