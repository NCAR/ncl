
      PROGRAM MPEX04
C
C This program produces a single frame, with polar stereographic
C views of the poles and a Mercator projection of the rest.
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
C Define the label for the top of the map.
C
      CHARACTER*26 PLBL
C
      DATA PLBL / 'OMNIS TERRA IN PARTES TRES' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','PO')
C
C Use dotted outlines and move the dots a little closer together
C than normal.
C
      CALL MAPSTI ('DO',1)
      CALL MAPSTI ('DD',24)
C
C Do the Mercator projection of the equatorial belt first.
C
      CALL MAPPOS (.05,.95,.05,.5)
      CALL MAPROJ ('ME',0.,0.,0.)
      CALL MAPSET ('LI',-3.1416,+3.1416,-1.5708,+1.5708)
      CALL MAPDRW
C
C Switch to an elliptical (in this case, circular) boundary.
C
      CALL MAPSTI ('EL',1)
C
C Do a polar stereographic view of the North Pole ...
C
      CALL MAPPOS (.07,.48,.52,.93)
      CALL MAPROJ ('ST',90.,0.,-90.)
      CALL MAPSET ('AN',30.,30.,30.,30.)
      CALL MAPDRW
C
C and then a similar view of the South Pole.
C
      CALL MAPPOS (.52,.93,.52,.93)
      CALL MAPROJ ('ST',-90.,0.,-90.)
      CALL MAPSET ('AN',30.,30.,30.,30.)
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,26,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END
