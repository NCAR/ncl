C
C	$Id: mpex01.f,v 1.2 1994-07-08 16:28:08 stautler Exp $
C
      PROGRAM EXMPL1
C
C The program EXMPL1 produces a map of the U.S., using a Lambert
C conformal conic.
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Define the label for the top of the map.
C
      CHARACTER*37 PLBL
C
      DATA PLBL / 'THE U.S. ON A LAMBERT CONFORMAL CONIC' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU','US')
C
C Set the projection-type parameters.
C
      CALL MAPROJ ('LC',30.,-100.,45.)
C
C Set the limits parameters.
C
      CALL MAPSET ('CO',22.6,-120.,46.9,-64.2)
C
C Draw the map.
C
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.925,PLBL,37,2,0,0)
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
