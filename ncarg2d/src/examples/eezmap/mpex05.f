
      PROGRAM MPEX05
C
C The program MPEX05 produces a single frame with maximal-area
C views of all the EZMAP projections of the globe.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define the label for the top of the map.
C
      CHARACTER*37 PLBL
C
      DATA PLBL / 'MAXIMAL-AREA PROJECTIONS OF ALL TYPES' /
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
      CALL MAPSTC ('OU','CO')
C
C Put meridians and parallels every 15 degrees.
C
      CALL MAPSTI ('GR',15)
C
C Reduce the label size.
C
      CALL MAPSTI ('LS',0)
C
C Lambert conformal conic.
C
      CALL MAPPOS (.025,.24375,.63125,.85)
      CALL MAPROJ ('LC',30.,0.,45.)
      CALL MAPDRW
C
C Stereographic.
C
      CALL MAPPOS (.26875,.4875,.63125,.85)
      CALL MAPROJ ('ST',0.,0.,0.)
      CALL MAPDRW
C
C Orthographic.
C
      CALL MAPPOS (.5125,.73125,.63125,.85)
      CALL MAPROJ ('OR',0.,0.,0.)
      CALL MAPDRW
C
C Lambert equal-area.
C
      CALL MAPPOS (.75625,.975,.63125,.85)
      CALL MAPROJ ('LE',0.,0.,0.)
      CALL MAPDRW
C
C Gnomonic.
C
      CALL MAPPOS (.025,.24375,.3875,.60625)
      CALL MAPROJ ('GN',0.,0.,0.)
      CALL MAPDRW
C
C Azimuthal equidistant.
C
      CALL MAPPOS (.26875,.4875,.3875,.60625)
      CALL MAPROJ ('AE',0.,0.,0.)
      CALL MAPDRW
C
C Satellite-view.
C
      CALL MAPPOS (.5125,.73125,.3875,.60625)
      CALL MAPROJ ('SV',0.,0.,0.)
      CALL MAPSTR ('SA',2.)
      CALL MAPDRW
C
C Mercator.
C
      CALL MAPPOS (.75625,.975,.3875,.60625)
      CALL MAPROJ ('ME',0.,0.,0.)
      CALL MAPDRW
C
C Cylindrical equidistant.
C
      CALL MAPPOS (.025,.4875,.13125,.3625)
      CALL MAPROJ ('CE',0.,0.,0.)
      CALL MAPDRW
C
C Mollweide type.
C
      CALL MAPPOS (.5125,.975,.13125,.3625)
      CALL MAPROJ ('MO',0.,0.,0.)
      CALL MAPDRW
C
C Put the label at the top of the plot ...
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.925,PLBL,37,2,0,0)
C
C and the labels under each sub-plot.
C
      CALL PWRIT (.134375,.61875,'LAMBERT CONFORMAL CONIC',
     +                                                  23,0,0,0)
      CALL PWRIT (.378125,.61875,'STEREOGRAPHIC',13,0,0,0)
      CALL PWRIT (.621875,.61875,'ORTHOGRAPHIC',12,0,0,0)
      CALL PWRIT (.865625,.61875,'LAMBERT EQUAL-AREA',18,0,0,0)
      CALL PWRIT (.134375,.375,'GNOMONIC',8,0,0,0)
      CALL PWRIT (.378125,.375,'AZIMUTHAL EQUIDISTANT',21,0,0,0)
      CALL PWRIT (.621875,.375,'SATELLITE-VIEW',14,0,0,0)
      CALL PWRIT (.865625,.375,'MERCATOR',8,0,0,0)
      CALL PWRIT (.25625,.11875,'CYLINDRICAL EQUIDISTANT',
     +                                                  23,0,0,0)
      CALL PWRIT (.74375,.11875,'MOLLWEIDE TYPE',14,0,0,0)
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
      SUBROUTINE MAPEOD (NOUT,NSEG,IDLS,IDRS,NPTS,PNTS)
C
      DIMENSION PNTS(*)
C
C This version of MAPEOD uses area identifiers for the outline
C dataset 'CO' to suppress all but the major global land masses.
C
      IF (IDLS.NE.  2.AND.IDRS.NE.  2) NPTS=0
C
      IF (IDLS.NE.  1.AND.IDRS.NE.  1.AND.
     +    IDLS.NE.  3.AND.IDRS.NE.  3.AND.
     +    IDLS.NE. 11.AND.IDRS.NE. 11.AND.
     +    IDLS.NE. 79.AND.IDRS.NE. 79.AND.
     +    IDLS.NE. 99.AND.IDRS.NE. 99.AND.
     +    IDLS.NE.104.AND.IDRS.NE.104.AND.
     +    IDLS.NE.107.AND.IDRS.NE.107.AND.
     +    IDLS.NE.163.AND.IDRS.NE.163     ) NPTS=0
C
C Done.
C
      RETURN
C
      END
