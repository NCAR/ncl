
      PROGRAM MPEX03
C
C Produce a Mercator projection of the whole globe, using
C simplified continental outlines.  See the routine MAPEOD,
C below.
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
      CHARACTER*46 PLBL
C
      DATA PLBL/'SIMPLIFIED CONTINENTS ON A MERCATOR PROJECTION'/
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
C Draw the map.
C
      CALL SUPMAP (9,0.,0.,0.,0.,0.,0.,0.,1,15,2,0,IERR)
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,46,2,0,0)
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
C In honor of Cicely Ridley, the British Isles are included.
C
C Cull the segment if there's no ocean on either side of it ...
C
      IF (IDLS.NE.  2.AND.IDRS.NE.  2) NPTS=0
C
C or if it's not an edge of any of the desired areas.
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
