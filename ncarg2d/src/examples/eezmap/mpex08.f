
      PROGRAM MPEX08
C
C Produce a Mercator projection of the whole globe, using a
C version of MAPUSR which dots the grid lines and dashes the
C continental outlines.
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
      CHARACTER*30 PLBL
C
      DATA PLBL / 'ILLUSTRATING THE USE OF MAPUSR' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Weird up the projection a little.
C
      CALL SUPMAP (9,0.,0.,90.,0.,0.,0.,0.,1,15,2,0,IERR)
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,30,2,0,0)
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
      SUBROUTINE MAPUSR (IPRT)
C
C This version of MAPUSR forces the grid lines to be dotted and
C the outlines to be dashed.
C
C Certain local parameters must be saved from call to call.
C
      SAVE IDTF,IDBD
C
C If IPRT is positive, a part is about to be drawn.  Save the
C dotted/solid flag and/or the distance between dots and then
C reset them and/or the dash pattern.
C
      IF (IPRT.GT.0) THEN
        IF (IPRT.EQ.2) THEN
          CALL MAPGTI ('DL',IDTF)
          CALL MAPGTI ('DD',IDBD)
          CALL MAPSTI ('DL',1)
          CALL MAPSTI ('DD',24)
        ELSE IF (IPRT.EQ.5) THEN
          CALL MAPGTI ('DL',IDTF)
          CALL MAPSTI ('DL',0)
          CALL DASHDB (21845,0,0,0)
        END IF
C
      ELSE
C
C Otherwise, a part has just been drawn.  Restore saved settings
C and/or select a solid dash pattern.
C
        IF (IPRT.EQ.-2) THEN
          CALL MAPSTI ('DL',IDTF)
          CALL MAPSTI ('DD',IDBD)
        ELSE IF (IPRT.EQ.-5) THEN
          CALL MAPSTI ('DL',IDTF)
          CALL DASHDB (65535,0,0,0)
        END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
