
      PROGRAM FGKE03
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1)
C
C  Illustrate creating multiple metafiles in the same job.
C
      CHARACTER*80 FNAME,CDUM
C
C  Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
C
C  Open and activate a metafile with the name META01.
C
      FNAME = 'META01'
      CALL GESC(-1391,1,FNAME,1,1,CDUM)
      CALL GOPWK (IWKID, LUNIT, 1)
      CALL GACWK (IWKID)
C
C  Draw a single polymarker in the center of the picture.
C
      CALL GPM(1,.5,.5)
      CALL FRAME
C
C  Deactivate and close the META01 metafile.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
C
C  Open and activate META02.
C
      FNAME = 'META02'
      CALL GESC(-1391,1,FNAME,1,1,CDUM)
      CALL GOPWK (IWKID, LUNIT, 1)
      CALL GACWK (IWKID)
C
C  Draw a single polymarker in the upper half of the picture.
C
      CALL GPM(1,.5,.75)
      CALL FRAME
C
C  Deactivate and close the META02 metafile.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
C
C  Close GKS.
C
      CALL GCLKS
C
      STOP
      END
