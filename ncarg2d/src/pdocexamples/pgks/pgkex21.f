      PROGRAM MMETA
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1)
C
C  Illustrate creating multiple metafiles in the same job.
C
      CHARACTER*80 FNAME,CDUM
      DIMENSION XA(2),YA(2)
      DATA XA(1),XA(2),YA(1),YA(2)/0.,1.,0.,1./
C
C  Open GKS.
C
      CALL GOPKS (IERRF,IDUM)
C
C  Open and activate a metafile with the name META01.
C
      FNAME = 'META01'
      CALL GESC(-1391,1,FNAME,1,IDUM,CDUM)
      CALL GOPWK (IWKID, LUNIT, 1)
      CALL GACWK (IWKID)
C
C  Draw a single red line in the only frame in META01 .
C
      CALL GSCR(IWKID,1,1.,0.,0.)
      CALL GSPLCI(1)
      CALL GPL(2,XA,YA)
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
      CALL GESC(-1391,1,FNAME,1,IDUM,CDUM)
      CALL GOPWK (IWKID, LUNIT, 1)
      CALL GACWK (IWKID)
C
C  Draw a single green line in the only frame in META02 (all color
C  table entries have to be redefined).
C
      CALL GSCR(IWKID,2,0.,1.,0.)
      CALL GSPLCI(2)
      CALL GPL(2,XA,YA)
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
