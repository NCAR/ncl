
      PROGRAM PGKEX26
C
C  Illustrate creating multiple metafiles in the same job.
C
      DIMENSION XA(2),YA(2)
      DATA XA(1),XA(2),YA(1),YA(2)/0.,1.,0.,1./
C
C  Open GKS.
C
      CALL GOPKS (6,IDUM)
C
C  Open and activate a metafile with the name META01.
C
      CALL NGSETC('ME','META01')
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Draw a single red line in the only frame in META01 .
C
      CALL GSCR(1,1,1.,0.,0.)
      CALL GSPLCI(1)
      CALL GPL(2,XA,YA)
      CALL FRAME
C
C  Deactivate and close the META01 metafile.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
C
C  Open and activate META02.
C
      CALL NGSETC('ME','META02')
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Draw a single green line in the only frame in META02 (all color
C  table entries have to be redefined).
C
      CALL GSCR(1,2,0.,1.,0.)
      CALL GSPLCI(2)
      CALL GPL(2,XA,YA)
      CALL FRAME
C
C  Deactivate and close the META02 metafile.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
C
C  Close GKS.
C
      CALL GCLKS
C
      STOP
      END
