C
C ---------------------------------------------------------------------
C
      SUBROUTINE EZXY (XDRA,YDRA,NPTS,LABG)
C
      REAL XDRA(*),YDRA(*)
C
      CHARACTER*(*) LABG
C
C The routine EZXY draws one curve through the points (XDRA(I),YDRA(I)),
C for I = 1, 2, ... NPTS.
C
      CALL AGGETI ('SET .',ISET)
      CALL AGGETI ('FRAM.',IFRA)
C
      CALL AGEZSU (2,XDRA,YDRA,NPTS,1,NPTS,LABG,IIVX,IIEX,IIVY,IIEY)
      CALL AGBACK
C
      IF (ISET.GE.0) CALL AGCURV (XDRA,1,YDRA,1,NPTS,1)
C
      IF (IFRA.EQ.1) CALL FRAME
C
      RETURN
C
      END
