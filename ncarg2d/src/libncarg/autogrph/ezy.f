C
C $Id: ezy.f,v 1.4 1996-04-18 17:46:24 kennison Exp $
C
      SUBROUTINE EZY (YDRA,NPTS,LABG)
C
      REAL XDRA(1),YDRA(*)
C
      CHARACTER*(*) LABG
C
C The subroutine EZY draws one curve through the points (I,YDRA(I)), for
C I = 1, 2, ... NPTS.
C
      CALL AGGETI ('SET .',ISET)
      CALL AGGETI ('FRAM.',IFRA)
C
      CALL AGEZSU (1,XDRA,YDRA,NPTS,1,NPTS,LABG,IIVX,IIEX,IIVY,IIEY)
      CALL AGBACK
C
      IF (ISET.GE.0) CALL AGCURV (XDRA,0,YDRA,1,NPTS,1)
C
      IF (IFRA.EQ.1) CALL FRAME
C
      RETURN
C
      END
