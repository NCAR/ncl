C
C	$Id: ezy.f,v 1.1.1.1 1992-04-17 22:31:04 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
C Do statistics-gathering call.
C
      LOGICAL Q8Q4
      SAVE Q8Q4
      DATA Q8Q4 /.TRUE./
      IF (Q8Q4) THEN
        CALL Q8QST4('GRAPHX','AUTOGRAPH','EZY','VERSION 07')
        Q8Q4 = .FALSE.
      ENDIF
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
