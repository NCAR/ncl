C
C	$Id: gqtxal.f,v 1.2 1993-01-09 02:01:38 fred Exp $
C
      SUBROUTINE GQTXAL(ERRIND,TXALH,TXALV)
C
C  INQUIRE TEXT ALIGNMENT
C
      include 'gkscom.h'
C
      INTEGER ERRIND,TXALH,TXALV
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
        TXALH = CTXAL(1)
        TXALV = CTXAL(2)
      ELSE
        TXALH = -1
        TXALV = -1
      ENDIF
C
      RETURN
      END
