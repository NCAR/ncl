C
C	$Id: gqtxfp.f,v 1.2 1993-01-09 02:01:45 fred Exp $
C
      SUBROUTINE GQTXFP(ERRIND,FONT,PREC)
C
C  INQUIRE TEXT FONT AND PRECISION
C
      include 'gkscom.h'
C
      INTEGER ERRIND,FONT,PREC
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        FONT = CTXFP(1)
        PREC = CTXFP(2)
      ELSE
        FONT = -1
        PREC = -1
      ENDIF
C
      RETURN
      END
