C
C	$Id: gqtxci.f,v 1.2 1993-01-09 02:01:40 fred Exp $
C
      SUBROUTINE GQTXCI(ERRIND,COLI)
C
C  INQUIRE TEXT COLOR INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        COLI = CTXCI
      ELSE
        COLI = -1
      ENDIF
C
      RETURN
      END
