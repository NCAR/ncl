C
C	$Id: gqtxp.f,v 1.2 1993-01-09 02:01:51 fred Exp $
C
      SUBROUTINE GQTXP(ERRIND,TXP)
C
C  INQUIRE TEXT PATH
C
      include 'gkscom.h'
C
      INTEGER ERRIND,TXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        TXP = CTXP
      ELSE
        TXP = -1
      ENDIF
C
      RETURN
      END
