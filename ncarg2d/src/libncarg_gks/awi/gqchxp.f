C
C	$Id: gqchxp.f,v 1.2 1993-01-09 01:59:45 fred Exp $
C
      SUBROUTINE GQCHXP(ERRIND,CHXP)
C
C  INQUIRE CHARACTER EXPANSION FACTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHXP = CCHXP
      ELSE
        CHXP = 0.
      ENDIF
C
      RETURN
      END
