C
C	$Id: gqchup.f,v 1.2 1993-01-09 01:59:40 fred Exp $
C
      SUBROUTINE GQCHUP(ERRIND,CHUX,CHUY)
C
C  INQUIRE CHARACTER UP VECTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHUX,CHUY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHUX = CCHUP(1)
        CHUY = CCHUP(2)
      ELSE
        CHUX = 0.
        CHUY = 0.
      ENDIF
C
      RETURN
      END
