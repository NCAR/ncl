C
C	$Id: gqparf.f,v 1.2 1993-01-09 02:00:56 fred Exp $
C
      SUBROUTINE GQPARF(ERRIND,RFX,RFY)
C
C  INQUIRE PATTERN REFERENCE POINT
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    RFX,RFY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        RFX = CPARF(1)
        RFY = CPARF(2)
      ELSE
        RFX = -1.E20
        RFY = -1.E20
      ENDIF
C
      RETURN
      END
