C
C	$Id: gqpmi.f,v 1.2 1993-01-09 02:01:15 fred Exp $
C
      SUBROUTINE GQPMI(ERRIND,INDEX)
C
C  INQUIRE POLYMARKER INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
        INDEX = CPMI
      ELSE
        INDEX = -1
      ENDIF
C
      RETURN
      END
