C
C	$Id: gqpli.f,v 1.2 1993-01-09 02:01:08 fred Exp $
C
      SUBROUTINE GQPLI(ERRIND,INDEX)
C
C  INQUIRE POLYLINE INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        INDEX = CPLI
      ELSE
        INDEX = -1
      ENDIF
C
      RETURN
      END
