C
C	$Id: gqtxi.f,v 1.2 1993-01-09 02:01:48 fred Exp $
C
      SUBROUTINE GQTXI(ERRIND,INDEX)
C
C  INQUIRE TEXT INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        INDEX = CTXI
      ELSE
        INDEX = -1
      ENDIF
C
      RETURN
      END
