C
C	$Id: gqlvks.f,v 1.2 1993-01-09 02:00:26 fred Exp $
C
      SUBROUTINE GQLVKS(ERRIND,LEVEL)
C
C  INQUIRE LEVEL OF GKS
C
      include 'gkscom.h'
C
      INTEGER ERRIND,LEVEL
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        LEVEL = 0
      ELSE
        LEVEL = -4
      ENDIF
C
      RETURN
      END
