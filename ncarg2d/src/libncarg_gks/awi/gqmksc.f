C
C	$Id: gqmksc.f,v 1.2 1993-01-09 02:00:36 fred Exp $
C
      SUBROUTINE GQMKSC(ERRIND,MSZSF)
C
C  INQUIRE MARKER SIZE SCALE FACTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL MSZSF
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        MSZSF = CMKS
      ELSE
        MSZSF = 0
      ENDIF
C
      RETURN
      END
