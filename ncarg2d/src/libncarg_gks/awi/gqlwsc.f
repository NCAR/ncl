C
C	$Id: gqlwsc.f,v 1.2 1993-01-09 02:00:31 fred Exp $
C
      SUBROUTINE GQLWSC(ERRIND,LWIDTH)
C
C  INQUIRE LINEWIDTH SCALE FACTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    LWIDTH
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        LWIDTH = CLWSC
      ELSE
        LWIDTH = 0.
      ENDIF
C
      RETURN
      END
