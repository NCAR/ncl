C
C	$Id: gqchsp.f,v 1.2 1993-01-09 01:59:38 fred Exp $
C
      SUBROUTINE GQCHSP(ERRIND,CHSP)
C
C  INQUIRE CHARACTER SPACING
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHSP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHSP = CCHSP
      ELSE
        CHSP = -1.E20
      ENDIF
C
      RETURN
      END
