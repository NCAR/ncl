C
C	$Id: gqchh.f,v 1.2 1993-01-09 01:59:35 fred Exp $
C
      SUBROUTINE GQCHH (ERRIND,CHH)
C
C  INQUIRE CHARACTER HEIGHT
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHH
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHH = CCHH
      ELSE
        CHH = -1.
      ENDIF
C
      RETURN
      END
