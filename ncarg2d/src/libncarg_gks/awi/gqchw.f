C
C	$Id: gqchw.f,v 1.2 1993-01-09 01:59:43 fred Exp $
C
      SUBROUTINE GQCHW (ERRIND,CHW)
C
C  INQUIRE CHARACTER WIDTH
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHW = CCHH*CCHXP
      ELSE
        CHW = -1.
      ENDIF
C
      RETURN
      END
