C
C	$Id: gqewk.f,v 1.2 1993-01-09 02:00:05 fred Exp $
C
      SUBROUTINE GQEWK(N,ERRIND,NUMBER,WKTYP)
C
C  INQUIRE LIST element OF AVAILABLE WORKSTATION TYPES
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,NUMBER,WKTYP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if N is in bounds.
C
      IF (N.LT.0 .OR. N.GT.WK) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      NUMBER = WK
      IF (N .EQ. 0) RETURN
      WKTYP = LSWK(N)
      RETURN
C
  100 CONTINUE
      NUMBER = WK
      WKTYP = -1
      RETURN
      END
