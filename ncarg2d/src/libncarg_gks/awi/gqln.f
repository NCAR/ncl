C
C	$Id: gqln.f,v 1.2 1993-01-09 02:00:23 fred Exp $
C
      SUBROUTINE GQLN(ERRIND,LTYPE)
C
C  INQUIRE LINETYPE
C
      include 'gkscom.h'
C
      INTEGER ERRIND,LTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        LTYPE = CLN
      ELSE
        LTYPE = -1
      ENDIF
C
      RETURN
      END
