C
C	$Id: gqmk.f,v 1.2 1993-01-09 02:00:33 fred Exp $
C
      SUBROUTINE GQMK(ERRIND,MTYPE)
C 
C  INQUIRE MARKERTYPE
C
      include 'gkscom.h'
C
      INTEGER ERRIND,MTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        MTYPE = CMK
      ELSE
        MTYPE = -1
      ENDIF
C
      RETURN
      END
