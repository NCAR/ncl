C
C	$Id: gqfaci.f,v 1.2 1993-01-09 02:00:08 fred Exp $
C
      SUBROUTINE GQFACI(ERRIND,COLI)
C
C  INQUIRE FILL AREA COLOUR INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
       COLI = CFACI
      ELSE
       COLI = -1
      ENDIF
C
      RETURN
      END
