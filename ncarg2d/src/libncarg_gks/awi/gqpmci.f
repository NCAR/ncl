C
C	$Id: gqpmci.f,v 1.2 1993-01-09 02:01:10 fred Exp $
C
      SUBROUTINE GQPMCI(ERRIND,COLI)
C
C  INQUIRE POLYMARKER COLOUR INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        COLI = CPMCI
      ELSE
        COLI = -1
      ENDIF
C
      RETURN
      END
