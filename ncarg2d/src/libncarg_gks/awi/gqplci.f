C
C	$Id: gqplci.f,v 1.2 1993-01-09 02:01:03 fred Exp $
C
      SUBROUTINE GQPLCI(ERRIND,COLI)
C
C  INQUIRE POLYLINE COLOUR INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
        COLI = CPLCI
      ELSE
        COLI = -1
      ENDIF
C
      RETURN
      END
