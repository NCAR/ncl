C
C	$Id: gqfais.f,v 1.2 1993-01-09 02:00:16 fred Exp $
C
      SUBROUTINE GQFAIS(ERRIND,INTS)
C
C  INQUIRE FILL AREA INTERIOR STYLE
C
      include 'gkscom.h'
C
      INTEGER ERRIND,INTS
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
       INTS = CFAIS
      ELSE
       INTS = -1
      ENDIF
C
      RETURN
      END
