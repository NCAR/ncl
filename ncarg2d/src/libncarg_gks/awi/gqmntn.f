C
C	$Id: gqmntn.f,v 1.2 1993-01-09 02:00:38 fred Exp $
C
      SUBROUTINE GQMNTN(ERRIND,MAXTNR)
C
C  INQUIRE MAXIMUM NORMALIZATION TRANSFORMATION NUMBER
C
      include 'gkscom.h'
C
      INTEGER ERRIND,MAXTNR
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        MAXTNR = MNT
      ELSE
        MAXTNR = -1
      ENDIF
C
      RETURN
      END
