C
C	$Id: gqcntn.f,v 1.2 1993-01-09 01:59:50 fred Exp $
C
      SUBROUTINE GQCNTN(ERRIND,CTNR)
C
C  INQUIRE CURRENT NORMALIZATION TRANSFORMATION NUMBER
C
      include 'gkscom.h'
C
      INTEGER ERRIND,CTNR
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CTNR = CNT
      ELSE
        CTNR = -1
      ENDIF
C
      RETURN
      END
