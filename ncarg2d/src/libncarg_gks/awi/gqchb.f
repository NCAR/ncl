C
C	$Id: gqchb.f,v 1.2 1993-01-09 01:59:33 fred Exp $
C
      SUBROUTINE GQCHB (ERRIND,CHBX,CHBY)
C
C  INQUIRE CHARACTER BASE VECTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHBX,CHBY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
C
      IF (ERRIND .EQ. 0) THEN
        CHBX =  CCHUP(2)
        CHBY = -CCHUP(1)
      ELSE
        CHBX = 0.
        CHBY = 0.
      ENDIF
C
      RETURN
      END
