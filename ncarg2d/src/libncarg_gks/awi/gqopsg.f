C
C	$Id: gqopsg.f,v 1.2 1993-01-09 02:00:46 fred Exp $
C
      SUBROUTINE GQOPSG(ERRIND,SGNA)
C
C  INQUIRE NAME OF OPEN SEGMENT
C
      include 'gkscom.h'
C
      INTEGER ERRIND,SGNA
      ERRIND = 0
C
      IF (OPS .EQ. GSGOP) THEN
        SGNA = CURSEG
      ELSE
        ERRIND = 4
      ENDIF
C
      RETURN
      END
