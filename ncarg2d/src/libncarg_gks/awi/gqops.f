C
C	$Id: gqops.f,v 1.2 1993-01-09 02:00:43 fred Exp $
C
      SUBROUTINE GQOPS(OPSTA)
C
C  INQUIRE OPERATING STATE VALUE
C
      include 'gkscom.h'
C
      INTEGER OPSTA
      OPSTA = OPS
C
      RETURN
      END
