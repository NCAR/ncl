C
C	$Id: gputpr.f,v 1.2 1993-01-09 02:07:36 fred Exp $
C
      SUBROUTINE GPUTPR (BUFFER,BITS,COUNT,GKSERR)
C
C  Put the operand string into the metafile buffer.
C
C  INPUT
C    BUFFER -- List of operands to move.
C    BITS   -- Precision of the operands.
C    COUNT  -- Number of operands in the buffer.
C
C  OUTPUT
C    GKSERR -- Error status.
C
C  All data is type integer unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION BUFFER(*)
C
      include 'g01prm.h'
      include 'g01ins.h'
C
      DATA ALLOK /0/
C
      CTEMP = COUNT
      STRT = 1
C
   10 CONTINUE
C
C  Determine the number of operand words left in the current partition.
C
      WCBYT = (MCCBYT*8)/BITS
C
C  Compute and move the allowed number of operands.
C
      MOVIT = MIN0(WCBYT,CTEMP)
      CALL GMFLOD(BUFFER(STRT),BITS,MOVIT,GKSERR)
      IF (GKSERR .NE. ALLOK) RETURN
C
C  Check if another partition has to be started.
C
      CTEMP = CTEMP - MOVIT
      MCCBYT = MCCBYT - (MOVIT*BITS)/8
      IF (CTEMP .NE. 0) THEN
C
C  New partition required, so set up the element.
C
        STRT = STRT + MOVIT
C
C  Take the remainder of bytes left in current partition 
C  (they must be used).
C
        TCBYT = MCNBYT + MCCBYT
        CALL GMPART(TCBYT,GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
C
C  Move more operands into new partition.
C
        GO TO 10
      END IF
C
      RETURN
      END
