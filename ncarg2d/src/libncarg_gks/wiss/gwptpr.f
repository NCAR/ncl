C
C	$Id: gwptpr.f,v 1.5 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWPTPR (BUFFER,BITS,COUNT,GKSERR)
C
C  Put the operand string into the segment buffer.
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
C  Operand and instruction communication.
C
      include 'gwiins.h'
C
      SAVE
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
      MOVIT = MIN(WCBYT,CTEMP)
      CALL GWILOD(BUFFER(STRT),BITS,MOVIT,GKSERR)
      IF (GKSERR .NE. ALLOK) RETURN
C
C  Check if another partition has to be started.
C
      CTEMP = CTEMP - MOVIT
      MCCBYT = MCCBYT - (MOVIT*BITS)/8
      IF (CTEMP .NE. 0) THEN
C
C  New partition required so set up the instruction.
C
        STRT = STRT + MOVIT
C
C  Take remainder of bytes left in current partition (they must be used).
C
        TCBYT = MCNBYT + MCCBYT
        CALL GWPART(TCBYT,GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
C
C  Move more operands into new partition.
C
        GO TO 10
      END IF
C
      RETURN
      END
