C
C $Id: qsfnsp.f,v 1.5 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION QSFNSP (ECCENT,SINPHI,COSPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL Q).
C
      IMPLICIT REAL (A-Z)
      DATA HALF,ONE,TWO /0.5E0,1.0E0,2.0E0/
      DATA EPSLN /1.0E-7/
C
      IF (ECCENT .LT. EPSLN) GO TO 020
      CON = ECCENT * SINPHI
      QSFNSP = (ONE - ECCENT * ECCENT) * (SINPHI / (ONE - CON * CON) -
     .         (HALF / ECCENT) * LOG ((ONE - CON) / (ONE + CON)))
      RETURN
C
  020 QSFNSP = TWO * SINPHI
      RETURN
      END
