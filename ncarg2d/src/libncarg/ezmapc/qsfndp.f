C
C $Id: qsfndp.f,v 1.5 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION QSFNDP (ECCENT,SINPHI,COSPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL Q).
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA HALF,ONE,TWO /0.5D0,1.0D0,2.0D0/
      DATA EPSLN /1.0D-7/
C
      IF (ECCENT .LT. EPSLN) GO TO 020
      CON = ECCENT * SINPHI
      QSFNDP = (ONE - ECCENT * ECCENT) * (SINPHI / (ONE - CON * CON) -
     .         (HALF / ECCENT) * LOG ((ONE - CON) / (ONE + CON)))
      RETURN
C
  020 QSFNDP = TWO * SINPHI
      RETURN
      END
