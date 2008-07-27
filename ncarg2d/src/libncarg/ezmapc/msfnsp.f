C
C $Id: msfnsp.f,v 1.5 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION MSFNSP (ECCENT,SINPHI,COSPHI)
C
C This function computes the constant "small m".
C
      IMPLICIT REAL (A-Z)
      DATA ONE /1.0E0/
C
      CON = ECCENT * SINPHI
      MSFNSP = COSPHI / SQRT (ONE - CON * CON)
C
      RETURN
      END
