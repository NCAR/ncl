C
C $Id: msfndp.f,v 1.5 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION MSFNDP (ECCENT,SINPHI,COSPHI)
C
C This function computes the constant "small m".
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA ONE /1.0D0/
C
      CON = ECCENT * SINPHI
      MSFNDP = COSPHI / SQRT (ONE - CON * CON)
C
      RETURN
      END
