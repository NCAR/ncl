C
C $Id: msfndp.f,v 1.1 1999-04-19 22:09:58 kennison Exp $
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
