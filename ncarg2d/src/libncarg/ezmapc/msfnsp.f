C
C $Id: msfnsp.f,v 1.1 1999-04-19 22:09:58 kennison Exp $
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
