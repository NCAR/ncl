C
C $Id: e2fnsp.f,v 1.1 1999-04-19 22:09:43 kennison Exp $
C
      REAL FUNCTION E2FNSP (ECCNTS)
C
C This function computes the constant E2.
C
      IMPLICIT REAL (A-Z)
      DATA CON1,CON2 /0.05859375E0,0.75E0/
      DATA ONE /1.0E0/
C
      E2FNSP = CON1 * ECCNTS * ECCNTS * (ONE + CON2 * ECCNTS)
C
      RETURN
      END
