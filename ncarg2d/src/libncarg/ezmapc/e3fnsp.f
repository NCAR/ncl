C
C $Id: e3fnsp.f,v 1.1 1999-04-19 22:09:45 kennison Exp $
C
      REAL FUNCTION E3FNSP (ECCNTS)
C
C This function computes the constant E3.
C
      IMPLICIT REAL (A-Z)
C
      E3FNSP = ECCNTS*ECCNTS*ECCNTS*(35.E0/3072.E0)
C
      RETURN
      END
