C
C $Id: e3fndp.f,v 1.1 1999-04-19 22:09:44 kennison Exp $
C
      DOUBLE PRECISION FUNCTION E3FNDP (ECCNTS)
C
C This function computes the constant E3.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
C
      E3FNDP = ECCNTS*ECCNTS*ECCNTS*(35.D0/3072.D0)
C
      RETURN
      END
