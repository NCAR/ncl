C
C $Id: e0fndp.f,v 1.1 1999-04-19 22:09:39 kennison Exp $
C
      DOUBLE PRECISION FUNCTION E0FNDP (ECCNTS)
C
C This function computes the constant E0.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA QUART,ONE,ONEQ,THREE,SIXT /0.25D0,1.0D0,1.25D0,3.0D0,16.0D0/
C
      E0FNDP = ONE - QUART * ECCNTS * (ONE + ECCNTS / SIXT *
     .         (THREE + ONEQ * ECCNTS))
C
      RETURN
      END
