C
C $Id: e0fnsp.f,v 1.1 1999-04-19 22:09:40 kennison Exp $
C
      REAL FUNCTION E0FNSP (ECCNTS)
C
C This function computes the constant E0.
C
      IMPLICIT REAL (A-Z)
      DATA QUART,ONE,ONEQ,THREE,SIXT /0.25E0,1.0E0,1.25E0,3.0E0,16.0E0/
C
      E0FNSP = ONE - QUART * ECCNTS * (ONE + ECCNTS / SIXT *
     .         (THREE + ONEQ * ECCNTS))
C
      RETURN
      END
