C
C $Id: adjlz0.f,v 1.1 1999-04-02 23:05:46 kennison Exp $
C
      DOUBLE PRECISION FUNCTION ADJLZ0 (LON)
C
C This function adjusts a longitude angle modulo 180 degrees.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA TWO,PI /2.0D0,3.14159265358979323846D0/
C
   20 ADJLZ0 = LON
      IF (ABS(LON) .LE. PI) RETURN
      TWOPI = TWO * PI
      LON = LON - SIGN (TWOPI,LON)
      GO TO 20
C
      END
