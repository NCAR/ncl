C
C $Id: adjlsp.f,v 1.1 1999-04-19 22:09:35 kennison Exp $
C
      REAL FUNCTION ADJLSP (LON)
C
C This function adjusts a longitude angle modulo 180 degrees.
C
      IMPLICIT REAL (A-Z)
      DATA TWO,PI /2.0E0,3.14159265358979323846E0/
C
   20 ADJLSP = LON
      IF (ABS(LON) .LE. PI) RETURN
      TWOPI = TWO * PI
      LON = LON - SIGN (TWOPI,LON)
      GO TO 20
C
      END
