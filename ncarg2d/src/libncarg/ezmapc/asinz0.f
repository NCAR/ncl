C
C $Id: asinz0.f,v 1.1 1999-04-02 23:05:47 kennison Exp $
C
      DOUBLE PRECISION FUNCTION ASINZ0 (CON)
C
C This function adjusts for round-off errors in computing the arcsine.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA ONE /1.0D0/
C
      IF (ABS(CON) .GT. ONE) THEN
         CON = SIGN (ONE,CON)
      ENDIF
      ASINZ0 = ASIN (CON)
      RETURN
C
      END
