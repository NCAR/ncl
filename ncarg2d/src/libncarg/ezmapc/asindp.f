C
C $Id: asindp.f,v 1.1 1999-04-19 22:09:36 kennison Exp $
C
      DOUBLE PRECISION FUNCTION ASINDP (CON)
C
C This function adjusts for round-off errors in computing the arcsine.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA ONE /1.0D0/
C
      IF (ABS(CON) .GT. ONE) THEN
         CON = SIGN (ONE,CON)
      ENDIF
C
      ASINDP = ASIN (CON)
C
      RETURN
C
      END
