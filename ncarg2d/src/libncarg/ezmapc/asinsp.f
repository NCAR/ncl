C
C $Id: asinsp.f,v 1.1 1999-04-19 22:09:36 kennison Exp $
C
      REAL FUNCTION ASINSP (CON)
C
C This function adjusts for round-off errors in computing the arcsine.
C
      IMPLICIT REAL (A-Z)
      DATA ONE /1.0E0/
C
      IF (ABS(CON) .GT. ONE) THEN
         CON = SIGN (ONE,CON)
      ENDIF
C
      ASINSP = ASIN (CON)
C
      RETURN
C
      END
