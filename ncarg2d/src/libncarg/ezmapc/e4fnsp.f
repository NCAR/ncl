C
C $Id: e4fnsp.f,v 1.1 1999-04-19 22:09:46 kennison Exp $
C
      REAL FUNCTION E4FNSP (ECCENT)
C
C This function computes the constant E4.
C
      IMPLICIT REAL (A-Z)
      DATA ONE /1.0E0/
C
      CON = ONE + ECCENT
      COM = ONE - ECCENT
      E4FNSP = SQRT ((CON ** CON) * (COM ** COM))
C
      RETURN
      END
