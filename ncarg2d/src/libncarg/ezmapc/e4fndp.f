C
C $Id: e4fndp.f,v 1.1 1999-04-19 22:09:46 kennison Exp $
C
      DOUBLE PRECISION FUNCTION E4FNDP (ECCENT)
C
C This function computes the constant E4.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA ONE /1.0D0/
C
      CON = ONE + ECCENT
      COM = ONE - ECCENT
      E4FNDP = SQRT ((CON ** CON) * (COM ** COM))
C
      RETURN
      END
