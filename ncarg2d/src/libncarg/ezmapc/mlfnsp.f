C
C $Id: mlfnsp.f,v 1.1 1999-04-19 22:09:49 kennison Exp $
C
      REAL FUNCTION MLFNSP (E0,E1,E2,E3,PHI)
C
C This function computes the constant M.
C
      IMPLICIT REAL (A-Z)
      DATA TWO,FOUR,SIX /2.0E0,4.0E0,6.0E0/
C
      MLFNSP = E0 * PHI - E1 * SIN (TWO * PHI) + E2 * SIN (FOUR * PHI)
     * - E3 * SIN (SIX * PHI)
C
      RETURN
      END
