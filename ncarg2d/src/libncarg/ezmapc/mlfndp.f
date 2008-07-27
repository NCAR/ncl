C
C $Id: mlfndp.f,v 1.5 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION MLFNDP (E0,E1,E2,E3,PHI)
C
C This function computes the constant M.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA TWO,FOUR,SIX /2.0D0,4.0D0,6.0D0/
C
      MLFNDP = E0 * PHI - E1 * SIN (TWO * PHI) + E2 * SIN (FOUR * PHI)
     * - E3 * SIN (SIX * PHI)
C
      RETURN
      END
