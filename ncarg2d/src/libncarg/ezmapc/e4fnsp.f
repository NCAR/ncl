C
C $Id: e4fnsp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
