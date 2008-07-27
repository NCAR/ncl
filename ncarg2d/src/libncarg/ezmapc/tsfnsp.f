C
C $Id: tsfnsp.f,v 1.5 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION TSFNSP (ECCENT,PHI,SINPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL T).
C
      IMPLICIT REAL (A-Z)
      DATA HALF,ONE /0.5E0,1.0E0/
      DATA HALFPI /1.5707963267948966E0/
C
      CON = ECCENT * SINPHI
      COM = HALF * ECCENT
      CON = ((ONE - CON) / (ONE + CON)) ** COM
      TSFNSP = TAN (HALF * (HALFPI - PHI)) / CON
C
      RETURN
      END
