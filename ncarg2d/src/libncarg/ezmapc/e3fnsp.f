C
C $Id: e3fnsp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION E3FNSP (ECCNTS)
C
C This function computes the constant E3.
C
      IMPLICIT REAL (A-Z)
C
      E3FNSP = ECCNTS*ECCNTS*ECCNTS*(35.E0/3072.E0)
C
      RETURN
      END
