C
C $Id: e3fndp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION E3FNDP (ECCNTS)
C
C This function computes the constant E3.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
C
      E3FNDP = ECCNTS*ECCNTS*ECCNTS*(35.D0/3072.D0)
C
      RETURN
      END
