C
C $Id: e2fndp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION E2FNDP (ECCNTS)
C
C This function computes the constant E2.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA CON1,CON2 /0.05859375D0,0.75D0/
      DATA ONE /1.0D0/
C
      E2FNDP = CON1 * ECCNTS * ECCNTS * (ONE + CON2 * ECCNTS)
C
      RETURN
      END
