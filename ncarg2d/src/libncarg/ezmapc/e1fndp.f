C
C $Id: e1fndp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION E1FNDP (ECCNTS)
C
C This function computes the constant E1.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA CON1,CON2,CON3 /0.375D0,0.25D0,0.46875D0/
      DATA ONE /1.0D0/
C
      E1FNDP = CON1 * ECCNTS * (ONE + CON2 * ECCNTS *
     .         (ONE + CON3 * ECCNTS))
C
      RETURN
      END
