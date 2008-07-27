C
C $Id: e1fnsp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION E1FNSP (ECCNTS)
C
C This function computes the constant E1.
C
      IMPLICIT REAL (A-Z)
      DATA CON1,CON2,CON3 /0.375E0,0.25E0,0.46875E0/
      DATA ONE /1.0E0/
C
      E1FNSP = CON1 * ECCNTS * (ONE + CON2 * ECCNTS *
     .         (ONE + CON3 * ECCNTS))
C
      RETURN
      END
