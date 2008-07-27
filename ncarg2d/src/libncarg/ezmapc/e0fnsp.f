C
C $Id: e0fnsp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION E0FNSP (ECCNTS)
C
C This function computes the constant E0.
C
      IMPLICIT REAL (A-Z)
      DATA QUART,ONE,ONEQ,THREE,SIXT /0.25E0,1.0E0,1.25E0,3.0E0,16.0E0/
C
      E0FNSP = ONE - QUART * ECCNTS * (ONE + ECCNTS / SIXT *
     .         (THREE + ONEQ * ECCNTS))
C
      RETURN
      END
