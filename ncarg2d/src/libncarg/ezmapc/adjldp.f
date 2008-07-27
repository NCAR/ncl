C
C $Id: adjldp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION ADJLDP (LON)
C
C This function adjusts a longitude angle modulo 180 degrees.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA TWO,PI /2.0D0,3.14159265358979323846D0/
C
   20 ADJLDP = LON
      IF (ABS(LON) .LE. PI) RETURN
      TWOPI = TWO * PI
      LON = LON - SIGN (TWOPI,LON)
      GO TO 20
C
      END
