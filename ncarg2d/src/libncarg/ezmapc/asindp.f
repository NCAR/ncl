C
C $Id: asindp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION ASINDP (CON)
C
C This function adjusts for round-off errors in computing the arcsine.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA ONE /1.0D0/
C
      IF (ABS(CON) .GT. ONE) THEN
         CON = SIGN (ONE,CON)
      ENDIF
C
      ASINDP = ASIN (CON)
C
      RETURN
C
      END
