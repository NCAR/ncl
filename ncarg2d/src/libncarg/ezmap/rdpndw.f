C
C $Id: rdpndw.f,v 1.3 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION RDPNDW (DVIN)
C
        DOUBLE PRECISION DVIN
C
C "RDPNUW" stands for "Round Double Precision Number UpWard".  The
C value of RDPNDW(DVIN) is the smallest real number whose value is
C greater than or equal to the value of the double-precision argument
C DVIN.
C
        IF      (DVIN.LT.0.D0) THEN
          RDPNDW=RDPNAZ(DVIN)
        ELSE IF (DVIN.GT.0.D0) THEN
          RDPNDW=RDPNTZ(DVIN)
        ELSE
          RDPNDW=0.
        END IF
C
C Done.
C
        RETURN
C
      END
