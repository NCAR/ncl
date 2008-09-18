C
C $Id: rdpndw.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
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
C "RDPNDW" stands for "Round Double Precision Number DownWard".  The
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
