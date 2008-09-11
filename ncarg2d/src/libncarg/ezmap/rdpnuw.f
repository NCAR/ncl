C
C $Id: rdpnuw.f,v 1.5 2008-09-11 04:11:38 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION RDPNUW (DVIN)
C
        DOUBLE PRECISION DVIN
C
C "RDPNUW" stands for "Round Double Precision Number UpWard".  The
C value of RDPNUW(DVIN) is the largest real number whose value is
C less than or equal to the value of the double-precision argument
C DVIN.
C
        IF      (DVIN.LT.0.D0) THEN
          RDPNUW=RDPNTZ(DVIN)
        ELSE IF (DVIN.GT.0.D0) THEN
          RDPNUW=RDPNAZ(DVIN)
        ELSE
          RDPNUW=0.
        END IF
C
C Done.
C
        RETURN
C
      END
