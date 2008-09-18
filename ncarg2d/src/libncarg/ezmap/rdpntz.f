C
C $Id: rdpntz.f,v 1.7 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION RDPNTZ (DVIN)
C
        DOUBLE PRECISION DVIN,DVAL,SVAL,TVAL
C
C "RDPNTZ" stands for "Round Double Precision Number Toward Zero".  The
C value of RDPNTZ is the real number having the largest absolute value
C which is less than or equal to the value of the double-precision
C argument DVIN.
C
        IF (DVIN.LT.0.D0) THEN
          DVAL=-DVIN
          DSGN=-1.
        ELSE
          DVAL=+DVIN
          DSGN=+1.
        END IF
C
        IF (DBLE(REAL(DVAL)).LE.DVAL) THEN
          RDPNTZ=SIGN(REAL(DVAL),DSGN)
        ELSE
          SVAL=.5D0
          TVAL=.25D0
          NTMS=0
  101     IF (DBLE(REAL(SVAL*DVAL)).LE.DVAL) THEN
            RDPNTZ=SIGN(REAL(SVAL*DVAL),DSGN)
            SVAL=SVAL+TVAL
            TVAL=TVAL/2.D0
            NTMS=NTMS+1
            IF (NTMS.LT.64) GO TO 101
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
