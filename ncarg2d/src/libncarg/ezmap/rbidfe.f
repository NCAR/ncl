C
C $Id: rbidfe.f,v 1.12 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBIDFE (QDFE)
C
        DOUBLE PRECISION QDFE
C
C The value of RBIDFE(QDFE) is the latitude for which the distance of
C the parallel of the Robinson projection from the equator is QDFE.
C
C Declare local variables.
C
        DOUBLE PRECISION PDFE(19),XDFE
C
        INTEGER          ITST,IBEG,IEND
C
C Define the contents of PDFE per the definition of the Robinson
C projection.  The values are associated with values of latitude from
C 0 North to 90 North at five-degree intervals.
C
C
        DATA PDFE( 1) / 0.0000D0 /
        DATA PDFE( 2) / 0.0620D0 /
        DATA PDFE( 3) / 0.1240D0 /
        DATA PDFE( 4) / 0.1860D0 /
        DATA PDFE( 5) / 0.2480D0 /
        DATA PDFE( 6) / 0.3100D0 /
        DATA PDFE( 7) / 0.3720D0 /
        DATA PDFE( 8) / 0.4340D0 /
        DATA PDFE( 9) / 0.4958D0 /
        DATA PDFE(10) / 0.5571D0 /
        DATA PDFE(11) / 0.6176D0 /
        DATA PDFE(12) / 0.6769D0 /
        DATA PDFE(13) / 0.7346D0 /
        DATA PDFE(14) / 0.7903D0 /
        DATA PDFE(15) / 0.8435D0 /
        DATA PDFE(16) / 0.8936D0 /
        DATA PDFE(17) / 0.9394D0 /
        DATA PDFE(18) / 0.9761D0 /
        DATA PDFE(19) / 1.0000D0 /
C
C XDFE is the magnitude of QDFE, limited to the range of values in the
C table.
C
        XDFE=MAX(0.D0,MIN(1.D0,ABS(QDFE)/.5072D0))
C
C Find the indices of the values in the table between which XDFE lies.
C
        IBEG=1
        IEND=19
C
  101   ITST=(IBEG+IEND)/2
C
        IF (PDFE(ITST).LE.XDFE) THEN
          IBEG=ITST
        ELSE
          IEND=ITST
        END IF
C
        IF (IEND-IBEG.GT.1) GO TO 101
C
C Now, just interpolate to find the desired latitude.
C
        RBIDFE=SIGN(5.D0*(DBLE(IBEG-1)+(      XDFE-PDFE(IBEG))/
     +                                 (PDFE(IEND)-PDFE(IBEG))),QDFE)
C
C Done.
C
        RETURN
C
      END
