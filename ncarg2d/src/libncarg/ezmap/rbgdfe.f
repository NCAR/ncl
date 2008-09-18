C
C $Id: rbgdfe.f,v 1.12 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBGDFE (RLAT)
C
        DOUBLE PRECISION RLAT
C
C The value of RBGDFE(RLAT) is the signed distance, from the equator,
C of the parallel at latitude RLAT; its magnitude is a fraction of the
C length of the equator.
C
C Declare local variables.
C
        DOUBLE PRECISION FLAT,PDFE(19)
C
        INTEGER          ILAT
C
C Define the contents of PDFE per the definition of the Robinson
C projection.  The values are associated with values of latitude from
C 0 North to 90 North at five-degree intervals.
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
C Determine where the parallel of interest lies relative to the ones
C represented in the tables (between the ones associated with elements
C ILAT and ILAT+1 and a fractional distance FLAT from the former to the
C latter).
C
        ILAT=MAX(1,MIN(18,INT(1.D0+ABS(RLAT)/5.D0)))
C
        FLAT=1.D0+ABS(RLAT)/5.D0-DBLE(ILAT)
C
C Return the desired value.
C
        RBGDFE=.5072D0*SIGN((1.D0-FLAT)*PDFE(ILAT)+
     +                             FLAT*PDFE(ILAT+1),RLAT)
C
C Done.
C
        RETURN
C
      END
