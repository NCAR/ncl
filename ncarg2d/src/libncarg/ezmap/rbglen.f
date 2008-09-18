C
C $Id: rbglen.f,v 1.12 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION RBGLEN (RLAT)
C
        DOUBLE PRECISION RLAT
C
C The value of RBGLEN(RLAT) is the length of the parallel at latitude
C RLAT, stated as a fraction of the length of the equator.
C
C Declare local variables.
C
        DOUBLE PRECISION FLAT,PLEN(19)
C
        INTEGER          ILAT
C
C Define the contents of PLEN per the definition of the Robinson
C projection.  The values are associated with values of latitude from
C 0 North to 90 North at five-degree intervals.
C
        DATA PLEN( 1) / 1.0000D0 /
        DATA PLEN( 2) / 0.9986D0 /
        DATA PLEN( 3) / 0.9954D0 /
        DATA PLEN( 4) / 0.9900D0 /
        DATA PLEN( 5) / 0.9822D0 /
        DATA PLEN( 6) / 0.9730D0 /
        DATA PLEN( 7) / 0.9600D0 /
        DATA PLEN( 8) / 0.9427D0 /
        DATA PLEN( 9) / 0.9216D0 /
        DATA PLEN(10) / 0.8962D0 /
        DATA PLEN(11) / 0.8679D0 /
        DATA PLEN(12) / 0.8350D0 /
        DATA PLEN(13) / 0.7986D0 /
        DATA PLEN(14) / 0.7597D0 /
        DATA PLEN(15) / 0.7186D0 /
        DATA PLEN(16) / 0.6732D0 /
        DATA PLEN(17) / 0.6213D0 /
        DATA PLEN(18) / 0.5722D0 /
        DATA PLEN(19) / 0.5322D0 /
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
        RBGLEN=(1.D0-FLAT)*PLEN(ILAT)+FLAT*PLEN(ILAT+1)
C
C Done.
C
        RETURN
C
      END
