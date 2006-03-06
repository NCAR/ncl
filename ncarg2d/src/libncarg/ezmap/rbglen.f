C
C $Id: rbglen.f,v 1.7 2006-03-06 21:02:19 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
