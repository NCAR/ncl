C
C $Id: mdlblt.f,v 1.2 2001-11-02 22:36:59 kennison Exp $
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
      SUBROUTINE MDLBLT (XCOP,YCOP,XCOQ,YCOQ,OFFX,OFFY,SIZE,ANGL,CENT)
C
        REAL XCOP,YCOP,XCOQ,YCOQ,OFFX,OFFY,SIZE,ANGL,CENT
C
        DOUBLE PRECISION PLAT,PLON,QLAT,QLON,DNCE,RLAT,UCOR,VCOR
C
C This routine attempts to put tick marks and latitude labels along an
C arbitrary line on a map drawn by EZMAP.  XCOP, YCOP, XCOQ, and YCOQ
C are the X and Y coordinates, in the fractional system, of two points,
C P and Q.  The latitude labels will be placed at positions between P
C and Q where the latitude is a multiple of some "nice" value.  OFFX
C and OFFY specify fractional-system X and Y components of an offset
C vector from a point to be labeled to a point relative to which the
C label will be positioned.  SIZE is the desired character width, in
C the fractional system.  ANGL is the angle, in degrees, at which the
C label is to be written.  CENT specifies what part of the label is to
C be placed on the end of the offset vector; CENT = -1. means the center
C of the left end of the label, CENT = 0. means the center of the whole
C label, and CENT = +1. means the center of the right end of the label.
C
C Care should be taken to choose points P and Q such that the value of
C latitude is monotonically increasing or decreasing along the line
C from P to Q.
C
        CHARACTER*128 CHRS
C
C Declare other local variables.
C
        INTEGER       I,IPLT,IQLT,NCHR
C
C Get the latitude and longitude of P.
C
        CALL MDPTRI (DBLE(CFUX(XCOP)),DBLE(CFUY(YCOP)),PLAT,PLON)
        IF (PLAT.EQ.1.D12) RETURN
C
C Get the latitude and longitude of Q.
C
        CALL MDPTRI (DBLE(CFUX(XCOQ)),DBLE(CFUY(YCOQ)),QLAT,QLON)
        IF (QLAT.EQ.1.D12) RETURN
C
C Compute a "nice" latitude interval to use.
C
        CALL MDGNIN (ABS(QLAT-PLAT)/4.D0,DNCE)
        IF (DNCE.EQ.0.D0) RETURN
C
C Compute limits to be used in the labeling loop.
C
        IPLT=INT((PLAT+90.D0)/DNCE)
        IQLT=INT((QLAT+90.D0)/DNCE)
C
C Loop to generate the labels.
C
        DO 101 I=MIN(IPLT,IQLT)-1,MAX(IPLT,IQLT)+1
          RLAT=DBLE(I)*DNCE-90.D0
          IF (RLAT.GT.MIN(PLAT,QLAT)-DNCE/100.D0.AND.
     +        RLAT.LT.MAX(PLAT,QLAT)+DNCE/100.D0) THEN
            CALL MDILAT (PLAT,PLON,DBLE(CFUX(XCOP)),DBLE(CFUY(YCOP)),
     +                   QLAT,QLON,DBLE(CFUX(XCOQ)),DBLE(CFUY(YCOQ)),
     +                   RLAT,UCOR,VCOR)
            CALL MDLACH (RLAT,CHRS,NCHR)
            CALL PLCHHQ (CFUX(CUFX(REAL(UCOR))+OFFX),
     +                   CFUY(CUFY(REAL(VCOR))+OFFY),
     +                   CHRS(1:NCHR),SIZE,ANGL,CENT)
          END IF
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
