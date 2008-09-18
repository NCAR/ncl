C
C $Id: mdllnd.f,v 1.6 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDLLND (XCOP,YCOP,XCOQ,YCOQ,OFFX,OFFY,SIZE,ANGL,CENT)
C
        REAL XCOP,YCOP,XCOQ,YCOQ,OFFX,OFFY,SIZE,ANGL,CENT
C
        DOUBLE PRECISION PLAT,PLON,QLAT,QLON,DNCE,RLAT,RLON,UCOR,VCOR
C
C This routine attempts to put tick marks and longitude labels along an
C arbitrary line on a map drawn by EZMAP.  XCOP, YCOP, XCOQ, and YCOQ
C are the X and Y coordinates, in the fractional system, of two points,
C P and Q.  The longitude labels will be placed at positions between P
C and Q where the longitude is a multiple of some "nice" value.  OFFX
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
C longitude is monotonically increasing or decreasing along the line
C from P to Q.
C
        CHARACTER*128 CHRS
C
C Declare other local variables.
C
        INTEGER       I,IPLN,IQLN,NCHR
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
C Get the latitude and longitude of a point R that is 1/100th of the
C way along the straight line from P to Q.
C
        CALL MDPTRI (DBLE(CFUX((.99*XCOP+.01*XCOQ))),
     +               DBLE(CFUY((.99*YCOP+.01*YCOQ))),RLAT,RLON)
        IF (RLAT.EQ.1.D12) RETURN
C
C Adjust the longitudes to make sure that the labels go the proper
C way around the earth.
C
        IF (ABS(PLON-RLON).GT.180.) RLON=RLON+SIGN(360.D0,PLON-RLON)
        IF (ABS(PLON-QLON).LT.1D-6.OR.
     +    SIGN(1.D0,PLON-QLON).NE.SIGN(1.D0,PLON-RLON)) QLON=QLON+360.D0
C
C Compute a "nice" longitude interval to use.
C
        CALL MDGNID (ABS(QLON-PLON)/4.D0,DNCE,NOFD)
        IF (DNCE.EQ.0.D0) RETURN
C
C Compute limits to be used in the labeling loop.
C
        IPLN=INT((PLON+180.D0)/DNCE)
        IQLN=INT((QLON+180.D0)/DNCE)
C
C Loop to generate the labels.
C
        DO 101 I=MIN(IPLN,IQLN)-1,MAX(IPLN,IQLN)+1
          RLON=DBLE(I)*DNCE-180.D0
          IF (RLON.GT.MIN(PLON,QLON)-DNCE/100.D0.AND.
     +        RLON.LT.MAX(PLON,QLON)+DNCE/100.D0) THEN
            CALL MDILON (PLAT,PLON,DBLE(CFUX(XCOP)),DBLE(CFUY(YCOP)),
     +                   QLAT,QLON,DBLE(CFUX(XCOQ)),DBLE(CFUY(YCOQ)),
     +                   RLON,UCOR,VCOR)
            CALL MDLOCD (RLON,CHRS,NCHR,NOFD)
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
