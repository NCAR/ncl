C
C $Id: phi1dp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION PHI1DP (ECCENT,QS)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-1).
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER II,NIT
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      DATA HALF,ONE /0.5D0,1.0D0/
      DATA EPSLN,TOL,NIT /1.0D-7,1.0D-10,15/
C
      PHI1DP = ASINDP (HALF * QS)
      IF (ECCENT .LT. EPSLN) RETURN
C
      ECCNTS = ECCENT * ECCENT
      PHI = PHI1DP
      DO 020 II = 1,NIT
      SINPI = SIN (PHI)
      COSPI = COS (PHI)
      CON = ECCENT * SINPI
      COM = ONE - CON * CON
      DPHI = HALF * COM * COM / COSPI * (QS / (ONE - ECCNTS) -
     .       SINPI / COM + HALF / ECCENT * LOG ((ONE - CON) /
     .       (ONE + CON)))
      PHI = PHI + DPHI
      IF (ABS(DPHI) .GT. TOL) GO TO 020
      PHI1DP = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ECCENT,QS
 2000 FORMAT (/' ERROR PHI1DP' /
     .         ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .         ' ECCENTRICITY =',E25.16,'   QS =',E25.16)
      IERR = 001
      RETURN
C
      END
