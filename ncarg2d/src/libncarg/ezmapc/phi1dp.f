C
C $Id: phi1dp.f,v 1.3 2000-08-22 15:04:15 haley Exp $
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
