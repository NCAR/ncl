C
C $Id: phi2dp.f,v 1.4 2001-08-16 23:12:51 kennison Exp $
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
      DOUBLE PRECISION FUNCTION PHI2DP (ECCENT,TS)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-2).
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER II,NIT
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      DATA HALF,ONE,TWO /0.5D0,1.0D0,2.0D0/
      DATA TOL,NIT /1.0D-10,15/
      DATA HALFPI /1.5707963267948966D0/
C
      ECCNTH = HALF * ECCENT
      PHI = HALFPI - TWO * ATAN (TS)
      DO 020 II = 1,NIT
      SINPI = SIN (PHI)
      CON = ECCENT * SINPI
      DPHI = HALFPI - TWO * ATAN (TS * ((ONE - CON) /
     .       (ONE + CON)) ** ECCNTH) - PHI
      PHI = PHI + DPHI
      IF (ABS(DPHI) .GT. TOL) GO TO 020
      PHI2DP = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ECCENT,TS
 2000 FORMAT (/' ERROR PHI2DP' /
     .         ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .         ' ECCENTRICITY =',E25.16,'   TS =',E25.16)
      IERR = 002
      RETURN
C
      END
