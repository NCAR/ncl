C
C $Id: phi3dp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION PHI3DP (ML,E0,E1,E2,E3)
C
C FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-3).
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER II,NIT
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      DATA TWO,FOUR,SIX /2.0D0,4.0D0,6.0D0/
      DATA TOL,NIT /1.0D-10,15/
C
      PHI = ML
      DO 020 II = 1,NIT
      DPHI = (ML + E1 * SIN (TWO * PHI) - E2 * SIN (FOUR * PHI)
     .       + E3 * SIN (SIX * PHI)) / E0 - PHI
      PHI = PHI + DPHI
      IF (ABS(DPHI) .GT. TOL) GO TO 020
      PHI3DP = PHI
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,ML,E0,E1,E2,E3
 2000 FORMAT (/' ERROR PHI3DP' /
     .         ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .         ' ML =',E25.16,'   E0 =',E25.16/
     .         ' E1 =',E25.16,'   E2 =',E25.16,'   E3=',E25.16)
      IERR = 003
      RETURN
C
      END
