C
C $Id: phi4sp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PHI4SP (ECCNTS,E0,E1,E2,E3,A,B,C,PHI)
C
C SUBROUTINE TO COMPUTE LATITUDE ANGLE (PHI-4).
C
      IMPLICIT REAL (A-Z)
      INTEGER II,NIT
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      DATA ONE,TWO,FOUR,SIX /1.0E0,2.0E0,4.0E0,6.0E0/
      DATA TOL,NIT /1.0E-10,15/
C
      PHI = A
      DO 020 II = 1,NIT
      SINPHI = SIN (PHI)
      TANPHI = TAN (PHI)
      C = TANPHI * SQRT (ONE - ECCNTS * SINPHI * SINPHI)
      SIN2PH = SIN (TWO * PHI)
      ML = E0 * PHI - E1 * SIN2PH + E2 * SIN (FOUR * PHI)
     .      - E3 * SIN (SIX * PHI)
      MLP = E0 - TWO * E1 * COS (TWO * PHI) + FOUR * E2 *
     .      COS (FOUR * PHI) - SIX * E3 * COS (SIX * PHI)
      CON1 = TWO * ML + C * (ML * ML + B) - TWO * A *
     .       (C * ML + ONE)
      CON2 = ECCNTS * SIN2PH * (ML * ML + B - TWO * A * ML) / (TWO * C)
      CON3 = TWO * (A - ML) * (C * MLP - TWO / SIN2PH) - TWO * MLP
      DPHI = CON1 / (CON2 + CON3)
      PHI = PHI + DPHI
      IF (ABS(DPHI) .GT. TOL) GO TO 020
      RETURN
  020 CONTINUE
C
      IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) NIT,E0,E1,E2,E3,A,B,C,
     . ECCNTS
 2000 FORMAT (/' ERROR PHI4SP' /
     .         ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS'/
     .         ' E0 =',E25.16,'   E1 =',E25.16/
     .         ' E2 =',E25.16,'   E3 =',E25.16/
     .         ' A  =',E25.16,'   B  =',E25.16/
     .         ' C  =',E25.16/
     .         ' ECCENTRICITY SQUARE =',E25.16)
      IERR = 004
      RETURN
C
      END
