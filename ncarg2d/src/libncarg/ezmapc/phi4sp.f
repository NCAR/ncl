C
C $Id: phi4sp.f,v 1.2 2000-07-12 16:23:53 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
