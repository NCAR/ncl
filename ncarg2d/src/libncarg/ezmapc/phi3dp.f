C
C $Id: phi3dp.f,v 1.2 2000-07-12 16:23:53 haley Exp $
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
