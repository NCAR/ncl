C
C $Id: pj11dp.f,v 1.5 2008-07-27 00:17:11 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ11DP (COORD,CRDIO,INDIC)
C
C -- L A M B E R T   A Z I M U T H A L   E Q U A L - A R E A
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC11DP/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         LON = ADJLDP (GEOG(1) - LON0)
         SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         COSLON = COS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         IF (G .NE. -ONE) GO TO 140
         CON = TWO * A
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020) CON
 2020    FORMAT (' POINT PROJECTS INTO A CIRCLE OF RADIUS =',F12.2,
     .           ' METERS')
         IERR = 113
         RETURN
  140    KSP = SQRT (TWO / (ONE + G))
         PROJ(1) = X0 + A * KSP * COSPHI * SIN (LON)
         PROJ(2) = Y0 + A * KSP * (COSPH0 * SINPHI - SINPH0 * COSPHI *
     .             COSLON)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C -- I N V E R S E   . . .
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = SQRT (X * X + Y * Y)
         CON = RH / (TWO * A)
         IF (CON .LE. ONE) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT (/' ERROR PJ11DP'/
     .            ' INPUT DATA ERROR')
         IERR = 115
         RETURN
  230    Z = TWO * ASINDP (CON)
         SINZ = SIN (Z)
         COSZ = COS (Z)
         GEOG(1) = LON0
         IF (ABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINDP (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = ABS (LAT0) - HALFPI
         IF (ABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLDP (LON0 + ATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLDP (LON0 - ATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * SIN (GEOG(2))
         IF (CON .EQ. ZERO) RETURN
         GEOG(1) = ADJLDP (LON0 + ATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
