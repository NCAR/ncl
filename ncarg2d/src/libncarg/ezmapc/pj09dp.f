C
C $Id: pj09dp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ09DP (COORD,CRDIO,INDIC)
C
C -- T R A N S V E R S E   M E R C A T O R
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER I,IND,NIT
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS ** A,E,ES,KS0,LON0,LAT0,X0,Y0,E0,E1,E2,E3,ESP,ML0,IND
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC09DP/ A,LON0,X0,Y0,ES,ESP,E0,E1,E2,E3,KS0,LAT0,ML0,IND
      DATA ZERO,HALF,ONE,TWO,THREE /0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
      DATA FOUR,FIVE,SIX,EIGHT,NINE /4.0D0,5.0D0,6.0D0,8.0D0,9.0D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA TEN /10.0D0/
      DATA EPSLN,NIT /1.0D-10,6/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         DLON = ADJLDP (GEOG(1) - LON0)
         LAT = GEOG(2)
         IF (IND .EQ. 0) GO TO 240
         COSPHI = COS (LAT)
         B = COSPHI * SIN (DLON)
         IF (ABS(ABS(B) - ONE) .GT. EPSLN) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (/' ERROR PJ09DP'/
     .            ' POINT PROJECTS INTO INFINITY')
         IERR = 093
         RETURN
  230    PROJ(1) = HALF * A * KS0 * LOG ((ONE + B) / (ONE - B)) + X0
         CON=ACOS(MAX(-ONE,MIN(ONE,COSPHI*COS(DLON)/
     +                                        SQRT(MAX(ZERO,ONE-B*B)))))
         IF (LAT .LT. ZERO) CON =-CON
         PROJ(2) = A * KS0 * (CON - LAT0) + Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
C
  240    SINPHI = SIN (LAT)
         COSPHI = COS (LAT)
         AL = COSPHI * DLON
         ALS = AL * AL
         C = ESP * COSPHI * COSPHI
         TQ = TAN (LAT)
         T = TQ * TQ
         N = A / SQRT (ONE - ES * SINPHI * SINPHI)
         ML = A * MLFNDP (E0,E1,E2,E3,LAT)
         PROJ(1) = KS0 * N * AL * (ONE + ALS / SIX * (ONE - T + C +
     .             ALS / 20.0D0 * (FIVE - 18.0D0 * T + T * T + 72.0D0 *
     .             C - 58.0D0 * ESP))) + X0
         PROJ(2) = KS0 *(ML - ML0 + N * TQ *(ALS *(HALF + ALS / 24.0D0 *
     .             (FIVE - T + NINE * C + FOUR * C * C + ALS / 30.0D0 *
     .             (61.0D0 - 58.0D0 * T + T * T + 600.0D0 * C -
     .             330.0D0 * ESP))))) + Y0
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
         IF (IND .EQ. 0) GO TO 340
         F = EXP (X / (A * KS0))
         G = HALF * (F - ONE / F)
         TEMP = LAT0 + Y / (A * KS0)
         H = COS (TEMP)
         CON = SQRT ((ONE - H * H) / (ONE + G * G))
         GEOG(2) = ASINDP (CON)
         IF (TEMP .LT. ZERO) GEOG(2) =-GEOG(2)
         IF (G.NE.ZERO .OR. H.NE.ZERO) GO TO 330
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  330    GEOG(1) = ADJLDP (ATAN2 (G,H) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
C
  340    CON = (ML0 + Y / KS0) / A
         PHI = CON
         DO 360 I = 1,NIT
         DPHI = ((CON + E1 * SIN (TWO * PHI) - E2 * SIN (FOUR * PHI)
     .          + E3 * SIN (SIX * PHI)) / E0) - PHI
         PHI = PHI + DPHI
         IF (ABS(DPHI) .LE. EPSLN) GO TO 380
  360    CONTINUE
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030) NIT
 2030    FORMAT (/' ERROR PJ09DP' /
     .            ' LATITUDE FAILED TO CONVERGE AFTER',I3,' ITERATIONS')
         IERR = 095
         RETURN
  380    IF (ABS(PHI) .LT. HALFPI) GO TO 400
         GEOG(2) = SIGN (HALFPI , Y)
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  400    SINPHI = SIN (PHI)
         COSPHI = COS (PHI)
         TANPHI = TAN (PHI)
         C = ESP * COSPHI * COSPHI
         CS = C * C
         T = TANPHI * TANPHI
         TS = T * T
         CON = ONE - ES * SINPHI * SINPHI
         N = A / SQRT (CON)
         R = N * (ONE - ES) / CON
         D = X / (N * KS0)
         DS = D * D
         GEOG(2) = PHI - (N * TANPHI * DS / R) * (HALF - DS / 24.0D0 *
     .             (FIVE + THREE * T + TEN * C - FOUR * CS - NINE * ESP
     .             - DS / 30.0D0 * (61.0D0 + 90.0D0 * T + 298.0D0 * C +
     .             45.0D0 * TS - 252.0D0 * ESP - THREE * CS)))
         GEOG(1) = ADJLDP (LON0 + (D * (ONE - DS / SIX * (ONE + TWO *
     .             T + C - DS / 20.0D0 * (FIVE - TWO * C + 28.0D0 * T -
     .             THREE * CS + EIGHT * ESP + 24.0D0 * TS))) / COSPHI))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
