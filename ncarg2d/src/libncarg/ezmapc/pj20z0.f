C
C $Id: pj20z0.f,v 1.1 1999-04-02 23:06:02 kennison Exp $
C
      SUBROUTINE PJ20Z0 (COORD,CRDIO,INDIC)
C
C -- O B L I Q U E   M E R C A T O R   ( H O T I N E )
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,KS0,ALPHA,LONC,LON1,LAT1,LON2,LAT2,LAT0 **
C ********************** X0,Y0,GAMMA,LON0,AL,BL,EL *********************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ20/ LON0,X0,Y0,AL,BL,COSALF,COSGAM,E,EL,SINALF,SINGAM,U0
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA TOL,EPSLN /1.0D-7,1.0D-10/
      DATA ZERO,HALF,ONE /0.0D0,0.5D0,1.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         SINPHI = SIN (GEOG(2))
         DLON = ADJLZ0 (GEOG(1) - LON0)
         VL = SIN (BL * DLON)
         IF (ABS(ABS(GEOG(2)) - HALFPI) .GT. EPSLN) GO TO 230
         UL = SINGAM * SIGN (ONE , GEOG(2))
         US = AL * GEOG(2) / BL
         GO TO 250
  230    TS = TSFNZ0 (E,GEOG(2),SINPHI)
         Q = EL / TS ** BL
         S = HALF * (Q - ONE / Q)
         T = HALF * (Q + ONE / Q)
         UL = (S * SINGAM - VL * COSGAM) / T
         CON = COS (BL * DLON)
         IF (ABS(CON) .LT. TOL) GO TO 240
         US = AL * ATAN ((S * COSGAM + VL * SINGAM) / CON) / BL
         IF (CON .LT. ZERO) US = US + PI * AL / BL
         GO TO 250
  240    US = AL * BL * DLON
         IERROR = 204
         RETURN
  250    IF (ABS(ABS(UL) - ONE) .GT. EPSLN) GO TO 255
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2060)
 2060    FORMAT (/' ERROR PJ20Z0'/
     .            ' POINT PROJECTS INTO INFINITY')
         IERROR = 205
         RETURN
  255    VS = HALF * AL * LOG ((ONE - UL) / (ONE + UL)) / BL
         US = US - U0
         PROJ(1) = X0 + VS * COSALF + US * SINALF
         PROJ(2) = Y0 + US * COSALF - VS * SINALF
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
         IERROR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         VS = X * COSALF - Y * SINALF
         US = Y * COSALF + X * SINALF
         US = US + U0
         Q = EXP (- BL * VS / AL)
         S = HALF * (Q - ONE / Q)
         T = HALF * (Q + ONE / Q)
         VL = SIN (BL * US / AL)
         UL = (VL * COSGAM + S * SINGAM) / T
         IF (ABS (ABS (UL) - ONE) .GE. EPSLN) GO TO 300
         GEOG(1) = LON0
         GEOG(2) = SIGN (HALFPI , UL)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  300    CON = ONE / BL
         TS = (EL / SQRT ((ONE + UL) / (ONE - UL))) ** CON
         GEOG(2) = PHI2Z0 (E,TS)
         CON = COS (BL * US / AL)
         LON = LON0 - ATAN2 ((S * COSGAM - VL * SINGAM) , CON) / BL
         GEOG(1) = ADJLZ0 (LON)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
