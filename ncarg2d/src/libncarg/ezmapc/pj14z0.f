C
C $Id: pj14z0.f,v 1.1 1999-04-02 23:06:00 kennison Exp $
C
      SUBROUTINE PJ14Z0 (COORD,CRDIO,INDIC)
C
C -- O R T H O G R A P H I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *****************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ14/ A,LON0,X0,Y0,COSPH0,LAT0,SINPH0
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         LON = ADJLZ0 (GEOG(1) - LON0)
         SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         COSLON = COS (LON)
         G = SINPH0 * SINPHI + COSPH0 * COSPHI * COSLON
         KSP = ONE
         IF (G.GT.ZERO .OR. ABS(G).LE.EPSLN) GO TO 140
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (' POINT CANNOT BE PROJECTED')
         IERROR = 143
         RETURN
  140    PROJ(1) = X0 + A * KSP * COSPHI * SIN (LON)
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
         IERROR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         RH = SQRT (X * X + Y * Y)
         IF (RH .LE. A) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT (/' ERROR PJ14Z0'/
     .            ' INPUT DATA ERROR')
         IERROR = 145
         RETURN
  230    Z = ASINZ0 (RH / A)
         SINZ = SIN (Z)
         COSZ = COS (Z)
         GEOG(1) = LON0
         IF (ABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(2) = ASINZ0 (COSZ * SINPH0 + Y * SINZ * COSPH0 / RH)
         CON = ABS (LAT0) - HALFPI
         IF (ABS (CON) .GT. EPSLN) GO TO 260
         IF (LAT0 .LT. ZERO) GO TO 250
         GEOG(1) = ADJLZ0 (LON0 + ATAN2 (X , -Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  250    GEOG(1) = ADJLZ0 (LON0 - ATAN2 (-X , Y))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  260    CON = COSZ - SINPH0 * SIN (GEOG(2))
         IF (ABS(CON).LT.EPSLN.AND.ABS(X).LT.EPSLN) RETURN
         GEOG(1) = ADJLZ0 (LON0 + ATAN2 ((X*SINZ*COSPH0) , (CON*RH)))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
