C
C $Id: pj04z0.f,v 1.1 1999-04-02 23:05:56 kennison Exp $
C
      SUBROUTINE PJ04Z0 (COORD,CRDIO,INDIC)
C
C -- L A M B E R T   C O N F O R M A L   C O N I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,F,RH0 *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ04/ A,LON0,X0,Y0,E,F,NS,RH0
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
         CON = ABS (ABS (GEOG(2)) - HALFPI)
         IF (CON .GT. EPSLN) GO TO 220
         CON = GEOG(2) * NS
         IF (CON .GT. ZERO) GO TO 210
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT (/' ERROR PJ04Z0'/
     .            ' POINT CANNOT BE PROJECTED')
         IERROR = 044
         RETURN
  210    RH = ZERO
         GO TO 230
  220    SINPHI = SIN (GEOG(2))
         TS = TSFNZ0 (E,GEOG(2),SINPHI)
         RH = A * F * TS ** NS
  230    THETA = NS * ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + RH * SIN (THETA)
         PROJ(2) = Y0 + RH0 - RH * COS (THETA)
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
         Y = RH0 - PROJ(2) + Y0
         RH = SIGN (SQRT (X*X + Y*Y) , NS)
         THETA = ZERO
         CON = SIGN (ONE , NS)
         IF (RH .NE. ZERO) THETA = ATAN2 (CON * X , CON * Y)
         IF (RH.NE.ZERO .OR. NS.GT.ZERO) GO TO 250
         GEOG(2) = - HALFPI
         GO TO 260
  250    CON = ONE / NS
         TS = (RH / (A * F)) ** CON
         GEOG(2) = PHI2Z0 (E,TS)
         IF (IERROR .EQ. 0) GO TO 260
         IERROR = 046
         RETURN
  260    GEOG(1) = ADJLZ0 (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
