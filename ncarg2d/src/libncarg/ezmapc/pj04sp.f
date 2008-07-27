C
C $Id: pj04sp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ04SP (COORD,CRDIO,INDIC)
C
C -- L A M B E R T   C O N F O R M A L   C O N I C
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,F,RH0 *******
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC04SP/ A,LON0,X0,Y0,E,F,NS,RH0
      DATA HALFPI /1.5707963267948966E0/
      DATA EPSLN /1.0E-10/
      DATA ZERO,ONE /0.0E0,1.0E0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         CON = ABS (ABS (GEOG(2)) - HALFPI)
         IF (CON .GT. EPSLN) GO TO 220
         CON = GEOG(2) * NS
         IF (CON .GT. ZERO) GO TO 210
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2030)
 2030    FORMAT (/' ERROR PJ04SP'/
     .            ' POINT CANNOT BE PROJECTED')
         IERR = 044
         RETURN
  210    RH = ZERO
         GO TO 230
  220    SINPHI = SIN (GEOG(2))
         TS = TSFNSP (E,GEOG(2),SINPHI)
         RH = A * F * TS ** NS
  230    THETA = NS * ADJLSP (GEOG(1) - LON0)
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
         IERR = 0
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
         GEOG(2) = PHI2SP (E,TS)
         IF (IERR .EQ. 0) GO TO 260
         IERR = 046
         RETURN
  260    GEOG(1) = ADJLSP (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
