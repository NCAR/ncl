C
C $Id: pj18z0.f,v 1.1 1999-04-02 23:06:02 kennison Exp $
C
      SUBROUTINE PJ18Z0 (COORD,CRDIO,INDIC)
C
C -- M I L L E R   C Y L I N D R I C A L
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ18/ A,LON0,X0,Y0
      DATA FORTPI /0.78539816339744833D0/
      DATA ZERO,ONEQ,TWOH /0.0D0,1.25D0,2.5D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         LON = ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON
         PROJ(2) = Y0 + A * LOG (TAN (FORTPI + GEOG(2) / TWOH)) * ONEQ
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
         GEOG(1) = ADJLZ0 (LON0 + X / A)
         GEOG(2) = TWOH * ATAN (EXP (Y / A / ONEQ)) - FORTPI * TWOH
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
