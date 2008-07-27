C
C $Id: pj18sp.f,v 1.5 2008-07-27 00:17:11 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ18SP (COORD,CRDIO,INDIC)
C
C -- M I L L E R   C Y L I N D R I C A L
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC18SP/ A,LON0,X0,Y0
      DATA FORTPI /0.78539816339744833E0/
      DATA ZERO,ONEQ,TWOH /0.0E0,1.25E0,2.5E0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         LON = ADJLSP (GEOG(1) - LON0)
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
         IERR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         GEOG(1) = ADJLSP (LON0 + X / A)
         GEOG(2) = TWOH * ATAN (EXP (Y / A / ONEQ)) - FORTPI * TWOH
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
