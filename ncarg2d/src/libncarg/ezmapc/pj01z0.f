C
C $Id: pj01z0.f,v 1.1 1999-04-02 23:05:55 kennison Exp $
C
      SUBROUTINE PJ01Z0 (COORD,CRDIO,INDIC)
C
C -- U T M
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC, FWD, INV
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      PARAMETER (FWD=0, INV=1)
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         CALL PJ09Z0 (GEOG,PROJ,FWD)
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
         CALL PJ09Z0 (PROJ,GEOG,INV)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
