C
C $Id: pj01sp.f,v 1.1 1999-04-19 22:10:09 kennison Exp $
C
      SUBROUTINE PJ01SP (COORD,CRDIO,INDIC)
C
C -- U T M
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC, FWD, INV
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      PARAMETER (FWD=0, INV=1)
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         CALL PJ09SP (GEOG,PROJ,FWD)
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
         CALL PJ09SP (PROJ,GEOG,INV)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
