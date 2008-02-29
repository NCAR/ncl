C NCLFORTSTART
      SUBROUTINE DPRCWATDP(Q,DP,KLVL,QMSG,DPMSG,PRCWAT)
      IMPLICIT NONE
c Input
      INTEGER KLVL
      DOUBLE PRECISION Q(KLVL),DP(KLVL),QMSG,DPMSG
c Output
      DOUBLE PRECISION PRCWAT
C NCLEND

C NCL: prcwat = prcwater_dp (q,dp)

c nomenclature:
c     q      - specific humidity [kg/kg; ie: dimensionless]
c     dp     - layer thickness [Pa]
c     prcwat - precipitable water [kg/m2]

c Local
      INTEGER KL,KNT
      DOUBLE PRECISION G

      G = 9.81D0

      KNT = 0
      PRCWAT = 0.0D0
      DO KL = 1,KLVL
          IF (Q(KL).NE.QMSG .AND. DP(KL).NE.DPMSG) THEN
              KNT = KNT + 1
              PRCWAT = PRCWAT + Q(KL)*ABS(DP(KL))
          END IF
      END DO

      IF (KNT.GT.0) THEN
          PRCWAT = PRCWAT/G
      ELSE
          PRCWAT = QMSG
      END IF

      RETURN
      END
