      SUBROUTINE DPRESHYBRID(P0,PS,HYA,HYB,KLVL,PHY)
      IMPLICIT NONE

c NCL: phy  = presHybrid (ps,p0,hya,hyb)
c      klvl = dimsizes(hya)
c                              input
      INTEGER KLVL
      DOUBLE PRECISION P0,PS,HYA(KLVL),HYB(KLVL)
c                              output
      DOUBLE PRECISION PHY(KLVL)
c                              local
      INTEGER K

      DO K = 1,KLVL
          PHY(K) = P0*HYA(K) + HYB(K)*PS
      END DO

      RETURN
      END
