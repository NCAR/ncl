      SUBROUTINE PRESHYBRID(P0,PS,HYA,HYB,KLVL,PHY)
      IMPLICIT NONE

c NCL: phy  = pres_hybrid (ps,p0,hya,hyb)
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

      SUBROUTINE DPRESHYBRID(P0,PS,HYAI,HYBI,KLVL,DPHY)
      IMPLICIT NONE

c NCL: dphy  = dpres_hybrid (ps,p0,hyai,hybi)
c      klvl  = dimsizes(hyai)

c this routine interploates ccm2/3 hybrid coordinate data
c     to pressure coordinates. These are ordered from top-to-bottom.

c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input
c          hyai   - is the "a" or pressure hybrid coef
c          hybi   - is the "b" or sigma coeficient
c          p0     - is the base pressure in pascals [Pa]
c          psfc   - is the surface pressure in pascals [Pa]
c     output
c          dphy    - delta pressure [Pa]
c                              INPUT
      INTEGER KLVL
      DOUBLE PRECISION P0,PS,HYAI(KLVL),HYBI(KLVL)
c                              OUTPUT
      DOUBLE PRECISION DPHY(KLVL-1)
c                              LOCAL
      INTEGER K

      DO K = 1,KLVL - 1
          DPHY(K) = ABS(P0* (HYAI(K+1)-HYAI(K))+ (HYAI(K+1)-HYBI(K))*PS)
      END DO

      RETURN
      END
