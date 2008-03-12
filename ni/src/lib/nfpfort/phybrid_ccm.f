      SUBROUTINE DPHYBRID(P0,HYA,HYB,PSFC,MLON,NLAT,KLEV,PHY,PMSG)

c NCL: phy = pres_hybrid_ccm (psfc,p0,hya,hyb)

c this routine interploates ccm2/3 hybrid coordinate data
c     to pressure coordinates.

c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input
c          hya    - is the "a" or pressure hybrid coef
c          hyb    - is the "b" or sigma coeficient
c          p0     - is the base pressure in pascals [Pa]
c          psfc   - is the surface pressure in pascals [Pa]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klev   - number of levels
c     output
c          phy    - pressure at hybrid levels [Pa]

      IMPLICIT NONE
      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION P0,HYA(KLEV),HYB(KLEV),PSFC(MLON,NLAT),PMSG

      DOUBLE PRECISION PHY(MLON,NLAT,KLEV)

      INTEGER KL,NL,ML
c f77
      DO KL = 1,KLEV
          DO NL = 1,NLAT
              DO ML = 1,MLON
                 IF (PSFC(ML,NL).NE.PMSG) then
                     PHY(ML,NL,KL) = HYA(KL)*P0 + HYB(KL)*PSFC(ML,NL)
                 ELSE
                     PHY(ML,NL,KL) = PMSG
                 END IF
              END DO
          END DO
      END DO

c f90 do kl=1,klev
c f90    phy(:,:,kl) = hya(kl)*p0 + hyb(kl)*psfc(:,:)
c f90 end do

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DDPHYBRID(P0,HYA,HYB,PSFC,MLON,NLAT,KLEV,DPHY,PMSG)
      IMPLICIT NONE

c input
      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION P0,HYA(KLEV),HYB(KLEV),PSFC(MLON,NLAT),PMSG
c output
      DOUBLE PRECISION DPHY(MLON,NLAT,KLEV-1)
C NCLEND

c NCL: dphy = dpres_hybrid_ccm (ps,p0,hyai,hybi)
c Note: dphy will have one less vertical level

c this routine interploates ccm2/3 hybrid coordinate data
c     to pressure coordinates. then it computes

c     formula for the pressure of a hybrid surface is;
c          phy(k) = hya(k)*p0 + hyb(k)*psfc

c     input
c          hya    - is the "a" or pressure hybrid coef
c          hyb    - is the "b" or sigma coeficient
c          p0     - is the base pressure in pascals [Pa]
c          psfc   - is the surface pressure in pascals [Pa]
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klev   - number of levels
c     output
c          dphy    - delta pressure [Pa; always positive]

c local
      INTEGER KL,NL,ML
      DOUBLE PRECISION PA,PB

      DO NL = 1,NLAT
          DO ML = 1,MLON
              DO KL = 1,KLEV - 1
                 IF (PSFC(ML,NL).NE.PMSG) then
                     PA = P0*HYA(KL)   + HYB(KL)*PSFC(ML,NL)
                     PB = P0*HYA(KL+1) + HYB(KL+1)*PSFC(ML,NL)
                     DPHY(ML,NL,KL) = ABS(PB-PA)
c c c       dphy(ml,nl,kl) = abs((hya(kl+1)-hya(kl))*p0
c c c*                     -     (hyb(kl+1)-hya(kl))*psfc(ml,nl))
                 ELSE
                     DPHY(ML,NL,KL) = PMSG
                 END IF
              END DO
          END DO
      END DO

      RETURN
      END
