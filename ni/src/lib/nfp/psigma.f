      SUBROUTINE DPSIGMA(SIGMA,PSFC,MLON,NLAT,KLEV,PSIG)
      IMPLICIT NONE

c NCL: psig = pres_sigma (sigma,psfc)

c this routine interploates sigma to pressure coordinates.
c .   analogous to pres_hybrid_ccm

c     formula for the pressure of a sigma surface is;
c          psig(k,lat,lon) = sigma(k)*psfc(lat,lon)
c          psig(time,k,lat,lon) = sigma(k)*psfc(time,lat,lon)

c     input
c          sigma  - is the sigma coeficient
c          psfc   - is the surface pressure in pascals
c          mlon   - longitude dimension
c          nlat   - latitude  dimension
c          klev   - number of levels
c     output
c          psig   - pressure at sigma levels
c                                               INPUT
      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION SIGMA(KLEV),PSFC(MLON,NLAT)
c                                               OUTPUT
      DOUBLE PRECISION PSIG(MLON,NLAT,KLEV)

      INTEGER KL,NL,ML

      DO KL = 1,KLEV
          DO NL = 1,NLAT
              DO ML = 1,MLON
                  PSIG(ML,NL,KL) = SIGMA(KL)*PSFC(ML,NL)
              END DO
          END DO
      END DO

      RETURN
      END
