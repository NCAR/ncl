C NCLFORTSTART
      SUBROUTINE DAAMOM1(U,DP,LAT,WGT,MLON,NLAT,KLVL,UMSG,AAM)
      IMPLICIT NONE

c NCL:   aam = angmom_atm (u,dp,lat,wgt)

c                                        INPUT
      INTEGER MLON,NLAT,KLVL
      DOUBLE PRECISION U(MLON,NLAT,KLVL),LAT(NLAT),WGT(NLAT),UMSG,
     +                 DP(KLVL)
c                                        OUTPUT
      DOUBLE PRECISION AAM
C NCLEND

c Compute the atmosphere's relative angular momentum

c nomenclature:
c .   u       - zonal wind component (m/s)
c .   dp      - pressure thickness   (Pa)
c.              a 1D array
c .   lat     - latitudes
c .   wgt     - wgts associated with each latitude
c .             These could be gaussian weights or cos(lat) or
c .             set them to 1.0 for no weighting
c .   mlon    - number of longitudes
c .   nlat    - number of latitudes
c .   klvl    - number of levels
c .   umsg    - missing code for "u"
c .   aam     - atmospheric angular momentum [relative]

c                                        LOCAL
      INTEGER ML,NL,KL
      DOUBLE PRECISION PI,TWOPI,RAD,RE,RE3,G,CLAT,DLAT,SWT,UDP,WRK(NLAT)

      PI = 4.0D0*ATAN(1.0D0)
      RAD = PI/180.D0
      TWOPI = 2.0D0*PI

      RE = 6.37122D06
      RE3 = RE**3
      G = 9.81D0

c                                     latitude
      DO NL = 1,NLAT
          UDP = 0.0D0
          SWT = 0.0D0
          WRK(NL) = 0.0D0
c                                     vertical slice (lon,lev)
          DO ML = 1,MLON
              DO KL = 1,KLVL
                  IF (U(ML,NL,KL).NE.UMSG) THEN
                      UDP = UDP + U(ML,NL,KL)*DP(KL)
                  END IF
              END DO
          END DO
c                                     slice contribution to aam
c                                     all longitudes: 0-to-360 = twopi
          CLAT = COS(LAT(NL)*RAD)
          WRK(NL) = UDP*CLAT*CLAT*TWOPI*WGT(NL)
          SWT = SWT + WGT(NL)
      END DO
c                                     calcualate the wgted aam
      AAM = 0.0D0
      DO NL = 1,NLAT
          AAM = AAM + WRK(NL)/SWT
      END DO
c                                     dlat = lat span in radians
c                                     if lat(nlat)=90 and lat(1)=-90
c                                        then dlat=pi
      DLAT = ABS(LAT(NLAT)-LAT(1))*RAD
      AAM = (RE3/G)*AAM*DLAT

      RETURN
      END
c ---------------------------------------------------------
c The ONLY difference between the aamom1 and aamom3 is that
c in the the former "dp" is 1D while in the latter it is 3D
c ---------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DAAMOM3(U,DP,LAT,WGT,MLON,NLAT,KLVL,UMSG,AAM)
      IMPLICIT NONE

c NCL:   aam = angmom_atm (u,dp,lat,wgt)

c                                        INPUT
      INTEGER MLON,NLAT,KLVL
      DOUBLE PRECISION U(MLON,NLAT,KLVL),LAT(NLAT),WGT(NLAT),UMSG,
     +                 DP(MLON,NLAT,KLVL)
c                                        OUTPUT
      DOUBLE PRECISION AAM
C NCLEND
c                                        LOCAL
      INTEGER ML,NL,KL
      DOUBLE PRECISION PI,TWOPI,RAD,RE,RE3,G,CLAT,DLAT,SWT,UDP,WRK(NLAT)

      PI = 4.0D0*ATAN(1.0D0)
      RAD = PI/180.D0
      TWOPI = 2.0D0*PI

      RE = 6.37122D06
      RE3 = RE**3
      G = 9.81D0

c                                     latitude
      DO NL = 1,NLAT
          UDP = 0.0D0
          SWT = 0.0D0
          WRK(NL) = 0.0D0

c                                     vertical slice (lon,lev)
          DO ML = 1,MLON
              DO KL = 1,KLVL
                  IF (U(ML,NL,KL).NE.UMSG) THEN
                      UDP = UDP + U(ML,NL,KL)*DP(ML,NL,KL)
                  END IF
              END DO
          END DO
c                                     slice contribution to aam
c                                     all longitudes: 0-to-360 = twopi
          CLAT = COS(LAT(NL)*RAD)
          WRK(NL) = UDP*CLAT*CLAT*TWOPI*WGT(NL)
          SWT = SWT + WGT(NL)
      END DO
c                                     calcualate the wgted aam
      AAM = 0.0D0
      DO NL = 1,NLAT
          AAM = AAM + WRK(NL)/SWT
      END DO
c                                     dlat = lat span in radians
c                                     if lat(nlat)=90 and lat(1)=-90
c                                        then dlat=pi
      DLAT = ABS(LAT(NLAT)-LAT(1))*RAD
      AAM = (RE3/G)*AAM*DLAT

      RETURN
      END
