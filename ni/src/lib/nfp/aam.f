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
c .             These could be gaussian weights or cos(lat)*dlat
c .             where "dlat" is the latitudinal spacing in radians
c .   mlon    - number of longitudes
c .   nlat    - number of latitudes
c .   klvl    - number of levels
c .   umsg    - missing code for "u"
c .   aam     - atmospheric angular momentum [relative]
c .             units: kg*m2/s

c                                        LOCAL
      INTEGER ML,NL,KL
      DOUBLE PRECISION PI,TWOPI,RAD,RE,RE3,G,DLAT,UDP

      PI = 4.0D0*ATAN(1.0D0)
      RAD = PI/180.D0
      TWOPI = 2.0D0*PI

      RE = 6.37122D06
      RE3 = RE**3
      G = 9.81D0
      AAM = 0.0D0

      DO NL = 1,NLAT
c                                     at each latitude
c                                     sum u*dp over all longitudes
          UDP = 0.0D0
          DO ML = 1,MLON
              DO KL = 1,KLVL
                  IF (U(ML,NL,KL).NE.UMSG) THEN
                      UDP = UDP + U(ML,NL,KL)*DP(KL)
                  END IF
              END DO
          END DO
c                                     'zonal average' udp
          UDP = UDP/MLON
c                                     latitudinal contribution to aam
          AAM = AAM + UDP*COS(LAT(NL)*RAD)*WGT(NL)*TWOPI
      END DO

      AAM = (RE3/G)*AAM

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

c nomenclature: [same as above BUT
c .   dp      - pressure thickness   (Pa)
c.              a 3D array

c                                        LOCAL
      INTEGER ML,NL,KL
      DOUBLE PRECISION PI,TWOPI,RAD,RE,RE3,G,DLAT,UDP

      PI = 4.0D0*ATAN(1.0D0)
      RAD = PI/180.D0
      TWOPI = 2.0D0*PI

      RE = 6.37122D06
      RE3 = RE**3
      G = 9.81D0
      AAM = 0.0D0
c                                     latitude
      DO NL = 1,NLAT
c                                     at each latitude
c                                     sum u*dp over all longitudes
          UDP = 0.0D0
          DO ML = 1,MLON
              DO KL = 1,KLVL
                  IF (U(ML,NL,KL).NE.UMSG) THEN
                      UDP = UDP + U(ML,NL,KL)*DP(ML,NL,KL)
                  END IF
              END DO
          END DO
c                                     'zonal average' udp
          UDP = UDP/MLON
c                                     point contribution to aam
          AAM = AAM + UDP*COS(LAT(NL)*RAD)*WGT(NL)*TWOPI
      END DO

      AAM = (RE3/G)*AAM

      RETURN
      END
c -----------------------------------------------------------
c if a scalar "wgt" is passed from NCL to the angmom_atm
c .   function, it means that the interface will calculate
c .   the weights. This assumes constant latitudinal spacing.
c .                cos(theta)*dlambda
c -----------------------------------------------------------
      SUBROUTINE DCOSWEIGHT(LAT,NLAT,WGT)
      IMPLICIT NONE
c                                     INPUT
      INTEGER NLAT
      DOUBLE PRECISION LAT(NLAT)
c                                     OUTPUT
      DOUBLE PRECISION WGT(NLAT)
c                                     LOCAL
      DOUBLE PRECISION RAD,DLATR
      INTEGER NL

      RAD = 4.0D0*ATAN(1.0D0)/180.D0
c                                     latitudinal spacing (radians)
      DLATR = ABS(LAT(2)-LAT(1))*RAD
c                                     assume constant lat spacing
c                                     want exact zero at poles
      DO NL = 1,NLAT
          IF (ABS(LAT(NL)).EQ.90.D0) THEN
              WGT(NL) = 0.0D0
          ELSE
              WGT(NL) = COS(LAT(NL)*RAD)*DLATR
          END IF
      END DO

      RETURN
      END
