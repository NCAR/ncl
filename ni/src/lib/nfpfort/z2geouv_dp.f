C NCLFORTSTART
      SUBROUTINE Z2GEOUV(Z,MLON,NLAT,ZMSG,GLON,GLAT,UG,VG,IOPT)
      IMPLICIT NONE

c NCL: uv = z2geouv(z,lat,lon,iopt)           ; uv(2,...,nlat,mlon)
c                                               INPUT
      INTEGER MLON,NLAT,IOPT
      DOUBLE PRECISION Z(MLON,NLAT),ZMSG,GLAT(NLAT),GLON(MLON)
c                                               OUTPUT
      DOUBLE PRECISION UG(MLON,NLAT),VG(MLON,NLAT)
C NCLEND

c Given geopotential height (gpm): calculate geostrophic wind components

c INPUT
c .   z     - geopotential height (south to north order)
c .   mlon  - number of longitudes
c .   nlat  - number of latitudes
c .   zmsg  - missing value code
c .   glat  - latitudes  of grid points
c .   glon  - longitudes of grid points
c .   iopt  - option: iopt=0 z is not cyclic in longitude
c .                   iopt=1 z is cyclic in longitude
c OUTPUT
c .   ug,vg - geostrophic wind components

      INTEGER NL,ML

      IF (GLAT(2).GT.GLAT(1)) THEN
c                                         south to north
          CALL Z2GUV(Z,MLON,NLAT,ZMSG,GLON,GLAT,UG,VG,IOPT)
      ELSE
c                                         north to south [reorder]
          CALL ZUVNEW(Z,MLON,NLAT,ZMSG,GLON,GLAT,UG,VG,IOPT)
      END IF

      RETURN
      END
C -------
      SUBROUTINE ZUVNEW(Z,MLON,NLAT,ZMSG,GLON,GLAT,UG,VG,IOPT)
      IMPLICIT NONE
c
c data array need to be rearranged to go fron south to north
c
c                                               INPUT
      INTEGER MLON,NLAT,IOPT
      DOUBLE PRECISION Z(MLON,NLAT),ZMSG,GLAT(NLAT),GLON(MLON)
c                                               OUTPUT
      DOUBLE PRECISION UG(MLON,NLAT),VG(MLON,NLAT)

c local temporary arrays.
      DOUBLE PRECISION ZTMP(MLON,NLAT),GLATTMP(NLAT),UTMP(NLAT),
     +                 VTMP(NLAT)
      INTEGER NL,ML,NLAT1

c reorder [ S ==> N ]
      NLAT1 = NLAT + 1
      DO NL = 1,NLAT
          GLATTMP(NLAT1-NL) = GLAT(NL)
          DO ML = 1,MLON
              ZTMP(ML,NLAT1-NL) = Z(ML,NL)
          END DO
      END DO

      CALL Z2GUV(ZTMP,MLON,NLAT,ZMSG,GLON,GLATTMP,UG,VG,IOPT)

c put in original order

      DO ML = 1,MLON
          DO NL = 1,NLAT
              UTMP(NLAT1-NL) = UG(ML,NL)
              VTMP(NLAT1-NL) = VG(ML,NL)
          END DO
          DO NL = 1,NLAT
              UG(ML,NL) = UTMP(NL)
              VG(ML,NL) = VTMP(NL)
          END DO
      END DO

      RETURN
      END
C -------
      SUBROUTINE Z2GUV(Z,MLON,NLAT,ZMSG,GLON,GLAT,UG,VG,IOPT)
      IMPLICIT NONE
c                                               INPUT
      INTEGER MLON,NLAT,IOPT
      DOUBLE PRECISION Z(MLON,NLAT),ZMSG,GLAT(NLAT),GLON(MLON)
c                                               OUTPUT
      DOUBLE PRECISION UG(MLON,NLAT),VG(MLON,NLAT)

c Given geopotential height (gpm): calculate geostrophic components

c INPUT
c .   z     - geopotential height (south to north order)
c .   mlon  - number of longitudes
c .   nlat  - number of latitudes
c .   zmsg  - missing value code
c .   glat  - latitudes  of grid points
c .   glon  - longitudes of grid points
c .   iopt  - option: iopt=0 z is not cyclic in longitude
c .                   iopt=1 z is cyclic in longitude
c OUTPUT
c .   ug,vg - geostrophic wind components

      INTEGER NL,ML
      DOUBLE PRECISION ZZ(0:MLON+1,0:NLAT+1)
      DOUBLE PRECISION GF(0:NLAT+1),DX(0:NLAT+1),DY(0:NLAT+1)
      DOUBLE PRECISION GR,RAD,RE,RR,OMEGA,DLON,DLON2

c  calculate `constant' quantities

      GR = 9.80616D0
      RAD = 4.D0*ATAN(1.0D0)/180.D0
      RE = 6371220.D0
      RR = RE*RAD
      DLON = RR* (GLON(2)-GLON(1))
      DLON2 = 2.D0*DLON
      OMEGA = 7.292D-05

c  calculate the quantity [gravity/coriolis_force])
c .   dx and dy represent the distances over which finite diff apply
c .   Note: dy(1) and dy(nlat) are half width for
c .   one sided differences

c                       calculate the quantity [gravity/[cor force])
      DO NL = 1,NLAT
          IF (GLAT(NL).NE.0.D0) THEN
              GF(NL) = GR/ (2.D0*OMEGA*SIN(GLAT(NL)*RAD))
          ELSE
              GF(NL) = ZMSG
          END IF
          DX(NL) = DLON2*COS(GLAT(NL)*RAD)
      END DO
c                                     get around 'rounding' problem
      IF (GLAT(1).LT.-89.999D0) THEN
          DX(1) = 0.0D0
      END IF
      IF (GLAT(NLAT).GT.89.999D0) THEN
          DX(NLAT) = 0.0D0
      END IF
c                                     lat can have variable spacing
c                                     dy(1) and dy(nlat) are half size
c                                     for use in one-sided differences
      DY(1) = (GLAT(2)-GLAT(1))*RR
      DO NL = 2,NLAT - 1
          DY(NL) = (GLAT(NL+1)-GLAT(NL-1))*RR
      END DO
      DY(NLAT) = (GLAT(NLAT)-GLAT(NLAT-1))*RR

c set ug and vg to default values

      DO NL = 1,NLAT
          DO ML = 1,MLON
              UG(ML,NL) = ZMSG
              VG(ML,NL) = ZMSG
          END DO
      END DO

c use zz array to eliminate the need for special
c .   loops/statements to handle the east and west boundaries

c                                     fill zz 'middle'
      DO NL = 1,NLAT
          DO ML = 1,MLON
              ZZ(ML,NL) = Z(ML,NL)
          END DO
      END DO
c                                     bottom and top boundaries
      DO ML = 1,MLON
          ZZ(ML,0) = ZZ(ML,1)
          ZZ(ML,NLAT+1) = ZZ(ML,NLAT)
      END DO
c                                     left and right boundaries
      IF (IOPT.EQ.1) THEN
c                                     specify cyclic values
          DO NL = 1,NLAT
              ZZ(0,NL) = ZZ(MLON,NL)
              ZZ(MLON+1,NL) = ZZ(1,NL)
          END DO
      ELSE
c                                     duplicate adjacent points
          DO NL = 1,NLAT
              ZZ(0,NL) = ZZ(1,NL)
              ZZ(MLON+1,NL) = ZZ(MLON,NL)
          END DO
      END IF

c calculate the winds at all grid points of array zz
c .   ug calculated via a one-sided differences [crude approx]
c Note: a 'trick' is used
c .   (1) the top and bottom rows of zz are duplicates of adjacent rows
c .   (2) dy(1) and dy(nlat) are already set for one-sided differences
c .   (3) the combination will result in numerically correct results

      DO NL = 1,NLAT
          IF (.NOT. (GLAT(NL).EQ.0.0D0.OR.ABS(GLAT(NL)).GT.89.999D0))
     +        THEN
              DO ML = 1,MLON
                  IF (ZZ(ML,NL+1).NE.ZMSG .AND.
     +                ZZ(ML,NL-1).NE.ZMSG) THEN
                      UG(ML,NL) = -GF(NL)* (ZZ(ML,NL+1)-ZZ(ML,NL-1))/
     +                            DY(NL)
                  END IF
                  IF (ZZ(ML+1,NL).NE.ZMSG .AND.
     +                ZZ(ML-1,NL).NE.ZMSG) THEN
                      VG(ML,NL) = GF(NL)* (ZZ(ML+1,NL)-ZZ(ML-1,NL))/
     +                            DX(NL)
                  END IF
              END DO
          END IF
      END DO

c if not cyclic in lon then "adjust" the vg for the left/right columns
c .   Basically, they should be one sided differences BUT they were
c .   divided by 2*dx. The multiplication by 2 below undoes this.
c .   The reason for the "if" is to handle the case at the EQ

      IF (IOPT.NE.1) THEN
          DO NL = 1,NLAT
              IF (VG(1,NL).NE.ZMSG) VG(1,NL) = VG(1,NL)*2.0D0
              IF (VG(MLON,NL).NE.ZMSG) VG(MLON,NL) = VG(MLON,NL)*2.0D0
          END DO
      END IF

      RETURN
      END
