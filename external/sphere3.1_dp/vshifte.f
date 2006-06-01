c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
C*PL*ERROR* Comment line too long
c ... file vshifte.f contains code and documentation for subroutine vshifte
c     and its initialization subroutine vshifti
c
c ... required files
c
c     hrfft.f
c
c     subroutine vshifte(ioff,nlon,nlat,uoff,voff,ureg,vreg,
c    +                   wsave,lsave,work,lwork,ierror)
c
c *** purpose
c
c     subroutine vshifte does a highly accurate 1/2 grid increment shift
C*PL*ERROR* Comment line too long
c     in both longitude and latitude of equally spaced vector data on the
c     sphere. data is transferred between the nlon by nlat "offset grid"
C*PL*ERROR* Comment line too long
c     in (uoff,voff) (which excludes poles) and the nlon by nlat+1 "regular
C*PL*ERROR* Comment line too long
c     grid" in (ureg,vreg) (which includes poles).  the transfer can go from
C*PL*ERROR* Comment line too long
c     (uoff,voff) to (ureg,vreg) or vice versa (see ioff).  the grids which
C*PL*ERROR* Comment line too long
c     underly the vector fields are described below.  the north and south
C*PL*ERROR* Comment line too long
c     pole are at 0.5*pi and-0.5*pi radians respectively (pi=4.*atan(1.)).
C*PL*ERROR* Comment line too long
c     uoff and ureg are the east longitudinal vector data components.  voff
c     and vreg are the latitudinal vector data components.
c
c     subroutine sshifte can be used to shift scalar data on the sphere.
C*PL*ERROR* Comment line too long
c     notice that scalar and vector quantities are fundamentally different
C*PL*ERROR* Comment line too long
c     on the sphere.  for example, vectors are discontinuous and multiple
C*PL*ERROR* Comment line too long
c     valued at the poles.  scalars are continuous and single valued at the
C*PL*ERROR* Comment line too long
c     poles. erroneous results would be produced if one attempted to shift
c     vector fields with subroutine sshifte applied to each component of
c     of the vector.
c
c *** grid descriptions
c
c     let dlon = (pi+pi)/nlon and dlat = pi/nlat be the uniform grid
c     increments in longitude and latitude
c
c     offset grid
c
C*PL*ERROR* Comment line too long
c     the "1/2 increment offset" grid (long(j),lat(i)) on which uoff(j,i)
c     and voff(j,i) are given (ioff=0) or generated (ioff=1) is
c
c          long(j) =0.5*dlon + (j-1)*dlon  (j=1,...,nlon)
c
c     and
c
c          lat(i) = -0.5*pi + 0.5*dlat + (i-1)*dlat (i=1,...,nlat)
c
C*PL*ERROR* Comment line too long
c     the data in (uoff,voff) is "shifted" one half a grid increment in both
C*PL*ERROR* Comment line too long
c     longitude and latitude and excludes the poles.  each uoff(j,1),voff(j,1)
c     is given at latitude -pi/2+dlat/2.  uoff(j,nlat),voff(j,nlat) is
c     given at pi/2-dlat/2 (1/2 a grid increment away from the poles).
C*PL*ERROR* Comment line too long
c     uoff(1,i),voff(1,i) is given at longitude dlon/2.  each uoff(nlon,i),
c     voff(nlon,i) is given at longitude 2*pi-dlon/2.
c
c     regular grid
c
c     let dlat,dlon be as above.  then the nlon by nlat+1 grid on which
c     ureg(j,i),vreg(j,i) are generated (ioff=0) or given (ioff=1) is
c
c          lone(j) = (j-1)*dlon (j=1,...,nlon)
c
c      and
c
c          late(i) = -0.5*pi + (i-1)*dlat (i=1,...,nlat+1)
c
c     values in ureg,vreg include the poles and start at zero degrees
c     longitude and at the south pole this is the "usual" equally spaced
c     grid in geophysical coordinates.
c
c *** remark
c
C*PL*ERROR* Comment line too long
c     subroutine vshifte can be used in conjunction with subroutine trvsph
C*PL*ERROR* Comment line too long
c     when transferring vector data from an equally spaced "1/2 increment
C*PL*ERROR* Comment line too long
c     offset" grid to a gaussian or equally spaced grid (which includes poles)
C*PL*ERROR* Comment line too long
c     of any resolution.  this problem (personal communication with dennis
c     shea) is encountered in geophysical modeling and data analysis.
c
c *** method
c
c     fast fourier transform software from spherepack2 and trigonometric
c     identities are used to accurately "shift" periodic vectors half a
c     grid increment in latitude and longitude.  latitudinal shifts are
C*PL*ERROR* Comment line too long
c     accomplished by setting periodic 2*nlat vectors over the pole for each
c     longitude.  vector values must be negated on one side of the pole
c     to maintain periodicity prior to the 2*nlat shift over the poles.
C*PL*ERROR* Comment line too long
c     when nlon is odd, the 2*nlat latitudinal shift requires an additional
C*PL*ERROR* Comment line too long
c     longitude shift to obtain symmetry necessary for full circle shifts
c     over the poles.  finally longitudinal shifts are executed for each
c     shifted latitude.
c
c *** argument description
c
c ... ioff
c
c     ioff = 0 if values on the offset grid in (uoff,voff) are given and
C*PL*ERROR* Comment line too long
c              values on the regular grid in (ureg,vreg) are to be generated.
c
C*PL*ERROR* Comment line too long
c     ioff = 1 if values on the regular grid in (ureg,vreg) are given and
C*PL*ERROR* Comment line too long
c              values on the offset grid in (uoff,voff) are to be generated.
c
c ... nlon
c
c     the number of longitude points on both the "offset" and "regular"
c     uniform grid in longitude (see "grid description" above).  nlon
C*PL*ERROR* Comment line too long
c     is also the first dimension of uoff,voff,ureg,vreg.  nlon determines
C*PL*ERROR* Comment line too long
c     the grid increment in longitude as dlon = 2.*pi/nlon.  for example,
C*PL*ERROR* Comment line too long
c     nlon = 144 for a 2.5 degree grid.  nlon can be even or odd and must
c     be greater than or equal to 4.  the efficiency of the computation
c     is improved when nlon is a product of small primes.
c
c ... nlat
c
C*PL*ERROR* Comment line too long
c     the number of latitude points on the "offset" uniform grid.  nlat+1
C*PL*ERROR* Comment line too long
c     is the number of latitude points on the "regular" uniform grid (see
C*PL*ERROR* Comment line too long
c     "grid description" above).  nlat is the second dimension of uoff,voff.
c     nlat+1 must be the second dimension of ureg,vreg in the program
c     calling vshifte.  nlat determines the grid in latitude as pi/nlat.
C*PL*ERROR* Comment line too long
c     for example, nlat = 36 for a five degree grid.  nlat must be at least 3.
c
c ... uoff
c
c     a nlon by nlat array that contains the east longitudinal vector
c     data component on the offset grid described above.  uoff is a
c     given input argument if ioff=0.  uoff is a generated output
c     argument if ioff=1.
c
c ... voff
c
c     a nlon by nlat array that contains the latitudinal vector data
c     component on the offset grid described above.  voff is a given
c     input argument if ioff=0.  voff is a generated output argument
c     if ioff=1.
c
c ... ureg
c
c     a nlon by nlat+1 array that contains the east longitudinal vector
C*PL*ERROR* Comment line too long
c     data component on the regular grid described above.  ureg is a given
c     input argument if ioff=1.  ureg is a generated output argument
c     if ioff=0.
c
c ... vreg
c
c     a nlon by nlat+1 array that contains the latitudinal vector data
c     component on the regular grid described above.  vreg is a given
c     input argument if ioff=1.  vreg is a generated output argument
c     if ioff=0.
c
c ... wsav
c
c     a real saved work space array that must be initialized by calling
C*PL*ERROR* Comment line too long
c     subroutine vshifti(ioff,nlon,nlat,wsav,ier) before calling vshifte.
c     wsav can then be used repeatedly by vshifte as long as ioff, nlon,
c     and nlat do not change.  this bypasses redundant computations and
c     saves time.  undetectable errors will result if vshifte is called
c     without initializing wsav whenever ioff, nlon, or nlat change.
c
c ... lsav
c
C*PL*ERROR* Comment line too long
c     the length of the saved work space wsav in the routine calling vshifte
C*PL*ERROR* Comment line too long
c     and sshifti.  lsave must be greater than or equal to 2*(2*nlat+nlon+16).
c
c ... work
c
c     a real unsaved work space
c
c ... lwork
c
C*PL*ERROR* Comment line too long
c     the length of the unsaved work space in the routine calling vshifte
c     if nlon is even then lwork must be greater than or equal to
c
c          2*nlon*(nlat+1)
c
c     if nlon is odd then lwork must be greater than or equal to
c
c          nlon*(5*nlat+1)
c
c ... ier
c
c     indicates errors in input parameters
c
c     = 0 if no errors are detected
c
c     = 1 if ioff is not equal to 0 or 1
c
c     = 2 if nlon < 4
c
c     = 3 if nlat < 3
c
c     = 4 if lsave < 2*(nlon+2*nlat)+32
c
c     = 5 if lwork < 2*nlon*(nlat+1) for nlon even or
c            lwork < nlon*(5*nlat+1) for nlon odd
c
c *** end of vshifte documentation
c
c     subroutine vshifti(ioff,nlon,nlat,lsav,wsav,ier)
c
c     subroutine vshifti initializes the saved work space wsav
c     for ioff and nlon and nlat (see documentation for vshifte).
c     vshifti must be called before vshifte whenever ioff or nlon
c     or nlat change.
c
c ... ier
c
c     = 0 if no errors with input arguments
c
c     = 1 if ioff is not 0 or 1
c
c     = 2 if nlon < 4
c
c     = 3 if nlat < 3
c
c     = 4 if lsav < 2*(2*nlat+nlon+16)
c
c *** end of vshifti documentation
c
      SUBROUTINE DVSHIFTE(IOFF,NLON,NLAT,UOFF,VOFF,UREG,VREG,WSAV,LSAV,
     +                   WRK,LWRK,IER)
      IMPLICIT NONE
      INTEGER IOFF,NLON,NLAT,N2,NR,NLAT2,NLATP1,LSAV,LWRK,IER
      INTEGER I1,I2,I3
      DOUBLE PRECISION UOFF(NLON,NLAT),VOFF(NLON,NLAT)
      DOUBLE PRECISION UREG(NLON,*),VREG(NLON,*)
      DOUBLE PRECISION WSAV(LSAV),WRK(LWRK)
c
c     check input parameters
c
      IER = 1
      IF (IOFF* (IOFF-1).NE.0) RETURN
      IER = 2
      IF (NLON.LT.4) RETURN
      IER = 3
      IF (NLAT.LT.3) RETURN
      IER = 4
      IF (LSAV.LT.2* (2*NLAT+NLON+16)) RETURN
      NLAT2 = NLAT + NLAT
      NLATP1 = NLAT + 1
      N2 = (NLON+1)/2
      IER = 5
      IF (2*N2.EQ.NLON) THEN
          IF (LWRK.LT.2*NLON* (NLAT+1)) RETURN
          NR = N2
          I1 = 1
          I2 = 1
          I3 = I2 + NLON*NLATP1
      ELSE
          IF (LWRK.LT.NLON* (5*NLAT+1)) RETURN
          NR = NLON
          I1 = 1
          I2 = I1 + NLAT2*NLON
          I3 = I2 + NLATP1*NLON
      END IF
      IER = 0
      IF (IOFF.EQ.0) THEN
c
c     shift (uoff,voff) to (ureg,vreg)
c
          CALL DVHFTOFF(NLON,NLAT,UOFF,UREG,WSAV,NR,NLAT2,NLATP1,
     +                  WRK(I1),WRK(I2),WRK(I2),WRK(I3))
          CALL DVHFTOFF(NLON,NLAT,VOFF,VREG,WSAV,NR,NLAT2,NLATP1,
     +                  WRK(I1),WRK(I2),WRK(I2),WRK(I3))
      ELSE
c
c     shift (ureg,vreg) to (uoff,voff)
c
          CALL DVHFTREG(NLON,NLAT,UOFF,UREG,WSAV,NR,NLAT2,NLATP1,
     +                  WRK(I1),WRK(I2),WRK(I2),WRK(I3))
          CALL DVHFTREG(NLON,NLAT,VOFF,VREG,WSAV,NR,NLAT2,NLATP1,
     +                  WRK(I1),WRK(I2),WRK(I2),WRK(I3))
      END IF
      END

      SUBROUTINE DVHFTOFF(NLON,NLAT,UOFF,UREG,WSAV,NR,NLAT2,NLATP1,
     +                   RLATU,RLONU,RLOU,WRK)
c
c     generate ureg from uoff (a vector component!)
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,NLAT2,NLATP1,N2,NR,J,I,JS,ISAV
      DOUBLE PRECISION UOFF(NLON,NLAT),UREG(NLON,NLATP1)
      DOUBLE PRECISION RLATU(NR,NLAT2),RLONU(NLATP1,NLON),
     +                 RLOU(NLAT,NLON)
      DOUBLE PRECISION WSAV(*),WRK(*)

      ISAV = 4*NLAT + 17
      N2 = (NLON+1)/2
c
c     execute full circle latitude shifts for nlon odd or even
c
      IF (2*N2.GT.NLON) THEN
c
c     odd number of longitudes
c
          DO I = 1,NLAT
              DO J = 1,NLON
                  RLOU(I,J) = UOFF(J,I)
              END DO
          END DO
c
c       half shift in longitude
c
          CALL DVHIFTH(NLAT,NLON,RLOU,WSAV(ISAV),WRK)
c
c       set full 2*nlat circles in rlatu using shifted values in rlonu
c
          DO J = 1,N2 - 1
              JS = J + N2
              DO I = 1,NLAT
                  RLATU(J,I) = UOFF(J,I)
                  RLATU(J,NLAT+I) = -RLOU(NLAT+1-I,JS)
              END DO
          END DO
          DO J = N2,NLON
              JS = J - N2 + 1
              DO I = 1,NLAT
                  RLATU(J,I) = UOFF(J,I)
                  RLATU(J,NLAT+I) = -RLOU(NLAT+1-I,JS)
              END DO
          END DO
c
c       shift the nlon rlat vectors one half latitude grid
c
          CALL DVHIFTH(NLON,NLAT2,RLATU,WSAV,WRK)
c
c       set in ureg
c
          DO J = 1,NLON
              DO I = 1,NLAT + 1
                  UREG(J,I) = RLATU(J,I)
              END DO
          END DO
      ELSE
c
c     even number of longitudes (no initial longitude shift necessary)
C*PL*ERROR* Comment line too long
c     set full 2*nlat circles (over poles) for each longitude pair (j,js)
c     negating js vector side for periodicity
c
          DO J = 1,N2
              JS = N2 + J
              DO I = 1,NLAT
                  RLATU(J,I) = UOFF(J,I)
                  RLATU(J,NLAT+I) = -UOFF(JS,NLATP1-I)
              END DO
          END DO
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
          CALL DVHIFTH(N2,NLAT2,RLATU,WSAV,WRK)
c
c       set ureg,vreg shifted in latitude
c
          DO J = 1,N2
              JS = N2 + J
              UREG(J,1) = RLATU(J,1)
              UREG(JS,1) = -RLATU(J,1)
              DO I = 2,NLATP1
                  UREG(J,I) = RLATU(J,I)
                  UREG(JS,I) = -RLATU(J,NLAT2-I+2)
              END DO
          END DO
      END IF
c
c     execute full circle longitude shift
c
      DO J = 1,NLON
          DO I = 1,NLATP1
              RLONU(I,J) = UREG(J,I)
          END DO
      END DO
      CALL DVHIFTH(NLATP1,NLON,RLONU,WSAV(ISAV),WRK)
      DO J = 1,NLON
          DO I = 1,NLATP1
              UREG(J,I) = RLONU(I,J)
          END DO
      END DO
      END

      SUBROUTINE DVHFTREG(NLON,NLAT,UOFF,UREG,WSAV,NR,NLAT2,NLATP1,
     +                   RLATU,RLONU,RLOU,WRK)
c
c     generate uoff vector component from ureg
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,NLAT2,NLATP1,N2,NR,J,I,JS,ISAV
      DOUBLE PRECISION UOFF(NLON,NLAT),UREG(NLON,NLATP1)
      DOUBLE PRECISION RLATU(NR,NLAT2),RLONU(NLATP1,NLON),
     +                 RLOU(NLAT,NLON)
      DOUBLE PRECISION WSAV(*),WRK(*)

      ISAV = 4*NLAT + 17
      N2 = (NLON+1)/2
c
c     execute full circle latitude shifts for nlon odd or even
c
      IF (2*N2.GT.NLON) THEN
c
c     odd number of longitudes
c
          DO I = 1,NLATP1
              DO J = 1,NLON
                  RLONU(I,J) = UREG(J,I)
              END DO
          END DO
c
c       half shift in longitude in rlon
c
          CALL DVHIFTH(NLATP1,NLON,RLONU,WSAV(ISAV),WRK)
c
c       set full 2*nlat circles in rlat using shifted values in rlon
c
          DO J = 1,N2
              JS = J + N2 - 1
              RLATU(J,1) = UREG(J,1)
              DO I = 2,NLAT
                  RLATU(J,I) = UREG(J,I)
                  RLATU(J,NLAT+I) = -RLONU(NLAT+2-I,JS)
              END DO
              RLATU(J,NLAT+1) = UREG(J,NLAT+1)
          END DO
          DO J = N2 + 1,NLON
              JS = J - N2
              RLATU(J,1) = UREG(J,1)
              DO I = 2,NLAT
                  RLATU(J,I) = UREG(J,I)
                  RLATU(J,NLAT+I) = -RLONU(NLAT+2-I,JS)
              END DO
              RLATU(J,NLAT+1) = UREG(J,NLAT+1)
          END DO
c
c       shift the nlon rlat vectors one halflatitude grid
c
          CALL DVHIFTH(NLON,NLAT2,RLATU,WSAV,WRK)
c
c       set values in uoff
c
          DO J = 1,NLON
              DO I = 1,NLAT
                  UOFF(J,I) = RLATU(J,I)
              END DO
          END DO
      ELSE
c
c     even number of longitudes (no initial longitude shift necessary)
C*PL*ERROR* Comment line too long
c     set full 2*nlat circles (over poles) for each longitude pair (j,js)
c
          DO J = 1,N2
              JS = N2 + J
              RLATU(J,1) = UREG(J,1)
              DO I = 2,NLAT
                  RLATU(J,I) = UREG(J,I)
                  RLATU(J,NLAT+I) = -UREG(JS,NLAT+2-I)
              END DO
              RLATU(J,NLAT+1) = UREG(J,NLAT+1)
          END DO
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
          CALL DVHIFTH(N2,NLAT2,RLATU,WSAV,WRK)
c
c       set values in uoff
c
          DO J = 1,N2
              JS = N2 + J
              DO I = 1,NLAT
                  UOFF(J,I) = RLATU(J,I)
                  UOFF(JS,I) = -RLATU(J,NLAT2+1-I)
              END DO
          END DO
      END IF
c
c     execute full circle longitude shift for all latitude circles
c
      DO J = 1,NLON
          DO I = 1,NLAT
              RLOU(I,J) = UOFF(J,I)
          END DO
      END DO
      CALL DVHIFTH(NLAT,NLON,RLOU,WSAV(ISAV),WRK)
      DO J = 1,NLON
          DO I = 1,NLAT
              UOFF(J,I) = RLOU(I,J)
          END DO
      END DO
      END

      SUBROUTINE DVSHIFTI(IOFF,NLON,NLAT,LSAV,WSAV,IER)
c
c     initialize wsav for vshifte
c
      INTEGER IOFF,NLAT,NLON,NLAT2,ISAV,IER
      DOUBLE PRECISION WSAV(LSAV)
      DOUBLE PRECISION PI,DLAT,DLON,DP

      IER = 1
      IF (IOFF* (IOFF-1).NE.0) RETURN
      IER = 2
      IF (NLON.LT.4) RETURN
      IER = 3
      IF (NLAT.LT.3) RETURN
      IER = 4
      IF (LSAV.LT.2* (2*NLAT+NLON+16)) RETURN
      IER = 0
      PI = 4.0D0*ATAN(1.0D0)
c
c     set lat,long increments
c
      DLAT = PI/NLAT
      DLON = (PI+PI)/NLON
c
c     set left or right latitude shifts
c
      IF (IOFF.EQ.0) THEN
          DP = -0.5D0*DLAT
      ELSE
          DP = 0.5D0*DLAT
      END IF
      NLAT2 = NLAT + NLAT
      CALL DVHIFTHI(NLAT2,DP,WSAV)
c
c     set left or right longitude shifts
c
      IF (IOFF.EQ.0) THEN
          DP = -0.5D0*DLON
      ELSE
          DP = 0.5D0*DLON
      END IF
      ISAV = 4*NLAT + 17
      CALL DVHIFTHI(NLON,DP,WSAV(ISAV))
      RETURN
      END

      SUBROUTINE DVHIFTH(M,N,R,WSAV,WORK)
      IMPLICIT NONE
      INTEGER M,N,N2,K,L
      DOUBLE PRECISION R(M,N),WSAV(*),WORK(*),R2KM2,R2KM1

      N2 = (N+1)/2
c
c     compute fourier coefficients for r on shifted grid
c
      CALL DHRFFTF(M,N,R,M,WSAV(N+2),WORK)
      DO L = 1,M
          DO K = 2,N2
              R2KM2 = R(L,K+K-2)
              R2KM1 = R(L,K+K-1)
              R(L,K+K-2) = R2KM2*WSAV(N2+K) - R2KM1*WSAV(K)
              R(L,K+K-1) = R2KM2*WSAV(K) + R2KM1*WSAV(N2+K)
          END DO
      END DO
c
c     shift r with fourier synthesis and normalization
c
      CALL DHRFFTB(M,N,R,M,WSAV(N+2),WORK)
      DO L = 1,M
          DO K = 1,N
              R(L,K) = R(L,K)/N
          END DO
      END DO
      RETURN
      END

      SUBROUTINE DVHIFTHI(N,DP,WSAV)
c
c     initialize wsav for subroutine vhifth
c
      IMPLICIT NONE
      INTEGER N,N2,K
      DOUBLE PRECISION WSAV(*),DP

      N2 = (N+1)/2
      DO K = 2,N2
          WSAV(K) = SIN((K-1)*DP)
          WSAV(K+N2) = COS((K-1)*DP)
      END DO
      CALL DHRFFTI(N,WSAV(N+2))
      RETURN
      END
