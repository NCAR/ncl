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
C*PL*ERROR* Comment line too long
c ... file sshifte.f contains code and documentation for subroutine sshifte
c     and its' initialization subroutine sshifti
c
c ... required files off spherepack3.0
c
c     hrfft.f
c
C*PL*ERROR* Comment line too long
c     subroutine sshifte(ioff,nlon,nlat,goff,greg,wsav,lsav,work,lwork,ier)
c
c *** purpose
c
c     subroutine sshifte does a highly accurate 1/2 grid increment shift
C*PL*ERROR* Comment line too long
c     in both longitude and latitude of equally spaced data on the sphere.
c     data is transferred between the nlon by nlat "offset grid" in goff
C*PL*ERROR* Comment line too long
c     (which excludes poles) and the nlon by nlat+1 "regular grid" in greg
C*PL*ERROR* Comment line too long
c     (which includes poles).  the transfer can go from goff to greg or from
C*PL*ERROR* Comment line too long
c     greg to goff (see ioff).  the grids which underly goff and greg are
C*PL*ERROR* Comment line too long
c     described below.  the north and south poles are at latitude 0.5*pi and
c     -0.5*pi radians respectively where pi = 4.*atan(1.).
c
c *** grid descriptions
c
c     let dlon = (pi+pi)/nlon and dlat = pi/nlat be the uniform grid
c     increments in longitude and latitude
c
c     offset grid
c
C*PL*ERROR* Comment line too long
c     the "1/2 increment offset" grid (long(j),lat(i)) on which goff(j,i)
c     is given (ioff=0) or generated (ioff=1) is
c
c          long(j) =0.5*dlon + (j-1)*dlon  (j=1,...,nlon)
c
c     and
c
c          lat(i) = -0.5*pi + 0.5*dlat + (i-1)*dlat (i=1,...,nlat)
c
C*PL*ERROR* Comment line too long
c     the data in goff is "shifted" one half a grid increment in longitude
c     and latitude and excludes the poles.  each goff(j,1) is given at
C*PL*ERROR* Comment line too long
c     latitude -0.5*pi+0.5*dlat and goff(j,nlat) is given at 0.5*pi-0.5*dlat
C*PL*ERROR* Comment line too long
c     (1/2 a grid increment away from the poles).  each goff(1,i),goff(nlon,i)
c     is given at longitude 0.5*dlon and 2.*pi-0.5*dlon.
c
c     regular grid
c
c     let dlat,dlon be as above.  then the nlon by nlat+1 grid on which
c     greg(j,i) is generated (ioff=0) or given (ioff=1) is given by
c
c          lone(j) = (j-1)*dlon (j=1,...,nlon)
c
c      and
c
c          late(i) = -0.5*pi + (i-1)*dlat (i=1,...,nlat+1)
c
C*PL*ERROR* Comment line too long
c     values in greg include the poles and start at zero degrees longitude.
c
c *** remark
c
C*PL*ERROR* Comment line too long
c     subroutine sshifte can be used in conjunction with subroutine trssph
C*PL*ERROR* Comment line too long
c     when transferring data from an equally spaced "1/2 increment offset"
C*PL*ERROR* Comment line too long
c     grid to a gaussian or equally spaced grid (which includes poles) of
c     any resolution.  this problem (personal communication with dennis
c     shea) is encountered in geophysical modeling and data analysis.
c
c *** method
c
c     fast fourier transform software from spherepack2 and trigonometric
c     identities are used to accurately "shift" periodic vectors half a
c     grid increment in latitude and longitude.  latitudinal shifts are
C*PL*ERROR* Comment line too long
c     accomplished by setting periodic 2*nlat vectors over the pole for each
C*PL*ERROR* Comment line too long
c     longitude.  when nlon is odd, this requires an additional longitude
C*PL*ERROR* Comment line too long
c     shift.  longitudinal shifts are then executed for each shifted latitude.
c     when necessary (ioff=0) poles are obtained by averaging the nlon
c     shifted polar values.
c
c *** required files from spherepack3.0
c
c     hrfft.f
c
c *** argument description
c
c ... ioff
c
c     ioff = 0 if values on the offset grid in goff are given and values
c              on the regular grid in greg are to be generated.
c
C*PL*ERROR* Comment line too long
c     ioff = 1 if values on the regular grid in greg are given and values
c              on the offset grid in goff are to be generated.
c
c ... nlon
c
c     the number of longitude points on both the "offset" and "regular"
c     uniform grid in longitude (see "grid description" above).  nlon
C*PL*ERROR* Comment line too long
c     is also the first dimension of array goff and greg.  nlon determines
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
c     "grid description" above).  nlat is the second dimension of array goff.
C*PL*ERROR* Comment line too long
c     nlat+1 must be the second dimension of the array greg in the program
c     calling sshifte.  nlat determines the grid in latitude as pi/nlat.
C*PL*ERROR* Comment line too long
c     for example, nlat = 36 for a five degree grid.  nlat must be at least 3.
c
c ... goff
c
c     a nlon by nlat array that contains data on the offset grid
c     described above.  goff is a given input argument if ioff=0.
c     goff is a generated output argument if ioff=1.
c
c ... greg
c
c     a nlon by nlat+1 array that contains data on the regular grid
c     described above.  greg is a given input argument if ioff=1.
c     greg is a generated output argument if ioff=0.
c
c ... wsav
c
c     a real saved work space array that must be initialized by calling
C*PL*ERROR* Comment line too long
c     subroutine sshifti(ioff,nlon,nlat,wsav,ier) before calling sshifte.
c     wsav can then be used repeatedly by sshifte as long as ioff, nlon,
c     and nlat do not change.  this bypasses redundant computations and
c     saves time.  undetectable errors will result if sshifte is called
c     without initializing wsav whenever ioff, nlon, or nlat change.
c
c ... lsav
c
C*PL*ERROR* Comment line too long
c     the length of the saved work space wsav in the routine calling sshifte
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
c     the length of the unsaved work space in the routine calling sshifte
C*PL*ERROR* Comment line too long
c     lwork must be greater than or equal to 2*nlon*(nlat+1) if nlon is even.
C*PL*ERROR* Comment line too long
c     lwork must be greater than or equal to nlon*(5*nlat+1) if nlon is odd.
c
c ... ier
c
c     indicates errors in input parameters
c
c     = 0 if no errors are detected
c
c     = 1 if ioff is not equal to 0 or 1
c
c     = 1 if nlon < 4
c
c     = 2 if nlat < 3
c
c     = 3 if lsave < 2*(nlon+2*nlat+16)
c
c     = 4 if lwork < 2*nlon*(nlat+1) for nlon even or
c            lwork < nlon*(5*nlat+1) for nlon odd
c
c *** end of sshifte documentation
c
c     subroutine sshifti(ioff,nlon,nlat,lsav,wsav,ier)
c
c     subroutine sshifti initializes the saved work space wsav
c     for ioff and nlon and nlat (see documentation for sshifte).
c     sshifti must be called before sshifte whenever ioff or nlon
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
c *** end of sshifti documentation
c
      SUBROUTINE DSSHIFTE(IOFF,NLON,NLAT,GOFF,GREG,WSAV,LSAV,WRK,LWRK,
     +                   IER)
      IMPLICIT NONE
      INTEGER IOFF,NLON,NLAT,N2,NR,NLAT2,NLATP1,LSAV,LWRK,I1,I2,IER
      DOUBLE PRECISION GOFF(NLON,NLAT),GREG(NLON,*),WSAV(LSAV),WRK(LWRK)
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
      IER = 5
      N2 = (NLON+1)/2
      IF (2*N2.EQ.NLON) THEN
          IF (LWRK.LT.2*NLON* (NLAT+1)) RETURN
          I1 = 1
          NR = N2
      ELSE
          IF (LWRK.LT.NLON* (5*NLAT+1)) RETURN
          I1 = 1 + 2*NLAT*NLON
          NR = NLON
      END IF
      IER = 0
      NLAT2 = NLAT + NLAT
      I2 = I1 + (NLAT+1)*NLON
      IF (IOFF.EQ.0) THEN
          CALL DSHFTOFF(NLON,NLAT,GOFF,GREG,WSAV,NR,NLAT2,WRK,WRK(I1),
     +                 WRK(I2))
      ELSE
          NLATP1 = NLAT + 1
          CALL DSHFTREG(NLON,NLAT,GOFF,GREG,WSAV,NR,NLAT2,NLATP1,WRK,
     +                 WRK(I1),WRK(I2))
      END IF
      END

      SUBROUTINE DSHFTOFF(NLON,NLAT,GOFF,GREG,WSAV,NR,NLAT2,RLAT,RLON,
     +                   WRK)
c
c     shift offset grid to regular grid, i.e.,
c     goff is given, greg is to be generated
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,NLAT2,N2,NR,J,I,JS,ISAV
      DOUBLE PRECISION GOFF(NLON,NLAT),GREG(NLON,NLAT+1)
      DOUBLE PRECISION RLAT(NR,NLAT2),RLON(NLAT,NLON)
      DOUBLE PRECISION WSAV(*),WRK(*)
      DOUBLE PRECISION GNORTH,GSOUTH

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
                  RLON(I,J) = GOFF(J,I)
              END DO
          END DO
c
c       half shift in longitude
c
          CALL DSHIFTH(NLAT,NLON,RLON,WSAV(ISAV),WRK)
c
c       set full 2*nlat circles in rlat using shifted values in rlon
c
          DO J = 1,N2 - 1
              JS = J + N2
              DO I = 1,NLAT
                  RLAT(J,I) = GOFF(J,I)
                  RLAT(J,NLAT+I) = RLON(NLAT+1-I,JS)
              END DO
          END DO
          DO J = N2,NLON
              JS = J - N2 + 1
              DO I = 1,NLAT
                  RLAT(J,I) = GOFF(J,I)
                  RLAT(J,NLAT+I) = RLON(NLAT+1-I,JS)
              END DO
          END DO
c
c       shift the nlon rlat vectors one half latitude grid
c
          CALL DSHIFTH(NLON,NLAT2,RLAT,WSAV,WRK)
c
c       set nonpole values in greg and average for poles
c
          GNORTH = 0.0D0
          GSOUTH = 0.0D0
          DO J = 1,NLON
              GNORTH = GNORTH + RLAT(J,1)
              GSOUTH = GSOUTH + RLAT(J,NLAT+1)
              DO I = 2,NLAT
                  GREG(J,I) = RLAT(J,I)
              END DO
          END DO
          GNORTH = GNORTH/NLON
          GSOUTH = GSOUTH/NLON

      ELSE
c
c     even number of longitudes (no initial longitude shift necessary)
C*PL*ERROR* Comment line too long
c     set full 2*nlat circles (over poles) for each longitude pair (j,js)
c
          DO J = 1,N2
              JS = N2 + J
              DO I = 1,NLAT
                  RLAT(J,I) = GOFF(J,I)
                  RLAT(J,NLAT+I) = GOFF(JS,NLAT+1-I)
              END DO
          END DO
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
          CALL DSHIFTH(N2,NLAT2,RLAT,WSAV,WRK)
c
c       set nonpole values in greg and average poles
c
          GNORTH = 0.0D0
          GSOUTH = 0.0D0
          DO J = 1,N2
              JS = N2 + J
              GNORTH = GNORTH + RLAT(J,1)
              GSOUTH = GSOUTH + RLAT(J,NLAT+1)
              DO I = 2,NLAT
                  GREG(J,I) = RLAT(J,I)
                  GREG(JS,I) = RLAT(J,NLAT2-I+2)
              END DO
          END DO
          GNORTH = GNORTH/N2
          GSOUTH = GSOUTH/N2
      END IF
c
c     set poles
c
      DO J = 1,NLON
          GREG(J,1) = GNORTH
          GREG(J,NLAT+1) = GSOUTH
      END DO
c
c     execute full circle longitude shift
c
      DO J = 1,NLON
          DO I = 1,NLAT
              RLON(I,J) = GREG(J,I)
          END DO
      END DO
      CALL DSHIFTH(NLAT,NLON,RLON,WSAV(ISAV),WRK)
      DO J = 1,NLON
          DO I = 2,NLAT
              GREG(J,I) = RLON(I,J)
          END DO
      END DO
      END

      SUBROUTINE DSHFTREG(NLON,NLAT,GOFF,GREG,WSAV,NR,NLAT2,NLATP1,RLAT,
     +                   RLON,WRK)
c
c     shift regular grid to offset grid, i.e.,
c     greg is given, goff is to be generated
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,NLAT2,NLATP1,N2,NR,J,I,JS,ISAV
      DOUBLE PRECISION GOFF(NLON,NLAT),GREG(NLON,NLATP1)
      DOUBLE PRECISION RLAT(NR,NLAT2),RLON(NLATP1,NLON)
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
          DO I = 1,NLAT + 1
              DO J = 1,NLON
                  RLON(I,J) = GREG(J,I)
              END DO
          END DO
c
c       half shift in longitude in rlon
c
          CALL DSHIFTH(NLAT+1,NLON,RLON,WSAV(ISAV),WRK)
c
c       set full 2*nlat circles in rlat using shifted values in rlon
c
          DO J = 1,N2
              JS = J + N2 - 1
              RLAT(J,1) = GREG(J,1)
              DO I = 2,NLAT
                  RLAT(J,I) = GREG(J,I)
                  RLAT(J,NLAT+I) = RLON(NLAT+2-I,JS)
              END DO
              RLAT(J,NLAT+1) = GREG(J,NLAT+1)
          END DO
          DO J = N2 + 1,NLON
              JS = J - N2
              RLAT(J,1) = GREG(J,1)
              DO I = 2,NLAT
                  RLAT(J,I) = GREG(J,I)
                  RLAT(J,NLAT+I) = RLON(NLAT+2-I,JS)
              END DO
              RLAT(J,NLAT+1) = GREG(J,NLAT+1)
          END DO
c
c       shift the nlon rlat vectors one halflatitude grid
c
          CALL DSHIFTH(NLON,NLAT2,RLAT,WSAV,WRK)
c
c       set values in goff
c
          DO J = 1,NLON
              DO I = 1,NLAT
                  GOFF(J,I) = RLAT(J,I)
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
              RLAT(J,1) = GREG(J,1)
              DO I = 2,NLAT
                  RLAT(J,I) = GREG(J,I)
                  RLAT(J,NLAT+I) = GREG(JS,NLAT+2-I)
              END DO
              RLAT(J,NLAT+1) = GREG(J,NLAT+1)
          END DO
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
          CALL DSHIFTH(N2,NLAT2,RLAT,WSAV,WRK)
c
c       set values in goff
c
          DO J = 1,N2
              JS = N2 + J
              DO I = 1,NLAT
                  GOFF(J,I) = RLAT(J,I)
                  GOFF(JS,I) = RLAT(J,NLAT2+1-I)
              END DO
          END DO
      END IF
c
c     execute full circle longitude shift for all latitude circles
c
      DO J = 1,NLON
          DO I = 1,NLAT
              RLON(I,J) = GOFF(J,I)
          END DO
      END DO
      CALL DSHIFTH(NLAT+1,NLON,RLON,WSAV(ISAV),WRK)
      DO J = 1,NLON
          DO I = 1,NLAT
              GOFF(J,I) = RLON(I,J)
          END DO
      END DO
      END

      SUBROUTINE DSSHIFTI(IOFF,NLON,NLAT,LSAV,WSAV,IER)
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
c     initialize wsav for left or right latitude shifts
c
      IF (IOFF.EQ.0) THEN
          DP = -0.5D0*DLAT
      ELSE
          DP = 0.5D0*DLAT
      END IF
      NLAT2 = NLAT + NLAT
      CALL DSHIFTHI(NLAT2,DP,WSAV)
c
c     initialize wsav for left or right longitude shifts
c
      IF (IOFF.EQ.0) THEN
          DP = -0.5D0*DLON
      ELSE
          DP = 0.5D0*DLON
      END IF
      ISAV = 4*NLAT + 17
      CALL DSHIFTHI(NLON,DP,WSAV(ISAV))
      RETURN
      END

      SUBROUTINE DSHIFTH(M,N,R,WSAV,WORK)
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

      SUBROUTINE DSHIFTHI(N,DP,WSAV)
c
c     initialize wsav for subroutine shifth
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
