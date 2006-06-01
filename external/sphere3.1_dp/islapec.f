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
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file islapec.f
c
c     this file includes documentation and code for
c     subroutine islapec         i
c
c ... files which must be loaded with islapec.f
c
c     sphcom.f, hrfft.f, shaec.f, shsec.f
c
c     subroutine islapec(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
c    +mdab,ndab,wshsec,lshsec,work,lwork,pertrb,ierror)
c
c     islapec inverts the laplace or helmholz operator on an equally
c     spaced latitudinal grid using o(n**2) storage. given the
c     spherical harmonic coefficients a(m,n) and b(m,n) of the right
c     hand side slap(i,j), islapec computes a solution sf(i,j) to
c     the following helmhotz equation :
c
c           2                2
c     [d(sf(i,j))/dlambda /sint + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c                   - xlmbda * sf(i,j) = slap(i,j)
c
c      where sf(i,j) is computed at colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
C*PL*ERROR* Comment line too long
c     isym   this parameter should have the same value input to subroutine
C*PL*ERROR* Comment line too long
c            shaec to compute the coefficients a and b for the scalar field
c            slap.  isym is set as follows:
c
c            = 0  no symmetries exist in slap about the equator. scalar
c                 synthesis is used to compute sf on the entire sphere.
c                 i.e., in the array sf(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c   nt       the number of solutions. in the program that calls islapec
c            the arrays sf,a, and b can be three dimensional in which
c            case multiple solutions are computed. the third index
c            is the solution index with values k=1,...,nt.
c            for a single solution set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            and sf,a,b are two dimensional.
c
c   xlmbda   a one dimensional array with nt elements. if xlmbda is
c            is identically zero islapec solves poisson's equation.
c            if xlmbda > 0.0 islapec solves the helmholtz equation.
c            if xlmbda < 0.0 the nonfatal error flag ierror=-1 is
c            returned. negative xlambda could result in a division
c            by zero.
c
c   ids      the first dimension of the array sf as it appears in the
C*PL*ERROR* Comment line too long
c            program that calls islapec.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array sf as it appears in the
c            program that calls islapec. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field slap. a,b must be computed by shaec
c            prior to calling islapec.
c
c
c   mdab     the first dimension of the arrays a and b as it appears
c            in the program that calls islapec.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c   ndab     the second dimension of the arrays a and b as it appears
c            in the program that calls islapec. ndab must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shaec to
c            compute the coefficients a and b.
c
c
c   wshsec   an array which must be initialized by subroutine shseci.
c            once initialized, wshsec can be used repeatedly by
c            islapec as long as nlat and nlon  remain unchanged.
c            wshsec must not be altered between calls of islapec.
c
c   lshsec   the dimension of the array wshsec as it appears in the
c            program that calls islapec.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lsave must be greater than or equal to
c
c               2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls islapec. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 let
c
c               lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1).
c
c            if isym > 0 let
c
C*PL*ERROR* Comment line too long
c               lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     then lwork must be greater than or equal to lwkmin (see ierror=10)
c
c     **************************************************************
c
c     output parameters
c
c
c    sf      two or three dimensional arrays (see input parameter nt)
c            that contain the solution to either the helmholtz
c            (xlmbda>0.0) or poisson's equation. sf(i,j) is computed
c            at colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c   pertrb  a one dimensional array with nt elements (see input
c           parameter nt). in the discription that follows we assume
c           that nt=1. if xlmbda > 0.0 then pertrb=0.0 is always
c           returned because the helmholtz operator is invertible.
c           if xlmbda = 0.0 then a solution exists only if a(1,1)
c           is zero. islapec sets a(1,1) to zero. the resulting
c           solution sf(i,j) solves poisson's equation with
c           pertrb = a(1,1)/(2.*sqrt(2.)) subtracted from the
c           right side slap(i,j).
c
c
C*PL*ERROR* Comment line too long
c  ierror   a parameter which flags errors in input parameters as follows:
c
c            =-1  xlmbda is input negative (nonfatal error)
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of ids
c
c            = 6  error in the specification of jds
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lsave
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c
c     end of documentation for islapec
c
c **********************************************************************
c


      SUBROUTINE DISLAPEC(NLAT,NLON,ISYM,NT,XLMBDA,SF,IDS,JDS,A,B,MDAB,
     +                   NDAB,WSHSEC,LSHSEC,WORK,LWORK,PERTRB,IERROR)
      DOUBLE PRECISION XLMBDA
      DOUBLE PRECISION SF
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHSEC
      DOUBLE PRECISION WORK
      DOUBLE PRECISION PERTRB
      DIMENSION SF(IDS,JDS,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WSHSEC(LSHSEC),WORK(LWORK),PERTRB(NT),XLMBDA(NT)
c
c     check input parameters
c
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (ISYM.LT.0 .OR. ISYM.GT.2) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IMID = (NLAT+1)/2
      IF ((ISYM.EQ.0.AND.IDS.LT.NLAT) .OR.
     +    (ISYM.GT.0.AND.IDS.LT.IMID)) RETURN
      IERROR = 6
      IF (JDS.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT,NLON/2+1)
      IF (MDAB.LT.MMAX) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
c
c     set and verify saved work space length
c
c
      L1 = MIN0(NLAT, (NLON+2)/2)
      L2 = (NLAT+1)/2
      LWMIN = 2*NLAT*L2 + 3* ((L1-2)* (NLAT+NLAT-L1-1))/2 + NLON + 15
      IF (LSHSEC.LT.LWMIN) RETURN
      IERROR = 10
c
c     set and verify unsaved work space length
c
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
      MN = MMAX*NLAT*NT
c     lwmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwmin) return
      L2 = (NLAT+1)/2
      L1 = MIN0(NLAT,NLON/2+1)
      IF (ISYM.EQ.0) THEN
          LWKMIN = NLAT* (2*NT*NLON+MAX0(6*L2,NLON)+2*NT*L1+1)
      ELSE
          LWKMIN = L2* (2*NT*NLON+MAX0(6*NLAT,NLON)) + NLAT* (2*NT*L1+1)
      END IF
      IF (LWORK.LT.LWKMIN) RETURN
      IERROR = 0
c
c     check sign of xlmbda
c
      DO K = 1,NT
          IF (XLMBDA(K).LT.0.0D0) THEN
              IERROR = -1
          END IF
      END DO
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IFN = IB + MN
      IWK = IFN + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DISLPEC1(NLAT,NLON,ISYM,NT,XLMBDA,SF,IDS,JDS,A,B,MDAB,NDAB,
     +             WORK(IA),WORK(IB),MMAX,WORK(IFN),WSHSEC,LSHSEC,
     +             WORK(IWK),LWK,PERTRB,IERROR)
      RETURN
      END

      SUBROUTINE DISLPEC1(NLAT,NLON,ISYM,NT,XLMBDA,SF,IDS,JDS,A,B,MDAB,
     +                   NDAB,AS,BS,MMAX,FNN,WSHSEC,LSHSEC,WK,LWK,
     +                   PERTRB,IERROR)
      DOUBLE PRECISION XLMBDA
      DOUBLE PRECISION SF
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION AS
      DOUBLE PRECISION BS
      DOUBLE PRECISION FNN
      DOUBLE PRECISION WSHSEC
      DOUBLE PRECISION WK
      DOUBLE PRECISION PERTRB
      DOUBLE PRECISION FN
      DIMENSION SF(IDS,JDS,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION AS(MMAX,NLAT,NT),BS(MMAX,NLAT,NT),FNN(NLAT)
      DIMENSION WSHSEC(LSHSEC),WK(LWK),PERTRB(NT),XLMBDA(NT)
c
c     set multipliers and preset synthesis coefficients to zero
c
      DO N = 1,NLAT
          FN = DBLE(N-1)
          FNN(N) = FN* (FN+1.0D0)
          DO M = 1,MMAX
              DO K = 1,NT
                  AS(M,N,K) = 0.0D0
                  BS(M,N,K) = 0.0D0
              END DO
          END DO
      END DO

      DO K = 1,NT
c
c     compute synthesis coefficients for xlmbda zero or nonzero
c
          IF (XLMBDA(K).EQ.0.0D0) THEN
              DO N = 2,NLAT
                  AS(1,N,K) = -A(1,N,K)/FNN(N)
                  BS(1,N,K) = -B(1,N,K)/FNN(N)
              END DO
              DO M = 2,MMAX
                  DO N = M,NLAT
                      AS(M,N,K) = -A(M,N,K)/FNN(N)
                      BS(M,N,K) = -B(M,N,K)/FNN(N)
                  END DO
              END DO
          ELSE
c
c     xlmbda nonzero so operator invertible unless
c     -n*(n-1) = xlmbda(k) < 0.0  for some n
c
              PERTRB(K) = 0.0D0
              DO N = 1,NLAT
                  AS(1,N,K) = -A(1,N,K)/ (FNN(N)+XLMBDA(K))
                  BS(1,N,K) = -B(1,N,K)/ (FNN(N)+XLMBDA(K))
              END DO
              DO M = 2,MMAX
                  DO N = M,NLAT
                      AS(M,N,K) = -A(M,N,K)/ (FNN(N)+XLMBDA(K))
                      BS(M,N,K) = -B(M,N,K)/ (FNN(N)+XLMBDA(K))
                  END DO
              END DO
          END IF
      END DO
c
c     synthesize as,bs into sf
c
      CALL DSHSEC(NLAT,NLON,ISYM,NT,SF,IDS,JDS,AS,BS,MMAX,NLAT,WSHSEC,
     +           LSHSEC,WK,LWK,IERROR)
      RETURN
      END
