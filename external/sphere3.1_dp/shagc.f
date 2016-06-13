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
c ... file shagc.f
c
c     this file contains code and documentation for subroutines
c     shagc and shagci
c
c ... files which must be loaded with shagc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c
c     subroutine shagc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshagc,lshagc,work,lwork,ierror)
c
c     subroutine shagc performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on a gaussian grid in colatitude
c     and an equally spaced grid in longitude.  the associated
c     legendre functions are recomputed rather than stored as they
c     are in subroutine shags.  the analysis is described below
c     at output parameters a,b.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shagc,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the gaussian
c            point theta(i) and longitude point phi(j) = (j-1)*2*pi/nlon
c            the index ranges are defined above at the input parameter
c            isym.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shagc. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shagc. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shagc. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc.
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                      nlat*(nlon*nt+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                      l2*(nlon*nt+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shagc. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta).
c
c     2. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon of
c                       g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon of
c                       g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c
c     3. the gaussian points and weights on the sphere
c        (computed by subroutine gaqd).
c
c        theta(1),...,theta(nlat) (gaussian pts in radians)
c        wts(1),...,wts(nlat) (corresponding gaussian weights)
c
c     4. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)     =  the sum from i=1 to i=nlat of
c                       c(m+1,i)*wts(i)*pbar(m,n,theta(i))
c
c     b(m+1,n+1)      = the sum from i=1 to nlat of
c                       s(m+1,i)*wts(i)*pbar(m,n,theta(i))
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshagc
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
c
c     subroutine shagci initializes the array wshagc which can then
c     be used repeatedly by subroutines shagc. it precomputes
c     and stores in wshagc quantities such as gaussian weights,
c     legendre polynomial coefficients, and fft trigonometric tables.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
C*PL*ERROR* Comment line too long
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the dimension of the array dwork as it appears in the
c            program that calls shagci. ldwork must be at least
c
c                nlat*(nlat+4)
c
c     output parameter
c
c     wshagc an array which must be initialized before calling shagc or
c            once initialized, wshagc can be used repeatedly by shagc or
c            as long as nlat and nlon remain unchanged.  wshagc must not
c            altered between calls of shagc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshagc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      SUBROUTINE DSHAGC(NLAT,NLON,ISYM,NT,G,IDG,JDG,A,B,MDAB,NDAB,
     +                  WSHAGC,LSHAGC,WORK,LWORK,IERROR)
      DOUBLE PRECISION G
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHAGC
      DOUBLE PRECISION WORK
c     subroutine shagc performs the spherical harmonic analysis on
c     a gaussian grid on the array(s) in g and returns the coefficients
c     in array(s) a,b. the necessary legendre polynomials are computed
c     as needed in this version.
c
      DIMENSION G(IDG,JDG,1),A(MDAB,NDAB,1),B(MDAB,NDAB,1),
     +          WSHAGC(LSHAGC),WORK(LWORK)
c     check input parameters
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (ISYM.LT.0 .OR. ISYM.GT.2) RETURN
      IERROR = 4
      IF (NT.LT.1) RETURN
c     set upper limit on m for spherical harmonic basis
      L = MIN0((NLON+2)/2,NLAT)
c     set gaussian point nearest equator pointer
      LATE = (NLAT+MOD(NLAT,2))/2
c     set number of grid points for analysis/synthesis
      LAT = NLAT
      IF (ISYM.NE.0) LAT = LATE
      IERROR = 5
      IF (IDG.LT.LAT) RETURN
      IERROR = 6
      IF (JDG.LT.NLON) RETURN
      IERROR = 7
      IF (MDAB.LT.L) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      L1 = L
      L2 = LATE
      IERROR = 9
c     check permanent work space length
      IF (LSHAGC.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
      IERROR = 10
c     check temporary work space length
      IF (ISYM.EQ.0) THEN
          IF (LWORK.LT.NLAT* (NLON*NT+MAX0(3*L2,NLON))) RETURN
      ELSE
c     isym.ne.0
          IF (LWORK.LT.L2* (NLON*NT+MAX0(3*NLAT,NLON))) RETURN
      END IF
      IERROR = 0
c     starting address for gaussian wts in shigc and fft values
      IWTS = 1
      IFFT = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) + 1
c     set pointers for internal storage of g and legendre polys
      IPMN = LAT*NLON*NT + 1
      CALL DSHAGC1(NLAT,NLON,L,LAT,ISYM,G,IDG,JDG,NT,A,B,MDAB,NDAB,
     +             WSHAGC,WSHAGC(IWTS),WSHAGC(IFFT),LATE,WORK(IPMN),
     +             WORK)
      RETURN
      END
      SUBROUTINE DSHAGC1(NLAT,NLON,L,LAT,MODE,GS,IDG,JDG,NT,A,B,MDAB,
     +                  NDAB,W,WTS,WFFT,LATE,PMN,G)
      DOUBLE PRECISION GS
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION W
      DOUBLE PRECISION WTS
      DOUBLE PRECISION WFFT
      DOUBLE PRECISION PMN
      DOUBLE PRECISION G
      DOUBLE PRECISION SFN
      DOUBLE PRECISION T1
      DOUBLE PRECISION T2
      DIMENSION GS(IDG,JDG,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT),
     +          G(LAT,NLON,NT)
      DIMENSION W(1),WTS(NLAT),WFFT(1),PMN(NLAT,LATE,3)
c     set gs array internally in shagc1
      DO 100 K = 1,NT
          DO 100 J = 1,NLON
              DO 100 I = 1,LAT
                  G(I,J,K) = GS(I,J,K)
  100 CONTINUE
c     do fourier transform
      DO 101 K = 1,NT
          CALL DHRFFTF(LAT,NLON,G(1,1,K),LAT,WFFT,PMN)
  101 CONTINUE
c     scale result
      SFN = 2.0D0/DBLE(NLON)
      DO 102 K = 1,NT
          DO 102 J = 1,NLON
              DO 102 I = 1,LAT
                  G(I,J,K) = SFN*G(I,J,K)
  102 CONTINUE
c     compute using gaussian quadrature
c     a(n,m) = s (ga(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     b(n,m) = s (gb(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     here ga,gb are the cos(phi),sin(phi) coefficients of
c     the fourier expansion of g(theta,phi) in phi.  as a result
c     of the above fourier transform they are stored in array
c     g as follows:
c     for each theta(i) and k= l-1
c     ga(0),ga(1),gb(1),ga(2),gb(2),...,ga(k-1),gb(k-1),ga(k)
c     correspond to (in the case nlon=l+l-2)
c     g(i,1),g(i,2),g(i,3),g(i,4),g(i,5),...,g(i,2l-4),g(i,2l-3),g(i,2l-
c     initialize coefficients to zero
      DO 103 K = 1,NT
          DO 103 NP1 = 1,NLAT
              DO 103 MP1 = 1,L
                  A(MP1,NP1,K) = 0.0D0
                  B(MP1,NP1,K) = 0.0D0
  103 CONTINUE
c     set m+1 limit on b(m+1) calculation
      LM1 = L
      IF (NLON.EQ.L+L-2) LM1 = L - 1
      IF (MODE.EQ.0) THEN
c     for full sphere (mode=0) and even/odd reduction:
c     overwrite g(i) with (g(i)+g(nlat-i+1))*wts(i)
c     overwrite g(nlat-i+1) with (g(i)-g(nlat-i+1))*wts(i)
          NL2 = NLAT/2
          DO 104 K = 1,NT
              DO 104 J = 1,NLON
                  DO 105 I = 1,NL2
                      IS = NLAT - I + 1
                      T1 = G(I,J,K)
                      T2 = G(IS,J,K)
                      G(I,J,K) = WTS(I)* (T1+T2)
                      G(IS,J,K) = WTS(I)* (T1-T2)
  105             CONTINUE
c     adjust equator if necessary(nlat odd)
                  IF (MOD(NLAT,2).NE.0) G(LATE,J,K) = WTS(LATE)*
     +                G(LATE,J,K)
  104     CONTINUE
c     set m = 0 coefficients first
          M = 0
          CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
          DO 106 K = 1,NT
              DO 106 I = 1,LATE
                  IS = NLAT - I + 1
                  DO 107 NP1 = 1,NLAT,2
c     n even
                      A(1,NP1,K) = A(1,NP1,K) + G(I,1,K)*PMN(NP1,I,KM)
  107             CONTINUE
                  DO 108 NP1 = 2,NLAT,2
c     n odd
                      A(1,NP1,K) = A(1,NP1,K) + G(IS,1,K)*PMN(NP1,I,KM)
  108             CONTINUE
  106     CONTINUE
c     compute coefficients for which b(m,n) is available
          DO 109 MP1 = 2,LM1
              M = MP1 - 1
              MP2 = M + 2
c     compute pmn for all i and n=m,...,l-1
              CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
              DO 110 K = 1,NT
                  DO 111 I = 1,LATE
                      IS = NLAT - I + 1
c     n-m even
                      DO 112 NP1 = MP1,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(I,2*M,K)*PMN(NP1,I,KM)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(I,2*M+1,K)*PMN(NP1,I,KM)
  112                 CONTINUE
c     n-m odd
                      DO 113 NP1 = MP2,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(IS,2*M,K)*PMN(NP1,I,KM)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(IS,2*M+1,K)*PMN(NP1,I,KM)
  113                 CONTINUE
  111             CONTINUE
  110         CONTINUE
  109     CONTINUE
          IF (NLON.EQ.L+L-2) THEN
c     compute a(l,np1) coefficients only
              M = L - 1
              CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
              DO 114 K = 1,NT
                  DO 114 I = 1,LATE
                      IS = NLAT - I + 1
c     n-m even
                      DO 124 NP1 = L,NLAT,2
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(I,NLON,K)*PMN(NP1,I,KM)
  124                 CONTINUE
                      LP1 = L + 1
c     n-m odd
                      DO 125 NP1 = LP1,NLAT,2
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(IS,NLON,K)*PMN(NP1,I,KM)
  125                 CONTINUE
  114         CONTINUE
          END IF
      ELSE
c     half sphere
c     overwrite g(i) with wts(i)*(g(i)+g(i)) for i=1,...,nlate/2
          NL2 = NLAT/2
          DO 116 K = 1,NT
              DO 116 J = 1,NLON
                  DO 115 I = 1,NL2
                      G(I,J,K) = WTS(I)* (G(I,J,K)+G(I,J,K))
  115             CONTINUE
c     adjust equator separately if a grid point
                  IF (NL2.LT.LATE) G(LATE,J,K) = WTS(LATE)*G(LATE,J,K)
  116     CONTINUE
c     set m = 0 coefficients first
          M = 0
          CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
          MS = 1
          IF (MODE.EQ.1) MS = 2
          DO 117 K = 1,NT
              DO 117 I = 1,LATE
                  DO 117 NP1 = MS,NLAT,2
                      A(1,NP1,K) = A(1,NP1,K) + G(I,1,K)*PMN(NP1,I,KM)
  117     CONTINUE
c     compute coefficients for which b(m,n) is available
          DO 118 MP1 = 2,LM1
              M = MP1 - 1
              MS = MP1
              IF (MODE.EQ.1) MS = MP1 + 1
c     compute pmn for all i and n=m,...,nlat-1
              CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
              DO 119 K = 1,NT
                  DO 119 I = 1,LATE
                      DO 119 NP1 = MS,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(I,2*M,K)*PMN(NP1,I,KM)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(I,2*M+1,K)*PMN(NP1,I,KM)
  119         CONTINUE
  118     CONTINUE
          IF (NLON.EQ.L+L-2) THEN
c     compute coefficient a(l,np1) only
              M = L - 1
              CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
              NS = L
              IF (MODE.EQ.1) NS = L + 1
              DO 120 K = 1,NT
                  DO 120 I = 1,LATE
                      DO 120 NP1 = NS,NLAT,2
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(I,NLON,K)*PMN(NP1,I,KM)
  120         CONTINUE
          END IF
      END IF
      RETURN
      END
      SUBROUTINE DSHAGCI(NLAT,NLON,WSHAGC,LSHAGC,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHAGC
c     this subroutine must be called before calling shagc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      DIMENSION WSHAGC(LSHAGC)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
c     set triangular truncation limit for spherical harmonic basis
      L = MIN0((NLON+2)/2,NLAT)
c     set equator or nearest point (if excluded) pointer
      LATE = (NLAT+MOD(NLAT,2))/2
      L1 = L
      L2 = LATE
      IERROR = 3
c     check permanent work space length
      IF (LSHAGC.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
      IERROR = 4
      IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
      IERROR = 0
c     set pointers
      I1 = 1
      I2 = I1 + NLAT
      I3 = I2 + NLAT*LATE
      I4 = I3 + NLAT*LATE
      I5 = I4 + L* (L-1)/2 + (NLAT-L)* (L-1)
      I6 = I5 + L* (L-1)/2 + (NLAT-L)* (L-1)
      I7 = I6 + L* (L-1)/2 + (NLAT-L)* (L-1)
c     set indices in temp work for double precision gaussian wts and pts
      IDTH = 1
      IDWTS = IDTH + NLAT
      IW = IDWTS + NLAT
      CALL DSHAGCI1(NLAT,NLON,L,LATE,WSHAGC(I1),WSHAGC(I2),WSHAGC(I3),
     +              WSHAGC(I4),WSHAGC(I5),WSHAGC(I6),WSHAGC(I7),
     +              DWORK(IDTH),DWORK(IDWTS),DWORK(IW),IERROR)
      IF (IERROR.NE.0) IERROR = 5
      RETURN
      END
      SUBROUTINE DSHAGCI1(NLAT,NLON,L,LATE,WTS,P0N,P1N,ABEL,BBEL,CBEL,
     +                    WFFT,DTHETA,DWTS,WORK,IER)
      DOUBLE PRECISION WTS
      DOUBLE PRECISION P0N
      DOUBLE PRECISION P1N
      DOUBLE PRECISION ABEL
      DOUBLE PRECISION BBEL
      DOUBLE PRECISION CBEL
      DOUBLE PRECISION WFFT
      DOUBLE PRECISION FN
      DOUBLE PRECISION FM
      DIMENSION WTS(NLAT),P0N(NLAT,LATE),P1N(NLAT,LATE),ABEL(1),BBEL(1),
     +          CBEL(1),WFFT(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PB,DTHETA(NLAT),DWTS(NLAT),WORK(*),W
c     compute the nlat  gaussian points and weights, the
c     m=0,1 legendre polys for gaussian points and all n,
c     and the legendre recursion coefficients
c     define index function used in storing
c     arrays for recursion coefficients (functions of (m,n))
c     the index function indx(m,n) is defined so that
c     the pairs (m,n) map to [1,2,...,indx(l-1,l-1)] with no
c     "holes" as m varies from 2 to n and n varies from 2 to l-1.
c     (m=0,1 are set from p0n,p1n for all n)
c     define for 2.le.n.le.l-1
      INDX(M,N) = (N-1)* (N-2)/2 + M - 1
c     define index function for l.le.n.le.nlat
      IMNDX(M,N) = L* (L-1)/2 + (N-L-1)* (L-1) + M - 1
c     preset quantites for fourier transform
      CALL DHRFFTI(NLON,WFFT)
c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+1)+2
      LW = NLAT* (NLAT+2)
      CALL DGAQD(NLAT,DTHETA,DWTS,W,LW,IER)
      IF (IER.NE.0) RETURN
c     store gaussian weights single precision to save computation
c     in inner loops in analysis
      DO 100 I = 1,NLAT
          WTS(I) = DWTS(I)
  100 CONTINUE
c     initialize p0n,p1n using double precision dnlfk,dnlft
      DO 101 NP1 = 1,NLAT
          DO 101 I = 1,LATE
              P0N(NP1,I) = 0.0D0
              P1N(NP1,I) = 0.0D0
  101 CONTINUE
c     compute m=n=0 legendre polynomials for all theta(i)
      NP1 = 1
      N = 0
      M = 0
      CALL DDNLFK(M,N,WORK)
      DO 103 I = 1,LATE
          CALL DDNLFT(M,N,DTHETA(I),WORK,PB)
          P0N(1,I) = PB
  103 CONTINUE
c     compute p0n,p1n for all theta(i) when n.gt.0
      DO 104 NP1 = 2,NLAT
          N = NP1 - 1
          M = 0
          CALL DDNLFK(M,N,WORK)
          DO 105 I = 1,LATE
              CALL DDNLFT(M,N,DTHETA(I),WORK,PB)
              P0N(NP1,I) = PB
  105     CONTINUE
c     compute m=1 legendre polynomials for all n and theta(i)
          M = 1
          CALL DDNLFK(M,N,WORK)
          DO 106 I = 1,LATE
              CALL DDNLFT(M,N,DTHETA(I),WORK,PB)
              P1N(NP1,I) = PB
  106     CONTINUE
  104 CONTINUE
c     compute and store swarztrauber recursion coefficients
c     for 2.le.m.le.n and 2.le.n.le.nlat in abel,bbel,cbel
      DO 107 N = 2,NLAT
          FN = DBLE(N)
          MLIM = MIN0(N,L)
          DO 107 M = 2,MLIM
              FM = DBLE(M)
              IMN = INDX(M,N)
              IF (N.GE.L) IMN = IMNDX(M,N)
              ABEL(IMN) = DSQRT(((2*FN+1)* (FM+FN-2)* (FM+FN-3))/
     +                    (((2*FN-3)* (FM+FN-1)* (FM+FN))))
              BBEL(IMN) = DSQRT(((2*FN+1)* (FN-FM-1)* (FN-FM))/
     +                    (((2*FN-3)* (FM+FN-1)* (FM+FN))))
              CBEL(IMN) = DSQRT(((FN-FM+1)* (FN-FM+2))/
     +                    (((FN+FM-1)* (FN+FM))))
  107 CONTINUE
      RETURN
      END
