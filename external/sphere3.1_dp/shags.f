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
c ... file shags.f
c
c     this file contains code and documentation for subroutines
c     shags and shagsi
c
c ... files which must be loaded with shags.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine shags(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    1                    wshags,lshags,work,lwork,ierror)
c
c     subroutine shags performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on a gaussian grid in colatitude
c     and an equally spaced grid in longitude.  the associated
c     legendre functions are stored rather than recomputed as they
c     are in subroutine shagc.  the analysis is described below
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
c     nt     the number of analyses.  in the program that calls shags,
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
c            program that calls shags. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shags. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shags. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shags. ndab must be at least nlat
c
c     wshags an array which must be initialized by subroutine shagsi.
c            once initialized, wshags can be used repeatedly by shags
c            as long as nlat and nlon remain unchanged.  wshags must
c            not be altered between calls of shags.
c
c     lshags the dimension of the array wshags as it appears in the
c            program that calls shags. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshags must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shags. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c
c            if isym is zero then lwork must be at least
c
c                  nlat*nlon*(nt+1)
c
c            if isym is nonzero then lwork must be at least
c
c                  l2*nlon*(nt+1)
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shags. for isym=0, a(m,n) and
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
c            = 9  error in the specification of lshags
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shagsi(nlat,nlon,wshags,lshags,work,lwork,dwork,ldwork,
c    +                  ierror)
c
c     subroutine shagsi initializes the array wshags which can then
c     be used repeatedly by subroutines shags. it precomputes
c     and stores in wshags quantities such as gaussian weights,
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
c     wshags an array which must be initialized by subroutine shagsi.
c            once initialized, wshags can be used repeatedly by shags
c            as long as nlat and nlon remain unchanged.  wshags must
c            not be altered between calls of shags.
c
c     lshags the dimension of the array wshags as it appears in the
c            program that calls shags. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshags must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shagsi. lwork must be at least
c            4*nlat*(nlat+2)+2 in the routine calling shagsi
c
C*PL*ERROR* Comment line too long
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the length of dwork in the calling routine.  ldwork must
c             be at least nlat*(nlat+4)
c
c     output parameter
c
c     wshags an array which must be initialized before calling shags or
c            once initialized, wshags can be used repeatedly by shags or
c            as long as nlat and nlon remain unchanged.  wshags must not
c            altered between calls of shasc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshags
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c            = 6  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      SUBROUTINE DSHAGS(NLAT,NLON,MODE,NT,G,IDG,JDG,A,B,MDAB,NDAB,
     +                  WSHAGS,LSHAGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION G
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHAGS
      DOUBLE PRECISION WORK
c     subroutine shags performs the spherical harmonic analysis on
c     a gaussian grid on the array(s) in g and returns the coefficients
c     in array(s) a,b. the necessary legendre polynomials are fully
c     stored in this version.
c
      DIMENSION G(IDG,JDG,1),A(MDAB,NDAB,1),B(MDAB,NDAB,1),
     +          WSHAGS(LSHAGS),WORK(LWORK)
c     check input parameters
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (MODE.LT.0 .OR. MODE.GT.2) RETURN
c     set m limit for pmn
      L = MIN0((NLON+2)/2,NLAT)
c     set gaussian point nearest equator pointer
      LATE = (NLAT+MOD(NLAT,2))/2
c     set number of grid points for analysis/synthesis
      LAT = NLAT
      IF (MODE.NE.0) LAT = LATE
      IERROR = 4
      IF (NT.LT.1) RETURN
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
c
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHAGS.LT.LP) RETURN
      IERROR = 10
c     check temporary work space length
      IF (MODE.EQ.0 .AND. LWORK.LT.NLAT*NLON* (NT+1)) RETURN
      IF (MODE.NE.0 .AND. LWORK.LT.L2*NLON* (NT+1)) RETURN
      IERROR = 0
c     set starting address for gaussian wts ,fft values,
c     and fully stored legendre polys in wshags
      IWTS = 1
      IFFT = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) + 1
      IPMN = IFFT + NLON + 15
c     set pointer for internal storage of g
      IW = LAT*NLON*NT + 1
      CALL DSHAGS1(NLAT,NLON,L,LAT,MODE,G,IDG,JDG,NT,A,B,MDAB,NDAB,
     +             WSHAGS(IWTS),WSHAGS(IFFT),WSHAGS(IPMN),LATE,WORK,
     +             WORK(IW))
      RETURN
      END

      SUBROUTINE DSHAGS1(NLAT,NLON,L,LAT,MODE,GS,IDG,JDG,NT,A,B,MDAB,
     +                   NDAB,WTS,WFFT,PMN,LATE,G,WORK)
      DOUBLE PRECISION GS
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WTS
      DOUBLE PRECISION WFFT
      DOUBLE PRECISION PMN
      DOUBLE PRECISION G
      DOUBLE PRECISION WORK
      DOUBLE PRECISION SFN
      DOUBLE PRECISION T1
      DOUBLE PRECISION T2
      DIMENSION GS(IDG,JDG,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT),
     +          G(LAT,NLON,NT)
      DIMENSION WFFT(1),PMN(LATE,1),WTS(NLAT),WORK(1)
c     set gs array internally in shags1
      DO 100 K = 1,NT
          DO 100 J = 1,NLON
              DO 100 I = 1,LAT
                  G(I,J,K) = GS(I,J,K)
  100 CONTINUE

c     do fourier transform
      DO 101 K = 1,NT
          CALL DHRFFTF(LAT,NLON,G(1,1,K),LAT,WFFT,WORK)
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
c     correspond to
C*PL*ERROR* Comment line too long
c     g(i,1),g(i,2),g(i,3),g(i,4),g(i,5),...,g(i,2l-4),g(i,2l-3),g(i,2l-2)
c     whenever 2*l-2 = nlon exactly
c     initialize coefficients to zero
      DO 103 K = 1,NT
          DO 103 NP1 = 1,NLAT
              DO 103 MP1 = 1,L
                  A(MP1,NP1,K) = 0.0D0
                  B(MP1,NP1,K) = 0.0D0
  103 CONTINUE
c     set mp1 limit on b(mp1) calculation
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
          MP1 = 1
          M = 0
          MML1 = M* (2*NLAT-M-1)/2
          DO 106 K = 1,NT
              DO 106 I = 1,LATE
                  IS = NLAT - I + 1
                  DO 107 NP1 = 1,NLAT,2
c     n even
                      A(1,NP1,K) = A(1,NP1,K) + G(I,1,K)*PMN(I,MML1+NP1)
  107             CONTINUE
                  DO 108 NP1 = 2,NLAT,2
c     n odd
                      A(1,NP1,K) = A(1,NP1,K) +
     +                             G(IS,1,K)*PMN(I,MML1+NP1)
  108             CONTINUE
  106     CONTINUE
c     compute m.ge.1  coefficients next
          DO 109 MP1 = 2,LM1
              M = MP1 - 1
              MML1 = M* (2*NLAT-M-1)/2
              MP2 = MP1 + 1
              DO 110 K = 1,NT
                  DO 111 I = 1,LATE
                      IS = NLAT - I + 1
c     n-m even
                      DO 112 NP1 = MP1,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(I,2*M,K)*PMN(I,MML1+NP1)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(I,2*M+1,K)*PMN(I,MML1+NP1)
  112                 CONTINUE
c     n-m odd
                      DO 113 NP1 = MP2,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(IS,2*M,K)*PMN(I,MML1+NP1)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(IS,2*M+1,K)*PMN(I,MML1+NP1)
  113                 CONTINUE
  111             CONTINUE
  110         CONTINUE
  109     CONTINUE
          IF (NLON.EQ.L+L-2) THEN
c     compute m=l-1, n=l-1,l,...,nlat-1 coefficients
              M = L - 1
              MML1 = M* (2*NLAT-M-1)/2
              DO 114 K = 1,NT
                  DO 114 I = 1,LATE
                      IS = NLAT - I + 1
                      DO 124 NP1 = L,NLAT,2
                          MN = MML1 + NP1
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(I,NLON,K)*PMN(I,MN)
  124                 CONTINUE
c     n-m  odd
                      LP1 = L + 1
                      DO 125 NP1 = LP1,NLAT,2
                          MN = MML1 + NP1
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(IS,NLON,K)*PMN(I,MN)
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
          MP1 = 1
          M = 0
          MML1 = M* (2*NLAT-M-1)/2
          MS = 1
          IF (MODE.EQ.1) MS = 2
          DO 117 K = 1,NT
              DO 117 I = 1,LATE
                  DO 117 NP1 = MS,NLAT,2
                      A(1,NP1,K) = A(1,NP1,K) + G(I,1,K)*PMN(I,MML1+NP1)
  117     CONTINUE

c     compute m.ge.1  coefficients next
          DO 118 MP1 = 2,LM1
              M = MP1 - 1
              MML1 = M* (2*NLAT-M-1)/2
              MS = MP1
              IF (MODE.EQ.1) MS = MP1 + 1
              DO 119 K = 1,NT
                  DO 119 I = 1,LATE
                      DO 119 NP1 = MS,NLAT,2
                          A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                                   G(I,2*M,K)*PMN(I,MML1+NP1)
                          B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                                   G(I,2*M+1,K)*PMN(I,MML1+NP1)
  119         CONTINUE
  118     CONTINUE

          IF (NLON.EQ.L+L-2) THEN
c     compute n=m=l-1 coefficients last
              M = L - 1
              MML1 = M* (2*NLAT-M-1)/2
c     set starting n for mode even
              NS = L
c     set starting n for mode odd
              IF (MODE.EQ.1) NS = L + 1
              DO 120 K = 1,NT
                  DO 120 I = 1,LATE
                      DO 120 NP1 = NS,NLAT,2
                          MN = MML1 + NP1
                          A(L,NP1,K) = A(L,NP1,K) +
     +                                 0.5D0*G(I,NLON,K)*PMN(I,MN)
  120         CONTINUE
          END IF

      END IF

      RETURN
      END
      SUBROUTINE DSHAGSI(NLAT,NLON,WSHAGS,LSHAGS,WORK,LWORK,DWORK,
     +                   LDWORK,IERROR)
      DOUBLE PRECISION WSHAGS
      DOUBLE PRECISION WORK
c
c     this subroutine must be called before calling shags or shsgs with
c     fixed nlat,nlon. it precomputes the gaussian weights, points
c     and all necessary legendre polys and stores them in wshags.
c     these quantities must be preserved when calling shags or shsgs
c     repeatedly with fixed nlat,nlon.  dwork must be of length at
c     least nlat*(nlat+4) in the routine calling shagsi.  This is
c     not checked.  undetectable errors will result if dwork is
c     smaller than nlat*(nlat+4).
c
      DIMENSION WSHAGS(LSHAGS),WORK(LWORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
c     set triangular truncation limit for spherical harmonic basis
      L = MIN0((NLON+2)/2,NLAT)
c     set equator or nearest point (if excluded) pointer
      LATE = (NLAT+1)/2
      L1 = L
      L2 = LATE
c     check permanent work space length
      IERROR = 3
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHAGS.LT.LP) RETURN
      IERROR = 4
c     check temporary work space
      IF (LWORK.LT.4*NLAT* (NLAT+2)+2) RETURN
      IERROR = 5
c     check double precision temporary space
      IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
      IERROR = 0
C*PL*ERROR* Comment line too long
c     set preliminary quantites needed to compute and store legendre polys
      LDW = NLAT* (NLAT+4)
      CALL DSHAGSP(NLAT,NLON,WSHAGS,LSHAGS,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) RETURN
c     set legendre poly pointer in wshags
      IPMNF = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) +
     +        NLON + 16
      CALL DSHAGSS1(NLAT,L,LATE,WSHAGS,WORK,WSHAGS(IPMNF))
      RETURN
      END
      SUBROUTINE DSHAGSS1(NLAT,L,LATE,W,PMN,PMNF)
      DOUBLE PRECISION W
      DOUBLE PRECISION PMN
      DOUBLE PRECISION PMNF
      DIMENSION W(1),PMN(NLAT,LATE,3),PMNF(LATE,1)
c     compute and store legendre polys for i=1,...,late,m=0,...,l-1
c     and n=m,...,l-1
      DO I = 1,NLAT
          DO J = 1,LATE
              DO K = 1,3
                  PMN(I,J,K) = 0.0D0
              END DO
          END DO
      END DO
      DO 100 MP1 = 1,L
          M = MP1 - 1
          MML1 = M* (2*NLAT-M-1)/2
c     compute pmn for n=m,...,nlat-1 and i=1,...,(l+1)/2
          MODE = 0
          CALL DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
c     store above in pmnf
          DO 101 NP1 = MP1,NLAT
              MN = MML1 + NP1
              DO 102 I = 1,LATE
                  PMNF(I,MN) = PMN(NP1,I,KM)
  102         CONTINUE
  101     CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE DSHAGSP(NLAT,NLON,WSHAGS,LSHAGS,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHAGS
      DIMENSION WSHAGS(LSHAGS)
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
      IF (LSHAGS.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
      IERROR = 4
c     if (lwork.lt.4*nlat*(nlat+2)+2) return
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
c     idwts = idth+2*nlat
c     iw = idwts+2*nlat
      IDWTS = IDTH + NLAT
      IW = IDWTS + NLAT
      CALL DSHAGSP1(NLAT,NLON,L,LATE,WSHAGS(I1),WSHAGS(I2),WSHAGS(I3),
     +              WSHAGS(I4),WSHAGS(I5),WSHAGS(I6),WSHAGS(I7),
     +              DWORK(IDTH),DWORK(IDWTS),DWORK(IW),IERROR)
      IF (IERROR.NE.0) IERROR = 6
      RETURN
      END

      SUBROUTINE DSHAGSP1(NLAT,NLON,L,LATE,WTS,P0N,P1N,ABEL,BBEL,CBEL,
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
     +          CBEL(1),WFFT(1),DTHETA(NLAT),DWTS(NLAT)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PB,DTHETA,DWTS,WORK(*)

      INDX(M,N) = (N-1)* (N-2)/2 + M - 1
      IMNDX(M,N) = L* (L-1)/2 + (N-L-1)* (L-1) + M - 1
      CALL DHRFFTI(NLON,WFFT)

c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+2)
      LW = NLAT* (NLAT+2)
      CALL DGAQD(NLAT,DTHETA,DWTS,WORK,LW,IER)
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
c
c     compute and store swarztrauber recursion coefficients
c     for 2.le.m.le.n and 2.le.n.le.nlat in abel,bbel,cbel
      DO 107 N = 2,NLAT
          FN = DBLE(N)
          MLIM = MIN0(N,L)
          DO 107 M = 2,MLIM
              FM = DBLE(M)
              IMN = INDX(M,N)
              IF (N.GE.L) IMN = IMNDX(M,N)
              ABEL(IMN) = SQRT(((2*FN+1)* (FM+FN-2)* (FM+FN-3))/
     +                    (((2*FN-3)* (FM+FN-1)* (FM+FN))))
              BBEL(IMN) = SQRT(((2*FN+1)* (FN-FM-1)* (FN-FM))/
     +                    (((2*FN-3)* (FM+FN-1)* (FM+FN))))
              CBEL(IMN) = SQRT(((FN-FM+1)* (FN-FM+2))/
     +                    (((FN+FM-1)* (FN+FM))))
  107 CONTINUE
      RETURN
      END
