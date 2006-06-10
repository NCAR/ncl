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
c ... file shsgs.f
c
c     this file contains code and documentation for subroutines
c     shsgs and shsgsi
c
c ... files which must be loaded with shsgs.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine shsgs(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    1                    wshsgs,lshsgs,work,lwork,ierror)
c
c     subroutine shsgs performs the spherical harmonic synthesis
c     on the arrays a and b and stores the result in the array g.
c     the synthesis is performed on an equally spaced longitude grid
c     and a gaussian colatitude grid.  the associated legendre functions
c     are stored rather than recomputed as they are in subroutine
c     shsgc.  the synthesis is described below at output parameter
c     g.
c
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
c     isym   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the synthesis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the synthesis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls shsgs,
c            the arrays g,a and b can be three dimensional in which
c            case multiple synthesis will be performed.  the third
c            index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
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
c     a,b    two or three dimensional arrays (see the input parameter
c            nt) that contain the coefficients in the spherical harmonic
c            expansion of g(i,j) given below at the definition of the
c            output parameter g.  a(m,n) and b(m,n) are defined for
c            indices m=1,...,mmax and n=m,...,nlat where mmax is the
c            maximum (plus one) longitudinal wave number given by
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shsgs. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shsgs. ndab must be at least nlat
c
c     wshsgs an array which must be initialized by subroutine shsgsi.
c            once initialized, wshsgs can be used repeatedly by shsgs
c            as long as nlat and nlon remain unchanged.  wshsgs must
c            not be altered between calls of shsgs.
c
c     lshsgs the dimension of the array wshsgs as it appears in the
c            program that calls shsgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shsgs. define
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
c
c     **************************************************************
c
c     output parameters
c
c     g      a two or three dimensional array (see input parameter nt)
c            that contains the discrete function which is synthesized.
c            g(i,j) contains the value of the function at the gaussian
c            colatitude point theta(i) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.  for isym=0, g(i,j)
c            is given by the the equations listed below.  symmetric
c            versions are used when isym is greater than zero.
c
c     the normalized associated legendre functions are given by
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     define the maximum (plus one) longitudinal wave number
c     as   mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c          mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then g(i,j) = the sum from n=0 to n=nlat-1 of
c
c                   .5*pbar(0,n,theta(i))*a(1,n+1)
c
c              plus the sum from m=1 to m=mmax-1 of
c
c                   the sum from n=m to n=nlat-1 of
c
c              pbar(m,n,theta(i))*(a(m+1,n+1)*cos(m*phi(j))
c                                    -b(m+1,n+1)*sin(m*phi(j)))
c
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
c            = 9  error in the specification of lshsgs
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shsgsi(nlat,nlon,wshsgs,lshsgs,work,lwork,dwork,ldwork,
c    +                  ierror)
c
c     subroutine shsgsi initializes the array wshsgs which can then
c     be used repeatedly by subroutines shsgs. it precomputes
c     and stores in wshsgs quantities such as gaussian weights,
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
c     wshsgs an array which must be initialized by subroutine shsgsi.
c            once initialized, wshsgs can be used repeatedly by shsgs
c            as long as nlat and nlon remain unchanged.  wshsgs must
c            not be altered between calls of shsgs.
c
c     lshsgs the dimension of the array wshsgs as it appears in the
c            program that calls shsgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shsgsi. lwork must be at least
c            4*nlat*(nlat+2)+2 in the routine calling shsgsi
c
C*PL*ERROR* Comment line too long
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the length of dwork in the calling routine.  ldwork must
c             be at least nlat*(nlat+4)
c
c     output parameter
c
c     wshsgs an array which must be initialized before calling shsgs or
c            once initialized, wshsgs can be used repeatedly by shsgs or
c            as long as nlat and nlon remain unchanged.  wshsgs must not
c            altered between calls of shsgs.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshsgs
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
      SUBROUTINE DSHSGS(NLAT,NLON,MODE,NT,G,IDG,JDG,A,B,MDAB,NDAB,
     +                  WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION G
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK
      DIMENSION G(IDG,JDG,1),A(MDAB,NDAB,1),B(MDAB,NDAB,1),
     +          WSHSGS(LSHSGS),WORK(LWORK)
c     check input parameters
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (MODE.LT.0 .OR. MODE.GT.2) RETURN
      IERROR = 4
      IF (NT.LT.1) RETURN
c     set limit on m subscript
      L = MIN0((NLON+2)/2,NLAT)
c     set gaussian point nearest equator pointer
      LATE = (NLAT+MOD(NLAT,2))/2
c     set number of grid points for analysis/synthesis
      LAT = NLAT
      IF (MODE.NE.0) LAT = LATE
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
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
c     check temporary work space length
      IERROR = 10
      IF (MODE.EQ.0 .AND. LWORK.LT.NLAT*NLON* (NT+1)) RETURN
      IF (MODE.NE.0 .AND. LWORK.LT.L2*NLON* (NT+1)) RETURN
      IERROR = 0
c     starting address for fft values and legendre polys in wshsgs
      IFFT = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) + 1
      IPMN = IFFT + NLON + 15
c     set pointer for internal storage of g
      IW = LAT*NLON*NT + 1
      CALL DSHSGS1(NLAT,NLON,L,LAT,MODE,G,IDG,JDG,NT,A,B,MDAB,NDAB,
     +             WSHSGS(IFFT),WSHSGS(IPMN),LATE,WORK,WORK(IW))
      RETURN
      END

      SUBROUTINE DSHSGS1(NLAT,NLON,L,LAT,MODE,GS,IDG,JDG,NT,A,B,MDAB,
     +                   NDAB,WFFT,PMN,LATE,G,WORK)
      DOUBLE PRECISION GS
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WFFT
      DOUBLE PRECISION PMN
      DOUBLE PRECISION G
      DOUBLE PRECISION WORK
      DOUBLE PRECISION T1
      DOUBLE PRECISION T3
      DOUBLE PRECISION T2
      DOUBLE PRECISION T4
      DIMENSION GS(IDG,JDG,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WFFT(1),PMN(LATE,1),G(LAT,NLON,NT),WORK(1)

c     reconstruct fourier coefficients in g on gaussian grid
c     using coefficients in a,b

c     initialize to zero
      DO 100 K = 1,NT
          DO 100 J = 1,NLON
              DO 100 I = 1,LAT
                  G(I,J,K) = 0.0D0
  100 CONTINUE

      LM1 = L
      IF (NLON.EQ.L+L-2) LM1 = L - 1
      IF (MODE.EQ.0) THEN
c     set first column in g
          M = 0
          MML1 = M* (2*NLAT-M-1)/2
          DO 101 K = 1,NT
c     n even
              DO 102 NP1 = 1,NLAT,2
                  MN = MML1 + NP1
                  DO 102 I = 1,LATE
                      G(I,1,K) = G(I,1,K) + A(1,NP1,K)*PMN(I,MN)
  102         CONTINUE
c     n odd
              NL2 = NLAT/2
              DO 103 NP1 = 2,NLAT,2
                  MN = MML1 + NP1
                  DO 103 I = 1,NL2
                      IS = NLAT - I + 1
                      G(IS,1,K) = G(IS,1,K) + A(1,NP1,K)*PMN(I,MN)
  103         CONTINUE
  101     CONTINUE

c     restore m=0 coefficients from odd/even
          DO 112 K = 1,NT
              DO 112 I = 1,NL2
                  IS = NLAT - I + 1
                  T1 = G(I,1,K)
                  T3 = G(IS,1,K)
                  G(I,1,K) = T1 + T3
                  G(IS,1,K) = T1 - T3
  112     CONTINUE

c     sweep interior columns of g
          DO 104 MP1 = 2,LM1
              M = MP1 - 1
              MML1 = M* (2*NLAT-M-1)/2
              MP2 = M + 2
              DO 105 K = 1,NT
C*PL*ERROR* Comment line too long
c     for n-m even store (g(i,p,k)+g(nlat-i+1,p,k))/2 in g(i,p,k) p=2*m,2*m+1
c     for i=1,...,late
                  DO 106 NP1 = MP1,NLAT,2
                      MN = MML1 + NP1
                      DO 107 I = 1,LATE
                          G(I,2*M,K) = G(I,2*M,K) +
     +                                 A(MP1,NP1,K)*PMN(I,MN)
                          G(I,2*M+1,K) = G(I,2*M+1,K) +
     +                                   B(MP1,NP1,K)*PMN(I,MN)
  107                 CONTINUE
  106             CONTINUE

c     for n-m odd store g(i,p,k)-g(nlat-i+1,p,k) in g(nlat-i+1,p,k)
c     for i=1,...,nlat/2 (p=2*m,p=2*m+1)
                  DO 108 NP1 = MP2,NLAT,2
                      MN = MML1 + NP1
                      DO 109 I = 1,NL2
                          IS = NLAT - I + 1
                          G(IS,2*M,K) = G(IS,2*M,K) +
     +                                  A(MP1,NP1,K)*PMN(I,MN)
                          G(IS,2*M+1,K) = G(IS,2*M+1,K) +
     +                                    B(MP1,NP1,K)*PMN(I,MN)
  109                 CONTINUE
  108             CONTINUE

c     now set fourier coefficients using even-odd reduction above
                  DO 110 I = 1,NL2
                      IS = NLAT - I + 1
                      T1 = G(I,2*M,K)
                      T2 = G(I,2*M+1,K)
                      T3 = G(IS,2*M,K)
                      T4 = G(IS,2*M+1,K)
                      G(I,2*M,K) = T1 + T3
                      G(I,2*M+1,K) = T2 + T4
                      G(IS,2*M,K) = T1 - T3
                      G(IS,2*M+1,K) = T2 - T4
  110             CONTINUE

  105         CONTINUE
  104     CONTINUE

c     set last column (using a only) if necessary
          IF (NLON.EQ.L+L-2) THEN
              M = L - 1
              MML1 = M* (2*NLAT-M-1)/2
              DO 111 K = 1,NT
c     n-m even
                  DO 131 NP1 = L,NLAT,2
                      MN = MML1 + NP1
                      DO 131 I = 1,LATE
                          G(I,NLON,K) = G(I,NLON,K) +
     +                                  2.0D0*A(L,NP1,K)*PMN(I,MN)

  131             CONTINUE
                  LP1 = L + 1
c     n-m odd
                  DO 132 NP1 = LP1,NLAT,2
                      MN = MML1 + NP1
                      DO 132 I = 1,NL2
                          IS = NLAT - I + 1
                          G(IS,NLON,K) = G(IS,NLON,K) +
     +                                   2.0D0*A(L,NP1,K)*PMN(I,MN)
  132             CONTINUE
                  DO 133 I = 1,NL2
                      IS = NLAT - I + 1
                      T1 = G(I,NLON,K)
                      T3 = G(IS,NLON,K)
                      G(I,NLON,K) = T1 + T3
                      G(IS,NLON,K) = T1 - T3
  133             CONTINUE
  111         CONTINUE
          END IF

      ELSE
c     half sphere (mode.ne.0)
c     set first column in g
          M = 0
          MML1 = M* (2*NLAT-M-1)/2
          MEO = 1
          IF (MODE.EQ.1) MEO = 2
          MS = M + MEO
          DO 113 K = 1,NT
              DO 113 NP1 = MS,NLAT,2
                  MN = MML1 + NP1
                  DO 113 I = 1,LATE
                      G(I,1,K) = G(I,1,K) + A(1,NP1,K)*PMN(I,MN)
  113     CONTINUE

c     sweep interior columns of g

          DO 114 MP1 = 2,LM1
              M = MP1 - 1
              MML1 = M* (2*NLAT-M-1)/2
              MS = M + MEO
              DO 115 K = 1,NT
                  DO 115 NP1 = MS,NLAT,2
                      MN = MML1 + NP1
                      DO 115 I = 1,LATE
                          G(I,2*M,K) = G(I,2*M,K) +
     +                                 A(MP1,NP1,K)*PMN(I,MN)
                          G(I,2*M+1,K) = G(I,2*M+1,K) +
     +                                   B(MP1,NP1,K)*PMN(I,MN)
  115         CONTINUE
  114     CONTINUE

          IF (NLON.EQ.L+L-2) THEN
c     set last column
              M = L - 1
              MML1 = M* (2*NLAT-M-1)/2
              NS = L
              IF (MODE.EQ.1) NS = L + 1
              DO 116 K = 1,NT
                  DO 116 NP1 = NS,NLAT,2
                      MN = MML1 + NP1
                      DO 116 I = 1,LATE
                          G(I,NLON,K) = G(I,NLON,K) +
     +                                  2.0D0*A(L,NP1,K)*PMN(I,MN)
  116         CONTINUE
          END IF

      END IF


c     do inverse fourier transform
      DO 120 K = 1,NT
          CALL DHRFFTB(LAT,NLON,G(1,1,K),LAT,WFFT,WORK)
  120 CONTINUE
c     scale output in gs
      DO 122 K = 1,NT
          DO 122 J = 1,NLON
              DO 122 I = 1,LAT
                  GS(I,J,K) = 0.5D0*G(I,J,K)
  122 CONTINUE

      RETURN
      END
      SUBROUTINE DSHSGSI(NLAT,NLON,WSHSGS,LSHSGS,WORK,LWORK,DWORK,
     +                   LDWORK,IERROR)
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK
c
c     this subroutine must be called before calling shags or shsgs with
c     fixed nlat,nlon. it precomputes the gaussian weights, points
c     and all necessary legendre polys and stores them in wshsgs.
c     these quantities must be preserved when calling shsgs
c     repeatedly with fixed nlat,nlon.
c
      DIMENSION WSHSGS(LSHSGS),WORK(LWORK)
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
      IF (LSHSGS.LT.LP) RETURN
      IERROR = 4
c     check temporary work space
      IF (LWORK.LT.4*NLAT* (NLAT+2)+2) RETURN
      IERROR = 5
      IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
      IERROR = 0
C*PL*ERROR* Comment line too long
c     set preliminary quantites needed to compute and store legendre polys
      LDW = NLAT* (NLAT+4)
      CALL DSHSGSP(NLAT,NLON,WSHSGS,LSHSGS,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) RETURN
c     set legendre poly pointer in wshsgs
      IPMNF = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) +
     +        NLON + 16
      CALL DSHSGSS1(NLAT,L,LATE,WSHSGS,WORK,WSHSGS(IPMNF))
      RETURN
      END
      SUBROUTINE DSHSGSS1(NLAT,L,LATE,W,PMN,PMNF)
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
      SUBROUTINE DSHSGSP(NLAT,NLON,WSHSGS,LSHSGS,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHSGS
      DIMENSION WSHSGS(LSHSGS)
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
      IF (LSHSGS.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
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
      CALL DSHSGSP1(NLAT,NLON,L,LATE,WSHSGS(I1),WSHSGS(I2),WSHSGS(I3),
     +              WSHSGS(I4),WSHSGS(I5),WSHSGS(I6),WSHSGS(I7),
     +              DWORK(IDTH),DWORK(IDWTS),DWORK(IW),IERROR)
      IF (IERROR.NE.0) IERROR = 6
      RETURN
      END
      SUBROUTINE DSHSGSP1(NLAT,NLON,L,LATE,WTS,P0N,P1N,ABEL,BBEL,CBEL,
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
c
c     compute double precision gaussian points and weights
c
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
