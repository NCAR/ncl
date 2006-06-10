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
c ... file shigc.f
c
c     this file contains code and documentation for subroutine shigc
c
c ... files which must be loaded with shigc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     3/6/98
c
c *** shigc is functionally the same as shagci or shsgci.  It
c     it included in spherepack3.0 because legacy codes, using
c     the older version of spherepack call shigc to initialize
c     the saved work space wshigc, for either shagc or shsgc
c
c     subroutine shigc(nlat,nlon,wshigc,lshigc,dwork,ldwork,ierror)
c
c     subroutine shigc initializes the array wshigc which can then
c     be used repeatedly by subroutines shsgc or shagc. it precomputes
c     and stores in wshigc quantities such as gaussian weights,
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
c     wshigc an array which must be initialized by subroutine shigc.
c            once initialized, wshigc can be used repeatedly by shsgc
c            or shagc as long as nlat and nlon remain unchanged.  wshigc
c            must not be altered between calls of shsgc or shagc.
c
c     lshigc the dimension of the array wshigc as it appears in the
c            program that calls shsgc or shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshigc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
C*PL*ERROR* Comment line too long
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shigc. ldwork must be at least
c
c               nlat*(nlat+4)
c
c     output parameter
c
C*PL*ERROR* Comment line too long
c     wshigc an array which must be initialized before calling shsgc or shagc.
C*PL*ERROR* Comment line too long
c            once initialized, wshigc can be used repeatedly by shsgc or shagc
c            as long as nlat and nlon remain unchanged.  wshigc must not
c            altered between calls of shsgc or shagc
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshigc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      SUBROUTINE DSHIGC(NLAT,NLON,WSHIGC,LSHIGC,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHIGC
c     this subroutine must be called before calling shsgc/shagc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      DIMENSION WSHIGC(LSHIGC)
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
      IF (LSHIGC.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
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
      CALL DSHIGC1(NLAT,NLON,L,LATE,WSHIGC(I1),WSHIGC(I2),WSHIGC(I3),
     +            WSHIGC(I4),WSHIGC(I5),WSHIGC(I6),WSHIGC(I7),
     +            DWORK(IDTH),DWORK(IDWTS),DWORK(IW),IERROR)
      IF (IERROR.NE.0) IERROR = 5
      RETURN
      END
      SUBROUTINE DSHIGC1(NLAT,NLON,L,LATE,WTS,P0N,P1N,ABEL,BBEL,CBEL,
     +                   WFFT,DTHETA,DWTS,WORK,IER)
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
