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
c ... file shigs.f
c
c     this file contains code and documentation for subroutine shigs
c
c ... files which must be loaded with shigs.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     3/6/98
c
c *** shigs is functionally the same as shagsi or shsgsi.  It
c     it included in spherepack3.0 because legacy codes, using
c     the older version of spherepack, call shigs to initialize
c     the saved work space wshigs for either shags or shsgs
c     Its arguments are identical to those of shagsi or shsgsi.
c
c ****************************************************************
c
c     subroutine shigs(nlat,nlon,wshigs,lshigs,work,lwork,dwork,ldwork,
c    +                 ierror)
c
c     subroutine shigs initializes the array wshigs which can then
c     be used repeatedly by subroutines shags,shsgs. it precomputes
c     and stores in wshigs quantities such as gaussian weights,
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
c     wshigs an array which must be initialized by subroutine shigs .
c            once initialized, wshigs can be used repeatedly by shigs
c            as long as nlat and nlon remain unchanged.  wshigs must
c            not be altered between calls of shigs.
c
c     lshigs the dimension of the array wshigs as it appears in the
c            program that calls shigs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshigs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shigs. lwork must be at least
c            4*nlat*(nlat+2)+2 in the routine calling shigs
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
c
      SUBROUTINE DSHIGS(NLAT,NLON,WSHIGS,LSHIGS,WORK,LWORK,DWORK,
     +                  LDWORK,IERROR)
      DOUBLE PRECISION WSHIGS
      DOUBLE PRECISION WORK
c
c     this subroutine must be called before calling shags or shsgs with
c     fixed nlat,nlon. it precomputes the gaussian weights, points
c     and all necessary legendre polys and stores them in wshigs.
c     these quantities must be preserved when calling shsgs or shags
c     repeatedly with fixed nlat,nlon.
c
      DIMENSION WSHIGS(LSHIGS),WORK(LWORK)
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
      IF (LSHIGS.LT.LP) RETURN
      IERROR = 4
c     check temporary work space
      IF (LWORK.LT.4*NLAT* (NLAT+2)+2) RETURN
c     check temp double precision space
      IERROR = 5
      IF (LDWORK.LT.NLAT* (NLAT+4)) RETURN
      IERROR = 0
C*PL*ERROR* Comment line too long
c     set preliminary quantites needed to compute and store legendre polys
      CALL DSHIGSP(NLAT,NLON,WSHIGS,LSHIGS,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) RETURN
c     set legendre poly pointer in wshigs
      IPMNF = NLAT + 2*NLAT*LATE + 3* (L* (L-1)/2+ (NLAT-L)* (L-1)) +
     +        NLON + 16
      CALL DSHIGSS1(NLAT,L,LATE,WSHIGS,WORK,WSHIGS(IPMNF))
      RETURN
      END

      SUBROUTINE DSHIGSS1(NLAT,L,LATE,W,PMN,PMNF)
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
      SUBROUTINE DSHIGSP(NLAT,NLON,WSHIGS,LSHIGS,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHIGS
      DIMENSION WSHIGS(LSHIGS)
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
      IF (LSHIGS.LT.NLAT* (2*L2+3*L1-2)+3*L1* (1-L1)/2+NLON+15) RETURN
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
      CALL DSHIGSP1(NLAT,NLON,L,LATE,WSHIGS(I1),WSHIGS(I2),WSHIGS(I3),
     +              WSHIGS(I4),WSHIGS(I5),WSHIGS(I6),WSHIGS(I7),
     +              DWORK(IDTH),DWORK(IDWTS),DWORK(IW),IERROR)
      IF (IERROR.NE.0) IERROR = 5
      RETURN
      END

      SUBROUTINE DSHIGSP1(NLAT,NLON,L,LATE,WTS,P0N,P1N,ABEL,BBEL,CBEL,
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
