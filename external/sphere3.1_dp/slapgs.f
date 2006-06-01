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
c
c ... file slapgs.f
c
c     this file includes documentation and code for
c     subroutine slapgs          i
c
c ... files which must be loaded with slapgs.f
c
c     sphcom.f, hrfft.f, shags.f, shsgs.f
c
c
c
c     subroutine slapgs(nlat,nlon,isym,nt,slap,ids,jds,a,b,
c    +mdab,ndab,wshsgs,lshsgs,work,lwork,ierror)
c
c
C*PL*ERROR* Comment line too long
c     given the scalar spherical harmonic coefficients a and b, precomputed
C*PL*ERROR* Comment line too long
c     by subroutine shags for a scalar field sf, subroutine slapgs computes
c     the laplacian of sf in the scalar array slap.  slap(i,j) is the
c     laplacian of sf at the gaussian colatitude theta(i) (see nlat as
c     an input parameter) and east longitude lambda(j) = (j-1)*2*pi/nlon
c     on the sphere.  i.e.
c
c         slap(i,j) =
c
c                  2                2
C*PL*ERROR* Comment line too long
c         [1/sint*d (sf(i,j)/dlambda + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c
c     where sint = sin(theta(i)).  the scalar laplacian in slap has the
C*PL*ERROR* Comment line too long
c     same symmetry or absence of symmetry about the equator as the scalar
c     field sf.  the input parameters isym,nt,mdab,ndab must have the
C*PL*ERROR* Comment line too long
c     same values used by shags to compute a and b for sf. the associated
c     legendre functions are stored rather than recomputed as they are
c     in subroutine slapgc.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
C*PL*ERROR* Comment line too long
c            full sphere. these lie in the interval (0,pi) and are computed
C*PL*ERROR* Comment line too long
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
C*PL*ERROR* Comment line too long
c            if nlat is odd the equator will be included as the grid point
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
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
c            shags to compute the coefficients a and b for the scalar field
c            sf.  isym is set as follows:
c
c            = 0  no symmetries exist in sf about the equator. scalar
C*PL*ERROR* Comment line too long
c                 synthesis is used to compute slap on the entire sphere.
c                 i.e., in the array slap(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute slap is performed on the
c                northern hemisphere only.  if nlat is odd, slap(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, slap(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute slap is performed on the
c                northern hemisphere only.  if nlat is odd, slap(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, slap(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c     nt     the number of analyses.  in the program that calls slapgs
c            the arrays slap,a, and b can be three dimensional in which
c            case multiple synthesis will be performed.  the third index
c            is the synthesis index which assumes the values k=1,...,nt.
c            for a single analysis set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            or that all the arrays are two dimensional.
c
c   ids      the first dimension of the array slap as it appears in the
c            program that calls slapgs.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array slap as it appears in the
c            program that calls slapgs. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field sf as computed by subroutine shags.
c     ***    a,b must be computed by shags prior to calling slapgs.
c
c
c    mdab    the first dimension of the arrays a and b as it appears
c            in the program that calls slapgs.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndab    the second dimension of the arrays a and b as it appears
c            in the program that calls slapgs. ndbc must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shags to
c            compute the coefficients a and b.
c
c
c    wshsgs  an array which must be initialized by subroutine slapgsi
c            (or equivalently by shsgsi).  once initialized, wshsgs
c            can be used repeatedly by slapgs as long as nlat and nlon
c            remain unchanged.  wshsgs must not be altered between calls
c            of slapgs.
c
c    lshsgs  the dimension of the array wshsgs as it appears in the
c            program that calls slapgs.  let
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
C*PL*ERROR* Comment line too long
c               nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls slapgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym is zero then lwork must be at least
c
c               (nt+1)*nlat*nlon + nlat*(2*nt*l1+1)
c
c            if isym is nonzero lwork must be at least
c
c               (nt+1)*l2*nlon + nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c    slap    a two or three dimensional arrays (see input parameter nt) that
C*PL*ERROR* Comment line too long
c            contain the scalar laplacian of the scalar field sf.  slap(i,j)
c            is the scalar laplacian at the gaussian colatitude theta(i)
c            and longitude lambda(j) = (j-1)*2*pi/nlon for i=1,...,nlat
c            and j=1,...,nlon.
c
c
C*PL*ERROR* Comment line too long
c  ierror    a parameter which flags errors in input parameters as follows:
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
c            = 9  error in the specification of lshsgs
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c
c     end of documentation for slapgs
c
c **********************************************************************
c
c
      SUBROUTINE DSLAPGS(NLAT,NLON,ISYM,NT,SLAP,IDS,JDS,A,B,MDAB,NDAB,
     +                  WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION SLAP
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK
      DIMENSION SLAP(IDS,JDS,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WSHSGS(LSHSGS),WORK(LWORK)
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
      IMID = (NLAT+1)/2
      L2 = (NLAT+MOD(NLAT,2))/2
      L1 = MIN0((NLON+2)/2,NLAT)
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
      IERROR = 10
c
c     set and verify unsaved work space length
c
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
      MN = MMAX*NLAT*NT
c     lwkmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwkmin) return
      L2 = (NLAT+1)/2
      L1 = MIN0(NLAT,NLON/2+1)
      IF (ISYM.EQ.0) THEN
          LWKMIN = (NT+1)*NLAT*NLON + NLAT* (2*NT*L1+1)
      ELSE
          LWKMIN = (NT+1)*L2*NLON + NLAT* (2*NT*L1+1)
      END IF
      IF (LWORK.LT.LWKMIN) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IFN = IB + MN
      IWK = IFN + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DSLAPGS1(NLAT,NLON,ISYM,NT,SLAP,IDS,JDS,A,B,MDAB,NDAB,
     +             WORK(IA),WORK(IB),MMAX,WORK(IFN),WSHSGS,LSHSGS,
     +             WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DSLAPGS1(NLAT,NLON,ISYM,NT,SLAP,IDS,JDS,A,B,MDAB,NDAB,
     +                   ALAP,BLAP,MMAX,FNN,WSAVE,LSAVE,WK,LWK,IERROR)
      DOUBLE PRECISION SLAP
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION ALAP
      DOUBLE PRECISION BLAP
      DOUBLE PRECISION FNN
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION SLAP(IDS,JDS,NT),A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION ALAP(MMAX,NLAT,NT),BLAP(MMAX,NLAT,NT),FNN(NLAT)
      DIMENSION WSAVE(LSAVE),WK(LWK)
c
c     set coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          FNN(N) = FN* (FN+1.D0)
    1 CONTINUE
c
c     compute scalar laplacian coefficients for each vector field
c
      DO 2 K = 1,NT
          DO 3 N = 1,NLAT
              DO 4 M = 1,MMAX
                  ALAP(M,N,K) = 0.0D0
                  BLAP(M,N,K) = 0.0D0
    4         CONTINUE
    3     CONTINUE
c
c     compute m=0 coefficients
c
          DO 5 N = 2,NLAT
              ALAP(1,N,K) = -FNN(N)*A(1,N,K)
              BLAP(1,N,K) = -FNN(N)*B(1,N,K)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  ALAP(M,N,K) = -FNN(N)*A(M,N,K)
                  BLAP(M,N,K) = -FNN(N)*B(M,N,K)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     synthesize alap,blap into slap
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,SLAP,IDS,JDS,ALAP,BLAP,MMAX,NLAT,
     +           WSAVE,LSAVE,WK,LWK,IERROR)
      RETURN
      END
