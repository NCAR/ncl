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
c ... file sfvpgs.f
c
c     this file includes documentation and code for
c     subroutine sfvpgs          i
c
c ... files which must be loaded with sfvpgs.f
c
c     sphcom.f, hrfft.f, vhags.f, shsgs.f, gaqd.f
c
c
c     subroutine sfvpgs(nlat,nlon,isym,nt,sf,vp,idv,jdv,br,bi,cr,ci,
c    +                   mdb,ndb,wshsgs,lshsgs,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients br,bi,cr,ci,
c     computed by subroutine vhags for a vector field (v,w), sfvpgs
c     computes a scalar stream function sf and scalar velocity potential
c     vp for (v,w).  (v,w) is expressed in terms of sf and vp by the
c     helmholtz relations (in mathematical spherical coordinates):
c
c          v = -1/sint*d(vp)/dlambda + d(st)/dtheta
c
c          w =  1/sint*d(st)/dlambda + d(vp)/dtheta
c
c     where sint = sin(theta).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     br,bi,cr,ci were precomputed.  required associated legendre
c     polynomials are stored rather than recomputed as they are in
c     subroutine sfvpgc. sf(i,j) and vp(i,j) are given at the i(th)
c     gaussian colatitude point theta(i) (see nlat description below)
c     and east longitude lambda(j) = (j-1)*2*pi/nlon on the sphere.
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater than
c            3.  the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
C*PL*ERROR* Comment line too long
c     isym   a parameter which determines whether the stream function and
c            velocity potential are computed on the full or half sphere
c            as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case st
c            and vp are not necessarily symmetric or antisymmetric about
c            the equator.  sf and vp are computed on the entire sphere.
c            i.e., in arrays sf(i,j),vp(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case sf is symmetric and vp antisymmetric about
c            the equator and are computed for the northern hemisphere
C*PL*ERROR* Comment line too long
c            only.  i.e., if nlat is odd the sf(i,j),vp(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then sf(i,j),vp(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            w is symmetric and v is antisymmetric about the equator.
c            in this case sf is antisymmetric and vp symmetric about
c            the equator and are computed for the northern hemisphere
C*PL*ERROR* Comment line too long
c            only.  i.e., if nlat is odd the sf(i,j),vp(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then sf(i,j),vp(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c     nt     nt is the number of scalar and vector fields.  some
C*PL*ERROR* Comment line too long
c            computational efficiency is obtained for multiple fields. arrays
C*PL*ERROR* Comment line too long
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute sf,vp for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
C*PL*ERROR* Comment line too long
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     idv    the first dimension of the arrays sf,vp as it appears in
c            the program that calls sfvpgs. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the arrays sf,vp as it appears in
c            the program that calls sfvpgs. jdv must be at least nlon.
c
c     br,bi, two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c
c     mdb    the first dimension of the arrays br,bi,cr,ci as it
c            appears in the program that calls sfvpgs. mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br,bi,cr,ci as it
c            appears in the program that calls sfvpgs. ndb must be at
c            least nlat.
c
c     wshsgs an array which must be initialized by subroutine shsgsi.
c            once initialized, wshsgs can be used repeatedly by sfvpgs
c            as long as nlon and nlat remain unchanged.  wshsgs must
c            not bel altered between calls of sfvpgs.
c
c
c     lshsgs the dimension of the array wshsgs as it appears in the
c            program that calls sfvpgs. define
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
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls sfvpgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                  nlat*nlon*(nt+1) + nlat*(2*l1*nt + 1)
c
c            if isym is nonzero then lwork must be at least
c
c                  l2*nlon*(nt+1) + nlat*(2*l1*nt + 1)
c
c     **************************************************************
c
c     output parameters
c
c    sf,vp  two or three dimensional arrays (see input parameter nt)
c           that contains the stream function and velocity potential
c           of the vector field (v,w) whose coefficients br,bi,cr,ci
c           where precomputed by subroutine vhags.  sf(i,j),vp(i,j)
c           are given at the i(th) gaussian colatitude point theta(i)
c           and longitude point lambda(j) = (j-1)*2*pi/nlon.  the index
c           ranges are defined above at the input parameter isym.
c
c
c    ierror = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idv
c           = 6  error in the specification of jdv
c           = 7  error in the specification of mdb
c           = 8  error in the specification of ndb
c           = 9  error in the specification of lshsgs
c           = 10 error in the specification of lwork
c **********************************************************************
c
      SUBROUTINE DSFVPGS(NLAT,NLON,ISYM,NT,SF,VP,IDV,JDV,BR,BI,CR,CI,
     +                   MDB,NDB,WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      IMPLICIT NONE
      INTEGER NLAT,NLON,ISYM,NT,IDV,JDV,MDB,NDB,LSHSGS,LWORK,IERROR
      DOUBLE PRECISION SF(IDV,JDV,NT),VP(IDV,JDV,NT)
      DOUBLE PRECISION BR(MDB,NDB,NT),BI(MDB,NDB,NT)
      DOUBLE PRECISION CR(MDB,NDB,NT),CI(MDB,NDB,NT)
      DOUBLE PRECISION WSHSGS(LSHSGS),WORK(LWORK)
      INTEGER IMID,LS,MAB,MN,IA,IB,IS,LWK,IWK
      INTEGER LAT,LATE,L1,L2,LP
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
      IF ((ISYM.EQ.0.AND.IDV.LT.NLAT) .OR.
     +    (ISYM.GT.0.AND.IDV.LT.IMID)) RETURN
      IERROR = 6
      IF (JDV.LT.NLON) RETURN
      IERROR = 7
      IF (MDB.LT.MIN0(NLAT, (NLON+1)/2)) RETURN
      IERROR = 8
      IF (NDB.LT.NLAT) RETURN
      IERROR = 9
      L1 = MIN0((NLON+2)/2,NLAT)
      LATE = (NLAT+MOD(NLAT,2))/2
      LAT = NLAT
      IF (ISYM.NE.0) LAT = LATE
      L2 = LATE
c     check permanent work space length
      L2 = (NLAT+MOD(NLAT,2))/2
      L1 = MIN0((NLON+2)/2,NLAT)
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
c
c     verify unsaved work space
c
      IERROR = 10
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
c
c     set first dimension for a,b (as requried by shsgs)
c
      MAB = MIN0(NLAT,NLON/2+1)
      MN = MAB*NLAT*NT
      IF (LWORK.LT.LS*NLON+ (NT+1)+NLAT* (2*L1*NT+1)) RETURN

      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IS = IB + MN
      IWK = IS + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DSTVPGS1(NLAT,NLON,ISYM,NT,SF,VP,IDV,JDV,BR,BI,CR,CI,MDB,NDB,
     +             WORK(IA),WORK(IB),MAB,WORK(IS),WSHSGS,LSHSGS,
     +             WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DSTVPGS1(NLAT,NLON,ISYM,NT,SF,VP,IDV,JDV,BR,BI,CR,CI,
     +                   MDB,NDB,A,B,MAB,FNN,WSHSGS,LSHSGS,WK,LWK,
     +                   IERROR)
      IMPLICIT NONE
      INTEGER NLAT,NLON,ISYM,NT,IDV,JDV,MDB,NDB,MAB,LSHSGS,LWK,IERROR
      DOUBLE PRECISION SF(IDV,JDV,NT),VP(IDV,JDV,NT)
      DOUBLE PRECISION BR(MDB,NDB,NT),BI(MDB,NDB,NT),CR(MDB,NDB,NT),
     +                 CI(MDB,NDB,NT)
      DOUBLE PRECISION A(MAB,NLAT,NT),B(MAB,NLAT,NT)
      DOUBLE PRECISION WSHSGS(LSHSGS),WK(LWK),FNN(NLAT)
      INTEGER N,M,MMAX,K
c
c     set coefficient multiplyers
c
      DO N = 2,NLAT
          FNN(N) = 1.0D0/SQRT(DBLE(N* (N-1)))
      END DO
      MMAX = MIN0(NLAT, (NLON+1)/2)
c
c     compute st scalar coefficients from cr,ci
c
      DO K = 1,NT
          DO N = 1,NLAT
              DO M = 1,MAB
                  A(M,N,K) = 0.0D0
                  B(M,N,K) = 0.0D0
              END DO
          END DO
c
c     compute m=0 coefficients
c
          DO N = 2,NLAT
              A(1,N,K) = -FNN(N)*CR(1,N,K)
              B(1,N,K) = -FNN(N)*CI(1,N,K)
          END DO
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
          DO M = 2,MMAX
              DO N = M,NLAT
                  A(M,N,K) = -FNN(N)*CR(M,N,K)
                  B(M,N,K) = -FNN(N)*CI(M,N,K)
              END DO
          END DO
      END DO
c
c     synthesize a,b into st
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,SF,IDV,JDV,A,B,MAB,NLAT,WSHSGS,
     +           LSHSGS,WK,LWK,IERROR)
c
c    set coefficients for vp from br,bi
c
      DO K = 1,NT
          DO N = 1,NLAT
              DO M = 1,MAB
                  A(M,N,K) = 0.0D0
                  B(M,N,K) = 0.0D0
              END DO
          END DO
c
c     compute m=0 coefficients
c
          DO N = 2,NLAT
              A(1,N,K) = FNN(N)*BR(1,N,K)
              B(1,N,K) = FNN(N)*BI(1,N,K)
          END DO
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
          MMAX = MIN0(NLAT, (NLON+1)/2)
          DO M = 2,MMAX
              DO N = M,NLAT
                  A(M,N,K) = FNN(N)*BR(M,N,K)
                  B(M,N,K) = FNN(N)*BI(M,N,K)
              END DO
          END DO
      END DO
c
c     synthesize a,b into vp
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,VP,IDV,JDV,A,B,MAB,NLAT,WSHSGS,
     +           LSHSGS,WK,LWK,IERROR)
      RETURN
      END
