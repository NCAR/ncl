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
c ... file igradgs.f
c
c     this file includes documentation and code for
c     subroutine igradgs         i
c
c ... files which must be loaded with igradgs.f
c
c     sphcom.f, hrfft.f, shsgs.f,vhags.f
c
c     subroutine igradgs(nlat,nlon,isym,nt,sf,isf,jsf,br,bi,mdb,ndb,
c    +                   wshsgs,lshsgs,work,lwork,ierror)
c
c     let br,bi,cr,ci be the vector spherical harmonic coefficients
c     precomputed by vhags for a vector field (v,w).  let (v',w') be
c     the irrotational component of (v,w) (i.e., (v',w') is generated
c     by assuming cr,ci are zero and synthesizing br,bi with vhsgs).
c     then subroutine igradgs computes a scalar field sf such that
c
c            gradient(sf) = (v',w').
c
c     i.e.,
c
C*PL*ERROR* Comment line too long
c            v'(i,j) = d(sf(i,j))/dtheta          (colatitudinal component of
c                                                 the gradient)
c     and
c
C*PL*ERROR* Comment line too long
c            w'(i,j) = 1/sint*d(sf(i,j))/dlambda  (east longitudinal component
c                                                 of the gradient)
c
c     at the gaussian colatitude theta(i) (see nlat as input parameter)
C*PL*ERROR* Comment line too long
c     and longitude lambda(j) = (j-1)*2*pi/nlon where sint = sin(theta(i)).
c
c     note:  for an irrotational vector field (v,w), subroutine igradgs
c     computes a scalar field whose gradient is (v,w).  in ay case,
c     subroutine igradgs "inverts" the gradient subroutine gradgs.
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
c     isym   a parameter which determines whether the scalar field sf is
c            computed on the full or half sphere as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case sf
c            is neither symmetric nor antisymmetric about the equator.
c            sf is computed on the entire sphere.  i.e., in the array
c            sf(i,j) for i=1,...,nlat and  j=1,...,nlon
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case sf is antisymmetyric about the equator and
c            is computed for the northern hemisphere only.  i.e.,
c            if nlat is odd sf is computed in the array sf(i,j) for
c            i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is even
c            sf is computed in the array sf(i,j) for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            w is symmetric and v is antisymmetric about the equator.
c            in this case sf is symmetyric about the equator and
c            is computed for the northern hemisphere only.  i.e.,
c            if nlat is odd sf is computed in the array sf(i,j) for
c            i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is even
c            sf is computed in the array sf(i,j) for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
C*PL*ERROR* Comment line too long
c            the arrays br,bi, and sf can be three dimensional corresponding
c            to an indexed multiple vector field (v,w).  in this case,
c            multiple scalar synthesis will be performed to compute each
C*PL*ERROR* Comment line too long
c            scalar field.  the third index for br,bi, and sf is the synthesis
c            index which assumes the values k = 1,...,nt.  for a single
c            synthesis set nt = 1.  the description of the remaining
C*PL*ERROR* Comment line too long
c            parameters is simplified by assuming that nt=1 or that br,bi,
c            and sf are two dimensional arrays.
c
c     isf    the first dimension of the array sf as it appears in
c            the program that calls igradgs. if isym = 0 then isf
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then isf must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then isf must be at least (nlat+1)/2.
c
c     jsf    the second dimension of the array sf as it appears in
c            the program that calls igradgs. jsf must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c     ***    br,bi must be computed by vhags prior to calling igradgs.
c
C*PL*ERROR* Comment line too long
c     mdb    the first dimension of the arrays br and bi as it appears in
c            the program that calls igradgs (and vhags). mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
C*PL*ERROR* Comment line too long
c     ndb    the second dimension of the arrays br and bi as it appears in
c            the program that calls igradgs (and vhags). ndb must be at
c            least nlat.
c
c
c  wshsgs    an array which must be initialized by subroutine igradgsi
c            (or equivalently by subroutine shsesi).  once initialized,
c            wshsgs can be used repeatedly by igradgs as long as nlon
c            and nlat remain unchanged.  wshsgs must not be altered
c            between calls of igradgs.
c
c
c  lshsgs    the dimension of the array wshsgs as it appears in the
c            program that calls igradgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c
c            then lshsgs must be greater than or equal to
c
C*PL*ERROR* Comment line too long
c               nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls igradgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 lwork must be greater than or equal to
c
c               nlat*((nt+1)*nlon+2*nt*l1+1)
c
c            if isym > 0 lwork must be greater than or equal to
c
c               (nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
c
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c     sf    a two or three dimensional array (see input parameter nt) that
c           contain a scalar field whose gradient is the irrotational
c           component of the vector field (v,w).  the vector spherical
c           harmonic coefficients br,bi were precomputed by subroutine
c           vhags.  sf(i,j) is given at the gaussian colatitude theta(i)
c           and longitude lambda(j) = (j-1)*2*pi/nlon.  the index ranges
c           are defined at input parameter isym.
c
c
c  ierror   = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of isf
c           = 6  error in the specification of jsf
c           = 7  error in the specification of mdb
c           = 8  error in the specification of ndb
c           = 9  error in the specification of lshsgs
c           = 10 error in the specification of lwork
c
c **********************************************************************
c
      SUBROUTINE DIGRADGS(NLAT,NLON,ISYM,NT,SF,ISF,JSF,BR,BI,MDB,NDB,
     +                   WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION SF
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK
      DIMENSION SF(ISF,JSF,NT)
      DIMENSION BR(MDB,NDB,NT),BI(MDB,NDB,NT)
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
      IF ((ISYM.EQ.0.AND.ISF.LT.NLAT) .OR.
     +    (ISYM.NE.0.AND.ISF.LT.IMID)) RETURN
      IERROR = 6
      IF (JSF.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT, (NLON+2)/2)
      IF (MDB.LT.MIN0(NLAT, (NLON+1)/2)) RETURN
      IERROR = 8
      IF (NDB.LT.NLAT) RETURN
      IERROR = 9
c
c     verify saved work space length
c
      L2 = (NLAT+MOD(NLAT,2))/2
      L1 = MIN0((NLON+2)/2,NLAT)
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
      IERROR = 10
c
c     set minimum and verify unsaved work space
c
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
c
c     set first dimension for a,b (as requried by shses)
c
      MAB = MIN0(NLAT,NLON/2+1)
      MN = MAB*NLAT*NT
      LWKMIN = NLN + LS*NLON + 2*MN + NLAT
      IF (LWORK.LT.LWKMIN) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IS = IB + MN
      IWK = IS + NLAT
      LIWK = LWORK - 2*MN - NLAT
      CALL DIGRDGS1(NLAT,NLON,ISYM,NT,SF,ISF,JSF,WORK(IA),WORK(IB),MAB,
     +             WORK(IS),MDB,NDB,BR,BI,WSHSGS,LSHSGS,WORK(IWK),LIWK,
     +             IERROR)
      RETURN
      END

      SUBROUTINE DIGRDGS1(NLAT,NLON,ISYM,NT,SF,ISF,JSF,A,B,MAB,SQNN,MDB,
     +                   NDB,BR,BI,WSAV,LSAV,WK,LWK,IERROR)
      DOUBLE PRECISION SF
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION WSAV
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION SF(ISF,JSF,NT)
      DIMENSION BR(MDB,NDB,NT),BI(MDB,NDB,NT),SQNN(NLAT)
      DIMENSION A(MAB,NLAT,NT),B(MAB,NLAT,NT)
      DIMENSION WSAV(LSAV),WK(LWK)
c
c     preset coefficient multiplyers in vector
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          SQNN(N) = 1.0D0/SQRT(FN* (FN+1.D0))
    1 CONTINUE
c
c     set upper limit for vector m subscript
c
      MMAX = MIN0(NLAT, (NLON+1)/2)
c
c     compute multiple scalar field coefficients
c
      DO 2 K = 1,NT
c
c     preset to 0.0
c
          DO 3 N = 1,NLAT
              DO 4 M = 1,MAB
                  A(M,N,K) = 0.0D0
                  B(M,N,K) = 0.0D0
    4         CONTINUE
    3     CONTINUE
c
c     compute m=0 coefficients
c
          DO 5 N = 2,NLAT
              A(1,N,K) = BR(1,N,K)*SQNN(N)
              B(1,N,K) = BI(1,N,K)*SQNN(N)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  A(M,N,K) = SQNN(N)*BR(M,N,K)
                  B(M,N,K) = SQNN(N)*BI(M,N,K)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     scalar sythesize a,b into sf
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,SF,ISF,JSF,A,B,MAB,NLAT,WSAV,LSAV,
     +            WK,LWK,IERROR)
      RETURN
      END
