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
c ... file divgs.f
c
c     this file includes documentation and code for
c     subroutine divgs          i
c
c ... files which must be loaded with divgs.f
c
c     sphcom.f, hrfft.f, vhags.f, shsgs.f, gaqd.f
c
c
c     subroutine divgs(nlat,nlon,isym,nt,divg,idiv,jdiv,br,bi,mdb,ndb,
c    +                 wshsgs,lshsgs,work,lwork,ierror)
c
C*PL*ERROR* Comment line too long
c     given the vector spherical harmonic coefficients br and bi, precomputed
c     by subroutine vhags for a vector field (v,w), subroutine divgs
C*PL*ERROR* Comment line too long
c     computes the divergence of the vector field in the scalar array divg.
C*PL*ERROR* Comment line too long
c     divg(i,j) is the divergence at the gaussian colatitude point theta(i)
c     (see nlat as input parameter) and east longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     on the sphere.  i.e.
c
C*PL*ERROR* Comment line too long
c            dv(i,j) = 1/sint*[ d(sint*v(i,j))/dtheta + d(w(i,j))/dlambda ]
c
c     where sint = sin(theta(i)).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     br,bi were precomputed
c
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
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   a parameter which determines whether the divergence is
c            computed on the full or half sphere as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case the
c            divergence is neither symmetric nor antisymmetric about
c            the equator.  the divergence is computed on the entire
c            sphere.  i.e., in the array divg(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the divergence is antisymmetyric about
c            the equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the divergence is computed
c            in the array divg(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the divergence is computed
c            in the array divg(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the divergence is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the divergence is computed
c            in the array divg(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the divergence is computed
c            in the array divg(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            in the program that calls divgs, the arrays br,bi, and divg
C*PL*ERROR* Comment line too long
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute the divergence for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
C*PL*ERROR* Comment line too long
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     idiv   the first dimension of the array divg as it appears in
c            the program that calls divgs. if isym = 0 then idiv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idiv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idiv must be at least (nlat+1)/2.
c
c     jdiv   the second dimension of the array divg as it appears in
c            the program that calls divgs. jdiv must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c     ***    br and bi must be computed by vhags prior to calling
c            divgs.
c
c     mdb    the first dimension of the arrays br and bi as it
c            appears in the program that calls divgs. mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br and bi as it
c            appears in the program that calls divgs. ndb must be at
c            least nlat.
c
c
c     wshsgs an array which must be intialized by subroutine shsgsi.
c            once initialized,
c            wshsgs can be used repeatedly by divgs as long as nlon
c            and nlat remain unchanged.  wshsgs must not be altered
c            between calls of divgs.
c
c
c     lshsgs the dimension of the array wshsgs as it appears in the
c            program that calls divgs. define
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
c            program that calls divgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym = 0 then lwork must be at least
c
c               nlat*((nt+1)*nlon+2*nt*l1+1)
c
c            if isym > 0 then lwork must be at least
c
c               (nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
c     **************************************************************
c
c     output parameters
c
c
c    divg   a two or three dimensional array (see input parameter nt)
c           that contains the divergence of the vector field (v,w)
c           whose coefficients br,bi where computed by subroutine
C*PL*ERROR* Comment line too long
c           vhags.  divg(i,j) is the divergence at the gaussian colatitude
C*PL*ERROR* Comment line too long
c           point theta(i) and longitude point lambda(j) = (j-1)*2*pi/nlon.
c           the index ranges are defined above at the input parameter
c           isym.
c
c
c    ierror = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idiv
c           = 6  error in the specification of jdiv
c           = 7  error in the specification of mdb
c           = 8  error in the specification of ndb
c           = 9  error in the specification of lshsgs
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DDIVGS(NLAT,NLON,ISYM,NT,DIVG,IDIV,JDIV,BR,BI,MDB,NDB,
     +                  WSHSGS,LSHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION DIVG
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WORK

      DIMENSION DIVG(IDIV,JDIV,NT),BR(MDB,NDB,NT),BI(MDB,NDB,NT)
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
      IF ((ISYM.EQ.0.AND.IDIV.LT.NLAT) .OR.
     +    (ISYM.GT.0.AND.IDIV.LT.IMID)) RETURN
      IERROR = 6
      IF (JDIV.LT.NLON) RETURN
      IERROR = 7
      IF (MDB.LT.MIN0(NLAT, (NLON+1)/2)) RETURN
      MMAX = MIN0(NLAT, (NLON+2)/2)
      IERROR = 8
      IF (NDB.LT.NLAT) RETURN
      IERROR = 9
      IMID = (NLAT+1)/2
      LPIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
c     check permanent work space length
      L2 = (NLAT+1)/2
      L1 = MIN0((NLON+2)/2,NLAT)
      LP = NLAT* (3* (L1+L2)-2) + (L1-1)* (L2* (2*NLAT-L1)-3*L1)/2 +
     +     NLON + 15
      IF (LSHSGS.LT.LP) RETURN
      IERROR = 10
c
c     verify unsaved work space (add to what shses requires, file f3)
c
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
c
c     set first dimension for a,b (as requried by shses)
c
      MAB = MIN0(NLAT,NLON/2+1)
      MN = MAB*NLAT*NT
      IF (LWORK.LT.NLN+LS*NLON+2*MN+NLAT) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IA = 1
      IB = IA + MN
      IS = IB + MN
      IWK = IS + NLAT
      LWK = LWORK - 2*MN - NLAT
      CALL DDIVGS1(NLAT,NLON,ISYM,NT,DIVG,IDIV,JDIV,BR,BI,MDB,NDB,
     +            WORK(IA),WORK(IB),MAB,WORK(IS),WSHSGS,LSHSGS,
     +            WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DDIVGS1(NLAT,NLON,ISYM,NT,DIVG,IDIV,JDIV,BR,BI,MDB,NDB,
     +                  A,B,MAB,SQNN,WSHSGS,LSHSGS,WK,LWK,IERROR)
      DOUBLE PRECISION DIVG
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION WSHSGS
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION DIVG(IDIV,JDIV,NT),BR(MDB,NDB,NT),BI(MDB,NDB,NT)
      DIMENSION A(MAB,NLAT,NT),B(MAB,NLAT,NT),SQNN(NLAT)
      DIMENSION WSHSGS(LSHSGS),WK(LWK)
c
c     set coefficient multiplyers
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          SQNN(N) = SQRT(FN* (FN+1.D0))
    1 CONTINUE
c
c     compute divergence scalar coefficients for each vector field
c
      DO 2 K = 1,NT
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
              A(1,N,K) = -SQNN(N)*BR(1,N,K)
              B(1,N,K) = -SQNN(N)*BI(1,N,K)
    5     CONTINUE
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
          MMAX = MIN0(NLAT, (NLON+1)/2)
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  A(M,N,K) = -SQNN(N)*BR(M,N,K)
                  B(M,N,K) = -SQNN(N)*BI(M,N,K)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     synthesize a,b into divg
c
      CALL DSHSGS(NLAT,NLON,ISYM,NT,DIVG,IDIV,JDIV,A,B,MAB,NLAT,WSHSGS,
     +           LSHSGS,WK,LWK,IERROR)
      RETURN
      END
