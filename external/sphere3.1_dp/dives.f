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
c ... file dives.f
c
c     this file includes documentation and code for
c     subroutine dives          i
c
c ... files which must be loaded with dives.f
c
c     sphcom.f, hrfft.f, vhaes.f,shses.f
c
c
c     subroutine dives(nlat,nlon,isym,nt,dv,idv,jdv,br,bi,mdb,ndb,
c    +                 wshses,lshses,work,lwork,ierror)
c
C*PL*ERROR* Comment line too long
c     given the vector spherical harmonic coefficients br and bi, precomputed
c     by subroutine vhaes for a vector field (v,w), subroutine dives
C*PL*ERROR* Comment line too long
c     computes the divergence of the vector field in the scalar array dv.
c     dv(i,j) is the divergence at the colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and east longitude
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
c     br,bi were precomputed.  required associated legendre polynomials
c     are stored rather than recomputed as they are in subroutine divec.
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater than
c            3.  the efficiency of the computation is improved when nlon
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
c            sphere.  i.e., in the array dv(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the divergence is antisymmetyric about
c            the equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the divergence is computed
c            in the array dv(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the divergence is computed
c            in the array dv(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the divergence is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the divergence is computed
c            in the array dv(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the divergence is computed
c            in the array dv(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
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
c     idv    the first dimension of the array dv as it appears in
c            the program that calls dives. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the array dv as it appears in
c            the program that calls dives. jdv must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhaes.
c     ***    br and bi must be computed by vhaes prior to calling
c            dives.
c
c     mdb    the first dimension of the arrays br and bi as it
c            appears in the program that calls dives. mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br and bi as it
c            appears in the program that calls dives. ndb must be at
c            least nlat.
c
c
c     wshses an array which must be initialized by subroutine shsesi
c            once initialized,
c            wshses can be used repeatedly by dives as long as nlon
c            and nlat remain unchanged.  wshses must not be altered
c            between calls of dives.  wdives is identical to the saved
c            work space initialized by subroutine shsesi and can be
c            set by calling that subroutine instead of divesi.
c
c
c     lshses the dimension of the array wshses as it appears in the
c            program that calls dives. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshses must be at least
c
c              (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls dives. define
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
c    dv     a two or three dimensional array (see input parameter nt)
c           that contains the divergence of the vector field (v,w)
c           whose coefficients br,bi where computed by subroutine
c           vhaes.  dv(i,j) is the divergence at the colatitude point
c           theta(i) = (i-1)*pi/(nlat-1) and longitude point
c           lambda(j) = (j-1)*2*pi/nlon. the index ranges are defined
c           above at the input parameter isym.
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
c           = 9  error in the specification of lshses
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DDIVES(NLAT,NLON,ISYM,NT,DV,IDV,JDV,BR,BI,MDB,NDB,
     +                  WSHSES,LSHSES,WORK,LWORK,IERROR)
      DOUBLE PRECISION DV
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION WSHSES
      DOUBLE PRECISION WORK

      DIMENSION DV(IDV,JDV,NT),BR(MDB,NDB,NT),BI(MDB,NDB,NT)
      DIMENSION WSHSES(LSHSES),WORK(LWORK)
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
      MMAX = MIN0(NLAT, (NLON+2)/2)
      IERROR = 8
      IF (NDB.LT.NLAT) RETURN
      IERROR = 9
c
c     verify save work space (same as shes, file f3)
c
      IMID = (NLAT+1)/2
      LPIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
      IF (LSHSES.LT.LPIMN+NLON+15) RETURN
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
      CALL DDIVES1(NLAT,NLON,ISYM,NT,DV,IDV,JDV,BR,BI,MDB,NDB,WORK(IA),
     +            WORK(IB),MAB,WORK(IS),WSHSES,LSHSES,WORK(IWK),LWK,
     +            IERROR)
      RETURN
      END

      SUBROUTINE DDIVES1(NLAT,NLON,ISYM,NT,DV,IDV,JDV,BR,BI,MDB,NDB,A,B,
     +                  MAB,SQNN,WSHSES,LSHSES,WK,LWK,IERROR)
      DOUBLE PRECISION DV
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION WSHSES
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DIMENSION DV(IDV,JDV,NT),BR(MDB,NDB,NT),BI(MDB,NDB,NT)
      DIMENSION A(MAB,NLAT,NT),B(MAB,NLAT,NT),SQNN(NLAT)
      DIMENSION WSHSES(LSHSES),WK(LWK)
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
c     synthesize a,b into dv
c
      CALL DSHSES(NLAT,NLON,ISYM,NT,DV,IDV,JDV,A,B,MAB,NLAT,WSHSES,
     +           LSHSES,WK,LWK,IERROR)
      RETURN
      END
