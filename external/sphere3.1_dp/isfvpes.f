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
c ... file isfvpes.f
c
c     this file includes documentation and code for
c     subroutine isfvpes          i
c
c ... files which must be loaded with isfvpes.f
c
c     sphcom.f, hrfft.f, vhses.f,shaes.f
c
c
c     subroutine isfvpes(nlat,nlon,isym,nt,sf,vp,idv,jdv,as,bs,av,bv,
c    +                   mdb,ndb,wvhses,lvhses,work,lwork,ierror)
c
c     given the scalar spherical harmonic coefficients as,bs precomputed
C*PL*ERROR* Comment line too long
c     by shaes for the scalar stream function sf and av,bv precomputed by
C*PL*ERROR* Comment line too long
c     shaes for the scalar velocity potenital vp, subroutine isfvpes computes
c     the vector field (v,w) corresponding to sf and vp.  w is the east
C*PL*ERROR* Comment line too long
c     longitudinal and v is the colatitudinal component of the vector field.
C*PL*ERROR* Comment line too long
c     (v,w) is expressed in terms of sf,vp by the helmholtz relations (in
c     mathematical spherical coordinates):
c
c          v = -1/sin(theta)*d(vp)/dlambda + d(st)/dtheta
c
c          w =  1/sin(theta)*d(st)/dlambda + d(vp)/dtheta
c
c     required legendre functions are stored rather than recomputed as
c     they are in subroutine isfvpes.  v(i,j) and w(i,j) are given at
c     colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and east longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     on the sphere (pi=4.0*atan(1.0)).
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
c     isym   a parameter which determines whether the vector field is
c            computed on the full or half sphere as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in sf,vp about the equator.  in this case v
c            and w are not necessarily symmetric or antisymmetric about
c            equator.  v and w are computed on the entire sphere.
c            i.e., in arrays sf(i,j),vp(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            vp is antisymmetric and sf is symmetric about the equator.
c            in this case v is symmetric and w antisymmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the v(i,j),w(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then v(i,j),w(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            vp is symmetric and sf is antisymmetric about the equator.
c            in this case v is antisymmetric and w symmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the v(i,j),w(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then v(i,j),w(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c     nt     nt is the number of scalar and vector fields.  some
C*PL*ERROR* Comment line too long
c            computational efficiency is obtained for multiple fields. arrays
C*PL*ERROR* Comment line too long
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple vector synthesis will
c            be performed to compute (v,w) for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
C*PL*ERROR* Comment line too long
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     idv    the first dimension of the arrays v,w as it appears in
c            the program that calls isfvpes. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the arrays v,w as it appears in
c            the program that calls isfvpes. jdv must be at least nlon.
c
c     as,bs  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field sf as computed by subroutine shaes.
c
c     av,bv  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field vp as computed by subroutine shaes.
c
c     mdb    the first dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpes. mdb must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpes. ndb must be at
c            least nlat.
c
c     wvhses an array which must be initialized by subroutine vhsesi.
c            once initialized, wvhses can be used repeatedly by isfvpes
c            as long as nlon and nlat remain unchanged.  wvhses must
c            not bel altered between calls of isfvpes.
c
c
c     lvhses the dimension of the array wvhses as it appears in the
c            program that calls isfvpes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhses must be at least
c
c               l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls isfvpes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym=0 then lwork must be at least
c
c                       nlat*((2*nt+1)*nlon + 4*l1*nt + 1)
c
c            if isym=1 or 2 then lwork must be at least
c
c                       (2*nt+1)*l2*nlon + nlat*(4*l1*nt + 1)
c
c
c     **************************************************************
c
c     output parameters
c
c    v,w    two or three dimensional arrays (see input parameter nt)
c           that contains the vector field corresponding to the stream
c           function sf and velocity potential vp whose coefficients,
c           as,bs (for sf) and av,bv (for vp), were precomputed by
c           subroutine shaes.  v(i,j) and w(i,j) are given at the
c           colatitude point
c
c                theta(i) = (i-1)*pi/(nlat-1)
c
c           and longitude point
c
c                lambda(j) = (j-1)*2*pi/nlon
c
C*PL*ERROR* Comment line too long
c           the index ranges are defined above at the input parameter isym.
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
c           = 9  error in the specification of lvhses
c           = 10 error in the specification of lwork
c **********************************************************************
c
      SUBROUTINE DISFVPES(NLAT,NLON,ISYM,NT,V,W,IDV,JDV,AS,BS,AV,BV,MDB,
     +                   NDB,WVHSES,LVHSES,WORK,LWORK,IERROR)
      IMPLICIT NONE
      INTEGER NLAT,NLON,ISYM,NT,IDV,JDV,MDB,NDB,LVHSES,LWORK,IERROR
      DOUBLE PRECISION V(IDV,JDV,NT),W(IDV,JDV,NT)
      DOUBLE PRECISION AS(MDB,NDB,NT),BS(MDB,NDB,NT)
      DOUBLE PRECISION AV(MDB,NDB,NT),BV(MDB,NDB,NT)
      DOUBLE PRECISION WVHSES(LVHSES),WORK(LWORK)
      INTEGER L1,L2,MN,IS,LWK,IWK
      INTEGER IBR,IBI,ICR,ICI
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
      L2 = (NLAT+1)/2
      IF ((ISYM.EQ.0.AND.IDV.LT.NLAT) .OR.
     +    (ISYM.GT.0.AND.IDV.LT.L2)) RETURN
      IERROR = 6
      IF (JDV.LT.NLON) RETURN
      IERROR = 7
      L1 = MIN0(NLAT, (NLON+1)/2)
      IF (MDB.LT.MIN0(NLAT, (NLON+2)/2)) RETURN
      IERROR = 8
      IF (NDB.LT.NLAT) RETURN
      IERROR = 9
      L1 = MIN0(NLAT, (NLON+2)/2)
      IF (LVHSES.LT.L1*L2* (NLAT+NLAT-L1+1)+NLON+15) RETURN
      IERROR = 10
      IF (ISYM.EQ.0) THEN
          IF (LWORK.LT.NLAT* ((2*NT+1)*NLON+4*L1*NT+1)) RETURN
      ELSE
          IF (LWORK.LT. (2*NT+1)*NLON+NLAT* (4*L1*NT+1)) RETURN
      END IF
c
c     set first dimension for br,bi,cr,ci (as requried by vhses)
c
      MN = L1*NLAT*NT
      IERROR = 0
c
c     set work space pointers
c
      IBR = 1
      IBI = IBR + MN
      ICR = IBI + MN
      ICI = ICR + MN
      IS = ICI + MN
      IWK = IS + NLAT
      LWK = LWORK - 4*MN - NLAT
      CALL DISFVPES1(NLAT,NLON,ISYM,NT,V,W,IDV,JDV,AS,BS,AV,BV,MDB,NDB,
     +              WORK(IBR),WORK(IBI),WORK(ICR),WORK(ICI),L1,WORK(IS),
     +              WVHSES,LVHSES,WORK(IWK),LWK,IERROR)
      RETURN
      END

      SUBROUTINE DISFVPES1(NLAT,NLON,ISYM,NT,V,W,IDV,JDV,AS,BS,AV,BV,
     +                     MDB,NDB,BR,BI,CR,CI,MAB,FNN,WVHSES,LVHSES,
     +                     WK,LWK,IERROR)
      IMPLICIT NONE
      INTEGER NLAT,NLON,ISYM,NT,IDV,JDV,MDB,NDB,MAB,LVHSES,LWK,IERROR
      DOUBLE PRECISION V(IDV,JDV,NT),W(IDV,JDV,NT)
      DOUBLE PRECISION AS(MDB,NDB,NT),BS(MDB,NDB,NT)
      DOUBLE PRECISION AV(MDB,NDB,NT),BV(MDB,NDB,NT)
      DOUBLE PRECISION BR(MAB,NLAT,NT),BI(MAB,NLAT,NT)
      DOUBLE PRECISION CR(MAB,NLAT,NT),CI(MAB,NLAT,NT)
      DOUBLE PRECISION WVHSES(LVHSES),WK(LWK),FNN(NLAT)
      INTEGER N,M,MMAX,K,ITYP
c
c     set coefficient multiplyers
c
      DO N = 2,NLAT
          FNN(N) = -SQRT(DBLE(N* (N-1)))
      END DO
      MMAX = MIN0(NLAT, (NLON+1)/2)
c
c     compute (v,w) coefficients from as,bs,av,bv
c
      DO K = 1,NT
          DO N = 1,NLAT
              DO M = 1,MAB
                  BR(M,N,K) = 0.0D0
                  BI(M,N,K) = 0.0D0
                  CR(M,N,K) = 0.0D0
                  CI(M,N,K) = 0.0D0
              END DO
          END DO
c
c     compute m=0 coefficients
c
          DO N = 2,NLAT
              BR(1,N,K) = -FNN(N)*AV(1,N,K)
              BI(1,N,K) = -FNN(N)*BV(1,N,K)
              CR(1,N,K) = FNN(N)*AS(1,N,K)
              CI(1,N,K) = FNN(N)*BS(1,N,K)
          END DO
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
          DO M = 2,MMAX
              DO N = M,NLAT
                  BR(M,N,K) = -FNN(N)*AV(M,N,K)
                  BI(M,N,K) = -FNN(N)*BV(M,N,K)
                  CR(M,N,K) = FNN(N)*AS(M,N,K)
                  CI(M,N,K) = FNN(N)*BS(M,N,K)
              END DO
          END DO
      END DO
c
c     synthesize br,bi,cr,ci into (v,w)
c
      IF (ISYM.EQ.0) THEN
          ITYP = 0
      ELSE IF (ISYM.EQ.1) THEN
          ITYP = 3
      ELSE IF (ISYM.EQ.2) THEN
          ITYP = 6
      END IF
      CALL DVHSES(NLAT,NLON,ITYP,NT,V,W,IDV,JDV,BR,BI,CR,CI,MAB,NLAT,
     +           WVHSES,LVHSES,WK,LWK,IERROR)
      RETURN
      END
