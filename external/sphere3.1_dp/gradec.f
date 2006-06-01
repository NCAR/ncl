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
c ... file gradec.f
c
c     this file includes documentation and code for
c     subroutine gradec         i
c
c ... files which must be loaded with gradec.f
c
c     sphcom.f, hrfft.f, shaec.f,vhsec.f
c
c     subroutine gradec(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsec,lvhsec,work,lwork,ierror)
c
C*PL*ERROR* Comment line too long
c     given the scalar spherical harmonic coefficients a and b, precomputed
C*PL*ERROR* Comment line too long
c     by subroutine shaec for a scalar field sf, subroutine gradec computes
c     an irrotational vector field (v,w) such that
c
c           gradient(sf) = (v,w).
c
c     v is the colatitudinal and w is the east longitudinal component
c     of the gradient.  i.e.,
c
c            v(i,j) = d(sf(i,j))/dtheta
c
c     and
c
c            w(i,j) = 1/sint*d(sf(i,j))/dlambda
c
c     at colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon.
c
C*PL*ERROR* Comment line too long
c     where sint = sin(theta(i)).  required associated legendre polynomials
C*PL*ERROR* Comment line too long
c     are recomputed rather than stored as they are in subroutine grades. this
C*PL*ERROR* Comment line too long
c     saves storage (compare wvhsec here and wvhses in grades) but increases
c     computational requirements.
c
c
c     input parameters
c
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
c     isym   this has the same value as the isym that was input to
c            subroutine shaec to compute the arrays a and b from the
c            scalar field sf.  isym determines whether (v,w) are
c            computed on the full or half sphere as follows:
c
c      = 0
c
c           sf is not symmetric about the equator. in this case
c           the vector field (v,w) is computed on the entire sphere.
c           i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c           j=1,...,nlon.
c
c      = 1
c
c           sf is antisymmetric about the equator. in this case w is
c           antisymmetric and v is symmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c
c           sf is symmetric about the equator. in this case w is
c           symmetric and v is antisymmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
C*PL*ERROR* Comment line too long
c            the arrays a,b,v, and w can be three dimensional corresponding
c            to an indexed multiple array sf.  in this case, multiple
c            vector synthesis will be performed to compute each vector
c            field.  the third index for a,b,v, and w is the synthesis
c            index which assumes the values k = 1,...,nt.  for a single
c            synthesis set nt = 1.  the description of the remaining
C*PL*ERROR* Comment line too long
c            parameters is simplified by assuming that nt=1 or that a,b,v,
c            and w are two dimensional arrays.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls gradec. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls gradec. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
C*PL*ERROR* Comment line too long
c            of the scalar field array sf as computed by subroutine shaec.
c     ***    a,b must be computed by shaec prior to calling gradec.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls gradec (and shaec). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls gradec (and shaec). ndab must be at
c            least nlat.
c
c
c     wvhsec an array which must be initialized by subroutine vhseci.
c            once initialized,
c            wvhsec can be used repeatedly by gradec as long as nlon
c            and nlat remain unchanged.  wvhsec must not be altered
c            between calls of gradec.
c
c
c     lvhsec the dimension of the array wvhsec as it appears in the
c            program that calls gradec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            then lvhsec must be greater than or equal to
c
c               4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls gradec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                  if nlat is even or
c               l2 = (nlat+1)/2              if nlat is odd
c
c
c            if isym = 0 then lwork must be at least
c
c                nlat*(2*nt*nlon+max0(6*l2,nlon)) + nlat*(2*l1*nt+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c                l2*(2*nt*nlon+max0(6*nlat,nlon)) + nlat*(2*l1*nt+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c     v,w   two or three dimensional arrays (see input parameter nt) that
C*PL*ERROR* Comment line too long
c           contain an irrotational vector field such that the gradient of
c           the scalar field sf is (v,w).  w(i,j) is the east longitude
C*PL*ERROR* Comment line too long
c           component and v(i,j) is the colatitudinal component of velocity
c           at colatitude theta(i) = (i-1)*pi/(nlat-1) and longitude
C*PL*ERROR* Comment line too long
c           lambda(j) = (j-1)*2*pi/nlon. the indices for v and w are defined
C*PL*ERROR* Comment line too long
c           at the input parameter isym.  the vorticity of (v,w) is zero.
c           note that any nonzero vector field on the sphere will be
c           multiple valued at the poles [reference swarztrauber].
c
c
c  ierror   = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idvw
c           = 6  error in the specification of jdvw
c           = 7  error in the specification of mdab
c           = 8  error in the specification of ndab
c           = 9  error in the specification of lvhsec
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DGRADEC(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,A,B,MDAB,NDAB,
     +                  WVHSEC,LVHSEC,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WVHSEC
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WVHSEC(LVHSEC),WORK(LWORK)
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
      IF ((ISYM.EQ.0.AND.IDVW.LT.NLAT) .OR.
     +    (ISYM.NE.0.AND.IDVW.LT.IMID)) RETURN
      IERROR = 6
      IF (JDVW.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MDAB.LT.MIN0(NLAT, (NLON+2)/2)) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
c
c     verify minimum saved work space length
c
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      L1 = MIN0(NLAT, (NLON+1)/2)
      L2 = (NLAT+1)/2
      LWMIN = 4*NLAT*L2 + 3*MAX0(L1-2,0)* (NLAT+NLAT-L1-1) + NLON + 15
      IF (LVHSEC.LT.LWMIN) RETURN
      IERROR = 10
c
c     verify minimum unsaved work space length
c
      MN = MMAX*NLAT*NT
      IF (ISYM.EQ.0) THEN
          LWKMIN = NLAT* (2*NT*NLON+MAX0(6*L2,NLON)) + NLAT* (2*L1*NT+1)
      ELSE
          LWKMIN = L2* (2*NT*NLON+MAX0(6*NLAT,NLON)) + NLAT* (2*L1*NT+1)
      END IF
      IF (LWORK.LT.LWKMIN) RETURN

      IERROR = 0
c
c     set work space pointers
c
      IBR = 1
      IBI = IBR + MN
      IS = IBI + MN
      IWK = IS + NLAT
      LIWK = LWORK - 2*MN - NLAT
      CALL DGRADEC1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,WORK(IBR),WORK(IBI),
     +             MMAX,WORK(IS),MDAB,NDAB,A,B,WVHSEC,LVHSEC,WORK(IWK),
     +             LIWK,IERROR)
      RETURN
      END

      SUBROUTINE DGRADEC1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,BR,BI,MMAX,
     +                   SQNN,MDAB,NDAB,A,B,WVHSEC,LVHSEC,WK,LWK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WVHSEC
      DOUBLE PRECISION WK
      DOUBLE PRECISION FN
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT)
      DIMENSION BR(MMAX,NLAT,NT),BI(MMAX,NLAT,NT),SQNN(NLAT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WVHSEC(LVHSEC),WK(LWK)
c
c     preset coefficient multiplyers in vector
c
      DO 1 N = 2,NLAT
          FN = DBLE(N-1)
          SQNN(N) = SQRT(FN* (FN+1.D0))
    1 CONTINUE
c
c     compute multiple vector fields coefficients
c
      DO 2 K = 1,NT
c
c     preset br,bi to 0.0
c
          DO 3 N = 1,NLAT
              DO 4 M = 1,MMAX
                  BR(M,N,K) = 0.0D0
                  BI(M,N,K) = 0.0D0
    4         CONTINUE
    3     CONTINUE
c
c     compute m=0 coefficients
c
          DO 5 N = 2,NLAT
              BR(1,N,K) = SQNN(N)*A(1,N,K)
              BI(1,N,K) = SQNN(N)*B(1,N,K)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  BR(M,N,K) = SQNN(N)*A(M,N,K)
                  BI(M,N,K) = SQNN(N)*B(M,N,K)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     set ityp for irrotational vector synthesis to compute gradient
c
      IF (ISYM.EQ.0) THEN
          ITYP = 1
      ELSE IF (ISYM.EQ.1) THEN
          ITYP = 4
      ELSE IF (ISYM.EQ.2) THEN
          ITYP = 7
      END IF
c
c     vector sythesize br,bi into (v,w) (cr,ci are dummy variables)
c
      CALL DVHSEC(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,MMAX,NLAT,
     +           WVHSEC,LVHSEC,WK,LWK,IERROR)
      RETURN
      END
