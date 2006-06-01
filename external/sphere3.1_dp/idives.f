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
c ... file idives.f
c
c     this file includes documentation and code for
c     subroutine idives          i
c
c ... files which must be loaded with idivec.f
c
c     sphcom.f, hrfft.f, vhses.f,shaes.f
c
c
c     subroutine idives(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhses,lvhses,work,lwork,pertrb,ierror)
c
C*PL*ERROR* Comment line too long
c     given the scalar spherical harmonic coefficients a and b, precomputed
C*PL*ERROR* Comment line too long
c     by subroutine shaes for a scalar array dv, subroutine idives computes
C*PL*ERROR* Comment line too long
c     an irrotational vector field (v,w) whose divergence is dv - pertrb.
C*PL*ERROR* Comment line too long
c     w is the east longitude component and v is the colatitudinal component.
c     pertrb is a constant which must be subtracted from dv for (v,w) to
C*PL*ERROR* Comment line too long
c     exist (see the description of pertrb below).  usually pertrb is zero
c     or small relative to dv.  the vorticity of (v,w), as computed by
c     vortes, is the zero scalar field.  i.e., v(i,j) and w(i,j) are the
c     velocity components at colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon.
c
c     the
c
c            divergence[v(i,j),w(i,j)]
c
c          = [d(w(i,j)/dlambda + d(sint*v(i,j))/dtheta]/sint
c
c          = dv(i,j) - pertrb
c
c     and
c
c            vorticity(v(i,j),w(i,j))
c
c         =  [dv/dlambda - d(sint*w)/dtheta]/sint
c
c         =  0.0
c
C*PL*ERROR* Comment line too long
c     where sint = sin(theta(i)).  required associated legendre polynomials
C*PL*ERROR* Comment line too long
c     are stored rather than recomputed as they are in subroutine idivec.
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
c            subroutine shaes to compute the arrays a and b from the
c            scalar field dv.  isym determines whether (v,w) are
c            computed on the full or half sphere as follows:
c
c      = 0
c
c           dv is not symmetric about the equator. in this case
c           the vector field (v,w) is computed on the entire sphere.
c           i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c           j=1,...,nlon.
c
c      = 1
c
c           dv is antisymmetric about the equator. in this case w is
c           antisymmetric and v is symmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c
c           dv is symmetric about the equator. in this case w is
c           symmetric and v is antisymmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of divergence and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            the arrays a,b,v, and w can be three dimensional and pertrb
c            can be one dimensional corresponding to an indexed multiple
c            array dv.  in this case, multiple vector synthesis will be
C*PL*ERROR* Comment line too long
c            performed to compute each vector field.  the third index for
c            a,b,v,w and first for pertrb is the synthesis index which
C*PL*ERROR* Comment line too long
c            assumes the values k = 1,...,nt.  for a single synthesis set
c            nt = 1.  the description of the remaining parameters is
c            simplified by assuming that nt=1 or that a,b,v,w are two
c            dimensional and pertrb is a constant.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls idives. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls idives. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the divergence array dv as computed by subroutine shaes.
c     ***    a,b must be computed by shaes prior to calling idives.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls idives (and shaes). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls idives (and shaes). ndab must be at
c            least nlat.
c
c
c  wvhses    an array which must be initialized by subroutine vhesesi.
c            once initialized,
c            wvhses can be used repeatedly by idives as long as nlon
c            and nlat remain unchanged.  wvhses must not be altered
c            between calls of idives.
c
c
c  lvhses    the dimension of the array wvhses as it appears in the
c            program that calls idives. define
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
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls idives. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                  if nlat is even or
c               l2 = (nlat+1)/2              if nlat is odd
c
c            if isym = 0 then lwork must be at least
c
c               nlat*((2*nt+1)*nlon+2*nt*l1+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               (2*nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain an irrotational vector field whose divergence is
c           dv-pertrb at the colatitude point theta(i)=(i-1)*pi/(nlat-1)
C*PL*ERROR* Comment line too long
c           and longitude point lambda(j)=(j-1)*2*pi/nlon.  w is the east
C*PL*ERROR* Comment line too long
c           longitude component and v is the colatitudinal component.  the
c           indices for w and v are defined at the input parameter isym.
C*PL*ERROR* Comment line too long
c           the curl or vorticity of (v,w) is the zero vector field.  note
c           that any nonzero vector field on the sphere will be multiple
c           valued at the poles [reference swarztrauber].
c
C*PL*ERROR* Comment line too long
c   pertrb  a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  dv - pertrb is a scalar
c           field which can be the divergence of a vector field (v,w).
c           pertrb is related to the scalar harmonic coefficients a,b
c           of dv (computed by shaes) by the formula
c
c                pertrb = a(1,1)/(2.*sqrt(2.))
c
c
c
c           the unperturbed scalar field dv can be the divergence of a
c           vector field only if a(1,1) is zero.  if a(1,1) is nonzero
c           (flagged by pertrb nonzero) then subtracting pertrb from
c           dv yields a scalar field for which a(1,1) is zero.
c
c    ierror = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idvw
c           = 6  error in the specification of jdvw
c           = 7  error in the specification of mdab
c           = 8  error in the specification of ndab
c           = 9  error in the specification of lvhses
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DIDIVES(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,A,B,MDAB,NDAB,
     +                  WVHSES,LVHSES,WORK,LWORK,PERTRB,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WVHSES
      DOUBLE PRECISION WORK
      DOUBLE PRECISION PERTRB
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT),PERTRB(NT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WVHSES(LVHSES),WORK(LWORK)
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
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      IF (LVHSES.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 10
c
c     verify unsaved work space length
c
      MN = MMAX*NLAT*NT
      IF (ISYM.NE.0 .AND. LWORK.LT.NLAT* (2*NT*NLON+MAX0(6*IMID,NLON))+
     +    2*MN+NLAT) RETURN
      IF (ISYM.EQ.0 .AND. LWORK.LT.IMID* (2*NT*NLON+MAX0(6*NLAT,NLON))+
     +    2*MN+NLAT) RETURN
      IERROR = 0
c
c     set work space pointers
c
      IBR = 1
      IBI = IBR + MN
      IS = IBI + MN
      IWK = IS + NLAT
      LIWK = LWORK - 2*MN - NLAT
      CALL DIDVES1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,WORK(IBR),WORK(IBI),
     +            MMAX,WORK(IS),MDAB,NDAB,A,B,WVHSES,LVHSES,WORK(IWK),
     +            LIWK,PERTRB,IERROR)
      RETURN
      END

      SUBROUTINE DIDVES1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,BR,BI,MMAX,
     +                   SQNN,MDAB,NDAB,A,B,WSAV,LWSAV,WK,LWK,PERTRB,
     +                   IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSAV
      DOUBLE PRECISION WK
      DOUBLE PRECISION PERTRB
      DOUBLE PRECISION FN
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT),PERTRB(NT)
      DIMENSION BR(MMAX,NLAT,NT),BI(MMAX,NLAT,NT),SQNN(NLAT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WSAV(LWSAV),WK(LWK)
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
c     set divergence field perturbation adjustment
c
          PERTRB(K) = A(1,1,K)/ (2.D0*SQRT(2.D0))
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
              BR(1,N,K) = -A(1,N,K)/SQNN(N)
              BI(1,N,K) = -B(1,N,K)/SQNN(N)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  BR(M,N,K) = -A(M,N,K)/SQNN(N)
                  BI(M,N,K) = -B(M,N,K)/SQNN(N)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     set ityp for vector synthesis with curl=0
c
      IF (ISYM.EQ.0) THEN
          ITYP = 1
      ELSE IF (ISYM.EQ.1) THEN
          ITYP = 4
      ELSE IF (ISYM.EQ.2) THEN
          ITYP = 7
      END IF
c
c     vector sythesize br,bi into irrotational (v,w)
c
      CALL DVHSES(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,MMAX,NLAT,
     +           WSAV,LWSAV,WK,LWK,IERROR)
      RETURN
      END
