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
c ... file ivrtgs.f
c
c     this file includes documentation and code for
c     subroutine ivrtgs
c
c ... files which must be loaded with ivrtgs.f
c
c     sphcom.f, hrfft.f, vhsgs.f,shags.f, gaqd.f
c
c
c     subroutine ivrtgs(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsgs,lvhsgs,work,lwork,pertrb,ierror)
c
C*PL*ERROR* Comment line too long
c     given the scalar spherical harmonic coefficients a and b, precomputed
C*PL*ERROR* Comment line too long
c     by subroutine shags for a scalar array vt, subroutine ivrtgs computes
C*PL*ERROR* Comment line too long
c     a divergence free vector field (v,w) whose vorticity is vt - pertrb.
C*PL*ERROR* Comment line too long
c     w is the east longitude component and v is the colatitudinal component.
c     pertrb is a constant which must be subtracted from vt for (v,w) to
C*PL*ERROR* Comment line too long
c     exist (see the description of pertrb below).  usually pertrb is zero
c     or small relative to vt.  the divergence of (v,w), as computed by
c     ivrtgs, is the zero scalar field.  v(i,j) and w(i,j) are the
c     colatitudinal and east longitude velocity components at gaussian
c     colatitude theta(i) (see nlat as input parameter) and longitude
c     lambda(j) = (j-1)*2*pi/nlon.  the
c
c            vorticity(v(i,j),w(i,j))
c
c         =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c         =  vort(i,j) - pertrb
c
c     and
c
c            divergence(v(i,j),w(i,j))
c
c         =  [d(sint*v)/dtheta + dw/dlambda]/sint
c
c         =  0.0
c
C*PL*ERROR* Comment line too long
c     where sint = sin(theta(i)).  required associated legendre polynomials
C*PL*ERROR* Comment line too long
c     are stored rather than recomputed as they are in subroutine ivrtgc.
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
c            than 3. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   this has the same value as the isym that was input to
c            subroutine shags to compute the arrays a and b.  isym
c            determines whether (v,w) are computed on the full or half
c            sphere as follows:
c
c      = 0
c            vt is not symmetric about the equator. in this case
c            the vector field (v,w) is computed on the entire sphere.
c            i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            vt is symmetric about the equator. in this case w is
c            antiymmetric and v is symmetric about the equator. v
c            and w are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c       = 2
c
c            vt is antisymmetric about the equator. in this case w is
c            symmetric and v is antisymmetric about the equator. w
c            and v are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c
C*PL*ERROR* Comment line too long
c     nt     in the program that calls ivrtgs, nt is the number of vorticity
C*PL*ERROR* Comment line too long
c            and vector fields.  some computational efficiency is obtained
c            for multiple fields.  the arrays a,b,v, and w can be three
c            dimensional and pertrb can be one dimensional corresponding
C*PL*ERROR* Comment line too long
c            to an indexed multiple array vt.  in this case, multiple vector
C*PL*ERROR* Comment line too long
c            synthesis will be performed to compute each vector field.  the
C*PL*ERROR* Comment line too long
c            third index for a,b,v,w and first for pertrb is the synthesis
c            index which assumes the values k=1,...,nt.  for a single
C*PL*ERROR* Comment line too long
c            synthesis set nt=1. the description of the remaining parameters
c            is simplified by assuming that nt=1 or that a,b,v,w are two
c            dimensional and pertrb is a constant.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls ivrtgs. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls ivrtgs. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the vorticity array vt as computed by subroutine shags.
c     ***    a,b must be computed by shags prior to calling ivrtgs.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls ivrtgs (and shags). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls ivrtgs (and shags). ndab must be at
c            least nlat.
c
c
c  wvhsgs    an array which must be initialized by subroutine vhsgsi.
c            once initialized
c            wvhsgs can be used repeatedly by ivrtgs as long as nlon
c            and nlat remain unchanged.  wvhsgs must not be altered
c            between calls of ivrtgs.
c
c
c  lvhsgs    the dimension of the array wvhsgs as it appears in the
c            program that calls ivrtgs. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgs must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls ivrtgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2)         if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2)     if nlon is odd
c
c            if isym = 0 then lwork must be at least
c
c               (2*nt+1)*nlat*nlon + nlat*(2*nt*l1+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               (2*nt+1)*l2*nlon + nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain a divergence free vector field whose vorticity is
c           vt - pertrb at the gaussian colatitude point theta(i)
C*PL*ERROR* Comment line too long
c           and longitude point lambda(j)=(j-1)*2*pi/nlon.  w is the east
C*PL*ERROR* Comment line too long
c           longitude component and v is the colatitudinal component.  the
c           indices for v and w are defined at the input parameter isym.
c           the divergence of (v,w) is the zero scalar field.
c
C*PL*ERROR* Comment line too long
c   pertrb  a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  vt - pertrb is a scalar
c           field which can be the vorticity of a vector field (v,w).
c           pertrb is related to the scalar harmonic coefficients a,b
c           of vt (computed by shags) by the formula
c
c                pertrb = a(1,1)/(2.*sqrt(2.))
c
c           an unperturbed vt can be the vorticity of a vector field
c           only if a(1,1) is zero.  if a(1,1) is nonzero (flagged by
c           pertrb nonzero) then subtracting pertrb from vt yields a
c           scalar field for which a(1,1) is zero.
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
c           = 9  error in the specification of lvhsgs
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DIVRTGS(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,A,B,MDAB,NDAB,
     +                  WVHSGS,LVHSGS,WORK,LWORK,PERTRB,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WVHSGS
      DOUBLE PRECISION WORK
      DOUBLE PRECISION PERTRB
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT),PERTRB(NT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WVHSGS(LVHSGS),WORK(LWORK)
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
c     lzz1 = 2*nlat*imid
c     labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     if(lvhsgs .lt. 2*(lzz1+labc)+nlon+15) return
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      IF (LVHSGS.LT.LZIMN+LZIMN+NLON+15) RETURN
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
      ICR = 1
      ICI = ICR + MN
      IS = ICI + MN
      IWK = IS + NLAT
      LIWK = LWORK - 2*MN - NLAT
      CALL DIVTGS1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,WORK(ICR),WORK(ICI),
     +            MMAX,WORK(IS),MDAB,NDAB,A,B,WVHSGS,LVHSGS,WORK(IWK),
     +            LIWK,PERTRB,IERROR)
      RETURN
      END

      SUBROUTINE DIVTGS1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,CR,CI,MMAX,
     +                   SQNN,MDAB,NDAB,A,B,WSAV,LSAV,WK,LWK,PERTRB,
     +                   IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION SQNN
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSAV
      DOUBLE PRECISION WK
      DOUBLE PRECISION PERTRB
      DOUBLE PRECISION FN
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT),PERTRB(NT)
      DIMENSION CR(MMAX,NLAT,NT),CI(MMAX,NLAT,NT),SQNN(NLAT)
      DIMENSION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)
      DIMENSION WSAV(LSAV),WK(LWK)
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
c     set vorticity field perturbation adjustment
c
          PERTRB(K) = A(1,1,K)/ (2.D0*SQRT(2.D0))
c
c     preset br,bi to 0.0
c
          DO 3 N = 1,NLAT
              DO 4 M = 1,MMAX
                  CR(M,N,K) = 0.0D0
                  CI(M,N,K) = 0.0D0
    4         CONTINUE
    3     CONTINUE
c
c     compute m=0 coefficients
c
          DO 5 N = 2,NLAT
              CR(1,N,K) = A(1,N,K)/SQNN(N)
              CI(1,N,K) = B(1,N,K)/SQNN(N)
    5     CONTINUE
c
c     compute m>0 coefficients
c
          DO 6 M = 2,MMAX
              DO 7 N = M,NLAT
                  CR(M,N,K) = A(M,N,K)/SQNN(N)
                  CI(M,N,K) = B(M,N,K)/SQNN(N)
    7         CONTINUE
    6     CONTINUE
    2 CONTINUE
c
c     set ityp for vector synthesis with divergence=0
c
      IF (ISYM.EQ.0) THEN
          ITYP = 2
      ELSE IF (ISYM.EQ.1) THEN
          ITYP = 5
      ELSE IF (ISYM.EQ.2) THEN
          ITYP = 8
      END IF
c
c     vector sythesize cr,ci into divergence free vector field (v,w)
c
      CALL DVHSGS(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,MMAX,NLAT,
     +           WSAV,LSAV,WK,LWK,IERROR)
      RETURN
      END
