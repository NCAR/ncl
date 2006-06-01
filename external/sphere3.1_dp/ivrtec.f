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
c ... file ivrtec.f
c
c     this file includes documentation and code for
c     subroutine ivrtec         i
c
c ... files which must be loaded with ivrtec.f
c
c     sphcom.f, hrfft.f, vhsec.f,shaec.f
c
c     subroutine ivrtec(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsec,lvhsec,work,lwork,pertrb,ierror)
c
C*PL*ERROR* Comment line too long
c     given the scalar spherical harmonic coefficients a and b, precomputed
C*PL*ERROR* Comment line too long
c     by subroutine shaec for a scalar array vort, subroutine ivrtec computes
C*PL*ERROR* Comment line too long
c     a divergence free vector field (v,w) whose vorticity is vt - pertrb.
C*PL*ERROR* Comment line too long
c     w is the east longitude component and v is the colatitudinal component.
C*PL*ERROR* Comment line too long
c     pertrb is a constant which must be subtracted from vort for (v,w) to
C*PL*ERROR* Comment line too long
c     exist (see the description of pertrb below).  usually pertrb is zero
C*PL*ERROR* Comment line too long
c     or small relative to vort.  the divergence of (v,w), as computed by
c     ivrtec, is the zero scalar field.  i.e., v(i,j) and w(i,j) are the
C*PL*ERROR* Comment line too long
c     colaatitudinal and east longitude velocity components at colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon.
c
c     the
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
c     are recomputed rather than stored as they are in subroutine ivrtes.
c
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
c            nlon = 72 for a five degree grid. nlon must be greater
c            than 3. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   this has the same value as the isym that was input to
c            subroutine shaec to compute the arrays a and b.  isym
c            determines whether (v,w) are computed on the full or half
c            sphere as follows:
c
c      = 0
c            vort is not symmetric about the equator. in this case
c            the vector field (v,w) is computed on the entire sphere.
c            i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            vort is symmetric about the equator. in this case w is
c            antiymmetric and v is symmetric about the equator. v
c            and w are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c       = 2
c
c            vort is antisymmetric about the equator. in this case w is
c            symmetric and v is antisymmetric about the equator. w
c            and v are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c
C*PL*ERROR* Comment line too long
c     nt     in the program that calls ivrtec, nt is the number of vorticity
C*PL*ERROR* Comment line too long
c            and vector fields.  some computational efficiency is obtained
c            for multiple fields.  the arrays a,b,v, and w can be three
c            dimensional and pertrb can be one dimensional corresponding
C*PL*ERROR* Comment line too long
c            to an indexed multiple array vort.  in this case, multiple vector
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
c            the program that calls ivrtec. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls ivrtec. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
C*PL*ERROR* Comment line too long
c            of the vorticity array vort as computed by subroutine shaec.
c     ***    a,b must be computed by shaec prior to calling ivrtec.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls ivrtec (and shaec). mdab must be at
c            least min0(nlat,(nlon+2/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls ivrtec (and shaec). ndab must be at
c            least nlat.
c
c
c  wvhsec    an array which must be initialized by subroutine vhseci.
c            once initialized
c            wvhsec can be used repeatedly by ivrtec as long as nlon
c            and nlat remain unchanged.  wvhsec must not be altered
c            between calls of ivrtec.
c
c
c  lvhsec    the dimension of the array wvhsec as it appears in the
c            program that calls ivrtec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsec must be at least
c
c            4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls ivrtec. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2  )       if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2)     if nlon is odd
c
c            if isym = 0 then lwork must be at least
c
c               nlat*(2*nt*nlon+max0(6*l2,nlon) + 2*nt*l1 + 1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               l2*(2*nt*nlon+max0(6*nlat,nlon)) + nlat*(2*nt*l1+1)
c
c     **************************************************************
c
c     output parameters
c
c
C*PL*ERROR* Comment line too long
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain a divergence free vector field whose vorticity is
C*PL*ERROR* Comment line too long
c           vort - pertrb at the lattitude point theta(i)=pi/2-(i-1)*pi/(nlat-1)
C*PL*ERROR* Comment line too long
c           and longitude point lambda(j)=(j-1)*2*pi/nlon.  w is the east
C*PL*ERROR* Comment line too long
c           longitude component and v is the colatitudinal component.  the
c           indices for v and w are defined at the input parameter isym.
c           the divergence of (v,w) is the zero scalar field.
c
C*PL*ERROR* Comment line too long
c   pertrb  a nt dimensional array (see input parameter nt and assume nt=1
C*PL*ERROR* Comment line too long
c           for the description that follows).  vort - pertrb is a scalar
c           field which can be the vorticity of a vector field (v,w).
c           pertrb is related to the scalar harmonic coefficients a,b
c           of vort (computed by shaec) by the formula
c
c                pertrb = a(1,1)/(2.*sqrt(2.))
c
c           an unperturbed vort can be the vorticity of a vector field
c           only if a(1,1) is zero.  if a(1,1) is nonzero (flagged by
c           pertrb nonzero) then subtracting pertrb from vort yields a
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
c           = 9  error in the specification of lvhsec
c           = 10 error in the specification of lwork
c **********************************************************************
c
c
      SUBROUTINE DIVRTEC(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,A,B,MDAB,NDAB,
     +                  WVHSEC,LVHSEC,WORK,LWORK,PERTRB,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WVHSEC
      DOUBLE PRECISION WORK
      DOUBLE PRECISION PERTRB
      DIMENSION V(IDVW,JDVW,NT),W(IDVW,JDVW,NT),PERTRB(NT)
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
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LVHSEC.LT.2* (LZZ1+LABC)+NLON+15) RETURN
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
      CALL DIVTEC1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,WORK(ICR),WORK(ICI),
     +            MMAX,WORK(IS),MDAB,NDAB,A,B,WVHSEC,LVHSEC,WORK(IWK),
     +            LIWK,PERTRB,IERROR)
      RETURN
      END

      SUBROUTINE DIVTEC1(NLAT,NLON,ISYM,NT,V,W,IDVW,JDVW,CR,CI,MMAX,
     +                   SQNN,MDAB,NDAB,A,B,WSAV,LWSAV,WK,LWK,PERTRB,
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
      DIMENSION WSAV(LWSAV),WK(LWK)
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
c     preset cr,ci to 0.0
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
      CALL DVHSEC(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,MMAX,NLAT,
     +           WSAV,LWSAV,WK,LWK,IERROR)
      RETURN
      END
