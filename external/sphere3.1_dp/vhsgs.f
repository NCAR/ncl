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
c ... file vhsgs.f
c
c     this file contains code and documentation for subroutines
c     vhsgs and vhsgsi
c
c ... files which must be loaded with vhsgs.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine vhsgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhsgs,lvhsgs,work,lwork,ierror)
c
c
c     subroutine vhsgs performs the vector spherical harmonic synthesis
c     of the arrays br, bi, cr, and ci and stores the result in the
c     arrays v and w.  the synthesis is performed on an equally spaced
c     longitude grid and a gaussian colatitude grid (measured from
c     the north pole). v(i,j) and w(i,j) are the colatitudinal and
c     east longitudinal components respectively, located at the i(th)
c     colatitude gaussian point (see nlat below) and longitude
c     phi(j) = (j-1)*2*pi/nlon.  the spectral respresentation of (v,w)
c     is given below at output parameters v,w.
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
c     ityp   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c            = 1  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon. the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 2  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon. the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c            = 3  v is symmetric and w is antisymmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  v is symmetric and w is antisymmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 5  v is symmetric and w is antisymmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c            = 6  v is antisymmetric and w is symmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  v is antisymmetric and w is symmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 8  v is antisymmetric and w is symmetric about the
c                 equator. the synthesis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the synthesis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the synthesis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c
c     nt     the number of syntheses.  in the program that calls vhsgs,
c            the arrays v,w,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhags. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhsgs. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given
c            below at the discription of output parameters v and w.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhsgs. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhsgs. ndab must be at
c            least nlat.
c
c     wvhsgs an array which must be initialized by subroutine vhsgsi.
c            once initialized, wvhsgs can be used repeatedly by vhsgs
c            as long as nlon and nlat remain unchanged.  wvhsgs must
c            not be altered between calls of vhsgs.
c
c     lvhsgs the dimension of the array wvhsgs as it appears in the
c            program that calls vhsgs. define
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
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhsgs. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c                       (2*nt+1)*nlat*nlon
c
c            if ityp .gt. 2 then lwork must be at least
c
c                        (2*nt+1)*l2*nlon
c
c     **************************************************************
c
c     output parameters
c
c     v,w    two or three dimensional arrays (see input parameter nt)
c            in which the synthesis is stored. v is the colatitudinal
c            component and w is the east longitudinal component.
C*PL*ERROR* Comment line too long
c            v(i,j),w(i,j) contain the components at the guassian colatitude
c            point theta(i) and longitude phi(j) = (j-1)*2*pi/nlon.
c            the index ranges are defined above at the input parameter
c            ityp. v and w are computed from the formulas given below.
c
c
c     define
c
c     1.  theta is colatitude and phi is east longitude
c
c     2.  the normalized associated legendre funnctions
c
c         pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)
c                        /(2*factorial(n+m)))*sin(theta)**m/(2**n*
c                        factorial(n)) times the (n+m)th derivative
c                        of (x**2-1)**n with respect to x=cos(theta)
c
c     3.  vbar(m,n,theta) = the derivative of pbar(m,n,theta) with
c                           respect to theta divided by the square
c                           root of n(n+1).
c
c         vbar(m,n,theta) is more easily computed in the form
c
c         vbar(m,n,theta) = (sqrt((n+m)*(n-m+1))*pbar(m-1,n,theta)
c         -sqrt((n-m)*(n+m+1))*pbar(m+1,n,theta))/(2*sqrt(n*(n+1)))
c
c     4.  wbar(m,n,theta) = m/(sin(theta))*pbar(m,n,theta) divided
c                           by the square root of n(n+1).
c
c         wbar(m,n,theta) is more easily computed in the form
c
c         wbar(m,n,theta) = sqrt((2n+1)/(2n-1))*(sqrt((n+m)*(n+m-1))
c         *pbar(m-1,n-1,theta)+sqrt((n-m)*(n-m-1))*pbar(m+1,n-1,theta))
c         /(2*sqrt(n*(n+1)))
c
c
c    the colatitudnal dependence of the normalized surface vector
c                spherical harmonics are defined by
c
c     5.    bbar(m,n,theta) = (vbar(m,n,theta),i*wbar(m,n,theta))
c
c     6.    cbar(m,n,theta) = (i*wbar(m,n,theta),-vbar(m,n,theta))
c
c
c    the coordinate to index mappings
c
C*PL*ERROR* Comment line too long
c     7.   theta(i) = i(th) gaussian grid point and phi(j) = (j-1)*2*pi/nlon
c
c
c     the maximum (plus one) longitudinal wave number
c
c     8.     mmax = min0(nlat,nlon/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    if we further define the output vector as
c
c     9.    h(i,j) = (v(i,j),w(i,j))
c
c    and the complex coefficients
c
c     10.   b(m,n) = cmplx(br(m+1,n+1),bi(m+1,n+1))
c
c     11.   c(m,n) = cmplx(cr(m+1,n+1),ci(m+1,n+1))
c
c
c    then for i=1,...,nlat and  j=1,...,nlon
c
c        the expansion for real h(i,j) takes the form
c
c     h(i,j) = the sum from n=1 to n=nlat-1 of the real part of
c
c         .5*(b(0,n)*bbar(0,n,theta(i))+c(0,n)*cbar(0,n,theta(i)))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c              b(m,n)*bbar(m,n,theta(i))*exp(i*m*phi(j))
c             +c(m,n)*cbar(m,n,theta(i))*exp(i*m*phi(j))
c
c   *************************************************************
c
c   in terms of real variables this expansion takes the form
c
c             for i=1,...,nlat and  j=1,...,nlon
c
c     v(i,j) = the sum from n=1 to n=nlat-1 of
c
c               .5*br(1,n+1)*vbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c       (br(m+1,n+1)*vbar(m,n,theta(i))-ci(m+1,n+1)*wbar(m,n,theta(i)))
c                                          *cos(m*phi(j))
c      -(bi(m+1,n+1)*vbar(m,n,theta(i))+cr(m+1,n+1)*wbar(m,n,theta(i)))
c                                          *sin(m*phi(j))
c
c    and for i=1,...,nlat and  j=1,...,nlon
c
c     w(i,j) = the sum from n=1 to n=nlat-1 of
c
c              -.5*cr(1,n+1)*vbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c      -(cr(m+1,n+1)*vbar(m,n,theta(i))+bi(m+1,n+1)*wbar(m,n,theta(i)))
c                                          *cos(m*phi(j))
c      +(ci(m+1,n+1)*vbar(m,n,theta(i))-br(m+1,n+1)*wbar(m,n,theta(i)))
c                                          *sin(m*phi(j))
c
c
c      br(m+1,nlat),bi(m+1,nlat),cr(m+1,nlat), and ci(m+1,nlat) are
c      assumed zero for m even.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of ityp
c            = 4  error in the specification of nt
c            = 5  error in the specification of idvw
c            = 6  error in the specification of jdvw
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lvhsgs
c            = 10 error in the specification of lwork
c
c
c     subroutine vhsgsi(nlat,nlon,wvhsgs,lvhsgs,dwork,ldwork,ierror)
c
c     subroutine vhsgsi initializes the array wvhsgs which can then be
c     used repeatedly by subroutine vhsgs until nlat or nlon is changed.
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
c     lvhsgs the dimension of the array wvhsgs as it appears in the
c            program that calls vhsgs. define
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
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat
c
c     dwork a double precision work array that does not need to be saved
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls vhsgsi. ldwork must be at least
c
c                 (3*nlat*(nlat+3)+2)/2

c
c     **************************************************************
c
c     output parameters
c
c     wvhsgs an array which is initialized for use by subroutine vhsgs.
c            once initialized, wvhsgs can be used repeatedly by vhsgs
c            as long as nlat and nlon remain unchanged.  wvhsgs must not
c            be altered between calls of vhsgs.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhsgs
c            = 4  error in the specification of lwork
c
      SUBROUTINE DVHSGS(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,
     +                  MDAB,NDAB,WVHSGS,LVHSGS,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHSGS
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),WORK(1),
     +          WVHSGS(1)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IF (ITYP.LT.0 .OR. ITYP.GT.8) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IMID = (NLAT+1)/2
      IF ((ITYP.LE.2.AND.IDVW.LT.NLAT) .OR.
     +    (ITYP.GT.2.AND.IDVW.LT.IMID)) RETURN
      IERROR = 6
      IF (JDVW.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MDAB.LT.MMAX) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      IF (LVHSGS.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 10
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IF (LWORK.LT.LNL+LNL+IDV*NLON) RETURN
      IERROR = 0
      IST = 0
      IF (ITYP.LE.2) IST = IMID
c
c     set wvhsgs pointers
c
      LMN = NLAT* (NLAT+1)/2
      JW1 = 1
      JW2 = JW1 + IMID*LMN
      JW3 = JW2 + IMID*LMN
c
c     set work pointers
c
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL

      CALL DVHSGS1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,BR,
     +             BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +             WORK(IW4),IDZ,WVHSGS(JW1),WVHSGS(JW2),WVHSGS(JW3))
      RETURN
      END

      SUBROUTINE DVHSGS1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,
     +                  BR,BI,CR,CI,IDV,VE,VO,WE,WO,WORK,IDZ,VB,WB,
     +                  WRFFT)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION VE
      DOUBLE PRECISION VO
      DOUBLE PRECISION WE
      DOUBLE PRECISION WO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION WRFFT
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),
     +          VE(IDV,NLON,1),VO(IDV,NLON,1),WE(IDV,NLON,1),
     +          WO(IDV,NLON,1),WORK(1),WRFFT(1),VB(IMID,1),WB(IMID,1)

      NLP1 = NLAT + 1
      MLAT = MOD(NLAT,2)
      MLON = MOD(NLON,2)
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IMM1 = IMID
      IF (MLAT.NE.0) IMM1 = IMID - 1
      DO 10 K = 1,NT
          DO 10 J = 1,NLON
              DO 10 I = 1,IDV
                  VE(I,J,K) = 0.D0
                  WE(I,J,K) = 0.D0
   10 CONTINUE
      NDO1 = NLAT
      NDO2 = NLAT
      IF (MLAT.NE.0) NDO1 = NLAT - 1
      IF (MLAT.EQ.0) NDO2 = NLAT - 1
   18 ITYPP = ITYP + 1
      GO TO (1,100,200,300,400,500,600,700,800) ITYPP
c
c     case ityp=0   no symmetries
c
c     case m = 0
c
    1 CONTINUE
      DO 15 K = 1,NT
          DO 15 NP1 = 2,NDO2,2
              DO 15 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
   15 CONTINUE
      DO 16 K = 1,NT
          DO 16 NP1 = 3,NDO1,2
              DO 16 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
   16 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 30 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 26
          DO 25 K = 1,NT
              DO 24 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 23 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
   23             CONTINUE
                  IF (MLAT.EQ.0) GO TO 24
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,MN)
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,MN)
   24         CONTINUE
   25     CONTINUE
   26     IF (MP2.GT.NDO2) GO TO 30
          DO 29 K = 1,NT
              DO 28 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 27 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
   27             CONTINUE
                  IF (MLAT.EQ.0) GO TO 28
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,MN)
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,MN)
   28         CONTINUE
   29     CONTINUE
   30 CONTINUE
      GO TO 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
c     case m = 0
c
  100 CONTINUE
      DO 115 K = 1,NT
          DO 115 NP1 = 2,NDO2,2
              DO 115 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  115 CONTINUE
      DO 116 K = 1,NT
          DO 116 NP1 = 3,NDO1,2
              DO 116 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  116 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 130 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 126
          DO 125 K = 1,NT
              DO 124 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 123 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  123             CONTINUE
                  IF (MLAT.EQ.0) GO TO 124
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,MN)
  124         CONTINUE
  125     CONTINUE
  126     IF (MP2.GT.NDO2) GO TO 130
          DO 129 K = 1,NT
              DO 128 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 127 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  127             CONTINUE
                  IF (MLAT.EQ.0) GO TO 128
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,MN)
  128         CONTINUE
  129     CONTINUE
  130 CONTINUE
      GO TO 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
c     case m = 0
c
  200 DO 215 K = 1,NT
          DO 215 NP1 = 2,NDO2,2
              DO 215 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  215 CONTINUE
      DO 216 K = 1,NT
          DO 216 NP1 = 3,NDO1,2
              DO 216 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  216 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 230 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 226
          DO 225 K = 1,NT
              DO 224 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 223 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  223             CONTINUE
                  IF (MLAT.EQ.0) GO TO 224
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,MN)
  224         CONTINUE
  225     CONTINUE
  226     IF (MP2.GT.NDO2) GO TO 230
          DO 229 K = 1,NT
              DO 228 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 227 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  227             CONTINUE
                  IF (MLAT.EQ.0) GO TO 228
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,MN)
  228         CONTINUE
  229     CONTINUE
  230 CONTINUE
      GO TO 950
c
c     case ityp=3   v even,  w odd
c
c     case m = 0
c
  300 DO 315 K = 1,NT
          DO 315 NP1 = 2,NDO2,2
              DO 315 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  315 CONTINUE
      DO 316 K = 1,NT
          DO 316 NP1 = 3,NDO1,2
              DO 316 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  316 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 330 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 326
          DO 325 K = 1,NT
              DO 324 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 323 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  323             CONTINUE
                  IF (MLAT.EQ.0) GO TO 324
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,MN)
  324         CONTINUE
  325     CONTINUE
  326     IF (MP2.GT.NDO2) GO TO 330
          DO 329 K = 1,NT
              DO 328 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 327 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  327             CONTINUE
                  IF (MLAT.EQ.0) GO TO 328
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,MN)
  328         CONTINUE
  329     CONTINUE
  330 CONTINUE
      GO TO 950
c
c     case ityp=4   v even,  w odd, and both cr and ci equal zero
c
c     case m = 0
c
  400 DO 415 K = 1,NT
          DO 415 NP1 = 2,NDO2,2
              DO 415 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  415 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 430 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 430
          DO 429 K = 1,NT
              DO 428 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 427 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  427             CONTINUE
                  IF (MLAT.EQ.0) GO TO 428
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,MN)
  428         CONTINUE
  429     CONTINUE
  430 CONTINUE
      GO TO 950
c
c     case ityp=5   v even,  w odd,     br and bi equal zero
c
c     case m = 0
c
  500 DO 516 K = 1,NT
          DO 516 NP1 = 3,NDO1,2
              DO 516 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  516 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 530 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 530
          DO 525 K = 1,NT
              DO 524 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 523 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  523             CONTINUE
                  IF (MLAT.EQ.0) GO TO 524
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,MN)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,MN)
  524         CONTINUE
  525     CONTINUE
  530 CONTINUE
      GO TO 950
c
c     case ityp=6   v odd  ,  w even
c
c     case m = 0
c
  600 DO 615 K = 1,NT
          DO 615 NP1 = 2,NDO2,2
              DO 615 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  615 CONTINUE
      DO 616 K = 1,NT
          DO 616 NP1 = 3,NDO1,2
              DO 616 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  616 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 630 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 626
          DO 625 K = 1,NT
              DO 624 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 623 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  623             CONTINUE
                  IF (MLAT.EQ.0) GO TO 624
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,MN)
  624         CONTINUE
  625     CONTINUE
  626     IF (MP2.GT.NDO2) GO TO 630
          DO 629 K = 1,NT
              DO 628 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 627 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  627             CONTINUE
                  IF (MLAT.EQ.0) GO TO 628
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,MN)
  628         CONTINUE
  629     CONTINUE
  630 CONTINUE
      GO TO 950
c
c     case ityp=7   v odd, w even   cr and ci equal zero
c
c     case m = 0
c
  700 DO 716 K = 1,NT
          DO 716 NP1 = 3,NDO1,2
              DO 716 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1)
  716 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 730 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 730
          DO 725 K = 1,NT
              DO 724 NP1 = MP1,NDO1,2
                  MN = MB + NP1
                  DO 723 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,MN)
  723             CONTINUE
                  IF (MLAT.EQ.0) GO TO 724
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,MN)
  724         CONTINUE
  725     CONTINUE
  730 CONTINUE
      GO TO 950
c
c     case ityp=8   v odd,  w even   br and bi equal zero
c
c     case m = 0
c
  800 DO 815 K = 1,NT
          DO 815 NP1 = 2,NDO2,2
              DO 815 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1)
  815 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 830 MP1 = 2,MMAX
          M = MP1 - 1
c     mb = m*(nlat-1)-(m*(m-1))/2
          MB = M*NLAT - (M* (M+1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 830
          DO 829 K = 1,NT
              DO 828 NP1 = MP2,NDO2,2
                  MN = MB + NP1
                  DO 827 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,MN)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,MN)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,MN)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,MN)
  827             CONTINUE
                  IF (MLAT.EQ.0) GO TO 828
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,MN)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,MN)
  828         CONTINUE
  829     CONTINUE
  830 CONTINUE
  950 CONTINUE
      DO 14 K = 1,NT
          CALL DHRFFTB(IDV,NLON,VE(1,1,K),IDV,WRFFT,WORK)
          CALL DHRFFTB(IDV,NLON,WE(1,1,K),IDV,WRFFT,WORK)
   14 CONTINUE
      IF (ITYP.GT.2) GO TO 12
      DO 60 K = 1,NT
          DO 60 J = 1,NLON
              DO 60 I = 1,IMM1
                  V(I,J,K) = .5D0* (VE(I,J,K)+VO(I,J,K))
                  W(I,J,K) = .5D0* (WE(I,J,K)+WO(I,J,K))
                  V(NLP1-I,J,K) = .5D0* (VE(I,J,K)-VO(I,J,K))
                  W(NLP1-I,J,K) = .5D0* (WE(I,J,K)-WO(I,J,K))
   60 CONTINUE
      GO TO 13
   12 DO 11 K = 1,NT
          DO 11 J = 1,NLON
              DO 11 I = 1,IMM1
                  V(I,J,K) = .5D0*VE(I,J,K)
                  W(I,J,K) = .5D0*WE(I,J,K)
   11 CONTINUE
   13 IF (MLAT.EQ.0) RETURN
      DO 65 K = 1,NT
          DO 65 J = 1,NLON
              V(IMID,J,K) = .5D0*VE(IMID,J,K)
              W(IMID,J,K) = .5D0*WE(IMID,J,K)
   65 CONTINUE
      RETURN
      END
      SUBROUTINE DVHSGSI(NLAT,NLON,WVHSGS,LVHSGS,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WVHSGS
c
c     subroutine vhsfsi computes the gaussian points theta, gauss
c     weights wts, and the components vb and wb of the vector
c     harmonics. all quantities are computed internally in double
c     precision but returned in single precision and are therfore
c     accurate to single precision.
c
c     set imid = (nlat+1)/2 and lmn=(nlat*(nlat+1))/2 then
c     wvhsgs must have 2*(imid*lmn+nlat)+nlon+15 locations
c
c     double precision array dwork must have
c       3*nlat*(nlat+1)+5*nlat+1 = nlat*(3*nlat+8)+1
c     locations which is determined by the size of dthet,
c     dwts, dwork, and dpbar in vhsgs1
c
      DIMENSION WVHSGS(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IMID = (NLAT+1)/2
      LMN = (NLAT* (NLAT+1))/2
      IF (LVHSGS.LT.2* (IMID*LMN)+NLON+15) RETURN
      IERROR = 4
      IF (LDWORK.LT. (NLAT*3* (NLAT+3)+2)/2) RETURN
      IERROR = 0
c
c     set saved work space pointers
c
      JW1 = 1
      JW2 = JW1 + IMID*LMN
      JW3 = JW2 + IMID*LMN
c
c     set unsaved work space pointers
c
      IW1 = 1
      IW2 = IW1 + NLAT
      IW3 = IW2 + NLAT
      IW4 = IW3 + 3*IMID*NLAT
c     iw2 = iw1+nlat+nlat
c     iw3 = iw2+nlat+nlat
c     iw4 = iw3+6*imid*nlat
      CALL DVHGSI1(NLAT,IMID,WVHSGS(JW1),WVHSGS(JW2),DWORK(IW1),
     +             DWORK(IW2),DWORK(IW3),DWORK(IW4))
      CALL DHRFFTI(NLON,WVHSGS(JW3))
      RETURN
      END
      SUBROUTINE DVHGSI1(NLAT,IMID,VB,WB,DTHET,DWTS,DPBAR,WORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DIMENSION VB(IMID,*),WB(IMID,*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ABEL,BBEL,CBEL,SSQR2,DCF
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DTHET(*),DWTS(*),DPBAR(IMID,NLAT,3),WORK(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DN,DM
c
c     compute gauss points and weights
c     use dpbar (length 3*nnlat*(nnlat+1)) as work space for gaqd
c
      LWK = NLAT* (NLAT+2)
      CALL DGAQD(NLAT,DTHET,DWTS,DPBAR,LWK,IERROR)
C*PT*WARNING* Constant already double-precision
c
c     compute associated legendre functions
c
c     compute m=n=0 legendre polynomials for all theta(i)
c
      SSQR2 = 1.D0/DSQRT(2.d0)
      DO 90 I = 1,IMID
          DPBAR(I,1,1) = SSQR2
          VB(I,1) = 0.D0
          WB(I,1) = 0.D0
   90 CONTINUE
c
c     main loop for remaining vb, and wb
c
      DO 100 N = 1,NLAT - 1
C*PT*WARNING* Already double-precision (DBLE)
          DN = DBLE(N)
          NM = MOD(N-2,3) + 1
          NZ = MOD(N-1,3) + 1
          NP = MOD(N,3) + 1
c
c     compute dpbar for m=0
c
          CALL DDNLFK(0,N,WORK)
          MN = INDX(0,N,NLAT)
          DO 105 I = 1,IMID
              CALL DDNLFT(0,N,DTHET(I),WORK,DPBAR(I,1,NP))
c      pbar(i,mn) = dpbar(i,1,np)
  105     CONTINUE
c
c     compute dpbar for m=1
c
          CALL DDNLFK(1,N,WORK)
          MN = INDX(1,N,NLAT)
          DO 106 I = 1,IMID
              CALL DDNLFT(1,N,DTHET(I),WORK,DPBAR(I,2,NP))
c      pbar(i,mn) = dpbar(i,2,np)
  106     CONTINUE
  104     CONTINUE
c
c     compute and store dpbar for m=2,n
c
          IF (N.LT.2) GO TO 108
          DO 107 M = 2,N
C*PT*WARNING* Already double-precision (DBLE)
              DM = DBLE(M)
              ABEL = DSQRT(((2*DN+1)* (DM+DN-2)* (DM+DN-3))/
     +               ((2*DN-3)* (DM+DN-1)* (DM+DN)))
              BBEL = DSQRT(((2*DN+1)* (DN-DM-1)* (DN-DM))/
     +               ((2*DN-3)* (DM+DN-1)* (DM+DN)))
              CBEL = DSQRT(((DN-DM+1)* (DN-DM+2))/ ((DM+DN-1)* (DM+DN)))
              ID = INDX(M,N,NLAT)
              IF (M.GE.N-1) GO TO 102
              DO 103 I = 1,IMID
                  DPBAR(I,M+1,NP) = ABEL*DPBAR(I,M-1,NM) +
     +                              BBEL*DPBAR(I,M+1,NM) -
     +                              CBEL*DPBAR(I,M-1,NP)
c      pbar(i,id) = dpbar(i,m+1,np)
  103         CONTINUE
              GO TO 107
  102         DO 101 I = 1,IMID
                  DPBAR(I,M+1,NP) = ABEL*DPBAR(I,M-1,NM) -
     +                              CBEL*DPBAR(I,M-1,NP)
c      pbar(i,id) = dpbar(i,m+1,np)
  101         CONTINUE
  107     CONTINUE
c
c     compute the derivative of the functions
c
  108     IX = INDX(0,N,NLAT)
          IY = INDX(N,N,NLAT)
          DO 125 I = 1,IMID
              VB(I,IX) = -DPBAR(I,2,NP)
              VB(I,IY) = DPBAR(I,N,NP)/DSQRT(2* (DN+1))
  125     CONTINUE
c
          IF (N.EQ.1) GO TO 131
          DCF = DSQRT(4*DN* (DN+1))
          DO 130 M = 1,N - 1
              DM = DBLE(M)
              IX = INDX(M,N,NLAT)
              ABEL = DSQRT((DN+DM)* (DN-DM+1))/DCF
              BBEL = DSQRT((DN-DM)* (DN+DM+1))/DCF
              DO 130 I = 1,IMID
                  VB(I,IX) = ABEL*DPBAR(I,M,NP) - BBEL*DPBAR(I,M+2,NP)
  130     CONTINUE
c
c     compute the vector harmonic w(theta) = m*pbar/cos(theta)
c
c     set wb=0 for m=0
c
  131     IX = INDX(0,N,NLAT)
          DO 220 I = 1,IMID
C*PT*WARNING* Constant already double-precision
              WB(I,IX) = 0.d0
  220     CONTINUE
c
c     compute wb for m=1,n
c
          DCF = DSQRT((DN+DN+1)/ (4*DN* (DN+1)* (DN+DN-1)))
          DO 230 M = 1,N
              DM = DBLE(M)
              IX = INDX(M,N,NLAT)
              ABEL = DCF*DSQRT((DN+DM)* (DN+DM-1))
              BBEL = DCF*DSQRT((DN-DM)* (DN-DM-1))
              IF (M.GE.N-1) GO TO 231
              DO 229 I = 1,IMID
                  WB(I,IX) = ABEL*DPBAR(I,M,NZ) + BBEL*DPBAR(I,M+2,NZ)
  229         CONTINUE
              GO TO 230
  231         DO 228 I = 1,IMID
                  WB(I,IX) = ABEL*DPBAR(I,M,NZ)
  228         CONTINUE
  230     CONTINUE
  100 CONTINUE
      RETURN
      END
