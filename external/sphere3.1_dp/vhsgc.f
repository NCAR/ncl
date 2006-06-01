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
c ... file vhsgc.f
c
c     this file contains code and documentation for subroutines
c     vhsgc and vhsgci
c
c ... files which must be loaded with vhsgc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine vhsgc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhsgc,lvhsgc,work,lwork,ierror)
c
c     subroutine vhsgc performs the vector spherical harmonic synthesis
c     of the arrays br, bi, cr, and ci and stores the result in the
c     arrays v and w. v(i,j) and w(i,j) are the colatitudinal
c     (measured from the north pole) and east longitudinal components
c     respectively, located at the gaussian colatitude point theta(i)
c     and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (v,w) is given below at output parameters v,w.
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
c     nt     the number of syntheses.  in the program that calls vhsgc,
c            the arrays v,w,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhsgc. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhsgc. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given
c            below at the discription of output parameters v and w.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhsgc. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhsgc. ndab must be at
c            least nlat.
c
c     wvhsgc an array which must be initialized by subroutine vhsgci.
c            once initialized, wvhsgc can be used repeatedly by vhsgc
c            as long as nlon and nlat remain unchanged.  wvhsgc must
c            not be altered between calls of vhsgc.
c
c     lvhsgc the dimension of the array wvhsgc as it appears in the
c            program that calls vhsgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgc must be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhsgc. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c                    nlat*(2*nt*nlon+max0(6*l2,nlon))
c
c            if ityp .gt. 2 then lwork must be at least
c
c                    l2*(2*nt*nlon+max0(6*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     v,w    two or three dimensional arrays (see input parameter nt)
c            in which the synthesis is stored. v is the colatitudinal
c            component and w is the east longitudinal component.
c            v(i,j),w(i,j) contain the components at the gaussian
c            colatitude theta(i) and longitude phi(j) = (j-1)*2*pi/nlon.
c            the index ranges are defined above at the input parameter
c            ityp. v and w are computed from the formulas given below.
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
c     7.   phi(j) = (j-1)*2*pi/nlon, theta(i) is the i(th) guassian
c          point (see nlat as an input parameter).
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
c            = 9  error in the specification of lvhsgc
c            = 10 error in the specification of lwork
c
c*************************************************************
c
c     subroutine vhsgci(nlat,nlon,wvhsgc,lvhsgc,dwork,ldwork,ierror)
c
c     subroutine vhsgci initializes the array wvhsgc which can then be
c     used repeatedly by subroutine vhsgc until nlat or nlon is changed.
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
c     lvhsgc the dimension of the array wvhsgc as it appears in the
c            program that calls vhsgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgc must be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
c
c     work  a double precision work space that does not need to be saved
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls vhsgsi. ldwork must be at least
c
c               2*nlat*(nlat+1)+1
c
c     **************************************************************
c
c     output parameters
c
c     wvhsgc an array which is initialized for use by subroutine vhsgc.
c            once initialized, wvhsgc can be used repeatedly by vhsgc
c            as long as nlat and nlon remain unchanged.  wvhsgc must not
c            be altered between calls of vhsgc.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhsgc
c            = 4  error in the specification of ldwork
c
      SUBROUTINE DVHSGC(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,
     +                  MDAB,NDAB,WVHSGC,LVHSGC,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHSGC
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),WORK(1),
     +          WVHSGC(1)

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
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
c
c     check save work space length
c
      L1 = MIN0(NLAT, (NLON+1)/2)
      L2 = (NLAT+1)/2
      LWMIN = 4*NLAT*L2 + 3*MAX0(L1-2,0)* (2*NLAT-L1-1) + NLON + 15
      IF (LVHSGC.LT.LWMIN) RETURN


c     if(lvhsgc .lt. 2*(lzz1+labc)+nlon+15) return
      IERROR = 10
      IF (ITYP.LE.2 .AND. LWORK.LT.NLAT*
     +    (2*NT*NLON+MAX0(6*IMID,NLON))) RETURN
      IF (ITYP.GT.2 .AND. LWORK.LT.IMID*
     +    (2*NT*NLON+MAX0(6*NLAT,NLON))) RETURN
      IERROR = 0
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IST = 0
      IF (ITYP.LE.2) IST = IMID
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL
      IW5 = IW4 + 3*IMID*NLAT
      LZZ1 = 2*NLAT*IMID
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      LWZVIN = LZZ1 + LABC
      JW1 = LWZVIN + 1
      JW2 = JW1 + LWZVIN
      CALL DVHSGC1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,BR,
     +             BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +             WORK(IW4),WORK(IW5),WVHSGC,WVHSGC(JW1),WVHSGC(JW2))
      RETURN
      END
      SUBROUTINE DVHSGC1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,
     +                  BR,BI,CR,CI,IDV,VE,VO,WE,WO,VB,WB,WVBIN,WWBIN,
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
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION WVBIN
      DOUBLE PRECISION WWBIN
      DOUBLE PRECISION WRFFT
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),
     +          VE(IDV,NLON,1),VO(IDV,NLON,1),WE(IDV,NLON,1),
     +          WO(IDV,NLON,1),WVBIN(1),WWBIN(1),WRFFT(1),
     +          VB(IMID,NLAT,3),WB(IMID,NLAT,3)

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
    1 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 15 K = 1,NT
          DO 15 NP1 = 2,NDO2,2
              DO 15 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
   15 CONTINUE
      DO 16 K = 1,NT
          DO 16 NP1 = 3,NDO1,2
              DO 16 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
   16 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 30 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 26
          DO 25 K = 1,NT
              DO 24 NP1 = MP1,NDO1,2
                  DO 23 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
   23             CONTINUE
                  IF (MLAT.EQ.0) GO TO 24
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,NP1,IW)
   24         CONTINUE
   25     CONTINUE
   26     IF (MP2.GT.NDO2) GO TO 30
          DO 29 K = 1,NT
              DO 28 NP1 = MP2,NDO2,2
                  DO 27 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
   27             CONTINUE
                  IF (MLAT.EQ.0) GO TO 28
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,NP1,IV)
   28         CONTINUE
   29     CONTINUE
   30 CONTINUE
      GO TO 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
  100 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 115 K = 1,NT
          DO 115 NP1 = 2,NDO2,2
              DO 115 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  115 CONTINUE
      DO 116 K = 1,NT
          DO 116 NP1 = 3,NDO1,2
              DO 116 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  116 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 130 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 126
          DO 125 K = 1,NT
              DO 124 NP1 = MP1,NDO1,2
                  DO 123 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  123             CONTINUE
                  IF (MLAT.EQ.0) GO TO 124
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  124         CONTINUE
  125     CONTINUE
  126     IF (MP2.GT.NDO2) GO TO 130
          DO 129 K = 1,NT
              DO 128 NP1 = MP2,NDO2,2
                  DO 127 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  127             CONTINUE
                  IF (MLAT.EQ.0) GO TO 128
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  128         CONTINUE
  129     CONTINUE
  130 CONTINUE
      GO TO 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
  200 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 215 K = 1,NT
          DO 215 NP1 = 2,NDO2,2
              DO 215 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  215 CONTINUE
      DO 216 K = 1,NT
          DO 216 NP1 = 3,NDO1,2
              DO 216 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  216 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 230 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 226
          DO 225 K = 1,NT
              DO 224 NP1 = MP1,NDO1,2
                  DO 223 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  223             CONTINUE
                  IF (MLAT.EQ.0) GO TO 224
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  224         CONTINUE
  225     CONTINUE
  226     IF (MP2.GT.NDO2) GO TO 230
          DO 229 K = 1,NT
              DO 228 NP1 = MP2,NDO2,2
                  DO 227 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  227             CONTINUE
                  IF (MLAT.EQ.0) GO TO 228
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  228         CONTINUE
  229     CONTINUE
  230 CONTINUE
      GO TO 950
c
c     case ityp=3   v even,  w odd
c
  300 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 315 K = 1,NT
          DO 315 NP1 = 2,NDO2,2
              DO 315 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  315 CONTINUE
      DO 316 K = 1,NT
          DO 316 NP1 = 3,NDO1,2
              DO 316 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  316 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 330 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 326
          DO 325 K = 1,NT
              DO 324 NP1 = MP1,NDO1,2
                  DO 323 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  323             CONTINUE
                  IF (MLAT.EQ.0) GO TO 324
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  324         CONTINUE
  325     CONTINUE
  326     IF (MP2.GT.NDO2) GO TO 330
          DO 329 K = 1,NT
              DO 328 NP1 = MP2,NDO2,2
                  DO 327 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  327             CONTINUE
                  IF (MLAT.EQ.0) GO TO 328
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  328         CONTINUE
  329     CONTINUE
  330 CONTINUE
      GO TO 950
c
c     case ityp=4   v even,  w odd, and both cr and ci equal zero
c
  400 CALL DVBIN(1,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 415 K = 1,NT
          DO 415 NP1 = 2,NDO2,2
              DO 415 I = 1,IMID
                  VE(I,1,K) = VE(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  415 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 430 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(1,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(1,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP2.GT.NDO2) GO TO 430
          DO 429 K = 1,NT
              DO 428 NP1 = MP2,NDO2,2
                  DO 427 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  427             CONTINUE
                  IF (MLAT.EQ.0) GO TO 428
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) +
     +                                 BR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 BI(MP1,NP1,K)*VB(IMID,NP1,IV)
  428         CONTINUE
  429     CONTINUE
  430 CONTINUE
      GO TO 950
c
c     case ityp=5   v even,  w odd,     br and bi equal zero
c
  500 CALL DVBIN(2,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 516 K = 1,NT
          DO 516 NP1 = 3,NDO1,2
              DO 516 I = 1,IMM1
                  WO(I,1,K) = WO(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  516 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 530 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(2,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(2,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 530
          DO 525 K = 1,NT
              DO 524 NP1 = MP1,NDO1,2
                  DO 523 I = 1,IMM1
                      VE(I,2*MP1-2,K) = VE(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VE(I,2*MP1-1,K) = VE(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WO(I,2*MP1-2,K) = WO(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WO(I,2*MP1-1,K) = WO(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  523             CONTINUE
                  IF (MLAT.EQ.0) GO TO 524
                  VE(IMID,2*MP1-2,K) = VE(IMID,2*MP1-2,K) -
     +                                 CI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  VE(IMID,2*MP1-1,K) = VE(IMID,2*MP1-1,K) +
     +                                 CR(MP1,NP1,K)*WB(IMID,NP1,IW)
  524         CONTINUE
  525     CONTINUE
  530 CONTINUE
      GO TO 950
c
c     case ityp=6   v odd  ,  w even
c
  600 CALL DVBIN(0,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 615 K = 1,NT
          DO 615 NP1 = 2,NDO2,2
              DO 615 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  615 CONTINUE
      DO 616 K = 1,NT
          DO 616 NP1 = 3,NDO1,2
              DO 616 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  616 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 630 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(0,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(0,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 626
          DO 625 K = 1,NT
              DO 624 NP1 = MP1,NDO1,2
                  DO 623 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  623             CONTINUE
                  IF (MLAT.EQ.0) GO TO 624
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  624         CONTINUE
  625     CONTINUE
  626     IF (MP2.GT.NDO2) GO TO 630
          DO 629 K = 1,NT
              DO 628 NP1 = MP2,NDO2,2
                  DO 627 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  627             CONTINUE
                  IF (MLAT.EQ.0) GO TO 628
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  628         CONTINUE
  629     CONTINUE
  630 CONTINUE
      GO TO 950
c
c     case ityp=7   v odd, w even   cr and ci equal zero
c
  700 CALL DVBIN(2,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 716 K = 1,NT
          DO 716 NP1 = 3,NDO1,2
              DO 716 I = 1,IMM1
                  VO(I,1,K) = VO(I,1,K) + BR(1,NP1,K)*VB(I,NP1,IV)
  716 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 730 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(2,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(2,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP1.GT.NDO1) GO TO 730
          DO 725 K = 1,NT
              DO 724 NP1 = MP1,NDO1,2
                  DO 723 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) +
     +                                  BR(MP1,NP1,K)*VB(I,NP1,IV)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  BI(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  BI(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) +
     +                                  BR(MP1,NP1,K)*WB(I,NP1,IW)
  723             CONTINUE
                  IF (MLAT.EQ.0) GO TO 724
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 BI(MP1,NP1,K)*WB(IMID,NP1,IW)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) +
     +                                 BR(MP1,NP1,K)*WB(IMID,NP1,IW)
  724         CONTINUE
  725     CONTINUE
  730 CONTINUE
      GO TO 950
c
c     case ityp=8   v odd,  w even   br and bi equal zero
c
  800 CALL DVBIN(1,NLAT,NLON,0,VB,IV,WVBIN)
c
c     case m = 0
c
      DO 815 K = 1,NT
          DO 815 NP1 = 2,NDO2,2
              DO 815 I = 1,IMID
                  WE(I,1,K) = WE(I,1,K) - CR(1,NP1,K)*VB(I,NP1,IV)
  815 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) GO TO 950
      DO 830 MP1 = 2,MMAX
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DVBIN(1,NLAT,NLON,M,VB,IV,WVBIN)
          CALL DWBIN(1,NLAT,NLON,M,WB,IW,WWBIN)
          IF (MP2.GT.NDO2) GO TO 830
          DO 829 K = 1,NT
              DO 828 NP1 = MP2,NDO2,2
                  DO 827 I = 1,IMM1
                      VO(I,2*MP1-2,K) = VO(I,2*MP1-2,K) -
     +                                  CI(MP1,NP1,K)*WB(I,NP1,IW)
                      VO(I,2*MP1-1,K) = VO(I,2*MP1-1,K) +
     +                                  CR(MP1,NP1,K)*WB(I,NP1,IW)
                      WE(I,2*MP1-2,K) = WE(I,2*MP1-2,K) -
     +                                  CR(MP1,NP1,K)*VB(I,NP1,IV)
                      WE(I,2*MP1-1,K) = WE(I,2*MP1-1,K) -
     +                                  CI(MP1,NP1,K)*VB(I,NP1,IV)
  827             CONTINUE
                  IF (MLAT.EQ.0) GO TO 828
                  WE(IMID,2*MP1-2,K) = WE(IMID,2*MP1-2,K) -
     +                                 CR(MP1,NP1,K)*VB(IMID,NP1,IV)
                  WE(IMID,2*MP1-1,K) = WE(IMID,2*MP1-1,K) -
     +                                 CI(MP1,NP1,K)*VB(IMID,NP1,IV)
  828         CONTINUE
  829     CONTINUE
  830 CONTINUE
  950 DO 14 K = 1,NT
          CALL DHRFFTB(IDV,NLON,VE(1,1,K),IDV,WRFFT,VB)
          CALL DHRFFTB(IDV,NLON,WE(1,1,K),IDV,WRFFT,VB)
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
      SUBROUTINE DVHSGCI(NLAT,NLON,WVHSGC,LVHSGC,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WVHSGC
      DIMENSION WVHSGC(LVHSGC)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IMID = (NLAT+1)/2
      LZZ1 = 2*NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LVHSGC.LT.2* (LZZ1+LABC)+NLON+15) RETURN
      IERROR = 4
      IF (LDWORK.LT.2*NLAT* (NLAT+1)+1) RETURN
      IERROR = 0
c
c     compute gaussian points in first nlat+1 words of dwork
c     double precision
c
c     lwk = 2*nlat*(nlat+2)
      JW1 = 1
      JW2 = JW1 + NLAT
      JW3 = JW2 + NLAT
c     jw2 = jw1+nlat+nlat
c     jw3 = jw2+nlat+nlat
      CALL DGAQD(NLAT,DWORK(JW1),DWORK(JW2),DWORK(JW3),LDWORK,IERROR)
c     iwrk = nlat+2
      IWRK = (NLAT+1)/2 + 1
      CALL DVBGINT(NLAT,NLON,DWORK,WVHSGC,DWORK(IWRK))
      LWVBIN = LZZ1 + LABC
      IW1 = LWVBIN + 1
      CALL DWBGINT(NLAT,NLON,DWORK,WVHSGC(IW1),DWORK(IWRK))
      IW2 = IW1 + LWVBIN
      CALL DHRFFTI(NLON,WVHSGC(IW2))
      RETURN
      END
