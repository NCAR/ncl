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
c ... file vhses.f
c
c     this file contains code and documentation for subroutines
c     vhses and vhsesi
c
c ... files which must be loaded with vhses.f
c
c     sphcom.f, hrfft.f
c
c
c     subroutine vhses(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhses,lvhses,work,lwork,ierror)
c
c     subroutine vhses performs the vector spherical harmonic synthesis
c     of the arrays br, bi, cr, and ci and stores the result in the
c     arrays v and w. v(i,j) and w(i,j) are the colatitudinal
c     (measured from the north pole) and east longitudinal components
c     respectively, located at colatitude theta(i) = (i-1)*pi/(nlat-1)
c     and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (v,w) is given below at output parameters v,w.
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
c     nt     the number of syntheses.  in the program that calls vhses,
c            the arrays v,w,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhaes. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhses. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given
c            below at the discription of output parameters v and w.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhses. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhses. ndab must be at
c            least nlat.
c
c     wvhses an array which must be initialized by subroutine vhsesi.
c            once initialized, wvhses can be used repeatedly by vhses
c            as long as nlon and nlat remain unchanged.  wvhses must
c            not be altered between calls of vhses.
c
c     lvhses the dimension of the array wvhses as it appears in the
c            program that calls vhses. define
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
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhses. define
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
c            v(i,j),w(i,j) contain the components at colatitude
c            theta(i) = (i-1)*pi/(nlat-1) and longitude phi(j) =
c            (j-1)*2*pi/nlon. the index ranges are defined above at
c            the input parameter ityp. v and w are computed from the
c            formulas given below
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
c     7.   theta(i) = (i-1)*pi/(nlat-1) and phi(j) = (j-1)*2*pi/nlon
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
c            = 9  error in the specification of lvhses
c            = 10 error in the specification of lwork
c
c ************************************************************
c
c     subroutine vhsesi(nlat,nlon,wvhses,lvhses,work,lwork,dwork,
c    +                  ldwork,ierror)
c
c     subroutine vhsesi initializes the array wvhses which can then be
c     used repeatedly by subroutine vhses until nlat or nlon is changed.
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
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     lvhses the dimension of the array wvhses as it appears in the
c            program that calls vhses. define
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
c                  l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhses. lwork must be at least
c
c              3*(max0(l1-2,0)*(nlat+nlat-l1-1))/2+5*l2*nlat
c
c     dwork  an unsaved double precision work space
c
c     ldwork the length of the array dwork as it appears in the
c            program that calls vhsesi.  ldwork must be at least
c            2*(nlat+1)
c
c
c     **************************************************************
c
c     output parameters
c
c     wvhses an array which is initialized for use by subroutine vhses.
c            once initialized, wvhses can be used repeatedly by vhses
c            as long as nlat or nlon remain unchanged.  wvhses must not
c            be altered between calls of vhses.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhses
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c
c *****************************************
      SUBROUTINE DVHSES(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,
     +                 MDAB,NDAB,WVHSES,LVHSES,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHSES
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),WORK(1),
     +          WVHSES(1)

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
      IF (LVHSES.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 10
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IF (LWORK.LT.LNL+LNL+IDV*NLON) RETURN
      IERROR = 0
      IST = 0
      IF (ITYP.LE.2) IST = IMID
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL
      JW1 = LZIMN + 1
      JW2 = JW1 + LZIMN
      CALL DVHSES1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,BR,
     +             BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +             WORK(IW4),IDZ,WVHSES,WVHSES(JW1),WVHSES(JW2))
      RETURN
      END

      SUBROUTINE DVHSES1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,
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
    1 DO 15 K = 1,NT
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
          MB = M* (NLAT-1) - (M* (M-1))/2
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
  950 DO 14 K = 1,NT
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

      SUBROUTINE DVHSESI(NLAT,NLON,WVHSES,LVHSES,WORK,LWORK,DWORK,
     +                   LDWORK,IERROR)
      DOUBLE PRECISION WVHSES
      DOUBLE PRECISION WORK
      DIMENSION WVHSES(LVHSES),WORK(LWORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IMID = (NLAT+1)/2
      LZIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
      IF (LVHSES.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 4
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LWORK.LT.5*NLAT*IMID+LABC) RETURN
      IERROR = 5
      IF (LDWORK.LT.2* (NLAT+1)) RETURN
      IERROR = 0
      IW1 = 3*NLAT*IMID + 1
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      CALL DVES1(NLAT,NLON,IMID,WVHSES,WVHSES(LZIMN+1),IDZ,WORK,
     +          WORK(IW1),DWORK)
      CALL DHRFFTI(NLON,WVHSES(2*LZIMN+1))
      RETURN
      END
      SUBROUTINE DVES1(NLAT,NLON,IMID,VB,WB,IDZ,VIN,WZVIN,DWORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION WB
      DOUBLE PRECISION VIN
      DOUBLE PRECISION WZVIN
      DIMENSION VB(IMID,*),WB(IMID,*),VIN(IMID,NLAT,3),WZVIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      CALL DVBINIT(NLAT,NLON,WZVIN,DWORK)
      DO 33 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DVBIN(0,NLAT,NLON,M,VIN,I3,WZVIN)
          DO 33 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 33 I = 1,IMID
                  VB(I,MN) = VIN(I,NP1,I3)
   33 CONTINUE
      CALL DWBINIT(NLAT,NLON,WZVIN,DWORK)
      DO 34 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DWBIN(0,NLAT,NLON,M,VIN,I3,WZVIN)
          DO 34 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 34 I = 1,IMID
                  WB(I,MN) = VIN(I,NP1,I3)
   34 CONTINUE
      RETURN
      END
