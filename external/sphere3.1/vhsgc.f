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
c            full sphere. these lie in the interval (0,pi) and are computed
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
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
c            full sphere. these lie in the interval (0,pi) and are computed
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
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
      subroutine vhsgc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +                 mdab,ndab,wvhsgc,lvhsgc,work,lwork,ierror)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(1),wvhsgc(1)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      if(ityp.lt.0 .or. ityp.gt.8) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((ityp.le.2 .and. idvw.lt.nlat) .or.
     1   (ityp.gt.2 .and. idvw.lt.imid)) return
      ierror = 6
      if(jdvw .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+1)/2)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      lzz1 = 2*nlat*imid
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c
c     check save work space length
c
      l1 = min0(nlat,(nlon+1)/2)
      l2 = (nlat+1)/2
      lwmin =   4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
      if (lvhsgc .lt. lwmin) return


c     if(lvhsgc .lt. 2*(lzz1+labc)+nlon+15) return
      ierror = 10
      if(ityp .le. 2 .and. 
     1         lwork .lt. nlat*(2*nt*nlon+max0(6*imid,nlon))) return
      if(ityp .gt. 2 .and. 
     1         lwork .lt. imid*(2*nt*nlon+max0(6*nlat,nlon))) return
      ierror = 0
      idv = nlat
      if(ityp .gt. 2) idv = imid
      lnl = nt*idv*nlon
      ist = 0
      if(ityp .le. 2) ist = imid
      iw1 = ist+1
      iw2 = lnl+1
      iw3 = iw2+ist
      iw4 = iw2+lnl
      iw5 = iw4+3*imid*nlat
      lzz1 = 2*nlat*imid
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      lwzvin = lzz1+labc
      jw1 = lwzvin+1
      jw2 = jw1+lwzvin
      call vhsgc1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,ndab,
     1     br,bi,cr,ci,idv,work,work(iw1),work(iw2),work(iw3),
     2     work(iw4),work(iw5),wvhsgc,wvhsgc(jw1),wvhsgc(jw2))
      return
      end
      subroutine vhsgc1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,
     1   ndab,br,bi,cr,ci,idv,ve,vo,we,wo,vb,wb,wvbin,wwbin,wrfft)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          ve(idv,nlon,1),vo(idv,nlon,1),we(idv,nlon,1),
     3          wo(idv,nlon,1),wvbin(1),wwbin(1),wrfft(1),
     4          vb(imid,nlat,3),wb(imid,nlat,3)
      nlp1 = nlat+1
      mlat = mod(nlat,2)
      mlon = mod(nlon,2)
      mmax = min0(nlat,(nlon+1)/2)
      imm1 = imid
      if(mlat .ne. 0) imm1 = imid-1
      do 10 k=1,nt
      do 10 j=1,nlon
      do 10 i=1,idv
      ve(i,j,k) = 0.
      we(i,j,k) = 0.
   10 continue
      ndo1 = nlat
      ndo2 = nlat
      if(mlat .ne. 0) ndo1 = nlat-1
      if(mlat .eq. 0) ndo2 = nlat-1
   18 itypp = ityp+1
      go to (1,100,200,300,400,500,600,700,800),itypp
c
c     case ityp=0   no symmetries
c
    1 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 15 k=1,nt
      do 15 np1=2,ndo2,2
      do 15 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
   15 continue
      do 16 k=1,nt
      do 16 np1=3,ndo1,2
      do 16 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
   16 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 30 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 26
      do 25 k=1,nt
      do 24 np1=mp1,ndo1,2
      do 23 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
   23 continue
      if(mlat .eq. 0) go to 24
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
   24 continue
   25 continue
   26 if(mp2 .gt. ndo2) go to 30
      do 29 k=1,nt
      do 28 np1=mp2,ndo2,2
      do 27 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
   27 continue
      if(mlat .eq. 0) go to 28
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
   28 continue
   29 continue
   30 continue
      go to 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
  100 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 115 k=1,nt
      do 115 np1=2,ndo2,2
      do 115 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  115 continue
      do 116 k=1,nt
      do 116 np1=3,ndo1,2
      do 116 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  116 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 130 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 126
      do 125 k=1,nt
      do 124 np1=mp1,ndo1,2
      do 123 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  123 continue
      if(mlat .eq. 0) go to 124
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
  124 continue
  125 continue
  126 if(mp2 .gt. ndo2) go to 130
      do 129 k=1,nt
      do 128 np1=mp2,ndo2,2
      do 127 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  127 continue
      if(mlat .eq. 0) go to 128
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  128 continue
  129 continue
  130 continue
      go to 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
  200 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 215 k=1,nt
      do 215 np1=2,ndo2,2
      do 215 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  215 continue
      do 216 k=1,nt
      do 216 np1=3,ndo1,2
      do 216 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  216 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 230 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 226
      do 225 k=1,nt
      do 224 np1=mp1,ndo1,2
      do 223 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  223 continue
      if(mlat .eq. 0) go to 224
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  224 continue
  225 continue
  226 if(mp2 .gt. ndo2) go to 230
      do 229 k=1,nt
      do 228 np1=mp2,ndo2,2
      do 227 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  227 continue
      if(mlat .eq. 0) go to 228
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  228 continue
  229 continue
  230 continue
      go to 950
c
c     case ityp=3   v even,  w odd 
c
  300 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 315 k=1,nt
      do 315 np1=2,ndo2,2
      do 315 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  315 continue
      do 316 k=1,nt
      do 316 np1=3,ndo1,2
      do 316 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  316 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 330 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 326
      do 325 k=1,nt
      do 324 np1=mp1,ndo1,2
      do 323 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  323 continue
      if(mlat .eq. 0) go to 324
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  324 continue
  325 continue
  326 if(mp2 .gt. ndo2) go to 330
      do 329 k=1,nt
      do 328 np1=mp2,ndo2,2
      do 327 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  327 continue
      if(mlat .eq. 0) go to 328
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  328 continue
  329 continue
  330 continue
      go to 950
c
c     case ityp=4   v even,  w odd, and both cr and ci equal zero 
c
  400 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 415 k=1,nt
      do 415 np1=2,ndo2,2
      do 415 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  415 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 430 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(1,nlat,nlon,m,vb,iv,wvbin)
      call wbin(1,nlat,nlon,m,wb,iw,wwbin)
      if(mp2 .gt. ndo2) go to 430
      do 429 k=1,nt
      do 428 np1=mp2,ndo2,2
      do 427 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  427 continue
      if(mlat .eq. 0) go to 428
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  428 continue
  429 continue
  430 continue
      go to 950
c
c     case ityp=5   v even,  w odd,     br and bi equal zero 
c
  500 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 516 k=1,nt
      do 516 np1=3,ndo1,2
      do 516 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  516 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 530 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(2,nlat,nlon,m,vb,iv,wvbin)
      call wbin(2,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 530
      do 525 k=1,nt
      do 524 np1=mp1,ndo1,2
      do 523 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  523 continue
      if(mlat .eq. 0) go to 524
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  524 continue
  525 continue
  530 continue
      go to 950
c
c     case ityp=6   v odd  ,  w even
c
  600 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 615 k=1,nt
      do 615 np1=2,ndo2,2
      do 615 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  615 continue
      do 616 k=1,nt
      do 616 np1=3,ndo1,2
      do 616 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  616 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 630 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 626
      do 625 k=1,nt
      do 624 np1=mp1,ndo1,2
      do 623 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  623 continue
      if(mlat .eq. 0) go to 624
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
  624 continue
  625 continue
  626 if(mp2 .gt. ndo2) go to 630
      do 629 k=1,nt
      do 628 np1=mp2,ndo2,2
      do 627 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  627 continue
      if(mlat .eq. 0) go to 628
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  628 continue
  629 continue
  630 continue
      go to 950
c
c     case ityp=7   v odd, w even   cr and ci equal zero
c
  700 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 716 k=1,nt
      do 716 np1=3,ndo1,2
      do 716 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  716 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 730 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(2,nlat,nlon,m,vb,iv,wvbin)
      call wbin(2,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 730
      do 725 k=1,nt
      do 724 np1=mp1,ndo1,2
      do 723 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  723 continue
      if(mlat .eq. 0) go to 724
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
  724 continue
  725 continue
  730 continue
      go to 950
c
c     case ityp=8   v odd,  w even   br and bi equal zero
c
  800 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 815 k=1,nt
      do 815 np1=2,ndo2,2
      do 815 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  815 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 830 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(1,nlat,nlon,m,vb,iv,wvbin)
      call wbin(1,nlat,nlon,m,wb,iw,wwbin)
      if(mp2 .gt. ndo2) go to 830
      do 829 k=1,nt
      do 828 np1=mp2,ndo2,2
      do 827 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  827 continue
      if(mlat .eq. 0) go to 828
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  828 continue
  829 continue
  830 continue
  950 do 14 k=1,nt
      call hrfftb(idv,nlon,ve(1,1,k),idv,wrfft,vb)
      call hrfftb(idv,nlon,we(1,1,k),idv,wrfft,vb)
   14 continue
      if(ityp .gt. 2) go to 12
      do 60 k=1,nt
      do 60 j=1,nlon
      do 60 i=1,imm1
      v(i,j,k) = .5*(ve(i,j,k)+vo(i,j,k))
      w(i,j,k) = .5*(we(i,j,k)+wo(i,j,k))
      v(nlp1-i,j,k) = .5*(ve(i,j,k)-vo(i,j,k))
      w(nlp1-i,j,k) = .5*(we(i,j,k)-wo(i,j,k))
   60 continue
      go to 13
   12 do 11 k=1,nt
      do 11 j=1,nlon
      do 11 i=1,imm1
      v(i,j,k) = .5*ve(i,j,k)
      w(i,j,k) = .5*we(i,j,k)
   11 continue
   13 if(mlat .eq. 0) return
      do 65 k=1,nt
      do 65 j=1,nlon
      v(imid,j,k) = .5*ve(imid,j,k)
      w(imid,j,k) = .5*we(imid,j,k)
   65 continue
      return
      end
      subroutine vhsgci(nlat,nlon,wvhsgc,lvhsgc,dwork,ldwork,ierror)
      dimension wvhsgc(lvhsgc)
      double precision dwork(ldwork)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      if(lvhsgc .lt. 2*(lzz1+labc)+nlon+15) return
      ierror = 4
      if (ldwork .lt. 2*nlat*(nlat+1)+1) return
      ierror = 0
c
c     compute gaussian points in first nlat+1 words of dwork
c     double precision
c
c     lwk = 2*nlat*(nlat+2)
      jw1 = 1
      jw2 = jw1+nlat
      jw3 = jw2+nlat
c     jw2 = jw1+nlat+nlat
c     jw3 = jw2+nlat+nlat
      call gaqd(nlat,dwork(jw1),dwork(jw2),dwork(jw3),ldwork,ierror)
c     iwrk = nlat+2
      iwrk = (nlat+1)/2 + 1
      call vbgint (nlat,nlon,dwork,wvhsgc,dwork(iwrk))
      lwvbin = lzz1+labc
      iw1 = lwvbin+1
      call wbgint (nlat,nlon,dwork,wvhsgc(iw1),dwork(iwrk))
      iw2 = iw1+lwvbin
      call hrffti(nlon,wvhsgc(iw2))
      return
      end
