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
c  .                         SPHEREPACK3.0                       .
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
      subroutine vhsgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +                 mdab,ndab,wvhsgs,lvhsgs,work,lwork,ierror)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(1),wvhsgs(1)
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
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      if(lvhsgs .lt. lzimn+lzimn+nlon+15) return
      ierror = 10
      idv = nlat
      if(ityp .gt. 2) idv = imid
      lnl = nt*idv*nlon
      if(lwork .lt. lnl+lnl+idv*nlon) return
      ierror = 0
      ist = 0
      if(ityp .le. 2) ist = imid
c
c     set wvhsgs pointers
c
      lmn = nlat*(nlat+1)/2
      jw1 = 1
      jw2 = jw1+imid*lmn
      jw3 = jw2+imid*lmn
c
c     set work pointers
c
      iw1 = ist+1
      iw2 = lnl+1
      iw3 = iw2+ist
      iw4 = iw2+lnl

      call vhsgs1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,ndab,
     +           br,bi,cr,ci,idv,work,work(iw1),work(iw2),work(iw3),
     +          work(iw4),idz,wvhsgs(jw1),wvhsgs(jw2),wvhsgs(jw3))
      return
      end

      subroutine vhsgs1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,
     1   ndab,br,bi,cr,ci,idv,ve,vo,we,wo,work,idz,vb,wb,wrfft)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          ve(idv,nlon,1),vo(idv,nlon,1),we(idv,nlon,1),
     3          wo(idv,nlon,1),work(1),wrfft(1),
     4          vb(imid,1),wb(imid,1)
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
c     case m = 0
c
    1 continue
      do 15 k=1,nt
      do 15 np1=2,ndo2,2
      do 15 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1)
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1)
   15 continue
      do 16 k=1,nt
      do 16 np1=3,ndo1,2
      do 16 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1)
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1)
   16 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 30 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 26
      do 25 k=1,nt
      do 24 np1=mp1,ndo1,2
      mn = mb+np1
      do 23 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
   23 continue
      if(mlat .eq. 0) go to 24
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,mn)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,mn)
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,mn) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,mn)
   24 continue
   25 continue
   26 if(mp2 .gt. ndo2) go to 30
      do 29 k=1,nt
      do 28 np1=mp2,ndo2,2
      mn = mb+np1
      do 27 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
   27 continue
      if(mlat .eq. 0) go to 28
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,mn) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,mn)
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,mn)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,mn)
   28 continue
   29 continue
   30 continue
      go to 950
c
c     case ityp=1   no symmetries,  cr and ci equal zero
c
c     case m = 0
c
  100 continue
      do 115 k=1,nt
      do 115 np1=2,ndo2,2
      do 115 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1)
  115 continue
      do 116 k=1,nt
      do 116 np1=3,ndo1,2
      do 116 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1)
  116 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 130 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 126
      do 125 k=1,nt
      do 124 np1=mp1,ndo1,2
      mn = mb+np1
      do 123 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  123 continue
      if(mlat .eq. 0) go to 124
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,mn) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,mn)
  124 continue
  125 continue
  126 if(mp2 .gt. ndo2) go to 130
      do 129 k=1,nt
      do 128 np1=mp2,ndo2,2
      mn = mb+np1
      do 127 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  127 continue
      if(mlat .eq. 0) go to 128
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,mn) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,mn)
  128 continue
  129 continue
  130 continue
      go to 950
c
c     case ityp=2   no symmetries,  br and bi are equal to zero
c
c     case m = 0
c
  200 do 215 k=1,nt
      do 215 np1=2,ndo2,2
      do 215 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1)
  215 continue
      do 216 k=1,nt
      do 216 np1=3,ndo1,2
      do 216 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1)
  216 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 230 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 226
      do 225 k=1,nt
      do 224 np1=mp1,ndo1,2
      mn = mb+np1
      do 223 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  223 continue
      if(mlat .eq. 0) go to 224
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,mn)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,mn)
  224 continue
  225 continue
  226 if(mp2 .gt. ndo2) go to 230
      do 229 k=1,nt
      do 228 np1=mp2,ndo2,2
      mn = mb+np1
      do 227 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  227 continue
      if(mlat .eq. 0) go to 228
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,mn)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,mn)
  228 continue
  229 continue
  230 continue
      go to 950
c
c     case ityp=3   v even,  w odd 
c
c     case m = 0
c
  300 do 315 k=1,nt
      do 315 np1=2,ndo2,2
      do 315 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1)
  315 continue
      do 316 k=1,nt
      do 316 np1=3,ndo1,2
      do 316 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1)
  316 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 330 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 326
      do 325 k=1,nt
      do 324 np1=mp1,ndo1,2
      mn = mb+np1
      do 323 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  323 continue
      if(mlat .eq. 0) go to 324
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,mn)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,mn)
  324 continue
  325 continue
  326 if(mp2 .gt. ndo2) go to 330
      do 329 k=1,nt
      do 328 np1=mp2,ndo2,2
      mn = mb+np1
      do 327 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  327 continue
      if(mlat .eq. 0) go to 328
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,mn) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,mn)
  328 continue
  329 continue
  330 continue
      go to 950
c
c     case ityp=4   v even,  w odd, and both cr and ci equal zero 
c
c     case m = 0
c
  400 do 415 k=1,nt
      do 415 np1=2,ndo2,2
      do 415 i=1,imid
      ve(i,1,k)=ve(i,1,k)+br(1,np1,k)*vb(i,np1)
  415 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 430 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp2 .gt. ndo2) go to 430
      do 429 k=1,nt
      do 428 np1=mp2,ndo2,2
      mn = mb+np1
      do 427 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  427 continue
      if(mlat .eq. 0) go to 428
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,mn) 
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,mn)
  428 continue
  429 continue
  430 continue
      go to 950
c
c     case ityp=5   v even,  w odd,     br and bi equal zero 
c
c     case m = 0
c
  500 do 516 k=1,nt
      do 516 np1=3,ndo1,2
      do 516 i=1,imm1
      wo(i,1,k)=wo(i,1,k)-cr(1,np1,k)*vb(i,np1)
  516 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 530 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 530
      do 525 k=1,nt
      do 524 np1=mp1,ndo1,2
      mn = mb+np1
      do 523 i=1,imm1
      ve(i,2*mp1-2,k) = ve(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      ve(i,2*mp1-1,k) = ve(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      wo(i,2*mp1-2,k) = wo(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      wo(i,2*mp1-1,k) = wo(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  523 continue
      if(mlat .eq. 0) go to 524
      ve(imid,2*mp1-2,k) = ve(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,mn)
      ve(imid,2*mp1-1,k) = ve(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,mn)
  524 continue
  525 continue
  530 continue
      go to 950
c
c     case ityp=6   v odd  ,  w even
c
c     case m = 0
c
  600 do 615 k=1,nt
      do 615 np1=2,ndo2,2
      do 615 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1)
  615 continue
      do 616 k=1,nt
      do 616 np1=3,ndo1,2
      do 616 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1)
  616 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 630 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 626
      do 625 k=1,nt
      do 624 np1=mp1,ndo1,2
      mn = mb+np1
      do 623 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  623 continue
      if(mlat .eq. 0) go to 624
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,mn) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,mn)
  624 continue
  625 continue
  626 if(mp2 .gt. ndo2) go to 630
      do 629 k=1,nt
      do 628 np1=mp2,ndo2,2
      mn = mb+np1
      do 627 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  627 continue
      if(mlat .eq. 0) go to 628
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,mn)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,mn)
  628 continue
  629 continue
  630 continue
      go to 950
c
c     case ityp=7   v odd, w even   cr and ci equal zero
c
c     case m = 0
c
  700 do 716 k=1,nt
      do 716 np1=3,ndo1,2
      do 716 i=1,imm1
      vo(i,1,k)=vo(i,1,k)+br(1,np1,k)*vb(i,np1)
  716 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 730 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 730
      do 725 k=1,nt
      do 724 np1=mp1,ndo1,2
      mn = mb+np1
      do 723 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,mn)
  723 continue
      if(mlat .eq. 0) go to 724
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,mn) 
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,mn)
  724 continue
  725 continue
  730 continue
      go to 950
c
c     case ityp=8   v odd,  w even   br and bi equal zero
c
c     case m = 0
c
  800 do 815 k=1,nt
      do 815 np1=2,ndo2,2
      do 815 i=1,imid
      we(i,1,k)=we(i,1,k)-cr(1,np1,k)*vb(i,np1)
  815 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) go to 950
      do 830 mp1=2,mmax
      m = mp1-1
c     mb = m*(nlat-1)-(m*(m-1))/2
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp2 .gt. ndo2) go to 830
      do 829 k=1,nt
      do 828 np1=mp2,ndo2,2
      mn = mb+np1
      do 827 i=1,imm1
      vo(i,2*mp1-2,k) = vo(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,mn)
      vo(i,2*mp1-1,k) = vo(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,mn)
      we(i,2*mp1-2,k) = we(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,mn)
      we(i,2*mp1-1,k) = we(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,mn)
  827 continue
      if(mlat .eq. 0) go to 828
      we(imid,2*mp1-2,k) = we(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,mn)
      we(imid,2*mp1-1,k) = we(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,mn)
  828 continue
  829 continue
  830 continue
  950 continue
      do 14 k=1,nt
      call hrfftb(idv,nlon,ve(1,1,k),idv,wrfft,work)
      call hrfftb(idv,nlon,we(1,1,k),idv,wrfft,work)
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
      subroutine vhsgsi(nlat,nlon,wvhsgs,lvhsgs,dwork,ldwork,ierror)
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
      dimension wvhsgs(*)
      double precision dwork(*)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      imid = (nlat+1)/2
      lmn = (nlat*(nlat+1))/2
      if(lvhsgs .lt. 2*(imid*lmn)+nlon+15) return
      ierror = 4
      if (ldwork .lt. (nlat*3*(nlat+3)+2)/2) return
      ierror = 0
c
c     set saved work space pointers
c
      jw1 = 1
      jw2 = jw1+imid*lmn
      jw3 = jw2+imid*lmn
c
c     set unsaved work space pointers
c
      iw1 = 1
      iw2 = iw1+nlat
      iw3 = iw2+nlat
      iw4 = iw3+3*imid*nlat
c     iw2 = iw1+nlat+nlat
c     iw3 = iw2+nlat+nlat
c     iw4 = iw3+6*imid*nlat
      call vhgsi1(nlat,imid,wvhsgs(jw1),wvhsgs(jw2),
     +dwork(iw1),dwork(iw2),dwork(iw3),dwork(iw4))
      call hrffti(nlon,wvhsgs(jw3))
      return
      end
      subroutine vhgsi1(nlat,imid,vb,wb,dthet,dwts,dpbar,work)
      dimension vb(imid,*),wb(imid,*)
      double precision abel,bbel,cbel,ssqr2,dcf
      double precision dthet(*),dwts(*),dpbar(imid,nlat,3),work(*)
c
c     compute gauss points and weights
c     use dpbar (length 3*nnlat*(nnlat+1)) as work space for gaqd
c
      lwk = nlat*(nlat+2)
      call gaqd(nlat,dthet,dwts,dpbar,lwk,ierror)
c
c     compute associated legendre functions
c
c     compute m=n=0 legendre polynomials for all theta(i)
c
      ssqr2 = 1./dsqrt(2.d0)
      do 90 i=1,imid
      dpbar(i,1,1) = ssqr2
      vb(i,1) = 0.
      wb(i,1) = 0.
   90 continue
c
c     main loop for remaining vb, and wb
c
      do 100 n=1,nlat-1
      nm = mod(n-2,3)+1
      nz = mod(n-1,3)+1
      np = mod(n,3)+1
c
c     compute dpbar for m=0
c
      call dnlfk(0,n,work)
      mn = indx(0,n,nlat)
      do 105 i=1,imid
      call dnlft(0,n,dthet(i),work,dpbar(i,1,np))
c      pbar(i,mn) = dpbar(i,1,np)
  105 continue
c
c     compute dpbar for m=1
c
      call dnlfk(1,n,work)
      mn = indx(1,n,nlat)
      do 106 i=1,imid
      call dnlft(1,n,dthet(i),work,dpbar(i,2,np))
c      pbar(i,mn) = dpbar(i,2,np)
  106 continue
  104 continue
c
c     compute and store dpbar for m=2,n
c
      if(n.lt.2) go to 108
      do 107 m=2,n
      abel = dsqrt(dble(float((2*n+1)*(m+n-2)*(m+n-3)))/
     1                dble(float((2*n-3)*(m+n-1)*(m+n))))
      bbel = dsqrt(dble(float((2*n+1)*(n-m-1)*(n-m)))/
     1                dble(float((2*n-3)*(m+n-1)*(m+n))))
      cbel = dsqrt(dble(float((n-m+1)*(n-m+2)))/
     1                dble(float((m+n-1)*(m+n))))
      id = indx(m,n,nlat)
      if (m.ge.n-1) go to 102
      do 103 i=1,imid
      dpbar(i,m+1,np) = abel*dpbar(i,m-1,nm)+bbel*dpbar(i,m+1,nm)
     1                                         -cbel*dpbar(i,m-1,np)
c      pbar(i,id) = dpbar(i,m+1,np)
  103 continue
      go to 107
  102 do 101 i=1,imid
      dpbar(i,m+1,np) = abel*dpbar(i,m-1,nm)-cbel*dpbar(i,m-1,np)
c      pbar(i,id) = dpbar(i,m+1,np)
  101 continue
  107 continue
c
c     compute the derivative of the functions
c
  108 ix = indx(0,n,nlat)
      iy = indx(n,n,nlat)
      do 125 i=1,imid
      vb(i,ix) = -dpbar(i,2,np)
      vb(i,iy) = dpbar(i,n,np)/dsqrt(dble(float(2*(n+1))))
125   continue
c
      if(n.eq.1) go to 131 
      dcf = dsqrt(dble(float(4*n*(n+1))))
      do 130 m=1,n-1
      ix = indx(m,n,nlat)     
      abel = dsqrt(dble(float((n+m)*(n-m+1))))/dcf
      bbel = dsqrt(dble(float((n-m)*(n+m+1))))/dcf
      do 130 i=1,imid
      vb(i,ix) = abel*dpbar(i,m,np)-bbel*dpbar(i,m+2,np)
130   continue
c
c     compute the vector harmonic w(theta) = m*pbar/cos(theta)
c
c     set wb=0 for m=0
c
  131 ix = indx(0,n,nlat)
      do 220 i=1,imid
      wb(i,ix) = 0.d0
220   continue
c
c     compute wb for m=1,n
c
      dcf = dsqrt(dble(float(n+n+1))/dble(float(4*n*(n+1)*(n+n-1))))
      do 230 m=1,n
      ix = indx(m,n,nlat)     
      abel = dcf*dsqrt(dble(float((n+m)*(n+m-1))))
      bbel = dcf*dsqrt(dble(float((n-m)*(n-m-1))))
      if(m.ge.n-1) go to 231
      do 229 i=1,imid
      wb(i,ix) = abel*dpbar(i,m,nz) + bbel*dpbar(i,m+2,nz)
  229 continue
      go to 230
  231 do 228 i=1,imid
      wb(i,ix) = abel*dpbar(i,m,nz)
  228 continue
  230 continue
  100 continue
      return 
      end
