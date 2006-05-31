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
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file vhagc.f
c
c     this file contains code and documentation for subroutines
c     vhagc and vhagci
c
c ... files which must be loaded with vhagc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c                                                                              
c     subroutine vhagc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhagc,lvhagc,work,lwork,ierror)
c
c     subroutine vhagc performs the vector spherical harmonic analysis
c     on the vector field (v,w) and stores the result in the arrays
c     br,bi,cr, and ci. v(i,j) and w(i,j) are the colatitudinal
c     (measured from the north pole) and east longitudinal components
c     respectively, located at the gaussian colatitude point theta(i)
c     and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (v,w) is given at output parameters v,w in 
c     subroutine vhsec.  
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
c     ityp   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and 
c                 j=1,...,nlon.   
c
c            = 1  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and 
c                 j=1,...,nlon. the curl of (v,w) is zero. that is, 
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0. 
c                 the coefficients cr and ci are zero.
c
c            = 2  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and 
c                 j=1,...,nlon. the divergence of (v,w) is zero. i.e., 
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0. 
c                 the coefficients br and bi are zero.
c
c            = 3  v is symmetric and w is antisymmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  v is symmetric and w is antisymmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is, 
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0. 
c                 the coefficients cr and ci are zero.
c
c            = 5  v is symmetric and w is antisymmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e., 
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0. 
c                 the coefficients br and bi are zero.
c
c            = 6  v is antisymmetric and w is symmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  v is antisymmetric and w is symmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is, 
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0. 
c                 the coefficients cr and ci are zero.
c
c            = 8  v is antisymmetric and w is symmetric about the 
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for 
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e., 
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0. 
c                 the coefficients br and bi are zero.
c
c
c     nt     the number of analyses.  in the program that calls vhagc,
c            the arrays v,w,br,bi,cr, and ci can be three dimensional
c            in which case multiple analyses will be performed.
c            the third index is the analysis index which assumes the 
c            values k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     v,w    two or three dimensional arrays (see input parameter nt)
c            that contain the vector function to be analyzed.
c            v is the colatitudnal component and w is the east 
c            longitudinal component. v(i,j),w(i,j) contain the
c            components at colatitude theta(i) = (i-1)*pi/(nlat-1)
c            and longitude phi(j) = (j-1)*2*pi/nlon. the index ranges
c            are defined above at the input parameter ityp.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhagc. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhagc. jdvw must be at least nlon.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhagc. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhagc. ndab must be at
c            least nlat.
c
c     wvhagc an array which must be initialized by subroutine vhagci.
c            once initialized, wvhagc can be used repeatedly by vhagc
c            as long as nlon and nlat remain unchanged.  wvhagc must
c            not be altered between calls of vhagc.
c
c     lvhagc the dimension of the array wvhagc as it appears in the
c            program that calls vhagc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhagc must be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhagc. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c               2*nlat*(2*nlon*nt+3*l2)
c
c            if ityp .gt. 2 then lwork must be at least
c
c               2*l2*(2*nlon*nt+3*nlat)
c
c
c
c     **************************************************************
c
c     output parameters
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given 
c            in the discription of subroutine vhsec. br(mp1,np1),
c            bi(mp1,np1),cr(mp1,np1), and ci(mp1,np1) are computed 
c            for mp1=1,...,mmax and np1=mp1,...,nlat except for np1=nlat
c            and odd mp1. mmax=min0(nlat,nlon/2) if nlon is even or 
c            mmax=min0(nlat,(nlon+1)/2) if nlon is odd. 
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
c            = 9  error in the specification of lvhagc
c            = 10 error in the specification of lwork
c
c ****************************************************************
c
c     subroutine vhagci(nlat,nlon,wvhagc,lvhagc,dwork,ldwork,ierror)
c
c     subroutine vhagci initializes the array wvhagc which can then be
c     used repeatedly by subroutine vhagc until nlat or nlon is changed.
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
c     lvhagc the dimension of the array wvhagc as it appears in the
c            program that calls vhagci.  define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhagc must be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15
c
c
c     dwork  a double precision work array that does not need to be saved
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls vhagci. ldwork must be at least
c
c               2*nlat*(nlat+1)+1
c
c
c     **************************************************************
c
c     output parameters
c
c     wvhagc an array which is initialized for use by subroutine vhagc.
c            once initialized, wvhagc can be used repeatedly by vhagc
c            as long as nlat and nlon remain unchanged.  wvhagc must not
c            be altered between calls of vhagc.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhagc
c            = 4  error in the specification of lwork
c
      subroutine vhagc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     1           mdab,ndab,wvhagc,lvhagc,work,lwork,ierror)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(1),wvhagc(1)
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
     +   (ityp.gt.2 .and. idvw.lt.imid)) return
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
      if(lvhagc .lt. 2*(lzz1+labc)+nlon+imid+15) return
      ierror = 10
      if (ityp.le.2 .and. lwork.lt.nlat*(4*nlon*nt+6*imid)) return
      if (ityp.gt.2 .and. lwork.lt.imid*(4*nlon*nt+6*nlat)) return
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
      lwzvin = lzz1+labc
      jw1 = (nlat+1)/2+1
      jw2 = jw1+lwzvin
      jw3 = jw2+lwzvin
      call vhagc1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,ndab,
     +br,bi,cr,ci,idv,work,work(iw1),work(iw2),work(iw3),
     +work(iw4),work(iw5),wvhagc,wvhagc(jw1),wvhagc(jw2),wvhagc(jw3))
      return
      end
      subroutine vhagc1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,
     +ndab,br,bi,cr,ci,idv,ve,vo,we,wo,vb,wb,wts,wvbin,wwbin,wrfft)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          ve(idv,nlon,1),vo(idv,nlon,1),we(idv,nlon,1),
     3          wo(idv,nlon,1),wts(*),wvbin(1),wwbin(1),wrfft(1),
     4          vb(imid,nlat,3),wb(imid,nlat,3)
      nlp1 = nlat+1
      tsn = 2./nlon
      fsn = 4./nlon
      mlat = mod(nlat,2)
      mlon = mod(nlon,2)
      mmax = min0(nlat,(nlon+1)/2)
      imm1 = imid
      if(mlat .ne. 0) imm1 = imid-1
      if(ityp .gt. 2) go to 3  
      do 5 k=1,nt 
      do 5 i=1,imm1
      do 5 j=1,nlon
      ve(i,j,k) = tsn*(v(i,j,k)+v(nlp1-i,j,k))
      vo(i,j,k) = tsn*(v(i,j,k)-v(nlp1-i,j,k))
      we(i,j,k) = tsn*(w(i,j,k)+w(nlp1-i,j,k))
      wo(i,j,k) = tsn*(w(i,j,k)-w(nlp1-i,j,k))
    5 continue
      go to 2
    3 do 8 k=1,nt
      do 8 i=1,imm1 
      do 8 j=1,nlon
      ve(i,j,k) = fsn*v(i,j,k)
      vo(i,j,k) = fsn*v(i,j,k)
      we(i,j,k) = fsn*w(i,j,k)
      wo(i,j,k) = fsn*w(i,j,k)
    8 continue
    2 if(mlat .eq. 0) go to 7
      do 6 k=1,nt 
      do 6 j=1,nlon
      ve(imid,j,k) = tsn*v(imid,j,k)
      we(imid,j,k) = tsn*w(imid,j,k)
    6 continue
    7 do 9 k=1,nt
      call hrfftf(idv,nlon,ve(1,1,k),idv,wrfft,vb)
      call hrfftf(idv,nlon,we(1,1,k),idv,wrfft,vb)
    9 continue 
      ndo1 = nlat
      ndo2 = nlat
      if(mlat .ne. 0) ndo1 = nlat-1
      if(mlat .eq. 0) ndo2 = nlat-1
      if(ityp.eq.2 .or. ityp.eq.5 .or. ityp.eq.8) go to 11 
      do 10 k=1,nt
      do 10 mp1=1,mmax
      do 10 np1=mp1,nlat
      br(mp1,np1,k)=0.
      bi(mp1,np1,k)=0.
   10 continue
   11 if(ityp.eq.1 .or. ityp.eq.4 .or. ityp.eq.7) go to 13 
      do 12 k=1,nt
      do 12 mp1=1,mmax
      do 12 np1=mp1,nlat
      cr(mp1,np1,k)=0.
      ci(mp1,np1,k)=0.
   12 continue
   13 itypp = ityp+1
      go to (1,100,200,300,400,500,600,700,800),itypp
c
c     case ityp=0 ,  no symmetries
c
    1 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 15 k=1,nt
      do 1015 i=1,imid
      tv = ve(i,1,k)*wts(i)
      tw = we(i,1,k)*wts(i)
      do 10015 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
10015 continue
 1015 continue
   15 continue
      do 16 k=1,nt
      do 1016 i=1,imm1
      tv = vo(i,1,k)*wts(i)
      tw = wo(i,1,k)*wts(i)
      do 10016 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
10016 continue
 1016 continue
   16 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 20 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 17
      do 23 k=1,nt
      do 1023 i=1,imm1
c
c     set temps to optimize quadrature
c
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      do 10023 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tvo2
     +                             +wb(i,np1,iw)*twe1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tvo1
     +                             -wb(i,np1,iw)*twe2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*two2
     +                             +wb(i,np1,iw)*tve1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*two1
     +                             -wb(i,np1,iw)*tve2
10023 continue
 1023 continue
   23 continue

      if(mlat .eq. 0) go to 17
      i = imid
      do 24 k=1,nt
      do 1024 np1=mp1,ndo1,2
      br(mp1,np1,k)=br(mp1,np1,k)+wb(i,np1,iw)*we(i,2*mp1-1,k)*wts(i)
      bi(mp1,np1,k)=bi(mp1,np1,k)-wb(i,np1,iw)*we(i,2*mp1-2,k)*wts(i)
      cr(mp1,np1,k)=cr(mp1,np1,k)+wb(i,np1,iw)*ve(i,2*mp1-1,k)*wts(i)
      ci(mp1,np1,k)=ci(mp1,np1,k)-wb(i,np1,iw)*ve(i,2*mp1-2,k)*wts(i)
 1024 continue
   24 continue
   17 if(mp2 .gt. ndo2) go to 20
      do 21 k=1,nt
      do 1021 i=1,imm1
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      do 10021 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tve2
     1                             +wb(i,np1,iw)*two1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tve1
     1                             -wb(i,np1,iw)*two2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*twe2
     1                             +wb(i,np1,iw)*tvo1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*twe1
     1                             -wb(i,np1,iw)*tvo2
10021 continue
 1021 continue
   21 continue

      if(mlat .eq. 0) go to 20
      i = imid
      do 22 k=1,nt
      do 1022 np1=mp2,ndo2,2
      br(mp1,np1,k)=br(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-2,k)*wts(i)
      bi(mp1,np1,k)=bi(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-1,k)*wts(i)
      cr(mp1,np1,k)=cr(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-2,k)*wts(i)
      ci(mp1,np1,k)=ci(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-1,k)*wts(i)
 1022 continue
   22 continue
   20 continue
      return
c
c     case ityp=1 ,  no symmetries but cr and ci equal zero
c
  100 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 115 k=1,nt
      do 115 i=1,imid
      tv = ve(i,1,k)*wts(i)
      do 115 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  115 continue
      do 116 k=1,nt
      do 116 i=1,imm1
      tv = vo(i,1,k)*wts(i)
      do 116 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  116 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 120 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 117
      do 123 k=1,nt
      do 123 i=1,imm1
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      do 123 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tvo2
     +                             +wb(i,np1,iw)*twe1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tvo1
     +                             -wb(i,np1,iw)*twe2
  123 continue
      if(mlat .eq. 0) go to 117
      i = imid
      do 124 k=1,nt
      do 124 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(i,np1,iw)*we(i,2*mp1-1,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(i,np1,iw)*we(i,2*mp1-2,k)*wts(i)
  124 continue
  117 if(mp2 .gt. ndo2) go to 120
      do 121 k=1,nt
      do 121 i=1,imm1
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      do 121 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tve2
     +                             +wb(i,np1,iw)*two1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tve1
     +                             -wb(i,np1,iw)*two2
  121 continue
      if(mlat .eq. 0) go to 120
      i = imid
      do 122 k=1,nt
      do 122 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-2,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-1,k)*wts(i)
  122 continue
  120 continue
      return
c
c     case ityp=2 ,  no symmetries but br and bi equal zero   
c
  200 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 215 k=1,nt
      do 215 i=1,imid
      tw = we(i,1,k)*wts(i)
      do 215 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  215 continue
      do 216 k=1,nt
      do 216 i=1,imm1
      tw = wo(i,1,k)*wts(i)
      do 216 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  216 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 220 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 217
      do 223 k=1,nt
      do 223 i=1,imm1
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      do 223 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*two2
     +                             +wb(i,np1,iw)*tve1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*two1
     +                             -wb(i,np1,iw)*tve2
  223 continue
      if(mlat .eq. 0) go to 217
      i = imid
      do 224 k=1,nt
      do 224 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(i,np1,iw)*ve(i,2*mp1-1,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(i,np1,iw)*ve(i,2*mp1-2,k)*wts(i)
  224 continue
  217 if(mp2 .gt. ndo2) go to 220
      do 221 k=1,nt
      do 221 i=1,imm1
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      do 221 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*twe2
     +                             +wb(i,np1,iw)*tvo1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*twe1
     +                             -wb(i,np1,iw)*tvo2
  221 continue
      if(mlat .eq. 0) go to 220
      i = imid
      do 222 k=1,nt
      do 222 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-2,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-1,k)*wts(i)
  222 continue
  220 continue
      return
c
c     case ityp=3 ,  v even , w odd
c
  300 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 315 k=1,nt
      do 315 i=1,imid
      tv = ve(i,1,k)*wts(i)
      do 315 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  315 continue
      do 316 k=1,nt
      do 316 i=1,imm1
      tw = wo(i,1,k)*wts(i)
      do 316 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  316 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 320 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 317
      do 323 k=1,nt
      do 323 i=1,imm1
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      do 323 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*two2
     +                             +wb(i,np1,iw)*tve1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*two1
     +                             -wb(i,np1,iw)*tve2
  323 continue
      if(mlat .eq. 0) go to 317
      i = imid
      do 324 k=1,nt
      do 324 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(i,np1,iw)*ve(i,2*mp1-1,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(i,np1,iw)*ve(i,2*mp1-2,k)*wts(i)
  324 continue
  317 if(mp2 .gt. ndo2) go to 320
      do 321 k=1,nt
      do 321 i=1,imm1
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      do 321 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tve2
     +                             +wb(i,np1,iw)*two1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tve1
     +                             -wb(i,np1,iw)*two2
  321 continue
      if(mlat .eq. 0) go to 320
      i = imid
      do 322 k=1,nt
      do 322 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-2,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-1,k)*wts(i)
  322 continue
  320 continue
      return
c
c     case ityp=4 ,  v even, w odd, and cr and ci equal 0. 
c
  400 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 415 k=1,nt
      do 415 i=1,imid
      tv = ve(i,1,k)*wts(i)
      do 415 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  415 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 420 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(1,nlat,nlon,m,vb,iv,wvbin)
      call wbin(1,nlat,nlon,m,wb,iw,wwbin)
      if(mp2 .gt. ndo2) go to 420
      do 421 k=1,nt
      do 421 i=1,imm1
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      do 421 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tve2
     +                             +wb(i,np1,iw)*two1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tve1
     +                             -wb(i,np1,iw)*two2
  421 continue
      if(mlat .eq. 0) go to 420
      i = imid
      do 422 k=1,nt
      do 422 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-2,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*ve(i,2*mp1-1,k)*wts(i)
  422 continue
  420 continue
      return
c
c     case ityp=5   v even, w odd, and br and bi equal zero
c
  500 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 516 k=1,nt
      do 516 i=1,imm1
      tw = wo(i,1,k)*wts(i)
      do 516 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  516 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 520 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(2,nlat,nlon,m,vb,iv,wvbin)
      call wbin(2,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 520
      do 523 k=1,nt
      do 523 i=1,imm1
      two1 = wo(i,2*mp1-1,k)*wts(i)
      two2 = wo(i,2*mp1-2,k)*wts(i)
      tve1 = ve(i,2*mp1-1,k)*wts(i)
      tve2 = ve(i,2*mp1-2,k)*wts(i)
      do 523 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*two2
     +                             +wb(i,np1,iw)*tve1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*two1
     +                             -wb(i,np1,iw)*tve2
  523 continue
      if(mlat .eq. 0) go to 520
      i = imid
      do 524 k=1,nt
      do 524 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(i,np1,iw)*ve(i,2*mp1-1,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(i,np1,iw)*ve(i,2*mp1-2,k)*wts(i)
  524 continue
  520 continue
      return
c
c     case ityp=6 ,  v odd , w even
c
  600 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 615 k=1,nt
      do 615 i=1,imid
      tw = we(i,1,k)*wts(i)
      do 615 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  615 continue
      do 616 k=1,nt
      do 616 i=1,imm1
      tv = vo(i,1,k)*wts(i)
      do 616 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  616 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 620 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(0,nlat,nlon,m,vb,iv,wvbin)
      call wbin(0,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 617
      do 623 k=1,nt
      do 623 i=1,imm1
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      do 623 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tvo2
     +                             +wb(i,np1,iw)*twe1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tvo1
     +                             -wb(i,np1,iw)*twe2
  623 continue
      if(mlat .eq. 0) go to 617
      i = imid
      do 624 k=1,nt
      do 624 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(i,np1,iw)*we(i,2*mp1-1,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(i,np1,iw)*we(i,2*mp1-2,k)*wts(i)
  624 continue
  617 if(mp2 .gt. ndo2) go to 620
      do 621 k=1,nt
      do 621 i=1,imm1
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      do 621 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*twe2
     +                             +wb(i,np1,iw)*tvo1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*twe1
     +                             -wb(i,np1,iw)*tvo2
  621 continue
      if(mlat .eq. 0) go to 620
      i = imid
      do 622 k=1,nt
      do 622 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-2,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-1,k)*wts(i)
  622 continue
  620 continue
      return
c
c     case ityp=7   v odd, w even, and cr and ci equal zero
c
  700 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 716 k=1,nt
      do 716 i=1,imm1
      tv = vo(i,1,k)*wts(i)
      do 716 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1,iv)*tv
  716 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 720 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(2,nlat,nlon,m,vb,iv,wvbin)
      call wbin(2,nlat,nlon,m,wb,iw,wwbin)
      if(mp1 .gt. ndo1) go to 720
      do 723 k=1,nt
      do 723 i=1,imm1
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      do 723 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1,iv)*tvo2
     +                             +wb(i,np1,iw)*twe1
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1,iv)*tvo1
     +                             -wb(i,np1,iw)*twe2
  723 continue
      if(mlat .eq. 0) go to 720
      i = imid
      do 724 k=1,nt
      do 724 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(i,np1,iw)*we(i,2*mp1-1,k)*wts(i)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(i,np1,iw)*we(i,2*mp1-2,k)*wts(i)
  724 continue
  720 continue
      return
c
c     case ityp=8   v odd, w even, and both br and bi equal zero
c
  800 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m=0
c
      do 815 k=1,nt
      do 815 i=1,imid
      tw = we(i,1,k)*wts(i)
      do 815 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1,iv)*tw
  815 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 820 mp1=2,mmax
      m = mp1-1
      mp2 = mp1+1
      call vbin(1,nlat,nlon,m,vb,iv,wvbin)
      call wbin(1,nlat,nlon,m,wb,iw,wwbin)
      if(mp2 .gt. ndo2) go to 820
      do 821 k=1,nt
      do 821 i=1,imm1
      twe1 = we(i,2*mp1-1,k)*wts(i)
      twe2 = we(i,2*mp1-2,k)*wts(i)
      tvo1 = vo(i,2*mp1-1,k)*wts(i)
      tvo2 = vo(i,2*mp1-2,k)*wts(i)
      do 821 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*twe2
     +                             +wb(i,np1,iw)*tvo1
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*twe1
     +                             -wb(i,np1,iw)*tvo2
  821 continue
      if(mlat .eq. 0) go to 820
      i = imid
      do 822 k=1,nt
      do 822 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-2,k)*wts(i)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1,iv)*we(i,2*mp1-1,k)*wts(i)
  822 continue
  820 continue
      return
      end
      subroutine vhagci(nlat,nlon,wvhagc,lvhagc,dwork,ldwork,ierror)
      dimension wvhagc(1)
      double precision dwork(*)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      imid = (nlat+1)/2
      if(lvhagc .lt. 2*(lzz1+labc)+nlon+imid+15) return
      ierror = 4
      if (ldwork .lt. 2*nlat*(nlat+1)+1) return
      ierror = 0
c
c     compute gaussian points in first nlat+1 words of dwork
c     double precision
c
      lwk = nlat*(nlat+2)

      jw1 = 1
c     jw2 = jw1+nlat+nlat
c     jw3 = jw2+nlat+nlat
      jw2 = jw1+nlat
      jw3 = jw2+nlat
      call gaqd(nlat,dwork(jw1),dwork(jw2),dwork(jw3),lwk,ierror)
      imid = (nlat+1)/2
c
c     set first imid words of double precision weights in dwork
c     as single precision in first imid words of wvhagc
c
      call setwts(imid,dwork(nlat+1),wvhagc)
c
c     first nlat+1 words of dwork contain  double theta
c
c     iwrk = nlat+2
      iwrk = (nlat+1)/2 +1
      iw1 = imid+1
      call vbgint (nlat,nlon,dwork,wvhagc(iw1),dwork(iwrk))
      lwvbin = lzz1+labc
      iw2 = iw1+lwvbin
      call wbgint (nlat,nlon,dwork,wvhagc(iw2),dwork(iwrk))
      iw3 = iw2+lwvbin
      call hrffti(nlon,wvhagc(iw3))
      return
      end
      subroutine setwts(imid,dwts,wts)
c
c     set first imid =(nlat+1)/2 of double precision weights in dwts
c     as single precision in wts
c
      dimension dwts(imid),wts(imid)
      double precision dwts
      do 1 i=1,imid
      wts(i) = dwts(i)
    1 continue
      return
      end
