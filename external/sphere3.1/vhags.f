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
c ... file vhags.f
c
c     this file contains code and documentation for subroutines
c     vhags and vhagsi
c
c ... files which must be loaded with vhags.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine vhags(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhags,lvhags,work,lwork,ierror)
c
c     subroutine vhags performs the vector spherical harmonic analysis
c     on the vector field (v,w) and stores the result in the arrays
c     br, bi, cr, and ci. v(i,j) and w(i,j) are the colatitudinal 
c     (measured from the north pole) and east longitudinal components
c     respectively, located at the gaussian colatitude point theta(i)
c     and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (v,w) is given at output parameters v,w in 
c     subroutine vhses.  
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
c     nt     the number of analyses.  in the program that calls vhags,
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
c            components at the gaussian colatitude point theta(i)
c            and longitude phi(j) = (j-1)*2*pi/nlon. the index ranges
c            are defined above at the input parameter ityp.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhags. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhags. jdvw must be at least nlon.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhags. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhags. ndab must be at
c            least nlat.
c
c     wvhags an array which must be initialized by subroutine vhgsi.
c            once initialized, wvhags can be used repeatedly by vhags
c            as long as nlon and nlat remain unchanged.  wvhags must
c            not be altered between calls of vhags.
c
c     lvhags the dimension of the array wvhags as it appears in the
c            program that calls vhags. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhags must be at least
c
c            l1*l2(nlat+nlat-l1+1)+nlon+15
c
c        ??? (nlat+1)*(nlat+1)*nlat/2 + nlon + 15
c
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhags. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c            the larger of the two quantities
c
c               3*nlat*(nlat+1)+2  (required by vhagsi)
c
c            and
c
c               (2*nt+1)*nlat*nlon
c
c            if ityp .gt. 2 then lwork must be at least
c            the larger of the two quantities
c
c               3*nlat*(nlat+1)+2  (required by vhagsi)
c
c            and
c
c              (2*nt+1)*l2*nlon
c
c
c     **************************************************************
c
c     output parameters
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given 
c            in the discription of subroutine vhses. br(mp1,np1),
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
c            = 9  error in the specification of lvhags
c            = 10 error in the specification of lwork
c
c
c     subroutine vhagsi(nlat,nlon,wvhags,lvhags,work,lwork,ierror)
c
c     subroutine vhagsi initializes the array wvhags which can then be
c     used repeatedly by subroutine vhags until nlat or nlon is changed.
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
c     lvhags the dimension of the array wvhags as it appears in the
c            program that calls vhagsi.  define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhags must be at least
c
c               3*nlat*(nlat+1)+2  (required by vhagsi)
c
c     dwork  a double precision work space that does not need to be saved
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls vhagsi. ldwork must be at least
c
c                   (3*nlat*(nlat+3)+2)/2
c
c     **************************************************************
c
c     output parameters
c
c     wvhags an array which is initialized for use by subroutine vhags.
c            once initialized, wvhags can be used repeatedly by vhags
c            as long as nlat and nlon remain unchanged.  wvhags must not
c            be altered between calls of vhags.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhags
c            = 4  error in the specification of ldwork
c
      subroutine vhags(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     1           mdab,ndab,wvhags,lvhags,work,lwork,ierror)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(1),wvhags(1)
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
      if(lvhags .lt. lzimn+lzimn+nlon+15) return
      ierror = 10
      idv = nlat
      if(ityp .gt. 2) idv = imid
      lnl = nt*idv*nlon
      if(lwork .lt. lnl+lnl+idv*nlon) return
      ierror = 0
      ist = 0
      if(ityp .le. 2) ist = imid
c
c     set wvhags pointers
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
      call vhags1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,ndab,
     +     br,bi,cr,ci,idv,work,work(iw1),work(iw2),work(iw3),
     +     work(iw4),idz,wvhags(jw1),wvhags(jw2),wvhags(jw3))
      return
      end

      subroutine vhags1(nlat,nlon,ityp,nt,imid,idvw,jdvw,v,w,mdab,
     +ndab,br,bi,cr,ci,idv,ve,vo,we,wo,work,idz,vb,wb,wrfft)
      dimension v(idvw,jdvw,1),w(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          ve(idv,nlon,1),vo(idv,nlon,1),we(idv,nlon,1),
     3          wo(idv,nlon,1),work(1),
     4          vb(imid,1),wb(imid,1),wrfft(1)
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
      call hrfftf(idv,nlon,ve(1,1,k),idv,wrfft,work)
      call hrfftf(idv,nlon,we(1,1,k),idv,wrfft,work)
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
c     case m=0
c
    1 do 15 k=1,nt
      do 15 i=1,imid
      do 15 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*ve(i,1,k)
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*we(i,1,k)
   15 continue
      do 16 k=1,nt
      do 16 i=1,imm1
      do 16 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*vo(i,1,k)
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*wo(i,1,k)
   16 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 20 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 17
      do 23 k=1,nt
      do 23 i=1,imm1
      do 23 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*we(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*we(i,2*mp1-2,k)
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*ve(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*ve(i,2*mp1-2,k)
   23 continue
      if(mlat .eq. 0) go to 17
      do 24 k=1,nt
      do 24 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(imid,np1+mb)*we(imid,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(imid,np1+mb)*we(imid,2*mp1-2,k)
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(imid,np1+mb)*ve(imid,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(imid,np1+mb)*ve(imid,2*mp1-2,k)
   24 continue
   17 if(mp2 .gt. ndo2) go to 20
      do 21 k=1,nt
      do 21 i=1,imm1
      do 21 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*wo(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*wo(i,2*mp1-2,k)
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*vo(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*vo(i,2*mp1-2,k)
   21 continue
      if(mlat .eq. 0) go to 20
      do 22 k=1,nt
      do 22 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-2,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-1,k)
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-2,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-1,k)
   22 continue
   20 continue
      return
c
c     case ityp=1 ,  no symmetries but cr and ci equal zero
c
c     case m=0
c
  100 do 115 k=1,nt
      do 115 i=1,imid
      do 115 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*ve(i,1,k)
  115 continue
      do 116 k=1,nt
      do 116 i=1,imm1
      do 116 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*vo(i,1,k)
  116 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 120 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 117
      do 123 k=1,nt
      do 123 i=1,imm1
      do 123 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*we(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*we(i,2*mp1-2,k)
  123 continue
      if(mlat .eq. 0) go to 117
      do 124 k=1,nt
      do 124 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(imid,np1+mb)*we(imid,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(imid,np1+mb)*we(imid,2*mp1-2,k)
  124 continue
  117 if(mp2 .gt. ndo2) go to 120
      do 121 k=1,nt
      do 121 i=1,imm1
      do 121 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*wo(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*wo(i,2*mp1-2,k)
  121 continue
      if(mlat .eq. 0) go to 120
      do 122 k=1,nt
      do 122 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-2,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-1,k)
  122 continue
  120 continue
      return
c
c     case ityp=2 ,  no symmetries but br and bi equal zero   
c
c     case m=0
c
  200 do 215 k=1,nt
      do 215 i=1,imid
      do 215 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*we(i,1,k)
  215 continue
      do 216 k=1,nt
      do 216 i=1,imm1
      do 216 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*wo(i,1,k)
  216 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 220 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 217
      do 223 k=1,nt
      do 223 i=1,imm1
      do 223 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*ve(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*ve(i,2*mp1-2,k)
  223 continue
      if(mlat .eq. 0) go to 217
      do 224 k=1,nt
      do 224 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(imid,np1+mb)*ve(imid,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(imid,np1+mb)*ve(imid,2*mp1-2,k)
  224 continue
  217 if(mp2 .gt. ndo2) go to 220
      do 221 k=1,nt
      do 221 i=1,imm1
      do 221 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*vo(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*vo(i,2*mp1-2,k)
  221 continue
      if(mlat .eq. 0) go to 220
      do 222 k=1,nt
      do 222 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-2,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-1,k)
  222 continue
  220 continue
      return
c
c     case ityp=3 ,  v even , w odd
c
c     case m=0
c
  300 do 315 k=1,nt
      do 315 i=1,imid
      do 315 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*ve(i,1,k)
  315 continue
      do 316 k=1,nt
      do 316 i=1,imm1
      do 316 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*wo(i,1,k)
  316 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 320 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 317
      do 323 k=1,nt
      do 323 i=1,imm1
      do 323 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*ve(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*ve(i,2*mp1-2,k)
  323 continue
      if(mlat .eq. 0) go to 317
      do 324 k=1,nt
      do 324 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(imid,np1+mb)*ve(imid,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(imid,np1+mb)*ve(imid,2*mp1-2,k)
  324 continue
  317 if(mp2 .gt. ndo2) go to 320
      do 321 k=1,nt
      do 321 i=1,imm1
      do 321 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*wo(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*wo(i,2*mp1-2,k)
  321 continue
      if(mlat .eq. 0) go to 320
      do 322 k=1,nt
      do 322 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-2,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-1,k)
  322 continue
  320 continue
      return
c
c     case ityp=4 ,  v even, w odd, and cr and ci equal 0. 
c
c     case m=0
c
  400 do 415 k=1,nt
      do 415 i=1,imid
      do 415 np1=2,ndo2,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*ve(i,1,k)
  415 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 420 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp2 .gt. ndo2) go to 420
      do 421 k=1,nt
      do 421 i=1,imm1
      do 421 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*wo(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*ve(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*wo(i,2*mp1-2,k)
  421 continue
      if(mlat .eq. 0) go to 420
      do 422 k=1,nt
      do 422 np1=mp2,ndo2,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-2,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(imid,np1+mb)*ve(imid,2*mp1-1,k)
  422 continue
  420 continue
      return
c
c     case ityp=5   v even, w odd, and br and bi equal zero
c
c     case m=0
c
  500 do 516 k=1,nt
      do 516 i=1,imm1
      do 516 np1=3,ndo1,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*wo(i,1,k)
  516 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 520 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 520
      do 523 k=1,nt
      do 523 i=1,imm1
      do 523 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*ve(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*wo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*ve(i,2*mp1-2,k)
  523 continue
      if(mlat .eq. 0) go to 520
      do 524 k=1,nt
      do 524 np1=mp1,ndo1,2
      cr(mp1,np1,k) = cr(mp1,np1,k)+wb(imid,np1+mb)*ve(imid,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-wb(imid,np1+mb)*ve(imid,2*mp1-2,k)
  524 continue
  520 continue
      return
c
c     case ityp=6 ,  v odd , w even
c
c     case m=0
c
  600 do 615 k=1,nt
      do 615 i=1,imid
      do 615 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*we(i,1,k)
  615 continue
      do 616 k=1,nt
      do 616 i=1,imm1
      do 616 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*vo(i,1,k)
  616 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 620 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 617
      do 623 k=1,nt
      do 623 i=1,imm1
      do 623 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*we(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*we(i,2*mp1-2,k)
  623 continue
      if(mlat .eq. 0) go to 617
      do 624 k=1,nt
      do 624 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(imid,np1+mb)*we(imid,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(imid,np1+mb)*we(imid,2*mp1-2,k)
  624 continue
  617 if(mp2 .gt. ndo2) go to 620
      do 621 k=1,nt
      do 621 i=1,imm1
      do 621 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*vo(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*vo(i,2*mp1-2,k)
  621 continue
      if(mlat .eq. 0) go to 620
      do 622 k=1,nt
      do 622 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-2,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-1,k)
  622 continue
  620 continue
      return
c
c     case ityp=7   v odd, w even, and cr and ci equal zero
c
c     case m=0
c
  700 do 716 k=1,nt
      do 716 i=1,imm1
      do 716 np1=3,ndo1,2
      br(1,np1,k) = br(1,np1,k)+vb(i,np1)*vo(i,1,k)
  716 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 720 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp1 .gt. ndo1) go to 720
      do 723 k=1,nt
      do 723 i=1,imm1
      do 723 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*we(i,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)+vb(i,np1+mb)*vo(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*we(i,2*mp1-2,k)
  723 continue
      if(mlat .eq. 0) go to 720
      do 724 k=1,nt
      do 724 np1=mp1,ndo1,2
      br(mp1,np1,k) = br(mp1,np1,k)+wb(imid,np1+mb)*we(imid,2*mp1-1,k)
      bi(mp1,np1,k) = bi(mp1,np1,k)-wb(imid,np1+mb)*we(imid,2*mp1-2,k)
  724 continue
  720 continue
      return
c
c     case ityp=8   v odd, w even, and both br and bi equal zero
c
c     case m=0
c
  800 do 815 k=1,nt
      do 815 i=1,imid
      do 815 np1=2,ndo2,2
      cr(1,np1,k) = cr(1,np1,k)-vb(i,np1)*we(i,1,k)
  815 continue
c
c     case m = 1 through nlat-1
c
      if(mmax .lt. 2) return
      do 820 mp1=2,mmax
      m = mp1-1
      mb = m*nlat-(m*(m+1))/2
      mp2 = mp1+1
      if(mp2 .gt. ndo2) go to 820
      do 821 k=1,nt
      do 821 i=1,imm1
      do 821 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-2,k)
     1                             +wb(i,np1+mb)*vo(i,2*mp1-1,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(i,np1+mb)*we(i,2*mp1-1,k)
     1                             -wb(i,np1+mb)*vo(i,2*mp1-2,k)
  821 continue
      if(mlat .eq. 0) go to 820
      do 822 k=1,nt
      do 822 np1=mp2,ndo2,2
      cr(mp1,np1,k) = cr(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-2,k)
      ci(mp1,np1,k) = ci(mp1,np1,k)-vb(imid,np1+mb)*we(imid,2*mp1-1,k)
  822 continue
  820 continue
      return
      end
      subroutine vhagsi(nlat,nlon,wvhags,lvhags,dwork,ldwork,ierror)
      dimension wvhags(lvhags)
      double precision dwork(ldwork)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      imid = (nlat+1)/2
      lmn = (nlat*(nlat+1))/2
      if(lvhags .lt. 2*(imid*lmn)+nlon+15) return
      ierror = 4
c     if (ldwork.lt.nlat*(3*nlat+9)+2) return
      if (ldwork.lt.(nlat*(3*nlat+9)+2)/2) return
      ierror = 0
      jw1 = 1
      jw2 = jw1+imid*lmn
      jw3 = jw2+imid*lmn
      iw1 = 1
      iw2 = iw1+nlat
      iw3 = iw2+nlat
      iw4 = iw3+3*imid*nlat
c     iw2 = iw1+nlat+nlat
c     iw3 = iw2+nlat+nlat
c     iw4 = iw3+6*imid*nlat
      call vhgai1(nlat,imid,wvhags(jw1),wvhags(jw2),
     +dwork(iw1),dwork(iw2),dwork(iw3),dwork(iw4))
      call hrffti(nlon,wvhags(jw3))
      return
      end
      subroutine vhgai1(nlat,imid,vb,wb,dthet,dwts,dpbar,work)
      dimension vb(imid,*),wb(imid,*)
      double precision abel,bbel,cbel,ssqr2,dcf
      double precision dpbar(imid,nlat,3), dthet(*),dwts(*),work(*)
c     lwk = 4*nlat*(nlat+2)
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
  103 continue
      go to 107
  102 do 101 i=1,imid
      dpbar(i,m+1,np) = abel*dpbar(i,m-1,nm)-cbel*dpbar(i,m-1,np)
  101 continue
  107 continue
c
c     compute the derivative of the functions
c
  108 continue
      ix = indx(0,n,nlat)
      iy = indx(n,n,nlat)
      do 125 i=1,imid
      vb(i,ix) = -dpbar(i,2,np)*dwts(i)
      vb(i,iy) = dpbar(i,n,np)/dsqrt(dble(float(2*(n+1))))*dwts(i)
125   continue
c
      if(n.eq.1) go to 131 
      dcf = dsqrt(dble(float(4*n*(n+1))))
      do 130 m=1,n-1
      ix = indx(m,n,nlat)
      abel = dsqrt(dble(float((n+m)*(n-m+1))))/dcf
      bbel = dsqrt(dble(float((n-m)*(n+m+1))))/dcf
      do 130 i=1,imid
      vb(i,ix) = (abel*dpbar(i,m,np)-bbel*dpbar(i,m+2,np))*dwts(i)
130   continue
c
c     compute the vector harmonic w(theta) = m*pbar/cos(theta)
c
c     set wb=0 for m=0
c
  131 continue
      ix = indx(0,n,nlat)
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
      wb(i,ix) = (abel*dpbar(i,m,nz) + bbel*dpbar(i,m+2,nz))*dwts(i)
  229 continue
      go to 230
  231 do 228 i=1,imid
      wb(i,ix) = abel*dpbar(i,m,nz)*dwts(i)
  228 continue
  230 continue
  100 continue
      return 
      end

      function indx(m,n,nlat)
      integer indx
      indx = m*nlat-(m*(m+1))/2+n+1
      return
      end
