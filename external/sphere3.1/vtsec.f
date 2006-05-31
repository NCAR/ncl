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
c ... file vtsec.f
c
c     this file includes documentation and code for
c     subroutines vtsec and vtseci
c
c ... files which must be loaded with vtsec.f
c
c     sphcom.f, hrfft.f, vhaec.f, vhsec.f
c   
c     subroutine vtsec(nlat,nlon,ityp,nt,vt,wt,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvts,lwvts,work,lwork,ierror)
c
c     given the vector harmonic analysis br,bi,cr, and ci (computed
c     by subroutine vhaec) of some vector function (v,w), this 
c     subroutine computes the vector function (vt,wt) which is
c     the derivative of (v,w) with respect to colatitude theta. vtsec
c     is similar to vhsec except the vector harmonics are replaced by 
c     their derivative with respect to colatitude with the result that
c     (vt,wt) is computed instead of (v,w). vt(i,j) is the derivative
c     of the colatitudinal component v(i,j) at the point theta(i) =
c     (i-1)*pi/(nlat-1) and longitude phi(j) = (j-1)*2*pi/nlon. the
c     spectral representation of (vt,wt) is given below at output
c     parameters vt,wt.
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
c                 is performed on the entire sphere. i.e. the arrays
c                 vt(i,j),wt(i,j) are computed for i=1,...,nlat and
c                 j=1,...,nlon.   
c
c            = 1  no symmetries exist about the equator however the
c                 the coefficients cr and ci are zero. the synthesis
c                 is performed on the entire sphere.  i.e. the arrays
c                 vt(i,j),wt(i,j) are computed for i=1,...,nlat and
c                 j=1,...,nlon.
c
c            = 2  no symmetries exist about the equator however the
c                 the coefficients br and bi are zero. the synthesis
c                 is performed on the entire sphere.  i.e. the arrays
c                 vt(i,j),wt(i,j) are computed for i=1,...,nlat and 
c                 j=1,...,nlon.
c
c            = 3  vt is odd and wt is even about the equator. the 
c                 synthesis is performed on the northern hemisphere
c                 only.  i.e., if nlat is odd the arrays vt(i,j),wt(i,j)
c                 are computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the arrays vt(i,j),wt(i,j) are computed 
c                 for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  vt is odd and wt is even about the equator and the 
c                 coefficients cr and ci are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for 
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 5  vt is odd and wt is even about the equator and the 
c                 coefficients br and bi are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for 
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 6  vt is even and wt is odd about the equator. the 
c                 synthesis is performed on the northern hemisphere
c                 only.  i.e., if nlat is odd the arrays vt(i,j),wt(i,j)
c                 are computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the arrays vt(i,j),wt(i,j) are computed 
c                 for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  vt is even and wt is odd about the equator and the 
c                 coefficients cr and ci are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for 
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 8  vt is even and wt is odd about the equator and the 
c                 coefficients br and bi are zero. the synthesis is
c                 performed on the northern hemisphere only. i.e. if
c                 nlat is odd the arrays vt(i,j),wt(i,j) are computed
c                 for i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the arrays vt(i,j),wt(i,j) are computed for 
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls vtsec,
c            the arrays vt,wt,br,bi,cr, and ci can be three dimensional
c            in which case multiple syntheses will be performed.
c            the third index is the synthesis index which assumes the 
c            values k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     idvw   the first dimension of the arrays vt,wt as it appears in
c            the program that calls vtsec. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays vt,wt as it appears in
c            the program that calls vtsec. jdvw must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            of (v,w) as computed by subroutine vhaec.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsec. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vtsec. ndab must be at
c            least nlat.
c
c     wvts   an array which must be initialized by subroutine vtseci.
c            once initialized, wvts can be used repeatedly by vtsec
c            as long as nlon and nlat remain unchanged.  wvts must
c            not be altered between calls of vtsec.
c
c     lwvts  the dimension of the array wvts as it appears in the
c            program that calls vtsec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwvts must be at least
c
c            4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vtsec. define
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
c     vt,wt  two or three dimensional arrays (see input parameter nt)
c            in which the derivative of (v,w) with respect to 
c            colatitude theta is stored. vt(i,j),wt(i,j) contain the
c            derivatives at colatitude theta(i) = (i-1)*pi/(nlat-1)
c            and longitude phi(j) = (j-1)*2*pi/nlon. the index ranges
c            are defined above at the input parameter ityp. vt and wt
c            are computed from the formulas for v and w given in 
c            subroutine vhsec but with vbar and wbar replaced with
c            their derivatives with respect to colatitude. these
c            derivatives are denoted by vtbar and wtbar. 
c
c   in terms of real variables this expansion takes the form
c
c             for i=1,...,nlat and  j=1,...,nlon
c
c     vt(i,j) = the sum from n=1 to n=nlat-1 of
c
c               .5*br(1,n+1)*vtbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to 
c     n=nlat-1 of the real part of
c
c       (br(m+1,n+1)*vtbar(m,n,theta(i))
c                   -ci(m+1,n+1)*wtbar(m,n,theta(i)))*cos(m*phi(j))
c      -(bi(m+1,n+1)*vtbar(m,n,theta(i))
c                   +cr(m+1,n+1)*wtbar(m,n,theta(i)))*sin(m*phi(j))
c
c    and for i=1,...,nlat and  j=1,...,nlon
c
c     wt(i,j) = the sum from n=1 to n=nlat-1 of
c
c              -.5*cr(1,n+1)*vtbar(0,n,theta(i))
c
c     plus the sum from m=1 to m=mmax-1 of the sum from n=m to
c     n=nlat-1 of the real part of
c
c      -(cr(m+1,n+1)*vtbar(m,n,theta(i))
c                   +bi(m+1,n+1)*wtbar(m,n,theta(i)))*cos(m*phi(j))
c      +(ci(m+1,n+1)*vtbar(m,n,theta(i))
c                   -br(m+1,n+1)*wtbar(m,n,theta(i)))*sin(m*phi(j))
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
c            = 9  error in the specification of lwvts
c            = 10 error in the specification of lwork
c
c
c *******************************************************************
c
c     subroutine vtseci(nlat,nlon,wvts,lwvts,dwork,ldwork,ierror)
c
c     subroutine vtseci initializes the array wvts which can then be
c     used repeatedly by subroutine vtsec until nlat or nlon is changed.
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
c     lwvts  the dimension of the array wvts as it appears in the
c            program that calls vtsec. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwvts must be at least
c
c            4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array work as it appears in the
c            program that calls vtsec. lwork must be at least
c            2*(nlat+1)
c
c     **************************************************************
c
c     output parameters
c
c     wvts   an array which is initialized for use by subroutine vtsec.
c            once initialized, wvts can be used repeatedly by vtsec
c            as long as nlat or nlon remain unchanged.  wvts must not
c            be altered between calls of vtsec.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lwvts
c            = 4  error in the specification of ldwork
c
c **********************************************************************
c                                                                              
      subroutine vtsec(nlat,nlon,ityp,nt,vt,wt,idvw,jdvw,br,bi,cr,ci,
     1           mdab,ndab,wvts,lwvts,work,lwork,ierror)
c
      dimension vt(idvw,jdvw,1),wt(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(1),wvts(1)
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
      if(lwvts .lt. 2*(lzz1+labc)+nlon+15) return
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
      call vtsec1(nlat,nlon,ityp,nt,imid,idvw,jdvw,vt,wt,mdab,ndab,
     1     br,bi,cr,ci,idv,work,work(iw1),work(iw2),work(iw3),
     2     work(iw4),work(iw5),wvts,wvts(jw1),wvts(jw2))
      return
      end
      subroutine vtsec1(nlat,nlon,ityp,nt,imid,idvw,jdvw,vt,wt,mdab,
     1   ndab,br,bi,cr,ci,idv,vte,vto,wte,wto,vb,wb,wvbin,wwbin,wrfft)
      dimension vt(idvw,jdvw,1),wt(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          vte(idv,nlon,1),vto(idv,nlon,1),wte(idv,nlon,1),
     3          wto(idv,nlon,1),wvbin(1),wwbin(1),wrfft(1),
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
      vte(i,j,k) = 0.
      wte(i,j,k) = 0.
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
      do 15 i=1,imm1
      vto(i,1,k)=vto(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
      wto(i,1,k)=wto(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
   15 continue
      do 16 k=1,nt
      do 16 np1=3,ndo1,2
      do 16 i=1,imid
      vte(i,1,k)=vte(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
      wte(i,1,k)=wte(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
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
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
   23 continue
      if(mlat .eq. 0) go to 24
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv)
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv) 
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
   24 continue
   25 continue
   26 if(mp2 .gt. ndo2) go to 30
      do 29 k=1,nt
      do 28 np1=mp2,ndo2,2
      do 27 i=1,imm1
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
   27 continue
      if(mlat .eq. 0) go to 28
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw) 
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw)
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
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
      do 115 i=1,imm1
      vto(i,1,k)=vto(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  115 continue
      do 116 k=1,nt
      do 116 np1=3,ndo1,2
      do 116 i=1,imid
      vte(i,1,k)=vte(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
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
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  123 continue
      if(mlat .eq. 0) go to 124
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv)
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  124 continue
  125 continue
  126 if(mp2 .gt. ndo2) go to 130
      do 129 k=1,nt
      do 128 np1=mp2,ndo2,2
      do 127 i=1,imm1
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  127 continue
      if(mlat .eq. 0) go to 128
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw)
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
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
      do 215 i=1,imm1
      wto(i,1,k)=wto(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  215 continue
      do 216 k=1,nt
      do 216 np1=3,ndo1,2
      do 216 i=1,imid
      wte(i,1,k)=wte(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
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
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  223 continue
      if(mlat .eq. 0) go to 224
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv) 
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  224 continue
  225 continue
  226 if(mp2 .gt. ndo2) go to 230
      do 229 k=1,nt
      do 228 np1=mp2,ndo2,2
      do 227 i=1,imm1
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  227 continue
      if(mlat .eq. 0) go to 228
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw) 
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  228 continue
  229 continue
  230 continue
      go to 950
c
c     case ityp=3   v odd,  w even
c
  300 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 315 k=1,nt
      do 315 np1=2,ndo2,2
      do 315 i=1,imm1
      vto(i,1,k)=vto(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
  315 continue
      do 316 k=1,nt
      do 316 np1=3,ndo1,2
      do 316 i=1,imid
      wte(i,1,k)=wte(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
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
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  323 continue
      if(mlat .eq. 0) go to 324
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv) 
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  324 continue
  325 continue
  326 if(mp2 .gt. ndo2) go to 330
      do 329 k=1,nt
      do 328 np1=mp2,ndo2,2
      do 327 i=1,imm1
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  327 continue
      if(mlat .eq. 0) go to 328
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw)
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
  328 continue
  329 continue
  330 continue
      go to 950
c
c     case ityp=4   v odd,  w even, and both cr and ci equal zero 
c
  400 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 415 k=1,nt
      do 415 np1=2,ndo2,2
      do 415 i=1,imm1
      vto(i,1,k)=vto(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
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
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  427 continue
      if(mlat .eq. 0) go to 428
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -bi(mp1,np1,k)*wb(imid,np1,iw)
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     +br(mp1,np1,k)*wb(imid,np1,iw)
  428 continue
  429 continue
  430 continue
      go to 950
c
c     case ityp=5   v odd,  w even,     br and bi equal zero 
c
  500 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 516 k=1,nt
      do 516 np1=3,ndo1,2
      do 516 i=1,imid
      wte(i,1,k)=wte(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
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
      vto(i,2*mp1-2,k) = vto(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vto(i,2*mp1-1,k) = vto(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wte(i,2*mp1-2,k) = wte(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wte(i,2*mp1-1,k) = wte(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  523 continue
      if(mlat .eq. 0) go to 524
      wte(imid,2*mp1-2,k) = wte(imid,2*mp1-2,k)
     1                     -cr(mp1,np1,k)*vb(imid,np1,iv) 
      wte(imid,2*mp1-1,k) = wte(imid,2*mp1-1,k)
     1                     -ci(mp1,np1,k)*vb(imid,np1,iv)
  524 continue
  525 continue
  530 continue
      go to 950
c
c     case ityp=6   v even  ,  w odd
c
  600 call vbin(0,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 615 k=1,nt
      do 615 np1=2,ndo2,2
      do 615 i=1,imm1
      wto(i,1,k)=wto(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
  615 continue
      do 616 k=1,nt
      do 616 np1=3,ndo1,2
      do 616 i=1,imid
      vte(i,1,k)=vte(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
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
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  623 continue
      if(mlat .eq. 0) go to 624
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv)
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  624 continue
  625 continue
  626 if(mp2 .gt. ndo2) go to 630
      do 629 k=1,nt
      do 628 np1=mp2,ndo2,2
      do 627 i=1,imm1
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  627 continue
      if(mlat .eq. 0) go to 628
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw) 
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  628 continue
  629 continue
  630 continue
      go to 950
c
c     case ityp=7   v even, w odd   cr and ci equal zero
c
  700 call vbin(2,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 716 k=1,nt
      do 716 np1=3,ndo1,2
      do 716 i=1,imid
      vte(i,1,k)=vte(i,1,k)+br(1,np1,k)*vb(i,np1,iv)
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
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)+br(mp1,np1,k)*vb(i,np1,iv)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+bi(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-bi(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)+br(mp1,np1,k)*wb(i,np1,iw)
  723 continue
      if(mlat .eq. 0) go to 724
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     +br(mp1,np1,k)*vb(imid,np1,iv)
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +bi(mp1,np1,k)*vb(imid,np1,iv)
  724 continue
  725 continue
  730 continue
      go to 950
c
c     case ityp=8   v even,  w odd   br and bi equal zero
c
  800 call vbin(1,nlat,nlon,0,vb,iv,wvbin)
c
c     case m = 0
c
      do 815 k=1,nt
      do 815 np1=2,ndo2,2
      do 815 i=1,imm1
      wto(i,1,k)=wto(i,1,k)-cr(1,np1,k)*vb(i,np1,iv)
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
      vte(i,2*mp1-2,k) = vte(i,2*mp1-2,k)-ci(mp1,np1,k)*wb(i,np1,iw)
      vte(i,2*mp1-1,k) = vte(i,2*mp1-1,k)+cr(mp1,np1,k)*wb(i,np1,iw)
      wto(i,2*mp1-2,k) = wto(i,2*mp1-2,k)-cr(mp1,np1,k)*vb(i,np1,iv)
      wto(i,2*mp1-1,k) = wto(i,2*mp1-1,k)-ci(mp1,np1,k)*vb(i,np1,iv)
  827 continue
      if(mlat .eq. 0) go to 828
      vte(imid,2*mp1-2,k) = vte(imid,2*mp1-2,k)
     1                     -ci(mp1,np1,k)*wb(imid,np1,iw) 
      vte(imid,2*mp1-1,k) = vte(imid,2*mp1-1,k)
     1                     +cr(mp1,np1,k)*wb(imid,np1,iw)
  828 continue
  829 continue
  830 continue
  950 do 14 k=1,nt
      call hrfftb(idv,nlon,vte(1,1,k),idv,wrfft,vb)
      call hrfftb(idv,nlon,wte(1,1,k),idv,wrfft,vb)
   14 continue
      if(ityp .gt. 2) go to 12
      do 60 k=1,nt
      do 60 j=1,nlon
      do 60 i=1,imm1
      vt(i,j,k) = .5*(vte(i,j,k)+vto(i,j,k))
      wt(i,j,k) = .5*(wte(i,j,k)+wto(i,j,k))
      vt(nlp1-i,j,k) = .5*(vte(i,j,k)-vto(i,j,k))
      wt(nlp1-i,j,k) = .5*(wte(i,j,k)-wto(i,j,k))
   60 continue
      go to 13
   12 do 11 k=1,nt
      do 11 j=1,nlon
      do 11 i=1,imm1
      vt(i,j,k) = .5*vte(i,j,k)
      wt(i,j,k) = .5*wte(i,j,k)
   11 continue
   13 if(mlat .eq. 0) return
      do 65 k=1,nt
      do 65 j=1,nlon
      vt(imid,j,k) = .5*vte(imid,j,k)
      wt(imid,j,k) = .5*wte(imid,j,k)
   65 continue
      return
      end
      subroutine vtseci(nlat,nlon,wvts,lwvts,dwork,ldwork,ierror)
c
      dimension wvts(lwvts)
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
      if(lwvts .lt. 2*(lzz1+labc)+nlon+15) return
      ierror = 4
      if (ldwork .lt. 2*nlat+2) return
      ierror = 0
      call vtinit (nlat,nlon,wvts,dwork)
      lwvbin = lzz1+labc
      iw1 = lwvbin+1
      call wtinit (nlat,nlon,wvts(iw1),dwork)
      iw2 = iw1+lwvbin
      call hrffti(nlon,wvts(iw2))
      return
      end
