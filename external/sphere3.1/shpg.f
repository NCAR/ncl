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
c                           August 2003
c
c ... in file shpg.f
c
c     this file contains code and documentation for subroutines
c     shpgi and shpg.
c
c ... files which must be loaded with shpg.f
c
c     hrfft.f
c
c     shpgi initializes the arrays wshp and iwshp for subsequent 
c     use in subroutine shpg, which performs the harmonic projection 
c     which is equivalent to a harmonic analysis followed by 
c     harmonic synthesis but faster and with less memory.
c     (see description of subroutine shpg below).
c
c     subroutine shpgi(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
c    1 liwshp,work,lwork,ierror)
c
c     shpgi initializes arrays wshp and iwshp for repeated use
c     by subroutine shpg ....
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c            nlon must be at least 4.
c
c     isym   currently not used, no equatorial symmetries assumed,
c            only whole sphere computations.    
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpgi. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpgi. It must be at least
c            4*(nlat+1).
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpgi. It must be at least
c            1.25*(nlat+1)**2+7*nlat+8.
c
c     **************************************************************
c
c     output parameters
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpg.        
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpg.        
c
c     work   a double precision work array that does 
c            not have to be saved.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of mtrunc
c            = 5  error in the specification of lwshp
c            = 6  error in the specification of liwshp
c            = 7  error in the specification of lwork
c
      subroutine shpgi(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
     1 liwshp,work,lwork,ierror)
      double precision work(*)
      dimension wshp(*),iwshp(*)
c
      ierror = 1
      if(nlat.lt.1) return
      ierror = 2
      if(nlon.lt.1) return
c      ierror = 3
c      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      mmax = min(nlat-1,nlon/2)
      if(mtrunc.lt.0 .or. mtrunc.gt.mmax) return
      ierror = 5
      lw1 = 2*(nlat+1)**2
      log2n = log(float(nlon))/log(2.0)
      if(lwshp.lt.lw1+nlon+log2n) return
      ierror = 6
      if(liwshp.lt.4*(nlat+1)) return
      ierror = 7
      mlwk = 1.25*(nlat+1)**2+7*nlat+8
      if(lwork.lt.mlwk) return
      ierror = 0
c
      call hrffti(nlon,wshp(lw1+1))
c
      nte = (nlat+1)/2
      nloc1 = 2*nte*nte
      nloc2 = nlat+1
      iw1 = 1
      iw2 = iw1+nloc1
      iw3 = iw2+nloc1
      iw4 = iw3+nloc1
      jw1 = 1
      jw2 = jw1+nloc2
      jw3 = jw2+nloc2
      jw4 = jw3+nloc2
      kw1 = 1
      kw2 = kw1+nte
      kw3 = kw2+nte
      kw4 = kw3+2*nte
      kw5 = kw4+2*nte
      kw6 = kw5+nte
      kw7 = kw6+nte
      kw8 = kw7+4*nte
      kw9 = kw8+2*nte
      kw10 = kw9+nloc1
      kw11 = kw10+nloc1
      ktot = kw11+nte*nte
c
      call shpgi1(nlat,nlon,isym,mtrunc,nte,ierror,wshp(iw1),wshp(iw2),
     1  wshp(iw3),wshp(iw4),iwshp(jw1),iwshp(jw2),iwshp(jw3),
     2  iwshp(jw4),work(kw1),work(kw2),work(kw3),work(kw4),work(kw5),
     3  work(kw6),work(kw7),work(kw8),work(kw9),work(kw10),work(kw11))
      return
      end
      subroutine shpgi1(nlat,nlon,isym,mtrunc,idp,ierror,
     1  pe,po,ze,zo,ipse,jzse,ipso,jzso,
     2  cp,wx,thet,gwts,xx,z,a,b,ped,pod,u)
c
      double precision sum,eps,a1,b1,c1,work
      parameter (eps=5.0d-8)
      double precision cp(idp),wx(idp),
     1  thet(nlat),gwts(nlat),xx(idp),z(idp),a(4*idp),
     2  b(2*idp),ped(idp,idp,2),pod(idp,idp,2),u(idp,idp)
c
      dimension pe(idp,idp,2),po(idp,idp,2),ze(idp,idp,2),
     1  zo(idp,idp,2),
     2  ipse(idp,2),jzse(idp,2),ipso(idp,2),jzso(idp,2),
     3  nshe(2),nsho(2)
      dimension zort(64,64,2)
c
      ns2 = nlat/2
      modn = nlat-ns2-ns2
      nte = (nlat+1)/2
      nto = nlat-nte
      tusl = 0.
      toe = 0.
c
c     compute gauss grid distribution
c
      lwork = nlat+1
      call gaqdp(nlat,thet,gwts,work,lwork,ierr)
      if(ierr .ne. 0) write(*,160) ierr
 160  format(' error in gaqd =',i5)
      do i=1,nto
      gwts(i) = gwts(i)+gwts(i)
      end do
c
c     compute n**2 basis (even functions)
c
      do n=1,nlat+nlat-2
      dfn = n
      a(n) = dsqrt(dfn*(dfn+1.0d0))
      end do
      do n=1,nlat-1
      dfn = n
      b(n) = dsqrt((dfn+dfn+3.0d0)/(dfn+dfn-1.0d0))
      end do
c
      mxtr = min(nlat-1,nlon/2,mtrunc)
      ip = 2
      do 200 mp1=1,mxtr+1
      m = mp1-1
      ip = 3-ip
      ms2 = mp1/2
      nem = (nlat-m+1)/2
      nec = nte-nem
c
c     compute associated legendre functions
c
      if(m.le.1) then
      do 205 j=1,nem
      n = j+j+m-2
      call dlfkg(m,n,cp)
      do i=1,nte
      call dlftg (m,n,thet(i),cp,ped(i,j+nec,ip))
      end do
 205  continue
c
      else
c
      do 207 j=1,nem
      n = j+j+m-2
      if(m.gt.1.and.n.gt.mxtr) then
      do i=1,nte
      u(i,j+nec) = ped(i,j+nec,ip)
      end do
      go to 207
      end if
      a1 = b(n-1)*a(n+m-3)/a(n+m-1)
      b1 = a(n-m+1)/a(n+m-1)
      if(n-m.le.1) then
      do i=1,nte
      u(i,j+nec) = a1*ped(i,j+nec-1,ip)   
     1                   - b1*ped(i,j+nec,ip)    
      end do
      else
      c1 = b(n-1)*a(n-m-1)/a(n+m-1)
      do i=1,nte
      u(i,j+nec) = a1*ped(i,j+nec-1,ip)   
     1   - b1*ped(i,j+nec,ip) + c1*u(i,j+nec-1)    
      end do
      end if
 207  continue
      do j=1,nem
      do i=1,nte
      ped(i,j+nec,ip) = u(i,j+nec)
      end do
      end do
      end if
      if(nec.le.0) go to 200
c
c     generate orthogonal vector
c
      do i=1,nte
      xx(i) = rand(0)
      end do
c
      it = 0
 201  do i=1,nte
      z(i) = 0.0d0
      wx(i) = gwts(i)*xx(i)
      end do
      do 220 j=1,nte
      if(j.eq.nec) go to 220
      call gs(nte,wx,ped(1,j,ip),z)
 220  continue
c  
      do i=1,nte
      xx(i) = xx(i)-z(i)
      end do
      call normal(nte,xx,idp,gwts)
      it = it+1
      if(it.le.2) go to 201
      do i=1,nte
      ped(i,nec,ip) = xx(i)
      end do
 200  continue
c
c     reorder if mtrunc is less than nlat-1 
c         case of even functions
c
      nmx = nlat-mxtr
      if(modn.eq.1) then
      nshe(1) = nmx/2
      nshe(2) = (nmx-1)/2
      else
      nshe(1) = (nmx-1)/2
      nshe(2) = nmx/2
      end if
c
      do 210 mp1=1,2
      do j=1,nte
      js = j+nshe(mp1)
      if(js.gt.nte) js = js-nte
      do i=1,nte
      u(i,js) = ped(i,j,mp1)
      end do
      end do
      do j=1,nte
      do i=1,nte
      ped(i,j,mp1) = u(i,j)
      end do
      end do
 210  continue
c
      call trunc(0,nte,idp,ped(1,1,1),nte,ipse(1,1))
      call trunc(0,nte,idp,ped(1,1,2),nte,ipse(1,2))
c
c     compute the analysis matrices
c
      do 250 ip=1,2
      do i=1,nte
      lock = 0
      do j=1,nte
      sum = ped(j,i,ip)*gwts(j)
      ze(j,i,ip) =  sum
      pe(i,j,ip) = ped(i,j,ip)
      if(dabs(sum).gt.eps .and. lock.eq.0) then
      lock = 1
      jzse(i,ip) = j
      end if
      end do
      end do
 250  continue
c
c     check orthogonality of pe(i,j,mp1)  mp1=1,2
c
      do ip=1,2
      dmax = 0.
      do i=1,nte
      do j=1,nte
      sum1 = 0.
      do k=1,nte
      sum1 = sum1+ze(k,i,ip)*pe(k,j,ip)
      end do
      zo(i,j,ip) = sum1
      if(i.ne.j) then
      dmax = max(dmax,abs(sum1))
      else
      dmax = max(dmax,abs(sum1-1.0))
      end if
      end do
      end do
      end do
c
c     compute n**2 basis (odd functions)
c
      ip = 2
      do 300 mp1=1,mxtr+1
      ip = 3-ip
      m = mp1-1
      ms2 = mp1/2
      nem = (nlat-m+1)/2
      nom = nlat-m-nem 
      noc = nto-nom
c
c     compute associated legendre functions
c
      if(m.le.1) then
      do 305 j=1,nom
      n = j+j+m-1
      call dlfkg(m,n,cp)
      do i=1,nte
      call dlftg (m,n,thet(i),cp,pod(i,j+noc,ip))
      end do
      if(modn.gt.0) pod(nte,j+noc,ip) = 0.0d0
 305  continue
c
      else
c
      do 307 j=1,nom
      n = j+j+m-1
      if(m.gt.1.and.n.gt.mxtr) then
      do i=1,nte
      u(i,j+noc) = pod(i,j+noc,ip)
      end do
      go to 304
      end if
      a1 = b(n-1)*a(n+m-3)/a(n+m-1)
      b1 = a(n-m+1)/a(n+m-1)
      if(n-m.le.1) then
      do i=1,nte
      u(i,j+noc) = a1*pod(i,j+noc-1,ip)   
     1                   - b1*pod(i,j+noc,ip)    
      end do
      else
      c1 = b(n-1)*a(n-m-1)/a(n+m-1)
      do i=1,nte
      u(i,j+noc) = a1*pod(i,j+noc-1,ip)   
     1   - b1*pod(i,j+noc,ip) + c1*u(i,j+noc-1)    
      end do
      end if
 304  if(modn.eq.1) u(nte,j+noc) = 0.0d0
 307  continue
      do j=1,nom
      do i=1,nte
      pod(i,j+noc,ip) = u(i,j+noc)
      end do
      end do
      end if
c
      if(noc.le.0) go to 300
      do i=1,nte
      xx(i) = rand(0)
      end do
      if(modn.eq.1) xx(nte) = 0.0d0
      it = 0
 306  do i=1,nte
      z(i) = 0.
      wx(i) = gwts(i)*xx(i)
      end do
      do 330 j=1,nto
      if(j.eq.noc) go to 330
      call gs(nte,wx,pod(1,j,ip),z(1))
 330  continue
c  
      do i=1,nte
      xx(i) = xx(i)-z(i)
      end do
      call normal(nte,xx,idp,gwts)
      it = it+1
      if(it.le.2) go to 306
      do i=1,nte
      pod(i,noc,ip) = xx(i)
      end do
      if(modn.eq.1) pod(nte,noc,ip) = 0.0d0
 300  continue
c
      nmx = nlat-mxtr
      if(modn.eq.1) then
      nsho(1) = (nmx-1)/2
      nsho(2) = nmx/2
      else
      nsho(1) = nmx/2
      nsho(2) = (nmx-1)/2
      end if
c
      do 310 mp1=1,2
      do j=1,nto
      js = j+nsho(mp1)
      if(js.gt.nto) js = js-nto
      do i=1,nte
      u(i,js) = pod(i,j,mp1)
      end do
      end do
      do j=1,nto
      do i=1,nte
      pod(i,j,mp1) = u(i,j)
      end do
      end do
 310  continue
c
      call trunc(0,nte,idp,pod(1,1,1),nto,ipso(1,1))
      call trunc(0,nte,idp,pod(1,1,2),nto,ipso(1,2))
c
c     compute the analysis matrices (odd functions)
c
      do ip=1,2
      do i=1,nto
      lock = 0
      do j=1,nto
      sum = pod(j,i,ip)*gwts(j)
      zo(j,i,ip) = sum
      po(i,j,ip) = pod(i,j,ip)
      if(dabs(sum).gt.eps .and. lock.eq.0) then
      lock = 1
      jzso(i,ip) = j
      end if
      end do
      end do
      end do
c
c     check orthogonality of po(i,j,mp1)  mp1=1,2
c
      do ip=1,2
      dmax = 0.
      do i=1,nto
      do j=1,nto
      sum1 = 0.
      do k=1,nto
      sum1 = sum1+zo(k,i,ip)*po(k,j,ip)
      end do
      zort(i,j,ip) = sum1
      if(i.ne.j) then
      dmax = max(dmax,abs(sum1))
      else
      dmax = max(dmax,abs(sum1-1.0))
      end if
      end do
      end do
      end do
      return
      end
c
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
c ... file shpg.f
c
c ... files which must be loaded with shpg.f
c
c     hrfft.f
c
c     shpg computes the harmonic projection, which is
c     equivalent to a harmonic analysis (forward) followed
c     by a harmonic synthesis (backward transform).
c     shpg uses the n**2 projection or complement when appropriate
c     as well as  odd/even factorization and zero truncation on an
c     on a Gaussian distributed grid as defined in the JCP paper
c     "Generalized discrete spherical harmonic transforms" 
c     by Paul N. Swarztrauber and William F. Spotz
c     J. Comp. Phys., 159(2000) pp. 213-230.
c
c     subroutine shpg(nlat,nlon,isym,mtrunc,x,y,idxy,
c    1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
c     shpg projects the array x onto the set of functions represented
c     by a discrete set of spherical harmonics.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3.
c 
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c            nlon must be at least 4. 
c
c     isym   currently not used.    
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.

c            zero.
c
c     x      a two dimensional array that contains the the nlat
c            by nlon array x(i,j) defined at the colatitude point 
c            theta(i) = (i-1)*pi/(nlat-1) and longitude point phi(j) =
c            (j-1)*2*pi/nlon.
c
c     idxy   the first dimension of the arrays x and y as they
c            appear in the program that calls shpg. It must be
c            at least nlat. 
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpg.        
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpgi. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpg.        
c
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpgi. It must be at least
c            4*(nlat+1).
c
c     work   a single precision work array that does 
c            not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpg. It must be at least
c            max(nlat*nlon,4*(nlat+1)).
c
c     **************************************************************
c
c     output parameters
c
c     y      an nlat by nlon single precision array that contains 
c            the projection of x onto the set of functions that
c            can be represented by the discrete set of spherical 
c            harmonics. The arrays x(i,j) and y(i,j) are located
c            at colatitude point theta(i) = (i-1)*pi/(nlat-1) and 
c            longitude point phi(j) = (j-1)*2*pi/nlon.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of mtrunc
c            = 5  error in the specification of lwshp
c            = 6  error in the specification of liwshp
c            = 7  error in the specification of lwork
c
      subroutine shpg(nlat,nlon,isym,mtrunc,x,y,idxy,
     1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
      dimension wshp(*),iwshp(*),work(*),x(idxy,nlon),y(idxy,nlon)
c
      ierror = 1
      if(nlat.lt.1) return
      ierror = 2
      if(nlon.lt.1) return
c      ierror = 3
c      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      mmax = min(nlat-1,nlon/2)
      if(mtrunc.lt.0 .or. mtrunc.gt.mmax) return
      ierror = 5
      log2n = log(float(nlon))/log(2.0)
      lw1 = 2*(nlat+1)**2
      if(lwshp.lt.lw1+nlon+log2n) return
      ierror = 6
      if(liwshp.lt.4*(nlat+1)) return
      ierror = 7
      mwrk = max(nlat*nlon,4*(nlat+1))
      if(lwork.lt.mwrk) return
      ierror = 0
c
      do j=1,nlon
       do i=1,nlat
        y(i,j) = x(i,j)
       end do
      end do
      call hrfftf(nlat,nlon,y,idxy,wshp(lw1+1),work)
c
      nte = (nlat+1)/2
      nloc1 = 2*nte*nte
      nloc2 = nlat+1
      iw1 = 1
      iw2 = iw1+nloc1
      iw3 = iw2+nloc1
      iw4 = iw3+nloc1
      jw1 = 1
      jw2 = jw1+nloc2
      jw3 = jw2+nloc2
      jw4 = jw3+nloc2
c
      call shpg1(nlat,nlon,isym,mtrunc,y,y,idxy,ierror,
     1 nte,wshp(iw1),wshp(iw2),wshp(iw3),wshp(iw4),iwshp(jw1),
     2 iwshp(jw2),iwshp(jw3),iwshp(jw4),work(jw1),
     3 work(jw2),work(jw3),work(jw4))
c
      call hrfftb(nlat,nlon,y,idxy,wshp(lw1+1),work)
c
      sn = 1.0/nlon
      do j=1,nlon
       do i=1,nlat
        y(i,j) = sn*y(i,j)
       end do
      end do
      return
      end
      subroutine shpg1(nlat,nlon,isym,mtrunc,sx,sy,idxy,ierror,
     1 idp,pe,po,ze,zo,ipse,jzse,ipso,jzso,xe,xo,ye,yo)
c
      dimension sx(idxy,nlon),sy(idxy,nlon),nshe(2),nsho(2),
     1  pe(idp,idp,2),po(idp,idp,2),ze(idp,idp,2),zo(idp,idp,2),
     2  ipse(idp,2),jzse(idp,2),ipso(idp,2),jzso(idp,2),
     3  xe(idp,2),xo(idp,2),ye(idp,2),yo(idp,2)
c
      ns2 = nlat/2
      modn = nlat-ns2-ns2
      nte = (nlat+1)/2
      nto = nlat-nte
c
      mxtr = min(nlat-1,nlon/2,mtrunc)
      nmx = nlat-mxtr
      if(modn.eq.1) then
      nshe(1) = nmx/2
      nshe(2) = (nmx-1)/2
      nsho(1) = (nmx-1)/2
      nsho(2) = nmx/2
      else
      nshe(1) = (nmx-1)/2
      nshe(2) = nmx/2
      nsho(1) = nmx/2
      nsho(2) = (nmx-1)/2
      end if
c
      ip = 2 
      do 100 mp1=1,mxtr+1
      ip = 3-ip
      if(mxtr.eq.nlat-1.and.mp1.eq.1) then
      do i=1,nlat
      sy(i,mp1) = sx(i,mp1)
      end do
c      if(mp1.eq.2) then
c      sy(1,2) = 0.
c      sy(nlat,2) = 0.
c      end if
c      if(nlon.ge.3) then
c      sy(1,3) = 0.
c      sy(nlat,3) = 0.
c      do i=2,nlat-1
c      sy(i,3) = sx(i,3)
c      end do
c      end if
      go to 100
      end if
      m = mp1-1
      mpm = max(1,m+m)
      ms2 = mp1/2
c      mrank = min(nlat-m,nlat-ms2-ms2)   
c      nrank = nlat-mrank
c      nem = (mrank+1)/2-nshe(ip)
c      nom = mrank-(mrank+1)/2-nsho(ip)
      nem = (nlat-m+1)/2-nshe(ip)
      nom = (nlat-m)/2-nsho(ip)
      nec = nte-nem
      noc = nto-nom
      do i=1,nte
      xe(i,1) = .5*(sx(i,mpm)+sx(nlat+1-i,mpm))
      xo(i,1) = .5*(sx(i,mpm)-sx(nlat+1-i,mpm))
      end do
c      if(modn.eq.1) then
c      xe(nte,1) = sx(nte,mpm)
c      xo(nte,1) = 0.
c      end if
      if(mpm.lt.nlon) then
      do i=1,nte
      xe(i,2) = .5*(sx(i,mpm+1)+sx(nlat+1-i,mpm+1))
      xo(i,2) = .5*(sx(i,mpm+1)-sx(nlat+1-i,mpm+1))
      end do
c      if(modn.eq.1) then
c      xe(nte,2) = sx(nte,mpm+1)
c      xo(nte,2) = 0.
c      end if
      end if
      lag = 0
      if(m.eq.0.or.mpm.eq.nlon) lag = 1
      if(3*nec.lt.2*nem.or.nem.eq.0) then
      call tmxmx(lag,nte,nec,idp,pe(1,1,ip),nte,idp,
     1          ze(1,1,ip),xe,ye,ipse(1,ip),jzse(1,ip))  
      do i=1,nte
      ye(i,1) = xe(i,1)-ye(i,1)
      end do
      if(mpm.lt.nlon.and.m.ne.0) then
      do i=1,nte
      ye(i,2) = xe(i,2)-ye(i,2)
      end do
      end if
      else
      call tmxmx(lag,nte,nem,idp,pe(1,nec+1,ip),nte,idp,
     1ze(1,nec+1,ip),xe,ye,ipse(nec+1,ip),jzse(nec+1,ip))
      end if
      if(3*noc.lt.2*nom.or.nom.eq.0) then
      call tmxmx(lag,nto,noc,idp,po(1,1,ip),nto,idp,
     1          zo(1,1,ip),xo,yo,ipso(1,ip),jzso(1,ip))
      do i=1,nto
      yo(i,1) = xo(i,1)-yo(i,1)
      end do
      if(mpm.lt.nlon.and.m.ne.0) then
      do i=1,nto
      yo(i,2) = xo(i,2)-yo(i,2)
      end do
      end if
      else
      call tmxmx(lag,nto,nom,idp,po(1,noc+1,ip),nto,idp,
     1zo(1,noc+1,ip),xo,yo,ipso(noc+1,ip),jzso(noc+1,ip))  
      end if
      do i=1,nto
      sy(i,mpm) = ye(i,1)+yo(i,1)
      sy(nlat+1-i,mpm) = ye(i,1)-yo(i,1)
      end do
      if(nte.gt.nto) sy(nte,mpm) = ye(nte,1)
      if(mpm.lt.nlon.and.m.ne.0) then
      do i=1,nto
      sy(i,mpm+1) = ye(i,2)+yo(i,2)
      sy(nlat+1-i,mpm+1) = ye(i,2)-yo(i,2)
      end do 
      if(nte.gt.nto) sy(nte,mpm+1) = ye(nte,2)
      end if
 100  continue
c
      js = mxtr+mxtr+2
      do j=js,nlon
      do i=1,nlat
      sy(i,j) = 0.
      end do
      end do
      return
      end
      subroutine mxm(lr,lc,ld,a,mc,md,b,nd,c)
      double precision a(ld,*),b(md,*),c(nd,*)
      do i=1,lr
      do j=1,mc
      c(i,j) = 0.
      do k=1,lc 
      c(i,j) = c(i,j)+a(i,k)*b(k,j)
      end do
      end do
      end do
      return
      end
      subroutine smxm(lr,lc,ld,a,mc,md,b,nd,c)
      dimension a(ld,*),b(md,*),c(nd,*)
      do i=1,lr
      do j=1,mc
      c(i,j) = 0.
      do k=1,lc 
      c(i,j) = c(i,j)+a(i,k)*b(k,j)
      end do
      end do
      end do
      return
      end
      subroutine mxmx(lr,lc,ld,a,mc,md,b,x,y)
      dimension a(ld,*),b(md,*),x(ld,2),y(ld,2)
      do k=1,lr
      y(k,1) = 0.
      y(k,2) = 0.
      end do
c
      if(lc.le.0) return
      do i=1,lc
      sum1 = 0.
      sum2 = 0.
      do j=1,mc
      sum1 = sum1 + b(i,j)*x(j,1)
      sum2 = sum2 + b(i,j)*x(j,2)
      end do
      do k=1,lr
      y(k,1) = y(k,1)+sum1*a(k,i)
      y(k,2) = y(k,2)+sum2*a(k,i)
      end do
      end do
      return
      end
      subroutine dmxmx(lr,lc,ld,a,mc,md,b,x,y)
      double precision a(ld,*),b(md,*),x(ld,2),y(ld,2),
     1                 sum1,sum2
      do k=1,lr
      y(k,1) = 0.
      y(k,2) = 0.
      end do
c
      if(lc.le.0) return
      do i=1,lc
      sum1 = 0.
      sum2 = 0.
      do j=1,mc
      sum1 = sum1 + b(i,j)*x(j,1)
      sum2 = sum2 + b(i,j)*x(j,2)
      end do
      do k=1,lr
      y(k,1) = y(k,1)+sum1*a(k,i)
      y(k,2) = y(k,2)+sum2*a(k,i)
      end do
      end do
      return
      end
      subroutine tmxmx(lag,lr,lc,ld,a,mc,md,b,x,y,is,js)
      dimension a(ld,*),b(md,*),x(ld,2),y(ld,2),
     1              is(*),js(*)
c
      kmx = min(lr+1,ld)
      if(lag.eq.1) then
      do k=1,kmx
      y(k,1) = 0.
      end do
c      if(lc.eq.0) then
c      do k=1,lr
c      y(k,1) = x(k,1)
c      end do
c      return
c      end if
      if(lc.le.0) return
      do i=1,lc
      sum1 = 0.
      do j=js(i),mc
      sum1 = sum1 + b(j,i)*x(j,1)
      end do
      do k=is(i),lr
      y(k,1) = y(k,1)+sum1*a(k,i)
      end do
      end do
      return
      end if
      do k=1,kmx
      y(k,1) = 0.
      y(k,2) = 0.
      end do
      if(lc.le.0) return
c
      do i=1,lc
      sum1 = 0.
      sum2 = 0.
      do j=js(i),mc
      sum1 = sum1 + b(j,i)*x(j,1)
      sum2 = sum2 + b(j,i)*x(j,2)
      end do
      do k=is(i),lr
      y(k,1) = y(k,1)+sum1*a(k,i)
      y(k,2) = y(k,2)+sum2*a(k,i)
      end do
      end do
      return
      end
      subroutine trunc(irc,n,idp,a,nrc,ijs)
      double precision a,eps
      parameter (eps=5.d-8)
      dimension a(idp,*),ijs(n)
c
c     irc = 0 for columns , or irc = 1 for rows
c
      if(irc.ne.0) go to 30
      do 20 j=1,nrc
      do i=1,n
      ijs(j) = i
      if(dabs(a(i,j)) .gt. eps) go to 20
      end do
 20   continue
      return
 30   do 50 i=1,nrc
      do j=1,n
      ijs(i) = j
      if(abs(a(i,j)) .gt. eps) go to 50
      end do
 50   continue
      return
      end
      subroutine gs(n,x,y,z)
      dimension x(n),y(n),z(n)
      double precision x,y,z,sum
c
c     accumulate innerproducts of x with respect to y.
c
      sum = 0.
      do i=1,n
      sum = sum+x(i)*y(i)
      end do
      do i=1,n
      z(i) = z(i)+sum*y(i)
      end do
      return
      end
      subroutine normal(n,x,id,q)
      dimension x(n),q(n)
      double precision x,q,sqs
c
c     normalize x
c
      sqs = 0.
      do i=1,n
c      sum = 0.
c      do j=1,n
c      sum = sum+q(i,j)*x(j)
c      end do
c      sqs = sqs+sum*x(i)
      sqs = sqs+q(i)*x(i)*x(i)
      end do
c
      if(sqs .ne. 0) go to 4
      write(*,3)
 3    format(' norm of z is zero in subroutine normal')
      return
 4    sqs = dsqrt(sqs)
      do i=1,n
      x(i) = x(i)/sqs
      end do
      return
      end
      subroutine coe(moe,n,x,dmax)
      double precision x(n),dmax
      nh = (n+1)/2
      dmax = 0.
      if(moe.ne.0) go to 1
      do i=1,nh
      dmax = max(dmax,dabs(x(i)-x(n-i+1)))
      x(i) = .5*(x(i)+x(n-i+1))
      x(n-i+1) = x(i)
      end do
      return
 1    do i=1,nh
      dmax = max(dmax,dabs(x(i)+x(n-i+1)))
      x(i) = .5*(x(i)-x(n-i+1))
      x(n-i+1) = -x(i)
      end do
      if(mod(n,2).ne.0) x(nh) = 0.
      return
      end
c     subroutine dlfkg(m,n,cp)
c
c     subroutine dlfkg computes the coefficients in the trigonometric
c     expansion of the normalized associated legendre functions:
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                        *sin(theta)**m/(2**n*factorial(n)) times the
c                        (n+m)th derivative of (x**2-1)**n with respect
c                        to x=cos(theta)
c
c     where theta is colatitude.
c
c     subroutine dlfkg computes the coefficients cp(k) in the
c     following trigonometric expansion of pbar(m,n,theta).
c
c            1) for n even and m even, pbar(m,n,theta) =
c               .5*cp(1) plus the sum from k=1 to k=n/2
c               of cp(k)*cos(2*k*th)
c
c            2) for n even and m odd, pbar(m,n,theta) =
c               the sum from k=1 to k=n/2 of
c               cp(k)*sin(2*k*th)
c
c            3) for n odd and m even, pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*cos((2*k-1)*th)
c
c            4) for n odd and m odd,  pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*sin((2*k-1)*th)
c
c     input parameters
c
c     m      is the order of pbar(n,m,theta). m can be any integer
c            however pbar(n,m,theta) = 0  if abs(m) is greater than
c            n and pbar(n,m,theta) = (-1)**m*pbar(n,-m,theta) for
c            negative m.
c
c     n      nonnegative integer specifying the degree of
c            pbar(n,m,theta)
c
c     output parameters
c
c     cp     a double precision array that contains the fourier
c            coefficients for pbar(m,n,theta). the length of the
c            array depends on the parity of m and n
c
c                  parity            length of cp
c
c               n even m even           n/2+1
c               n even m odd             n/2
c               n odd  m even          (n+1)/2
c               n odd  m odd           (n+1)/2
c
c
c ****************************************************************
      subroutine dlfkg (m,n,cp)
c
      double precision cp,fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,
     1       t1,t2,pm1,sc10,sc20,sc40
      dimension       cp(1)
      parameter (sc10=1024.d0)
      parameter (sc20=sc10*sc10)
      parameter (sc40=sc20*sc20)
c
      cp(1) = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if(n-1) 2,3,5
    2 cp(1) = dsqrt(2.d0)
      return
    3 if(ma .ne. 0) go to 4
      cp(1) = dsqrt(1.5d0)
      return
    4 cp(1) = dsqrt(.75d0)
      if(m .eq. -1) cp(1) = -cp(1)
      return
    5 if(mod(n+ma,2) .ne. 0) go to 10
      nmms2 = (n-ma)/2
      fnum = n+ma+1
      fnmh = n-ma+1
      pm1 = 1.d0
      go to 15
   10 nmms2 = (n-ma-1)/2
      fnum = n+ma+2
      fnmh = n-ma+2
      pm1 = -1.d0
 15   t1 = 1.d0/sc20
      nex = 20
      fden = 2.d0
      if(nmms2 .lt. 1) go to 20
      do 18 i=1,nmms2
      t1 = fnum*t1/fden
      if(t1 .gt. sc20) then
      t1 = t1/sc40
      nex = nex+40
      end if
      fnum = fnum+2.
      fden = fden+2.
   18 continue
   20 t1 = t1/2.d0**(n-1-nex)
      if(mod(ma/2,2) .ne. 0) t1 = -t1
      t2 = 1. 
      if(ma .eq. 0) go to 26
      do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
   25 continue
   26 cp2 = t1*dsqrt((n+.5d0)*t2)
      fnnp1 = n*(n+1)
      fnmsq = fnnp1-2.d0*ma*ma
      l = (n+1)/2
      if(mod(n,2) .eq. 0 .and. mod(ma,2) .eq. 0) l = l+1
      cp(l) = cp2
      if(m .ge. 0) go to 29
      if(mod(ma,2) .ne. 0) cp(l) = -cp(l)
   29 if(l .le. 1) return
      fk = n
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = 2.*(fk*fk-fnmsq)
      cp(l-1) = b1*cp(l)/a1
   30 l = l-1
      if(l .le. 1) return
      fk = fk-2.
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = -2.*(fk*fk-fnmsq)
      c1 = (fk+1.)*(fk+2.)-fnnp1
      cp(l-1) = -(b1*cp(l)+c1*cp(l+1))/a1
      go to 30
      end
      subroutine dlftg (m,n,theta,cp,pb)
      dimension cp(1)
      double precision cp,pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(abs(m),2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = .5*cp(1)
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb+cp(k+1)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb+cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
c
      subroutine gaqdp(nlat,theta,wts,w,lwork,ierror)
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             April 2002
c                        
c     gauss points and weights are computed using the fourier-newton
c     described in "on computing the points and weights for 
c     gauss-legendre quadrature", paul n. swarztrauber, siam journal 
c     on scientific computing that has been accepted for publication.
c     This routine is faster and more accurate than older program 
c     with the same name.
c
c     subroutine gaqdp computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     w       unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a double precision array with length nlat
c             containing the gaussian colatitudes in
c             increasing radians on the interval (0,pi).
c
c     wts     a double precision array with lenght nlat
c             containing the gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      double precision theta(nlat),wts(nlat),
     1 x,pi,pis2,dtheta,dthalf,cmax,zprev,zlast,zero,
     2 zhold,pb,dpb,dcor,sum,w,cz
c
c     check work space length
c
      ierror = 1
      if (nlat.le.0) return
      ierror = 0
c
c     compute weights and points analytically when nlat=1,2
c
      if (nlat.eq.1) then
      theta(1) = dacos(0.0d0)
      wts(1) = 2.0d0
      return
      end if
      if (nlat.eq.2) then
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      wts(1) = 1.0d0
      wts(2) = 1.0d0
      return
      end if
      eps = sqrt(dzepp(1.0d0))
      eps = eps*sqrt(eps)
      pis2 = 2.0d0*datan(1.0d0)
      pi = pis2+pis2 
      mnlat = mod(nlat,2)
      ns2 = nlat/2
      nhalf = (nlat+1)/2
      idx = ns2+2
c
      call cpdp1 (nlat,cz,theta(ns2+1),wts(ns2+1))
c
      dtheta = pis2/nhalf
      dthalf = dtheta/2.0d0
      cmax = .2d0*dtheta
c
c     estimate first point next to theta = pi/2
c
      if(mnlat.ne.0) then
      zero = pis2-dtheta
      zprev = pis2
      nix = nhalf-1
      else
      zero = pis2-dthalf
      nix = nhalf
      end if
 9    it = 0
 10   it = it+1
      zlast = zero
c
c     newton iterations
c
      call tpdp1 (nlat,zero,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      dcor = pb/dpb
      sgnd = 1.0
      if(dcor .ne. 0.0d0) sgnd = dcor/dabs(dcor)
      dcor = sgnd*min(dabs(dcor),cmax)
      zero = zero-dcor
      if(dabs(zero-zlast).gt.eps*dabs(zero)) go to 10
      theta(nix) = zero
      zhold = zero
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c    
c     yakimiw's formula permits using old pb and dpb
c
      wts(nix) = (nlat+nlat+1)/(dpb+pb*dcos(zlast)/dsin(zlast))**2
      nix = nix-1
      if(nix.eq.0) go to 30
      if(nix.eq.nhalf-1)  zero = 3.0*zero-pi
      if(nix.lt.nhalf-1)  zero = zero+zero-zprev
      zprev = zhold
      go to 9
c
c     extend points and weights via symmetries
c
 30   if(mnlat.ne.0) then
      theta(nhalf) = pis2
      call tpdp1 (nlat,pis2,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      wts(nhalf) = (nlat+nlat+1)/(dpb*dpb)
      end if
      do i=1,ns2
      wts(nlat-i+1) = wts(i)
      theta(nlat-i+1) = pi-theta(i)
      end do
      sum = 0.0d0
      do i=1,nlat
      sum = sum+wts(i)
      end do
      do i=1,nlat
      wts(i) = 2.0d0*wts(i)/sum
      end do
      return
      end
      subroutine cpdp1(n,cz,cp,dcp)
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative. 
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant 
c     coefficient is returned in cz. 
c
      double precision cp(n/2+1),dcp(n/2+1),
     1 t1,t2,t3,t4,cz
      ncp = (n+1)/2
      t1 = -1.0d0
      t2 = n+1.0d0
      t3 = 0.0d0
      t4 = n+n+1.0d0
      if(mod(n,2).eq.0) then
      cp(ncp) = 1.0d0
      do j = ncp,2,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j-1) = (t1*t2)/(t3*t4)*cp(j)
      end do
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cz = (t1*t2)/(t3*t4)*cp(1)
      do j=1,ncp
      dcp(j) = (j+j)*cp(j)
      end do
      else
      cp(ncp) = 1.0d0
      do j = ncp-1,1,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j) = (t1*t2)/(t3*t4)*cp(j+1)
      end do
      do j=1,ncp
      dcp(j) = (j+j-1)*cp(j)
      end do
      end if
      return
      end
      subroutine tpdp1 (n,theta,cz,cp,dcp,pb,dpb)
c
c     computes pn(theta) and its derivative dpb(theta) with 
c     respect to theta
c
      double precision cp(n/2+1),dcp(n/2+1),cz,
     1  pb,dpb,fn,theta,cdt,sdt,cth,sth,chh
c
      fn = n
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      if(mod(n,2) .eq.0) then
c
c     n even
c
      kdo = n/2
      pb = .5d0*cz
      dpb = 0.0d0
      if(n .gt. 0) then
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c      pb = pb+cp(k)*cos(2*k*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k)*cp(k)*sin(2*k*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      end if
      else
c
c     n odd
c
      kdo = (n+1)/2
      pb = 0.0d0
      dpb = 0.0d0
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c      pb = pb+cp(k)*cos((2*k-1)*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k-1)*cp(k)*sin((2*k-1)*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      end if
      return
      end
      real function dzepp (x)
      double precision  x
c
c     estimate unit roundoff in quantities of size x.
c
      double precision a,b,c,eps
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to 
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying 
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      a = 4.0d0/3.0d0
   10 b = a - 1.0d0
      c = b + b + b
      eps = abs(c-1.0d0)
      if (eps .eq. 0.0d0) go to 10
      dzepp = eps*dabs(x)
      return
      end
