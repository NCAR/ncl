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
c                            August 2003
c
c ... file shpe.f
c
c     this file contains code and documentation for subroutines
c     shpei and shpe.
c
c ... files which must be loaded with shpe.f
c
c     hrfft.f
c
c     subroutine shpei initializes arrays wshp and iwshp for
c     subsequent repeated use by subroutine shpe, which
c     performs the harmonic projection equivalent to a
c     harmonic analysis followed by harmonic synthesis
c     but faster and with less memory. (see description of
c     subroutine shpe below)
c
c     subroutine shpei(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
c    1 liwshp,work,lwork,ierror)
c
c     shpei initializes arrays wshp and iwshp for repeated use
c     by subroutine shpe ....
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
c            nlon must beat least 4.
c
c     isym   currently not used.    
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpei. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpei. It must be at least
c            4*(nlat+1).
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpei. It must be at least
c            1.25*(nlat+1)**2+7*nlat+8.
c
c     **************************************************************
c
c     output parameters
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpe.        
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpe.        
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
      subroutine shpei(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
     1 liwshp,work,lwork,ierror)
      double precision work(*)
      dimension wshp(*),iwshp(*)
c
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
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
      kw4 = kw3+nte
      kw5 = kw4+nte+1
      kw6 = kw5+nte
      kw7 = kw6+nte
      kw8 = kw7+nte
      kw9 = kw8+nte
      kw10 = kw9+nloc2+nloc2
      kw11 = kw10+nloc2

      kw12 = kw11+nloc1
      kw13 = kw12+nloc1
c
      call shpei1(nlat,nlon,isym,mtrunc,nte,ierror,wshp(iw1),wshp(iw2),
     1  wshp(iw3),wshp(iw4),iwshp(jw1),iwshp(jw2),iwshp(jw3),
     2  iwshp(jw4),work(kw1),work(kw2),work(kw3),work(kw4),work(kw5),
     3  work(kw6),work(kw7),work(kw8),work(kw9),work(kw10),work(kw11),
     4  work(kw12),work(kw11),work(kw12),work(kw13))
      return
      end
      subroutine shpei1(nlat,nlon,isym,mtrunc,idp,ierror,
     1  pe,po,ze,zo,ipse,jzse,ipso,jzso,
     2  cp,work,wx,s,e,thet,xx,z,a,b,we,ped,wo,pod,u)
c
      double precision sum,eps,pi,dthet,v,a1,b1,c1
      parameter (eps=5.0d-8)
      double precision cp(idp),work(idp),wx(idp),s(idp+1),
     1  e(idp),thet(idp),xx(idp),z(idp),u(idp,idp),
     3  we(idp,idp,2),ped(idp,idp,2),a(4*idp),b(2*idp),
     4  wo(idp,idp,2),pod(idp,idp,2)
c
      dimension pe(idp,idp,2),po(idp,idp,2),ze(idp,idp,2),
     1  zo(idp,idp,2),
     2  ipse(idp,2),jzse(idp,2),ipso(idp,2),jzso(idp,2),
     3  nshe(2),nsho(2)
c
      ns2 = nlat/2
      modn = nlat-ns2-ns2
      nte = (nlat+1)/2
      nto = nlat-nte
      tusl = 0.
      toe = 0.
c
c     compute grid distribution
c
      pi = 4.d0*datan(1.0d0)
      dthet = pi/(nlat-1)
      do i=1,nte
      thet(i) = (i-1)*dthet
      end do
c
c     compute weight matrices for even functions
c
      do 40 mp1=1,2
      m = mp1-1
      mrank = nlat-m-m
      nem = (mrank+1)/2
      do j=1,nem
      n = j+j+m-2
      call dlfkp(m,n,cp)
      do i=1,nte
      call dlftp (m,n,thet(i),cp,ped(i,j,mp1))
      end do
      if(m.gt.0) ped(1,j,mp1) = 0.0d0
      end do
      call dsvdc(ped(m+1,1,mp1),idp,nem,nem,s,e,u,
     1                        idp,v,idp,work,10,info)
c
      do j=1,nem
      s(j) = 1.0d0/(s(j)*s(j)) 
      end do
c
c     compute weight matrix as u  s sup -2 u transpose
c
      do j=1,nte
      do i=1,nte
      we(i,j,mp1) = 0.0d0
      end do
      end do
      do i=1,nem
      do j=1,nem
      sum = 0.
      do k=1,nem
      sum = sum+s(k)*u(i,k)*u(j,k)
      end do
      we(i+m,j+m,mp1) = sum
      end do
      end do
 40   continue
      we(1,1,2) = 1.0d0
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
      nrank = ms2+ms2
      mrank = nlat-nrank
      nem = (mrank+1)/2
c
c     compute associated legendre functions
c
      if(m.le.1) then
      do 205 j=1,nem
      n = j+j+m-2
      call dlfkp(m,n,cp)
      do i=1,nte
      call dlftp (m,n,thet(i),cp,ped(i,j+ms2,ip))
      end do
 202  if(m.gt.0) ped(1,j+ms2,ip) = 0.0d0
 205  continue
c
      else
c
      do 207 j=1,nem
      n = j+j+m-2
      if(m.gt.1.and.n.gt.mxtr) then
      do i=1,nte
      u(i,j+ms2) = ped(i,j+ms2,ip)
      end do
      go to 207
      end if
      a1 = b(n-1)*a(n+m-3)/a(n+m-1)
      b1 = a(n-m+1)/a(n+m-1)
      if(n-m.le.1) then
      do i=1,nte
      u(i,j+ms2) = a1*ped(i,j+ms2-1,ip)   
     1                   - b1*ped(i,j+ms2,ip)    
      end do
      else
      c1 = b(n-1)*a(n-m-1)/a(n+m-1)
      do i=1,nte
      u(i,j+ms2) = a1*ped(i,j+ms2-1,ip)   
     1   - b1*ped(i,j+ms2,ip) + c1*u(i,j+ms2-1)    
      end do
      end if
 207  continue
      do j=1,nem
      do i=1,nte
      ped(i,j+ms2,ip) = u(i,j+ms2)
      end do
      end do
      end if
c
      if(ms2.le.0.or.ms2.ge.nte) go to 200
      do i=1,nte
      xx(i) = rand(0)
      end do
      it = 0
 201  do i=1,nte
      z(i) = 0.0d0
      wx(i) = 0.0d0
      do j=1,nte
      wx(i) = wx(i)+we(i,j,ip)*xx(j)
      end do
      end do
      do 220 j=1,nte
      if(j.eq.ms2) go to 220
      call gs(nte,wx,ped(1,j,ip),z)
 220  continue
c  
      do i=1,nte
      xx(i) = xx(i)-z(i)
      end do
      call normal(nte,xx,idp,we(1,1,ip))
      it = it+1
      if(it.le.2) go to 201
      do i=1,nte
      ped(i,ms2,ip) = xx(i)
      end do
 200  continue
c
c     reorder if mtrunc is less than nlat-1 
c         case of even functions
c
      if(modn.eq.0) then
      nshe(1) = (nlat-mtrunc-1)/2
      nshe(2) = (nlat-mtrunc-2)/2
      else
      nshe(1) = (nlat-mtrunc)/2
      nshe(2) = (nlat-mtrunc-1)/2
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
      sum = 0.0d0
      do k=1,nte
      sum = sum+ped(k,i,ip)*we(k,j,ip)
      end do
      pe(i,j,ip) = ped(i,j,ip)
      ze(j,i,ip) =  sum
      if(dabs(sum).gt.eps .and. lock.eq.0) then
      lock = 1
      jzse(i,ip) = j
      end if
      end do
      end do
 250  continue
c
c     compute weight matrices for odd functions
c
      do 50 mp1=1,2
      m = mp1-1
      mrank = nlat-m-m
      nem = (mrank+1)/2
      nom = mrank-nem
      do j=1,nom
      n = j+j+m-1
      call dlfkp(m,n,cp)
      do i=1,nte
      call dlftp (m,n,thet(i),cp,pod(i,j,mp1))
      end do
      if(modn.eq.1) pod(nte,j,mp1) = 0.0d0
      end do
      call dsvdc(pod(m+1,1,mp1),idp,nom,nom,s,e,u,
     1                        idp,v,idp,work,10,info)
c
      do j=1,nom
      s(j) = 1.0d0/(s(j)*s(j)) 
      end do
c
c     compute weight matrix as u  s sup -2 u transpose
c
      do j=1,nte
      do i=1,nte
      wo(i,j,mp1) = 0.0d0
      end do
      end do
      do i=1,nom
      do j=1,nom
      sum = 0.
      do k=1,nom
      sum = sum+s(k)*u(i,k)*u(j,k)
      end do
      wo(i+m,j+m,mp1) = sum
      end do
      end do
 50   continue
      wo(1,1,2) = 1.0d0  
      if(modn.eq.1) then
      wo(nte,nte,1) = 1.0d0  
      wo(nte,nte,2) = 1.0d0  
      end if
c
c     compute n**2 basis (odd functions)
c
      ip = 2
      do 300 mp1=1,mxtr+1
      ip = 3-ip
      m = mp1-1
      ms2 = mp1/2
      nrank = ms2+ms2
      mrank = nlat-nrank
      nem = (mrank+1)/2
      nom = mrank-nem
c
c     compute associated legendre functions
c
      if(m.le.1) then
      do 305 j=1,nom
      n = j+j+m-1
      call dlfkp(m,n,cp)
      do i=1,nte
      call dlftp (m,n,thet(i),cp,pod(i,j+ms2,ip))
      end do
 302  if(modn.eq.1) pod(nte,j+ms2,ip) = 0.0d0
      if(m.gt.0) pod(1,j+ms2,ip) = 0.0d0
 305  continue
c
      else
c
      do 307 j=1,nom
      n = j+j+m-1
      if(m.gt.1.and.n.gt.mxtr) then
      do i=1,nte
      u(i,j+ms2) = pod(i,j+ms2,ip)
      end do
      go to 304
      end if
      a1 = b(n-1)*a(n+m-3)/a(n+m-1)
      b1 = a(n-m+1)/a(n+m-1)
      if(n-m.le.1) then
      do i=1,nte
      u(i,j+ms2) = a1*pod(i,j+ms2-1,ip)   
     1                   - b1*pod(i,j+ms2,ip)    
      end do
      else
      c1 = b(n-1)*a(n-m-1)/a(n+m-1)
      do i=1,nte
      u(i,j+ms2) = a1*pod(i,j+ms2-1,ip)   
     1   - b1*pod(i,j+ms2,ip) + c1*u(i,j+ms2-1)    
      end do
      end if
 304  if(modn.eq.1) u(nte,j+ms2) = 0.0d0
 307  continue
      do j=1,nom
      do i=1,nte
      pod(i,j+ms2,ip) = u(i,j+ms2)
      end do
      end do
      end if
c
      if(ms2.le.0.or.ms2.ge.nto) go to 300
      do i=1,nte
      xx(i) = rand(0)
      end do
      if(modn.eq.1) xx(nte) = 0.0d0
      it = 0
 306  do i=1,nte
      z(i) = 0.
      wx(i) = 0.
      do j=1,nto
      wx(i) = wx(i)+wo(i,j,ip)*xx(j)
      end do
      end do
      do 330 j=1,nto
      if(j.eq.ms2) go to 330
      call gs(nte,wx,pod(1,j,ip),z(1))
 330  continue
c  
      do i=1,nte
      xx(i) = xx(i)-z(i)
      end do
      call normal(nte,xx,idp,wo(1,1,ip))
      it = it+1
      if(it.le.2) go to 306
      do i=1,nte
      pod(i,ms2,ip) = xx(i)
      end do
      if(modn.eq.1) pod(nte,ms2,ip) = 0.0d0
 300  continue
c
c     reorder if mtrunc is less than nlat-1
c        case of odd functions  
c
      if(modn.eq.0) then
      nsho(1) = (nlat-mtrunc)/2
      nsho(2) = (nlat-mtrunc-1)/2
      else
      nsho(1) = (nlat-mtrunc-1)/2
      nsho(2) = (nlat-mtrunc-2)/2
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
      sum = 0.0d0  
      do k=1,nte
      sum = sum+pod(k,i,ip)*wo(k,j,ip)
      end do
      po(i,j,ip) = pod(i,j,ip)
      zo(j,i,ip) = sum
      if(dabs(sum).gt.eps .and. lock.eq.0) then
      lock = 1
      jzso(i,ip) = j
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
c ... file shpe.f
c
c ... files which must be loaded with shpe.f
c
c     sphcom.f, hrfft.f
c
c     the n**2 projection with complement, odd/even
c     factorization and zero truncation on an
c     equally spaced grid as defined in the JCP paper
c     "Generalized discrete spherical harmonic transforms" 
c     by Paul N. Swarztrauber and William F. Spotz
c     It is equivalent to a harmonic analysis followed
c     by a synthesis except faster and requires less memory.
c
c     subroutine shpe(nlat,nlon,isym,mtrunc,x,y,idxy,
c    1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
c     shpe projects the array x onto the set of functions represented
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
c            nlon must beat least 4.
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
c            appear in the program that calls shpe. It must be
c            at least nlat. 
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpe.        
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpei. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpe.        
c
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpei. It must be at least
c            4*(nlat+1).
c
c     work   a single precision work array that does 
c            not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpe. It must be at least
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
      subroutine shpe(nlat,nlon,isym,mtrunc,x,y,idxy,
     1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
      dimension wshp(*),iwshp(*),work(*),x(idxy,nlon),y(idxy,nlon)
c
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
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
      call shpe1(nlat,nlon,isym,mtrunc,y,y,idxy,ierror,
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
      subroutine shpe1(nlat,nlon,isym,mtrunc,sx,sy,idxy,ierror,
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
      if(modn.eq.0) then
      nshe(1) = (nlat-mtrunc-1)/2
      nshe(2) = (nlat-mtrunc-2)/2
      nsho(1) = (nlat-mtrunc)/2
      nsho(2) = (nlat-mtrunc-1)/2
      else
      nshe(1) = (nlat-mtrunc)/2
      nshe(2) = (nlat-mtrunc-1)/2
      nsho(1) = (nlat-mtrunc-1)/2
      nsho(2) = (nlat-mtrunc-2)/2
      end if
      mxtr = min(nlat-1,nlon/2,mtrunc)
      ip = 2 
      do 100 mp1=1,mxtr+1
      ip = 3-ip
      if(mxtr.eq.nlat-1.and.mp1.le.2) then
      do i=1,nlat
      sy(i,mp1) = sx(i,mp1)
      end do
      if(mp1.eq.2) then
      sy(1,2) = 0.
      sy(nlat,2) = 0.
      end if
      if(nlon.ge.3) then
      sy(1,3) = 0.
      sy(nlat,3) = 0.
      do i=2,nlat-1
      sy(i,3) = sx(i,3)
      end do
      end if
      go to 100
      end if
      m = mp1-1
      mpm = max(1,m+m)
      ms2 = mp1/2
      mrank = min(nlat-m,nlat-ms2-ms2)   
c      mrank = mxtr+1-ms2-ms2
      nrank = nlat-mrank
      nem = (mrank+1)/2-nshe(ip)
      nom = mrank-(mrank+1)/2-nsho(ip)
      nec = nte-nem
      noc = nto-nom
c
      do i=1,nte
      xe(i,1) = .5*(sx(i,mpm)+sx(nlat+1-i,mpm))
      xo(i,1) = .5*(sx(i,mpm)-sx(nlat+1-i,mpm))
      end do
      if(mpm.lt.nlon) then
      do i=1,nte
      xe(i,2) = .5*(sx(i,mpm+1)+sx(nlat+1-i,mpm+1))
      xo(i,2) = .5*(sx(i,mpm+1)-sx(nlat+1-i,mpm+1))
      end do
      end if
      if(3*nec.lt.2*nem.or.nem.eq.0) then
      call tmxmx(nte,nec,idp,pe(1,1,ip),nte,idp,
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
      call tmxmx(nte,nem,idp,pe(1,nec+1,ip),nte,idp,
     1ze(1,nec+1,ip),xe,ye,ipse(nec+1,ip),jzse(nec+1,ip))
      end if
      if(3*noc.lt.2*nom.or.nom.eq.0) then
      call tmxmx(nto,noc,idp,po(1,1,ip),nto,idp,
     1          zo(1,1,ip),xo,yo,ipso(1,ip),jzso(1,ip))
      do i=1,nte
      yo(i,1) = xo(i,1)-yo(i,1)
      end do
      if(mpm.lt.nlon.and.m.ne.0) then
      do i=1,nte
      yo(i,2) = xo(i,2)-yo(i,2)
      end do
      end if
      else
      call tmxmx(nto,nom,idp,po(1,noc+1,ip),nto,idp,
     1zo(1,noc+1,ip),xo,yo,ipso(noc+1,ip),jzso(noc+1,ip))  
      end if
      do i=1,nte
      sy(i,mpm) = ye(i,1)+yo(i,1)
      sy(nlat+1-i,mpm) = ye(i,1)-yo(i,1)
      end do
      if(mpm.lt.nlon.and.m.ne.0) then
      do i=1,nte
      sy(i,mpm+1) = ye(i,2)+yo(i,2)
      sy(nlat+1-i,mpm+1) = ye(i,2)-yo(i,2)
      end do 
      end if
 100  continue
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
      subroutine tmxmx(lr,lc,ld,a,mc,md,b,x,y,is,js)
      dimension a(ld,*),b(md,*),x(ld,2),y(ld,2),
     1              is(*),js(*)
c
      kmx = min(lr+1,ld)
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
      dimension x(n),q(id,n)
      double precision x,q,sum,sqs
c
c     normalize x
c
      sqs = 0.
      do i=1,n
      sum = 0.
      do j=1,n
      sum = sum+q(i,j)*x(j)
      end do
      sqs = sqs+sum*x(i)
      end do
c
      sqs = dsqrt(sqs)
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
c     subroutine dlfkp(m,n,cp)
c
c     subroutine dlfkp computes the coefficients in the trigonometric
c     expansion of the normalized associated legendre functions:
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                        *sin(theta)**m/(2**n*factorial(n)) times the
c                        (n+m)th derivative of (x**2-1)**n with respect
c                        to x=cos(theta)
c
c     where theta is colatitude.
c
c     subroutine dlfkp computes the coefficients cp(k) in the
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
      subroutine dlfkp (m,n,cp)
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
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
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
      subroutine dlftp (m,n,theta,cp,pb)
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
      subroutine dsvdc(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info)
      integer ldx,n,p,ldu,ldv,job,info
      double precision x(ldx,1),s(1),e(1),u(ldu,1),v(ldv,1),work(1)
c
c
c     dsvdc is a subroutine to reduce a double precision nxp matrix x
c     by orthogonal transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         double precision(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by dsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u.
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v.
c                   (see below).
c
c         work      double precision(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    return the first min(n,p) singular
c                                  vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         double precision(mm), where mm=min(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         double precision(p), 
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         double precision(ldu,k), where ldu.ge.n.  if
c                                   joba.eq.1 then k.eq.n, if joba.ge.2
c                                   then k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.eq.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         double precision(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if job.eq.0.  if p.le.n,
c                   then v may be identified with x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = trans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (trans(u)
c                   is the transpose of u).  thus the singular
c                   values of x and b are the same.
c
c     linpack. this version dated 08/14/78 .
c              correction made to shift 2/84.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dsvdc uses the following functions and subprograms.
c
c     external drot
c     blas daxpy,ddot,dscal,dswap,dnrm2,drotg
c     fortran dabs,dmax1,max0,min0,mod,dsqrt
c
c     internal variables
c
      integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,
     *        mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
c      double precision ddot,t,r
      double precision ddot,t
      double precision b,c,cs,el,emm1,f,g,dnrm2,scale,shift,sl,sm,sn,
     *                 smm1,t1,test,ztest
      logical wantu,wantv
c
c
c     set the maximum number of iterations.
c
      maxit = 30
c
c     determine what is to be computed.
c
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min0(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
c
c     reduce x to bidiagonal form, storing the diagonal elements
c     in s and the super-diagonal elements in e.
c
      info = 0
      nct = min0(n-1,p)
      nrt = max0(0,min0(p-2,n))
      lu = max0(nct,nrt)
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
c
c           compute the transformation for the l-th column and
c           place the l-th diagonal in s(l).
c
            s(l) = dnrm2(n-l+1,x(l,l),1)
            if (s(l) .eq. 0.0d0) go to 10
               if (x(l,l) .ne. 0.0d0) s(l) = dsign(s(l),x(l,l))
               call dscal(n-l+1,1.0d0/s(l),x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (s(l) .eq. 0.0d0) go to 30
c
c              apply the transformation.
c
               t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
c
c           place the l-th row of x into  e for the
c           subsequent calculation of the row transformation.
c
            e(j) = x(l,j)
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
c
c           place the transformation in u for subsequent back
c           multiplication.
c
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
c
c           compute the l-th row transformation and place the
c           l-th super-diagonal in e(l).
c
            e(l) = dnrm2(p-l,e(lp1),1)
            if (e(l) .eq. 0.0d0) go to 80
               if (e(lp1) .ne. 0.0d0) e(l) = dsign(e(l),e(lp1))
               call dscal(p-l,1.0d0/e(l),e(lp1),1)
               e(lp1) = 1.0d0 + e(lp1)
   80       continue
            e(l) = -e(l)
            if (lp1 .gt. n .or. e(l) .eq. 0.0d0) go to 120
c
c              apply the transformation.
c
               do 90 i = lp1, n
                  work(i) = 0.0d0
   90          continue
               do 100 j = lp1, p
                  call daxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call daxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
c
c              place the transformation in v for subsequent
c              back multiplication.
c
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
c
c     set up the final bidiagonal matrix or order m.
c
      m = min0(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = 0.0d0
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = 0.0d0
c
c     if required, generate u.
c
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = 0.0d0
  180       continue
            u(j,j) = 1.0d0
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (s(l) .eq. 0.0d0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -ddot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call daxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call dscal(n-l+1,-1.0d0,u(l,l),1)
               u(l,l) = 1.0d0 + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = 0.0d0
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = 0.0d0
  260          continue
               u(l,l) = 1.0d0
  270       continue
  280    continue
  290    continue
  300 continue
c
c     if it is required, generate v.
c
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (e(l) .eq. 0.0d0) go to 320
               do 310 j = lp1, p
                  t = -ddot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call daxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = 0.0d0
  330       continue
            v(l,l) = 1.0d0
  340    continue
  350 continue
c
c     main iteration loop for the singular values.
c
      mm = m
      iter = 0
  360 continue
c
c        quit if all the singular values have been found.
c
c     ...exit
         if (m .eq. 0) go to 620
c
c        if too many iterations have been performed, set
c        flag and return.
c
         if (iter .lt. maxit) go to 370
            info = m
c     ......exit
            go to 620
  370    continue
c
c        this section of the program inspects for
c        negligible elements in the s and e arrays.  on
c        completion the variables kase and l are set as follows.
c
c           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m
c           kase = 2     if s(l) is negligible and l.lt.m
c           kase = 3     if e(l-1) is negligible, l.lt.m, and
c                        s(l), ..., s(m) are not negligible (qr step).
c           kase = 4     if e(m-1) is negligible (convergence).
c
         do 390 ll = 1, m
            l = m - ll
c        ...exit
            if (l .eq. 0) go to 400
            test = dabs(s(l)) + dabs(s(l+1))
            ztest = test + dabs(e(l))
            if (ztest .ne. test) go to 380
               e(l) = 0.0d0
c        ......exit
               go to 400
  380       continue
  390    continue
  400    continue
         if (l .ne. m - 1) go to 410
            kase = 4
         go to 480
  410    continue
            lp1 = l + 1
            mp1 = m + 1
            do 430 lls = lp1, mp1
               ls = m - lls + lp1
c           ...exit
               if (ls .eq. l) go to 440
               test = 0.0d0
               if (ls .ne. m) test = test + dabs(e(ls))
               if (ls .ne. l + 1) test = test + dabs(e(ls-1))
               ztest = test + dabs(s(ls))
               if (ztest .ne. test) go to 420
                  s(ls) = 0.0d0
c           ......exit
                  go to 440
  420          continue
  430       continue
  440       continue
            if (ls .ne. l) go to 450
               kase = 3
            go to 470
  450       continue
            if (ls .ne. m) go to 460
               kase = 1
            go to 470
  460       continue
               kase = 2
               l = ls
  470       continue
  480    continue
         l = l + 1
c
c        perform the task indicated by kase.
c
         go to (490,520,540,570), kase
c
c        deflate negligible s(m).
c
  490    continue
            mm1 = m - 1
            f = e(m-1)
            e(m-1) = 0.0d0
            do 510 kk = l, mm1
               k = mm1 - kk + l
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               if (k .eq. l) go to 500
                  f = -sn*e(k-1)
                  e(k-1) = cs*e(k-1)
  500          continue
               if (wantv) call drot(p,v(1,k),1,v(1,m),1,cs,sn)
  510       continue
         go to 610
c
c        split at negligible s(l).
c
  520    continue
            f = e(l-1)
            e(l-1) = 0.0d0
            do 530 k = l, m
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               f = -sn*e(k)
               e(k) = cs*e(k)
               if (wantu) call drot(n,u(1,k),1,u(1,l-1),1,cs,sn)
  530       continue
         go to 610
c
c        perform one qr step.
c
  540    continue
c
c           calculate the shift.
c
            scale = dmax1(dabs(s(m)),dabs(s(m-1)),dabs(e(m-1)),
     *                    dabs(s(l)),dabs(e(l)))
            sm = s(m)/scale
            smm1 = s(m-1)/scale
            emm1 = e(m-1)/scale
            sl = s(l)/scale
            el = e(l)/scale
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2.0d0
            c = (sm*emm1)**2
            shift = 0.0d0
            if (b .eq. 0.0d0 .and. c .eq. 0.0d0) go to 550
               shift = dsqrt(b**2+c)
               if (b .lt. 0.0d0) shift = -shift
               shift = c/(b + shift)
  550       continue
            f = (sl + sm)*(sl - sm) + shift
            g = sl*el
c
c           chase zeros.
c
            mm1 = m - 1
            do 560 k = l, mm1
               call drotg(f,g,cs,sn)
               if (k .ne. l) e(k-1) = f
               f = cs*s(k) + sn*e(k)
               e(k) = cs*e(k) - sn*s(k)
               g = sn*s(k+1)
               s(k+1) = cs*s(k+1)
               if (wantv) call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
               call drotg(f,g,cs,sn)
               s(k) = f
               f = cs*e(k) + sn*s(k+1)
               s(k+1) = -sn*e(k) + cs*s(k+1)
               g = sn*e(k+1)
               e(k+1) = cs*e(k+1)
               if (wantu .and. k .lt. n)
     *            call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
  560       continue
            e(m-1) = f
            iter = iter + 1
         go to 610
c
c        convergence.
c
  570    continue
c
c           make the singular value  positive.
c
            if (s(l) .ge. 0.0d0) go to 580
               s(l) = -s(l)
               if (wantv) call dscal(p,-1.0d0,v(1,l),1)
  580       continue
c
c           order the singular value.
c
  590       if (l .eq. mm) go to 600
c           ...exit
               if (s(l) .ge. s(l+1)) go to 600
               t = s(l)
               s(l) = s(l+1)
               s(l+1) = t
               if (wantv .and. l .lt. p)
     *            call dswap(p,v(1,l),1,v(1,l+1),1)
               if (wantu .and. l .lt. n)
     *            call dswap(n,u(1,l),1,u(1,l+1),1)
               l = l + 1
            go to 590
  600       continue
            iter = 0
            m = m - 1
  610    continue
      go to 360
  620 continue
      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      DNRM2 = NORM
      RETURN
*
*     End of DNRM2.
*
      END
      subroutine  drot (n,dx,incx,dy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp,c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end
      subroutine drotg(da,db,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,db,c,s,roe,scale,r,z
c
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         z = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
      c = da/r
      s = db/r
      z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
      if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
   20 da = r
      db = z
      return
      end
      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
