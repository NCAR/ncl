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
      program tvtsgs
c     
c     this program checks vtsgs for computing the colatitudinal
c     derivatives of the velocity .... 
c
      parameter (nlat=17,nlon=32)
      parameter (idp=nlat,jdp=nlon,mdab=nlat,ndab=nlat)
c
      dimension u(idp,jdp),v(idp,jdp),w(idp,jdp),
     1          x(idp,jdp),y(idp,jdp),z(idp,jdp),
     2          vt(idp,jdp),wt(idp,jdp),
     3          a(mdab,ndab,3),b(mdab,ndab,3),
     4          da(mdab,ndab,3,3),db(mdab,ndab,3,3),
     5          br(mdab,ndab),bi(mdab,ndab),
     6          cr(mdab,ndab),ci(mdab,ndab),
     7          c(idp,jdp,3,3),s(idp,jdp,3,3),
     8          dthet(idp),dwts(idp)
      dimension wshi(608411),wvha(1098771),wvhs(1098771),
     1          wvts(1098771),work(165765)
      double precision dthet,dwts,dwork(25741)
c
      lwshi = 608411
      lwvha = 1098771
      lwvhs = 1098771
      lwvts = 1098771
      lwork = 165765
      ldwork = 25741
c
      pi = 4.*atan(1.)
      hpi = pi/2.
      dtr = pi/180.
      isym = 0
      nt = 1
c 
c     initialize spherepack routines
c
      call shigs(nlat,nlon,wshi,lwshi,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(6,55) ierror
   55 format(' error' i4 ' in shigs')  
      call vhagsi(nlat,nlon,wvha,lwvha,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(6,57) ierror
   57 format(' error' i4 ' in vhagsi')  
      call vhsgsi(nlat,nlon,wvhs,lwvhs,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(6,58) ierror
   58 format(' error' i4 ' in vhsgsi')  
      call vtsgsi(nlat,nlon,wvts,lwvts,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(6,59) ierror
   59 format(' error' i4 ' in vtsgsi')  
c
c     compute gauss points and weights
c
      call gaqd(nlat,dthet,dwts,work,lwork,ierror)
c
c     set vector harmonic coefficients
c    
      do np1=1,nlat
      do mp1=1,np1
      br(mp1,np1) = 0.
      bi(mp1,np1) = 0.
      cr(mp1,np1) = 0.
      ci(mp1,np1) = 0.
      end do
      end do
      do np1=1,nlat-1
      do mp1=1,np1
      br(mp1,np1) = rand(om)
      bi(mp1,np1) = rand(om)
      cr(mp1,np1) = rand(om)
      ci(mp1,np1) = rand(om)
      end do
      end do
c
      call vhsgs(nlat,nlon,0,1,v,w,idp,jdp,br,bi,
     1           cr,ci,mdab,ndab,wvhs,lwvhs,work,lwork,ierror)
      if(ierror .ne. 0) write(6,79) ierror
 79   format(' error' i4 ' in vhsgs at point 1')  
c
      do j=1,nlon
      do i=1,nlat
      u(i,j) = 0.
      end do
      end do
c
c     convert to cartesian coordinates
c      
      dphi = 2.*pi/nlon
      do j=1,nlon
      phi = (j-1)*dphi
      do i=1,nlat
      theta = dthet(i)
      call stoc(theta,phi,u(i,j),v(i,j),w(i,j),x(i,j),y(i,j),z(i,j))
      end do
      end do
c
c     compute harmonic components of x component
c
      call shags(nlat,nlon,0,1,x,idp,jdp,a(1,1,1),b(1,1,1),
     1           mdab,ndab,wshi,lwshi,work,lwork,ierror)
      if(ierror .ne. 0) write(6,16) ierror
 16   format(' error' i4 ' in shags at point 2')  
c
c     write harmonic coefficients for x
c
c      write(6,20)
c 20   format(//'  harmonic coefficients for x'//)
c      do mp1=1,nlat
c      do np1=mp1,nlat
c      write(6,5) mp1,np1,a(mp1,np1,1),b(mp1,np1,1)
c 5    format(2i5,1p2e15.6)
c      end do
c      end do
c
c     compute harmonic components of y component
c
      call shags(nlat,nlon,0,1,y,idp,jdp,a(1,1,2),b(1,1,2),
     1           mdab,ndab,wshi,lwshi,work,lwork,ierror)
      if(ierror .ne. 0) write(6,17) ierror
 17   format(' error' i4 ' in shags at point 3')  
c
c     write harmonic coefficients for y
c
c      write(6,21)
c 21   format(//'  harmonic coefficients for y'//)
c      do mp1=1,nlat
c      do np1=mp1,nlat
c      write(6,5) mp1,np1,a(mp1,np1,2),b(mp1,np1,2)
c      end do
c      end do
c
c     compute harmonic components of z component
c
      call shags(nlat,nlon,0,1,z,idp,jdp,a(1,1,3),b(1,1,3),
     1           mdab,ndab,wshi,lwshi,work,lwork,ierror)
      if(ierror .ne. 0) write(6,18) ierror
   18 format(' error' i4 ' in shags at point 4')  
c
c     write harmonic coefficients for z
c
c      write(6,22)
c 22   format(//'  harmonic coefficients for z'//)
c      do mp1=1,nlat
c      do np1=mp1,nlat
c      write(6,5) mp1,np1,a(mp1,np1,3),b(mp1,np1,3)
c      end do
c      end do
c
c     compute partials of x, y, and z wrt z
c
      call dbdz(nlat,nlat,a(1,1,1),b(1,1,1),da(1,1,1,3),db(1,1,1,3))
      call dbdz(nlat,nlat,a(1,1,2),b(1,1,2),da(1,1,2,3),db(1,1,2,3))
      call dbdz(nlat,nlat,a(1,1,3),b(1,1,3),da(1,1,3,3),db(1,1,3,3))
c
c     compute partials of x, y, and z wrt y
c
      call dbdy(nlat,nlat,a(1,1,1),b(1,1,1),da(1,1,1,2),db(1,1,1,2))
      call dbdy(nlat,nlat,a(1,1,2),b(1,1,2),da(1,1,2,2),db(1,1,2,2))
      call dbdy(nlat,nlat,a(1,1,3),b(1,1,3),da(1,1,3,2),db(1,1,3,2))
c
c     compute partials of x, y, and z wrt x
c
      call dbdx(nlat,nlat,a(1,1,1),b(1,1,1),da(1,1,1,1),db(1,1,1,1))
      call dbdx(nlat,nlat,a(1,1,2),b(1,1,2),da(1,1,2,1),db(1,1,2,1))
      call dbdx(nlat,nlat,a(1,1,3),b(1,1,3),da(1,1,3,1),db(1,1,3,1))
c
c     transform cartesian jacobian to physical space
c
      call shsgs(nlat,nlon,0,9,c,idp,jdp,da,db,idp,idp,
     1           wshi,lwshi,work,lwork,ierror)
      if(ierror .ne. 0) write(6,19) ierror
 19   format(' error' i4 ' in shsgs at point 5')  
c
c     convert to jacobian to spherical coordinates
c        (multiply cartesian jacobian by q)
c      
      do k=1,3
      do j=1,nlon
      phi = (j-1)*dphi
      do i=1,nlat
      theta = dthet(i)
      call ctos(theta,phi,c(i,j,1,k),c(i,j,2,k),c(i,j,3,k),
     1                    s(i,j,1,k),s(i,j,2,k),s(i,j,3,k))
      end do
      end do
      end do
c
c     form s = (q sq**T)**T
c
      do k=1,3
      do j=1,nlon
      phi = (j-1)*dphi
      do i=1,nlat
      theta = dthet(i)
      call ctos(theta,phi,s(i,j,k,1),s(i,j,k,2),s(i,j,k,3),
     1                    c(i,j,k,1),c(i,j,k,2),c(i,j,k,3))
      end do
      end do
      end do

      do j=1,nlon
      do i=1,nlat
      do iq=1,3
      do jq=1,3
      s(i,j,iq,jq) = c(i,j,iq,jq)
      end do
      end do
      end do
      end do
c
c     check derivative program
c
      call vtsgs(nlat,nlon,0,1,vt,wt,idp,jdp,br,bi,cr,ci,
     1           mdab,ndab,wvts,lwvts,work,lwork,ierror)
      if(ierror .ne. 0) write(6,4) ierror
 4    format(' error' i4 ' in vtsgs during initialization')  
      dmax1 = 0.
      dmax2 = 0.
      do j=1,nlon
      do i=1,nlat
      dmax1 = max(dmax1,abs(s(i,j,2,2)-vt(i,j)))
      dmax2 = max(dmax2,abs(s(i,j,3,2)-wt(i,j)))
      end do
      end do
      write(6,2) dmax1,dmax2
 2    format(' error in vt '1pe15.6' error in wt '1pe15.6)
      end
      subroutine ctos(theta,phi,x,y,z,u,v,w)
c
c     this program computes the components of a vector
c     field in spherical coordinates u, v, and w, from
c     its components x, y, and z in cartesian coordinates
c
      st = sin(theta)
      ct = cos(theta)
      sp = sin(phi)
      cp = cos(phi)
      temp1 = cp*x+sp*y
      temp2 = cp*y-sp*x
      u = st*temp1+ct*z
      v = ct*temp1-st*z
      w = temp2
      return
      end
      subroutine stoc(theta,phi,u,v,w,x,y,z)
c
c     this program computes the components of a vector
c     field in cartesian coordinates x, y, and z, from
c     its components u, v, and w in spherical coordinates
c
      st = sin(theta)
      ct = cos(theta)
      sp = sin(phi)
      cp = cos(phi)
      temp1 = st*u+ct*v
      temp2 = ct*u-st*v
      x = cp*temp1-sp*w
      y = sp*temp1+cp*w
      z = temp2
      return
      end
      subroutine dbdx(l,mdim,a,b,dxa,dxb)
c
c     subroutine to compute the coefficients in the spherical
c     harmonic representation of the derivative with respect to x
c     of a scalar function. i.e. given the coefficients a and b
c     in the spectral representation of a function, then dbdx
c     computes the coefficients dxa and dxb in the spectral
c     representation of the derivative of the function
c     with respect to x.
c
c     the arrays a and dxa can be the same as well as the arrays
c     b and dxb, i.e. the arrays a and b can be overwritten by
c     dxa and dxb respectively.
c
      dimension a(mdim,1),b(mdim,1),dxa(mdim,1),dxb(mdim,1)
      dxa(1,1) = sqrt(6.)*a(2,2)
      dxb(1,1) = 0.
      lm1 = l-1
      do 10 np1=2,lm1
      n = np1-1
      fn = float(n)
      cn = (fn+fn+3.)/(fn+fn+1.)
      dxa(1,np1) = sqrt(cn*(fn+2.)*(fn+1.))*a(2,np1+1)
      dxb(1,np1) = 0.
      do 10 mp1=2,np1
      fm = float(mp1-1)
      a1 = .5*sqrt(cn*(fn+fm+2.)*(fn+fm+1.))
      a2 = .5*sqrt(cn*(fn-fm+2.)*(fn-fm+1.))
      dxa(mp1,np1) = a1*a(mp1+1,np1+1)-a2*a(mp1-1,np1+1)
      dxb(mp1,np1) = a1*b(mp1+1,np1+1)-a2*b(mp1-1,np1+1)
   10 continue
      do 5 mp1=1,l
      dxa(mp1,l) = 0.
      dxb(mp1,l) = 0.
    5 continue
      return
      end
      subroutine dbdy(l,mdim,a,b,dya,dyb)
c
c     subroutine to compute the coefficients in the spherical
c     harmonic representation of the derivative with respect to y
c     of a scalar function. i.e. given the coefficients a and b
c     in the spectral representation of a function, then dbdy
c     computes the coefficients dya and dyb in the spectral
c     representation of the derivative of the function
c     with respect to y.
c
c     the arrays a and dya can be the same as well as the arrays
c     b and dyb, i.e. the arrays a and b can be overwritten by
c     dya and dyb respectively.
c
      dimension a(mdim,1),b(mdim,1),dya(mdim,1),dyb(mdim,1)
      dya(1,1) = -sqrt(6.)*b(2,2)
      dyb(1,1) = 0.
      lm1 = l-1
      do 10 np1=2,lm1
      n = np1-1
      fn = float(n)
      cn = (fn+fn+3.)/(fn+fn+1.)
      dya(1,np1) = -sqrt(cn*(fn+2.)*(fn+1.))*b(2,np1+1)
      dyb(1,np1) = 0.
      do 10 mp1=2,np1
      fm = float(mp1-1)
      a1 = .5*sqrt(cn*(fn+fm+2.)*(fn+fm+1.))
      a2 = .5*sqrt(cn*(fn-fm+2.)*(fn-fm+1.))
      dya(mp1,np1) = -a1*b(mp1+1,np1+1)-a2*b(mp1-1,np1+1)
      dyb(mp1,np1) =  a1*a(mp1+1,np1+1)+a2*a(mp1-1,np1+1)
   10 continue
      do 5 mp1=1,l
      dya(mp1,l) = 0.
      dyb(mp1,l) = 0.
    5 continue
      return
      end
      subroutine dbdz(l,mdim,a,b,dza,dzb)
c
c     subroutine to compute the coefficients in the spherical
c     harmonic representation of the derivative with respect to z
c     of a scalar function. i.e. given the coefficients a and b
c     in the spectral representation of a function, then dbdz
c     computes the coefficients dza and dzb in the spectral
c     representation of the derivative of the function
c     with respect to z.
c
c     the arrays a and dza can be the same as well as the arrays
c     b and dzb, i.e. the arrays a and b can be overwritten by
c     dza and dzb respectively.
c
      dimension a(mdim,1),b(mdim,1),dza(mdim,1),dzb(mdim,1)
      lm1 = l-1
      do 10 np1=1,lm1
      n = np1-1
      fn = float(n)
      cn = (fn+fn+3.)/(fn+fn+1.)
      do 10 mp1=1,np1
      fm = float(mp1-1)
      a1 = sqrt(cn*(fn-fm+1.)*(fn+fm+1.))
      dza(mp1,np1) = a1*a(mp1,np1+1)
      dzb(mp1,np1) = a1*b(mp1,np1+1)
   10 continue
      do 5 mp1=1,l
      dza(mp1,l) = 0.
      dzb(mp1,l) = 0.
    5 continue
      return
      end


