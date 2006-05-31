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
c ... file shallow.f
c
c     subroutine shallow solves the nonlinear shallow-water equations
c     on the sphere using SPHEREPACK3.0 software.
c
c ... required spherepack3.0 files
c
c     vtses.f, dives.f, vrtes.f, grades.f, sphcom.f, hrfft.f,
c     vhaes.f,vhses.f,shaes.f,shses.f
c

      program shallow
c
c     the nonlinear shallow-water equations on the sphere are
c     solved using a spectral method based on the spherical
c     vector harmonics. the method is described in the paper:
c
c [1] p. n. swarztrauber, spectral transform methods for solving
c     the shallow-water equations on the sphere, p.n. swarztrauber,
c     monthly weather review, vol. 124, no. 4, april 1996, pp. 730-744.
c
c     this program implements test case 3 (steady nonlinear rotated flow)
c     in the paper:
c
c [2] d.l. williamson, j.b. drake, j.j. hack, r. jakob, and
c     p.n. swarztrauber, j. comp. phys., a standard test set
c     for numerical approximations to the shallow-water
c     equations in spherical geometry, j. comp. phys.,
c     vol. 102, no. 1, sept. 1992, pp. 211-224.
c
c definitions:
c
c
c     nlat          number of latitudes including poles
c     nlon          number of distinct longitudes
c     mmode         max wave number
c     omega         rotation rate of earth in radians per second
c     aa            radius of earth in meters
c     pzero         mean height of geopotential
c     uzero         maximum velocity
c     alpha         tilt angle of the rotated grid
c     ncycle        cycle number
c     time          model time in seconds
c     dt            time step
c     lambda        longitude
c     theta         colatitude
c
c   the first dimension of the following two dimensional arrays
c   corresponds to the latitude index with values i=1,...,nlat
c   where i=1 is the north pole and i=nlat is the south pole.
c   the second dimension is longitude with values j=1,...,nlon
c   where j=1 corresponds to zero longitude and j=nlon corresponds
c   to 2pi minus 2pi/nlon.
c
c     u(i,j)       east longitudinal velocity component at t=time
c     v(i,j)       latitudinal velocity component at t=time
c     p(i,j)       +pzero = geopotential at t=time
c
c     unew(i,j)    east longitudinal velocity component at t=time+dt
c     vnew(i,j)    latitudinal velocity component at t=time+dt
c     pnew(i,j)    +pzero = geopotential at t=time+dt
c
c     uold(i,j)    east longitudinal velocity component at t=time-dt
c     vold(i,j)    latitudinal velocity component at t=time-dt
c     pold(i,j)    +pzero = geopotential at t=time-dt
c
c     divg(i,j)    divergence (d/dtheta (cos(theta) v)
c                                          + du/dlambda)/cos(theta)
c     vort(i,j)    vorticity  (d/dtheta (cos(theta) u)
c                                          - dv/dlambda)/cos(theta)
c
c     ut(i,j)      latitudinal derivative of longitudinal
c                  velocity component
c     vt(i,j)      latitudinal derivative of latitudinal
c                  velocity component
c
c     dudt(i,j)    time derivative of longitudinal velocity component
c     dvdt(i,j)    time derivative of latitudinal  velocity component
c     dpdt(i,j)    time derivative of geopotential
c
c     gpdl(i,j)    first component of the gradient of p(i,j)
c                  the longitudinal derivative of the geopotential
c                  divided by the cosine of the latitude
c
c     gpdt(i,j)    second component of the gradient of p(i,j)
c                  the latitudinal derivative of the geopotential
c
c     uxact(i,j)   the "exact" longitudinal veloctiy component
c     vxact(i,j)   the "exact" latitudinal  veloctiy component
c     uxact(i,j)   the "exact" geopotential
c
c     f(i,j)       the coriolis force on rotated grid
c
c   the following two dimensional arrays are nonzero in the triangle
c   n=1,...,nlat and m less than or equal to n.
c
c     a(m,n),b(m,n)    spectral coefficients of the geopotential
c
c     br(m,n),bi(m,n)  spectral coefficients of the velocity
c     cr(m,n),ci(m,n)  vector [u(i,j),v(i,j)]
c
c
c     phlt(i)      the coefficients in the cosine series
c                  representation of the unrotated geopotential
c
      parameter (idp=73,jdp=144,mdab=73,ndab=73)
      parameter(lldwork = 2*(idp+1))
c
c
      dimension u(idp,jdp),v(idp,jdp),p(idp,jdp),f(idp,jdp),
     1          unew(idp,jdp),vnew(idp,jdp),pnew(idp,jdp),
     2          uold(idp,jdp),vold(idp,jdp),pold(idp,jdp),
     3          uxact(idp,jdp),vxact(idp,jdp),pxact(idp,jdp),
     4          divg(idp,jdp),vort(idp,jdp),ut(idp,jdp),
     5          vt(idp,jdp),dudt(idp,jdp),dvdt(idp,jdp),
     6          dpdt(idp,jdp),gpdt(idp,jdp),gpdl(idp,jdp),
     7          a(mdab,ndab),b(mdab,ndab),br(mdab,ndab),
     8          bi(mdab,ndab),cr(mdab,ndab),ci(mdab,ndab),
     9          phlt(361)
c
c   the following work arrays are initialized and subsequently
c   used repeatedly by spherepack (2.0) routines.
c
      dimension wsha(70928),wshs(70928),wvha(141647),wvhs(141647),
     1          wvts(141647),work(40000)
      dimension dwork(lldwork)
c
      real lambda,lhat

      lwsha = 70928
      lwshs = 70928
      lwvha = 141647
      lwvhs = 141647
      lwvts = 141647
      lwork = 40000
      ldwork = lldwork
c
      pi = 4.*atan(1.)
      hpi = pi/2.
      dtr = pi/180.
      aa = 6.37122e6
      omega = 7.292e-5
      fzero = omega+omega
      uzero = 40.
      pzero = 2.94e4
      alphad = 60.
      alpha = dtr*alphad
c
      itmax = 720
      mprint = 72
      mmode = 42
      nlat = 65
      nlon = 128
      dt = 600.
      tdt = dt+dt
c
c     initialize spherepack 2.0 routines
c
      call shaesi(nlat,nlon,wsha,lwsha,work,lwork,dwork,lwork,ierror)
      if(ierror .ne. 0) write(*,55) ierror
   55 format(' error' i4 ' in shaesi')
      call shsesi(nlat,nlon,wshs,lwshs,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,56) ierror
   56 format(' error' i4 ' in shsesi')
      call vhaesi(nlat,nlon,wvha,lwvha,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,57) ierror
   57 format(' error' i4 ' in vhaesi')
      call vhsesi(nlat,nlon,wvhs,lwvhs,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,58) ierror
   58 format(' error' i4 ' in vhsesi')
      call vtsesi(nlat,nlon,wvts,lwvts,work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,59) ierror
   59 format(' error' i4 ' in vtsesi')
c
c
c     compute the derivative of the unrotated geopotential
c             p as a function of latitude
c
      nl = 91
      nlm1 = nl-1
      nlm2 = nl-2
      cfn = 1./nlm1
      dlath = pi/nlm1
      do 10 i=1,nlm2
      theta = i*dlath
      sth = sin(theta)
      cth = cos(theta)
      uhat = ui(uzero,hpi-theta)
      phlt(i) = cfn*cth*uhat*(uhat/sth+aa*fzero)
   10 continue
c
c     compute sine transform of the derivative of the geopotential
c     for the purpose of computing the geopotential by integration
c     see equation (3.9) in reference [1] above
c
      call sine(nlm2,phlt,work)
c
c     compute the cosine coefficients of the unrotated geopotential
c     by the formal integration of the sine series representation
c
      do 12 i=1,nlm2
      phlt(i) = -phlt(i)/i
   12 continue
c
c     phlt(i) contains the coefficients in the cosine series
c     representation of the unrotated geopotential that are used
c     below to compute the geopotential on the rotated grid.
c
c     compute the initial values of  east longitudinal
c     and latitudinal velocities u and v as well as the
c     geopotential p and coriolis f on the rotated grid.
c
      ca = cos(alpha)
      sa = sin(alpha)
      dtheta = pi/(nlat-1)
      dlam = (pi+pi)/nlon
      do 50 j=1,nlon
      lambda = (j-1)*dlam
      cl = cos(lambda)
      sl = sin(lambda)
      do 50 i=1,nlat
c
c     lambda is longitude, theta is colatitude, and pi/2-theta is
c     latitude on the rotated grid. lhat and that are longitude
c     and colatitude on the unrotated grid. see text starting at
c     equation (3.10)
c
      theta = (i-1)*dtheta
      st = cos(theta)
      ct = sin(theta)
      sth = ca*st+sa*ct*cl
      cthclh = ca*ct*cl-sa*st
      cthslh = ct*sl
      lhat = atanxy(cthclh,cthslh)
      clh = cos(lhat)
      slh = sin(lhat)
      cth = clh*cthclh+slh*cthslh
      that = atanxy(sth,cth)
      uhat = ui(uzero,hpi-that)
      pxact(i,j) = cosine(that,nlm2,phlt)
      uxact(i,j) = uhat*(ca*sl*slh+cl*clh)
      vxact(i,j) = uhat*(ca*cl*slh*st-clh*sl*st+sa*slh*ct)
      f(i,j) = fzero*sth
   50 continue
c
      vmax = 0.
      pmax = 0.
      v2max = 0.
      p2max = 0.
      do 54 j=1,nlon
      do 54 i=1,nlat
      v2max = v2max+uxact(i,j)**2+vxact(i,j)**2
      p2max = p2max+pxact(i,j)**2
      vmax = amax1(abs(uxact(i,j)),abs(vxact(i,j)),vmax)
      pmax = amax1(abs(pxact(i,j)),pmax)
 54   continue
c
c     initialize first time step
c
      do 60 j=1,nlon
      do 60 i=1,nlat
      u(i,j) = uxact(i,j)
      v(i,j) = vxact(i,j)
      p(i,j) = pxact(i,j)
   60 continue
c
      isym = 0
      nt = 1
      time = 0.
      ctime = 0.
      ncycle = 0
c
c     start of the time loop
c
c   begin step 1, section 3
c
c     analyze the velocity components (u,v)
c
   90 call vhaesgo(nlat,nlon,isym,nt,u,v,idp,jdp,br,bi,cr,ci,
     1           mdab,ndab,wvha,lwvha,work,lwork,ierror)
      if(ierror .ne. 0) write(*,91) ierror
   91 format(' error' i4 ' in vhaes')
c
c     truncate spectrum to eliminate aliasing of the
c     product terms in the shallow-water equations
c
      call truncw(nlat,mmode,mdab,br,bi)
      call truncw(nlat,mmode,mdab,cr,ci)
c
c     resynthesize the velocity components
c
      call vhsesgo(nlat,nlon,isym,nt,u,v,idp,jdp,br,bi,cr,ci,
     1           mdab,ndab,wvhs,lwvhs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,92) ierror
   92 format(' error' i4 ' in vhses')
c
c   begin step 2, section 3
c
c     analyze geopotential p
c
      call shaes(nlat,nlon,isym,nt,p,idp,jdp,a,b,mdab,ndab,
     1                    wsha,lwsha,work,lwork,ierror)
      if(ierror .ne. 0) write(*,93) ierror
   93 format(' error' i4 ' in shaes')
c
c     truncate spectrum to eliminate aliasing of the
c     product terms in the shallow-water equations
c
      call truncw(nlat,mmode,mdab,a,b)
c
c     resynthesize the geopotential p
c
      call shses(nlat,nlon,isym,nt,p,idp,jdp,a,b,mdab,ndab,
     1                    wshs,lwshs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,94) ierror
   94 format(' error' i4 ' in shses')
c
c
c   begin step 3, section 3
c
c     compute the vorticity of the velocity (u,v)
c
      call vrtes(nlat,nlon,isym,nt,vort,idp,jdp,cr,ci,mdab,ndab,
     1                 wshs,lwshs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,95) ierror
   95 format(' error' i4 ' in vrtes')
c
c     compute the divergence of the velocity (u,v)
c
      call dives(nlat,nlon,isym,nt,divg,idp,jdp,br,bi,mdab,ndab,
     1           wshs,lwshs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,96) ierror
   96 format(' error' i4 ' in dives')
c
c   begin step 4, section 3
c
c     compute the derivative of the velocity (u,v) with
c     respect to colatitude theta.
c
      call vtsesgo(nlat,nlon,isym,nt,ut,vt,idp,jdp,br,bi,cr,ci,
     1           mdab,ndab,wvts,lwvts,work,lwork,ierror)
      if(ierror .ne. 0) write(*,97) ierror
   97 format(' error' i4 ' in vtsesgo')
c
c   begin step 5, section 3
c
c     compute the gradient of the geopotential p
c
      call gradesgo(nlat,nlon,isym,nt,gpdl,gpdt,idp,jdp,a,b,mdab,ndab,
     1wvhs,lwvhs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,98) ierror
   98 format(' error' i4 ' in grades')
c
c     compute the time derivatives of the velocity (u,v)
c     and the geopotential p using the shallow-water 
c     equations (2.8), (2.9), and (2.10), section 3.
c
      do 200 j=1,nlon
      do 200 i=1,nlat
      dudt(i,j) = (u(i,j)*(vt(i,j)-divg(i,j))-v(i,j)*ut(i,j)
     1            -gpdl(i,j))/aa+f(i,j)*v(i,j)
      dvdt(i,j) = -(u(i,j)*(vort(i,j)+ut(i,j))+v(i,j)*vt(i,j)
     1            +gpdt(i,j))/aa-f(i,j)*u(i,j)
      dpdt(i,j) = -((p(i,j)+pzero)*divg(i,j)+v(i,j)*gpdt(i,j)
     1             +u(i,j)*gpdl(i,j))/aa
  200 continue
c
      if(mod(ncycle,mprint) .ne. 0) go to 370
      htime = time/3600.
      write(*,390) ncycle,htime,dt,nlat,nlon,mmode,omega,pzero,
     1             uzero,alphad
  390 format(//' steady nonlinear rotated flow, test case 3'/
     1         ' cycle number              ' i10
     2         ' model time in  hours      ' f10.2/
     3         ' time step in seconds      ' f10.0
     4         ' number of latitudes       ' i10/
     5         ' number of longitudes      ' i10
     6         ' max wave number           ' i10/
     7         ' rotation rate        ' 1pe15.6
     8         ' mean height          ' 1pe15.6/
     9         ' maximum velocity     ' 1pe15.6
     1         ' tilt angle                ' f10.2)
      dvgm = 0.
      dvmax = 0.
      dpmax = 0.
      evmax = 0.0
      epmax = 0.0
      do 217 j=1,nlon
      do 217 i=1,nlat
      dvgm = amax1(dvgm,abs(divg(i,j)))
      dvmax = dvmax+(u(i,j)-uxact(i,j))**2+(v(i,j)-vxact(i,j))**2
      dpmax = dpmax+(p(i,j)-pxact(i,j))**2
      evmax = amax1(evmax,abs(v(i,j)-vxact(i,j)),abs(u(i,j)-uxact(i,j)))
      epmax = amax1(epmax,abs(p(i,j)-pxact(i,j)))
  217 continue
      dvmax = sqrt(dvmax/v2max)
      dpmax = sqrt(dpmax/p2max)
      evmax = evmax/vmax
      epmax = epmax/pmax
      write(*,391) evmax,epmax,dvmax,dpmax,dvgm
  391 format(' max error in velocity' 1pe15.6
     +       ' max error in geopot. ' 1pe15.6/
     +       ' l2 error in velocity ' 1pe15.6
     +       ' l2 error in geopot.  ' 1pe15.6/
     +       ' maximum divergence   ' 1pe15.6)
c
c     set values at time = -dt to values at time = 0.
c
  370 if(ncycle .gt. 0) go to 206
      do 205 j=1,nlon
      do 205 i=1,nlat
      uold(i,j) = u(i,j)
      vold(i,j) = v(i,j)
      pold(i,j) = p(i,j)
  205 continue
c
c     compute values at next time level using leap frog
c     time differencing
c
  206 do 210 j=1,nlon
      do 210 i=1,nlat
      unew(i,j) = uold(i,j)+tdt*dudt(i,j)
      vnew(i,j) = vold(i,j)+tdt*dvdt(i,j)
      pnew(i,j) = pold(i,j)+tdt*dpdt(i,j)
  210 continue
c
c     update values to next time level
c
      do 300 j=1,nlon
      do 300 i=1,nlat
      uold(i,j) = u(i,j)
      vold(i,j) = v(i,j)
      pold(i,j) = p(i,j)
      u(i,j) = unew(i,j)
      v(i,j) = vnew(i,j)
      p(i,j) = pnew(i,j)
  300 continue
      ncycle = ncycle+1
      time = time+dt
      if(ncycle .le. itmax) go to 90
      end

      subroutine vtsesgo(nlat,nlon,ityp,nt,ut,vt,idvw,jdvw,br,bi,cr,ci,
     1           mdab,ndab,wvts,lwvts,work,lwork,ierror)
c
c     vtsesgo computes the latitudinal derivatives of the
c     velocity components using subroutine vtses which
c     assumes the velocity components are given in terms
c     of mathematical coordinates
c
      dimension ut(idvw,jdvw,1),vt(idvw,jdvw,1),br(mdab,ndab,1),
     1          bi(mdab,ndab,1),cr(mdab,ndab,1),ci(mdab,ndab,1),
     2          work(*),wvts(*)
      call vtses(nlat,nlon,ityp,nt,vt,ut,idvw,jdvw,br,bi,cr,ci,
     1           mdab,ndab,wvts,lwvts,work,lwork,ierror)
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    ut(i,j,k) = -ut(i,j,k)
	  end do
	end do
      end do
      return
      end
c
      function ui(amp,thetad)
c
c     computes the initial unrotated longitudinal velocity
c     see section 3.3.
c
      pi=4.*atan(1.)
      thetab=-pi/6.
      thetae= pi/2.
      xe=3.e-1
      x =xe*(thetad-thetab)/(thetae-thetab)
      ui = 0.
      if(x.le.0. .or. x.ge.xe) return
      ui=amp*exp(-1./x-1./(xe-x)+4./xe)
      return
      end
c
      function atanxy(x,y)
      atanxy = 0.
      if(x.eq.0. .and. y.eq.0.) return
      atanxy = atan2(y,x)
      return
      end
      subroutine sine(n,x,w)
c
c     computes the sine transform
c
      dimension x(n),w(n)
      arg = 4.*atan(1.)/(n+1)
      do 10 j=1,n
      w(j) = 0.
      do 10 i=1,n
      w(j) = w(j)+x(i)*sin(i*j*arg)
   10 continue
      do 15 i=1,n
      x(i) = 2.*w(i)
   15 continue
      return
      end
c
      function cosine(theta,n,cf)
c
c     computes the cosine transform
c
      dimension cf(n)
      cosine = 0.
      do 10 i=1,n
      cosine = cosine+cf(i)*cos(i*theta)
   10 continue
      return
      end
c
      subroutine truncw(nm,ms,id,a,b)
c
c     truncates spectral coefficients so that aliasing
c     does not occur when computing the spectral representations
c     of the product terms.
c
      dimension a(id,1),b(id,1)
      mp = ms+2
      do 10 n=mp,nm
      do 10 m=1,n
      a(m,n) = 0.
      b(m,n) = 0.
   10 continue
      return
      end

      subroutine vhaesgo(nlat,nlon,ityp,nt,u,v,iduv,jduv,
     +br,bi,cr,ci,mdab,ndab,wsav,lwsav,work,lwork,ierror)
      dimension u(iduv,jduv,*),v(iduv,jduv,*),br(mdab,ndab,*),
     +          bi(mdab,ndab,*),cr(mdab,ndab,*),ci(mdab,ndab,*),
     2          work(1),wsav(1)
c
c     vhaesgo computes the vector harmonic analysis of (u,v) using vhaes which
c     assumes the velocity components are given in mathematical coordinates
c
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    v(i,j,k) = -v(i,j,k)
	  end do
	end do
      end do
      call vhaes(nlat,nlon,ityp,nt,v,u,iduv,jduv,
     +br,bi,cr,ci,mdab,ndab,wsav,lwsav,work,lwork,ierror)
c
c     restore v
c
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    v(i,j,k) = -v(i,j,k)
	  end do
	end do
      end do
      if (ierror.ne.0) return
      return
      end

      subroutine vhsesgo(nlat,nlon,ityp,nt,u,v,iduv,jduv,
     +br,bi,cr,ci,mdab,ndab,wsav,lwsav,work,lwork,ierror)
      dimension u(iduv,jduv,*),v(iduv,jduv,*),br(mdab,ndab,*),
     +          bi(mdab,ndab,*),cr(mdab,ndab,*),ci(mdab,ndab,*),
     2          work(1),wsav(1)
c
c     vhsesgo computes a vector harmonic synthesis in (u,v) using vhses which
c     assumes the velocity components are given in mathematical coordinates
c
      call vhses(nlat,nlon,ityp,nt,v,u,iduv,jduv,
     +br,bi,cr,ci,mdab,ndab,wsav,lwsav,work,lwork,ierror)
      if (ierror.ne.0) return
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    v(i,j,k) = -v(i,j,k)
	  end do
	end do
      end do
      return
      end

      subroutine gradesgo(nlat,nlon,isym,nt,u,v,iduv,jduv,a,b,
     +mdab,ndab,wsav,lwsav,work,lwork,ierror)
      dimension u(iduv,jduv,nt),v(iduv,jduv,nt)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wsav(lwsav),work(lwork)
c
c     gradesgo computes the gradient in (u,v) using grades which assumes
c     the velocity components are given in mathematical coordinates
c
      call grades(nlat,nlon,isym,nt,v,u,iduv,jduv,a,b,
     +mdab,ndab,wsav,lwsav,work,lwork,ierror)
      if (ierror .ne.0) return
      do k=1,nt
	do j=1,nlon
	  do i=1,nlat
	    v(i,j,k) = -v(i,j,k)
	  end do
	end do
      end do
      return
      end
