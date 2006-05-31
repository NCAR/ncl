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
c ... file advec.f
c
c     subroutine advec solves the time-dependent linear advection 
c     equation for geopotential phi using the SPHEREPACK 3.0 software
c
c          d(phi)/dt = -(u,v) DOT gradient(phi)
c
c                    = -(u*gdphl + v*gdpht)
c
c ... required files
c
c     gradgc.f,shagc.f,shsgc.f,vhsgc.f,sphcom.f hrfft.f,gaqd.f
c
c
c definitions:
c
c
c     nlat          number of gaussian latitudes excluding poles
c     nlon          number of distinct longitudes
c     omega         rotation rate of earth in radians per second
c     alpha         angle between axis of rotation and the coordinate
c                   axis
c     beta          latitude of the cosine bell
c     aa            radius of earth in meters
c     ncycle        cycle number
c     time          model time in seconds
c     dt            time step
c     lambda        longitude
c     theta         latitude
c
c   the first dimension of the following two dimensional arrays
c   corresponds to the latitude index with values i=1,...,nlat
c   where i=1 is the northern most gaussian point thetag(i)
c   and i=nlat is the southern most gaussian point thetag(nlat).
c   the second dimension is longitude with values j=1,...,nlon
c   where j=1 corresponds to zero longitude and j=nlon corresponds
c   to 2pi minus 2pi/nlon.
c
c
c     thetag(i)    vector of gaussian points on the full sphere which
c                  have north to south orientation as i=1,...,nlat
c
c     u(i,j)       east longitudinal velocity component
c     v(i,j)       latitudinal velocity component
c
c     phi(i,j)     the geopotential at t = time
c
c     phnew(i,j)   the geopotential at t=time+dt
c
c     phold(i,j)   the geopotential at t=time-dt
c
c     gdphl(i,j)   the longitudinal derivative component of
c                  the gradient of phi
c
c                       gdphl = 1/(cos(theta))*d(phi)/dlambda

c
c     gdpht(i,j)   the latitudinal derivative component of
c                  the gradient of phi
c
c                       gdpht = d(phi)/dtheta

c
c   the following two dimensional arrays are nonzero in the triangle
c   n=1,...,nlat and m less than or equal to n.
c
c     ar(m,n),br(m,n)    spectral coefficients of phi
c
      program advec
c
c     set grid size with parameter statements
c
      integer nnlat,nnlon,nn15,llwork,lldwork,llvhsgc,llshagc
      integer nlat,nlon,lwork,ldwork,lvhsgc,lshagc

c     parameter (nnlat=12,nnlon=23,ddt=1200.)
      parameter (nnlat=23,nnlon=45,ddt=600.)
c     parameter (nnlat=45,nnlon=90,ddt=300.)
c
c     set saved and unsaved work space lengths in terms of nnlat,nnlon
c     (see documentation for shagc,vhsgc,vhsgci,gradgc for estimates)
c
      parameter (nn15=nnlon+15)
      parameter(llwork=4*nnlat*nnlon+2*nnlat*(nnlat+1))
      parameter (lldwork = 2*nnlat*(nnlat+1)+1 )
      parameter (llvhsgc = 7*nnlat*nnlat+nnlon+15)
      parameter (llshagc = 5*nnlat*nnlat + nnlon+15)
c
c     dimension arrays
c
      real u(nnlat,nnlon),v(nnlat,nnlon)
      real phold(nnlat,nnlon),phnew(nnlat,nnlon),phi(nnlat,nnlon)
      real pexact(nnlat,nnlon)
      real dpdt(nnlat,nnlon)
      real gdphl(nnlat,nnlon),gdpht(nnlat,nnlon), work(llwork)
      double precision dwork(lldwork)
      real wshagc(llshagc),wvhsgc(llvhsgc),wshsgc(llshagc)
      real ar(nnlat,nnlat),br(nnlat,nnlat)
      real thetag(nnlat),colat(nnlat)
      double precision dtheta(nnlat),dwts(nnlat)
c
c     set constants
c
      pi = 4.0*atan(1.0)
      omega = (pi+pi)/(12.*24.*3600.)
      p0 = 1000.
      re = 1.0/3.0
      hzero = 1000.    
      alphad = 60.
      alpha = pi*alphad/180.
      beta = pi/6.
c
c     set one array and no equatorial symmetry
c
      nt = 1
      isym = 0
c
c     set time step depending on resolution
c
      dt = ddt
      tdt = dt+dt
c
c     set work space length arguments
c
      lwork = llwork
      ldwork = lldwork
      lshagc = llshagc
      lvhsgc = llvhsgc
c
c     set grid size arguments
c
      nlat = nnlat
      nlon = nnlon
c
c     compute nlat latitudinal gaussian points in thetag  with
c     north to south orientation using gaqd from SPHEREPACK3.0
c
      call gaqd(nlat,dtheta,dwts,DWORK,ldwork,ier)
      do  i=1,nlat
	thetag(i) = 0.5*pi- dtheta(i)
	colat(i) = dtheta(i)
      end do
c
c     preset saved work spaces for gradgc and shagc and shsgc
c
      call vhsgci(nlat,nlon,wvhsgc,lvhsgc,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,10) ierror
 10   format(' error in vsgci = ',i5)
      call shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,20) ierror
 20   format(' error in shagci = ',i5)
      call shsgci(nlat,nlon,wshsgc,lshagc,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,21) ierror
 21   format(' error in shsgci = ',i5)
c
c     set vector velocities and cosine bell in geopotential
c
      ca = cos(alpha)
      sa = sin(alpha)
      dlon = (pi+pi)/nlon
      do j=1,nlon
       xlm = (j-1)*dlon
       sl = sin(xlm)
       cl = cos(xlm)
       do i=1,nlat
        st = cos(colat(i))
        ct = sin(colat(I))
        sth = ca*st+sa*ct*cl
        cthclh = ca*ct*cl-sa*st
        cthslh = ct*sl
        xlhat = atanxy(cthclh,cthslh)
        clh = cos(xlhat)
        slh = sin(xlhat)
        cth = clh*cthclh+slh*cthslh
        uhat = omega*cth
        u(i,j) = (ca*sl*slh+cl*clh)*uhat
        v(i,j) = (ca*st*cl*slh-st*sl*clh+sa*ct*slh)*uhat
       end do
      end do
c
c       compute geopotential at t=-dt in phold and at t=0.0 in phi
c       to start up leapfrog scheme
c
	call gpot(-dt,alpha,beta,omega,hzero,re,nlat,nlon,
     +                                    nlat,colat,phold)
	call gpot(0.,alpha,beta,omega,hzero,re,nlat,nlon,
     +                                    nlat,colat,phi)
c
c     smooth geopotential at t=-dt and t=0. by synthesizing after analysis
c
	call shagc(nlat,nlon,isym,nt,phold,nlat,nlon,ar,br,nlat,
     +             nlat,wshagc,lshagc,work,lwork,ierror)
	if (ierror .ne.0) write(*,26) ierror
	call shsgc(nlat,nlon,isym,nt,phold,nlat,nlon,ar,br,nlat,
     +             nlat,wshsgc,lshagc,work,lwork,ierror)
	if (ierror .ne.0) write(*,28) ierror
	call shagc(nlat,nlon,isym,nt,phi,nlat,nlon,ar,br,nlat,
     +             nlat,wshagc,lshagc,work,lwork,ierror)
	if (ierror .ne.0) write(*,26) ierror
	call shsgc(nlat,nlon,isym,nt,phi,nlat,nlon,ar,br,nlat,
     +             nlat,wshsgc,lshagc,work,lwork,ierror)
	if (ierror .ne.0) write(*,28) ierror
 28     format(' ierror in shsgc = ',i5)
c
c     compute l2 and max norms of geopotential at t=0.
c
	p2 = 0.0
	pmax = 0.0
	do j=1,nlon
	 do i=1,nlat
	  pmax = amax1(abs(phi(i,j)),pmax)
	  p2 = p2 + phi(i,j)**2
	 end do
	end do
	p2 = sqrt(p2)
c
c     set number of time steps for 12 days
c     (time to circumvent the earth)
c
      ntime = int((12.*24.*3600.)/dt+0.5)
      mprint = ntime/12
      time = 0.0
      ncycle = 0
      do k=1,ntime+1
c
c       compute harmonic coefficients for phi at current time
c
	call shagc(nlat,nlon,isym,nt,phi,nlat,nlon,ar,br,nlat,
     +             nlat,wshagc,lshagc,work,lwork,ierror)
	if (ierror .ne.0) write(*,26) ierror
 26     format(' ierror in shagc = ',i5)
c
c       compute gradient of phi at current time
c
	call gradgc(nlat,nlon,isym,nt,gdpht,gdphl,nlat,nlon,ar,br,
     +               nlat,nlat,wvhsgc,lvhsgc,work,lwork,ierror)
	if (ierror .ne.0) write(*,27) ierror
 27     format(' ierror in gradgc = ',i5)
c
c       compute the time derivative of phi, note that the sign
c       of the last term is positive because the gradient is
C       computed with respect to colatitude rather than latitude.
c
	do j=1,nlon
	  do i=1,nlat
	    dpdt(i,j) = -u(i,j)*gdphl(i,j) + v(i,j)*gdpht(i,j)
	  end do
	end do
c
      if (mod(ncycle,mprint) .eq. 0) then
c
c     write variables
c
      err2 = 0.0
      errm = 0.0
      call gpot(time,alpha,beta,omega,hzero,re,nlat,nlon,nlat,
     +          colat,pexact)
      do j=1,nlon
	do i=1,nlat
	   err2 = err2 + (pexact(i,j)-phi(i,j))**2
	   errm = amax1(abs(pexact(i,j)-phi(i,j)),errm)
	end do
      end do
      errm = errm/pmax
      err2 = sqrt(err2)/p2
      htime = time/3600.
      write(*,390) ncycle,htime,dt,nlat,nlon,omega,hzero,
     1             alphad,errm,err2
  390 format(//' advecting cosine bell, test case 2',/
     1        ,' cycle number              ',i10
     2        ,' model time in  hours      ',f10.2/
     3        ,' time step in seconds      ',f10.0
     4        ,' number of latitudes       ',i10/
     5        ,' number of longitudes      ',i10
     6        ,' rotation rate        ',1pe15.6/
     7        ,' mean height          ',1pe15.6
     8        ,' tilt angle                ',0pf10.2/
     9        ,' max geopot. error    ',1pe15.6
     1        ,' RMS geopot. error    ',1pe15.6)

      end if
	time = time + dt
	ncycle = ncycle+1
c
c       update phold,phi for next time step
c
	do j=1,nlon
	  do i=1,nlat
	    phnew(i,j) = phold(i,j) + tdt*dpdt(i,j)
	    phold(i,j) = phi(i,j)
	    phi(i,j) = phnew(i,j)
	  end do
	end do
c
c     end of time loop
c
      end do
      end
      subroutine gpot(t,alpha,beta,omega,hzero,re,nlat,nlon,idim,
     +                colat,h)
c
c     computes advecting cosine bell on a tilted grid a time t.
c    
c input parameters
c
c     t      time in seconds
c
c     alpha  tilt angle in radians
c
c     beta   colatitude of cosine bell in untilted coordinate
c            system in radians
c
c     omega  angular velocity in radians per second
c
c     hzero  maximum value of cosine bell
c
c     re     radius of support for cosine bell in radians
c
c     nlat   number of latitudes including the poles
c
c     nlon   number of distinct longitude lines
c
c     idim   first dimension of output array h
c
c     colat  vector of Gauss colatitude grid points
c
c output parameter
c
c     h      an nlat by nlon array containing the geopotential
c
c             on a tilted grid
c
      dimension h(idim,nlon),colat(nlat)
      real lambda,lambdc,lhat
      lambdc = omega*t
      call stoc(1.,beta,lambdc,xc,yc,zc)
      ca = cos(alpha)
      sa = sin(alpha)
      pi = 4.*atan(1.)
      tpi = pi+pi
      dlon = tpi/nlon
      do 10 j=1,nlon
      lambda = (j-1)*dlon
      cl = cos(lambda)
      sl = sin(lambda)
      do 10 i=1,nlat
      theta = colat(i)
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
      call stoc(1.,that,lhat,x1,y1,z1)
      dist = sqrt((x1-xc)**2+(y1-yc)**2
     1           +(z1-zc)**2)
      h(i,j) = 0.
      if(dist .ge. re) go to 10
      r = 2.*asin(dist/2.)     
      if(r .ge. re) go to 10
      h(i,j) = hzero*.5*(cos(r*pi/re)+1.)
 10   continue
      return
      end
      function atanxy(x,y)
      atanxy = 0.
      if(x.eq.0. .and. y.eq.0.) return
      atanxy = atan2(y,x)
      return
      end
      subroutine ctos(x,y,z,r,theta,phi)
      r1 = x*x+y*y
      if(r1 .ne. 0.) go to 10
      phi = 0.
      theta = 0.
      if(z .lt. 0.) theta = 4.*atan(1.)
      return
   10 r = sqrt(r1+z*z)
      r1 = sqrt(r1) 
      phi = atan2(y,x)
      theta = atan2(r1,z)
      return
      end
      subroutine stoc(r,theta,phi,x,y,z)
      st = sin(theta)
      x = r*st*cos(phi)
      y = r*st*sin(phi)
      z = r*cos(theta)
      return
      end
