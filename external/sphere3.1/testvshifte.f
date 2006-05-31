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
c ... file tvshifte.f is a test program illustrating the
c     use of subroutine vshifte (see documentation for vshifte)
c
c ... required files off spherepack3.0
c
c     hrfft.f, vshifte.f
c
c     Let the analytic vector field (u,v) in geophysical coordinates be
c     given by
c
c          u  = -exp(x)*sin(p) + exp(-z)*cos(p) + exp(y)*sin(t)*sin(p)
c
c          v  = exp(y)*cos(p) + exp(z)*cos(t) - exp(x)*sin(t)*cos(p)
c
c     where t is the latitude coordinate, p is the longitude coordinate,
c     and x=cos(t)*cos(p),y=cos(t)*sin(p),z=sin(t) are the cartesian coordinates
c     restricted to the sphere.

c     The "offset" vector field (uoff,voff) is set equal to (u,v).
c     This is transferred to the "regular" grid in (ureg,vreg).  (ureg,vreg)
c     is then compared with (u,v) on the regular grid.  Finally (ureg,vreg)
c     is transferred back to (uoff,voff) which is again compared with (u,v).
c     The least squares error after each transformation with vshifte
c     is computed and printed.  Results from running the program on
c     a 2.5 degree equally spaced regular and offset grid is given.
c     Output from runs on separate platforms with 32 bit and 64 bit
c     floating point arithmetic is listed.
c
C *********************************************
C     OUTPUT
C *********************************************
c
c       vshifte arguments
c       ioff =  0 nlon = 144 nlat =  72
c       lsave =   608 lwork = 21024
c       ier =  0
c       least squares error
C ***   32 BIT ARITHMETIC
c       err2u =  0.377E-06 err2v =  0.328E-06
C ***   64 BIT ARITHMETIC
c       err2u =  0.777E-13 err2v =  0.659E-13

c       vshifte arguments
c       ioff =  1 nlon = 144 nlat =  72
c       lsave =   608 lwork = 21024
c       ier =  0
c       least squares error
C ***   32 BIT ARITHMETIC
c       err2u =  0.557E-06 err2v =  0.434E-06
C ***   64 BIT AIRTHMETIC
c       err2u =  0.148E-12 err2v =  0.118E-12
c
C *********************************************
C     END OF OUTPUT (CODE FOLLOWS)
C *********************************************
c
      program testvshifte
      implicit none
      integer nnlon,nnlat,nnlatp1,nnlat2,llsave,llwork
c
c     set equally spaced grid sizes in nnlat,nnlon
c
      parameter(nnlon=144,nnlat=72)
c
c     set parameters which depend on nnlat,nnlon
c
      parameter(nnlatp1=nnlat+1,nnlat2=nnlat+nnlat)
c     save work space
      parameter (llsave=2*(2*nnlat+nnlon)+32)
c     unsaved work space for nnlon even
      parameter (llwork = 2*nnlon*(nnlat+1))
c     unsaved work space for nnlon odd
c     parameter (llwork = nnlon*(5*nnlat+1))
      integer ioff,nlon,nlat,nlat2,j,i,lsave,lwork,ier
      real pi,dlat,dlon,dlat2,dlon2,lat,long,x,y,z,ex,ey,ez,emz
      real err2u,err2v,ue,ve,sint,sinp,cost,cosp
      real uoff(nnlon,nnlat),voff(nnlon,nnlat)
      real ureg(nnlon,nnlatp1),vreg(nnlon,nnlatp1)
      real wsave(llsave),work(llwork)
c
c     set resolution, work space lengths, and grid increments
c
      nlat = nnlat
      nlon = nnlon
      lsave = llsave
      nlat2 = nnlat2
      lwork = llwork
      pi = 4.0*atan(1.0)
      dlat = pi/nlat
      dlon = (pi+pi)/nlon
      dlat2 = 0.5*dlat
      dlon2 = 0.5*dlon
c
c     set (uoff,voff) = (u,v) on offset grid
c
      do j=1,nlon
	long = dlon2+(j-1)*dlon
	sinp = sin(long)
	cosp = cos(long)
	do i=1,nlat
	  lat = -0.5*pi+dlat2+(i-1)*dlat
	  sint = sin(lat)
	  cost = cos(lat)
	  x = cost*cosp
	  y = cost*sinp
	  z = sint
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  uoff(j,i) =-ex*sinp+emz*cost+ey*sint*sinp
	  voff(j,i) = ey*cosp+ez*cost-ex*sint*cosp
	end do
      end do
c
c    initialize wsav for offset to regular shift
c
      ioff = 0
      call vshifti(ioff,nlon,nlat,lsave,wsave,ier)
c
c     write input arguments to vshifte
c
      write(*,100) ioff,nlon,nlat,lsave,lwork
  100 format(' vshifte arguments',
     +/' ioff = ',i2, ' nlon = ',i3,' nlat = ',i3,
     +/' lsave = ',i5, ' lwork = ',i5)
c
c     shift offset to regular grid
c
      call vshifte(ioff,nlon,nlat,uoff,voff,ureg,vreg,
     +             wsave,lsave,work,lwork,ier)
      write(*,200) ier
  200 format(' ier = ',i2)
      if (ier.eq.0) then
c
c     compute error in ureg,vreg
c
      err2u = 0.0
      err2v = 0.0
      do j=1,nlon
	long = (j-1)*dlon
	sinp = sin(long)
	cosp = cos(long)
	do i=1,nlat+1
	  lat = -0.5*pi+(i-1)*dlat
	  sint = sin(lat)
	  cost = cos(lat)
	  x = cost*cosp
	  y = cost*sinp
	  z = sint
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  ue = -ex*sinp+emz*cost+ey*sint*sinp
	  ve = ey*cosp+ez*cost-ex*sint*cosp
	  err2u = err2u + (ureg(j,i)-ue)**2
	  err2v = err2v + (vreg(j,i)-ve)**2
	end do
      end do
      err2u = sqrt(err2u/(nlon*(nlat+1)))
      err2v = sqrt(err2v/(nlon*(nlat+1)))
      write(*,300) err2u,err2v
  300 format(' least squares error ',
     +/' err2u = ',e10.3, ' err2v = ',e10.3)
      end if
c
c    initialize wsav for regular to offset shift
c
      ioff = 1
      call vshifti(ioff,nlon,nlat,lsave,wsave,ier)
c
c     transfer regular grid values in (ureg,vreg) to offset grid in (uoff,voff)
c
      do j=1,nlon
	do i=1,nlat
	  uoff(j,i) = 0.0
	  voff(j,i) = 0.0
	end do
      end do
      write(*,100) ioff,nlon,nlat,lsave,lwork
      call vshifte(ioff,nlon,nlat,uoff,voff,ureg,vreg,
     +             wsave,lsave,work,lwork,ier)
      write(*,200) ier
      if (ier .eq. 0) then
c
c     compute error in uoff,voff
c
      err2u = 0.0
      err2v = 0.0
      do j=1,nlon
	long = dlon2+(j-1)*dlon
	sinp = sin(long)
	cosp = cos(long)
	do i=1,nlat
	  lat = -0.5*pi+dlat2+(i-1)*dlat
	  sint = sin(lat)
	  cost = cos(lat)
	  x = cost*cosp
	  y = cost*sinp
	  z = sint
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  ue = -ex*sinp+emz*cost+ey*sint*sinp
	  ve = ey*cosp+ez*cost-ex*sint*cosp
	  err2u = err2u + (uoff(j,i)-ue)**2
	  err2v = err2v + (voff(j,i)-ve)**2
	end do
      end do
      err2u = sqrt(err2u/(nlon*(nlat+1)))
      err2v = sqrt(err2v/(nlon*(nlat+1)))
      write(*,300) err2u,err2v
      end if
      end
