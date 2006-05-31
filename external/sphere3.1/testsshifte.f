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
c ... file testsshifte.f contains a test program illustrating the
c     use of subroutine sshifte (see documentation for sshifte)
c
c ... required files off spherepack3.0
c
c     hrfft.f, sshifte.f
c
c     The "offset" scalar field goff is set equal to exp(x+y+z) where
c     x,y,z are the cartesian coordinates restricted to the surface
c     of the unit sphere.  This is transferred to the "regular" grid
c     in greg.  greg is then compared with exp(x+y+z) on the regular grid.
c     Finally greg is transferred back to goff which is again compared
c     with exp(x+y+z).  The least squares error after each transformation
c     with sshifte is computed and printed.  Output from running the
c     program below on a 2.5 degree equally spaced regular and offset
c     grid is listed below.
c
c *** OUTPUT (from execution on 32 bit machine)
c
c           sshifte arguments
c           ioff =  0 nlon = 144 nlat =  72
c           lsave =   608 lwork = 21024
c           ier =  0
c           least squares error =  0.500E-06
c           sshifte arguments
c           ioff =  1 nlon = 144 nlat =  72
c           lsave =   608 lwork = 21024
c           ier =  0
c           least squares error =  0.666E-06
c
c *** END OF OUTPUT
c
      program testsshifte
      implicit none
      integer nnlon,nnlat,nnlatp1,nnlat2,llsave,llwork
      parameter(nnlon=144,nnlat=72)
      parameter (nnlatp1 = nnlat+1, nnlat2 = nnlat+nnlat)
      parameter (llsave=2*(2*nnlat+nnlon)+32)
c     for nnlon even
      parameter (llwork = 2*(nnlat+1)*nnlon)
c     for nnlon odd
c     parameter (llwork = nnlon*(5*nnlat+1))
      integer ioff,nlon,nlat,nlat2,j,i,lsave,lwork,ier
      real pi,dlat,dlon,dlat2,dlon2,lat,long,x,y,z,gexact,err2
      real goff(nnlon,nnlat),greg(nnlon,nnlatp1)
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
c     set offset grid values in goff
c
      do j=1,nlon
	long = dlon2+(j-1)*dlon
	do i=1,nlat
	  lat = -0.5*pi + dlat2+(i-1)*dlat
	  x = cos(lat)*cos(long)
	  y = cos(lat)*sin(long)
	  z = sin(lat)
	  goff(j,i) = exp(x+y+z)
	end do
      end do
c
c    initialize wsav for offset to regular shift
c
      ioff = 0
      call sshifti(ioff,nlon,nlat,lsave,wsave,ier)
c
c     write input arguments to sshifte
c
      write(*,100) ioff,nlon,nlat,lsave,lwork
  100 format(' sshifte arguments',
     +/' ioff = ',i2, ' nlon = ',i3,' nlat = ',i3,
     +/' lsave = ',i5, ' lwork = ',i5)
c
c     shift offset to regular grid
c
      call sshifte(ioff,nlon,nlat,goff,greg,wsave,lsave,work,lwork,ier)

      write(*,200) ier
  200 format(' ier = ',i2)

      if (ier.eq.0) then
c
c     compute error in greg
c
      err2 = 0.0
      do j=1,nlon
	long = (j-1)*dlon
	do i=1,nlat+1
	  lat = -0.5*pi+(i-1)*dlat
	  x = cos(lat)*cos(long)
	  y = cos(lat)*sin(long)
	  z = sin(lat)
	  gexact = exp(x+y+z)
	  err2 = err2 + (greg(j,i)-gexact)**2
	end do
      end do
      err2 = sqrt(err2/(nlon*(nlat+1)))
      write(*,300) err2
  300 format(' least squares error = ', e10.3)
      end if
c    initialize wsav for regular to offset shift
c
      ioff = 1
      call sshifti(ioff,nlon,nlat,lsave,wsave,ier)
c
c     now transfer regular grid values in greg back to offset grid in goff
c
      do j=1,nlon
	do i=1,nlat
	  goff(j,i) = 0.0
	end do
      end do
      write(*,100) ioff,nlon,nlat,lsave,lwork
      call sshifte(ioff,nlon,nlat,goff,greg,wsave,lsave,work,lwork,ier)
      write(*,200) ier
      if (ier .eq. 0) then
c
c     compute error in goff by comparing with exp(x+y+z) on offset grid
c
      err2 = 0.0
      do j=1,nlon
	long = dlon2+(j-1)*dlon
	do i=1,nlat
	  lat = -0.5*pi+dlat2+(i-1)*dlat
	  x = cos(lat)*cos(long)
	  y = cos(lat)*sin(long)
	  z = sin(lat)
	  gexact = exp(x+y+z)
	  err2 = err2 + (goff(j,i)-gexact)**2
	end do
      end do
      err2 = sqrt(err2/(nlon*(nlat+1)))
      write(*,300) err2
      end if
      end
