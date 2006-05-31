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
c ... file vshifte.f contains code and documentation for subroutine vshifte
c     and its initialization subroutine vshifti
c
c ... required files
c
c     hrfft.f
c
c     subroutine vshifte(ioff,nlon,nlat,uoff,voff,ureg,vreg,
c    +                   wsave,lsave,work,lwork,ierror)
c
c *** purpose
c
c     subroutine vshifte does a highly accurate 1/2 grid increment shift
c     in both longitude and latitude of equally spaced vector data on the
c     sphere. data is transferred between the nlon by nlat "offset grid"
c     in (uoff,voff) (which excludes poles) and the nlon by nlat+1 "regular
c     grid" in (ureg,vreg) (which includes poles).  the transfer can go from
c     (uoff,voff) to (ureg,vreg) or vice versa (see ioff).  the grids which
c     underly the vector fields are described below.  the north and south
c     pole are at 0.5*pi and-0.5*pi radians respectively (pi=4.*atan(1.)).
c     uoff and ureg are the east longitudinal vector data components.  voff
c     and vreg are the latitudinal vector data components.
c
c     subroutine sshifte can be used to shift scalar data on the sphere.
c     notice that scalar and vector quantities are fundamentally different
c     on the sphere.  for example, vectors are discontinuous and multiple
c     valued at the poles.  scalars are continuous and single valued at the
c     poles. erroneous results would be produced if one attempted to shift
c     vector fields with subroutine sshifte applied to each component of
c     of the vector.
c
c *** grid descriptions
c
c     let dlon = (pi+pi)/nlon and dlat = pi/nlat be the uniform grid
c     increments in longitude and latitude
c
c     offset grid
c
c     the "1/2 increment offset" grid (long(j),lat(i)) on which uoff(j,i)
c     and voff(j,i) are given (ioff=0) or generated (ioff=1) is
c
c          long(j) =0.5*dlon + (j-1)*dlon  (j=1,...,nlon)
c
c     and
c
c          lat(i) = -0.5*pi + 0.5*dlat + (i-1)*dlat (i=1,...,nlat)
c
c     the data in (uoff,voff) is "shifted" one half a grid increment in both
c     longitude and latitude and excludes the poles.  each uoff(j,1),voff(j,1)
c     is given at latitude -pi/2+dlat/2.  uoff(j,nlat),voff(j,nlat) is
c     given at pi/2-dlat/2 (1/2 a grid increment away from the poles).
c     uoff(1,i),voff(1,i) is given at longitude dlon/2.  each uoff(nlon,i),
c     voff(nlon,i) is given at longitude 2*pi-dlon/2.
c
c     regular grid
c
c     let dlat,dlon be as above.  then the nlon by nlat+1 grid on which
c     ureg(j,i),vreg(j,i) are generated (ioff=0) or given (ioff=1) is
c
c          lone(j) = (j-1)*dlon (j=1,...,nlon)
c
c      and
c
c          late(i) = -0.5*pi + (i-1)*dlat (i=1,...,nlat+1)
c
c     values in ureg,vreg include the poles and start at zero degrees
c     longitude and at the south pole this is the "usual" equally spaced
c     grid in geophysical coordinates.
c
c *** remark
c
c     subroutine vshifte can be used in conjunction with subroutine trvsph
c     when transferring vector data from an equally spaced "1/2 increment
c     offset" grid to a gaussian or equally spaced grid (which includes poles)
c     of any resolution.  this problem (personal communication with dennis
c     shea) is encountered in geophysical modeling and data analysis.
c
c *** method
c
c     fast fourier transform software from spherepack2 and trigonometric
c     identities are used to accurately "shift" periodic vectors half a
c     grid increment in latitude and longitude.  latitudinal shifts are
c     accomplished by setting periodic 2*nlat vectors over the pole for each
c     longitude.  vector values must be negated on one side of the pole
c     to maintain periodicity prior to the 2*nlat shift over the poles.
c     when nlon is odd, the 2*nlat latitudinal shift requires an additional
c     longitude shift to obtain symmetry necessary for full circle shifts
c     over the poles.  finally longitudinal shifts are executed for each
c     shifted latitude.
c
c *** argument description
c
c ... ioff
c
c     ioff = 0 if values on the offset grid in (uoff,voff) are given and
c              values on the regular grid in (ureg,vreg) are to be generated.
c
c     ioff = 1 if values on the regular grid in (ureg,vreg) are given and
c              values on the offset grid in (uoff,voff) are to be generated.
c
c ... nlon
c
c     the number of longitude points on both the "offset" and "regular"
c     uniform grid in longitude (see "grid description" above).  nlon
c     is also the first dimension of uoff,voff,ureg,vreg.  nlon determines
c     the grid increment in longitude as dlon = 2.*pi/nlon.  for example,
c     nlon = 144 for a 2.5 degree grid.  nlon can be even or odd and must
c     be greater than or equal to 4.  the efficiency of the computation
c     is improved when nlon is a product of small primes.
c
c ... nlat
c
c     the number of latitude points on the "offset" uniform grid.  nlat+1
c     is the number of latitude points on the "regular" uniform grid (see
c     "grid description" above).  nlat is the second dimension of uoff,voff.
c     nlat+1 must be the second dimension of ureg,vreg in the program
c     calling vshifte.  nlat determines the grid in latitude as pi/nlat.
c     for example, nlat = 36 for a five degree grid.  nlat must be at least 3.
c
c ... uoff
c
c     a nlon by nlat array that contains the east longitudinal vector
c     data component on the offset grid described above.  uoff is a
c     given input argument if ioff=0.  uoff is a generated output
c     argument if ioff=1.
c
c ... voff
c
c     a nlon by nlat array that contains the latitudinal vector data
c     component on the offset grid described above.  voff is a given
c     input argument if ioff=0.  voff is a generated output argument
c     if ioff=1.
c
c ... ureg
c
c     a nlon by nlat+1 array that contains the east longitudinal vector
c     data component on the regular grid described above.  ureg is a given
c     input argument if ioff=1.  ureg is a generated output argument
c     if ioff=0.
c
c ... vreg
c
c     a nlon by nlat+1 array that contains the latitudinal vector data
c     component on the regular grid described above.  vreg is a given
c     input argument if ioff=1.  vreg is a generated output argument
c     if ioff=0.
c
c ... wsav
c
c     a real saved work space array that must be initialized by calling
c     subroutine vshifti(ioff,nlon,nlat,wsav,ier) before calling vshifte.
c     wsav can then be used repeatedly by vshifte as long as ioff, nlon,
c     and nlat do not change.  this bypasses redundant computations and
c     saves time.  undetectable errors will result if vshifte is called
c     without initializing wsav whenever ioff, nlon, or nlat change.
c
c ... lsav
c
c     the length of the saved work space wsav in the routine calling vshifte
c     and sshifti.  lsave must be greater than or equal to 2*(2*nlat+nlon+16).
c
c ... work
c
c     a real unsaved work space
c
c ... lwork
c
c     the length of the unsaved work space in the routine calling vshifte
c     if nlon is even then lwork must be greater than or equal to
c
c          2*nlon*(nlat+1)
c
c     if nlon is odd then lwork must be greater than or equal to
c
c          nlon*(5*nlat+1)
c
c ... ier
c
c     indicates errors in input parameters
c
c     = 0 if no errors are detected
c
c     = 1 if ioff is not equal to 0 or 1
c
c     = 2 if nlon < 4
c
c     = 3 if nlat < 3
c
c     = 4 if lsave < 2*(nlon+2*nlat)+32
c
c     = 5 if lwork < 2*nlon*(nlat+1) for nlon even or
c            lwork < nlon*(5*nlat+1) for nlon odd
c
c *** end of vshifte documentation
c
c     subroutine vshifti(ioff,nlon,nlat,lsav,wsav,ier)
c
c     subroutine vshifti initializes the saved work space wsav
c     for ioff and nlon and nlat (see documentation for vshifte).
c     vshifti must be called before vshifte whenever ioff or nlon
c     or nlat change.
c
c ... ier
c
c     = 0 if no errors with input arguments
c
c     = 1 if ioff is not 0 or 1
c
c     = 2 if nlon < 4
c
c     = 3 if nlat < 3
c
c     = 4 if lsav < 2*(2*nlat+nlon+16)
c
c *** end of vshifti documentation
c
      subroutine vshifte(ioff,nlon,nlat,uoff,voff,ureg,vreg,
     +                   wsav,lsav,wrk,lwrk,ier)
      implicit none
      integer ioff,nlon,nlat,n2,nr,nlat2,nlatp1,lsav,lwrk,ier
      integer i1,i2,i3
      real uoff(nlon,nlat),voff(nlon,nlat)
      real ureg(nlon,*),vreg(nlon,*)
      real wsav(lsav),wrk(lwrk)
c
c     check input parameters
c
      ier = 1
      if (ioff*(ioff-1).ne.0) return
      ier = 2
      if (nlon.lt.4) return
      ier = 3
      if (nlat .lt. 3) return
      ier = 4
      if (lsav .lt. 2*(2*nlat+nlon+16)) return
      nlat2 = nlat+nlat
      nlatp1 = nlat+1
      n2 = (nlon+1)/2
      ier = 5
      if (2*n2 .eq. nlon) then
	if (lwrk .lt. 2*nlon*(nlat+1)) return
	nr = n2
	i1 = 1
	i2 = 1
	i3 = i2+nlon*nlatp1
      else
	if (lwrk .lt. nlon*(5*nlat+1)) return
	nr = nlon
	i1 = 1
	i2 = i1+nlat2*nlon
	i3 = i2+nlatp1*nlon
      end if
      ier = 0
      if (ioff.eq.0) then
c
c     shift (uoff,voff) to (ureg,vreg)
c
	call vhftoff(nlon,nlat,uoff,ureg,wsav,nr,nlat2,
     +  nlatp1,wrk(i1),wrk(i2),wrk(i2),wrk(i3))
	call vhftoff(nlon,nlat,voff,vreg,wsav,nr,nlat2,
     +  nlatp1,wrk(i1),wrk(i2),wrk(i2),wrk(i3))
      else
c
c     shift (ureg,vreg) to (uoff,voff)
c
	call vhftreg(nlon,nlat,uoff,ureg,wsav,nr,nlat2,
     +  nlatp1,wrk(i1),wrk(i2),wrk(i2),wrk(i3))
	call vhftreg(nlon,nlat,voff,vreg,wsav,nr,nlat2,
     +  nlatp1,wrk(i1),wrk(i2),wrk(i2),wrk(i3))
      end if
      end

      subroutine vhftoff(nlon,nlat,uoff,ureg,wsav,nr,
     +nlat2,nlatp1,rlatu,rlonu,rlou,wrk)
c
c     generate ureg from uoff (a vector component!)
c
      implicit none
      integer nlon,nlat,nlat2,nlatp1,n2,nr,j,i,js,isav
      real uoff(nlon,nlat),ureg(nlon,nlatp1)
      real rlatu(nr,nlat2),rlonu(nlatp1,nlon),rlou(nlat,nlon)
      real wsav(*),wrk(*)
      isav = 4*nlat+17
      n2 = (nlon+1)/2
c
c     execute full circle latitude shifts for nlon odd or even
c
      if (2*n2 .gt. nlon) then
c
c     odd number of longitudes
c
	do i=1,nlat
	  do j=1,nlon
	    rlou(i,j) = uoff(j,i)
	  end do
	end do
c
c       half shift in longitude
c
	call vhifth(nlat,nlon,rlou,wsav(isav),wrk)
c
c       set full 2*nlat circles in rlatu using shifted values in rlonu
c
	do j=1,n2-1
	  js = j+n2
	  do i=1,nlat
	    rlatu(j,i)      = uoff(j,i)
	    rlatu(j,nlat+i) = -rlou(nlat+1-i,js)
	  end do
	end do
	do j=n2,nlon
	  js = j-n2+1
	  do i=1,nlat
	    rlatu(j,i)      = uoff(j,i)
	    rlatu(j,nlat+i) = -rlou(nlat+1-i,js)
	  end do
	end do
c
c       shift the nlon rlat vectors one half latitude grid
c
	call vhifth(nlon,nlat2,rlatu,wsav,wrk)
c
c       set in ureg
c
	do j=1,nlon
	  do i=1,nlat+1
	    ureg(j,i) =  rlatu(j,i)
	  end do
	end do
      else
c
c     even number of longitudes (no initial longitude shift necessary)
c     set full 2*nlat circles (over poles) for each longitude pair (j,js)
c     negating js vector side for periodicity
c
	do j=1,n2
	  js = n2+j
	  do i=1,nlat
	    rlatu(j,i)      = uoff(j,i)
	    rlatu(j,nlat+i) =-uoff(js,nlatp1-i)
	  end do
	end do
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
	call vhifth(n2,nlat2,rlatu,wsav,wrk)
c
c       set ureg,vreg shifted in latitude
c
	do j=1,n2
	  js = n2+j
	  ureg(j,1) =   rlatu(j,1)
	  ureg(js,1) = -rlatu(j,1)
	  do i=2,nlatp1
	    ureg(j,i) =  rlatu(j,i)
	    ureg(js,i) =-rlatu(j,nlat2-i+2)
	  end do
	end do
      end if
c
c     execute full circle longitude shift
c
      do j=1,nlon
	do i=1,nlatp1
	  rlonu(i,j) = ureg(j,i)
	end do
      end do
      call vhifth(nlatp1,nlon,rlonu,wsav(isav),wrk)
      do j=1,nlon
	do i=1,nlatp1
	  ureg(j,i) = rlonu(i,j)
	end do
      end do
      end

      subroutine vhftreg(nlon,nlat,uoff,ureg,wsav,nr,nlat2,
     +                   nlatp1,rlatu,rlonu,rlou,wrk)
c
c     generate uoff vector component from ureg
c
      implicit none
      integer nlon,nlat,nlat2,nlatp1,n2,nr,j,i,js,isav
      real uoff(nlon,nlat),ureg(nlon,nlatp1)
      real rlatu(nr,nlat2),rlonu(nlatp1,nlon),rlou(nlat,nlon)
      real wsav(*),wrk(*)
      isav = 4*nlat+17
      n2 = (nlon+1)/2
c
c     execute full circle latitude shifts for nlon odd or even
c
      if (2*n2 .gt. nlon) then
c
c     odd number of longitudes
c
	do i=1,nlatp1
	  do j=1,nlon
	    rlonu(i,j) = ureg(j,i)
	  end do
	end do
c
c       half shift in longitude in rlon
c
	call vhifth(nlatp1,nlon,rlonu,wsav(isav),wrk)
c
c       set full 2*nlat circles in rlat using shifted values in rlon
c
	do j=1,n2
	  js = j+n2-1
	  rlatu(j,1) = ureg(j,1)
	  do i=2,nlat
	    rlatu(j,i) = ureg(j,i)
	    rlatu(j,nlat+i) =-rlonu(nlat+2-i,js)
	  end do
	  rlatu(j,nlat+1) = ureg(j,nlat+1)
	end do
	do j=n2+1,nlon
	  js = j-n2
	  rlatu(j,1) = ureg(j,1)
	  do i=2,nlat
	    rlatu(j,i) = ureg(j,i)
	    rlatu(j,nlat+i) =-rlonu(nlat+2-i,js)
	  end do
	  rlatu(j,nlat+1) = ureg(j,nlat+1)
	end do
c
c       shift the nlon rlat vectors one halflatitude grid
c
	call vhifth(nlon,nlat2,rlatu,wsav,wrk)
c
c       set values in uoff
c
	do j=1,nlon
	  do i=1,nlat
	    uoff(j,i) = rlatu(j,i)
	  end do
	end do
      else
c
c     even number of longitudes (no initial longitude shift necessary)
c     set full 2*nlat circles (over poles) for each longitude pair (j,js)
c
	do j=1,n2
	  js = n2+j
	  rlatu(j,1) = ureg(j,1)
	  do i=2,nlat
	    rlatu(j,i) = ureg(j,i)
	    rlatu(j,nlat+i) =-ureg(js,nlat+2-i)
	  end do
	  rlatu(j,nlat+1) = ureg(j,nlat+1)
	end do
c
c       shift the n2=(nlon+1)/2 rlat vectors one half latitude grid
c
	call vhifth(n2,nlat2,rlatu,wsav,wrk)
c
c       set values in uoff
c
	do j=1,n2
	  js = n2+j
	  do i=1,nlat
	    uoff(j,i) =  rlatu(j,i)
	    uoff(js,i) =-rlatu(j,nlat2+1-i)
	  end do
	end do
      end if
c
c     execute full circle longitude shift for all latitude circles
c
      do j=1,nlon
	do i=1,nlat
	  rlou(i,j) = uoff(j,i)
	end do
      end do
      call vhifth(nlat,nlon,rlou,wsav(isav),wrk)
      do j=1,nlon
	do i=1,nlat
	  uoff(j,i) = rlou(i,j)
	end do
      end do
      end

      subroutine vshifti(ioff,nlon,nlat,lsav,wsav,ier)
c
c     initialize wsav for vshifte
c
      integer ioff,nlat,nlon,nlat2,isav,ier
      real wsav(lsav)
      real pi,dlat,dlon,dp
      ier = 1
      if (ioff*(ioff-1).ne.0) return
      ier = 2
      if (nlon .lt. 4) return
      ier = 3
      if (nlat .lt. 3) return
      ier = 4
      if (lsav .lt. 2*(2*nlat+nlon+16)) return
      ier = 0
      pi = 4.0*atan(1.0)
c
c     set lat,long increments
c
      dlat = pi/nlat
      dlon = (pi+pi)/nlon
c
c     set left or right latitude shifts
c
      if (ioff.eq.0) then
	dp = -0.5*dlat
      else
	dp = 0.5*dlat
      end if
      nlat2 = nlat+nlat
      call vhifthi(nlat2,dp,wsav)
c
c     set left or right longitude shifts
c
      if (ioff.eq.0) then
	dp = -0.5*dlon
      else
	dp = 0.5*dlon
      end if
      isav = 4*nlat + 17
      call vhifthi(nlon,dp,wsav(isav))
      return
      end

      subroutine vhifth(m,n,r,wsav,work)
      implicit none
      integer m,n,n2,k,l
      real r(m,n),wsav(*),work(*),r2km2,r2km1
      n2 = (n+1)/2
c
c     compute fourier coefficients for r on shifted grid
c
      call hrfftf(m,n,r,m,wsav(n+2),work)
      do l=1,m
	do k=2,n2
	  r2km2 = r(l,k+k-2)
	  r2km1 = r(l,k+k-1)
	  r(l,k+k-2)   =  r2km2*wsav(n2+k) - r2km1*wsav(k)
	  r(l,k+k-1)   =  r2km2*wsav(k)    + r2km1*wsav(n2+k)
	end do
      end do
c
c     shift r with fourier synthesis and normalization
c
      call hrfftb(m,n,r,m,wsav(n+2),work)
      do l=1,m
	do k=1,n
	  r(l,k) = r(l,k)/n
	end do
      end do
      return
      end

      subroutine vhifthi(n,dp,wsav)
c
c     initialize wsav for subroutine vhifth
c
      implicit none
      integer n,n2,k
      real wsav(*),dp
      n2 = (n+1)/2
      do k=2,n2
	wsav(k)    =  sin((k-1)*dp)
	wsav(k+n2) =  cos((k-1)*dp)
      end do
      call hrffti(n,wsav(n+2))
      return
      end
