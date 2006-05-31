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
c ... file islapgc.f
c
c     this file includes documentation and code for
c     subroutine islapgc         i
c
c ... files which must be loaded with islapgc.f
c
c     sphcom.f, hrfft.f, shagc.f, shsgc.f
c
c     subroutine islapgc(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
c    +mdab,ndab,wshsgc,lshsgc,work,lwork,pertrb,ierror)
c
c     islapgc inverts the laplace or helmholz operator on a Gaussian
c     grid using o(n**2) storage. given the
c     spherical harmonic coefficients a(m,n) and b(m,n) of the right
c     hand side slap(i,j), islapgc computes a solution sf(i,j) to
c     the following helmhotz equation :
c
c           2                2
c     [d(sf(i,j))/dlambda /sint + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c                   - xlmbda * sf(i,j) = slap(i,j)
c
c      where sf(i,j) is computed at the Gaussian colatitude point theta(i)
c      (see nlat as an input argument) and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
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
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     isym   this parameter should have the same value input to subroutine
c            shagc to compute the coefficients a and b for the scalar field
c            slap.  isym is set as follows:
c
c            = 0  no symmetries exist in slap about the equator. scalar
c                 synthesis is used to compute sf on the entire sphere.
c                 i.e., in the array sf(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c   nt       the number of solutions. in the program that calls islapgc
c            the arrays sf,a, and b can be three dimensional in which
c            case multiple solutions are computed. the third index
c            is the solution index with values k=1,...,nt.
c            for a single solution set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            and sf,a,b are two dimensional.
c
c   xlmbda   a one dimensional array with nt elements. if xlmbda is
c            is identically zero islapgc solves poisson's equation.
c            if xlmbda > 0.0 islapgc solves the helmholtz equation.
c            if xlmbda < 0.0 the nonfatal error flag ierror=-1 is
c            returned. negative xlambda could result in a division
c            by zero.
c
c   ids      the first dimension of the array sf as it appears in the
c            program that calls islapgc.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array sf as it appears in the
c            program that calls islapgc. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field slap. a,b must be computed by shagc
c            prior to calling islapgc.
c
c
c   mdab     the first dimension of the arrays a and b as it appears
c            in the program that calls islapgc.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c   ndab     the second dimension of the arrays a and b as it appears
c            in the program that calls islapgc. ndbc must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shagc to
c            compute the coefficients a and b.
c
c     wshsgc an array which must be initialized by subroutine shsgci
c            once initialized, wshsgc can be used repeatedly by islapgc
c            as long as nlon and nlat remain unchanged.  wshsgc must
c            not be altered between calls of islapgc.
c
c
c    lshsgc  the dimension of the array wshsgc as it appears in the
c            program that calls islapgc.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c               nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls islapgc. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 let
c
c               lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1).
c
c            if isym > 0 let
c
c               lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     then lwork must be greater than or equal to lwkmin (see ierror=10)
c
c     **************************************************************
c
c     output parameters
c
c
c    sf      a two or three dimensional arrays (see input parameter nt) that
c            inverts the scalar laplacian in slap.  sf(i,j) is given at
c            the colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c   pertrb  a one dimensional array with nt elements (see input 
c           parameter nt). in the discription that follows we assume 
c           that nt=1. if xlmbda > 0.0 then pertrb=0.0 is always 
c           returned because the helmholtz operator is invertible.
c           if xlmbda = 0.0 then a solution exists only if a(1,1)
c           is zero. islapgc sets a(1,1) to zero. the resulting
c           solution sf(i,j) solves poisson's equation with
c           pertrb = a(1,1)/(2.*sqrt(2.)) subtracted from the
c           right side slap(i,j).
c
c  ierror    a parameter which flags errors in input parameters as follows:
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of ids
c
c            = 6  error in the specification of jds
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lshsgc
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for islapgc
c
c **********************************************************************
c
      subroutine islapgc(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,wshsgc,lshsgc,work,lwork,pertrb,ierror)
      dimension sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wshsgc(lshsgc),work(lwork),xlmbda(nt),pertrb(nt)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if (isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. ids.lt.nlat) .or.
     1   (isym.gt.0 .and. ids.lt.imid)) return
      ierror = 6
      if(jds .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
c
c     set and verify saved work space length
c
c
      l1 = min0(nlat,(nlon+2)/2)
      l2 = (nlat+1)/2
      lwmin = nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
      if(lshsgc .lt. lwmin) return
      ierror = 10
c
c     set and verify unsaved work space length
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      mn = mmax*nlat*nt
c     lwmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwmin) return
      if (isym .eq. 0) then
	lwmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*l1*nt+1)
      else
	lwmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*l1*nt+1)
      end if
      if (lwork .lt. lwmin) return
      ierror = 0
c
c     check sign of xlmbda
c
      do  k=1,nt
	if (xlmbda(k).lt.0.0) then
	  ierror = -1
	end if
      end do
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      ifn = ib+mn
      iwk = ifn+nlat
      lwk = lwork-2*mn-nlat
      call islpgc1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,mdab,ndab,
     +work(ia),work(ib),mmax,work(ifn),wshsgc,lshsgc,work(iwk),lwk,
     +pertrb,ierror)
      return
      end

      subroutine islpgc1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,as,bs,mmax,fnn,wsav,lsav,wk,lwk,pertrb,ierror)
      dimension sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension as(mmax,nlat,nt),bs(mmax,nlat,nt),fnn(nlat)
      dimension wsav(lsav),wk(lwk),xlmbda(nt),pertrb(nt)
c
c     set multipliers and preset synthesis coefficients to zero
c
      do n=1,nlat
	fn = float(n-1)
	fnn(n) = fn*(fn+1.0)
	do m=1,mmax
	  do k=1,nt
	    as(m,n,k) = 0.0
	    bs(m,n,k) = 0.0
	  end do
	end do
      end do

      do k=1,nt
c
c     compute synthesis coefficients for xlmbda zero or nonzero
c
	if (xlmbda(k) .eq. 0.0) then
	  do n=2,nlat
	    as(1,n,k) = -a(1,n,k)/fnn(n)
	    bs(1,n,k) = -b(1,n,k)/fnn(n)
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/fnn(n)
	      bs(m,n,k) = -b(m,n,k)/fnn(n)
	    end do
	  end do
	else
c
c     xlmbda nonzero so operator invertible unless
c     -n*(n-1) = xlmbda(k) < 0.0  for some n
c
	  pertrb(k) = 0.0
	  do n=1,nlat
	    as(1,n,k) = -a(1,n,k)/(fnn(n)+xlmbda(k))
	    bs(1,n,k) = -b(1,n,k)/(fnn(n)+xlmbda(k))
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/(fnn(n)+xlmbda(k))
	      bs(m,n,k) = -b(m,n,k)/(fnn(n)+xlmbda(k))
	    end do
	  end do
	end if
      end do
c
c     synthesize as,bs into sf
c
      call shsgc(nlat,nlon,isym,nt,sf,ids,jds,as,bs,mmax,nlat,
     +           wsav,lsav,wk,lwk,ierror)
      return
      end
