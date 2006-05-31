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
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file sfvpgc.f
c
c     this file includes documentation and code for
c     subroutine sfvpgc          i
c
c ... files which must be loaded with sfvpgc.f
c
c     sphcom.f, hrfft.f, vhagc.f, shsgc.f, gaqd.f
c
c
c     subroutine sfvpgc(nlat,nlon,isym,nt,sf,vp,idv,jdv,br,bi,cr,ci,
c    +                   mdb,ndb,wshsgc,lshsgc,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients br,bi,cr,ci,
c     computed by subroutine vhagc for a vector field (v,w), sfvpgc
c     computes a scalar stream function sf and scalar velocity potential
c     vp for (v,w).  (v,w) is expressed in terms of sf and vp by the
c     helmholtz relations (in mathematical spherical coordinates):
c
c          v = -1/sint*d(vp)/dlambda + d(st)/dtheta
c
c          w =  1/sint*d(st)/dlambda + d(vp)/dtheta
c
c     where sint = sin(theta).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     br,bi,cr,ci were precomputed.  required associated legendre
c     polynomials are recomputed rather than stored as they are in
c     subroutine sfvpgs. sf(i,j) and vp(i,j) are given at the i(th)
c     gaussian colatitude point theta(i) (see nlat description below)
c     and east longitude lambda(j) = (j-1)*2*pi/nlon on the sphere.
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater than
c            3.  the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   a parameter which determines whether the stream function and
c            velocity potential are computed on the full or half sphere
c            as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case st
c            and vp are not necessarily symmetric or antisymmetric about
c            the equator.  sf and vp are computed on the entire sphere.
c            i.e., in arrays sf(i,j),vp(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case sf is symmetric and vp antisymmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the sf(i,j),vp(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then sf(i,j),vp(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            w is symmetric and v is antisymmetric about the equator.
c            in this case sf is antisymmetric and vp symmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the sf(i,j),vp(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then sf(i,j),vp(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields. arrays
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute sf,vp for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     idv    the first dimension of the arrays sf,vp as it appears in
c            the program that calls sfvpgc. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the arrays sf,vp as it appears in
c            the program that calls sfvpgc. jdv must be at least nlon.
c
c     br,bi, two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhagc.
c
c     mdb    the first dimension of the arrays br,bi,cr,ci as it
c            appears in the program that calls sfvpgc. mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br,bi,cr,ci as it
c            appears in the program that calls sfvpgc. ndb must be at
c            least nlat.
c
c     wshsgc an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc can be used repeatedly by sfvpgc
c            as long as nlon and nlat remain unchanged.  wshsgc must
c            not bel altered between calls of sfvpgc.
c
c
c     lshsgc the dimension of the array wshsgc as it appears in the
c            program that calls sfvpgc. define
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
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls sfvpgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                nlat*((nt*nlon+max0(3*l2,nlon)) + 2*l1*nt+1)
c
c            if isym is not zero then lwork must be at least
c
c                l2*(nt*nlon+max0(3*nlat,nlon)) + nlat*(2*l1*nt+1)
c
c     **************************************************************
c
c     output parameters
c
c    sf,vp  two or three dimensional arrays (see input parameter nt)
c           that contains the stream function and velocity potential
c           of the vector field (v,w) whose coefficients br,bi,cr,ci
c           where precomputed by subroutine vhagc.  sf(i,j),vp(i,j)
c           are given at the i(th) gaussian colatitude point theta(i)
c           and longitude point lambda(j) = (j-1)*2*pi/nlon.  the index
c           ranges are defined above at the input parameter isym.
c
c
c    ierror = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idv
c           = 6  error in the specification of jdv
c           = 7  error in the specification of mdb
c           = 8  error in the specification of ndb
c           = 9  error in the specification of lshsgc
c           = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
      subroutine sfvpgc(nlat,nlon,isym,nt,sf,vp,idv,jdv,br,bi,cr,ci,
     +                   mdb,ndb,wshsgc,lshsgc,work,lwork,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,lshsgc,lwork,ierror
      real sf(idv,jdv,nt),vp(idv,jdv,nt)
      real br(mdb,ndb,nt),bi(mdb,ndb,nt)
      real cr(mdb,ndb,nt),ci(mdb,ndb,nt)
      real wshsgc(lshsgc),work(lwork)
      integer imid,mmax,lzz1,labc,ls,nln,mab,mn,ia,ib,is,lwk,iwk,lwmin
      integer l1,l2
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if (nlon .lt. 4) return
      ierror = 3
      if (isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if (nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. idv.lt.nlat) .or.
     +   (isym.gt.0 .and. idv.lt.imid)) return
      ierror = 6
      if(jdv .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+2)/2)
      if(mdb .lt. min0(nlat,(nlon+1)/2)) return
      ierror = 8
      if (ndb .lt. nlat) return
      ierror = 9
c
c     verify saved work space (same as shsgc)
c
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      lwmin = lzz1+labc+nlon+15
      l2 = (nlat+1)/2
      l1 = min0((nlon+2)/2,nlat)
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
c
c     verify unsaved work space (add to what shsgc requires)
c
      ierror = 10
      ls = nlat
      if (isym.gt. 0) ls = imid
      nln = nt*ls*nlon
c
c     set first dimension for a,b (as requried by shsgc)
c
      mab = min0(nlat,nlon/2+1)
      mn = mab*nlat*nt
      if(lwork .lt. nln+max0(ls*nlon,3*nlat*imid)+2*mn+nlat) return
      ierror = 0
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      is = ib+mn
      iwk = is+nlat
      lwk = lwork-2*mn-nlat
      call sfvpgc1(nlat,nlon,isym,nt,sf,vp,idv,jdv,br,bi,cr,ci,mdb,ndb,
     +work(ia),work(ib),mab,work(is),wshsgc,lshsgc,work(iwk),lwk,
     +ierror)
      return
      end

      subroutine sfvpgc1(nlat,nlon,isym,nt,sf,vp,idv,jdv,br,bi,cr,ci,
     +mdb,ndb,a,b,mab,fnn,wshsgc,lshsgc,wk,lwk,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,mab,lshsgc,lwk,ierror
      real sf(idv,jdv,nt),vp(idv,jdv,nt)
      real br(mdb,ndb,nt),bi(mdb,ndb,nt),cr(mdb,ndb,nt),ci(mdb,ndb,nt)
      real a(mab,nlat,nt),b(mab,nlat,nt)
      real wshsgc(lshsgc),wk(lwk),fnn(nlat)
      integer n,m,mmax,k
c
c     set coefficient multiplyers
c
      do n=2,nlat
	fnn(n) = 1.0/sqrt(float(n*(n-1)))
      end do
      mmax = min0(nlat,(nlon+1)/2)
c
c     compute sf scalar coefficients from cr,ci
c
      do k=1,nt
	do n=1,nlat
	  do m=1,mab
	    a(m,n,k) = 0.0
	    b(m,n,k) = 0.0
	  end do
	end do
c
c     compute m=0 coefficients
c
	do n=2,nlat
	  a(1,n,k) =-fnn(n)*cr(1,n,k)
	  b(1,n,k) =-fnn(n)*ci(1,n,k)
	end do
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
	do m=2,mmax
	  do n=m,nlat
	    a(m,n,k) =-fnn(n)*cr(m,n,k)
	    b(m,n,k) =-fnn(n)*ci(m,n,k)
	  end do
	end do
      end do
c
c     synthesize a,b into st
c
      call shsgc(nlat,nlon,isym,nt,sf,idv,jdv,a,b,
     +           mab,nlat,wshsgc,lshsgc,wk,lwk,ierror)
c
c    set coefficients for vp from br,bi
c
      do k=1,nt
	do n=1,nlat
	  do m=1,mab
	    a(m,n,k) = 0.0
	    b(m,n,k) = 0.0
	  end do
	end do
c
c     compute m=0 coefficients
c
	do n=2,nlat
	  a(1,n,k) = fnn(n)*br(1,n,k)
	  b(1,n,k) = fnn(n)*bi(1,n,k)
	end do
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
	mmax = min0(nlat,(nlon+1)/2)
	do m=2,mmax
	  do n=m,nlat
	    a(m,n,k) = fnn(n)*br(m,n,k)
	    b(m,n,k) = fnn(n)*bi(m,n,k)
	  end do
	end do
      end do
c
c     synthesize a,b into vp
c
      call shsgc(nlat,nlon,isym,nt,vp,idv,jdv,a,b,
     +           mab,nlat,wshsgc,lshsgc,wk,lwk,ierror)
      return
      end
