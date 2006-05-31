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
c ... file isfvpes.f
c
c     this file includes documentation and code for
c     subroutine isfvpes          i
c
c ... files which must be loaded with isfvpes.f
c
c     sphcom.f, hrfft.f, vhses.f,shaes.f
c
c
c     subroutine isfvpes(nlat,nlon,isym,nt,sf,vp,idv,jdv,as,bs,av,bv,
c    +                   mdb,ndb,wvhses,lvhses,work,lwork,ierror)
c
c     given the scalar spherical harmonic coefficients as,bs precomputed
c     by shaes for the scalar stream function sf and av,bv precomputed by
c     shaes for the scalar velocity potenital vp, subroutine isfvpes computes
c     the vector field (v,w) corresponding to sf and vp.  w is the east
c     longitudinal and v is the colatitudinal component of the vector field.
c     (v,w) is expressed in terms of sf,vp by the helmholtz relations (in
c     mathematical spherical coordinates):
c
c          v = -1/sin(theta)*d(vp)/dlambda + d(st)/dtheta
c
c          w =  1/sin(theta)*d(st)/dlambda + d(vp)/dtheta
c
c     required legendre functions are stored rather than recomputed as
c     they are in subroutine isfvpes.  v(i,j) and w(i,j) are given at
c     colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and east longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     on the sphere (pi=4.0*atan(1.0)).
c
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater than
c            3.  the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   a parameter which determines whether the vector field is
c            computed on the full or half sphere as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in sf,vp about the equator.  in this case v
c            and w are not necessarily symmetric or antisymmetric about
c            equator.  v and w are computed on the entire sphere.
c            i.e., in arrays sf(i,j),vp(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            vp is antisymmetric and sf is symmetric about the equator.
c            in this case v is symmetric and w antisymmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the v(i,j),w(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then v(i,j),w(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            vp is symmetric and sf is antisymmetric about the equator.
c            in this case v is antisymmetric and w symmetric about
c            the equator and are computed for the northern hemisphere
c            only.  i.e., if nlat is odd the v(i,j),w(i,j) are computed
c            for i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is
c            even then v(i,j),w(i,j) are computed for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields. arrays
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple vector synthesis will
c            be performed to compute (v,w) for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     idv    the first dimension of the arrays v,w as it appears in
c            the program that calls isfvpes. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the arrays v,w as it appears in
c            the program that calls isfvpes. jdv must be at least nlon.
c
c     as,bs  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field sf as computed by subroutine shaes.
c
c     av,bv  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field vp as computed by subroutine shaes.
c
c     mdb    the first dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpes. mdb must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpes. ndb must be at
c            least nlat.
c
c     wvhses an array which must be initialized by subroutine vhsesi.
c            once initialized, wvhses can be used repeatedly by isfvpes
c            as long as nlon and nlat remain unchanged.  wvhses must
c            not bel altered between calls of isfvpes.
c
c
c     lvhses the dimension of the array wvhses as it appears in the
c            program that calls isfvpes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhses must be at least
c
c               l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls isfvpes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym=0 then lwork must be at least
c
c                       nlat*((2*nt+1)*nlon + 4*l1*nt + 1)
c
c            if isym=1 or 2 then lwork must be at least
c
c                       (2*nt+1)*l2*nlon + nlat*(4*l1*nt + 1)
c
c
c     **************************************************************
c
c     output parameters
c
c    v,w    two or three dimensional arrays (see input parameter nt)
c           that contains the vector field corresponding to the stream
c           function sf and velocity potential vp whose coefficients,
c           as,bs (for sf) and av,bv (for vp), were precomputed by
c           subroutine shaes.  v(i,j) and w(i,j) are given at the
c           colatitude point
c
c                theta(i) = (i-1)*pi/(nlat-1)
c
c           and longitude point
c
c                lambda(j) = (j-1)*2*pi/nlon
c
c           the index ranges are defined above at the input parameter isym.
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
c           = 9  error in the specification of lvhses
c           = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
      subroutine isfvpes(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,
     +                   mdb,ndb,wvhses,lvhses,work,lwork,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,lvhses,lwork,ierror
      real v(idv,jdv,nt),w(idv,jdv,nt)
      real as(mdb,ndb,nt),bs(mdb,ndb,nt)
      real av(mdb,ndb,nt),bv(mdb,ndb,nt)
      real wvhses(lvhses),work(lwork)
      integer l1,l2,mn,is,lwk,iwk
      integer ibr,ibi,icr,ici
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
      l2 = (nlat+1)/2
      if((isym.eq.0 .and. idv.lt.nlat) .or.
     +   (isym.gt.0 .and. idv.lt.l2)) return
      ierror = 6
      if(jdv .lt. nlon) return
      ierror = 7
      l1 = min0(nlat,(nlon+1)/2)
      if (mdb .lt. min0(nlat,(nlon+2)/2)) return
      ierror = 8
      if (ndb .lt. nlat) return
      ierror = 9
      l1 = min0(nlat,(nlon+2)/2)
      if (lvhses .lt. l1*l2*(nlat+nlat-l1+1)+nlon+15) return
      ierror = 10
      if (isym.eq.0) then
	if (lwork .lt. nlat*((2*nt+1)*nlon+4*l1*nt+1)) return
      else
	if (lwork .lt. (2*nt+1)*nlon+nlat*(4*l1*nt+1)) return
      end if
c
c     set first dimension for br,bi,cr,ci (as requried by vhses)
c
      mn = l1*nlat*nt
      ierror = 0
c
c     set work space pointers
c
      ibr = 1
      ibi = ibr+mn
      icr = ibi+mn
      ici = icr+mn
      is = ici+mn
      iwk = is+nlat
      lwk = lwork-4*mn-nlat
      call isfvpes1(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,mdb,
     +ndb,work(ibr),work(ibi),work(icr),work(ici),l1,work(is),
     +wvhses,lvhses,work(iwk),lwk,ierror)
      return
      end

      subroutine isfvpes1(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,
     +mdb,ndb,br,bi,cr,ci,mab,fnn,wvhses,lvhses,wk,lwk,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,mab,lvhses,lwk,ierror
      real v(idv,jdv,nt),w(idv,jdv,nt)
      real as(mdb,ndb,nt),bs(mdb,ndb,nt)
      real av(mdb,ndb,nt),bv(mdb,ndb,nt)
      real br(mab,nlat,nt),bi(mab,nlat,nt)
      real cr(mab,nlat,nt),ci(mab,nlat,nt)
      real wvhses(lvhses),wk(lwk),fnn(nlat)
      integer n,m,mmax,k,ityp
c
c     set coefficient multiplyers
c
      do n=2,nlat
	fnn(n) = -sqrt(float(n*(n-1)))
      end do
      mmax = min0(nlat,(nlon+1)/2)
c
c     compute (v,w) coefficients from as,bs,av,bv
c
      do k=1,nt
	do n=1,nlat
	  do m=1,mab
	    br(m,n,k) = 0.0
	    bi(m,n,k) = 0.0
	    cr(m,n,k) = 0.0
	    ci(m,n,k) = 0.0
	  end do
	end do
c
c     compute m=0 coefficients
c
	do n=2,nlat
	  br(1,n,k) = -fnn(n)*av(1,n,k)
	  bi(1,n,k) = -fnn(n)*bv(1,n,k)
	  cr(1,n,k) =  fnn(n)*as(1,n,k)
	  ci(1,n,k) =  fnn(n)*bs(1,n,k)
	end do
c
c     compute m>0 coefficients using vector spherepack value for mmax
c
	do m=2,mmax
	  do n=m,nlat
	    br(m,n,k) = -fnn(n)*av(m,n,k)
	    bi(m,n,k) = -fnn(n)*bv(m,n,k)
	    cr(m,n,k) =  fnn(n)*as(m,n,k)
	    ci(m,n,k) =  fnn(n)*bs(m,n,k)
	  end do
	end do
      end do
c
c     synthesize br,bi,cr,ci into (v,w)
c
      if (isym .eq.0) then
	ityp = 0
      else if (isym .eq.1) then
	ityp = 3
      else if (isym .eq.2) then
	ityp = 6
      end if
      call vhses(nlat,nlon,ityp,nt,v,w,idv,jdv,br,bi,cr,ci,
     +           mab,nlat,wvhses,lvhses,wk,lwk,ierror)
      return
      end
