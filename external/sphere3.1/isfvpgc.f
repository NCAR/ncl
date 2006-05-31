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
c ... file isfvpgc.f
c
c     this file includes documentation and code for
c     subroutine isfvpgc          i
c
c ... files which must be loaded with isfvpgc.f
c
c     sphcom.f, hrfft.f, vhsgc.f, shagc.f, gaqd.f
c
c
c     subroutine isfvpgc(nlat,nlon,isym,nt,sf,vp,idv,jdv,as,bs,av,bv,
c    +                   mdb,ndb,wvhsgc,lvhsgc,work,lwork,ierror)
c
c     given the scalar spherical harmonic coefficients as,bs precomputed
c     by shagc for the scalar stream function sf and av,bv precomputed by
c     shagc for the scalar velocity potenital vp, subroutine isfvpgc computes
c     the vector field (v,w) corresponding to sf and vp.  w is the east
c     longitudinal and v is the colatitudinal component of the vector field.
c     (v,w) is expressed in terms of sf,vp by the helmholtz relations (in
c     mathematical spherical coordinates):
c
c          v = -1/sin(theta)*d(vp)/dlambda + d(st)/dtheta
c
c          w =  1/sin(theta)*d(st)/dlambda + d(vp)/dtheta
c
c     required legendre functions are recomputed rather than stored as
c     they are in subroutine isfvpgs.  v(i,j) and w(i,j) are given at
c     the i(th) gaussian colatitude point (see gaqd) theta(i) and east
c     longitude lambda(j) = (j-1)*2.*pi/nlon on the sphere.
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
c            the program that calls isfvpgc. if isym = 0 then idv
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idv must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idv must be at least (nlat+1)/2.
c
c     jdv    the second dimension of the arrays v,w as it appears in
c            the program that calls isfvpgc. jdv must be at least nlon.
c
c     as,bs  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field sf as computed by subroutine shagc.
c
c     av,bv  two or three dimensional arrays (see input parameter nt)
c            that contain the spherical harmonic coefficients of
c            the scalar field vp as computed by subroutine shagc.
c
c     mdb    the first dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpgc. mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays as,bs,av,bv as it
c            appears in the program that calls isfvpgc. ndb must be at
c            least nlat.
c
c     wvhsgc an array which must be initialized by subroutine vhsgci.
c            once initialized, wvhsgc can be used repeatedly by isfvpgc
c            as long as nlon and nlat remain unchanged.  wvhsgc must
c            not bel altered between calls of isfvpgc.
c
c
c     lvhsgc the dimension of the array wvhsgc as it appears in the
c            program that calls isfvpgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgc must be at least
c
c
c               4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls isfvpgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c
c            if isym = 0 then lwork must be at least
c
c               nlat*(2*nt*nlon+max0(6*l2,nlon)+4*l1*nt+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(4*l1*nt+1)
c
c     **************************************************************
c
c     output parameters
c
c    v,w    two or three dimensional arrays (see input parameter nt)
c           that contains the vector field corresponding to the stream
c           function sf and velocity potential vp whose coefficients,
c           as,bs (for sf) and av,bv (for vp), were precomputed by
c           subroutine shagc.  v(i,j) and w(i,j) are given at the
c           i(th) gaussian colatitude point theta(i) and east longitude
c           point lambda(j) = (j-1)*2*pi/nlon.  the index ranges are
c           defined above at the input parameter isym.
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
c           = 9  error in the specification of lvhsgc
c           = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
      subroutine isfvpgc(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,
     +                   mdb,ndb,wvhsgc,lvhsgc,work,lwork,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,lvhsgc,lwork,ierror
      real v(idv,jdv,nt),w(idv,jdv,nt)
      real as(mdb,ndb,nt),bs(mdb,ndb,nt)
      real av(mdb,ndb,nt),bv(mdb,ndb,nt)
      real wvhsgc(lvhsgc),work(lwork)
      integer l1,l2,mn,is,lwk,iwk,lwmin
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
      lwmin = 4*nlat*l2+3*max0(l1-2,0)*(nlat+nlat-l1+1)+nlon+15
      if (lvhsgc .lt. lwmin) return
      ierror = 10
      if (isym .eq. 0) then
	lwmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+4*l1*nt+1)
      else
	lwmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(4*l1*nt+1)
      end if
      if (lwork .lt. lwmin) return
c
c     set first dimension for br,bi,cr,ci (as requried by vhsgc)
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
      call isfvpgc1(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,mdb,
     +ndb,work(ibr),work(ibi),work(icr),work(ici),l1,work(is),
     +wvhsgc,lvhsgc,work(iwk),lwk,ierror)
      return
      end

      subroutine isfvpgc1(nlat,nlon,isym,nt,v,w,idv,jdv,as,bs,av,bv,
     +mdb,ndb,br,bi,cr,ci,mab,fnn,wvhsgc,lvhsgc,wk,lwk,ierror)
      implicit none
      integer nlat,nlon,isym,nt,idv,jdv,mdb,ndb,mab,lvhsgc,lwk,ierror
      real v(idv,jdv,nt),w(idv,jdv,nt)
      real as(mdb,ndb,nt),bs(mdb,ndb,nt)
      real av(mdb,ndb,nt),bv(mdb,ndb,nt)
      real br(mab,nlat,nt),bi(mab,nlat,nt)
      real cr(mab,nlat,nt),ci(mab,nlat,nt)
      real wvhsgc(lvhsgc),wk(lwk),fnn(nlat)
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
      call vhsgc(nlat,nlon,ityp,nt,v,w,idv,jdv,br,bi,cr,ci,
     +           mab,nlat,wvhsgc,lvhsgc,wk,lwk,ierror)
      return
      end
