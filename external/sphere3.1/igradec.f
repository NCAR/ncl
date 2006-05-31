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
c ... file igradec.f
c
c     this file includes documentation and code for
c     subroutine igradec         i
c
c ... files which must be loaded with igradec.f
c
c     sphcom.f, hrfft.f, shsec.f,vhaec.f
c
c     subroutine igradec(nlat,nlon,isym,nt,sf,isf,jsf,br,bi,mdb,ndb,
c    +                   wshsec,lshsec,work,lwork,ierror)
c
c     let br,bi,cr,ci be the vector spherical harmonic coefficients
c     precomputed by vhaec for a vector field (v,w).  let (v',w') be
c     the irrotational component of (v,w) (i.e., (v',w') is generated
c     by assuming cr,ci are zero and synthesizing br,bi with vhsec).
c     then subroutine igradec computes a scalar field sf such that
c
c            gradient(sf) = (v',w').
c
c     i.e.,
c
c            v'(i,j) = d(sf(i,j))/dtheta          (colatitudinal component of
c                                                 the gradient)
c     and
c
c            w'(i,j) = 1/sint*d(sf(i,j))/dlambda  (east longitudinal component
c                                                 of the gradient)
c
c     at colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     where sint = sin(theta(i)).  required associated legendre polynomials
c     are recomputed rather than stored as they are in subroutine igrades. this
c     saves storage (compare lshsec and lshses in igrades) but increases
c     computational requirements.
c
c     note:  for an irrotational vector field (v,w), subroutine igradec
c     computes a scalar field whose gradient is (v,w).  in ay case,
c     subroutine igradec "inverts" the gradient subroutine gradec.
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
c     isym   a parameter which determines whether the scalar field sf is
c            computed on the full or half sphere as follows:
c
c      = 0
c
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case sf
c            is neither symmetric nor antisymmetric about the equator.
c            sf is computed on the entire sphere.  i.e., in the array
c            sf(i,j) for i=1,...,nlat and  j=1,...,nlon
c
c      = 1
c
c            w is antisymmetric and v is symmetric about the equator.
c            in this case sf is antisymmetyric about the equator and
c            is computed for the northern hemisphere only.  i.e.,
c            if nlat is odd sf is computed in the array sf(i,j) for
c            i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is even
c            sf is computed in the array sf(i,j) for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c      = 2
c
c            w is symmetric and v is antisymmetric about the equator.
c            in this case sf is symmetyric about the equator and
c            is computed for the northern hemisphere only.  i.e.,
c            if nlat is odd sf is computed in the array sf(i,j) for
c            i=1,...,(nlat+1)/2 and for j=1,...,nlon.  if nlat is even
c            sf is computed in the array sf(i,j) for i=1,...,nlat/2
c            and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            the arrays br,bi, and sf can be three dimensional corresponding
c            to an indexed multiple vector field (v,w).  in this case,
c            multiple scalar synthesis will be performed to compute each
c            scalar field.  the third index for br,bi, and sf is the synthesis
c            index which assumes the values k = 1,...,nt.  for a single
c            synthesis set nt = 1.  the description of the remaining
c            parameters is simplified by assuming that nt=1 or that br,bi,
c            and sf are two dimensional arrays.
c
c     isf    the first dimension of the array sf as it appears in
c            the program that calls igradec. if isym = 0 then isf
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then isf must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then isf must be at least (nlat+1)/2.
c
c     jsf    the second dimension of the array sf as it appears in
c            the program that calls igradec. jsf must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhaec.
c     ***    br,bi must be computed by vhaec prior to calling igradec.
c
c     mdb    the first dimension of the arrays br and bi as it appears in
c            the program that calls igradec (and vhaec). mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br and bi as it appears in
c            the program that calls igradec (and vhaec). ndb must be at
c            least nlat.
c
c
c  wshsec    an array which must be initialized by subroutine shseci.
c            once initialized,
c            wshsec can be used repeatedly by igradec as long as nlon
c            and nlat remain unchanged.  wshsec must not be altered
c            between calls of igradec.
c
c
c  lshsec    the dimension of the array wshsec as it appears in the
c            program that calls igradec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c
c            then lshsec must be greater than or equal to
c
c               2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls igradec. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2)     if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2)     if nlon is odd
c
c            if isym is zero then lwork must be at least
c
c               nlat*(nt*nlon+max0(3*l2,nlon)+2*nt*l1+1)
c
c            if isym is not zero then lwork must be at least
c
c               l2*(nt*nlon+max0(3*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
c     sf    a two or three dimensional array (see input parameter nt) that
c           contain a scalar field whose gradient is the irrotational
c           component of the vector field (v,w).  the vector spherical
c           harmonic coefficients br,bi were precomputed by subroutine
c           vhaec.  sf(i,j) is given at the gaussian colatitude theta(i)
c           and longitude lambda(j) = (j-1)*2*pi/nlon.  the index ranges
c           are defined at input parameter isym.
c
c
c  ierror   = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of isf
c           = 6  error in the specification of jsf
c           = 7  error in the specification of mdb
c           = 8  error in the specification of ndb
c           = 9  error in the specification of lshsec
c           = 10 error in the specification of lwork
c
c **********************************************************************
c   
      subroutine igradec(nlat,nlon,isym,nt,sf,isf,jsf,br,bi,mdb,ndb,
     +wshsec,lshsec,work,lwork,ierror)
      dimension sf(isf,jsf,nt)
      dimension br(mdb,ndb,nt),bi(mdb,ndb,nt)
      dimension wshsec(lshsec),work(lwork)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. isf.lt.nlat) .or.
     +   (isym.ne.0 .and. isf.lt.imid)) return
      ierror = 6
      if(jsf .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+2)/2)
      if(mdb .lt. min0(nlat,(nlon+1)/2)) return
      ierror = 8
      if(ndb .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      lpimn = (imid*mmax*(nlat+nlat-mmax+1))/2
c
c     verify saved work space length
c
      l1 = min0(nlat,(nlon+2)/2)
      l2 = (nlat+1)/2
      lwkmin=2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
      if (lshsec .lt. lwkmin) return
      ierror = 10
c
c     set minimum and verify unsaved work space
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
c
c     set first dimension for a,b (as requried by shsec)
c
      mab = min0(nlat,nlon/2+1)
      mn = mab*nlat*nt
c     lwkmin = nln+ls*nlon+2*mn+nlat
      if (isym .eq. 0) then
	lwkmin = nlat*(nt*nlon+max0(3*l2,nlon)+2*nt*l1+1)
      else
	lwkmin = l2*(nt*nlon+max0(3*nlat,nlon))+nlat*(2*nt*l1+1)
      end if
      if (lwork .lt. lwkmin) return
      ierror = 0
c
c     set work space pointers
c
      ia = 1
      ib = ia + mn
      is = ib + mn
      iwk = is + nlat
      liwk = lwork-2*mn-nlat
      call igrdec1(nlat,nlon,isym,nt,sf,isf,jsf,work(ia),work(ib),mab,
     +work(is),mdb,ndb,br,bi,wshsec,lshsec,work(iwk),liwk,ierror)
      return
      end

      subroutine igrdec1(nlat,nlon,isym,nt,sf,isf,jsf,a,b,mab,
     +sqnn,mdb,ndb,br,bi,wshsec,lshsec,wk,lwk,ierror)
      dimension sf(isf,jsf,nt)
      dimension br(mdb,ndb,nt),bi(mdb,ndb,nt),sqnn(nlat)
      dimension a(mab,nlat,nt),b(mab,nlat,nt)
      dimension wshsec(lshsec),wk(lwk)
c
c     preset coefficient multiplyers in vector
c
      do 1 n=2,nlat
      fn = float(n-1)
      sqnn(n) = 1.0/sqrt(fn*(fn+1.))
    1 continue
c
c     set upper limit for vector m subscript
c
      mmax = min0(nlat,(nlon+1)/2)
c
c     compute multiple scalar field coefficients
c
      do 2 k=1,nt
c
c     preset to 0.0
c
      do 3 n=1,nlat
      do 4 m=1,mab
      a(m,n,k) = 0.0
      b(m,n,k) = 0.0
    4 continue
    3 continue
c
c     compute m=0 coefficients
c
      do 5 n=2,nlat
      a(1,n,k) = br(1,n,k)*sqnn(n)
      b(1,n,k)= bi(1,n,k)*sqnn(n)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      a(m,n,k) = sqnn(n)*br(m,n,k)
      b(m,n,k) = sqnn(n)*bi(m,n,k)
    7 continue
    6 continue
    2 continue
c
c     scalar sythesize a,b into sf
c
      call shsec(nlat,nlon,isym,nt,sf,isf,jsf,a,b,mab,nlat,
     +wshsec,lshsec,wk,lwk,ierror)
      return
      end
