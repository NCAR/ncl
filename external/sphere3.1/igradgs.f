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
c ... file igradgs.f
c
c     this file includes documentation and code for
c     subroutine igradgs         i
c
c ... files which must be loaded with igradgs.f
c
c     sphcom.f, hrfft.f, shsgs.f,vhags.f
c
c     subroutine igradgs(nlat,nlon,isym,nt,sf,isf,jsf,br,bi,mdb,ndb,
c    +                   wshsgs,lshsgs,work,lwork,ierror)
c
c     let br,bi,cr,ci be the vector spherical harmonic coefficients
c     precomputed by vhags for a vector field (v,w).  let (v',w') be
c     the irrotational component of (v,w) (i.e., (v',w') is generated
c     by assuming cr,ci are zero and synthesizing br,bi with vhsgs).
c     then subroutine igradgs computes a scalar field sf such that
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
c     at the gaussian colatitude theta(i) (see nlat as input parameter)
c     and longitude lambda(j) = (j-1)*2*pi/nlon where sint = sin(theta(i)).
c
c     note:  for an irrotational vector field (v,w), subroutine igradgs
c     computes a scalar field whose gradient is (v,w).  in ay case,
c     subroutine igradgs "inverts" the gradient subroutine gradgs.
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
c            the program that calls igradgs. if isym = 0 then isf
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then isf must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then isf must be at least (nlat+1)/2.
c
c     jsf    the second dimension of the array sf as it appears in
c            the program that calls igradgs. jsf must be at least nlon.
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c     ***    br,bi must be computed by vhags prior to calling igradgs.
c
c     mdb    the first dimension of the arrays br and bi as it appears in
c            the program that calls igradgs (and vhags). mdb must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndb    the second dimension of the arrays br and bi as it appears in
c            the program that calls igradgs (and vhags). ndb must be at
c            least nlat.
c
c
c  wshsgs    an array which must be initialized by subroutine igradgsi
c            (or equivalently by subroutine shsesi).  once initialized,
c            wshsgs can be used repeatedly by igradgs as long as nlon
c            and nlat remain unchanged.  wshsgs must not be altered
c            between calls of igradgs.
c
c
c  lshsgs    the dimension of the array wshsgs as it appears in the
c            program that calls igradgs. define
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
c            then lshsgs must be greater than or equal to
c
c               nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls igradgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 lwork must be greater than or equal to
c
c               nlat*((nt+1)*nlon+2*nt*l1+1)
c
c            if isym > 0 lwork must be greater than or equal to
c
c               (nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
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
c           vhags.  sf(i,j) is given at the gaussian colatitude theta(i)
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
c           = 9  error in the specification of lshsgs
c           = 10 error in the specification of lwork
c
c **********************************************************************
c   
      subroutine igradgs(nlat,nlon,isym,nt,sf,isf,jsf,br,bi,mdb,ndb,
     +wshsgs,lshsgs,work,lwork,ierror)
      dimension sf(isf,jsf,nt)
      dimension br(mdb,ndb,nt),bi(mdb,ndb,nt)
      dimension wshsgs(lshsgs),work(lwork)
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
c
c     verify saved work space length
c
      l2 = (nlat+mod(nlat,2))/2
      l1 = min0((nlon+2)/2,nlat)
      lp=nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
      if(lshsgs.lt.lp) return
      ierror = 10
c
c     set minimum and verify unsaved work space
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
c
c     set first dimension for a,b (as requried by shses)
c
      mab = min0(nlat,nlon/2+1)
      mn = mab*nlat*nt
      lwkmin = nln+ls*nlon+2*mn+nlat
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
      call igrdgs1(nlat,nlon,isym,nt,sf,isf,jsf,work(ia),work(ib),mab,
     +work(is),mdb,ndb,br,bi,wshsgs,lshsgs,work(iwk),liwk,ierror)
      return
      end

      subroutine igrdgs1(nlat,nlon,isym,nt,sf,isf,jsf,a,b,mab,
     +sqnn,mdb,ndb,br,bi,wsav,lsav,wk,lwk,ierror)
      dimension sf(isf,jsf,nt)
      dimension br(mdb,ndb,nt),bi(mdb,ndb,nt),sqnn(nlat)
      dimension a(mab,nlat,nt),b(mab,nlat,nt)
      dimension wsav(lsav),wk(lwk)
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
      call shsgs(nlat,nlon,isym,nt,sf,isf,jsf,a,b,mab,nlat,wsav,
     +           lsav,wk,lwk,ierror)
      return
      end
