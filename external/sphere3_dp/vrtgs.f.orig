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
c  .                         SPHEREPACK3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file vrtgs.f
c
c     this file includes documentation and code for
c     subroutine divgs          i
c
c ... files which must be loaded with vrtgs.f
c
c     sphcom.f, hrfft.f, vhgsc.f, shsgs.f, gaqd.f
c
c     subroutine vrtgs(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
c    +                 wshsgs,lshsgs,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients cr and ci, precomputed
c     by subroutine vhags for a vector field (v,w), subroutine vrtgs
c     computes the vorticity of the vector field in the scalar array
c     vort.  vort(i,j) is the vorticity at the gaussian colatitude
c     theta(i) (see nlat as input parameter) and longitude
c     lambda(j) = (j-1)*2*pi/nlon on the sphere.  i.e.,
c
c            vort(i,j) =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c     where sint = sin(theta(i)).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     cr,ci were precomputed.  required associated legendre polynomials
c     are stored rather than recomputed as they are in subroutine vrtgc.
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than 3. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   a parameter which determines whether the vorticity is
c            computed on the full or half sphere as follows:
c
c      = 0
c            the symmetries/antsymmetries described in isym=1,2 below
c            do not exist in (v,w) about the equator.  in this case the
c            vorticity is neither symmetric nor antisymmetric about
c            the equator.  the vorticity is computed on the entire
c            sphere.  i.e., in the array vort(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the vorticity is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vort(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vort(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the vorticity is antisymmetric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vort(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vort(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c      nt    nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            in the program that calls vrtgs, the arrays cr,ci, and vort
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute the vorticity for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     ivrt   the first dimension of the array vort as it appears in
c            the program that calls vrtgs. if isym = 0 then ivrt
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then ivrt must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then ivrt must be at least (nlat+1)/2.
c
c     jvrt   the second dimension of the array vort as it appears in
c            the program that calls vrtgs. jvrt must be at least nlon.
c
c    cr,ci   two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhags.
c     ***    cr and ci must be computed by vhags prior to calling
c            vrtgs.
c
c      mdc   the first dimension of the arrays cr and ci as it
c            appears in the program that calls vrtgs. mdc must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c      ndc   the second dimension of the arrays cr and ci as it
c            appears in the program that calls vrtgs. ndc must be at
c            least nlat.
c
c   wshsgs   an array which must be initialized by subroutine shsgsi.
c            once initialized,
c            wshsgs can be used repeatedly by vrtgs as long as nlon
c            and nlat remain unchanged.  wshsgs must not be altered
c            between calls of vrtgs
c
c   lshsgs   the dimension of the array wshsgs   as it appears in the
c            program that calls vrtgs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c    lwork   the dimension of the array work as it appears in the
c            program that calls vrtgs. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            if isym = 0 then lwork must be at least
c
c               nlat*((nt+1)*nlon+2*nt*l1+1)
c
c            if isym > 0 then lwork must be at least
c
c               (nt+1)*l2*nlon+nlat*(2*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
c     vort   a two or three dimensional array (see input parameter nt)
c            that contains the vorticity of the vector field (v,w)
c            whose coefficients cr,ci where computed by subroutine vhags.
c            vort(i,j) is the vorticity at the gaussian colatitude point
c            theta(i) and longitude point lambda(j) = (j-1)*2*pi/nlon.
c            the index ranges are defined above at the input parameter
c            isym.
c
c
c   ierror   an error parameter which indicates fatal errors with input
c            parameters when returned positive.
c          = 0  no errors
c          = 1  error in the specification of nlat
c          = 2  error in the specification of nlon
c          = 3  error in the specification of isym
c          = 4  error in the specification of nt
c          = 5  error in the specification of ivrt
c          = 6  error in the specification of jvrt
c          = 7  error in the specification of mdc
c          = 8  error in the specification of ndc
c          = 9  error in the specification of lshsgs
c          = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
c   
      subroutine vrtgs(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
     +                 wshsgs,lshsgs,work,lwork,ierror)

      dimension vort(ivrt,jvrt,nt),cr(mdc,ndc,nt),ci(mdc,ndc,nt)
      dimension wshsgs(lshsgs),work(lwork)
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
      if((isym.eq.0 .and. ivrt.lt.nlat) .or.
     1   (isym.gt.0 .and. ivrt.lt.imid)) return
      ierror = 6
      if(jvrt .lt. nlon) return
      ierror = 7
      if(mdc .lt. min0(nlat,(nlon+1)/2)) return
      mmax = min0(nlat,(nlon+2)/2)
      ierror = 8
      if(ndc .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      lpimn = (imid*mmax*(nlat+nlat-mmax+1))/2
      l2 = (nlat+mod(nlat,2))/2
      l1 = min0((nlon+2)/2,nlat)
      lp=nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
      if(lshsgs.lt.lp) return
      ierror = 10
c
c     verify unsaved work space (add to what shses requires, file f3)
c
c
c     set first dimension for a,b (as requried by shses)
c
      mab = min0(nlat,nlon/2+1)
      mn = mab*nlat*nt
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork.lt. nln+ls*nlon+2*mn+nlat) return
      ierror = 0
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      is = ib+mn
      iwk = is+nlat
      lwk = lwork-2*mn-nlat
      call vrtgs1(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
     +work(ia),work(ib),mab,work(is),wshsgs,lshsgs,work(iwk),lwk,
     +ierror)
      return
      end

      subroutine vrtgs1(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
     +                  a,b,mab,sqnn,wsav,lwsav,wk,lwk,ierror)
      dimension vort(ivrt,jvrt,nt),cr(mdc,ndc,nt),ci(mdc,ndc,nt)
      dimension a(mab,nlat,nt),b(mab,nlat,nt),sqnn(nlat)
      dimension wsav(lwsav),wk(lwk)
c
c     set coefficient multiplyers
c
      do 1 n=2,nlat
      fn = float(n-1)
      sqnn(n) = sqrt(fn*(fn+1.))
    1 continue
c
c     compute divergence scalar coefficients for each vector field
c
      do 2 k=1,nt
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
      a(1,n,k) = sqnn(n)*cr(1,n,k)
      b(1,n,k) = sqnn(n)*ci(1,n,k)
    5 continue
c
c     compute m>0 coefficients
c
      mmax = min0(nlat,(nlon+1)/2)
      do 6 m=2,mmax
      do 7 n=m,nlat
      a(m,n,k) = sqnn(n)*cr(m,n,k)
      b(m,n,k) = sqnn(n)*ci(m,n,k)
    7 continue
    6 continue
    2 continue
c
c     synthesize a,b into vort
c
      call shsgs(nlat,nlon,isym,nt,vort,ivrt,jvrt,a,b,
     +           mab,nlat,wsav,lwsav,wk,lwk,ierror)
      return
      end
