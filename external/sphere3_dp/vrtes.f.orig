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
c ... file vrtes.f
c
c     this file includes documentation and code for
c     subroutine divec          i
c
c ... files which must be loaded with vrtes.f
c
c     sphcom.f, hrfft.f, vhaes.f,shses.f
c
c     subroutine vrtes(nlat,nlon,isym,nt,vt,ivrt,jvrt,cr,ci,mdc,ndc,
c    +                 wshses,lshses,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients cr and ci, precomputed
c     by subroutine vhaes for a vector field (v,w), subroutine vrtes
c     computes the vorticity of the vector field in the scalar array
c     vt.  vt(i,j) is the vorticity at the colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     and longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     on the sphere.  i.e.,
c
c            vt(i,j) =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c     where sint = sin(theta(i)).  w is the east longitudinal and v
c     is the colatitudinal component of the vector field from which
c     cr,ci were precomputed.  required associated legendre polynomials
c     are stored rather than recomputed  as they are in subroutine vrtec.
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
c            sphere.  i.e., in the array vt(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c            w is antisymmetric and v is symmetric about the equator.
c            in this case the vorticity is symmetyric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vt(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vt(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c            w is symmetric and v is antisymmetric about the equator
c            in this case the vorticity is antisymmetric about the
c            equator and is computed for the northern hemisphere
c            only.  i.e., if nlat is odd the vorticity is computed
c            in the array vt(i,j) for i=1,...,(nlat+1)/2 and for
c            j=1,...,nlon.  if nlat is even the vorticity is computed
c            in the array vt(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c      nt    nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            in the program that calls vrtes, the arrays cr,ci, and vort
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple scalar synthesis will
c            be performed to compute the vorticity for each field.  the
c            third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt = 1.  the
c            description of the remaining parameters is simplified by
c            assuming that nt=1 or that all the arrays are two dimensional.
c
c     ivrt   the first dimension of the array vt as it appears in
c            the program that calls vrtes. if isym = 0 then ivrt
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then ivrt must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then ivrt must be at least (nlat+1)/2.
c
c     jvrt   the second dimension of the array vt as it appears in
c            the program that calls vrtes. jvrt must be at least nlon.
c
c    cr,ci   two or three dimensional arrays (see input parameter nt)
c            that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhaes.
c     ***    cr and ci must be computed by vhaes prior to calling
c            vrtes.
c
c      mdc   the first dimension of the arrays cr and ci as it
c            appears in the program that calls vrtes. mdc must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c      ndc   the second dimension of the arrays cr and ci as it
c            appears in the program that calls vrtes. ndc must be at
c            least nlat.
c
c   wshses   an array which must be initialized by subroutine shsesi.
c            once initialized,
c            wshses can be used repeatedly by vrtes as long as nlon
c            and nlat remain unchanged.  wshses must not be altered
c            between calls of vrtes
c
c   lshses   the dimension of the array wshses as it appears in the
c            program that calls vrtes. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshses must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c    lwork   the dimension of the array work as it appears in the
c            program that calls vrtes. define
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
c     vt     a two or three dimensional array (see input parameter nt)
c            that contains the vorticity of the vector field (v,w)
c            whose coefficients cr,ci where computed by subroutine vhaes.
c            vt(i,j) is the vorticity at the colatitude point theta(i) =
c            (i-1)*pi/(nlat-1) and longitude point lambda(j) =
c            (j-1)*2*pi/nlon. the index ranges are defined above at the
c            input parameter isym.
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
c          = 9  error in the specification of lshses
c          = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
c   
      subroutine vrtes(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
     +                 wshses,lshses,work,lwork,ierror)

      dimension vort(ivrt,jvrt,nt),cr(mdc,ndc,nt),ci(mdc,ndc,nt)
      dimension wshses(lshses),work(lwork)
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
      if(lshses .lt. lpimn+nlon+15) return
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
      call vrtes1(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
     +work(ia),work(ib),mab,work(is),wshses,lshses,work(iwk),lwk,
     +ierror)
      return
      end

      subroutine vrtes1(nlat,nlon,isym,nt,vort,ivrt,jvrt,cr,ci,mdc,ndc,
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
      call shses(nlat,nlon,isym,nt,vort,ivrt,jvrt,a,b,
     +           mab,nlat,wsav,lwsav,wk,lwk,ierror)
      return
      end
