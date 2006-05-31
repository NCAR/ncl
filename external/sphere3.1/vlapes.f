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
c ... file vlapes.f
c
c     this file includes documentation and code for
c     subroutine vlapes          i
c
c ... files which must be loaded with vlapes.f
c
c     sphcom.f, hrfft.f, vhaes.f, vhses.f
c
c
c
c
c     subroutine vlapes(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,br,bi,cr,ci,
c    +mdbc,ndbc,wvhses,lvhses,work,lwork,ierror)
c
c
c     subroutine vlapes computes the vector laplacian of the vector field
c     (v,w) in (vlap,wlap) (see the definition of the vector laplacian at
c     the output parameter description of vlap,wlap below).  w and wlap
c     are east longitudinal components of the vectors.  v and vlap are
c     colatitudinal components of the vectors.  br,bi,cr, and ci are the
c     vector harmonic coefficients of (v,w).  these must be precomputed by
c     vhaes and are input parameters to vlapes.  the laplacian components
c     in (vlap,wlap) have the same symmetry or lack of symmetry about the
c     equator as (v,w).  the input parameters ityp,nt,mdbc,nbdc must have
c     the same values used by vhaes to compute br,bi,cr, and ci for (v,w).
c     vlap(i,j) and wlap(i,j) are given on the sphere at the colatitude
c
c            theta(i) = (i-1)*pi/(nlat-1)
c
c     for i=1,...,nlat and east longitude
c
c            lambda(j) = (j-1)*2*pi/nlon
c
c     for j=1,...,nlon.
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
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     ityp   this parameter should have the same value input to subroutine
c            vhaes to compute the coefficients br,bi,cr, and ci for the
c            vector field (v,w).  ityp is set as follows:
c
c            = 0  no symmetries exist in (v,w) about the equator. (vlap,wlap)
c                 is computed and stored on the entire sphere in the arrays
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
c
c
c            = 1  no symmetries exist in (v,w) about the equator. (vlap,wlap)
c                 is computed and stored on the entire sphere in the arrays
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 the vorticity of (v,w) is zero so the coefficients cr and
c                 ci are zero and are not used.  the vorticity of (vlap,wlap)
c                 is also zero.
c
c
c            = 2  no symmetries exist in (v,w) about the equator. (vlap,wlap)
c                 is computed and stored on the entire sphere in the arrays
c                 vlap(i,j) and wlap(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 the divergence of (v,w) is zero so the coefficients br and
c                 bi are zero and are not used.  the divergence of (vlap,wlap)
c                 is also zero.
c
c            = 3  w is antisymmetric and v is symmetric about the equator.
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  w is antisymmetric and v is symmetric about the equator.
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 vorticity of (v,w) is zero so the coefficients cr,ci are
c                 zero and are not used. the vorticity of (vlap,wlap) is
c                 also zero.
c
c            = 5  w is antisymmetric and v is symmetric about the equator.
c                 consequently wlap is antisymmetric and vlap is symmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 divergence of (v,w) is zero so the coefficients br,bi
c                 are zero and are not used. the divergence of (vlap,wlap)
c                 is also zero.
c
c
c            = 6  w is symmetric and v is antisymmetric about the equator.
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  w is symmetric and v is antisymmetric about the equator.
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 vorticity of (v,w) is zero so the coefficients cr,ci are
c                 zero and are not used. the vorticity of (vlap,wlap) is
c                 also zero.
c
c            = 8  w is symmetric and v is antisymmetric about the equator.
c                 consequently wlap is symmetric and vlap is antisymmetric.
c                 (vlap,wlap) is computed and stored on the northern
c                 hemisphere only.  if nlat is odd, storage is in the arrays
c                 vlap(i,j),wlap(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even, storage is in the arrays vlap(i,j),
c                 wlap(i,j) for i=1,...,nlat/2 and j=1,...,nlon.  the
c                 divergence of (v,w) is zero so the coefficients br,bi
c                 are zero and are not used. the divergence of (vlap,wlap)
c                 is also zero.
c
c
c     nt     nt is the number of vector fields (v,w).  some computational
c            efficiency is obtained for multiple fields.  in the program
c            that calls vlapes, the arrays vlap,wlap,br,bi,cr and ci
c            can be three dimensional corresponding to an indexed multiple
c            vector field.  in this case multiple vector synthesis will
c            be performed to compute the vector laplacian for each field.
c            the third index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1.  the description
c            of the remaining parameters is simplified by assuming that nt=1
c            or that all arrays are two dimensional.
c
c   idvw     the first dimension of the arrays vlap and wlap as it appears
c            in the program that calls vlapes.  if ityp=0,1, or 2  then idvw
c            must be at least nlat.  if ityp > 2 and nlat is even then idvw
c            must be at least nlat/2. if ityp > 2 and nlat is odd then idvw
c            must be at least (nlat+1)/2.
c
c   jdvw     the second dimension of the arrays vlap and wlap as it appears
c            in the program that calls vlapes. jdvw must be at least nlon.
c
c
c   br,bi    two or three dimensional arrays (see input parameter nt)
c   cr,ci    that contain vector spherical harmonic coefficients
c            of the vector field (v,w) as computed by subroutine vhaes.
c            br,bi,cr and ci must be computed by vhaes prior to calling
c            vlapes.  if ityp=1,4, or 7 then cr,ci are not used and can
c            be dummy arguments.  if ityp=2,5, or 8 then br,bi are not
c            used and can be dummy arguments.
c
c    mdbc    the first dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls vlapes.  mdbc must be
c            at least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndbc    the second dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls vlapes. ndbc must be at
c            least nlat.
c
c    wvhses  an array which must be initialized by subroutine vhsesi.
c            once initialized, vhses
c            can be used repeatedly by vlapes as long as nlat and nlon
c            remain unchanged.  wvhses must not be altered between calls
c            of vlapes.
c
c    lvhses  the dimension of the array wvhses as it appears in the
c            program that calls vlapes.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            then lvhses must be greater than or equal
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vlapes. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if ityp .le. 2 then
c
c               (2*nt+1)*nlat*nlon + nlat*(4*nt*l1+1)
c
c            or if ityp .gt. 2 then
c
c               (2*nt+1)*l2*nlon + nlat*(4*nt*l1+1)
c
c            will suffice as a length for lwork.
c
c     **************************************************************
c
c     output parameters
c
c
c    vlap,   two or three dimensional arrays (see input parameter nt) that
c    wlap    contain the vector laplacian of the field (v,w).  wlap(i,j) is
c            the east longitude component and vlap(i,j) is the colatitudinal
c            component of the vector laplacian.  the definition of the
c            vector laplacian follows:
c
c            let cost and sint be the cosine and sine at colatitude theta.
c            let d( )/dlambda  and d( )/dtheta be the first order partial
c            derivatives in longitude and colatitude.  let del2 be the scalar
c            laplacian operator
c
c                 del2(s) = [d(sint*d(s)/dtheta)/dtheta +
c                             2            2
c                            d (s)/dlambda /sint]/sint
c
c            then the vector laplacian opeator
c
c                 dvel2(v,w) = (vlap,wlap)
c
c            is defined by
c
c                 vlap = del2(v) - (2*cost*dw/dlambda + v)/sint**2
c
c                 wlap = del2(w) + (2*cost*dv/dlambda - w)/sint**2
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
c            = 5  error in the specification of idvw
c
c            = 6  error in the specification of jdvw
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lvhses
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for vlapes
c
c **********************************************************************
c
      subroutine vlapes(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,br,bi,
     +cr,ci,mdbc,ndbc,wvhses,lvhses,work,lwork,ierror)
      dimension vlap(idvw,jdvw,nt),wlap(idvw,jdvw,nt)
      dimension br(mdbc,ndbc,nt),bi(mdbc,ndbc,nt)
      dimension cr(mdbc,ndbc,nt),ci(mdbc,ndbc,nt)
      dimension wvhses(lvhses),work(lwork)
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 1) return
      ierror = 3
      if(ityp.lt.0 .or. ityp.gt.8) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((ityp.le.2 .and. idvw.lt.nlat) .or.
     1   (ityp.gt.2 .and. idvw.lt.imid)) return
      ierror = 6
      if(jdvw .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+1)/2)
      if(mdbc .lt. mmax) return
      ierror = 8
      if(ndbc .lt. nlat) return
      ierror = 9
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      lsavmin = lzimn+lzimn+nlon+15
      if(lvhses .lt. lsavmin) return
c
c     verify unsaved work space length
c
      mn = mmax*nlat*nt
      l2 = (nlat+1)/2
      l1 = min0(nlat,nlon/2+1)
      if (ityp .le. 2) then
	lwkmin = (2*nt+1)*nlat*nlon + nlat*(4*nt*l1+1)
      else
	lwkmin = (2*nt+1)*l2*nlon + nlat*(4*nt*l1+1)
      end if
      if (lwork .lt. lwkmin) return
      ierror = 0
c
c     set work space pointers for vector laplacian coefficients
c
      if (ityp.eq.0 .or. ityp.eq.3 .or. ityp.eq.6) then
      ibr = 1
      ibi = ibr+mn
      icr = ibi+mn
      ici = icr+mn
      else if (ityp.eq.1 .or. ityp.eq.4 .or. ityp.eq.7) then
      ibr = 1
      ibi = ibr+mn
      icr = ibi+mn
      ici = icr
      else
      ibr = 1
      ibi = 1
      icr = ibi+mn
      ici = icr+mn
      end if
      ifn = ici + mn
      iwk = ifn + nlat
      liwk = lwork-4*mn-nlat
      call vlapes1(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,work(ibr),
     +work(ibi),work(icr),work(ici),mmax,work(ifn),mdbc,ndbc,br,bi,
     +cr,ci,wvhses,lvhses,work(iwk),liwk,ierror)
      return
      end

      subroutine vlapes1(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,brlap,
     +bilap,crlap,cilap,mmax,fnn,mdb,ndb,br,bi,cr,ci,wsave,lsave,
     +wk,lwk,ierror)
      dimension vlap(idvw,jdvw,nt),wlap(idvw,jdvw,nt)
      dimension fnn(nlat),brlap(mmax,nlat,nt),bilap(mmax,nlat,nt)
      dimension crlap(mmax,nlat,nt),cilap(mmax,nlat,nt)
      dimension br(mdb,ndb,nt),bi(mdb,ndb,nt)
      dimension cr(mdb,ndb,nt),ci(mdb,ndb,nt)
      dimension wsave(lsave),wk(lwk)
c
c     preset coefficient multiplyers
c
      do 1 n=2,nlat
      fn = float(n-1)
      fnn(n) = -fn*(fn+1.)
    1 continue
c
c     set laplacian coefficients from br,bi,cr,ci
c
      if (ityp.eq.0 .or. ityp.eq.3 .or. ityp.eq.6) then
c
c     all coefficients needed
c
      do 2 k=1,nt
      do 3 n=1,nlat
      do 4 m=1,mmax
      brlap(m,n,k) = 0.0
      bilap(m,n,k) = 0.0
      crlap(m,n,k) = 0.0
      cilap(m,n,k) = 0.0
    4 continue
    3 continue
      do 5 n=2,nlat
      brlap(1,n,k) = fnn(n)*br(1,n,k)
      bilap(1,n,k) = fnn(n)*bi(1,n,k)
      crlap(1,n,k) = fnn(n)*cr(1,n,k)
      cilap(1,n,k) = fnn(n)*ci(1,n,k)
    5 continue
      do 6 m=2,mmax
      do 7 n=m,nlat
      brlap(m,n,k) = fnn(n)*br(m,n,k)
      bilap(m,n,k) = fnn(n)*bi(m,n,k)
      crlap(m,n,k) = fnn(n)*cr(m,n,k)
      cilap(m,n,k) = fnn(n)*ci(m,n,k)
    7 continue
    6 continue
    2 continue
      else if (ityp.eq.1 .or. ityp.eq.4 .or. ityp.eq.7) then
c
c     vorticity is zero so cr,ci=0 not used
c
      do 12 k=1,nt
      do 13 n=1,nlat
      do 14 m=1,mmax
      brlap(m,n,k) = 0.0
      bilap(m,n,k) = 0.0
   14 continue
   13 continue
      do 15 n=2,nlat
      brlap(1,n,k) = fnn(n)*br(1,n,k)
      bilap(1,n,k) = fnn(n)*bi(1,n,k)
   15 continue
      do 16 m=2,mmax
      do 17 n=m,nlat
      brlap(m,n,k) = fnn(n)*br(m,n,k)
      bilap(m,n,k) = fnn(n)*bi(m,n,k)
   17 continue
   16 continue
   12 continue
      else
c
c     divergence is zero so br,bi=0 not used
c
      do 22 k=1,nt
      do 23 n=1,nlat
      do 24 m=1,mmax
      crlap(m,n,k) = 0.0
      cilap(m,n,k) = 0.0
   24 continue
   23 continue
      do 25 n=2,nlat
      crlap(1,n,k) = fnn(n)*cr(1,n,k)
      cilap(1,n,k) = fnn(n)*ci(1,n,k)
   25 continue
      do 26 m=2,mmax
      do 27 n=m,nlat
      crlap(m,n,k) = fnn(n)*cr(m,n,k)
      cilap(m,n,k) = fnn(n)*ci(m,n,k)
   27 continue
   26 continue
   22 continue
      end if
c
c     sythesize coefs into vector field (vlap,wlap)
c
      call vhses(nlat,nlon,ityp,nt,vlap,wlap,idvw,jdvw,brlap,bilap,
     +           crlap,cilap,mmax,nlat,wsave,lsave,wk,lwk,ierror)
      return
      end
