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
c ... file ivlapgs.f
c
c     this file includes documentation and code for
c     subroutine ivlapgs
c
c ... files which must be loaded with ivlapgs.f
c
c     sphcom.f, hrfft.f, vhags.f, vhsgs.f, gaqd.f
c
c
c     subroutine ivlapgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +mdbc,ndbc,wvhsgs,lvhsgs,work,lwork,ierror)
c
c     given the vector spherical harmonic coefficients (br,bi,cr,ci)
c     precomputed by subroutine vhags for a vector field (vlap,wlap),
c     subroutine ivlapgs computes a vector field (v,w) whose vector
c     laplacian is (vlap,wlap).  v,vlap are the colatitudinal
c     components and w,wlap are the east longitudinal components of
c     the vectors.  (v,w) have the same symmetry or lack of symmetry
c     about the equator as (vlap,wlap).  the input parameters ityp,
c     nt,mdbc,ndbc must have the same values used by vhags to compute
c     br,bi,cr,ci for (vlap,wlap).
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
c     ityp   this parameter should have the same value input to subroutine
c            vhags to compute the coefficients br,bi,cr, and ci for the
c            vector field (vlap,wlap).  ityp is set as follows:
c
c            = 0  no symmetries exist in (vlap,wlap) about the equator. (v,w)
c                 is computed and stored on the entire sphere in the arrays
c                 arrays v(i,j) and w(i,j) for i=1,...,nlat and j=1,...,nlon.
c
c            = 1  no symmetries exist in (vlap,wlap) about the equator. (v,w)
c                 is computed and stored on the entire sphere in the arrays
c                 v(i,j) and w(i,j) for i=1,...,nlat and j=1,...,nlon.  the
c                 vorticity of (vlap,wlap) is zero so the coefficients cr and
c                 ci are zero and are not used.  the vorticity of (v,w) is
c                 also zero.
c
c
c            = 2  no symmetries exist in (vlap,wlap) about the equator. (v,w)
c                 is computed and stored on the entire sphere in the arrays
c                 w(i,j) and v(i,j) for i=1,...,nlat and j=1,...,nlon.  the
c                 divergence of (vlap,wlap) is zero so the coefficients br and
c                 bi are zero and are not used.  the divergence of (v,w) is
c                 also zero.
c
c            = 3  wlap is antisymmetric and vlap is symmetric about the
c                 equator. consequently w is antisymmetric and v is symmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays v(i,j),
c                 w(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays v(i,j),w(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  wlap is antisymmetric and vlap is symmetric about the
c                 equator. consequently w is antisymmetric and v is symmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays v(i,j),
c                 w(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays v(i,j),w(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.  the vorticity of (vlap,
c                 wlap) is zero so the coefficients cr,ci are zero and
c                 are not used. the vorticity of (v,w) is also zero.
c
c            = 5  wlap is antisymmetric and vlap is symmetric about the
c                 equator. consequently w is antisymmetric and v is symmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.  the divergence of (vlap,
c                 wlap) is zero so the coefficients br,bi are zero and
c                 are not used. the divergence of (v,w) is also zero.
c
c
c            = 6  wlap is symmetric and vlap is antisymmetric about the
c                 equator. consequently w is symmetric and v is antisymmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  wlap is symmetric and vlap is antisymmetric about the
c                 equator. consequently w is symmetric and v is antisymmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.  the vorticity of (vlap,
c                 wlap) is zero so the coefficients cr,ci are zero and
c                 are not used. the vorticity of (v,w) is also zero.
c
c            = 8  wlap is symmetric and vlap is antisymmetric about the
c                 equator. consequently w is symmetric and v is antisymmetric.
c                 (v,w) is computed and stored on the northern hemisphere
c                 only.  if nlat is odd, storage is in the arrays w(i,j),
c                 v(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if nlat
c                 is even, storage is in the arrays w(i,j),v(i,j) for
c                 i=1,...,nlat/2 and j=1,...,nlon.  the divergence of (vlap,
c                 wlap) is zero so the coefficients br,bi are zero and
c                 are not used. the divergence of (v,w) is also zero.
c
c
c     nt     nt is the number of vector fields (vlap,wlap). some computational
c            efficiency is obtained for multiple fields.  in the program
c            that calls ivlapgs, the arrays v,w,br,bi,cr and ci can be
c            three dimensional corresponding to an indexed multiple vector
c            field.  in this case multiple vector synthesis will be performed
c            to compute the (v,w) for each field (vlap,wlap).  the third
c            index is the synthesis index which assumes the values k=1,...,nt.
c            for a single synthesis set nt=1.  the description of the
c            remaining parameters is simplified by assuming that nt=1 or
c            that all arrays are two dimensional.
c
c   idvw     the first dimension of the arrays w and v as it appears in
c            the program that calls ivlapgs.  if ityp=0,1, or 2  then idvw
c            must be at least nlat.  if ityp > 2 and nlat is even then idvw
c            must be at least nlat/2. if ityp > 2 and nlat is odd then idvw
c            must be at least (nlat+1)/2.
c
c   jdvw     the second dimension of the arrays w and v as it appears in
c            the program that calls ivlapgs. jdvw must be at least nlon.
c
c
c   br,bi    two or three dimensional arrays (see input parameter nt)
c   cr,ci    that contain vector spherical harmonic coefficients of the
c            vector field (vlap,wlap) as computed by subroutine vhags.
c            br,bi,cr and ci must be computed by vhags prior to calling
c            ivlapgs.  if ityp=1,4, or 7 then cr,ci are not used and can
c            be dummy arguments.  if ityp=2,5, or 8 then br,bi are not
c            used and can be dummy arguments.
c
c    mdbc    the first dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls ivlapgs.  mdbc must be
c            at least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndbc    the second dimension of the arrays br,bi,cr and ci as it
c            appears in the program that calls ivlapgs. ndbc must be at
c            least nlat.
c
c    wvhsgs  an array which must be initialized by subroutine vhsgsi.
c            once initialized, wvhsgsi
c            can be used repeatedly by ivlapgs as long as nlat and nlon
c            remain unchanged.  wvhsgs must not be altered between calls
c            of ivlapgs.
c
c    lvhsgs  the dimension of the array wvhsgs as it appears in the
c            program that calls ivlapgs.  let
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd.
c
c            let
c
c               lsavmin = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c            then lvhsgs must be greater than or equal to lsavmin
c            (see ierror=9 below).
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls ivlapgs. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2) if nlon is even or
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
c    v,w     two or three dimensional arrays (see input parameter nt) that
c            contain a vector field whose vector laplacian is (vlap,wlap).
c            w(i,j) is the east longitude and v(i,j) is the colatitudinal
c            component of the vector. v(i,j) and w(i,j) are given on the
c            sphere at the guassian colatitude theta(i) for i=1,...,nlat
c            and east longitude lambda(j)=(j-1)*2*pi/nlon for j = 1,...,nlon.
c            let cost and sint be the cosine and sine at colatitude theta.
c            let d( )/dlambda  and d( )/dtheta be the first order partial
c            derivatives in longitude and colatitude.  let sf be either v
c            or w.  define:
c
c                 del2s(sf) = [d(sint*d(sf)/dtheta)/dtheta +
c                               2            2
c                              d (sf)/dlambda /sint]/sint
c
c            then the vector laplacian of (v,w) in (vlap,wlap) satisfies
c
c                 vlap = del2s(v) + (2*cost*dw/dlambda - v)/sint**2
c
c            and
c
c                 wlap = del2s(w) - (2*cost*dv/dlambda + w)/sint**2
c
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
c            = 9  error in the specification of lvhsgs
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for ivlapgs
c
c **********************************************************************
c
      subroutine ivlapgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +mdbc,ndbc,wvhsgs,lvhsgs,work,lwork,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt)
      dimension br(mdbc,ndbc,nt),bi(mdbc,ndbc,nt)
      dimension cr(mdbc,ndbc,nt),ci(mdbc,ndbc,nt)
      dimension wvhsgs(lvhsgs),work(lwork)
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
c
c     set minimum and verify saved workspace length
c
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      lsavmin = lzimn+lzimn+nlon+15
      if (lvhsgs .lt. lsavmin) return
c
c     set minimum and verify unsaved work space length
c
      mn = mmax*nlat*nt
      l2 = (nlat+1)/2
      l1 = min0(nlat,(nlon+1)/2)
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
      if (ityp.eq.0 .or. ityp.eq.3 .or. ityp.eq.6) then
      liwk = lwork-4*mn-nlat
      else
      liwk = lwork-2*mn-nlat
      end if
      call ivlapgs1(nlat,nlon,ityp,nt,v,w,idvw,jdvw,work(ibr),
     +work(ibi),work(icr),work(ici),mmax,work(ifn),mdbc,ndbc,br,bi,
     +cr,ci,wvhsgs,lvhsgs,work(iwk),liwk,ierror)
      return
      end

      subroutine ivlapgs1(nlat,nlon,ityp,nt,v,w,idvw,jdvw,brvw,
     +bivw,crvw,civw,mmax,fnn,mdbc,ndbc,br,bi,cr,ci,wsave,lsave,
     +wk,lwk,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt)
      dimension fnn(nlat),brvw(mmax,nlat,nt),bivw(mmax,nlat,nt)
      dimension crvw(mmax,nlat,nt),civw(mmax,nlat,nt)
      dimension br(mdbc,ndbc,nt),bi(mdbc,ndbc,nt)
      dimension cr(mdbc,ndbc,nt),ci(mdbc,ndbc,nt)
      dimension wsave(lsave),wk(lwk)
c
c     preset coefficient multiplyers
c
      do 1 n=2,nlat
      fn = float(n-1)
      fnn(n) = -1.0/(fn*(fn+1.))
    1 continue
c
c     set (u,v) coefficients from br,bi,cr,ci
c
      if (ityp.eq.0 .or. ityp.eq.3 .or. ityp.eq.6) then
c
c     all coefficients needed
c
      do 2 k=1,nt
      do 3 n=1,nlat
      do 4 m=1,mmax
      brvw(m,n,k) = 0.0
      bivw(m,n,k) = 0.0
      crvw(m,n,k) = 0.0
      civw(m,n,k) = 0.0
    4 continue
    3 continue
      do 5 n=2,nlat
      brvw(1,n,k) = fnn(n)*br(1,n,k)
      bivw(1,n,k) = fnn(n)*bi(1,n,k)
      crvw(1,n,k) = fnn(n)*cr(1,n,k)
      civw(1,n,k) = fnn(n)*ci(1,n,k)
    5 continue
      do 6 m=2,mmax
      do 7 n=m,nlat
      brvw(m,n,k) = fnn(n)*br(m,n,k)
      bivw(m,n,k) = fnn(n)*bi(m,n,k)
      crvw(m,n,k) = fnn(n)*cr(m,n,k)
      civw(m,n,k) = fnn(n)*ci(m,n,k)
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
      brvw(m,n,k) = 0.0
      bivw(m,n,k) = 0.0
   14 continue
   13 continue
      do 15 n=2,nlat
      brvw(1,n,k) = fnn(n)*br(1,n,k)
      bivw(1,n,k) = fnn(n)*bi(1,n,k)
   15 continue
      do 16 m=2,mmax
      do 17 n=m,nlat
      brvw(m,n,k) = fnn(n)*br(m,n,k)
      bivw(m,n,k) = fnn(n)*bi(m,n,k)
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
      crvw(m,n,k) = 0.0
      civw(m,n,k) = 0.0
   24 continue
   23 continue
      do 25 n=2,nlat
      crvw(1,n,k) = fnn(n)*cr(1,n,k)
      civw(1,n,k) = fnn(n)*ci(1,n,k)
   25 continue
      do 26 m=2,mmax
      do 27 n=m,nlat
      crvw(m,n,k) = fnn(n)*cr(m,n,k)
      civw(m,n,k) = fnn(n)*ci(m,n,k)
   27 continue
   26 continue
   22 continue
      end if
c
c     sythesize coefs into vector field (v,w)
c
      call vhsgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,brvw,bivw,
     +           crvw,civw,mmax,nlat,wsave,lsave,wk,lwk,ierror)
      return
      end
