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
c ... file gradgs.f
c
c     this file includes documentation and code for
c     subroutine gradgs         i
c
c ... files which must be loaded with gradgec.f
c
c     sphcom.f, hrfft.f, shags.f,vhsgs.f
c
c     subroutine gradgs(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsgs,lvhsgs,work,lwork,ierror)
c
c     given the scalar spherical harmonic coefficients a and b, precomputed
c     by subroutine shags for a scalar field sf, subroutine gradgs computes
c     an irrotational vector field (v,w) such that
c
c           gradient(sf) = (v,w).
c
c     v is the colatitudinal and w is the east longitudinal component
c     of the gradient.  i.e.,
c
c            v(i,j) = d(sf(i,j))/dtheta
c
c     and
c
c            w(i,j) = 1/sint*d(sf(i,j))/dlambda
c
c     at the gaussian colatitude point theta(i) (see nlat as input
c     parameter) and longitude lambda(j) = (j-1)*2*pi/nlon where
c     sint = sin(theta(i)).
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
c            nlon = 72 for a five degree grid. nlon must be greater than
c            3.  the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   this has the same value as the isym that was input to
c            subroutine shags to compute the arrays a and b from the
c            scalar field sf.  isym determines whether (v,w) are
c            computed on the full or half sphere as follows:
c
c      = 0
c
c           sf is not symmetric about the equator. in this case
c           the vector field (v,w) is computed on the entire sphere.
c           i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c           j=1,...,nlon.
c
c      = 1
c
c           sf is antisymmetric about the equator. in this case w is
c           antisymmetric and v is symmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c
c           sf is symmetric about the equator. in this case w is
c           symmetric and v is antisymmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of scalar and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            the arrays a,b,v, and w can be three dimensional corresponding
c            to an indexed multiple array sf.  in this case, multiple
c            vector synthesis will be performed to compute each vector
c            field.  the third index for a,b,v, and w is the synthesis
c            index which assumes the values k = 1,...,nt.  for a single
c            synthesis set nt = 1.  the description of the remaining
c            parameters is simplified by assuming that nt=1 or that a,b,v,
c            and w are two dimensional arrays.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls gradgs. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls gradgs. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field array sf as computed by subroutine shags.
c     ***    a,b must be computed by shags prior to calling gradgs.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls gradgs (and shags). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls gradgs (and shags). ndab must be at
c            least nlat.
c
c
c     wvhsgs an array which must be initialized by subroutine vhsgsi.
c            once initialized,
c            wvhsgs can be used repeatedly by gradgs as long as nlon
c            and nlat remain unchanged.  wvhsgs must not be altered
c            between calls of gradgs.
c
c
c     lvhsgs the dimension of the array wvhsgs as it appears in the
c            program that calls grradgs.  define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgs must be at least
c
c                 l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls gradgs. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                  if nlat is even or
c               l2 = (nlat+1)/2              if nlat is odd
c
c            if isym = 0, lwork must be greater than or equal to
c
c               nlat*((2*nt+1)*nlon+2*l1*nt+1).
c
c            if isym = 1 or 2, lwork must be greater than or equal to
c
c               (2*nt+1)*l2*nlon+nlat*(2*l1*nt+1).
c
c
c     **************************************************************
c
c     output parameters
c
c
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain an irrotational vector field such that the gradient of
c           the scalar field sf is (v,w).  w(i,j) is the east longitude
c           component and v(i,j) is the colatitudinal component of velocity
c           at gaussian colatitude and longitude lambda(j) = (j-1)*2*pi/nlon
c           the indices for v and w are defined at the input parameter
c           isym.  the vorticity of (v,w) is zero.  note that any nonzero
c           vector field on the sphere will be multiple valued at the poles
c           [reference swarztrauber].
c
c
c  ierror   = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idvw
c           = 6  error in the specification of jdvw
c           = 7  error in the specification of mdab
c           = 8  error in the specification of ndab
c           = 9  error in the specification of lvhsgs
c           = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
c   
      subroutine gradgs(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
     +wvhsgs,lvhsgs,work,lwork,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wvhsgs(lvhsgs),work(lwork)
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
      if((isym.eq.0 .and. idvw.lt.nlat) .or.
     +   (isym.ne.0 .and. idvw.lt.imid)) return
      ierror = 6
      if(jdvw .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+1)/2)
      if(mdab .lt. min0(nlat,(nlon+2)/2)) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
c
c     verify minimum saved work space length
c
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      lgdmin = lzimn+lzimn+nlon+15
      if(lvhsgs .lt. lgdmin) return
      ierror = 10
c
c     verify minimum unsaved work space length
c
      mn = mmax*nlat*nt
      idv = nlat
      if (isym.ne.0) idv = imid
      lnl = nt*idv*nlon
      lwkmin =  lnl+lnl+idv*nlon+2*mn+nlat
      if(lwork .lt. lwkmin) return
      ierror = 0
c
c     set work space pointers
c
      ibr = 1
      ibi = ibr + mn
      is = ibi + mn
      iwk = is + nlat
      liwk = lwork-2*mn-nlat
      call gradgs1(nlat,nlon,isym,nt,v,w,idvw,jdvw,work(ibr),work(ibi),
     +mmax,work(is),mdab,ndab,a,b,wvhsgs,lvhsgs,work(iwk),liwk,
     +ierror)
      return
      end

      subroutine gradgs1(nlat,nlon,isym,nt,v,w,idvw,jdvw,br,bi,mmax,
     +sqnn,mdab,ndab,a,b,wvhsgs,lvhsgs,wk,lwk,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt)
      dimension br(mmax,nlat,nt),bi(mmax,nlat,nt),sqnn(nlat)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wvhsgs(lvhsgs),wk(lwk)
c
c     preset coefficient multiplyers in vector
c
      do 1 n=2,nlat
      fn = float(n-1)
      sqnn(n) = sqrt(fn*(fn+1.))
    1 continue
c
c     compute multiple vector fields coefficients
c
      do 2 k=1,nt
c
c     preset br,bi to 0.0
c
      do 3 n=1,nlat
      do 4 m=1,mmax
      br(m,n,k) = 0.0
      bi(m,n,k) = 0.0
    4 continue
    3 continue
c
c     compute m=0 coefficients
c
      do 5 n=2,nlat
      br(1,n,k) = sqnn(n)*a(1,n,k)
      bi(1,n,k) = sqnn(n)*b(1,n,k)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      br(m,n,k) = sqnn(n)*a(m,n,k)
      bi(m,n,k) = sqnn(n)*b(m,n,k)
    7 continue
    6 continue
    2 continue
c
c     set ityp for irrotational vector synthesis to compute gradient
c
      if (isym.eq.0) then
      ityp = 1
      else if (isym.eq.1) then
      ityp = 4
      else if (isym.eq.2) then
      ityp = 7
      end if
c
c     vector sythesize br,bi into (v,w) (cr,ci are dummy variables)
c
      call vhsgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +           mmax,nlat,wvhsgs,lvhsgs,wk,lwk,ierror)
      return
      end
