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
c ... file idivec.f
c
c     this file includes documentation and code for
c     subroutine idivgs          i
c
c ... files which must be loaded with idivgs.f
c
c     sphcom.f, hrfft.f, vhsgs.f,shags.f
c
c
c     subroutine idivgs(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsgs,lvhsgs,work,lwork,pertrb,ierror)
c
c     given the scalar spherical harmonic coefficients a and b, precomputed
c     by subroutine shags for a scalar array divg, subroutine idivgs computes
c     an irrotational vector field (v,w) whose divergence is divg - pertrb.
c     w is the east longitude component and v is the colatitudinal component.
c     pertrb is a constant which must be subtracted from divg for (v,w) to
c     exist (see the description of pertrb below).  usually pertrb is zero
c     or small relative to divg.  the vorticity of (v,w) is the zero scalar
c     field.  v(i,j) and w(i,j) are the velocity components at the gaussian
c     colatitude theta(i) (see nlat) and longitude lambda(j)=(j-1)*2*pi/nlon.
c     the
c
c            divergence[v(i,j),w(i,j)]
c
c          = [d(w(i,j)/dlambda + d(sint*v(i,j))/dtheta]/sint
c
c          = divg(i,j) - pertrb
c
c     and
c
c            vorticity(v(i,j),w(i,j))
c
c         =  [dv/dlambda - d(sint*w)/dtheta]/sint
c
c         =  0.0
c
c     where sint = sin(theta(i)).
c
c     input parameters
c
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
c            scalar field divg.  isym determines whether (v,w) are
c            computed on the full or half sphere as follows:
c
c      = 0
c
c           divg is not symmetric about the equator. in this case
c           the vector field (v,w) is computed on the entire sphere.
c           i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c           j=1,...,nlon.
c
c      = 1
c
c           divg is antisymmetric about the equator. in this case w is
c           antisymmetric and v is symmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c
c           divg is symmetric about the equator. in this case w is
c           symmetric and v is antisymmetric about the equator. w
c           and v are computed on the northern hemisphere only.  i.e.,
c           if nlat is odd they are computed for i=1,...,(nlat+1)/2
c           and j=1,...,nlon.  if nlat is even they are computed for
c           i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     nt is the number of divergence and vector fields.  some
c            computational efficiency is obtained for multiple fields.
c            the arrays a,b,v, and w can be three dimensional and pertrb
c            can be one dimensional corresponding to an indexed multiple
c            array divg.  in this case, multiple vector synthesis will be
c            performed to compute each vector field.  the third index for
c            a,b,v,w and first for pertrb is the synthesis index which
c            assumes the values k = 1,...,nt.  for a single synthesis set
c            nt = 1.  the description of the remaining parameters is
c            simplified by assuming that nt=1 or that a,b,v,w are two
c            dimensional and pertrb is a constant.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls idivgs. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls idivgs. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the divergence array divg as computed by subroutine shags.
c     ***    a,b must be computed by shags prior to calling idivgs.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls idivgs (and shags). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls idivgs (and shags). ndab must be at
c            least nlat.
c
c
c  wvhsgs    an array which must be initialized by subroutine vhsgsi.
c            once initialized,
c            wvhsgs can be used repeatedly by idivgs as long as nlon
c            and nlat remain unchanged.  wvhsgs must not be altered
c            between calls of idivgs.
c
c
c  lvhsgs    the dimension of the array wvhsgs as it appears in the
c            program that calls idivgs. define
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
c               l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls idivgs. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2                  if nlat is even or
c               l2 = (nlat+1)/2              if nlat is odd
c
c            if isym = 0 then lwork must be at least
c
c               (2*nt+1)*nlat*nlon + nlat*(2*nt*l1+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               (2*nt+1)*l2*nlon + nlat*(2*nt*l1+1)
c
c     **************************************************************
c
c     output parameters
c
c
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain an irrotational vector field whose divergence is
c           divg-pertrb at the guassian colatitude point theta(i) and
c           longitude point lambda(j)=(j-1)*2*pi/nlon.  w is the east
c           longitude component and v is the colatitudinal component.  the
c           indices for w and v are defined at the input parameter isym.
c           the curl or vorticity of (v,w) is the zero vector field.  note
c           that any nonzero vector field on the sphere will be multiple
c           valued at the poles [reference swarztrauber].
c
c   pertrb  a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  divg - pertrb is a scalar
c           field which can be the divergence of a vector field (v,w).
c           pertrb is related to the scalar harmonic coefficients a,b
c           of divg (computed by shags) by the formula
c
c                pertrb = a(1,1)/(2.*sqrt(2.))
c
c
c
c           the unperturbed scalar field divg can be the divergence of a
c           vector field only if a(1,1) is zero.  if a(1,1) is nonzero
c           (flagged by pertrb nonzero) then subtracting pertrb from
c           divg yields a scalar field for which a(1,1) is zero.
c
c    ierror = 0  no errors
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
      subroutine idivgs(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
     +                  wvhsgs,lvhsgs,work,lwork,pertrb,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt),pertrb(nt)
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
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      l1 = min0(nlat,(nlon+1)/2)
      l2 = (nlat+1)/2
      lwmin = l1*l2*(nlat+nlat-l1+1)+nlon+15
      if(lvhsgs .lt. lwmin) return
      ierror = 10
c
c     verify unsaved work space length
c
      mn = mmax*nlat*nt
      if(isym.ne.0  .and. lwork .lt.
     +nlat*(2*nt*nlon+max0(6*imid,nlon))+2*mn+nlat) return
      if(isym.eq.0  .and. lwork .lt.
     +imid*(2*nt*nlon+max0(6*nlat,nlon))+2*mn+nlat) return
      ierror = 0
c
c     set work space pointers
c
      ibr = 1
      ibi = ibr + mn
      is = ibi + mn
      iwk = is + nlat
      liwk = lwork-2*mn-nlat
      call idvgs1(nlat,nlon,isym,nt,v,w,idvw,jdvw,work(ibr),work(ibi),
     +            mmax,work(is),mdab,ndab,a,b,wvhsgs,lvhsgs,work(iwk),
     +            liwk,pertrb,ierror)
      return
      end

      subroutine idvgs1(nlat,nlon,isym,nt,v,w,idvw,jdvw,br,bi,mmax,
     +sqnn,mdab,ndab,a,b,wsav,lwsav,wk,lwk,pertrb,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt),pertrb(nt)
      dimension br(mmax,nlat,nt),bi(mmax,nlat,nt),sqnn(nlat)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wsav(lwsav),wk(lwk)
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
c     set divergence field perturbation adjustment
c
      pertrb(k) = a(1,1,k)/(2.*sqrt(2.))
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
      br(1,n,k) = -a(1,n,k)/sqnn(n)
      bi(1,n,k) = -b(1,n,k)/sqnn(n)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      br(m,n,k) = -a(m,n,k)/sqnn(n)
      bi(m,n,k) = -b(m,n,k)/sqnn(n)
    7 continue
    6 continue
    2 continue
c
c     set ityp for vector synthesis with curl=0
c
      if (isym.eq.0) then
      ityp = 1
      else if (isym.eq.1) then
      ityp = 4
      else if (isym.eq.2) then
      ityp = 7
      end if
c
c     vector sythesize br,bi into irrotational (v,w)
c
      call vhsgs(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +           mmax,nlat,wsav,lwsav,wk,lwk,ierror)
      return
      end
