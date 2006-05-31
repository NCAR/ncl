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
c ... file shigc.f
c
c     this file contains code and documentation for subroutine shigc
c
c ... files which must be loaded with shigc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     3/6/98
c
c *** shigc is functionally the same as shagci or shsgci.  It
c     it included in spherepack3.0 because legacy codes, using
c     the older version of spherepack call shigc to initialize
c     the saved work space wshigc, for either shagc or shsgc
c
c     subroutine shigc(nlat,nlon,wshigc,lshigc,dwork,ldwork,ierror)
c
c     subroutine shigc initializes the array wshigc which can then
c     be used repeatedly by subroutines shsgc or shagc. it precomputes
c     and stores in wshigc quantities such as gaussian weights,
c     legendre polynomial coefficients, and fft trigonometric tables.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
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
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     wshigc an array which must be initialized by subroutine shigc.
c            once initialized, wshigc can be used repeatedly by shsgc
c            or shagc as long as nlat and nlon remain unchanged.  wshigc
c            must not be altered between calls of shsgc or shagc.
c
c     lshigc the dimension of the array wshigc as it appears in the
c            program that calls shsgc or shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshigc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shigc. ldwork must be at least
c
c               nlat*(nlat+4)
c
c     output parameter
c
c     wshigc an array which must be initialized before calling shsgc or shagc.
c            once initialized, wshigc can be used repeatedly by shsgc or shagc
c            as long as nlat and nlon remain unchanged.  wshigc must not
c            altered between calls of shsgc or shagc
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshigc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      subroutine shigc(nlat,nlon,wshigc,lshigc,dwork,ldwork,ierror)
c     this subroutine must be called before calling shsgc/shagc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      dimension wshigc(lshigc)
      double precision dwork(ldwork)
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
c     set triangular truncation limit for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set equator or nearest point (if excluded) pointer
      late = (nlat+mod(nlat,2))/2
      l1 = l
      l2 = late
      ierror = 3
c     check permanent work space length
      if (lshigc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
c     if (lwork.lt.4*nlat*(nlat+2)+2) return
      if (ldwork .lt. nlat*(nlat+4)) return
      ierror = 0
c     set pointers
      i1 = 1
      i2 = i1+nlat
      i3 = i2+nlat*late
      i4 = i3+nlat*late
      i5 = i4+l*(l-1)/2 +(nlat-l)*(l-1)
      i6 = i5+l*(l-1)/2 +(nlat-l)*(l-1)
      i7 = i6+l*(l-1)/2 +(nlat-l)*(l-1)
c     set indices in temp work for double precision gaussian wts and pts
      idth = 1
c     idwts = idth+2*nlat
c     iw = idwts+2*nlat
      idwts = idth+nlat
      iw = idwts+nlat
      call shigc1(nlat,nlon,l,late,wshigc(i1),wshigc(i2),wshigc(i3),
     1wshigc(i4),wshigc(i5),wshigc(i6),wshigc(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end
      subroutine shigc1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
     1                  wfft,dtheta,dwts,work,ier)
      dimension wts(nlat),p0n(nlat,late),p1n(nlat,late),abel(1),bbel(1),
     1 cbel(1),wfft(1),dtheta(nlat),dwts(nlat)
      double precision pb,dtheta,dwts,work(*)
c     compute the nlat  gaussian points and weights, the
c     m=0,1 legendre polys for gaussian points and all n,
c     and the legendre recursion coefficients
c     define index function used in storing
c     arrays for recursion coefficients (functions of (m,n))
c     the index function indx(m,n) is defined so that
c     the pairs (m,n) map to [1,2,...,indx(l-1,l-1)] with no
c     "holes" as m varies from 2 to n and n varies from 2 to l-1.
c     (m=0,1 are set from p0n,p1n for all n)
c     define for 2.le.n.le.l-1
      indx(m,n) = (n-1)*(n-2)/2+m-1
c     define index function for l.le.n.le.nlat
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1
c     preset quantites for fourier transform
      call hrffti(nlon,wfft)
c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+1)+2
      lw = nlat*(nlat+2)
      call gaqd(nlat,dtheta,dwts,work,lw,ier)
      if (ier.ne.0) return
c     store gaussian weights single precision to save computation
c     in inner loops in analysis
      do 100 i=1,nlat
      wts(i) = dwts(i)
  100 continue
c     initialize p0n,p1n using double precision dnlfk,dnlft
      do 101 np1=1,nlat
      do 101 i=1,late
      p0n(np1,i) = 0.0
      p1n(np1,i) = 0.0
  101 continue
c     compute m=n=0 legendre polynomials for all theta(i)
      np1 = 1
      n = 0
      m = 0
      call dnlfk(m,n,work)
      do 103 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(1,i) = pb
  103 continue
c     compute p0n,p1n for all theta(i) when n.gt.0
      do 104 np1=2,nlat
      n = np1-1
      m = 0
      call dnlfk(m,n,work)
      do 105 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(np1,i) = pb
  105 continue
c     compute m=1 legendre polynomials for all n and theta(i)
      m = 1
      call dnlfk(m,n,work)
      do 106 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p1n(np1,i) = pb
  106 continue
  104 continue
c     compute and store swarztrauber recursion coefficients
c     for 2.le.m.le.n and 2.le.n.le.nlat in abel,bbel,cbel
      do 107 n=2,nlat
      mlim = min0(n,l)
      do 107 m=2,mlim
      imn = indx(m,n)
      if (n.ge.l) imn = imndx(m,n)
      abel(imn)=sqrt(float((2*n+1)*(m+n-2)*(m+n-3))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      bbel(imn)=sqrt(float((2*n+1)*(n-m-1)*(n-m))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      cbel(imn)=sqrt(float((n-m+1)*(n-m+2))/
     1               float(((n+m-1)*(n+m))))
  107 continue
      return
      end
