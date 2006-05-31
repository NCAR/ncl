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
c ... file shigs.f
c
c     this file contains code and documentation for subroutine shigs
c
c ... files which must be loaded with shigs.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     3/6/98
c
c *** shigs is functionally the same as shagsi or shsgsi.  It
c     it included in spherepack3.0 because legacy codes, using
c     the older version of spherepack, call shigs to initialize
c     the saved work space wshigs for either shags or shsgs
c     Its arguments are identical to those of shagsi or shsgsi.
c
c ****************************************************************
c
c     subroutine shigs(nlat,nlon,wshigs,lshigs,work,lwork,dwork,ldwork,
c    +                 ierror)
c
c     subroutine shigs initializes the array wshigs which can then
c     be used repeatedly by subroutines shags,shsgs. it precomputes
c     and stores in wshigs quantities such as gaussian weights,
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
c     wshigs an array which must be initialized by subroutine shigs .
c            once initialized, wshigs can be used repeatedly by shigs
c            as long as nlat and nlon remain unchanged.  wshigs must
c            not be altered between calls of shigs.
c
c     lshigs the dimension of the array wshigs as it appears in the
c            program that calls shigs. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshigs must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shigs. lwork must be at least
c            4*nlat*(nlat+2)+2 in the routine calling shigs
c
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the length of dwork in the calling routine.  ldwork must
c             be at least nlat*(nlat+4)
c
c     output parameter
c
c     wshags an array which must be initialized before calling shags or
c            once initialized, wshags can be used repeatedly by shags or
c            as long as nlat and nlon remain unchanged.  wshags must not
c            altered between calls of shasc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshags
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c            = 6  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
c
      subroutine shigs(nlat,nlon,wshigs,lshigs,work,lwork,dwork,
     +                 ldwork,ierror)
c
c     this subroutine must be called before calling shags or shsgs with
c     fixed nlat,nlon. it precomputes the gaussian weights, points
c     and all necessary legendre polys and stores them in wshigs.
c     these quantities must be preserved when calling shsgs or shags
c     repeatedly with fixed nlat,nlon.
c
      dimension wshigs(lshigs),work(lwork)
      double precision dwork(ldwork)
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
c     set triangular truncation limit for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set equator or nearest point (if excluded) pointer
      late = (nlat+1)/2
      l1 = l
      l2 = late
c     check permanent work space length
      ierror = 3
      lp=nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
      if(lshigs.lt.lp) return
      ierror = 4
c     check temporary work space
      if (lwork.lt.4*nlat*(nlat+2)+2) return
c     check temp double precision space
      ierror = 5
      if (ldwork .lt. nlat*(nlat+4)) return
      ierror = 0
c     set preliminary quantites needed to compute and store legendre polys
      call shigsp(nlat,nlon,wshigs,lshigs,dwork,ldwork,ierror)
      if (ierror.ne.0) return
c     set legendre poly pointer in wshigs
      ipmnf = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+nlon+16
      call shigss1(nlat,l,late,wshigs,work,wshigs(ipmnf))
      return
      end

      subroutine shigss1(nlat,l,late,w,pmn,pmnf)
      dimension w(1),pmn(nlat,late,3),pmnf(late,1)
c     compute and store legendre polys for i=1,...,late,m=0,...,l-1
c     and n=m,...,l-1
      do i=1,nlat
	do j=1,late
	  do k=1,3
	    pmn(i,j,k) = 0.0
	  end do
	end do
      end do
      do 100 mp1=1,l
      m = mp1-1
      mml1 = m*(2*nlat-m-1)/2
c     compute pmn for n=m,...,nlat-1 and i=1,...,(l+1)/2
      mode = 0
      call legin(mode,l,nlat,m,w,pmn,km)
c     store above in pmnf
      do 101 np1=mp1,nlat
      mn = mml1+np1
      do 102 i=1,late
      pmnf(i,mn) = pmn(np1,i,km)
  102 continue
  101 continue
  100 continue
      return
      end
      subroutine shigsp(nlat,nlon,wshigs,lshigs,dwork,ldwork,ierror)
      dimension wshigs(lshigs)
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
      if (lshigs .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
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
      call shigsp1(nlat,nlon,l,late,wshigs(i1),wshigs(i2),wshigs(i3),
     1wshigs(i4),wshigs(i5),wshigs(i6),wshigs(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end

      subroutine shigsp1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
     +                   wfft,dtheta,dwts,work,ier)
      dimension wts(nlat),p0n(nlat,late),p1n(nlat,late),abel(1),bbel(1),
     1 cbel(1),wfft(1),dtheta(nlat),dwts(nlat)
      double precision pb,dtheta,dwts,work(*)
      indx(m,n) = (n-1)*(n-2)/2+m-1
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1
      call hrffti(nlon,wfft)
c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+2)
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
c
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
