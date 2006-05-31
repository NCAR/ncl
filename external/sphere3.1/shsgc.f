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
c ... file shsgc.f
c
c     this file contains code and documentation for subroutines
c     shsgc and shsgci
c
c ... files which must be loaded with shsgc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine shsgc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshsgc,lshsgc,work,lwork,ierror)
c
c     subroutine shsgc performs the spherical harmonic synthesis
c     on the arrays a and b and stores the result in the array g.
c     the synthesis is performed on an equally spaced longitude grid
c     and a gaussian colatitude grid.  the associated legendre functions
c     are recomputed rather than stored as they are in subroutine
c     shsgs.  the synthesis is described below at output parameter
c     g.
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
c     isym   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the synthesis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the synthesis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls shsgc,
c            the arrays g,a and b can be three dimensional in which
c            case multiple synthesis will be performed.  the third
c            index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shsgc. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shsgc. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shsgc. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shsgc. ndab must be at least nlat
c
c     a,b    two or three dimensional arrays (see the input parameter
c            nt) that contain the coefficients in the spherical harmonic
c            expansion of g(i,j) given below at the definition of the
c            output parameter g.  a(m,n) and b(m,n) are defined for
c            indices m=1,...,mmax and n=m,...,nlat where mmax is the
c            maximum (plus one) longitudinal wave number given by
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     wshsgc an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must
c            not be altered between calls of shsgc.
c
c     lshsgc the dimension of the array wshsgc as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c               nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                      nlat*(nlon*nt+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                      l2*(nlon*nt+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     g      a two or three dimensional array (see input parameter nt)
c            that contains the discrete function which is synthesized.
c            g(i,j) contains the value of the function at the gaussian
c            colatitude point theta(i) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.  for isym=0, g(i,j)
c            is given by the the equations listed below.  symmetric
c            versions are used when isym is greater than zero.
c
c     the normalized associated legendre functions are given by
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c
c     define the maximum (plus one) longitudinal wave number
c     as   mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c          mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then g(i,j) = the sum from n=0 to n=nlat-1 of
c
c                   .5*pbar(0,n,theta(i))*a(1,n+1)
c
c              plus the sum from m=1 to m=mmax-1 of
c
c                   the sum from n=m to n=nlat-1 of
c
c              pbar(m,n,theta(i))*(a(m+1,n+1)*cos(m*phi(j))
c                                    -b(m+1,n+1)*sin(m*phi(j)))
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lwshig
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shsgci(nlat,nlon,wshsgc,lshsgc,dwork,ldwork,ierror)
c
c     subroutine shsgci initializes the array wshsgc which can then
c     be used repeatedly by subroutines shsgc. it precomputes
c     and stores in wshsgc quantities such as gaussian weights,
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
c     wshsgc an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must
c            not be altered between calls of shsgc.
c
c     lshsgc the dimension of the array wshsgc as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shsgci. ldwork must be at least
c
c                 nlat*(nlat+4)
c
c     output parameter
c
c     wshsgc an array which must be initialized before calling shsgc.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must not
c            altered between calls of shsgc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshsgc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      subroutine shsgc(nlat,nlon,mode,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshsgc,lshsgc,work,lwork,ierror)
c     subroutine shsgc performs the spherical harmonic synthesis on
c     a gaussian grid using the coefficients in array(s) a,b and returns
c     the results in array(s) g.  the legendre polynomials are computed
c     as needed in this version.
c
      dimension g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          wshsgc(lshsgc),work(lwork)
c     check input parameters
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
      ierror = 3
      if (mode.lt.0 .or.mode.gt.2) return
      ierror = 4
      if (nt.lt.1) return
c     set limit for m iin a(m,n),b(m,n) computation
      l = min0((nlon+2)/2,nlat)
c     set gaussian point nearest equator pointer
      late = (nlat+mod(nlat,2))/2
c     set number of grid points for analysis/synthesis
      lat = nlat
      if (mode.ne.0) lat = late
      ierror = 5
      if (idg.lt.lat) return
      ierror = 6
      if (jdg.lt.nlon) return
      ierror = 7
      if(mdab .lt. l) return
      ierror = 8
      if(ndab .lt. nlat) return
      l1 = l
      l2 = late
      ierror = 9
c     check permanent work space length
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 10
c     check temporary work space length
      if (mode.eq.0) then
      if(lwork.lt.nlat*(nlon*nt+max0(3*l2,nlon)))return
      else
c     mode.ne.0
      if(lwork.lt.l2*(nlon*nt+max0(3*nlat,nlon))) return
      end if
      ierror = 0
c     starting address  fft values
      ifft = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+1
c     set pointers for internal storage of g and legendre polys
      ipmn = lat*nlon*nt+1
      call shsgc1(nlat,nlon,l,lat,mode,g,idg,jdg,nt,a,b,mdab,ndab,
     1wshsgc,wshsgc(ifft),late,work(ipmn),work)
      return
      end
      subroutine shsgc1(nlat,nlon,l,lat,mode,gs,idg,jdg,nt,a,b,mdab,
     1                  ndab,w,wfft,late,pmn,g)
      dimension gs(idg,jdg,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension w(1),pmn(nlat,late,3),g(lat,nlon,nt),wfft(1)
c     reconstruct fourier coefficients in g on gaussian grid
c     using coefficients in a,b
c     set m+1 limit for b coefficient calculation
      lm1 = l
      if (nlon .eq. l+l-2) lm1 = l-1
c     initialize to zero
      do 100 k=1,nt
      do 100 j=1,nlon
      do 100 i=1,lat
      g(i,j,k) = 0.0
  100 continue
      if (mode.eq.0) then
c     set first column in g
      m = 0
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 101 k=1,nt
c     n even
      do 102 np1=1,nlat,2
      do 102 i=1,late
      g(i,1,k) = g(i,1,k)+a(1,np1,k)*pmn(np1,i,km)
  102 continue
c     n odd
      nl2 = nlat/2
      do 103 np1=2,nlat,2
      do 103 i=1,nl2
      is = nlat-i+1
      g(is,1,k) = g(is,1,k)+a(1,np1,k)*pmn(np1,i,km)
  103 continue
c     restore m=0 coefficents (reverse implicit even/odd reduction)
      do 112 i=1,nl2
      is = nlat-i+1
      t1 = g(i,1,k)
      t3 = g(is,1,k)
      g(i,1,k) = t1+t3
      g(is,1,k) = t1-t3
  112 continue
  101 continue
c     sweep  columns of g for which b is available
      do 104 mp1=2,lm1
      m = mp1-1
      mp2 = m+2
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 105 k=1,nt
c     for n-m even store (g(i,p,k)+g(nlat-i+1,p,k))/2 in g(i,p,k) p=2*m,
c     for i=1,...,late
      do 106 np1=mp1,nlat,2
      do 107 i=1,late
      g(i,2*m,k) = g(i,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(i,2*m+1,k) = g(i,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  107 continue
  106 continue
c     for n-m odd store g(i,p,k)-g(nlat-i+1,p,k) in g(nlat-i+1,p,k)
c     for i=1,...,nlat/2 (p=2*m,p=2*m+1)
      do 108 np1=mp2,nlat,2
      do 109 i=1,nl2
      is = nlat-i+1
      g(is,2*m,k) = g(is,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(is,2*m+1,k) = g(is,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  109 continue
  108 continue
c     now set fourier coefficients using even-odd reduction above
      do 110 i=1,nl2
      is = nlat-i+1
      t1 = g(i,2*m,k)
      t2 = g(i,2*m+1,k)
      t3 = g(is,2*m,k)
      t4 = g(is,2*m+1,k)
      g(i,2*m,k) = t1+t3
      g(i,2*m+1,k) = t2+t4
      g(is,2*m,k) = t1-t3
      g(is,2*m+1,k) = t2-t4
  110 continue
  105 continue
  104 continue
c     set last column (using a only)
      if (nlon.eq. l+l-2) then
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 111 k=1,nt
c     n-m even
      do 131 np1=l,nlat,2
      do 131 i=1,late
      g(i,nlon,k) = g(i,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  131 continue
      lp1 = l+1
c     n-m odd
      do 132 np1=lp1,nlat,2
      do 132 i=1,nl2
      is = nlat-i+1
      g(is,nlon,k) = g(is,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  132 continue
      do 133 i=1,nl2
      is = nlat-i+1
      t1 = g(i,nlon,k)
      t3 = g(is,nlon,k)
      g(i,nlon,k)= t1+t3
      g(is,nlon,k)= t1-t3
  133 continue
  111 continue
      end if
      else
c     half sphere (mode.ne.0)
c     set first column in g
      m = 0
      meo = 1
      if (mode.eq.1) meo = 2
      ms = m+meo
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 113 k=1,nt
      do 113 np1=ms,nlat,2
      do 113 i=1,late
      g(i,1,k) = g(i,1,k)+a(1,np1,k)*pmn(np1,i,km)
  113 continue
c     sweep interior columns of g
      do 114 mp1=2,lm1
      m = mp1-1
      ms = m+meo
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 115 k=1,nt
      do 115 np1=ms,nlat,2
      do 115 i=1,late
      g(i,2*m,k) = g(i,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(i,2*m+1,k) = g(i,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  115 continue
  114 continue
      if (nlon.eq.l+l-2) then
c     set last column
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      ns = l
      if (mode.eq.1) ns = l+1
      do 116 k=1,nt
      do 116 i=1,late
      do 116 np1=ns,nlat,2
      g(i,nlon,k) = g(i,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  116 continue
      end if
      end if
c     do inverse fourier transform
      do 120 k=1,nt
      call hrfftb(lat,nlon,g(1,1,k),lat,wfft,pmn)
  120 continue
c     scale output in gs
      do 122 k=1,nt
      do 122 j=1,nlon
      do 122 i=1,lat
      gs(i,j,k) = 0.5*g(i,j,k)
  122 continue
      return
      end
      subroutine shsgci(nlat,nlon,wshsgc,lshsgc,dwork,ldwork,ierror)
c     this subroutine must be called before calling shsgc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      dimension wshsgc(lshsgc)
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
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
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
      idwts = idth+nlat
      iw = idwts+nlat
      call shsgci1(nlat,nlon,l,late,wshsgc(i1),wshsgc(i2),wshsgc(i3),
     1wshsgc(i4),wshsgc(i5),wshsgc(i6),wshsgc(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end
      subroutine shsgci1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
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
