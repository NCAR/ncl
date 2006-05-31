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
c ... file shagc.f
c
c     this file contains code and documentation for subroutines
c     shagc and shagci
c
c ... files which must be loaded with shagc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c
c     subroutine shagc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshagc,lshagc,work,lwork,ierror)
c
c     subroutine shagc performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on a gaussian grid in colatitude
c     and an equally spaced grid in longitude.  the associated
c     legendre functions are recomputed rather than stored as they
c     are in subroutine shags.  the analysis is described below
c     at output parameters a,b.
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
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shagc,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the gaussian
c            point theta(i) and longitude point phi(j) = (j-1)*2*pi/nlon
c            the index ranges are defined above at the input parameter
c            isym.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shagc. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shagc. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shagc. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc.
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shagc. define
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
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shagc. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta).
c
c     2. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon of
c                       g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon of
c                       g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c
c     3. the gaussian points and weights on the sphere
c        (computed by subroutine gaqd).
c
c        theta(1),...,theta(nlat) (gaussian pts in radians)
c        wts(1),...,wts(nlat) (corresponding gaussian weights)
c
c     4. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)     =  the sum from i=1 to i=nlat of
c                       c(m+1,i)*wts(i)*pbar(m,n,theta(i))
c
c     b(m+1,n+1)      = the sum from i=1 to nlat of
c                       s(m+1,i)*wts(i)*pbar(m,n,theta(i))
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
c            = 9  error in the specification of lshagc
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
c
c     subroutine shagci initializes the array wshagc which can then
c     be used repeatedly by subroutines shagc. it precomputes
c     and stores in wshagc quantities such as gaussian weights,
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
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the dimension of the array dwork as it appears in the
c            program that calls shagci. ldwork must be at least
c
c                nlat*(nlat+4)
c
c     output parameter
c
c     wshagc an array which must be initialized before calling shagc or
c            once initialized, wshagc can be used repeatedly by shagc or
c            as long as nlat and nlon remain unchanged.  wshagc must not
c            altered between calls of shagc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshagc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      subroutine shagc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshagc,lshagc,work,lwork,ierror)
c     subroutine shagc performs the spherical harmonic analysis on
c     a gaussian grid on the array(s) in g and returns the coefficients
c     in array(s) a,b. the necessary legendre polynomials are computed
c     as needed in this version.
c
      dimension g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          wshagc(lshagc),work(lwork)
c     check input parameters
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
      ierror = 3
      if (isym.lt.0 .or.isym.gt.2) return
      ierror = 4
      if (nt.lt.1) return
c     set upper limit on m for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set gaussian point nearest equator pointer
      late = (nlat+mod(nlat,2))/2
c     set number of grid points for analysis/synthesis
      lat = nlat
      if (isym.ne.0) lat = late
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
      if (lshagc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 10
c     check temporary work space length
      if (isym.eq.0) then
      if(lwork.lt.nlat*(nlon*nt+max0(3*l2,nlon))) return
      else
c     isym.ne.0
      if(lwork.lt.l2*(nlon*nt+max0(3*nlat,nlon))) return
      end if
      ierror = 0
c     starting address for gaussian wts in shigc and fft values
      iwts = 1
      ifft = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+1
c     set pointers for internal storage of g and legendre polys
      ipmn = lat*nlon*nt+1
      call shagc1(nlat,nlon,l,lat,isym,g,idg,jdg,nt,a,b,mdab,ndab,
     1wshagc,wshagc(iwts),wshagc(ifft),late,work(ipmn),work)
      return
      end
      subroutine shagc1(nlat,nlon,l,lat,mode,gs,idg,jdg,nt,a,b,mdab,
     1                  ndab,w,wts,wfft,late,pmn,g)
      dimension gs(idg,jdg,nt),a(mdab,ndab,nt),
     1          b(mdab,ndab,nt),g(lat,nlon,nt)
      dimension w(1),wts(nlat),wfft(1),pmn(nlat,late,3)
c     set gs array internally in shagc1
      do 100 k=1,nt
      do 100 j=1,nlon
      do 100 i=1,lat
      g(i,j,k) = gs(i,j,k)
  100 continue
c     do fourier transform
      do 101 k=1,nt
      call hrfftf(lat,nlon,g(1,1,k),lat,wfft,pmn)
  101 continue
c     scale result
      sfn = 2.0/float(nlon)
      do 102 k=1,nt
      do 102 j=1,nlon
      do 102 i=1,lat
      g(i,j,k) = sfn*g(i,j,k)
  102 continue
c     compute using gaussian quadrature
c     a(n,m) = s (ga(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     b(n,m) = s (gb(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     here ga,gb are the cos(phi),sin(phi) coefficients of
c     the fourier expansion of g(theta,phi) in phi.  as a result
c     of the above fourier transform they are stored in array
c     g as follows:
c     for each theta(i) and k= l-1
c     ga(0),ga(1),gb(1),ga(2),gb(2),...,ga(k-1),gb(k-1),ga(k)
c     correspond to (in the case nlon=l+l-2)
c     g(i,1),g(i,2),g(i,3),g(i,4),g(i,5),...,g(i,2l-4),g(i,2l-3),g(i,2l-
c     initialize coefficients to zero
      do 103 k=1,nt
      do 103 np1=1,nlat
      do 103 mp1=1,l
      a(mp1,np1,k) = 0.0
      b(mp1,np1,k) = 0.0
  103 continue
c     set m+1 limit on b(m+1) calculation
      lm1 = l
      if (nlon .eq. l+l-2) lm1 = l-1
      if (mode.eq.0) then
c     for full sphere (mode=0) and even/odd reduction:
c     overwrite g(i) with (g(i)+g(nlat-i+1))*wts(i)
c     overwrite g(nlat-i+1) with (g(i)-g(nlat-i+1))*wts(i)
      nl2 = nlat/2
      do 104 k=1,nt
      do 104 j=1,nlon
      do 105 i=1,nl2
      is = nlat-i+1
      t1 = g(i,j,k)
      t2 = g(is,j,k)
      g(i,j,k) = wts(i)*(t1+t2)
      g(is,j,k) = wts(i)*(t1-t2)
  105 continue
c     adjust equator if necessary(nlat odd)
      if (mod(nlat,2).ne.0) g(late,j,k) = wts(late)*g(late,j,k)
  104 continue
c     set m = 0 coefficients first
      m = 0
      call legin(mode,l,nlat,m,w,pmn,km)
      do 106 k=1,nt
      do 106 i=1,late
      is = nlat-i+1
      do 107 np1=1,nlat,2
c     n even
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(np1,i,km)
  107 continue
      do 108 np1=2,nlat,2
c     n odd
      a(1,np1,k) = a(1,np1,k)+g(is,1,k)*pmn(np1,i,km)
  108 continue
  106 continue
c     compute coefficients for which b(m,n) is available
      do 109 mp1=2,lm1
      m = mp1-1
      mp2 = m+2
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 110 k=1,nt
      do 111 i=1,late
      is = nlat-i+1
c     n-m even
      do 112 np1=mp1,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(np1,i,km)
  112 continue
c     n-m odd
      do 113 np1=mp2,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(is,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(is,2*m+1,k)*pmn(np1,i,km)
  113 continue
  111 continue
  110 continue
  109 continue
      if (nlon .eq. l+l-2) then
c     compute a(l,np1) coefficients only
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 114 k=1,nt
      do 114 i=1,late
      is = nlat-i+1
c     n-m even
      do 124 np1=l,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(np1,i,km)
  124 continue
      lp1 = l+1
c     n-m odd
      do 125 np1=lp1,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(is,nlon,k)*pmn(np1,i,km)
  125 continue
  114 continue
      end if
      else
c     half sphere
c     overwrite g(i) with wts(i)*(g(i)+g(i)) for i=1,...,nlate/2
      nl2 = nlat/2
      do 116  k=1,nt
      do 116 j=1,nlon
      do 115 i=1,nl2
      g(i,j,k) = wts(i)*(g(i,j,k)+g(i,j,k))
  115 continue
c     adjust equator separately if a grid point
      if (nl2.lt.late) g(late,j,k) = wts(late)*g(late,j,k)
  116 continue
c     set m = 0 coefficients first
      m = 0
      call legin(mode,l,nlat,m,w,pmn,km)
      ms = 1
      if (mode.eq.1) ms = 2
      do 117 k=1,nt
      do 117 i=1,late
      do 117 np1=ms,nlat,2
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(np1,i,km)
  117 continue
c     compute coefficients for which b(m,n) is available
      do 118 mp1=2,lm1
      m = mp1-1
      ms = mp1
      if (mode.eq.1) ms = mp1+1
c     compute pmn for all i and n=m,...,nlat-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 119 k=1,nt
      do 119 i=1,late
      do 119 np1=ms,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(np1,i,km)
  119 continue
  118 continue
      if (nlon.eq.l+l-2) then
c     compute coefficient a(l,np1) only
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      ns = l
      if (mode.eq.1) ns = l+1
      do 120 k=1,nt
      do 120 i=1,late
      do 120 np1=ns,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(np1,i,km)
  120 continue
      end if
      end if
      return
      end
      subroutine shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
c     this subroutine must be called before calling shagc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      dimension wshagc(lshagc)
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
      if (lshagc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
      if (ldwork.lt.nlat*(nlat+4))return
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
      call shagci1(nlat,nlon,l,late,wshagc(i1),wshagc(i2),wshagc(i3),
     1wshagc(i4),wshagc(i5),wshagc(i6),wshagc(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end
      subroutine shagci1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
     1                  wfft,dtheta,dwts,work,ier)
      dimension wts(nlat),p0n(nlat,late),p1n(nlat,late),abel(1),bbel(1),
     1 cbel(1),wfft(1)
      double precision pb,dtheta(nlat),dwts(nlat),work(*)
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
