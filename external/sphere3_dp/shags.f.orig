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
c ... file shags.f
c
c     this file contains code and documentation for subroutines
c     shags and shagsi
c
c ... files which must be loaded with shags.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine shags(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    1                    wshags,lshags,work,lwork,ierror)
c
c     subroutine shags performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on a gaussian grid in colatitude
c     and an equally spaced grid in longitude.  the associated
c     legendre functions are stored rather than recomputed as they
c     are in subroutine shagc.  the analysis is described below
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
c     nt     the number of analyses.  in the program that calls shags,
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
c            program that calls shags. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shags. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shags. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shags. ndab must be at least nlat
c
c     wshags an array which must be initialized by subroutine shagsi.
c            once initialized, wshags can be used repeatedly by shags
c            as long as nlat and nlon remain unchanged.  wshags must
c            not be altered between calls of shags.
c
c     lshags the dimension of the array wshags as it appears in the
c            program that calls shags. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshags must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shags. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c
c            if isym is zero then lwork must be at least
c
c                  nlat*nlon*(nt+1)
c
c            if isym is nonzero then lwork must be at least
c
c                  l2*nlon*(nt+1)
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shags. for isym=0, a(m,n) and
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
c            = 9  error in the specification of lshags
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shagsi(nlat,nlon,wshags,lshags,work,lwork,dwork,ldwork,
c    +                  ierror)
c
c     subroutine shagsi initializes the array wshags which can then
c     be used repeatedly by subroutines shags. it precomputes
c     and stores in wshags quantities such as gaussian weights,
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
c     wshags an array which must be initialized by subroutine shagsi.
c            once initialized, wshags can be used repeatedly by shags
c            as long as nlat and nlon remain unchanged.  wshags must
c            not be altered between calls of shags.
c
c     lshags the dimension of the array wshags as it appears in the
c            program that calls shags. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshags must be at least
c
c            nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
c
c     work   a real work space which need not be saved
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shagsi. lwork must be at least
c            4*nlat*(nlat+2)+2 in the routine calling shagsi
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
      subroutine shags(nlat,nlon,mode,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshags,lshags,work,lwork,ierror)
c     subroutine shags performs the spherical harmonic analysis on
c     a gaussian grid on the array(s) in g and returns the coefficients
c     in array(s) a,b. the necessary legendre polynomials are fully
c     stored in this version.
c
      dimension g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          wshags(lshags),work(lwork)
c     check input parameters
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
      ierror = 3
      if (mode.lt.0 .or.mode.gt.2) return
c     set m limit for pmn
      l = min0((nlon+2)/2,nlat)
c     set gaussian point nearest equator pointer
      late = (nlat+mod(nlat,2))/2
c     set number of grid points for analysis/synthesis
      lat = nlat
      if (mode.ne.0) lat = late
      ierror = 4
      if (nt.lt.1) return
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
c
      lp= nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15
      if(lshags.lt.lp) return
      ierror = 10
c     check temporary work space length
      if (mode.eq.0 .and. lwork.lt.nlat*nlon*(nt+1)) return
      if (mode.ne.0 .and. lwork.lt.l2*nlon*(nt+1)) return
      ierror = 0
c     set starting address for gaussian wts ,fft values,
c     and fully stored legendre polys in wshags
      iwts = 1
      ifft = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+1
      ipmn = ifft+nlon+15
c     set pointer for internal storage of g
      iw = lat*nlon*nt+1
      call shags1(nlat,nlon,l,lat,mode,g,idg,jdg,nt,a,b,mdab,ndab,
     1wshags(iwts),wshags(ifft),wshags(ipmn),late,work,work(iw))
      return
      end

      subroutine shags1(nlat,nlon,l,lat,mode,gs,idg,jdg,nt,a,b,mdab,
     1                  ndab,wts,wfft,pmn,late,g,work)
      dimension gs(idg,jdg,nt),a(mdab,ndab,nt),
     1          b(mdab,ndab,nt),g(lat,nlon,nt)
      dimension wfft(1),pmn(late,1),wts(nlat),work(1)
c     set gs array internally in shags1
      do 100 k=1,nt
      do 100 j=1,nlon
      do 100 i=1,lat
      g(i,j,k) = gs(i,j,k)
  100 continue

c     do fourier transform
      do 101 k=1,nt
      call hrfftf(lat,nlon,g(1,1,k),lat,wfft,work)
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
c     correspond to
c     g(i,1),g(i,2),g(i,3),g(i,4),g(i,5),...,g(i,2l-4),g(i,2l-3),g(i,2l-2)
c     whenever 2*l-2 = nlon exactly
c     initialize coefficients to zero
      do 103 k=1,nt
      do 103 np1=1,nlat
      do 103 mp1=1,l
      a(mp1,np1,k) = 0.0
      b(mp1,np1,k) = 0.0
  103 continue
c     set mp1 limit on b(mp1) calculation
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
      mp1 = 1
      m = 0
      mml1 = m*(2*nlat-m-1)/2
      do 106 k=1,nt
      do 106 i=1,late
      is = nlat-i+1
      do 107 np1=1,nlat,2
c     n even
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(i,mml1+np1)
  107 continue
      do 108 np1=2,nlat,2
c     n odd
      a(1,np1,k) = a(1,np1,k)+g(is,1,k)*pmn(i,mml1+np1)
  108 continue
  106 continue
c     compute m.ge.1  coefficients next
      do 109 mp1=2,lm1
      m = mp1-1
      mml1 = m*(2*nlat-m-1)/2
      mp2 = mp1+1
      do 110 k=1,nt
      do 111 i=1,late
      is = nlat-i+1
c     n-m even
      do 112 np1=mp1,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(i,mml1+np1)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(i,mml1+np1)
  112 continue
c     n-m odd
      do 113 np1=mp2,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(is,2*m,k)*pmn(i,mml1+np1)
      b(mp1,np1,k) = b(mp1,np1,k)+g(is,2*m+1,k)*pmn(i,mml1+np1)
  113 continue
  111 continue
  110 continue
  109 continue
      if (nlon .eq. l+l-2) then
c     compute m=l-1, n=l-1,l,...,nlat-1 coefficients
      m = l-1
      mml1 = m*(2*nlat-m-1)/2
      do 114 k=1,nt
      do 114 i=1,late
      is = nlat-i+1
      do 124 np1=l,nlat,2
      mn = mml1+np1
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(i,mn)
  124 continue
c     n-m  odd
      lp1 = l+1
      do 125 np1=lp1,nlat,2
      mn = mml1+np1
      a(l,np1,k) = a(l,np1,k)+0.5*g(is,nlon,k)*pmn(i,mn)
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
      mp1 = 1
      m = 0
      mml1 = m*(2*nlat-m-1)/2
      ms = 1
      if (mode.eq.1) ms = 2
      do 117 k=1,nt
      do 117 i=1,late
      do 117 np1=ms,nlat,2
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(i,mml1+np1)
  117 continue

c     compute m.ge.1  coefficients next
      do 118 mp1=2,lm1
      m = mp1-1
      mml1 = m*(2*nlat-m-1)/2
      ms = mp1
      if (mode.eq.1) ms = mp1+1
      do 119 k=1,nt
      do 119 i=1,late
      do 119 np1=ms,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(i,mml1+np1)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(i,mml1+np1)
  119 continue
  118 continue

      if (nlon.eq.l+l-2) then
c     compute n=m=l-1 coefficients last
      m = l-1
      mml1 = m*(2*nlat-m-1)/2
c     set starting n for mode even
      ns = l
c     set starting n for mode odd
      if (mode.eq.1) ns = l+1
      do 120 k=1,nt
      do 120 i=1,late
      do 120 np1=ns,nlat,2
      mn = mml1+np1
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(i,mn)
  120 continue
      end if

      end if

      return
      end
      subroutine shagsi(nlat,nlon,wshags,lshags,work,lwork,dwork,ldwork,
     +                  ierror)
c
c     this subroutine must be called before calling shags or shsgs with
c     fixed nlat,nlon. it precomputes the gaussian weights, points
c     and all necessary legendre polys and stores them in wshags.
c     these quantities must be preserved when calling shags or shsgs
c     repeatedly with fixed nlat,nlon.  dwork must be of length at
c     least nlat*(nlat+4) in the routine calling shagsi.  This is
c     not checked.  undetectable errors will result if dwork is
c     smaller than nlat*(nlat+4).
c
      dimension wshags(lshags),work(lwork)
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
      if(lshags.lt.lp) return
      ierror = 4
c     check temporary work space
      if (lwork.lt.4*nlat*(nlat+2)+2) return
      ierror = 5
c     check double precision temporary space
      if (ldwork .lt. nlat*(nlat+4)) return
      ierror = 0
c     set preliminary quantites needed to compute and store legendre polys
      ldw = nlat*(nlat+4)
      call shagsp(nlat,nlon,wshags,lshags,dwork,ldwork,ierror)
      if (ierror.ne.0) return
c     set legendre poly pointer in wshags
      ipmnf = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+nlon+16
      call shagss1(nlat,l,late,wshags,work,wshags(ipmnf))
      return
      end
      subroutine shagss1(nlat,l,late,w,pmn,pmnf)
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
      subroutine shagsp(nlat,nlon,wshags,lshags,dwork,ldwork,ierror)
      dimension wshags(lshags)
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
      if (lshags .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
c     if (lwork.lt.4*nlat*(nlat+2)+2) return
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
c     idwts = idth+2*nlat
c     iw = idwts+2*nlat
      idwts = idth+nlat
      iw = idwts+nlat
      call shagsp1(nlat,nlon,l,late,wshags(i1),wshags(i2),wshags(i3),
     1wshags(i4),wshags(i5),wshags(i6),wshags(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 6
      return
      end

      subroutine shagsp1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
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
