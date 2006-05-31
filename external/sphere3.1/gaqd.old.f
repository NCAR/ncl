c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         spherepack3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file gaqd.f
c
c     this file includes documentation and code for subroutine gaqd.
c
c     modified on april 14, 1999 based on an observation by R. Loft
c     that reduces both storage and compute time by a factor of nlat!
c
c ... required files
c
c     none
c
c     subroutine gaqd(nlat,theta,wts,dwork,ldwork,ierror)
c
c     subroutine gaqd computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     dwork   a double precision temporary work space.
c
c     ldwork  the length of the work space  in the routine calling gaqd
c             ldwork must be at least nlat.
c
c     output parameters
c
c     theta   a double precision vector of length nlat containing the
c             nlat gaussian colatitudes on the sphere in increasing radians
c             in the interval (0,pi).
c
c             the accuracy of the theta array can be improvred by calling
c             subroutine gpnewt
c
c     wts     a double precision vector of length nlat containing the
c             nlat gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if ldwork.lt.nlat
c            = 2 if nlat.le.0
c            = 3 if unable to compute gaussian points
c                (failure in in the eigenvalue routine)
c
c  *****************************************************************
      subroutine gaqd(nlat,theta,wts,dwork,ldwork,ierror)
      double precision theta(nlat),wts(nlat),dwork(nlat),x
      n = nlat
      ierror = 1
c     check work space length
      if (ldwork .lt. n) return
      ierror = 2
      if (n.le.0) return
      ierror = 0
      if (n.gt.2) then
c
      call gaqd1(n,theta,wts,dwork,ierror)
      if (ierror.ne.0) then
      ierror = 3
      return
      end if
      return
      else if (n.eq.1) then
      wts(1) = 2.0d0
      theta(1) = dacos(0.0d0)
      else if (n.eq.2) then
c     compute weights and points analytically when n=2
      wts(1) = 1.0d0
      wts(2) = 1.0d0
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      return
      end if
      end
      subroutine gaqd1(n,theta,wts,e,ier)
      dimension theta(n),wts(n),e(n)
      double precision theta,wts,e
c     set symmetric tridiagnonal matrix subdiagonal and diagonal
c     coefficients for matrix coming from coefficients in the
c     recursion formula for legendre polynomials
c     a(n)*p(n-1)+b(n)*p(n)+c(n)*p(n+1) = 0.
      theta(1) = 0.d0
      e(1) = 0.d0
      do 100 j=2,n
      e(j)= (j-1.d0)/dsqrt((2.d0*j-1.d0)*(2.d0*j-3.d0))
      theta(j) = 0.d0
  100 continue
c
c     compute eigenvalues and first elemnt of the eigenvectors
c
      call gaqd2(n,theta,e,wts,ier)
      if (ier.ne.0) return
c

      do 102 i=1,n
      theta(i) = -theta(i)
c      theta(n-i+1) = -theta(i)
  102 continue
      if(mod(n,2).ne.0) theta(n/2+1) = 0.d0
c
c     compute gaussian weights and points
c

      do 101 j=1,n
      theta(j) = dacos(theta(j))
c
c     set gaussian weights as 1st components of eigenvectors squared
c 
      wts(j) = 2.0d0*wts(j)**2
  101 continue
c
c     reverse order of gaussian points to be
c     monotonic increasing in radians
c
      return
      end
      subroutine gaqd2(n,w,e,z,ierr)
c     drst is a double precision modification of rst off eispack
c     to be used  to compute gaussian points and weights

c
      integer i,n,ierr
      double precision w(n),e(n),z(n)

c
c     .......... find both eigenvalues and eigenvectors ..........
   20 do 40 i = 2, n
         z(i) = 0.0d0
   40 continue
      z(1) = 1.0d0
c
      call  gaqd3(n,w,e,z,ierr)
      return
      end
      subroutine gaqd3(n,d,e,z,ierr)
c
c     dintql is a modification of intql2 in eispack that
c     is used by gaqd in spherepack for computing
c     gaussian weights and points
c
      integer i,j,k,l,m,n,ii,mml,ierr
      double precision d(n),e(n),z(n)
      double precision b,c,f,g,p,r,s,tst1,tst2,dpytha
      ierr = 0
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n
  100 e(i-1) = e(i)
c
      e(n) = 0.0d0
c
      do 240 l = 1, n
	 j = 0
c     .......... look for small sub-diagonal element ..........
c
  105    nm1 = n-1     
         if(l .gt. nm1) go to 111
         do 110 mdo = l, nm1
            m = mdo
	    tst1 = dabs(d(m)) + dabs(d(m+1))
	    tst2 = tst1 + dabs(e(m))
	    if (tst2 .eq. tst1) go to 120
  110    continue
  111    m = n
c
  120    p = d(l)
	 if (m .eq. l) go to 240
	 if (j .eq. 30) go to 1000
	 j = j + 1
c     .......... form shift ..........
	 g = (d(l+1) - p) / (2.0d0 * e(l))
	 r = dpytha(g,1.0d0)
	 g = d(m) - p + e(l) / (g + sign(r,g))
	 s = 1.0d0
	 c = 1.0d0
	 p = 0.0d0
	 mml = m - l
c     .......... for i=m-1 step -1 until l do -- ..........
	 do 200 ii = 1, mml
	    i = m - ii
	    f = s * e(i)
	    b = c * e(i)
	    r = dpytha(f,g)
	    e(i+1) = r
	    if (r .eq. 0.0d0) go to 210
	    s = f / r
	    c = g / r
	    g = d(i+1) - p
	    r = (d(i) - g) * s + 2.0d0 * c * b
	    p = s * r
	    d(i+1) = g + p
	    g = c * r - b
c     .......... form vector ..........
            f = z(i+1)
            z(i+1) = s * z(i) + c * f
            z(i) = c * z(i) - s * f
  200    continue
c
	 d(l) = d(l) - p
	 e(l) = g
	 e(m) = 0.0d0
	 go to 105
c     .......... recover from underflow ..........
  210    d(i+1) = d(i+1) - p
	 e(m) = 0.0d0
	 go to 105
  240 continue
c     .......... order eigenvalues and eigenvectors ..........
      do 300 ii = 2, n
	 i = ii - 1
	 k = i
	 p = d(i)
c
	 do 260 j = ii, n
	    if (d(j) .ge. p) go to 260
	    k = j
	    p = d(j)
  260    continue
c
	 if (k .eq. i) go to 300
	 d(k) = d(i)
	 d(i) = p
c
         p = z(i)
         z(i) = z(k)
         z(k) = p
  300 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end
      double precision function dpytha(a,b)
      double precision a,b
c     dpytha is a double precision modification of pythag off eispack
c     for use by dimtql

c
c     finds sqrt(a**2+b**2) without overflow or destructive underflow
c
      double precision p,r,s,t,u
      p = dabs(a)
      if (dabs(b).ge.dabs(a)) p = dabs(b)
      if (p .eq. 0.0d0) go to 20
      r = (dabs(a)/p)**2
      if (dabs(b).lt.dabs(a)) r = (dabs(b)/p)**2
   10 continue
	 t = 4.0d0 + r
	 if (t .eq. 4.0d0) go to 20
	 s = r/t
	 u = 1.0d0 + 2.0d0*s
	 p = u*p
	 r = (s/u)**2 * r
      go to 10
   20 dpytha = p
      return
      end
      subroutine gpnewt(n,theta,work)
c
c     one newton iteration to correct the gauss points
c     computed by subroutine gaqd
c
c     calls subroutines dnlfk, dnlft, and dnlftd in file
c     ~/codes/packages/spherepack/sphpk3/sphcom.f
c
      double precision theta(n),work(5*n/2+2)
c
      call gpnewt1(n,theta,work,work(n+1),work(2*n+1))
      return
      end
      subroutine gpnewt1(n,theta,pb,dpb,cp)
      double precision theta(n),pb(n),dpb(n),cp(n/2+2)      
c
      call dnlfk (0,n,cp)
      do i=1,n
      call dnlft (0,n,theta(i),cp,pb(i))
      call dnlftd (0,n,theta(i),cp,dpb(i))
      end do
      do i=1,n
      theta(i) = theta(i)-pb(i)/dpb(i)
      end do
      return
      end
c     subroutine dnlfk(m,n,cp)
c
c     subroutine dnlfk computes the coefficients in the trigonometric
c     expansion of the normalized associated legendre functions:
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                        *sin(theta)**m/(2**n*factorial(n)) times the
c                        (n+m)th derivative of (x**2-1)**n with respect
c                        to x=cos(theta)
c
c     where theta is colatitude.
c
c     subroutine dnlfk computes the coefficients cp(k) in the
c     following trigonometric expansion of pbar(m,n,theta).
c
c            1) for n even and m even, pbar(m,n,theta) =
c               .5*cp(1) plus the sum from k=1 to k=n/2
c               of cp(k)*cos(2*k*th)
c
c            2) for n even and m odd, pbar(m,n,theta) =
c               the sum from k=1 to k=n/2 of
c               cp(k)*sin(2*k*th)
c
c            3) for n odd and m even, pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*cos((2*k-1)*th)
c
c            4) for n odd and m odd,  pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*sin((2*k-1)*th)
c
c     input parameters
c
c     m      is the order of pbar(n,m,theta). m can be any integer
c            however pbar(n,m,theta) = 0  if abs(m) is greater than
c            n and pbar(n,m,theta) = (-1)**m*pbar(n,-m,theta) for
c            negative m.
c
c     n      nonnegative integer specifying the degree of
c            pbar(n,m,theta)
c
c     output parameters
c
c     cp     a double precision array that contains the fourier
c            coefficients for pbar(m,n,theta). the length of the
c            array depends on the parity of m and n
c
c                  parity            length of cp
c
c               n even m even           n/2+1
c               n even m odd             n/2
c               n odd  m even          (n+1)/2
c               n odd  m odd           (n+1)/2
c
c
c ****************************************************************
      subroutine dnlfk (m,n,cp)
c
      double precision cp,fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,
     1       t1,t2,pm1,sc10,sc20,sc40
      dimension       cp(1)
      parameter (sc10=1024.d0)
      parameter (sc20=sc10*sc10)
      parameter (sc40=sc20*sc20)
c
      cp(1) = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if(n-1) 2,3,5
    2 cp(1) = dsqrt(2.d0)
      return
    3 if(ma .ne. 0) go to 4
      cp(1) = dsqrt(1.5d0)
      return
    4 cp(1) = dsqrt(.75d0)
      if(m .eq. -1) cp(1) = -cp(1)
      return
    5 if(mod(n+ma,2) .ne. 0) go to 10
      nmms2 = (n-ma)/2
      fnum = n+ma+1
      fnmh = n-ma+1
      pm1 = 1.d0
      go to 15
   10 nmms2 = (n-ma-1)/2
      fnum = n+ma+2
      fnmh = n-ma+2
      pm1 = -1.d0
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
 15   t1 = 1.d0/sc20
      nex = 20
      fden = 2.d0
      if(nmms2 .lt. 1) go to 20
      do 18 i=1,nmms2
      t1 = fnum*t1/fden
      if(t1 .gt. sc20) then
      t1 = t1/sc40
      nex = nex+40
      end if
      fnum = fnum+2.
      fden = fden+2.
   18 continue
   20 t1 = t1/2.d0**(n-1-nex)
      if(mod(ma/2,2) .ne. 0) t1 = -t1
      t2 = 1. 
      if(ma .eq. 0) go to 26
      do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
   25 continue
   26 cp2 = t1*dsqrt((n+.5d0)*t2)
      fnnp1 = n*(n+1)
      fnmsq = fnnp1-2.d0*ma*ma
      l = (n+1)/2
      if(mod(n,2) .eq. 0 .and. mod(ma,2) .eq. 0) l = l+1
      cp(l) = cp2
      if(m .ge. 0) go to 29
      if(mod(ma,2) .ne. 0) cp(l) = -cp(l)
   29 if(l .le. 1) return
      fk = n
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = 2.*(fk*fk-fnmsq)
      cp(l-1) = b1*cp(l)/a1
   30 l = l-1
      if(l .le. 1) return
      fk = fk-2.
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = -2.*(fk*fk-fnmsq)
      c1 = (fk+1.)*(fk+2.)-fnnp1
      cp(l-1) = -(b1*cp(l)+c1*cp(l+1))/a1
      go to 30
      end
      subroutine dnlft (m,n,theta,cp,pb)
      dimension cp(1)
      double precision cp,pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(abs(m),2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = .5*cp(1)
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb+cp(k+1)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb+cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
      subroutine dnlftd (m,n,theta,cp,pb)
c
c     computes the derivative of pmn(theta) with respect to theta
c
      dimension cp(1)
      double precision cp,pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(abs(m),2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = 0.d0
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb-2.d0*k*cp(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+2.d0*k*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb-(2.d0*k-1)*cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+(2.d0*k-1)*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
