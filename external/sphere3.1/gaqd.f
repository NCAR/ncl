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
c  .                          SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             August 2003
c
c
c     This version of gaqd implements the method presented in:
c     P. N. swarztrauber, Computing the points and weights for 
c     Gauss-Legendre quadrature, SIAM J. Sci. Comput.,
c     24(2002) pp. 945-954.
c     
c     It the version that is new to spherepack 3.1
c     The w and lwork arrays are dummy and included only to
c     permit a simple pluggable exchange with the
c     old gaqd in spherepack 3.0. 
c
c
c
      subroutine gaqd(nlat,theta,wts,w,lwork,ierror)
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             February 2002
c                        
c     gauss points and weights are computed using the fourier-newton
c     described in "on computing the points and weights for 
c     gauss-legendre quadrature", paul n. swarztrauber, siam journal 
c     on scientific computing that has been accepted for publication.
c     This routine is faster and more accurate than older program 
c     with the same name.
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
c     w       unused double precision variable that permits a simple 
c             exchange with the old routine with the same name 
c             in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a double precision array with length nlat
c             containing the gaussian colatitudes in
c             increasing radians on the interval (0,pi).
c
c     wts     a double precision array with lenght nlat
c             containing the gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      double precision theta(nlat),wts(nlat),w,
     1 x,pi,pis2,dtheta,dthalf,cmax,zprev,zlast,zero,
     2 zhold,pb,dpb,dcor,sum,cz
c
c     check work space length
c
      ierror = 1
      if (nlat.le.0) return
      ierror = 0
c
c     compute weights and points analytically when nlat=1,2
c
      if (nlat.eq.1) then
      theta(1) = dacos(0.0d0)
      wts(1) = 2.0d0
      return
      end if
      if (nlat.eq.2) then
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      wts(1) = 1.0d0
      wts(2) = 1.0d0
      return
      end if
      eps = sqrt(dzeps(1.0d0))
      eps = eps*sqrt(eps)
      pis2 = 2.0d0*datan(1.0d0)
      pi = pis2+pis2 
      mnlat = mod(nlat,2)
      ns2 = nlat/2
      nhalf = (nlat+1)/2
      idx = ns2+2
c
      call cpdp (nlat,cz,theta(ns2+1),wts(ns2+1))
c
      dtheta = pis2/nhalf
      dthalf = dtheta/2.0d0
      cmax = .2d0*dtheta
c
c     estimate first point next to theta = pi/2
c
      if(mnlat.ne.0) then
      zero = pis2-dtheta
      zprev = pis2
      nix = nhalf-1
      else
      zero = pis2-dthalf
      nix = nhalf
      end if
 9    it = 0
 10   it = it+1
      zlast = zero
c
c     newton iterations
c
      call tpdp (nlat,zero,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      dcor = pb/dpb
      sgnd = 1.0
      if(dcor .ne. 0.0d0) sgnd = dcor/dabs(dcor)
      dcor = sgnd*min(dabs(dcor),cmax)
      zero = zero-dcor
      if(dabs(zero-zlast).gt.eps*dabs(zero)) go to 10
      theta(nix) = zero
      zhold = zero
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c    
c     yakimiw's formula permits using old pb and dpb
c
      wts(nix) = (nlat+nlat+1)/(dpb+pb*dcos(zlast)/dsin(zlast))**2
      nix = nix-1
      if(nix.eq.0) go to 30
      if(nix.eq.nhalf-1)  zero = 3.0*zero-pi
      if(nix.lt.nhalf-1)  zero = zero+zero-zprev
      zprev = zhold
      go to 9
c
c     extend points and weights via symmetries
c
 30   if(mnlat.ne.0) then
      theta(nhalf) = pis2
      call tpdp (nlat,pis2,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      wts(nhalf) = (nlat+nlat+1)/(dpb*dpb)
      end if
      do i=1,ns2
      wts(nlat-i+1) = wts(i)
      theta(nlat-i+1) = pi-theta(i)
      end do
      sum = 0.0d0
      do i=1,nlat
      sum = sum+wts(i)
      end do
      do i=1,nlat
      wts(i) = 2.0d0*wts(i)/sum
      end do
      return
      end
      subroutine cpdp(n,cz,cp,dcp)
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative. 
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant 
c     coefficient is returned in cz. 
c
      double precision cp(n/2+1),dcp(n/2+1),
     1 t1,t2,t3,t4,cz
      ncp = (n+1)/2
      t1 = -1.0d0
      t2 = n+1.0d0
      t3 = 0.0d0
      t4 = n+n+1.0d0
      if(mod(n,2).eq.0) then
      cp(ncp) = 1.0d0
      do j = ncp,2,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j-1) = (t1*t2)/(t3*t4)*cp(j)
      end do
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cz = (t1*t2)/(t3*t4)*cp(1)
      do j=1,ncp
      dcp(j) = (j+j)*cp(j)
      end do
      else
      cp(ncp) = 1.0d0
      do j = ncp-1,1,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j) = (t1*t2)/(t3*t4)*cp(j+1)
      end do
      do j=1,ncp
      dcp(j) = (j+j-1)*cp(j)
      end do
      end if
      return
      end
      subroutine tpdp (n,theta,cz,cp,dcp,pb,dpb)
c
c     computes pn(theta) and its derivative dpb(theta) with 
c     respect to theta
c
      double precision cp(n/2+1),dcp(n/2+1),cz,
     1  pb,dpb,fn,theta,cdt,sdt,cth,sth,chh
c
      fn = n
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      if(mod(n,2) .eq.0) then
c
c     n even
c
      kdo = n/2
      pb = .5d0*cz
      dpb = 0.0d0
      if(n .gt. 0) then
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c      pb = pb+cp(k)*cos(2*k*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k)*cp(k)*sin(2*k*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      end if
      else
c
c     n odd
c
      kdo = (n+1)/2
      pb = 0.0d0
      dpb = 0.0d0
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c      pb = pb+cp(k)*cos((2*k-1)*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k-1)*cp(k)*sin((2*k-1)*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      end if
      return
      end
      real function dzeps (x)
      double precision  x
c
c     estimate unit roundoff in quantities of size x.
c
      double precision a,b,c,eps
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to 
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying 
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      a = 4.0d0/3.0d0
   10 b = a - 1.0d0
      c = b + b + b
      eps = abs(c-1.0d0)
      if (eps .eq. 0.0d0) go to 10
      dzeps = eps*dabs(x)
      return
      end
