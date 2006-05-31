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
c     this code tests subroutine gaqd for computing 
c     the Gauss Legendre points and weights in file gaqd.f
c     It tests only the april 2002 version and not the
c     older version in file gaqd.old.f
c     gauss points and weights are computed using newtons method
c     with equally spaced points as first guess. Points are
c     computed as theta_i where x_i = cos(theta_i)
c     the method is discussed in "On computing the Gauss_Legendre
c     points and weights" by Paul N Swarztrauber, accepted for 
c     publication in the SIAM journal of scientific computing.
c                         April 2002
c
      program testint
      parameter (nlat=63)
      double precision theta(nlat),wts(nlat),work(nlat+2),
     1                dtheta(nlat),dwts(nlat),dw(nlat+1)
      double precision dwmx,tmax,dwmax,dtmax
      dimension stheta(nlat),swts(nlat),swork(nlat+1)
      dimension diff(nlat),t1(2),t2(2)
      double precision sumw
c
      lwork = nlat+1
      hold = etime(t1)
      hold = t1(1)
      call gsqd(nlat,theta,wts,work,lwork,ierror)
      tdoub = etime(t1)
      tdoub = t1(1)-hold
      if(ierror .ne. 0) write(6,4) ierror
 4    format(' ierror=',i5) 
      write(*,739) tdoub
 739  format(' tdoub ',1pe15.6)
c
c     compare double with double
c
      ldw = nlat+2
      hold = etime(t1)
      hold = t1(1)
      call gaqd(nlat,dtheta,dwts,dw,ldw,ierror)
      tdoub = etime(t1)
      tdoub = t1(1)-hold
      if(ierror .ne. 0) write(6,30) ierror
 30   format(' ierror=',i5) 
      write(*,31) tdoub
 31   format(' tdoub gaqd',1pe15.6)
c      
      dwmx = 0.0d0
      tmax = 0.0d0
      dwmax = 0.0d0
      dtmax = 0.0d0
      ido = (nlat+1)/2
      do 32 i=1,ido
      dtmax = max(dtmax,dabs(theta(i)-dtheta(i)))
      dwmax = max(dwmax,dabs(wts(i)-dwts(i)))
      tmax = max(tmax,abs(theta(i)))
      dwmx = max(dwmx,abs(wts(i)))
 32   continue
      dtmax = dtmax/tmax
      dwmax = dwmax/dwmx
      write(*,33) nlat,dtmax,dwmax
 33   format(' nlat',i6,'  points ',1pd15.6,' weights ',d15.6)
c
      sumw = 0.
      do i=1,nlat
      sumw = sumw+wts(i)
      end do
      write(*,638) sumw
 638  format('  sumw ',1pd39.30)
c      if(0.eq.0) go to 671
      hold = etime(t2)
      hold = t2(1)
      lwork = nlat+2
      call sgaqd(nlat,stheta,swts,swork,lwork,ierror)
      tsing = etime(t2)
      tsing = t2(1)-hold
      if(ierror .ne. 0) write(6,5) ierror
 5    format(' iserror=',i5) 
      sums = 0.
      do i=1,nlat
      sums = sums+swts(i)
      end do
      write(*,636) sums
 636  format('  sums ',1pe24.15)
      dmax = 0.
      wmax = 0.
      rmax = 0.
      ido = (nlat+1)/2
      do 6 i=1,ido
      diff(i) = wts(i)-swts(i)
      dmax = max(dmax,abs(diff(i)))
      wmax = max(wmax,abs(swts(i)))
      rerr = abs(diff(i)/swts(i))
      if(rerr.gt.rmax) then
      rmax = rerr
      irm = i
      end if
c      if(i.lt.121) write(*,27) wts(i),swts(i),rerr
 27   format(' wts ',1pd15.6,' swts ',e15.6,' rpter ',e15.6)
 6    continue
c      write(*,7) (diff(i),i=nlat-25,nlat)
 7    format(' diff in weights'/(1p8e10.3))
      dmax = dmax/wmax
      write(*,9) nlat,irm,dmax,rmax
 9    format(' weights: nlat ',i6,' irele ',i6,
     1    ' dmax ',1pe15.6,' rmax ',1pe15.6)
      dmax = 0.
      pmax = 0.
      rmax = 0.
      do 10 i=1,ido
      diff(i) = theta(i)-stheta(i)
      dmax = max(dmax,abs(diff(i)))
      pmax = max(pmax,abs(stheta(i)))
      rerr = abs(diff(i)/stheta(i))
      if(rerr.gt.rmax) then
      rmax = rerr
      irm = i
      end if
 10   continue
c      write(*,11) (diff(i),i=nlat-25,nlat)
 11   format(' diff in points'/(1p8e10.3))
      dmax = dmax/pmax
      write(*,12) nlat,irm,dmax,rmax
 12   format(' points:  nlat ',i6,' irele ',i6,
     1    ' dmax ',1pe15.6,' rmax ',1pe15.6)
      dmax = 0.
      do 110 i=1,nlat
      diff(i) = dcos(theta(i))-cos(stheta(i))
      dmax = max(dmax,abs(diff(i)))
 110  continue
c      write(*,111) (diff(i),i=nlat-25,nlat)
 111  format(' diff in points'/(1p8e10.3))
      write(*,112) dmax
 112  format(' max difference in mu',1pe15.6)
      write(*,1) nlat,tsing,tdoub
 1    format(' nlat',i6,' tsing',1pe15.6,' tdoub',e15.6)
 671  end
c
c     a single precision version of gaqd.
c     gauss points and weights are computed using newtons method
c     with equally spaced points as first guess. Points are
c     computed as theta_i where x_i = cos(theta_i)
c     the method is discussed in "On computing the Gauss_Legendre
c     points and weights" by Paul N Swarztrauber, accepted for 
c     publication in the SIAM journal of scientific computing.
c                         April 2002
c
      subroutine gsqd(nlat,theta,wts,dwork,ldwork,ierror)
c
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         spherepack 3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c     subroutine gsqd computes the nlat gaussian colatitudes and weights
c     in double precision. The colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     dwork   a double precision temporary work space.
c
c     ldwork  the length of the work space  in the routine calling gsqd
c             ldwork must be at least nlat+1.
c
c     output parameters
c
c     theta   a double precision vector of length nlat containing the
c             nlat gaussian colatitudes on the sphere in increasing radians
c             in the interval (0,pi).
c
c     wts     a double precision vector of length nlat containing the
c             nlat gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if ldwork.lt.nlat+1
c            = 2 if nlat.le.0
c
c  *****************************************************************
c
      dimension dwork(nlat+1),theta(nlat),wts(nlat)
      double precision pis2,x,theta,wts,dwork
      ierror = 1
c
c     check work space length
c
      if (ldwork.lt.nlat+1) return
      ierror = 2
      if (nlat.le.0) return
      ierror = 0
c
c     compute weights and points analytically when nlat=1,2
c
      if (nlat.eq.1) then
      theta(1) = dacos(0.0d0)
      wts(1) = 2.d0
      return
      end if
      if (nlat.eq.2) then
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      wts(1) = 1.d0
      wts(2) = 1.d0
      return
      end if
c
c     compute points
c
      call gsqd1(nlat,theta,dwork)
c
c     compute weights
c
      call egwts(nlat,theta,wts,dwork)
c
c     extend points and weights via symmetries
c
      pis2 = 4.0d0*datan(1.0d0)
      ns2 = nlat/2
      do i=1,ns2
      wts(nlat-i+1) = wts(i)
      theta(nlat-i+1) = pis2-theta(i)
      end do
      return
      end
c
      subroutine gsqd1(nlat,theta,cp)
      double precision  theta((nlat+1)/2),cp(nlat/2+1)
      double precision pi,pis2,dtheta,dthalf,
     1  cmax,dcor,pb,dpb,sgnd,zero,zlast
c
      eps = sqrt(dzeps(1.0d0))
      eps = eps*sqrt(eps)
      pis2 = 2.0d0*datan(1.0d0)
      pi = pis2+pis2 
      theta(1) = pis2 
      if(nlat.eq.1) go to 30
      ns2 = nlat/2
      nhalf = (nlat+1)/2
c
      call dlfcz (nlat,cp)
c
c      check fourier-legendre coefficients
c
      sum = 0.
      do i=1,ns2+1
      sum = sum+cp(i) 
      end do
      sum = sum/ns2
c      write(*,689)  sum
 689  format(' check on dble f-l coefficients ',1pe15.6)
c
      dtheta = pis2/nhalf
      dthalf = dtheta/2.0d0
      cmax = .2d0*dtheta
      if(mod(nlat,2).ne.0) then
      theta(nhalf) = pis2
      zero = pis2-dtheta
      nix = nhalf-1
      else
      zero = pis2-dthalf
      nix = nhalf
      end if
 9    it = 0
 10   it = it+1
      zlast = zero
c
c     newton iterations to convergence
c
      call lft (0,nlat,zero,cp,pb)
      call dlft (0,nlat,zero,cp,dpb)
      dcor = pb/dpb
      sgnd = 1.0d0
      if(dcor .ne. 0.0d0) sgnd = dcor/dabs(dcor)
c      write(*,2) nix,zero,theta(nix),dcor,cmax,sgnd
 2    format(i7,1p5d15.6)
      dcor = sgnd*min(dabs(dcor),cmax)
      zero = zero-dcor
      if(dabs(zero-zlast).gt.eps*dabs(zero)) go to 10
      theta(nix) = zero
      nix = nix-1
      if(nix.eq.0) go to 30
      if(nix.eq.nhalf-1)  zero = 3.0d0*zero-pi
      if(nix.lt.nhalf-1)  zero = zero+zero-theta(nix+2)
      go to 9
 30   return
      end
      subroutine egwts(n,theta,wts,work)
c
c     computes gauss weights as described in swarztrauber
c     and spotz, generalized harmonic transforms
c
      double precision theta(n),wts(n),work(n+1)
c
      call egwts1(n,theta,wts,work,work(n/2+2))
      return
      end
      subroutine egwts1(n,theta,wts,dcp,cp)
      double precision theta((n+1)/2),wts((n+1)/2),cp((n-1)/2+1),
     1                 dcp(n/2+1),fn,sqnn,pb,dpb      
      fn = n
      sqnn = dsqrt((fn+fn-1)*(fn+fn+1))
      call dlfcz (n-1,cp)
      call dlfcz (n,dcp)
      nhalf = (n+1)/2
      do i=1,nhalf
      call lft (0,n-1,theta(i),cp,pb)
      call dlft (0,n,theta(i),dcp,dpb)
      wts(i) = -sqnn*dsin(theta(i))/(fn*pb*dpb)
      end do
      return
      end
      subroutine lfc (m,n,cp)
c
      double precision cp,fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,
     1       t1,t2,pm1,sc10,sc20,sc40
      dimension       cp(n/2+1)
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
      subroutine lft (m,n,theta,cp,pb)
      double precision cp(*),pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(m,2)
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
      subroutine dlft (m,n,theta,cp,pb)
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
      subroutine dlfcz(n,cp)
c
c     computes the fourier coefficients of the legendre
c     polynomials. n is the degree and integer(n/2+1)
c     coefficients are returned in cp
c
      double precision cp(n/2+1)
      double precision cn,t1,t2,t3,t4,coef,fi
c
      cn = 2.0d0
       write(*,9) cn
 9     format(' check1 on dble cn ',1pd20.11)
      if(n.gt.0) then
      ic = 0
      fi = 0.0d0
      do i=1,n
      fi = fi+2.0d0 
      cn = (1.0d0-1.0d0/fi**2)*cn
      if(dabs(cn).gt. 5.0d0.and.ic.eq.0) then
      ic = 1
      write(*,7) i,cn
 7    format('  i ',i7,' check3 on cn',1pd15.6)
      end if 
      end do
      end if
       write(*,8) cn
 8     format(' check2 on dble cn ',1pd20.11)
      cn = dsqrt(cn)
      ncp = n/2+1
      t1 = -1.0d0
      t2 = n+1.0d0
      t3 = 0.
      t4 = n+n+1.0d0
      cp(ncp) = cn
      coef = 1.0d0
       write(*,11) cn
 11    format(' check on dble cn ',1pd20.11)
c      do j = ncp-1,1,-1
      j = ncp
 10   j = j-1 
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      coef = (t1*t2)/(t3*t4)*coef
      cp(j) = coef*cn
      if(j.gt.1) go to 10
c      end do
      return
      end
c
      subroutine sgaqd(nlat,theta,wts,w,lwork,ierror)
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
c     subroutine sgaqd computes the nlat gaussian colatitudes and
c     weights in single precision. the colatitudes are in radians
c     and lie in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     w       unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a vector of length nlat containing the nlat gaussian 
c             colatitudes on the sphere in increasing radians
c             in the interval (0,pi).
c
c     wts     a vector of length nlat containing the
c                   nlat gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      dimension theta(nlat),wts(nlat)
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
      theta(1) = acos(0.0d0)
      wts(1) = 2.0
      return
      end if
      if (nlat.eq.2) then
      x = sqrt(1.0/3.0)
      theta(1) = acos(x)
      theta(2) = acos(-x)
      wts(1) = 1.0
      wts(2) = 1.0
      return
      end if
      eps = sqrt(zeps(1.0))
      eps = eps*sqrt(eps)
      pis2 = 2.0*atan(1.0)
      pi = pis2+pis2 
      mnlat = mod(nlat,2)
      ns2 = nlat/2
      nhalf = (nlat+1)/2
      idx = ns2+2
c
      call lfcz (nlat,cz,theta(ns2+1),wts(ns2+1))
c
      dtheta = pis2/nhalf
      dthalf = dtheta/2.0
      cmax = .2*dtheta
c
c     estimate first point next to theta = pi/2
c
      if(mnlat.ne.0) then
      zprev = pis2
      zero = pis2-dtheta
      nix = nhalf-1
      else
      zero = pis2-dthalf
      nix = nhalf
      end if
      itmax = 0
 9    it = 0
 10   it = it+1
      zlast = zero
c
c     newton iterations
c
      call slpdp (nlat,zero,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      dcor = pb/dpb
      sgnd = 1.0
      if(dcor .ne. 0.0) sgnd = dcor/abs(dcor)
      dcor = sgnd*min(abs(dcor),cmax)
      zero = zero-dcor
      if(abs(zero-zlast).gt.eps*abs(zero)) go to 10
      theta(nix) = zero
      zhold = zero
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c    
c     yakimiw's formula permits using old pb and dpb
c
      wts(nix) = (nlat+nlat+1)/(dpb+pb*cos(zlast)/sin(zlast))**2
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
      call slpdp (nlat,pis2,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      wts(nhalf) = (nlat+nlat+1)/(dpb*dpb)
      end if
      do i=1,ns2
      wts(nlat-i+1) = wts(i)
      theta(nlat-i+1) = pi-theta(i)
      end do
      sum = 0.
      do i=1,nlat
      sum = sum+wts(i)
      end do
      do i=1,nlat
      wts(i) = 2.0*wts(i)/sum
      end do
      return
      end
      subroutine lfcz(n,cz,cp,dcp)
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative. 
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant 
c     coefficient is returned in cz. 
c
      dimension cp((n+1)/2),dcp((n+1)/2)
      ncp = (n+1)/2
      t1 = -1.0
      t2 = n+1.0
      t3 = 0.
      t4 = n+n+1.0
      if(mod(n,2).eq.0) then
      cp(ncp) = 1.0
      do j = ncp,2,-1
      t1 = t1+2.0
      t2 = t2-1.0
      t3 = t3+1.0
      t4 = t4-2.0
      cp(j-1) = (t1*t2)/(t3*t4)*cp(j)
      end do
      t1 = t1+2.0
      t2 = t2-1.0
      t3 = t3+1.0
      t4 = t4-2.0
      cz = (t1*t2)/(t3*t4)*cp(1)
      do j=1,ncp
      dcp(j) = (j+j)*cp(j)
      end do
      else
      cp(ncp) = 1.0
      do j = ncp-1,1,-1
      t1 = t1+2.0
      t2 = t2-1.0
      t3 = t3+1.0
      t4 = t4-2.0
      cp(j) = (t1*t2)/(t3*t4)*cp(j+1)
      end do
      do j=1,ncp
      dcp(j) = (j+j-1)*cp(j)
      end do
      end if
c      write(*,23) (cp(j),dcp(j),j=1,ncp)
 23   format(' coefficients '/(1p4e15.6))
      return
      end
      subroutine slpdp (n,theta,cz,cp,dcp,pb,dpb)
c
c     computes pn(theta) and its derivative dpb(theta) with 
c                          respect to theta
c
      dimension cp(n/2+1),dcp(n/2+1)
c
      fn = float(n)
      cdt = cos(theta+theta)
      sdt = sin(theta+theta)
      if(mod(n,2) .eq.0) then
c
c     n even
c
      kdo = n/2
      pb = .5*cz
      dpb = 0.0
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
      pb = 0.
      dpb = 0.
      cth = cos(theta)
      sth = sin(theta)
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
      real function zeps (x)
c
c     estimate unit roundoff in quantities of size x.
c
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
      a = 4.0/3.0
   10 b = a - 1.0
      c = b + b + b
      eps = abs(c-1.0)
      if (eps .eq. 0.0) go to 10
      zeps = eps*abs(x)
      return
      end
c
