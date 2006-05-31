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
c     file alf.f contains subroutines alfk,lfim,lfim1,lfin,lfin1,lfpt
c     for computing normalized associated legendre polynomials
c
c subroutine alfk (n,m,cp)
c
c dimension of           real cp(n/2 + 1)
c arguments
c
c purpose                routine alfk computes single precision fourier
c                        coefficients in the trigonometric series
c                        representation of the normalized associated
c                        legendre function pbar(n,m,theta) for use by
c                        routines lfp and lfpt in calculating single
c                        precision pbar(n,m,theta).
c
c                        first define the normalized associated
c                        legendre functions
c
c                        pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)
c                        /(2*factorial(n+m)))*sin(theta)**m/(2**n*
c                        factorial(n)) times the (n+m)th derivative of
c                        (x**2-1)**n with respect to x=cos(theta)
c
c                        where theta is colatitude.
c
c                        then subroutine alfk computes the coefficients
c                        cp(k) in the following trigonometric
c                        expansion of pbar(m,n,theta).
c
c                        1) for n even and m even, pbar(m,n,theta) =
c                           .5*cp(1) plus the sum from k=1 to k=n/2
c                           of cp(k+1)*cos(2*k*th)
c
c                        2) for n even and m odd, pbar(m,n,theta) =
c                           the sum from k=1 to k=n/2 of
c                           cp(k)*sin(2*k*th)
c
c                        3) for n odd and m even, pbar(m,n,theta) =
c                           the sum from k=1 to k=(n+1)/2 of
c                           cp(k)*cos((2*k-1)*th)
c
c                        4) for n odd and m odd,  pbar(m,n,theta) =
c                           the sum from k=1 to k=(n+1)/2 of
c                           cp(k)*sin((2*k-1)*th)
c
c
c usage                  call alfk(n,m,cp)
c
c arguments
c
c on input               n
c                          nonnegative integer specifying the degree of
c                          pbar(n,m,theta)
c
c                        m
c                          is the order of pbar(n,m,theta). m can be
c                          any integer however cp is computed such that
c                          pbar(n,m,theta) = 0 if abs(m) is greater
c                          than n and pbar(n,m,theta) = (-1)**m*
c                          pbar(n,-m,theta) for negative m.
c
c on output              cp
c                          single precision array of length (n/2)+1
c                          which contains the fourier coefficients in
c                          the trigonometric series representation of
c                          pbar(n,m,theta)
c
c
c special conditions     none
c
c precision              single
c
c algorithm              the highest order coefficient is determined in
c                        closed form and the remainig coefficients are
c                        determined as the solution of a backward
c                        recurrence relation.
c
c accuracy               comparison between routines alfk and double
c                        precision dalfk on the cray1 indicates
c                        greater accuracy for smaller values
c                        of input parameter n.  agreement to 14
c                        places was obtained for n=10 and to 13
c                        places for n=100.
c
      subroutine alfk (n,m,cp)
      dimension       cp(n/2+1)
      parameter (sc10=1024.)
      parameter (sc20=sc10*sc10)
      parameter (sc40=sc20*sc20)
c
      cp(1) = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if(n-1) 2,3,5
    2 cp(1) = sqrt(2.)
      return
    3 if(ma .ne. 0) go to 4
      cp(1) = sqrt(1.5)
      return
    4 cp(1) = sqrt(.75)
      if(m .eq. -1) cp(1) = -cp(1)
      return
    5 if(mod(n+ma,2) .ne. 0) go to 10
      nmms2 = (n-ma)/2
      fnum = n+ma+1
      fnmh = n-ma+1
      pm1 = 1.
      go to 15
   10 nmms2 = (n-ma-1)/2
      fnum = n+ma+2
      fnmh = n-ma+2
      pm1 = -1.
 15   t1 = 1./sc20
      nex = 20
      fden = 2.
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
   20 t1 = t1/2.**(n-1-nex)
      if(mod(ma/2,2) .ne. 0) t1 = -t1
      t2 = 1. 
      if(ma .eq. 0) go to 26
      do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
   25 continue
   26 cp2 = t1*sqrt((n+.5)*t2)
      fnnp1 = n*(n+1)
      fnmsq = fnnp1-2.*ma*ma
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
c subroutine lfim (init,theta,l,n,nm,pb,id,wlfim)
c
c dimension of           theta(l),  pb(id,nm+1),  wlfim(4*l*(nm+1))
c arguments
c
c purpose                given n and l, routine lfim calculates
c                        the normalized associated legendre functions
c                        pbar(n,m,theta) for m=0,...,n and theta(i)
c                        for i=1,...,l where
c
c                        pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)
c                        /(2*factorial(n+m)))*sin(theta)**m/(2**n*
c                        factorial(n)) times the (n+m)th derivative of
c                        (x**2-1)**n with respect to x=cos(theta)
c
c usage                  call lfim (init,theta,l,n,nm,pb,id,wlfim)
c
c arguments
c on input               init
c                        = 0
c                            initialization only - using parameters
c                            l, nm and array theta, subroutine lfim
c                            initializes array wlfim for subsequent
c                            use in the computation of the associated
c                            legendre functions pb. initialization
c                            does not have to be repeated unless
c                            l, nm, or array theta are changed.
c                        = 1
c                            subroutine lfim uses the array wlfim that
c                            was computed with init = 0 to compute pb.
c
c                        theta
c                          an array that contains the colatitudes
c                          at which the associated legendre functions
c                          will be computed. the colatitudes must be
c                          specified in radians.
c
c                        l
c                          the length of the theta array. lfim is
c                          vectorized with vector length l.
c
c                        n
c                          nonnegative integer, less than nm, specifying
c                          degree of pbar(n,m,theta). subroutine lfim
c                          must be called starting with n=0. n must be
c                          incremented by one in subsequent calls and
c                          must not exceed nm.
c
c                        nm
c                          the maximum value of n and m
c
c                        id
c                          the first dimension of the two dimensional
c                          array pb as it appears in the program that
c                          calls lfim. (see output parameter pb)
c
c                        wlfim
c                          an array with length 4*l*(nm+1) which
c                          must be initialized by calling lfim
c                          with init=0 (see parameter init)  it
c                          must not be altered between calls to
c                          lfim.
c
c
c on output              pb
c                          a two dimensional array with first
c                          dimension id in the program that calls
c                          lfim. the second dimension of pb must
c                          be at least nm+1. starting with n=0
c                          lfim is called repeatedly with n being
c                          increased by one between calls. on each
c                          call, subroutine lfim computes
c                          = pbar(m,n,theta(i)) for m=0,...,n and
c                          i=1,...l.
c
c                        wlfim
c                          array containing values which must not
c                          be altered unless l, nm or the array theta
c                          are changed in which case lfim must be
c                          called with init=0 to reinitialize the
c                          wlfim array.
c
c special conditions     n must be increased by one between calls
c                        of lfim in which n is not zero.
c
c precision              single
c
c
c algorithm              routine lfim calculates pbar(n,m,theta) using
c                        a four term recurrence relation. (unpublished
c                        notes by paul n. swarztrauber)
c
      subroutine lfim (init,theta,l,n,nm,pb,id,wlfim)
      dimension       pb(1)        ,wlfim(1)
c
c     total length of wlfim is 4*l*(nm+1)
c
      lnx = l*(nm+1)
      iw1 = lnx+1
      iw2 = iw1+lnx
      iw3 = iw2+lnx
      call lfim1(init,theta,l,n,nm,id,pb,wlfim,wlfim(iw1),
     1                wlfim(iw2),wlfim(iw3),wlfim(iw2))
      return
      end
      subroutine lfim1(init,theta,l,n,nm,id,p3,phz,ph1,p1,p2,cp)
      dimension       p1(l,1)    ,p2(l,1)    ,p3(id,1)   ,phz(l,1)   ,
     1                ph1(l,1)   ,cp(1)      ,theta(1)
      nmp1 = nm+1
      if(init .ne. 0) go to 5
      ssqrt2 = 1./sqrt(2.)
      do 10 i=1,l
      phz(i,1) = ssqrt2
   10 continue
      do 15 np1=2,nmp1
      nh = np1-1
      call alfk(nh,0,cp)
      do 16 i=1,l
      call lfpt(nh,0,theta(i),cp,phz(i,np1))
   16 continue
      call alfk(nh,1,cp)
      do 17 i=1,l
      call lfpt(nh,1,theta(i),cp,ph1(i,np1))
   17 continue
   15 continue
      return
    5 if(n .gt. 2) go to 60
      if(n-1)25,30,35
   25 do 45 i=1,l
      p3(i,1)=phz(i,1)
   45 continue
      return
   30 do 50 i=1,l
      p3(i,1) = phz(i,2)
      p3(i,2) = ph1(i,2)
   50 continue
      return
   35 sq5s6 = sqrt(5./6.)
      sq1s6 = sqrt(1./6.)
      do 55 i=1,l
      p3(i,1) = phz(i,3)
      p3(i,2) = ph1(i,3)
      p3(i,3) = sq5s6*phz(i,1)-sq1s6*p3(i,1)
      p1(i,1) = phz(i,2)
      p1(i,2) = ph1(i,2)
      p2(i,1) = phz(i,3)
      p2(i,2) = ph1(i,3)
      p2(i,3) = p3(i,3)
   55 continue
      return
   60 nm1 = n-1
      np1 = n+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      do 65 i=1,l
      p3(i,1) = phz(i,np1)
      p3(i,2) = ph1(i,np1)
   65 continue
      if(nm1 .lt. 3) go to 71
      do 70 mp1=3,nm1
      m = mp1-1
      fm = float(m)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      cc = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      dd = sqrt(cn*fnmm*(fnmm-1.)/temp)
      ee = sqrt((fnmm+1.)*(fnmm+2.)/temp)
      do 70 i=1,l
      p3(i,mp1) = cc*p1(i,mp1-2)+dd*p1(i,mp1)-ee*p3(i,mp1-2)
   70 continue
   71 fnpm = fn+fn-1.
      temp = fnpm*(fnpm-1.)
      cc = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      ee = sqrt(6./temp)
      do 75 i=1,l
      p3(i,n) = cc*p1(i,n-2)-ee*p3(i,n-2)
   75 continue
      fnpm = fn+fn
      temp = fnpm*(fnpm-1.)
      cc = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      ee = sqrt(2./temp)
      do 80 i=1,l
      p3(i,n+1) = cc*p1(i,n-1)-ee*p3(i,n-1)
   80 continue
      do 90 mp1=1,np1
      do 90 i=1,l
      p1(i,mp1) = p2(i,mp1)
      p2(i,mp1) = p3(i,mp1)
   90 continue
      return
      end
c subroutine lfin (init,theta,l,m,nm,pb,id,wlfin)
c
c dimension of           theta(l),  pb(id,nm+1),  wlfin(4*l*(nm+1))
c arguments
c
c purpose                given m and l, routine lfin calculates
c                        the normalized associated legendre functions
c                        pbar(n,m,theta) for n=m,...,nm and theta(i)
c                        for i=1,...,l where
c
c                        pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)
c                        /(2*factorial(n+m)))*sin(theta)**m/(2**n*
c                        factorial(n)) times the (n+m)th derivative of
c                        (x**2-1)**n with respect to x=cos(theta)
c
c usage                  call lfin (init,theta,l,m,nm,pb,id,wlfin)
c
c arguments
c on input               init
c                        = 0
c                            initialization only - using parameters
c                            l, nm and the array theta, subroutine lfin
c                            initializes the array wlfin for subsequent
c                            use in the computation of the associated
c                            legendre functions pb. initialization does
c                            not have to be repeated unless l, nm or
c                            the array theta are changed.
c                        = 1
c                            subroutine lfin uses the array wlfin that
c                            was computed with init = 0 to compute pb
c
c                        theta
c                          an array that contains the colatitudes
c                          at which the associated legendre functions
c                          will be computed. the colatitudes must be
c                          specified in radians.
c
c                        l
c                          the length of the theta array. lfin is
c                          vectorized with vector length l.
c
c                        m
c                          nonnegative integer, less than nm, specifying
c                          degree of pbar(n,m,theta). subroutine lfin
c                          must be called starting with n=0. n must be
c                          incremented by one in subsequent calls and
c                          must not exceed nm.
c
c                        nm
c                          the maximum value of n and m
c
c                        id
c                          the first dimension of the two dimensional
c                          array pb as it appears in the program that
c                          calls lfin. (see output parameter pb)
c
c                        wlfin
c                          an array with length 4*l*(nm+1) which
c                          must be initialized by calling lfin
c                          with init=0 (see parameter init)  it
c                          must not be altered between calls to
c                          lfin.
c
c
c on output              pb
c                          a two dimensional array with first
c                          dimension id in the program that calls
c                          lfin. the second dimension of pb must
c                          be at least nm+1. starting with m=0
c                          lfin is called repeatedly with m being
c                          increased by one between calls. on each
c                          call, subroutine lfin computes pb(i,n+1)
c                          = pbar(m,n,theta(i)) for n=m,...,nm and
c                          i=1,...l.
c
c                        wlfin
c                          array containing values which must not
c                          be altered unless l, nm or the array theta
c                          are changed in which case lfin must be
c                          called with init=0 to reinitialize the
c                          wlfin array.
c
c special conditions     m must be increased by one between calls
c                        of lfin in which m is not zero.
c
c precision              single
c
c algorithm              routine lfin calculates pbar(n,m,theta) using
c                        a four term recurrence relation. (unpublished
c                        notes by paul n. swarztrauber)
c
      subroutine lfin (init,theta,l,m,nm,pb,id,wlfin)
      dimension       pb(1)        ,wlfin(1)
c
c     total length of wlfin is 4*l*(nm+1)
c
      lnx = l*(nm+1)
      iw1 = lnx+1
      iw2 = iw1+lnx
      iw3 = iw2+lnx
      call lfin1(init,theta,l,m,nm,id,pb,wlfin,wlfin(iw1),
     1                wlfin(iw2),wlfin(iw3),wlfin(iw2))
      return
      end
      subroutine lfin1(init,theta,l,m,nm,id,p3,phz,ph1,p1,p2,cp)
      dimension       p1(l,1)    ,p2(l,1)    ,p3(id,1)   ,phz(l,1)   ,
     1                ph1(l,1)   ,cp(1)      ,theta(1)
      nmp1 = nm+1
      if(init .ne. 0) go to 5
      ssqrt2 = 1./sqrt(2.)
      do 10 i=1,l
      phz(i,1) = ssqrt2
   10 continue
      do 15 np1=2,nmp1
      nh = np1-1
      call alfk(nh,0,cp)
      do 16 i=1,l
      call lfpt(nh,0,theta(i),cp,phz(i,np1))
   16 continue
      call alfk(nh,1,cp)
      do 17 i=1,l
      call lfpt(nh,1,theta(i),cp,ph1(i,np1))
   17 continue
   15 continue
      return
    5 mp1 = m+1
      fm = float(m)
      tm = fm+fm
      if(m-1)25,30,35
   25 do 45 np1=1,nmp1
      do 45 i=1,l
      p3(i,np1) = phz(i,np1)
      p1(i,np1) = phz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nmp1
      do 50 i=1,l
      p3(i,np1) = ph1(i,np1)
      p2(i,np1) = ph1(i,np1)
   50 continue
      return
   35 temp = tm*(tm-1.)
      cc = sqrt((tm+1.)*(tm-2.)/temp)
      ee = sqrt(2./temp)
      do 85 i=1,l
      p3(i,m+1) = cc*p1(i,m-1)-ee*p1(i,m+1)
   85 continue
      if(m .eq. nm) return
      temp = tm*(tm+1.)
      cc = sqrt((tm+3.)*(tm-2.)/temp)
      ee = sqrt(6./temp)
      do 70 i=1,l
      p3(i,m+2) = cc*p1(i,m)-ee*p1(i,m+2)
   70 continue
      mp3 = m+3
      if(nmp1 .lt. mp3) go to 80
      do 75 np1=mp3,nmp1
      n = np1-1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      cc = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      dd = sqrt(cn*fnmm*(fnmm-1.)/temp)
      ee = sqrt((fnmm+1.)*(fnmm+2.)/temp)
      do 75 i=1,l
      p3(i,np1) = cc*p1(i,np1-2)+dd*p3(i,np1-2)-ee*p1(i,np1)
   75 continue
   80 do 90 np1=m,nmp1
      do 90 i=1,l
      p1(i,np1) = p2(i,np1)
      p2(i,np1) = p3(i,np1)
   90 continue
      return
      end
c subroutine lfpt (n,m,theta,cp,pb)
c
c dimension of
c arguments
c                        cp((n/2)+1)
c
c purpose                routine lfpt uses coefficients computed by
c                        routine alfk to compute the single precision
c                        normalized associated legendre function pbar(n,
c                        m,theta) at colatitude theta.
c
c usage                  call lfpt(n,m,theta,cp,pb)
c
c arguments
c
c on input               n
c                          nonnegative integer specifying the degree of
c                          pbar(n,m,theta)
c                        m
c                          is the order of pbar(n,m,theta). m can be
c                          any integer however pbar(n,m,theta) = 0
c                          if abs(m) is greater than n and
c                          pbar(n,m,theta) = (-1)**m*pbar(n,-m,theta)
c                          for negative m.
c
c                        theta
c                          single precision colatitude in radians
c
c                        cp
c                          single precision array of length (n/2)+1
c                          containing coefficients computed by routine
c                          alfk
c
c on output              pb
c                          single precision variable containing
c                          pbar(n,m,theta)
c
c special conditions     calls to routine lfpt must be preceded by an
c                        appropriate call to routine alfk.
c
c precision              single
c
c algorithm              the trigonometric series formula used by
c                        routine lfpt to calculate pbar(n,m,th) at
c                        colatitude th depends on m and n as follows:
c
c                           1) for n even and m even, the formula is
c                              .5*cp(1) plus the sum from k=1 to k=n/2
c                              of cp(k)*cos(2*k*th)
c                           2) for n even and m odd. the formula is
c                              the sum from k=1 to k=n/2 of
c                              cp(k)*sin(2*k*th)
c                           3) for n odd and m even, the formula is
c                              the sum from k=1 to k=(n+1)/2 of
c                              cp(k)*cos((2*k-1)*th)
c                           4) for n odd and m odd, the formula is
c                              the sum from k=1 to k=(n+1)/2 of
c                              cp(k)*sin((2*k-1)*th)
c
c accuracy               comparison between routines lfpt and double
c                        precision dlfpt on the cray1 indicates greater
c                        accuracy for greater values on input parameter
c                        n.  agreement to 13 places was obtained for
c                        n=10 and to 12 places for n=100.
c
c timing                 time per call to routine lfpt is dependent on
c                        the input parameter n.
c
      subroutine lfpt (n,m,theta,cp,pb)
      dimension       cp(1)
c
      pb = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if (n)  10, 10, 30
   10 if (ma)  20, 20, 30
   20 pb= sqrt(.5)
      go to 140
   30 np1 = n+1
      nmod = mod(n,2)
      mmod = mod(ma,2)
      if (nmod)  40, 40, 90
   40 if (mmod)  50, 50, 70
   50 kdo = n/2+1
      cdt = cos(theta+theta)
      sdt = sin(theta+theta)
      ct = 1.
      st = 0.
      sum = .5*cp(1)
      do  60 kp1=2,kdo
         cth = cdt*ct-sdt*st
         st = sdt*ct+cdt*st
         ct = cth
         sum = sum+cp(kp1)*ct
   60 continue
      pb= sum
      go to 140
   70 kdo = n/2
      cdt = cos(theta+theta)
      sdt = sin(theta+theta)
      ct = 1.
      st = 0.
      sum = 0.
      do  80 k=1,kdo
         cth = cdt*ct-sdt*st
         st = sdt*ct+cdt*st
         ct = cth
         sum = sum+cp(k)*st
   80 continue
      pb= sum
      go to 140
   90 kdo = (n+1)/2
      if (mmod) 100,100,120
  100 cdt = cos(theta+theta)
      sdt = sin(theta+theta)
      ct = cos(theta)
      st = -sin(theta)
      sum = 0.
      do 110 k=1,kdo
         cth = cdt*ct-sdt*st
         st = sdt*ct+cdt*st
         ct = cth
         sum = sum+cp(k)*ct
  110 continue
      pb= sum
      go to 140
  120 cdt = cos(theta+theta)
      sdt = sin(theta+theta)
      ct = cos(theta)
      st = -sin(theta)
      sum = 0.
      do 130 k=1,kdo
         cth = cdt*ct-sdt*st
         st = sdt*ct+cdt*st
         ct = cth
         sum = sum+cp(k)*st
  130 continue
      pb= sum
  140 return
      end
