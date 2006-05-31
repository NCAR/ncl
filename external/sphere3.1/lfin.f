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

