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
