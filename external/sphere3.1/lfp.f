c subroutine lfp (init,n,m,l,cp,pb,w)
c
c dimension of           cp((n/2)+1), pb(l), w(5*l+41)
c arguments
c
c purpose                routine lfp uses coefficients computed by
c                        routine alfk to calculate the single precision
c                        normalized associated legendre function pbar(n,
c                        m,theta) at colatitudes theta=(i-1)*pi/(l-1),
c                        i=1,...,l. subroutine lfp evaluates pbar
c                        using one of the following trigonometric
c                        expansions
c
c                        1) for n even and m even, pbar(m,n,theta) =
c                           .5*cp(1) plus the sum from k=1 to k=n/2
c                           of cp(k)*cos(2*k*th)
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
c usage                  call lfp(init,n,m,l,cp,pb,w)
c
c arguments
c
c on input               init
c                          = 0 initialization only
c                          = 1 compute pbar(n,m,theta)
c
c                          lfp call with init = 0 initializes array w;
c                          no values of pbar(n,m,theta) are computed.
c                          init=0 should be used on the first call, or
c                          if l or w values differ from those in the
c                          previous call.
c
c                        n
c                          nonnegative integer, less than l, specifying
c                          the degree of pbar(n,m,theta)
c
c                        m
c                          is the order of pbar(n,m,theta). m can be
c                          any integer however pbar(n,m,theta) = 0
c                          if abs(m) is greater than n and
c                          pbar(n,m,theta) = (-1)**m*pbar(n,-m,theta)
c                          for negative m.
c
c                        l
c                          number of colatitudes theta=(i-1)*pi/(l-1)
c                          for i=1,...,l where l is greater than 1.
c                          l must be an odd integer.
c
c                        cp
c                          single precision array of length (n/2)+1
c                          containing coefficients computed by routine
c                          alfk
c
c                        w
c                          a single precision work array with at
c                          least 5*l+41 locations
c
c on output              pb
c                          single precision array of length l containing
c                          pbar(n,m,theta), theta=(i-1)*pi/(l-1) for i=1
c                          ,...,l.
c
c                        w
c                          a single precision array containing values
c                          which must not be destroyed if the next call
c                          will have the same value of input parameter n
c
c special conditions     calls to routine lfp must be preceded by an
c                        appropriate call to routine alfk.
c
c precision              single
c
c algorithm              the trigonometric series formula used by
c                        routine lfp to calculate pbar(n,m,theta) for
c                        theta=(i-1)*pi/(l-1), i=1,...,n, depends on
c                        m and n as follows:
c
c                           1) for n even and m even, the formula is
c                              .5*cp(1) plus the sum from k=1 to k=n/2
c                              of cp(k)*cos(2*k*theta)
c                           2) for n even and m odd. the formula is
c                              the sum from k=1 to k=n/2 of
c                              cp(k)*sin(2*k*theta)
c                           3) for n odd and m even, the formula is
c                              the sum from k=1 to k=(n+1)/2 of
c                              cp(k)*cos((2*k-1)*theta)
c                           4) for n odd and m odd, the formula is
c                              the sum from k=1 to k=(n+1)/2 of
c                              cp(k)*sin((2*k-1)*theta)
c
c accuracy               comparison between routines lfp and double
c                        precision dlfp on the cray1 indicates greater
c                        accuracy for smaller values of input parameter
c                        n.  agreement to 12 places was obtained for
c                        n=10 and to 11 places for n=100.
c
c timing                 time per call to routine lfp is dependent on
c                        the input parameters l and n.
c
      subroutine lfp (init,n,m,l,cp,pb,w)
      dimension       cp(1)       ,pb(1)    ,w(1)
c
      do 10 i=1,l
      pb(i) = 0.
   10 continue
      ma = iabs(m)
      if(ma .gt. n) return
      iw1 = l+l+12
      iw2 = iw1+3*(l+1)/2+15
      call lfp1(init,n,ma,l,cp,pb,w,w(iw1),w(iw2))
      return
      end
      subroutine lfp1(init,n,m,l,cp,p,wsave1,wsave2,wsave3)
      dimension cp(1),p(1),wsave1(1),wsave2(1),wsave3(1)
      save lc, lq, ls
      if(init.ne.0) go to 41
      lc=(l+1)/2
      ls=lc-2
      lq=lc-1
      call sinti(ls,wsave1)
      call costi(lc,wsave2)
      call cosqi(lq,wsave3)
      return
   41 if (n)  10, 10, 40
   10 if (m)  20, 20, 40
   20 ssqrt2 = 1./sqrt(2.)
      do  30 i=1,l
         p(i) = ssqrt2
   30 continue
      return
   40 ls2 = (l+1)/2
      lm1 = l-1
      np1 = n+1
      pi = 4.*atan(1.)
      dt = pi/lm1
      nmod = mod(n,2)
      mmod = mod(m,2)
      if (nmod)  50, 50,120
   50 if (mmod)  60, 60, 90
   60 kdp = n/2+1
      do 70 i=1,kdp
      p(i)=.5*cp(i)
   70 continue
      p(lc)=p(lc)+p(lc)
      call cost(lc,p,wsave2)
      do 80 i=1,lc
      lmi=l-i
      p(lmi+1)=p(i)
   80 continue
      go to 190
   90 kdp=n/2
      do 100 i=1,kdp
      p(i+1)=.5*cp(i)
  100 continue
      p(ls+2)=0.
      call sint(ls,p(2),wsave1)
      do 110 i=1,ls
      lmi=l-i
      p(lmi)=-p(i+1)
  110 continue
      p(l)=0.
      go to 190
  120 kdp=(n+1)/2
      if(mmod)140,140,160
  140 do 130 i=1,kdp
      p(i)=.25*cp(i)
  130 continue
      call cosqb(lq,p,wsave3)
      do 150 i=1,lq
      lmi=l-i
      p(lmi+1)=-p(i)
  150 continue
      go to 190
  160 do 180 i=1,kdp
      p(i+1)=.25*cp(i)
  180 continue
      call sinqb(lq,p(2),wsave3)
      do 170 i=1,lq
      lmi=l-i
      p(lmi)=p(i+1)
  170 continue
      p(l)=0.
  190 return
      end

