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

