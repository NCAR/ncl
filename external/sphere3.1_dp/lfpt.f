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
      SUBROUTINE DLFPT(N,M,THETA,CP,PB)
      DOUBLE PRECISION THETA
      DOUBLE PRECISION CP
      DOUBLE PRECISION PB
      DOUBLE PRECISION CDT
      DOUBLE PRECISION SDT
      DOUBLE PRECISION CT
      DOUBLE PRECISION ST
      DOUBLE PRECISION SUM
      DOUBLE PRECISION CTH
      DIMENSION CP(1)
c
      PB = 0.D0
      MA = IABS(M)
      IF (MA.GT.N) RETURN
      IF (N) 10,10,30
   10 IF (MA) 20,20,30
   20 PB = SQRT(.5D0)
      GO TO 140
   30 NP1 = N + 1
      NMOD = MOD(N,2)
      MMOD = MOD(MA,2)
      IF (NMOD) 40,40,90
   40 IF (MMOD) 50,50,70
   50 KDO = N/2 + 1
      CDT = COS(THETA+THETA)
      SDT = SIN(THETA+THETA)
      CT = 1.D0
      ST = 0.D0
      SUM = .5D0*CP(1)
      DO 60 KP1 = 2,KDO
          CTH = CDT*CT - SDT*ST
          ST = SDT*CT + CDT*ST
          CT = CTH
          SUM = SUM + CP(KP1)*CT
   60 CONTINUE
      PB = SUM
      GO TO 140
   70 KDO = N/2
      CDT = COS(THETA+THETA)
      SDT = SIN(THETA+THETA)
      CT = 1.D0
      ST = 0.D0
      SUM = 0.D0
      DO 80 K = 1,KDO
          CTH = CDT*CT - SDT*ST
          ST = SDT*CT + CDT*ST
          CT = CTH
          SUM = SUM + CP(K)*ST
   80 CONTINUE
      PB = SUM
      GO TO 140
   90 KDO = (N+1)/2
      IF (MMOD) 100,100,120
  100 CDT = COS(THETA+THETA)
      SDT = SIN(THETA+THETA)
      CT = COS(THETA)
      ST = -SIN(THETA)
      SUM = 0.D0
      DO 110 K = 1,KDO
          CTH = CDT*CT - SDT*ST
          ST = SDT*CT + CDT*ST
          CT = CTH
          SUM = SUM + CP(K)*CT
  110 CONTINUE
      PB = SUM
      GO TO 140
  120 CDT = COS(THETA+THETA)
      SDT = SIN(THETA+THETA)
      CT = COS(THETA)
      ST = -SIN(THETA)
      SUM = 0.D0
      DO 130 K = 1,KDO
          CTH = CDT*CT - SDT*ST
          ST = SDT*CT + CDT*ST
          CT = CTH
          SUM = SUM + CP(K)*ST
  130 CONTINUE
      PB = SUM
  140 RETURN
      END
