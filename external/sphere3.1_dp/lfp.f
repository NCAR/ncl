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
      SUBROUTINE DLFP(INIT,N,M,L,CP,PB,W)
      DOUBLE PRECISION CP
      DOUBLE PRECISION PB
      DOUBLE PRECISION W
      DIMENSION CP(1),PB(1),W(1)
c
      DO 10 I = 1,L
          PB(I) = 0.D0
   10 CONTINUE
      MA = IABS(M)
      IF (MA.GT.N) RETURN
      IW1 = L + L + 12
      IW2 = IW1 + 3* (L+1)/2 + 15
      CALL DLFP1(INIT,N,MA,L,CP,PB,W,W(IW1),W(IW2))
      RETURN
      END
      SUBROUTINE DLFP1(INIT,N,M,L,CP,P,WSAVE1,WSAVE2,WSAVE3)
      DOUBLE PRECISION CP
      DOUBLE PRECISION P
      DOUBLE PRECISION WSAVE1
      DOUBLE PRECISION WSAVE2
      DOUBLE PRECISION WSAVE3
      DOUBLE PRECISION SSQRT2
      DOUBLE PRECISION PI
      DOUBLE PRECISION DT
      DIMENSION CP(1),P(1),WSAVE1(1),WSAVE2(1),WSAVE3(1)
      SAVE LC,LQ,LS

      IF (INIT.NE.0) GO TO 41
      LC = (L+1)/2
      LS = LC - 2
      LQ = LC - 1
      CALL SINTI(LS,WSAVE1)
      CALL COSTI(LC,WSAVE2)
      CALL COSQI(LQ,WSAVE3)
      RETURN
   41 IF (N) 10,10,40
   10 IF (M) 20,20,40
   20 SSQRT2 = 1.D0/SQRT(2.D0)
      DO 30 I = 1,L
          P(I) = SSQRT2
   30 CONTINUE
      RETURN
   40 LS2 = (L+1)/2
      LM1 = L - 1
      NP1 = N + 1
      PI = 4.D0*ATAN(1.D0)
      DT = PI/LM1
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD) 50,50,120
   50 IF (MMOD) 60,60,90
   60 KDP = N/2 + 1
      DO 70 I = 1,KDP
          P(I) = .5D0*CP(I)
   70 CONTINUE
      P(LC) = P(LC) + P(LC)
      CALL COST(LC,P,WSAVE2)
      DO 80 I = 1,LC
          LMI = L - I
          P(LMI+1) = P(I)
   80 CONTINUE
      GO TO 190
   90 KDP = N/2
      DO 100 I = 1,KDP
          P(I+1) = .5D0*CP(I)
  100 CONTINUE
      P(LS+2) = 0.D0
      CALL SINT(LS,P(2),WSAVE1)
      DO 110 I = 1,LS
          LMI = L - I
          P(LMI) = -P(I+1)
  110 CONTINUE
      P(L) = 0.D0
      GO TO 190
  120 KDP = (N+1)/2
      IF (MMOD) 140,140,160
  140 DO 130 I = 1,KDP
          P(I) = .25D0*CP(I)
  130 CONTINUE
      CALL COSQB(LQ,P,WSAVE3)
      DO 150 I = 1,LQ
          LMI = L - I
          P(LMI+1) = -P(I)
  150 CONTINUE
      GO TO 190
  160 DO 180 I = 1,KDP
          P(I+1) = .25D0*CP(I)
  180 CONTINUE
      CALL SINQB(LQ,P(2),WSAVE3)
      DO 170 I = 1,LQ
          LMI = L - I
          P(LMI) = P(I+1)
  170 CONTINUE
      P(L) = 0.D0
  190 RETURN
      END
