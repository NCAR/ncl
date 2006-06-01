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
      SUBROUTINE DLFIM(INIT,THETA,L,N,NM,PB,ID,WLFIM)
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PB
      DOUBLE PRECISION WLFIM
      DIMENSION PB(1),WLFIM(1)
c
c     total length of wlfim is 4*l*(nm+1)
c
      LNX = L* (NM+1)
      IW1 = LNX + 1
      IW2 = IW1 + LNX
      IW3 = IW2 + LNX
      CALL DLFIM1(INIT,THETA,L,N,NM,ID,PB,WLFIM,WLFIM(IW1),WLFIM(IW2),
     +           WLFIM(IW3),WLFIM(IW2))
      RETURN
      END
      SUBROUTINE DLFIM1(INIT,THETA,L,N,NM,ID,P3,PHZ,PH1,P1,P2,CP)
      DOUBLE PRECISION THETA
      DOUBLE PRECISION P3
      DOUBLE PRECISION PHZ
      DOUBLE PRECISION PH1
      DOUBLE PRECISION P1
      DOUBLE PRECISION P2
      DOUBLE PRECISION CP
      DOUBLE PRECISION SSQRT2
      DOUBLE PRECISION SQ5S6
      DOUBLE PRECISION SQ1S6
      DOUBLE PRECISION FN
      DOUBLE PRECISION TN
      DOUBLE PRECISION CN
      DOUBLE PRECISION FM
      DOUBLE PRECISION FNPM
      DOUBLE PRECISION FNMM
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION CC
      DOUBLE PRECISION DD
      DOUBLE PRECISION EE
      DIMENSION P1(L,1),P2(L,1),P3(ID,1),PHZ(L,1),PH1(L,1),CP(1),
     +          THETA(1)

      NMP1 = NM + 1
      IF (INIT.NE.0) GO TO 5
      SSQRT2 = 1.D0/SQRT(2.D0)
      DO 10 I = 1,L
          PHZ(I,1) = SSQRT2
   10 CONTINUE
      DO 15 NP1 = 2,NMP1
          NH = NP1 - 1
          CALL DALFK(NH,0,CP)
          DO 16 I = 1,L
              CALL DLFPT(NH,0,THETA(I),CP,PHZ(I,NP1))
   16     CONTINUE
          CALL DALFK(NH,1,CP)
          DO 17 I = 1,L
              CALL DLFPT(NH,1,THETA(I),CP,PH1(I,NP1))
   17     CONTINUE
   15 CONTINUE
      RETURN
    5 IF (N.GT.2) GO TO 60
      IF (N-1) 25,30,35
   25 DO 45 I = 1,L
          P3(I,1) = PHZ(I,1)
   45 CONTINUE
      RETURN
   30 DO 50 I = 1,L
          P3(I,1) = PHZ(I,2)
          P3(I,2) = PH1(I,2)
   50 CONTINUE
      RETURN
   35 SQ5S6 = SQRT(5.D0/6.D0)
      SQ1S6 = SQRT(1.D0/6.D0)
      DO 55 I = 1,L
          P3(I,1) = PHZ(I,3)
          P3(I,2) = PH1(I,3)
          P3(I,3) = SQ5S6*PHZ(I,1) - SQ1S6*P3(I,1)
          P1(I,1) = PHZ(I,2)
          P1(I,2) = PH1(I,2)
          P2(I,1) = PHZ(I,3)
          P2(I,2) = PH1(I,3)
          P2(I,3) = P3(I,3)
   55 CONTINUE
      RETURN
   60 NM1 = N - 1
      NP1 = N + 1
      FN = DBLE(N)
      TN = FN + FN
      CN = (TN+1.D0)/ (TN-3.D0)
      DO 65 I = 1,L
          P3(I,1) = PHZ(I,NP1)
          P3(I,2) = PH1(I,NP1)
   65 CONTINUE
      IF (NM1.LT.3) GO TO 71
      DO 70 MP1 = 3,NM1
          M = MP1 - 1
          FM = DBLE(M)
          FNPM = FN + FM
          FNMM = FN - FM
          TEMP = FNPM* (FNPM-1.D0)
          CC = SQRT(CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
          DD = SQRT(CN*FNMM* (FNMM-1.D0)/TEMP)
          EE = SQRT((FNMM+1.D0)* (FNMM+2.D0)/TEMP)
          DO 70 I = 1,L
              P3(I,MP1) = CC*P1(I,MP1-2) + DD*P1(I,MP1) - EE*P3(I,MP1-2)
   70 CONTINUE
   71 FNPM = FN + FN - 1.D0
      TEMP = FNPM* (FNPM-1.D0)
      CC = SQRT(CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
      EE = SQRT(6.D0/TEMP)
      DO 75 I = 1,L
          P3(I,N) = CC*P1(I,N-2) - EE*P3(I,N-2)
   75 CONTINUE
      FNPM = FN + FN
      TEMP = FNPM* (FNPM-1.D0)
      CC = SQRT(CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
      EE = SQRT(2.D0/TEMP)
      DO 80 I = 1,L
          P3(I,N+1) = CC*P1(I,N-1) - EE*P3(I,N-1)
   80 CONTINUE
      DO 90 MP1 = 1,NP1
          DO 90 I = 1,L
              P1(I,MP1) = P2(I,MP1)
              P2(I,MP1) = P3(I,MP1)
   90 CONTINUE
      RETURN
      END
