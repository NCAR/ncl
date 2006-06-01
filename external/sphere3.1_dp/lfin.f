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
      SUBROUTINE DLFIN(INIT,THETA,L,M,NM,PB,ID,WLFIN)
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PB
      DOUBLE PRECISION WLFIN
      DIMENSION PB(1),WLFIN(1)
c
c     total length of wlfin is 4*l*(nm+1)
c
      LNX = L* (NM+1)
      IW1 = LNX + 1
      IW2 = IW1 + LNX
      IW3 = IW2 + LNX
      CALL DLFIN1(INIT,THETA,L,M,NM,ID,PB,WLFIN,WLFIN(IW1),WLFIN(IW2),
     +           WLFIN(IW3),WLFIN(IW2))
      RETURN
      END
      SUBROUTINE DLFIN1(INIT,THETA,L,M,NM,ID,P3,PHZ,PH1,P1,P2,CP)
      DOUBLE PRECISION THETA
      DOUBLE PRECISION P3
      DOUBLE PRECISION PHZ
      DOUBLE PRECISION PH1
      DOUBLE PRECISION P1
      DOUBLE PRECISION P2
      DOUBLE PRECISION CP
      DOUBLE PRECISION SSQRT2
      DOUBLE PRECISION FM
      DOUBLE PRECISION TM
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION CC
      DOUBLE PRECISION EE
      DOUBLE PRECISION FN
      DOUBLE PRECISION TN
      DOUBLE PRECISION CN
      DOUBLE PRECISION FNPM
      DOUBLE PRECISION FNMM
      DOUBLE PRECISION DD
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
    5 MP1 = M + 1
      FM = DBLE(M)
      TM = FM + FM
      IF (M-1) 25,30,35
   25 DO 45 NP1 = 1,NMP1
          DO 45 I = 1,L
              P3(I,NP1) = PHZ(I,NP1)
              P1(I,NP1) = PHZ(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 2,NMP1
          DO 50 I = 1,L
              P3(I,NP1) = PH1(I,NP1)
              P2(I,NP1) = PH1(I,NP1)
   50 CONTINUE
      RETURN
   35 TEMP = TM* (TM-1.D0)
      CC = SQRT((TM+1.D0)* (TM-2.D0)/TEMP)
      EE = SQRT(2.D0/TEMP)
      DO 85 I = 1,L
          P3(I,M+1) = CC*P1(I,M-1) - EE*P1(I,M+1)
   85 CONTINUE
      IF (M.EQ.NM) RETURN
      TEMP = TM* (TM+1.D0)
      CC = SQRT((TM+3.D0)* (TM-2.D0)/TEMP)
      EE = SQRT(6.D0/TEMP)
      DO 70 I = 1,L
          P3(I,M+2) = CC*P1(I,M) - EE*P1(I,M+2)
   70 CONTINUE
      MP3 = M + 3
      IF (NMP1.LT.MP3) GO TO 80
      DO 75 NP1 = MP3,NMP1
          N = NP1 - 1
          FN = DBLE(N)
          TN = FN + FN
          CN = (TN+1.D0)/ (TN-3.D0)
          FNPM = FN + FM
          FNMM = FN - FM
          TEMP = FNPM* (FNPM-1.D0)
          CC = SQRT(CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
          DD = SQRT(CN*FNMM* (FNMM-1.D0)/TEMP)
          EE = SQRT((FNMM+1.D0)* (FNMM+2.D0)/TEMP)
          DO 75 I = 1,L
              P3(I,NP1) = CC*P1(I,NP1-2) + DD*P3(I,NP1-2) - EE*P1(I,NP1)
   75 CONTINUE
   80 DO 90 NP1 = M,NMP1
          DO 90 I = 1,L
              P1(I,NP1) = P2(I,NP1)
              P2(I,NP1) = P3(I,NP1)
   90 CONTINUE
      RETURN
      END
