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
      SUBROUTINE DALFK(N,M,CP)
      DOUBLE PRECISION CP
      DOUBLE PRECISION SC10
      DOUBLE PRECISION SC20
      DOUBLE PRECISION SC40
      DOUBLE PRECISION FNUM
      DOUBLE PRECISION FNMH
      DOUBLE PRECISION PM1
      DOUBLE PRECISION T1
      DOUBLE PRECISION FDEN
      DOUBLE PRECISION T2
      DOUBLE PRECISION CP2
      DOUBLE PRECISION FNNP1
      DOUBLE PRECISION FNMSQ
      DOUBLE PRECISION FK
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1
      DOUBLE PRECISION C1
      DIMENSION CP(N/2+1)
      PARAMETER (SC10=1024.D0)
      PARAMETER (SC20=SC10*SC10)
      PARAMETER (SC40=SC20*SC20)
c
      CP(1) = 0.D0
      MA = IABS(M)
      IF (MA.GT.N) RETURN
      IF (N-1) 2,3,5
    2 CP(1) = SQRT(2.D0)
      RETURN
    3 IF (MA.NE.0) GO TO 4
      CP(1) = SQRT(1.5D0)
      RETURN
    4 CP(1) = SQRT(.75D0)
      IF (M.EQ.-1) CP(1) = -CP(1)
      RETURN
    5 IF (MOD(N+MA,2).NE.0) GO TO 10
      NMMS2 = (N-MA)/2
      FNUM = N + MA + 1
      FNMH = N - MA + 1
      PM1 = 1.D0
      GO TO 15
   10 NMMS2 = (N-MA-1)/2
      FNUM = N + MA + 2
      FNMH = N - MA + 2
      PM1 = -1.D0
   15 T1 = 1.D0/SC20
      NEX = 20
      FDEN = 2.D0
      IF (NMMS2.LT.1) GO TO 20
      DO 18 I = 1,NMMS2
          T1 = FNUM*T1/FDEN
          IF (T1.GT.SC20) THEN
              T1 = T1/SC40
              NEX = NEX + 40
          END IF
          FNUM = FNUM + 2.D0
          FDEN = FDEN + 2.D0
   18 CONTINUE
   20 T1 = T1/2.D0** (N-1-NEX)
      IF (MOD(MA/2,2).NE.0) T1 = -T1
      T2 = 1.D0
      IF (MA.EQ.0) GO TO 26
      DO 25 I = 1,MA
          T2 = FNMH*T2/ (FNMH+PM1)
          FNMH = FNMH + 2.D0
   25 CONTINUE
   26 CP2 = T1*SQRT((N+.5D0)*T2)
      FNNP1 = N* (N+1)
      FNMSQ = FNNP1 - 2.D0*MA*MA
      L = (N+1)/2
      IF (MOD(N,2).EQ.0 .AND. MOD(MA,2).EQ.0) L = L + 1
      CP(L) = CP2
      IF (M.GE.0) GO TO 29
      IF (MOD(MA,2).NE.0) CP(L) = -CP(L)
   29 IF (L.LE.1) RETURN
      FK = N
      A1 = (FK-2.D0)* (FK-1.D0) - FNNP1
      B1 = 2.D0* (FK*FK-FNMSQ)
      CP(L-1) = B1*CP(L)/A1
   30 L = L - 1
      IF (L.LE.1) RETURN
      FK = FK - 2.D0
      A1 = (FK-2.D0)* (FK-1.D0) - FNNP1
      B1 = -2.D0* (FK*FK-FNMSQ)
      C1 = (FK+1.D0)* (FK+2.D0) - FNNP1
      CP(L-1) = - (B1*CP(L)+C1*CP(L+1))/A1
      GO TO 30
      END
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
