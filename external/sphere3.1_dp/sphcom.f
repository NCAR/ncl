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
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file sphcom.f
c
c     this file must be loaded with all driver level files
c     in spherepack3.0.  it includes undocumented subroutines
c     called by some or all of the drivers
c
      SUBROUTINE DDNLFK(M,N,CP)
C*PT*WARNING* Already double-precision
c
c     cp requires n/2+1 double precision locations
c
      DOUBLE PRECISION CP,FNUM,FDEN,FNMH,A1,B1,C1,CP2,FNNP1,FNMSQ,FK,T1,
     +                 T2,PM1,SC10,SC20,SC40
      DIMENSION CP(1)
C*PT*WARNING* Constant already double-precision
      PARAMETER (SC10=1024.d0)
      PARAMETER (SC20=SC10*SC10)
      PARAMETER (SC40=SC20*SC20)
c
      CP(1) = 0.D0
      MA = IABS(M)
      IF (MA.GT.N) RETURN
      IF (N-1) 2,3,5
C*PT*WARNING* Constant already double-precision
    2 CP(1) = DSQRT(2.d0)
      RETURN
    3 IF (MA.NE.0) GO TO 4
C*PT*WARNING* Constant already double-precision
      CP(1) = DSQRT(1.5d0)
      RETURN
C*PT*WARNING* Constant already double-precision
    4 CP(1) = DSQRT(.75d0)
      IF (M.EQ.-1) CP(1) = -CP(1)
      RETURN
    5 IF (MOD(N+MA,2).NE.0) GO TO 10
      NMMS2 = (N-MA)/2
      FNUM = N + MA + 1
      FNMH = N - MA + 1
C*PT*WARNING* Constant already double-precision
      PM1 = 1.d0
      GO TO 15
   10 NMMS2 = (N-MA-1)/2
      FNUM = N + MA + 2
      FNMH = N - MA + 2
C*PT*WARNING* Constant already double-precision
      PM1 = -1.d0
C*PT*WARNING* Constant already double-precision
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
   15 T1 = 1.d0/SC20
      NEX = 20
C*PT*WARNING* Constant already double-precision
      FDEN = 2.d0
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
C*PT*WARNING* Constant already double-precision
   20 T1 = T1/2.d0** (N-1-NEX)
      IF (MOD(MA/2,2).NE.0) T1 = -T1
      T2 = 1.D0
      IF (MA.EQ.0) GO TO 26
      DO 25 I = 1,MA
          T2 = FNMH*T2/ (FNMH+PM1)
          FNMH = FNMH + 2.D0
   25 CONTINUE
C*PT*WARNING* Constant already double-precision
   26 CP2 = T1*DSQRT((N+.5d0)*T2)
      FNNP1 = N* (N+1)
C*PT*WARNING* Constant already double-precision
      FNMSQ = FNNP1 - 2.d0*MA*MA
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
      SUBROUTINE DDNLFT(M,N,THETA,CP,PB)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP(*),PB,THETA,CDT,SDT,CTH,STH,CHH

      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     n even, m even
c
    3 KDO = N/2
      PB = .5D0*CP(1)
      IF (N.EQ.0) RETURN
      CTH = CDT
      STH = SDT
      DO 170 K = 1,KDO
c     pb = pb+cp(k+1)*dcos(2*k*theta)
          PB = PB + CP(K+1)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  170 CONTINUE
      RETURN
c
c     n even, m odd
c
    4 KDO = N/2
      PB = 0.D0
      CTH = CDT
      STH = SDT
      DO 180 K = 1,KDO
c     pb = pb+cp(k)*dsin(2*k*theta)
          PB = PB + CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  180 CONTINUE
      RETURN
    2 IF (MMOD) 13,13,14
c
c     n odd, m even
c
   13 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 190 K = 1,KDO
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
          PB = PB + CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  190 CONTINUE
      RETURN
c
c     n odd, m odd
c
   14 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 200 K = 1,KDO
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
          PB = PB + CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DNLFTD(M,N,THETA,CP,PB)
c
c     computes the derivative of pmn(theta) with respect to theta
c
      DIMENSION CP(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP,PB,THETA,CDT,SDT,CTH,STH,CHH

      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      NMOD = MOD(N,2)
      MMOD = MOD(ABS(M),2)
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     n even, m even
c
    3 KDO = N/2
C*PT*WARNING* Constant already double-precision
      PB = 0.d0
      IF (N.EQ.0) RETURN
      CTH = CDT
      STH = SDT
      DO 170 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k+1)*dcos(2*k*theta)
          PB = PB - 2.d0*K*CP(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  170 CONTINUE
      RETURN
c
c     n even, m odd
c
    4 KDO = N/2
      PB = 0.D0
      CTH = CDT
      STH = SDT
      DO 180 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dsin(2*k*theta)
          PB = PB + 2.d0*K*CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  180 CONTINUE
      RETURN
    2 IF (MMOD) 13,13,14
c
c     n odd, m even
c
   13 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 190 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
          PB = PB - (2.d0*K-1)*CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  190 CONTINUE
      RETURN
c
c     n odd, m odd
c
   14 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 200 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
          PB = PB + (2.d0*K-1)*CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DLEGIN(MODE,L,NLAT,M,W,PMN,KM)
      DOUBLE PRECISION W
      DOUBLE PRECISION PMN
c     this subroutine computes legendre polynomials for n=m,...,l-1
c     and  i=1,...,late (late=((nlat+mod(nlat,2))/2)gaussian grid
c     in pmn(n+1,i,km) using swarztrauber's recursion formula.
c     the vector w contains quantities precomputed in shigc.
c     legin must be called in the order m=0,1,...,l-1
c     (e.g., if m=10 is sought it must be preceded by calls with
c     m=0,1,2,...,9 in that order)
      DIMENSION W(1),PMN(1)
c     set size of pole to equator gaussian grid
      LATE = (NLAT+MOD(NLAT,2))/2
c     partition w (set pointers for p0n,p1n,abel,bbel,cbel,pmn)
      I1 = 1 + NLAT
      I2 = I1 + NLAT*LATE
      I3 = I2 + NLAT*LATE
      I4 = I3 + (2*NLAT-L)* (L-1)/2
      I5 = I4 + (2*NLAT-L)* (L-1)/2
      CALL DLEGIN1(MODE,L,NLAT,LATE,M,W(I1),W(I2),W(I3),W(I4),W(I5),PMN,
     +            KM)
      RETURN
      END
      SUBROUTINE DLEGIN1(MODE,L,NLAT,LATE,M,P0N,P1N,ABEL,BBEL,CBEL,PMN,
     +                  KM)
      DOUBLE PRECISION P0N
      DOUBLE PRECISION P1N
      DOUBLE PRECISION ABEL
      DOUBLE PRECISION BBEL
      DOUBLE PRECISION CBEL
      DOUBLE PRECISION PMN
      DIMENSION P0N(NLAT,LATE),P1N(NLAT,LATE)
      DIMENSION ABEL(1),BBEL(1),CBEL(1),PMN(NLAT,LATE,3)
      DATA KM0,KM1,KM2/1,2,3/
      SAVE KM0,KM1,KM2
c     define index function used in storing triangular
c     arrays for recursion coefficients (functions of (m,n))
c     for 2.le.m.le.n-1 and 2.le.n.le.l-1
      INDX(M,N) = (N-1)* (N-2)/2 + M - 1
c     for l.le.n.le.nlat and 2.le.m.le.l
      IMNDX(M,N) = L* (L-1)/2 + (N-L-1)* (L-1) + M - 1

c     set do loop indices for full or half sphere
      MS = M + 1
      NINC = 1
      IF (MODE.EQ.1) THEN
c     only compute pmn for n-m odd
          MS = M + 2
          NINC = 2
      ELSE IF (MODE.EQ.2) THEN
c     only compute pmn for n-m even
          MS = M + 1
          NINC = 2
      END IF


      IF (M.GT.1) THEN
          DO 100 NP1 = MS,NLAT,NINC
              N = NP1 - 1
              IMN = INDX(M,N)
              IF (N.GE.L) IMN = IMNDX(M,N)
              DO 100 I = 1,LATE
                  PMN(NP1,I,KM0) = ABEL(IMN)*PMN(N-1,I,KM2) +
     +                             BBEL(IMN)*PMN(N-1,I,KM0) -
     +                             CBEL(IMN)*PMN(NP1,I,KM2)
  100     CONTINUE

      ELSE IF (M.EQ.0) THEN
          DO 101 NP1 = MS,NLAT,NINC
              DO 101 I = 1,LATE
                  PMN(NP1,I,KM0) = P0N(NP1,I)
  101     CONTINUE

      ELSE IF (M.EQ.1) THEN
          DO 102 NP1 = MS,NLAT,NINC
              DO 102 I = 1,LATE
                  PMN(NP1,I,KM0) = P1N(NP1,I)
  102     CONTINUE
      END IF

c     permute column indices
c     km0,km1,km2 store m,m-1,m-2 columns
      KMT = KM0
      KM0 = KM2
      KM2 = KM1
      KM1 = KMT
c     set current m index in output param km
      KM = KMT
      RETURN
      END


      SUBROUTINE DZFIN(ISYM,NLAT,NLON,M,Z,I3,WZFIN)
      DOUBLE PRECISION Z
      DOUBLE PRECISION WZFIN
      DIMENSION Z(1),WZFIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT,NLON/2+1)
      LABC = ((MMAX-2)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of wzfin is 2*lim+3*labc
c
      CALL DZFIN1(ISYM,NLAT,M,Z,IMID,I3,WZFIN,WZFIN(IW1),WZFIN(IW2),
     +           WZFIN(IW3),WZFIN(IW4))
      RETURN
      END
      SUBROUTINE DZFIN1(ISYM,NLAT,M,Z,IMID,I3,ZZ,Z1,A,B,C)
      DOUBLE PRECISION Z
      DOUBLE PRECISION ZZ
      DOUBLE PRECISION Z1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION Z(IMID,NLAT,3),ZZ(IMID,1),Z1(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-1) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 1,NLAT
          DO 45 I = 1,IMID
              Z(I,NP1,I3) = ZZ(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 2,NLAT
          DO 50 I = 1,IMID
              Z(I,NP1,I3) = Z1(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ISYM.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          Z(I,M+1,I3) = A(NS)*Z(I,M-1,I1) - C(NS)*Z(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ISYM.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          Z(I,M+2,I3) = A(NS)*Z(I,M,I1) - C(NS)*Z(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ISYM.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ISYM.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              Z(I,NP1,I3) = A(NS)*Z(I,NP1-2,I1) + B(NS)*Z(I,NP1-2,I3) -
     +                      C(NS)*Z(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DZFINIT(NLAT,NLON,WZFIN,DWORK)
      DOUBLE PRECISION WZFIN
      DIMENSION WZFIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wzfin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of dwork is nlat+2
c
      CALL DZFINI1(NLAT,NLON,IMID,WZFIN,WZFIN(IW1),DWORK,
     +             DWORK(NLAT/2+1))
      RETURN
      END
      SUBROUTINE DZFINI1(NLAT,NLON,IMID,Z,ABC,CZ,WORK)
      DOUBLE PRECISION Z
      DOUBLE PRECISION ABC
c
c     abc must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations
c     where mmax = min0(nlat,nlon/2+1)
c     cz and work must each have nlat+1 locations
c
      DIMENSION Z(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,TH,ZH,CZ(*),WORK(*)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      DO 160 MP1 = 1,2
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDNZFK(NLAT,M,N,CZ,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDNZFT(NLAT,M,N,TH,CZ,ZH)
                  Z(I,NP1,MP1) = ZH
  165         CONTINUE
              Z(1,NP1,MP1) = .5D0*Z(1,NP1,MP1)
  160 CONTINUE
      CALL DRABCP(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DDNZFK(NLAT,M,N,CZ,WORK)
c
c     dnzfk computes the coefficients in the trigonometric
c     expansion of the z functions that are used in spherical
c     harmonic analysis.
c
      DIMENSION CZ(1),WORK(1)
C*PT*WARNING* Already double-precision
c
c     cz and work must both have nlat/2+1 locations
c
      DOUBLE PRECISION SUM,SC1,T1,T2,WORK,CZ

      LC = (NLAT+1)/2
C*PT*WARNING* Constant already double-precision
      SC1 = 2.d0/DBLE(NLAT-1)
      CALL DDNLFK(M,N,WORK)
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     n even, m even
c
    3 KDO = N/2 + 1
      DO 5 IDX = 1,LC
          I = IDX + IDX - 2
C*PT*WARNING* Constant already double-precision
          SUM = WORK(1)/ (1.d0-I*I)
          IF (KDO.LT.2) GO TO 29
          DO 6 KP1 = 2,KDO
              K = KP1 - 1
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
    8         SUM = SUM + WORK(KP1)* (T1+T2)/ (T1*T2)
    6     CONTINUE
   29     CZ(IDX) = SC1*SUM
    5 CONTINUE
      RETURN
c
c     n even, m odd
c
    4 KDO = N/2
      DO 9 IDX = 1,LC
          I = IDX + IDX - 2
          SUM = 0.D0
          DO 101 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
   12         SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
  101     CONTINUE
          CZ(IDX) = SC1*SUM
    9 CONTINUE
      RETURN
    2 IF (MMOD) 13,13,14
c
c     n odd, m even
c
   13 KDO = (N+1)/2
      DO 15 IDX = 1,LC
          I = IDX + IDX - 1
          SUM = 0.D0
          DO 16 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
   18         SUM = SUM + WORK(K)* (T1+T2)/ (T1*T2)
   16     CONTINUE
          CZ(IDX) = SC1*SUM
   15 CONTINUE
      RETURN
c
c     n odd, m odd
c
   14 KDO = (N+1)/2
      DO 19 IDX = 1,LC
          I = IDX + IDX - 3
          SUM = 0.D0
          DO 20 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
   22         SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
   20     CONTINUE
          CZ(IDX) = SC1*SUM
   19 CONTINUE
      RETURN
      END
      SUBROUTINE DDNZFT(NLAT,M,N,TH,CZ,ZH)
      DIMENSION CZ(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CZ,ZH,TH,CDT,SDT,CTH,STH,CHH

      ZH = 0.D0
      CDT = DCOS(TH+TH)
      SDT = DSIN(TH+TH)
      LMOD = MOD(NLAT,2)
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (LMOD) 20,20,10
   10 LC = (NLAT+1)/2
      LQ = LC - 1
      LS = LC - 2
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     nlat odd n even m even
c
    3 ZH = .5D0* (CZ(1)+CZ(LC)*DCOS(2*LQ*TH))
      CTH = CDT
      STH = SDT
      DO 201 K = 2,LQ
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
          ZH = ZH + CZ(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  201 CONTINUE
      RETURN
c
c     nlat odd n even m odd
c
    4 CTH = CDT
      STH = SDT
      DO 202 K = 1,LS
c     zh = zh+cz(k+1)*dsin(2*k*th)
          ZH = ZH + CZ(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  202 CONTINUE
      RETURN
c
c     nlat odd n odd, m even
c
    2 IF (MMOD) 5,5,6
    5 CTH = DCOS(TH)
      STH = DSIN(TH)
      DO 203 K = 1,LQ
c     zh = zh+cz(k)*dcos((2*k-1)*th)
          ZH = ZH + CZ(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  203 CONTINUE
      RETURN
c
c     nlat odd n odd m odd
c
    6 CTH = DCOS(TH)
      STH = DSIN(TH)
      DO 204 K = 1,LQ
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
          ZH = ZH + CZ(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  204 CONTINUE
      RETURN
   20 LC = NLAT/2
      LQ = LC - 1
      IF (NMOD) 30,30,80
   30 IF (MMOD) 40,40,60
c
c     nlat even n even m even
c
   40 ZH = .5D0*CZ(1)
      CTH = CDT
      STH = SDT
      DO 50 K = 2,LC
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
          ZH = ZH + CZ(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   50 CONTINUE
      RETURN
c
c     nlat even n even m odd
c
   60 CTH = CDT
      STH = SDT
      DO 70 K = 1,LQ
c     zh = zh+cz(k+1)*dsin(2*k*th)
          ZH = ZH + CZ(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   70 CONTINUE
      RETURN
c
c     nlat even n odd m even
c
   80 IF (MMOD) 90,90,110
   90 ZH = .5D0*CZ(LC)*DCOS((NLAT-1)*TH)
      CTH = DCOS(TH)
      STH = DSIN(TH)
      DO 100 K = 1,LQ
c     zh = zh+cz(k)*dcos((2*k-1)*th)
          ZH = ZH + CZ(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  100 CONTINUE
      RETURN
c
c     nlat even n odd m odd
c
  110 CTH = DCOS(TH)
      STH = DSIN(TH)
      DO 120 K = 1,LQ
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
          ZH = ZH + CZ(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  120 CONTINUE
      RETURN
      END
      SUBROUTINE DALIN(ISYM,NLAT,NLON,M,P,I3,WALIN)
      DOUBLE PRECISION P
      DOUBLE PRECISION WALIN
      DIMENSION P(1),WALIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT,NLON/2+1)
      LABC = ((MMAX-2)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of walin is ((5*l-7)*l+6)/2
c
      CALL DALIN1(ISYM,NLAT,M,P,IMID,I3,WALIN,WALIN(IW1),WALIN(IW2),
     +           WALIN(IW3),WALIN(IW4))
      RETURN
      END
      SUBROUTINE DALIN1(ISYM,NLAT,M,P,IMID,I3,PZ,P1,A,B,C)
      DOUBLE PRECISION P
      DOUBLE PRECISION PZ
      DOUBLE PRECISION P1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION P(IMID,NLAT,3),PZ(IMID,1),P1(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-1) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 1,NLAT
          DO 45 I = 1,IMID
              P(I,NP1,I3) = PZ(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 2,NLAT
          DO 50 I = 1,IMID
              P(I,NP1,I3) = P1(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ISYM.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          P(I,M+1,I3) = A(NS)*P(I,M-1,I1) - C(NS)*P(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ISYM.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          P(I,M+2,I3) = A(NS)*P(I,M,I1) - C(NS)*P(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ISYM.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ISYM.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              P(I,NP1,I3) = A(NS)*P(I,NP1-2,I1) + B(NS)*P(I,NP1-2,I3) -
     +                      C(NS)*P(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DALINIT(NLAT,NLON,WALIN,DWORK)
      DOUBLE PRECISION WALIN
      DIMENSION WALIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of walin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of work is nlat+1
c
      CALL DALINI1(NLAT,NLON,IMID,WALIN,WALIN(IW1),DWORK)
      RETURN
      END
      SUBROUTINE DALINI1(NLAT,NLON,IMID,P,ABC,CP)
      DOUBLE PRECISION P
      DOUBLE PRECISION ABC
      DIMENSION P(IMID,NLAT,2),ABC(1),CP(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,TH,CP,PH
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      DO 160 MP1 = 1,2
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDNLFK(M,N,CP)
              DO 160 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDNLFT(M,N,TH,CP,PH)
                  P(I,NP1,MP1) = PH
  160 CONTINUE
      CALL DRABCP(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DRABCP(NLAT,NLON,ABC)
      DOUBLE PRECISION ABC
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the associated legendre fuctions. array abc
c     must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations.
c
      DIMENSION ABC(1)

      MMAX = MIN0(NLAT,NLON/2+1)
      LABC = ((MMAX-2)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LABC + 1
      IW2 = IW1 + LABC
      CALL DRABCP1(NLAT,NLON,ABC,ABC(IW1),ABC(IW2))
      RETURN
      END
      SUBROUTINE DRABCP1(NLAT,NLON,A,B,C)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION FM
      DOUBLE PRECISION TM
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION FN
      DOUBLE PRECISION TN
      DOUBLE PRECISION CN
      DOUBLE PRECISION FNPM
      DOUBLE PRECISION FNMM
c
c     coefficients a, b, and c for computing pbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      DIMENSION A(1),B(1),C(1)

      MMAX = MIN0(NLAT,NLON/2+1)
      DO 215 MP1 = 3,MMAX
          M = MP1 - 1
          NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
          FM = DBLE(M)
          TM = FM + FM
          TEMP = TM* (TM-1.D0)
          A(NS) = SQRT((TM+1.D0)* (TM-2.D0)/TEMP)
          C(NS) = SQRT(2.D0/TEMP)
          IF (M.EQ.NLAT-1) GO TO 215
          NS = NS + 1
          TEMP = TM* (TM+1.D0)
          A(NS) = SQRT((TM+3.D0)* (TM-2.D0)/TEMP)
          C(NS) = SQRT(6.D0/TEMP)
          MP3 = M + 3
          IF (MP3.GT.NLAT) GO TO 215
          DO 210 NP1 = MP3,NLAT
              N = NP1 - 1
              NS = NS + 1
              FN = DBLE(N)
              TN = FN + FN
              CN = (TN+1.D0)/ (TN-3.D0)
              FNPM = FN + FM
              FNMM = FN - FM
              TEMP = FNPM* (FNPM-1.D0)
              A(NS) = SQRT(CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
              B(NS) = SQRT(CN*FNMM* (FNMM-1.D0)/TEMP)
              C(NS) = SQRT((FNMM+1.D0)* (FNMM+2.D0)/TEMP)
  210     CONTINUE
  215 CONTINUE
      RETURN
      END
      SUBROUTINE DSEA1(NLAT,NLON,IMID,Z,IDZ,ZIN,WZFIN,DWORK)
      DOUBLE PRECISION Z
      DOUBLE PRECISION ZIN
      DOUBLE PRECISION WZFIN
      DIMENSION Z(IDZ,*),ZIN(IMID,NLAT,3),WZFIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      CALL DZFINIT(NLAT,NLON,WZFIN,DWORK)
      MMAX = MIN0(NLAT,NLON/2+1)
      DO 33 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DZFIN(0,NLAT,NLON,M,ZIN,I3,WZFIN)
          DO 33 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 33 I = 1,IMID
                  Z(MN,I) = ZIN(I,NP1,I3)
   33 CONTINUE
      RETURN
      END
      SUBROUTINE DSES1(NLAT,NLON,IMID,P,PIN,WALIN,DWORK)
      DOUBLE PRECISION P
      DOUBLE PRECISION PIN
      DOUBLE PRECISION WALIN
      DIMENSION P(IMID,*),PIN(IMID,NLAT,3),WALIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      CALL DALINIT(NLAT,NLON,WALIN,DWORK)
      MMAX = MIN0(NLAT,NLON/2+1)
      DO 10 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DALIN(0,NLAT,NLON,M,PIN,I3,WALIN)
          DO 10 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 10 I = 1,IMID
                  P(I,MN) = PIN(I,NP1,I3)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE DZVINIT(NLAT,NLON,WZVIN,DWORK)
      DOUBLE PRECISION WZVIN
      DIMENSION WZVIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wzvin is
c         2*nlat*imid +3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     the length of dwork is nlat+2
c
      CALL DZVINI1(NLAT,NLON,IMID,WZVIN,WZVIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DZVINI1(NLAT,NLON,IMID,ZV,ABC,CZV,WORK)
      DOUBLE PRECISION ZV
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czv and work must each have nlat/2+1  locations
c
      DIMENSION ZV(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CZV(1),ZVH,TH,WORK(1)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(2,NLAT, (NLON+1)/2)
      DO 160 MP1 = 1,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDZVK(NLAT,M,N,CZV,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDZVT(NLAT,M,N,TH,CZV,ZVH)
                  ZV(I,NP1,MP1) = ZVH
  165         CONTINUE
              ZV(1,NP1,MP1) = .5D0*ZV(1,NP1,MP1)
  160 CONTINUE
      CALL DRABCV(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DZWINIT(NLAT,NLON,WZWIN,DWORK)
      DOUBLE PRECISION WZWIN
      DIMENSION WZWIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wzvin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      CALL DZWINI1(NLAT,NLON,IMID,WZWIN,WZWIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DZWINI1(NLAT,NLON,IMID,ZW,ABC,CZW,WORK)
      DOUBLE PRECISION ZW
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czw and work must each have nlat+1 locations
c
      DIMENSION ZW(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CZW(1),ZWH,TH,WORK(1)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(3,NLAT, (NLON+1)/2)
      IF (MDO.LT.2) RETURN
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDZWK(NLAT,M,N,CZW,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDZWT(NLAT,M,N,TH,CZW,ZWH)
                  ZW(I,NP1,M) = ZWH
  165         CONTINUE
              ZW(1,NP1,M) = .5D0*ZW(1,NP1,M)
  160 CONTINUE
      CALL DRABCW(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DZVIN(ITYP,NLAT,NLON,M,ZV,I3,WZVIN)
      DOUBLE PRECISION ZV
      DOUBLE PRECISION WZVIN
      DIMENSION ZV(1),WZVIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of wzvin is 2*lim+3*labc
c
      CALL DZVIN1(ITYP,NLAT,M,ZV,IMID,I3,WZVIN,WZVIN(IW1),WZVIN(IW2),
     +           WZVIN(IW3),WZVIN(IW4))
      RETURN
      END
      SUBROUTINE DZVIN1(ITYP,NLAT,M,ZV,IMID,I3,ZVZ,ZV1,A,B,C)
      DOUBLE PRECISION ZV
      DOUBLE PRECISION ZVZ
      DOUBLE PRECISION ZV1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION ZV(IMID,NLAT,3),ZVZ(IMID,1),ZV1(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-1) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 1,NLAT
          DO 45 I = 1,IMID
              ZV(I,NP1,I3) = ZVZ(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 2,NLAT
          DO 50 I = 1,IMID
              ZV(I,NP1,I3) = ZV1(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ITYP.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          ZV(I,M+1,I3) = A(NS)*ZV(I,M-1,I1) - C(NS)*ZV(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ITYP.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          ZV(I,M+2,I3) = A(NS)*ZV(I,M,I1) - C(NS)*ZV(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ITYP.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ITYP.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              ZV(I,NP1,I3) = A(NS)*ZV(I,NP1-2,I1) +
     +                       B(NS)*ZV(I,NP1-2,I3) - C(NS)*ZV(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DZWIN(ITYP,NLAT,NLON,M,ZW,I3,WZWIN)
      DOUBLE PRECISION ZW
      DOUBLE PRECISION WZWIN
      DIMENSION ZW(1),WZWIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of wzwin is 2*lim+3*labc
c
      CALL DZWIN1(ITYP,NLAT,M,ZW,IMID,I3,WZWIN,WZWIN(IW1),WZWIN(IW2),
     +           WZWIN(IW3),WZWIN(IW4))
      RETURN
      END
      SUBROUTINE DZWIN1(ITYP,NLAT,M,ZW,IMID,I3,ZW1,ZW2,A,B,C)
      DOUBLE PRECISION ZW
      DOUBLE PRECISION ZW1
      DOUBLE PRECISION ZW2
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION ZW(IMID,NLAT,3),ZW1(IMID,1),ZW2(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-2) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 2,NLAT
          DO 45 I = 1,IMID
              ZW(I,NP1,I3) = ZW1(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 3,NLAT
          DO 50 I = 1,IMID
              ZW(I,NP1,I3) = ZW2(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ITYP.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          ZW(I,M+1,I3) = A(NS)*ZW(I,M-1,I1) - C(NS)*ZW(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ITYP.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          ZW(I,M+2,I3) = A(NS)*ZW(I,M,I1) - C(NS)*ZW(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ITYP.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ITYP.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              ZW(I,NP1,I3) = A(NS)*ZW(I,NP1-2,I1) +
     +                       B(NS)*ZW(I,NP1-2,I3) - C(NS)*ZW(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DVBINIT(NLAT,NLON,WVBIN,DWORK)
      DOUBLE PRECISION WVBIN
      DIMENSION WVBIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      CALL DVBINI1(NLAT,NLON,IMID,WVBIN,WVBIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DVBINI1(NLAT,NLON,IMID,VB,ABC,CVB,WORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat+1 locations
c
      DIMENSION VB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CVB(1),TH,VBH,WORK(1)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(2,NLAT, (NLON+1)/2)
      DO 160 MP1 = 1,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDVBK(M,N,CVB,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDVBT(M,N,TH,CVB,VBH)
                  VB(I,NP1,MP1) = VBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCV(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DWBINIT(NLAT,NLON,WWBIN,DWORK)
      DOUBLE PRECISION WWBIN
      DIMENSION WWBIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      CALL DWBINI1(NLAT,NLON,IMID,WWBIN,WWBIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DWBINI1(NLAT,NLON,IMID,WB,ABC,CWB,WORK)
      DOUBLE PRECISION WB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      DIMENSION WB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CWB(1),WBH,TH,WORK(1)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(3,NLAT, (NLON+1)/2)
      IF (MDO.LT.2) RETURN
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDWBK(M,N,CWB,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDWBT(M,N,TH,CWB,WBH)
                  WB(I,NP1,M) = WBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCW(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DVBIN(ITYP,NLAT,NLON,M,VB,I3,WVBIN)
      DOUBLE PRECISION VB
      DOUBLE PRECISION WVBIN
      DIMENSION VB(1),WVBIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of wvbin is 2*lim+3*labc
c
      CALL DVBIN1(ITYP,NLAT,M,VB,IMID,I3,WVBIN,WVBIN(IW1),WVBIN(IW2),
     +           WVBIN(IW3),WVBIN(IW4))
      RETURN
      END
      SUBROUTINE DVBIN1(ITYP,NLAT,M,VB,IMID,I3,VBZ,VB1,A,B,C)
      DOUBLE PRECISION VB
      DOUBLE PRECISION VBZ
      DOUBLE PRECISION VB1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION VB(IMID,NLAT,3),VBZ(IMID,1),VB1(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-1) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 1,NLAT
          DO 45 I = 1,IMID
              VB(I,NP1,I3) = VBZ(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 2,NLAT
          DO 50 I = 1,IMID
              VB(I,NP1,I3) = VB1(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ITYP.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          VB(I,M+1,I3) = A(NS)*VB(I,M-1,I1) - C(NS)*VB(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ITYP.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          VB(I,M+2,I3) = A(NS)*VB(I,M,I1) - C(NS)*VB(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ITYP.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ITYP.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              VB(I,NP1,I3) = A(NS)*VB(I,NP1-2,I1) +
     +                       B(NS)*VB(I,NP1-2,I3) - C(NS)*VB(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DWBIN(ITYP,NLAT,NLON,M,WB,I3,WWBIN)
      DOUBLE PRECISION WB
      DOUBLE PRECISION WWBIN
      DIMENSION WB(1),WWBIN(1)

      IMID = (NLAT+1)/2
      LIM = NLAT*IMID
      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LIM + 1
      IW2 = IW1 + LIM
      IW3 = IW2 + LABC
      IW4 = IW3 + LABC
c
c     the length of wwbin is 2*lim+3*labc
c
      CALL DWBIN1(ITYP,NLAT,M,WB,IMID,I3,WWBIN,WWBIN(IW1),WWBIN(IW2),
     +           WWBIN(IW3),WWBIN(IW4))
      RETURN
      END
      SUBROUTINE DWBIN1(ITYP,NLAT,M,WB,IMID,I3,WB1,WB2,A,B,C)
      DOUBLE PRECISION WB
      DOUBLE PRECISION WB1
      DOUBLE PRECISION WB2
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION WB(IMID,NLAT,3),WB1(IMID,1),WB2(IMID,1),A(1),B(1),C(1)
      SAVE I1,I2

      IHOLD = I1
      I1 = I2
      I2 = I3
      I3 = IHOLD
      IF (M-2) 25,30,35
   25 I1 = 1
      I2 = 2
      I3 = 3
      DO 45 NP1 = 2,NLAT
          DO 45 I = 1,IMID
              WB(I,NP1,I3) = WB1(I,NP1)
   45 CONTINUE
      RETURN
   30 DO 50 NP1 = 3,NLAT
          DO 50 I = 1,IMID
              WB(I,NP1,I3) = WB2(I,NP1)
   50 CONTINUE
      RETURN
   35 NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
      IF (ITYP.EQ.1) GO TO 36
      DO 85 I = 1,IMID
          WB(I,M+1,I3) = A(NS)*WB(I,M-1,I1) - C(NS)*WB(I,M+1,I1)
   85 CONTINUE
   36 IF (M.EQ.NLAT-1) RETURN
      IF (ITYP.EQ.2) GO TO 71
      NS = NS + 1
      DO 70 I = 1,IMID
          WB(I,M+2,I3) = A(NS)*WB(I,M,I1) - C(NS)*WB(I,M+2,I1)
   70 CONTINUE
   71 NSTRT = M + 3
      IF (ITYP.EQ.1) NSTRT = M + 4
      IF (NSTRT.GT.NLAT) GO TO 80
      NSTP = 2
      IF (ITYP.EQ.0) NSTP = 1
      DO 75 NP1 = NSTRT,NLAT,NSTP
          NS = NS + NSTP
          DO 75 I = 1,IMID
              WB(I,NP1,I3) = A(NS)*WB(I,NP1-2,I1) +
     +                       B(NS)*WB(I,NP1-2,I3) - C(NS)*WB(I,NP1,I1)
   75 CONTINUE
   80 RETURN
      END
      SUBROUTINE DDZVK(NLAT,M,N,CZV,WORK)
c
c     subroutine dzvk computes the coefficients in the trigonometric
c     expansion of the quadrature function zvbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of wbarv(n,m,theta)
c
c     m      the order (superscript) of wbarv(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czv     the fourier coefficients of zvbar(n,m,theta).
c
      DIMENSION CZV(1),WORK(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CZV,SC1,SUM,WORK,T1,T2

      IF (N.LE.0) RETURN
      LC = (NLAT+1)/2
C*PT*WARNING* Constant already double-precision
      SC1 = 2.d0/DBLE(NLAT-1)
      CALL DDVBK(M,N,WORK,CZV)
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD.NE.0) GO TO 1
      IF (MMOD.NE.0) GO TO 2
c
c     n even, m even
c
      KDO = N/2
      DO 9 ID = 1,LC
          I = ID + ID - 2
          SUM = 0.D0
          DO 10 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
              SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
   10     CONTINUE
          CZV(ID) = SC1*SUM
    9 CONTINUE
      RETURN
c
c     n even, m odd
c
    2 KDO = N/2
      DO 5 ID = 1,LC
          I = ID + ID - 2
          SUM = 0.D0
          DO 6 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
              SUM = SUM + WORK(K)* (T1+T2)/ (T1*T2)
    6     CONTINUE
          CZV(ID) = SC1*SUM
    5 CONTINUE
      RETURN
    1 IF (MMOD.NE.0) GO TO 3
c
c     n odd, m even
c
      KDO = (N+1)/2
      DO 19 ID = 1,LC
          I = ID + ID - 3
          SUM = 0.D0
          DO 20 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
              SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
   20     CONTINUE
          CZV(ID) = SC1*SUM
   19 CONTINUE
      RETURN
c
c     n odd, m odd
c
    3 KDO = (N+1)/2
      DO 15 ID = 1,LC
          I = ID + ID - 1
          SUM = 0.D0
          DO 16 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
              SUM = SUM + WORK(K)* (T1+T2)/ (T1*T2)
   16     CONTINUE
          CZV(ID) = SC1*SUM
   15 CONTINUE
      RETURN
      END
      SUBROUTINE DDZVT(NLAT,M,N,TH,CZV,ZVH)
c
c     subroutine dzvt tabulates the function zvbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zvbar(n,m,theta)
c
c     m      the order (superscript) of zvbar(n,m,theta)
c
c     czv     the fourier coefficients of zvbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zvh     zvbar(m,n,theta) evaluated at theta = th
c
      DIMENSION CZV(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TH,CZV,ZVH,CTH,STH,CDT,SDT,CHH

      ZVH = 0.D0
      IF (N.LE.0) RETURN
      LC = (NLAT+1)/2
      LQ = LC - 1
      LS = LC - 2
      CTH = DCOS(TH)
      STH = DSIN(TH)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      LMOD = MOD(NLAT,2)
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (LMOD.EQ.0) GO TO 50
      IF (NMOD.NE.0) GO TO 1
      CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 2
c
c     nlat odd  n even  m even
c
      DO 10 K = 1,LS
          ZVH = ZVH + CZV(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     nlat odd  n even  m odd
c
    2 ZVH = .5D0*CZV(1)
      DO 20 K = 2,LQ
          ZVH = ZVH + CZV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      ZVH = ZVH + .5D0*CZV(LC)*DCOS((NLAT-1)*TH)
      RETURN
    1 IF (MMOD.NE.0) GO TO 3
c
c     nlat odd  n odd  m even
c
      DO 30 K = 1,LQ
          ZVH = ZVH + CZV(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   30 CONTINUE
      RETURN
c
c     nlat odd  n odd  m odd
c
    3 DO 40 K = 1,LQ
          ZVH = ZVH + CZV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   40 CONTINUE
      RETURN
   50 IF (NMOD.NE.0) GO TO 51
      CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 52
c
c     nlat even  n even  m even
c
      DO 55 K = 1,LQ
          ZVH = ZVH + CZV(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   55 CONTINUE
      RETURN
c
c     nlat even  n even  m odd
c
   52 ZVH = .5D0*CZV(1)
      DO 57 K = 2,LC
          ZVH = ZVH + CZV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   57 CONTINUE
      RETURN
   51 IF (MMOD.NE.0) GO TO 53
c
c     nlat even  n odd  m even
c
      DO 58 K = 1,LQ
          ZVH = ZVH + CZV(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   58 CONTINUE
      RETURN
c
c     nlat even  n odd  m odd
c
   53 ZVH = .5D0*CZV(LC)*DCOS((NLAT-1)*TH)
      DO 60 K = 1,LQ
          ZVH = ZVH + CZV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   60 CONTINUE
      RETURN
      END
      SUBROUTINE DDZWK(NLAT,M,N,CZW,WORK)
c
c     subroutine dzwk computes the coefficients in the trigonometric
c     expansion of the quadrature function zwbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czw     the fourier coefficients of zwbar(n,m,theta).
c
      DIMENSION CZW(1),WORK(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CZW,WORK,SC1,SUM,T1,T2

      IF (N.LE.0) RETURN
      LC = (NLAT+1)/2
C*PT*WARNING* Constant already double-precision
      SC1 = 2.d0/DBLE(NLAT-1)
      CALL DDWBK(M,N,WORK,CZW)
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD.NE.0) GO TO 1
      IF (MMOD.NE.0) GO TO 2
c
c     n even, m even
c
      KDO = N/2
      DO 19 ID = 1,LC
          I = ID + ID - 3
          SUM = 0.D0
          DO 20 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
              SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
   20     CONTINUE
          CZW(ID) = SC1*SUM
   19 CONTINUE
      RETURN
c
c     n even, m odd
c
    2 KDO = N/2
      DO 15 ID = 1,LC
          I = ID + ID - 1
          SUM = 0.D0
          DO 16 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K-1+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-1-I)**2
              SUM = SUM + WORK(K)* (T1+T2)/ (T1*T2)
   16     CONTINUE
          CZW(ID) = SC1*SUM
   15 CONTINUE
      RETURN
    1 IF (MMOD.NE.0) GO TO 3
c
c     n odd, m even
c
      KDO = (N-1)/2
      DO 9 ID = 1,LC
          I = ID + ID - 2
          SUM = 0.D0
          DO 10 K = 1,KDO
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
              SUM = SUM + WORK(K)* (T1-T2)/ (T1*T2)
   10     CONTINUE
          CZW(ID) = SC1*SUM
    9 CONTINUE
      RETURN
c
c     n odd, m odd
c
    3 KDO = (N+1)/2
      DO 5 ID = 1,LC
          I = ID + ID - 2
C*PT*WARNING* Constant already double-precision
          SUM = WORK(1)/ (1.d0-I*I)
          IF (KDO.LT.2) GO TO 29
          DO 6 KP1 = 2,KDO
              K = KP1 - 1
C*PT*WARNING* Constant already double-precision
              T1 = 1.d0 - (K+K+I)**2
C*PT*WARNING* Constant already double-precision
              T2 = 1.d0 - (K+K-I)**2
              SUM = SUM + WORK(KP1)* (T1+T2)/ (T1*T2)
    6     CONTINUE
   29     CZW(ID) = SC1*SUM
    5 CONTINUE
      RETURN
      END
      SUBROUTINE DDZWT(NLAT,M,N,TH,CZW,ZWH)
c
c     subroutine dzwt tabulates the function zwbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c            nlat must be an odd integer
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     czw     the fourier coefficients of zwbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zwh     zwbar(m,n,theta) evaluated at theta = th
c
      DIMENSION CZW(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CZW,ZWH,TH,CTH,STH,CDT,SDT,CHH

      ZWH = 0.D0
      IF (N.LE.0) RETURN
      LC = (NLAT+1)/2
      LQ = LC - 1
      LS = LC - 2
      CTH = DCOS(TH)
      STH = DSIN(TH)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      LMOD = MOD(NLAT,2)
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (LMOD.EQ.0) GO TO 50
      IF (NMOD.NE.0) GO TO 1
      IF (MMOD.NE.0) GO TO 2
c
c     nlat odd  n even  m even
c
      DO 30 K = 1,LQ
          ZWH = ZWH + CZW(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   30 CONTINUE
      RETURN
c
c     nlat odd  n even  m odd
c
    2 DO 40 K = 1,LQ
          ZWH = ZWH + CZW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   40 CONTINUE
      RETURN
    1 CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 3
c
c     nlat odd  n odd  m even
c
      DO 10 K = 1,LS
          ZWH = ZWH + CZW(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     nlat odd  n odd  m odd
c
    3 ZWH = .5D0*CZW(1)
      DO 20 K = 2,LQ
          ZWH = ZWH + CZW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      ZWH = ZWH + .5D0*CZW(LC)*DCOS((NLAT-1)*TH)
      RETURN
   50 IF (NMOD.NE.0) GO TO 51
      IF (MMOD.NE.0) GO TO 52
c
c     nlat even  n even  m even
c
      DO 55 K = 1,LQ
          ZWH = ZWH + CZW(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   55 CONTINUE
      RETURN
c
c     nlat even  n even  m odd
c
   52 ZWH = .5D0*CZW(LC)*DCOS((NLAT-1)*TH)
      DO 60 K = 1,LQ
          ZWH = ZWH + CZW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   60 CONTINUE
      RETURN
   51 CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 53
c
c     nlat even  n odd  m even
c
      DO 65 K = 1,LQ
          ZWH = ZWH + CZW(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   65 CONTINUE
      RETURN
c
c     nlat even  n odd  m odd
c
   53 ZWH = .5D0*CZW(1)
      DO 70 K = 2,LC
          ZWH = ZWH + CZW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   70 CONTINUE
      RETURN
      END
      SUBROUTINE DDVBK(M,N,CV,WORK)
      DOUBLE PRECISION SRNP1
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CV(1),WORK(1),FN,FK,CF

      CV(1) = 0.D0
      IF (N.LE.0) RETURN
      FN = N
      SRNP1 = DSQRT(FN* (FN+1.D0))
      CF = 2.D0*M/SRNP1
      MODN = MOD(N,2)
      MODM = MOD(M,2)
      CALL DDNLFK(M,N,WORK)
      IF (MODN.NE.0) GO TO 70
      NCV = N/2
      IF (NCV.EQ.0) RETURN
      FK = 0.D0
      IF (MODM.NE.0) GO TO 60
c
c     n even m even
c
      DO 55 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*WORK(L+1)/SRNP1
   55 CONTINUE
      RETURN
c
c     n even m odd
c
   60 DO 65 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = FK*WORK(L)/SRNP1
   65 CONTINUE
      RETURN
   70 NCV = (N+1)/2
      FK = -1.D0
      IF (MODM.NE.0) GO TO 80
c
c     n odd m even
c
      DO 75 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*WORK(L)/SRNP1
   75 CONTINUE
      RETURN
c
c     n odd m odd
c
   80 DO 85 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = FK*WORK(L)/SRNP1
   85 CONTINUE
      RETURN
      END
      SUBROUTINE DDWBK(M,N,CW,WORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CW(1),WORK(1),FN,CF,SRNP1

      CW(1) = 0.D0
      IF (N.LE.0 .OR. M.LE.0) RETURN
      FN = N
      SRNP1 = DSQRT(FN* (FN+1.D0))
      CF = 2.D0*M/SRNP1
      MODN = MOD(N,2)
      MODM = MOD(M,2)
      CALL DDNLFK(M,N,WORK)
      IF (M.EQ.0) GO TO 50
      IF (MODN.NE.0) GO TO 30
      L = N/2
      IF (L.EQ.0) GO TO 50
      IF (MODM.NE.0) GO TO 20
c
c     n even m even
c
      CW(L) = -CF*WORK(L+1)
   10 L = L - 1
      IF (L.LE.0) GO TO 50
      CW(L) = CW(L+1) - CF*WORK(L+1)
      GO TO 10
c
c     n even m odd
c
   20 CW(L) = CF*WORK(L)
   25 L = L - 1
      IF (L.LE.0) GO TO 50
      CW(L) = CW(L+1) + CF*WORK(L)
      GO TO 25
   30 IF (MODM.NE.0) GO TO 40
      L = (N-1)/2
      IF (L.EQ.0) GO TO 50
c
c     n odd m even
c
      CW(L) = -CF*WORK(L+1)
   35 L = L - 1
      IF (L.LE.0) GO TO 50
      CW(L) = CW(L+1) - CF*WORK(L+1)
      GO TO 35
c
c     n odd m odd
c
   40 L = (N+1)/2
      CW(L) = CF*WORK(L)
   45 L = L - 1
      IF (L.LE.0) GO TO 50
      CW(L) = CW(L+1) + CF*WORK(L)
      GO TO 45
   50 RETURN
      END
      SUBROUTINE DDVBT(M,N,THETA,CV,VH)
      DIMENSION CV(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CV,VH,THETA,CTH,STH,CDT,SDT,CHH

      VH = 0.D0
      IF (N.EQ.0) RETURN
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (NMOD.NE.0) GO TO 1
      CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 2
c
c     n even  m even
c
      NCV = N/2
      DO 10 K = 1,NCV
          VH = VH + CV(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     n even  m odd
c
    2 NCV = N/2
      DO 15 K = 1,NCV
          VH = VH + CV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   15 CONTINUE
      RETURN
    1 IF (MMOD.NE.0) GO TO 3
c
c     n odd m even
c
      NCV = (N+1)/2
      DO 20 K = 1,NCV
          VH = VH + CV(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      RETURN
c
c case m odd and n odd
c
    3 NCV = (N+1)/2
      DO 25 K = 1,NCV
          VH = VH + CV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   25 CONTINUE
      RETURN
      END
      SUBROUTINE DDWBT(M,N,THETA,CW,WH)
      DIMENSION CW(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA,CW,WH,CTH,STH,CDT,SDT,CHH

      WH = 0.D0
      IF (N.LE.0 .OR. M.LE.0) RETURN
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (NMOD.NE.0) GO TO 1
      IF (MMOD.NE.0) GO TO 2
c
c     n even  m even
c
      NCW = N/2
      DO 10 K = 1,NCW
          WH = WH + CW(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     n even  m odd
c
    2 NCW = N/2
      DO 8 K = 1,NCW
          WH = WH + CW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
    8 CONTINUE
      RETURN
    1 CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 3
c
c     n odd m even
c
      NCW = (N-1)/2
      DO 20 K = 1,NCW
          WH = WH + CW(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      RETURN
c
c case m odd and n odd
c
    3 NCW = (N+1)/2
      WH = .5D0*CW(1)
      IF (NCW.LT.2) RETURN
      DO 25 K = 2,NCW
          WH = WH + CW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   25 CONTINUE
      RETURN
      END
      SUBROUTINE DRABCV(NLAT,NLON,ABC)
      DOUBLE PRECISION ABC
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the functions vbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      DIMENSION ABC(1)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LABC + 1
      IW2 = IW1 + LABC
      CALL DRABCV1(NLAT,NLON,ABC,ABC(IW1),ABC(IW2))
      RETURN
      END
      SUBROUTINE DRABCV1(NLAT,NLON,A,B,C)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION FM
      DOUBLE PRECISION TM
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION TPN
      DOUBLE PRECISION FN
      DOUBLE PRECISION TN
      DOUBLE PRECISION CN
      DOUBLE PRECISION FNPM
      DOUBLE PRECISION FNMM
c
c     coefficients a, b, and c for computing vbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      DIMENSION A(1),B(1),C(1)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MMAX.LT.3) RETURN
      DO 215 MP1 = 3,MMAX
          M = MP1 - 1
          NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
          FM = DBLE(M)
          TM = FM + FM
          TEMP = TM* (TM-1.D0)
          TPN = (FM-2.D0)* (FM-1.D0)/ (FM* (FM+1.D0))
          A(NS) = SQRT(TPN* (TM+1.D0)* (TM-2.D0)/TEMP)
          C(NS) = SQRT(2.D0/TEMP)
          IF (M.EQ.NLAT-1) GO TO 215
          NS = NS + 1
          TEMP = TM* (TM+1.D0)
          TPN = (FM-1.D0)*FM/ ((FM+1.D0)* (FM+2.D0))
          A(NS) = SQRT(TPN* (TM+3.D0)* (TM-2.D0)/TEMP)
          C(NS) = SQRT(6.D0/TEMP)
          MP3 = M + 3
          IF (MP3.GT.NLAT) GO TO 215
          DO 210 NP1 = MP3,NLAT
              N = NP1 - 1
              NS = NS + 1
              FN = DBLE(N)
              TN = FN + FN
              CN = (TN+1.D0)/ (TN-3.D0)
              TPN = (FN-2.D0)* (FN-1.D0)/ (FN* (FN+1.D0))
              FNPM = FN + FM
              FNMM = FN - FM
              TEMP = FNPM* (FNPM-1.D0)
              A(NS) = SQRT(TPN*CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
              B(NS) = SQRT(TPN*CN*FNMM* (FNMM-1.D0)/TEMP)
              C(NS) = SQRT((FNMM+1.D0)* (FNMM+2.D0)/TEMP)
  210     CONTINUE
  215 CONTINUE
      RETURN
      END
      SUBROUTINE DRABCW(NLAT,NLON,ABC)
      DOUBLE PRECISION ABC
c
c     subroutine rabcw computes the coefficients in the recurrence
c     relation for the functions wbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      DIMENSION ABC(1)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      LABC = (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IW1 = LABC + 1
      IW2 = IW1 + LABC
      CALL DRABCW1(NLAT,NLON,ABC,ABC(IW1),ABC(IW2))
      RETURN
      END
      SUBROUTINE DRABCW1(NLAT,NLON,A,B,C)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION FM
      DOUBLE PRECISION TM
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION TPN
      DOUBLE PRECISION TPH
      DOUBLE PRECISION FN
      DOUBLE PRECISION TN
      DOUBLE PRECISION CN
      DOUBLE PRECISION FNPM
      DOUBLE PRECISION FNMM
c
c     coefficients a, b, and c for computing wbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      DIMENSION A(1),B(1),C(1)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MMAX.LT.4) RETURN
      DO 215 MP1 = 4,MMAX
          M = MP1 - 1
          NS = ((M-2)* (NLAT+NLAT-M-1))/2 + 1
          FM = DBLE(M)
          TM = FM + FM
          TEMP = TM* (TM-1.D0)
          TPN = (FM-2.D0)* (FM-1.D0)/ (FM* (FM+1.D0))
          TPH = FM/ (FM-2.D0)
          A(NS) = TPH*SQRT(TPN* (TM+1.D0)* (TM-2.D0)/TEMP)
          C(NS) = TPH*SQRT(2.D0/TEMP)
          IF (M.EQ.NLAT-1) GO TO 215
          NS = NS + 1
          TEMP = TM* (TM+1.D0)
          TPN = (FM-1.D0)*FM/ ((FM+1.D0)* (FM+2.D0))
          TPH = FM/ (FM-2.D0)
          A(NS) = TPH*SQRT(TPN* (TM+3.D0)* (TM-2.D0)/TEMP)
          C(NS) = TPH*SQRT(6.D0/TEMP)
          MP3 = M + 3
          IF (MP3.GT.NLAT) GO TO 215
          DO 210 NP1 = MP3,NLAT
              N = NP1 - 1
              NS = NS + 1
              FN = DBLE(N)
              TN = FN + FN
              CN = (TN+1.D0)/ (TN-3.D0)
              FNPM = FN + FM
              FNMM = FN - FM
              TEMP = FNPM* (FNPM-1.D0)
              TPN = (FN-2.D0)* (FN-1.D0)/ (FN* (FN+1.D0))
              TPH = FM/ (FM-2.D0)
              A(NS) = TPH*SQRT(TPN*CN* (FNPM-3.D0)* (FNPM-2.D0)/TEMP)
              B(NS) = SQRT(TPN*CN*FNMM* (FNMM-1.D0)/TEMP)
              C(NS) = TPH*SQRT((FNMM+1.D0)* (FNMM+2.D0)/TEMP)
  210     CONTINUE
  215 CONTINUE
      RETURN
      END
      SUBROUTINE DVTINIT(NLAT,NLON,WVBIN,DWORK)
      DOUBLE PRECISION WVBIN
      DIMENSION WVBIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      CALL DVTINI1(NLAT,NLON,IMID,WVBIN,WVBIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DVTINI1(NLAT,NLON,IMID,VB,ABC,CVB,WORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      DIMENSION VB(IMID,NLAT,2),ABC(1),CVB(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CVB,TH,VBH,WORK(*)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(2,NLAT, (NLON+1)/2)
      DO 160 MP1 = 1,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDVTK(M,N,CVB,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDVTT(M,N,TH,CVB,VBH)
                  VB(I,NP1,MP1) = VBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCV(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DWTINIT(NLAT,NLON,WWBIN,DWORK)
      DOUBLE PRECISION WWBIN
      DIMENSION WWBIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      CALL DWTINI1(NLAT,NLON,IMID,WWBIN,WWBIN(IW1),DWORK,
     +             DWORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DWTINI1(NLAT,NLON,IMID,WB,ABC,CWB,WORK)
      DOUBLE PRECISION WB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      DIMENSION WB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION PI,DT,CWB(*),WBH,TH,WORK(*)
C*PT*WARNING* Constant already double-precision
      PI = 4.D0*DATAN(1.d0)
      DT = PI/ (NLAT-1)
      MDO = MIN0(3,NLAT, (NLON+1)/2)
      IF (MDO.LT.2) RETURN
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDWTK(M,N,CWB,WORK)
              DO 165 I = 1,IMID
                  TH = (I-1)*DT
                  CALL DDWTT(M,N,TH,CWB,WBH)
                  WB(I,NP1,M) = WBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCW(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DVTGINT(NLAT,NLON,THETA,WVBIN,WORK)
      DOUBLE PRECISION WVBIN
      DIMENSION WVBIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*),WORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      CALL DVTGIT1(NLAT,NLON,IMID,THETA,WVBIN,WVBIN(IW1),WORK,
     +            WORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DVTGIT1(NLAT,NLON,IMID,THETA,VB,ABC,CVB,WORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1   locations
c
      DIMENSION VB(IMID,NLAT,2),ABC(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*),CVB(*),WORK(*),VBH

      MDO = MIN0(2,NLAT, (NLON+1)/2)
      DO 160 MP1 = 1,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDVTK(M,N,CVB,WORK)
              DO 165 I = 1,IMID
                  CALL DDVTT(M,N,THETA(I),CVB,VBH)
                  VB(I,NP1,MP1) = VBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCV(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DWTGINT(NLAT,NLON,THETA,WWBIN,WORK)
      DOUBLE PRECISION WWBIN
      DIMENSION WWBIN(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*),WORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      CALL DWTGIT1(NLAT,NLON,IMID,THETA,WWBIN,WWBIN(IW1),WORK,
     +            WORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DWTGIT1(NLAT,NLON,IMID,THETA,WB,ABC,CWB,WORK)
      DOUBLE PRECISION WB
      DOUBLE PRECISION ABC
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      DIMENSION WB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*),CWB(*),WORK(*),WBH

      MDO = MIN0(3,NLAT, (NLON+1)/2)
      IF (MDO.LT.2) RETURN
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDWTK(M,N,CWB,WORK)
              DO 165 I = 1,IMID
                  CALL DDWTT(M,N,THETA(I),CWB,WBH)
                  WB(I,NP1,M) = WBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCW(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DDVTK(M,N,CV,WORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CV(*),WORK(*),FN,FK,CF,SRNP1

      CV(1) = 0.D0
      IF (N.LE.0) RETURN
      FN = N
      SRNP1 = DSQRT(FN* (FN+1.D0))
      CF = 2.D0*M/SRNP1
      MODN = MOD(N,2)
      MODM = MOD(M,2)
      CALL DDNLFK(M,N,WORK)
      IF (MODN.NE.0) GO TO 70
      NCV = N/2
      IF (NCV.EQ.0) RETURN
      FK = 0.D0
      IF (MODM.NE.0) GO TO 60
c
c     n even m even
c
      DO 55 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*FK*WORK(L+1)/SRNP1
   55 CONTINUE
      RETURN
c
c     n even m odd
c
   60 DO 65 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*FK*WORK(L)/SRNP1
   65 CONTINUE
      RETURN
   70 NCV = (N+1)/2
      FK = -1.D0
      IF (MODM.NE.0) GO TO 80
c
c     n odd m even
c
      DO 75 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*FK*WORK(L)/SRNP1
   75 CONTINUE
      RETURN
c
c     n odd m odd
c
   80 DO 85 L = 1,NCV
          FK = FK + 2.D0
          CV(L) = -FK*FK*WORK(L)/SRNP1
   85 CONTINUE
      RETURN
      END
      SUBROUTINE DDWTK(M,N,CW,WORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CW(*),WORK(*),FN,CF,SRNP1

      CW(1) = 0.D0
      IF (N.LE.0 .OR. M.LE.0) RETURN
      FN = N
      SRNP1 = DSQRT(FN* (FN+1.D0))
      CF = 2.D0*M/SRNP1
      MODN = MOD(N,2)
      MODM = MOD(M,2)
      CALL DDNLFK(M,N,WORK)
      IF (M.EQ.0) GO TO 50
      IF (MODN.NE.0) GO TO 30
      L = N/2
      IF (L.EQ.0) GO TO 50
      IF (MODM.NE.0) GO TO 20
c
c     n even m even
c
      CW(L) = -CF*WORK(L+1)
   10 L = L - 1
      IF (L.LE.0) GO TO 50
      CW(L) = CW(L+1) - CF*WORK(L+1)
      CW(L+1) = (L+L+1)*CW(L+1)
      GO TO 10
c
c     n even m odd
c
   20 CW(L) = CF*WORK(L)
   25 L = L - 1
      IF (L) 50,27,26
   26 CW(L) = CW(L+1) + CF*WORK(L)
   27 CW(L+1) = - (L+L+1)*CW(L+1)
      GO TO 25
   30 IF (MODM.NE.0) GO TO 40
      L = (N-1)/2
      IF (L.EQ.0) GO TO 50
c
c     n odd m even
c
      CW(L) = -CF*WORK(L+1)
   35 L = L - 1
      IF (L) 50,37,36
   36 CW(L) = CW(L+1) - CF*WORK(L+1)
   37 CW(L+1) = (L+L+2)*CW(L+1)
      GO TO 35
c
c     n odd m odd
c
   40 L = (N+1)/2
      CW(L) = CF*WORK(L)
   45 L = L - 1
      IF (L) 50,47,46
   46 CW(L) = CW(L+1) + CF*WORK(L)
   47 CW(L+1) = - (L+L)*CW(L+1)
      GO TO 45
   50 RETURN
      END
      SUBROUTINE DDVTT(M,N,THETA,CV,VH)
      DIMENSION CV(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CV,VH,THETA,CTH,STH,CDT,SDT,CHH

      VH = 0.D0
      IF (N.EQ.0) RETURN
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (NMOD.NE.0) GO TO 1
      CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 2
c
c     n even  m even
c
      NCV = N/2
      DO 10 K = 1,NCV
          VH = VH + CV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     n even  m odd
c
    2 NCV = N/2
      DO 15 K = 1,NCV
          VH = VH + CV(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   15 CONTINUE
      RETURN
    1 IF (MMOD.NE.0) GO TO 3
c
c     n odd m even
c
      NCV = (N+1)/2
      DO 20 K = 1,NCV
          VH = VH + CV(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      RETURN
c
c case m odd and n odd
c
    3 NCV = (N+1)/2
      DO 25 K = 1,NCV
          VH = VH + CV(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   25 CONTINUE
      RETURN
      END
      SUBROUTINE DDWTT(M,N,THETA,CW,WH)
      DIMENSION CW(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA,CW,WH,CTH,STH,CDT,SDT,CHH

      WH = 0.D0
      IF (N.LE.0 .OR. M.LE.0) RETURN
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      CDT = CTH*CTH - STH*STH
      SDT = 2.D0*STH*CTH
      MMOD = MOD(M,2)
      NMOD = MOD(N,2)
      IF (NMOD.NE.0) GO TO 1
      IF (MMOD.NE.0) GO TO 2
c
c     n even  m even
c
      NCW = N/2
      DO 10 K = 1,NCW
          WH = WH + CW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   10 CONTINUE
      RETURN
c
c     n even  m odd
c
    2 NCW = N/2
      DO 8 K = 1,NCW
          WH = WH + CW(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
    8 CONTINUE
      RETURN
    1 CTH = CDT
      STH = SDT
      IF (MMOD.NE.0) GO TO 3
c
c     n odd m even
c
      NCW = (N-1)/2
      DO 20 K = 1,NCW
          WH = WH + CW(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   20 CONTINUE
      RETURN
c
c case m odd and n odd
c
    3 NCW = (N+1)/2
      WH = 0.D0
      IF (NCW.LT.2) RETURN
      DO 25 K = 2,NCW
          WH = WH + CW(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
   25 CONTINUE
      RETURN
      END
      SUBROUTINE DVBGINT(NLAT,NLON,THETA,WVBIN,WORK)
      DOUBLE PRECISION WVBIN
      DIMENSION WVBIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(*),WORK(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      CALL DVBGIT1(NLAT,NLON,IMID,THETA,WVBIN,WVBIN(IW1),WORK,
     +            WORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DVBGIT1(NLAT,NLON,IMID,THETA,VB,ABC,CVB,WORK)
      DOUBLE PRECISION VB
      DOUBLE PRECISION ABC
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      DIMENSION VB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CVB(1),THETA(1),VBH,WORK(1)

      MDO = MIN0(2,NLAT, (NLON+1)/2)
      DO 160 MP1 = 1,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDVBK(M,N,CVB,WORK)
              DO 165 I = 1,IMID
                  CALL DDVBT(M,N,THETA(I),CVB,VBH)
                  VB(I,NP1,MP1) = VBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCV(NLAT,NLON,ABC)
      RETURN
      END
      SUBROUTINE DWBGINT(NLAT,NLON,THETA,WWBIN,WORK)
      DOUBLE PRECISION WWBIN
      DIMENSION WWBIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION WORK(*),THETA(*)

      IMID = (NLAT+1)/2
      IW1 = 2*NLAT*IMID + 1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      CALL DWBGIT1(NLAT,NLON,IMID,THETA,WWBIN,WWBIN(IW1),WORK,
     +            WORK(NLAT/2+2))
      RETURN
      END
      SUBROUTINE DWBGIT1(NLAT,NLON,IMID,THETA,WB,ABC,CWB,WORK)
      DOUBLE PRECISION WB
      DOUBLE PRECISION ABC
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      DIMENSION WB(IMID,NLAT,2),ABC(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CWB(1),THETA(1),WBH,WORK(1)

      MDO = MIN0(3,NLAT, (NLON+1)/2)
      IF (MDO.LT.2) RETURN
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          DO 160 NP1 = MP1,NLAT
              N = NP1 - 1
              CALL DDWBK(M,N,CWB,WORK)
              DO 165 I = 1,IMID
                  CALL DDWBT(M,N,THETA(I),CWB,WBH)
                  WB(I,NP1,M) = WBH
  165         CONTINUE
  160 CONTINUE
      CALL DRABCW(NLAT,NLON,ABC)
      RETURN
      END
