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
c                            August 2003
c
c ... file shpe.f
c
c     this file contains code and documentation for subroutines
c     shpei and shpe.
c
c ... files which must be loaded with shpe.f
c
c     hrfft.f
c
c     subroutine shpei initializes arrays wshp and iwshp for
c     subsequent repeated use by subroutine shpe, which
c     performs the harmonic projection equivalent to a
c     harmonic analysis followed by harmonic synthesis
c     but faster and with less memory. (see description of
c     subroutine shpe below)
c
c     subroutine shpei(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
c    1 liwshp,work,lwork,ierror)
c
c     shpei initializes arrays wshp and iwshp for repeated use
c     by subroutine shpe ....
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c            nlon must beat least 4.
c
c     isym   currently not used.
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpei. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpei. It must be at least
c            4*(nlat+1).
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpei. It must be at least
c            1.25*(nlat+1)**2+7*nlat+8.
c
c     **************************************************************
c
c     output parameters
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpe.
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpe.
c
c     work   a double precision work array that does
c            not have to be saved.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of mtrunc
c            = 5  error in the specification of lwshp
c            = 6  error in the specification of liwshp
c            = 7  error in the specification of lwork
c
      SUBROUTINE DSHPEI(NLAT,NLON,ISYM,MTRUNC,WSHP,LWSHP,IWSHP,LIWSHP,
     +                 WORK,LWORK,IERROR)
      DOUBLE PRECISION WSHP
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION WORK(*)
      DIMENSION WSHP(*),IWSHP(*)
c
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
c      ierror = 3
c      if(isym.lt.0 .or. isym.gt.2) return
      IERROR = 4
      MMAX = MIN(NLAT-1,NLON/2)
      IF (MTRUNC.LT.0 .OR. MTRUNC.GT.MMAX) RETURN
      IERROR = 5
      LW1 = 2* (NLAT+1)**2
      LOG2N = LOG(DBLE(NLON))/LOG(2.0D0)
      IF (LWSHP.LT.LW1+NLON+LOG2N) RETURN
      IERROR = 6
      IF (LIWSHP.LT.4* (NLAT+1)) RETURN
      IERROR = 7
      MLWK = 1.25D0* (NLAT+1)**2 + 7*NLAT + 8
      IF (LWORK.LT.MLWK) RETURN
      IERROR = 0
c
      CALL DHRFFTI(NLON,WSHP(LW1+1))
c
      NTE = (NLAT+1)/2
      NLOC1 = 2*NTE*NTE
      NLOC2 = NLAT + 1
      IW1 = 1
      IW2 = IW1 + NLOC1
      IW3 = IW2 + NLOC1
      IW4 = IW3 + NLOC1
      JW1 = 1
      JW2 = JW1 + NLOC2
      JW3 = JW2 + NLOC2
      JW4 = JW3 + NLOC2
      KW1 = 1
      KW2 = KW1 + NTE
      KW3 = KW2 + NTE
      KW4 = KW3 + NTE
      KW5 = KW4 + NTE + 1
      KW6 = KW5 + NTE
      KW7 = KW6 + NTE
      KW8 = KW7 + NTE
      KW9 = KW8 + NTE
      KW10 = KW9 + NLOC2 + NLOC2
      KW11 = KW10 + NLOC2

      KW12 = KW11 + NLOC1
      KW13 = KW12 + NLOC1
c
      CALL DSHPEI1(NLAT,NLON,ISYM,MTRUNC,NTE,IERROR,WSHP(IW1),WSHP(IW2),
     +            WSHP(IW3),WSHP(IW4),IWSHP(JW1),IWSHP(JW2),IWSHP(JW3),
     +            IWSHP(JW4),WORK(KW1),WORK(KW2),WORK(KW3),WORK(KW4),
     +            WORK(KW5),WORK(KW6),WORK(KW7),WORK(KW8),WORK(KW9),
     +            WORK(KW10),WORK(KW11),WORK(KW12),WORK(KW11),
     +            WORK(KW12),WORK(KW13))
      RETURN
      END
      SUBROUTINE DSHPEI1(NLAT,NLON,ISYM,MTRUNC,IDP,IERROR,PE,PO,ZE,ZO,
     +                  IPSE,JZSE,IPSO,JZSO,CP,WORK,WX,S,E,THET,XX,Z,A,
     +                  B,WE,PED,WO,POD,U)
      DOUBLE PRECISION PE
      DOUBLE PRECISION PO
      DOUBLE PRECISION ZE
      DOUBLE PRECISION ZO
      DOUBLE PRECISION TUSL
      DOUBLE PRECISION TOE
      DOUBLE PRECISION DFN
      DOUBLE PRECISION RAND
C*PT*WARNING* Already double-precision
c
      DOUBLE PRECISION SUM,EPS,PI,DTHET,V,A1,B1,C1
C*PT*WARNING* Constant already double-precision
      PARAMETER (EPS=5.0d-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP(IDP),WORK(IDP),WX(IDP),S(IDP+1),E(IDP),
     +                 THET(IDP),XX(IDP),Z(IDP),U(IDP,IDP),
     +                 WE(IDP,IDP,2),PED(IDP,IDP,2),A(4*IDP),B(2*IDP),
     +                 WO(IDP,IDP,2),POD(IDP,IDP,2)
c
      DIMENSION PE(IDP,IDP,2),PO(IDP,IDP,2),ZE(IDP,IDP,2),ZO(IDP,IDP,2),
     +          IPSE(IDP,2),JZSE(IDP,2),IPSO(IDP,2),JZSO(IDP,2),NSHE(2),
     +          NSHO(2)
c
      NS2 = NLAT/2
      MODN = NLAT - NS2 - NS2
      NTE = (NLAT+1)/2
      NTO = NLAT - NTE
      TUSL = 0.D0
      TOE = 0.D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
c
c     compute grid distribution
c
      PI = 4.d0*DATAN(1.0d0)
      DTHET = PI/ (NLAT-1)
      DO I = 1,NTE
          THET(I) = (I-1)*DTHET
      END DO
c
c     compute weight matrices for even functions
c
      DO 40 MP1 = 1,2
          M = MP1 - 1
          MRANK = NLAT - M - M
          NEM = (MRANK+1)/2
          DO J = 1,NEM
              N = J + J + M - 2
              CALL DDLFKP(M,N,CP)
              DO I = 1,NTE
                  CALL DDLFTP(M,N,THET(I),CP,PED(I,J,MP1))
              END DO
C*PT*WARNING* Constant already double-precision
              IF (M.GT.0) PED(1,J,MP1) = 0.0d0
          END DO
          CALL DDSVDC(PED(M+1,1,MP1),IDP,NEM,NEM,S,E,U,IDP,V,IDP,WORK,
     +               10,INFO)
c
          DO J = 1,NEM
C*PT*WARNING* Constant already double-precision
              S(J) = 1.0d0/ (S(J)*S(J))
          END DO
c
c     compute weight matrix as u  s sup -2 u transpose
c
          DO J = 1,NTE
              DO I = 1,NTE
C*PT*WARNING* Constant already double-precision
                  WE(I,J,MP1) = 0.0d0
              END DO
          END DO
          DO I = 1,NEM
              DO J = 1,NEM
                  SUM = 0.D0
                  DO K = 1,NEM
                      SUM = SUM + S(K)*U(I,K)*U(J,K)
                  END DO
                  WE(I+M,J+M,MP1) = SUM
              END DO
          END DO
   40 CONTINUE
C*PT*WARNING* Constant already double-precision
      WE(1,1,2) = 1.0d0
c
c     compute n**2 basis (even functions)
c
      DO N = 1,NLAT + NLAT - 2
          DFN = N
C*PT*WARNING* Constant already double-precision
          A(N) = DSQRT(DFN* (DFN+1.0d0))
      END DO
      DO N = 1,NLAT - 1
          DFN = N
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          B(N) = DSQRT((DFN+DFN+3.0d0)/ (DFN+DFN-1.0d0))
      END DO
c
      MXTR = MIN(NLAT-1,NLON/2,MTRUNC)
      IP = 2
      DO 200 MP1 = 1,MXTR + 1
          M = MP1 - 1
          IP = 3 - IP
          MS2 = MP1/2
          NRANK = MS2 + MS2
          MRANK = NLAT - NRANK
          NEM = (MRANK+1)/2
c
c     compute associated legendre functions
c
          IF (M.LE.1) THEN
              DO 205 J = 1,NEM
                  N = J + J + M - 2
                  CALL DDLFKP(M,N,CP)
                  DO I = 1,NTE
                      CALL DDLFTP(M,N,THET(I),CP,PED(I,J+MS2,IP))
                  END DO
C*PT*WARNING* Constant already double-precision
  202             IF (M.GT.0) PED(1,J+MS2,IP) = 0.0d0
  205         CONTINUE
c
          ELSE
c
              DO 207 J = 1,NEM
                  N = J + J + M - 2
                  IF (M.GT.1 .AND. N.GT.MXTR) THEN
                      DO I = 1,NTE
                          U(I,J+MS2) = PED(I,J+MS2,IP)
                      END DO
                      GO TO 207
                  END IF
                  A1 = B(N-1)*A(N+M-3)/A(N+M-1)
                  B1 = A(N-M+1)/A(N+M-1)
                  IF (N-M.LE.1) THEN
                      DO I = 1,NTE
                          U(I,J+MS2) = A1*PED(I,J+MS2-1,IP) -
     +                                 B1*PED(I,J+MS2,IP)
                      END DO
                  ELSE
                      C1 = B(N-1)*A(N-M-1)/A(N+M-1)
                      DO I = 1,NTE
                          U(I,J+MS2) = A1*PED(I,J+MS2-1,IP) -
     +                                 B1*PED(I,J+MS2,IP) +
     +                                 C1*U(I,J+MS2-1)
                      END DO
                  END IF
  207         CONTINUE
              DO J = 1,NEM
                  DO I = 1,NTE
                      PED(I,J+MS2,IP) = U(I,J+MS2)
                  END DO
              END DO
          END IF
c
          IF (MS2.LE.0 .OR. MS2.GE.NTE) GO TO 200
          DO I = 1,NTE
              XX(I) = RAND()
          END DO
          IT = 0
  201     DO I = 1,NTE
C*PT*WARNING* Constant already double-precision
              Z(I) = 0.0d0
C*PT*WARNING* Constant already double-precision
              WX(I) = 0.0d0
              DO J = 1,NTE
                  WX(I) = WX(I) + WE(I,J,IP)*XX(J)
              END DO
          END DO
          DO 220 J = 1,NTE
              IF (J.EQ.MS2) GO TO 220
              CALL DGS(NTE,WX,PED(1,J,IP),Z)
  220     CONTINUE
c
          DO I = 1,NTE
              XX(I) = XX(I) - Z(I)
          END DO
          CALL DNORMAL(NTE,XX,IDP,WE(1,1,IP))
          IT = IT + 1
          IF (IT.LE.2) GO TO 201
          DO I = 1,NTE
              PED(I,MS2,IP) = XX(I)
          END DO
  200 CONTINUE
c
c     reorder if mtrunc is less than nlat-1
c         case of even functions
c
      IF (MODN.EQ.0) THEN
          NSHE(1) = (NLAT-MTRUNC-1)/2
          NSHE(2) = (NLAT-MTRUNC-2)/2
      ELSE
          NSHE(1) = (NLAT-MTRUNC)/2
          NSHE(2) = (NLAT-MTRUNC-1)/2
      END IF
c
      DO 210 MP1 = 1,2
          DO J = 1,NTE
              JS = J + NSHE(MP1)
              IF (JS.GT.NTE) JS = JS - NTE
              DO I = 1,NTE
                  U(I,JS) = PED(I,J,MP1)
              END DO
          END DO
          DO J = 1,NTE
              DO I = 1,NTE
                  PED(I,J,MP1) = U(I,J)
              END DO
          END DO
  210 CONTINUE
c
      CALL DTRUNC(0,NTE,IDP,PED(1,1,1),NTE,IPSE(1,1))
      CALL DTRUNC(0,NTE,IDP,PED(1,1,2),NTE,IPSE(1,2))
c
c     compute the analysis matrices
c
      DO 250 IP = 1,2
          DO I = 1,NTE
              LOCK = 0
              DO J = 1,NTE
C*PT*WARNING* Constant already double-precision
                  SUM = 0.0d0
                  DO K = 1,NTE
                      SUM = SUM + PED(K,I,IP)*WE(K,J,IP)
                  END DO
                  PE(I,J,IP) = PED(I,J,IP)
                  ZE(J,I,IP) = SUM
                  IF (DABS(SUM).GT.EPS .AND. LOCK.EQ.0) THEN
                      LOCK = 1
                      JZSE(I,IP) = J
                  END IF
              END DO
          END DO
  250 CONTINUE
c
c     compute weight matrices for odd functions
c
      DO 50 MP1 = 1,2
          M = MP1 - 1
          MRANK = NLAT - M - M
          NEM = (MRANK+1)/2
          NOM = MRANK - NEM
          DO J = 1,NOM
              N = J + J + M - 1
              CALL DDLFKP(M,N,CP)
              DO I = 1,NTE
                  CALL DDLFTP(M,N,THET(I),CP,POD(I,J,MP1))
              END DO
C*PT*WARNING* Constant already double-precision
              IF (MODN.EQ.1) POD(NTE,J,MP1) = 0.0d0
          END DO
          CALL DDSVDC(POD(M+1,1,MP1),IDP,NOM,NOM,S,E,U,IDP,V,IDP,WORK,
     +                10,INFO)
c
          DO J = 1,NOM
C*PT*WARNING* Constant already double-precision
              S(J) = 1.0d0/ (S(J)*S(J))
          END DO
c
c     compute weight matrix as u  s sup -2 u transpose
c
          DO J = 1,NTE
              DO I = 1,NTE
C*PT*WARNING* Constant already double-precision
                  WO(I,J,MP1) = 0.0d0
              END DO
          END DO
          DO I = 1,NOM
              DO J = 1,NOM
                  SUM = 0.D0
                  DO K = 1,NOM
                      SUM = SUM + S(K)*U(I,K)*U(J,K)
                  END DO
                  WO(I+M,J+M,MP1) = SUM
              END DO
          END DO
   50 CONTINUE
C*PT*WARNING* Constant already double-precision
      WO(1,1,2) = 1.0d0
      IF (MODN.EQ.1) THEN
C*PT*WARNING* Constant already double-precision
          WO(NTE,NTE,1) = 1.0d0
C*PT*WARNING* Constant already double-precision
          WO(NTE,NTE,2) = 1.0d0
      END IF
c
c     compute n**2 basis (odd functions)
c
      IP = 2
      DO 300 MP1 = 1,MXTR + 1
          IP = 3 - IP
          M = MP1 - 1
          MS2 = MP1/2
          NRANK = MS2 + MS2
          MRANK = NLAT - NRANK
          NEM = (MRANK+1)/2
          NOM = MRANK - NEM
c
c     compute associated legendre functions
c
          IF (M.LE.1) THEN
              DO 305 J = 1,NOM
                  N = J + J + M - 1
                  CALL DDLFKP(M,N,CP)
                  DO I = 1,NTE
                      CALL DDLFTP(M,N,THET(I),CP,POD(I,J+MS2,IP))
                  END DO
C*PT*WARNING* Constant already double-precision
  302             IF (MODN.EQ.1) POD(NTE,J+MS2,IP) = 0.0d0
C*PT*WARNING* Constant already double-precision
                  IF (M.GT.0) POD(1,J+MS2,IP) = 0.0d0
  305         CONTINUE
c
          ELSE
c
              DO 307 J = 1,NOM
                  N = J + J + M - 1
                  IF (M.GT.1 .AND. N.GT.MXTR) THEN
                      DO I = 1,NTE
                          U(I,J+MS2) = POD(I,J+MS2,IP)
                      END DO
                      GO TO 304
                  END IF
                  A1 = B(N-1)*A(N+M-3)/A(N+M-1)
                  B1 = A(N-M+1)/A(N+M-1)
                  IF (N-M.LE.1) THEN
                      DO I = 1,NTE
                          U(I,J+MS2) = A1*POD(I,J+MS2-1,IP) -
     +                                 B1*POD(I,J+MS2,IP)
                      END DO
                  ELSE
                      C1 = B(N-1)*A(N-M-1)/A(N+M-1)
                      DO I = 1,NTE
                          U(I,J+MS2) = A1*POD(I,J+MS2-1,IP) -
     +                                 B1*POD(I,J+MS2,IP) +
     +                                 C1*U(I,J+MS2-1)
                      END DO
                  END IF
C*PT*WARNING* Constant already double-precision
  304             IF (MODN.EQ.1) U(NTE,J+MS2) = 0.0d0
  307         CONTINUE
              DO J = 1,NOM
                  DO I = 1,NTE
                      POD(I,J+MS2,IP) = U(I,J+MS2)
                  END DO
              END DO
          END IF
c
          IF (MS2.LE.0 .OR. MS2.GE.NTO) GO TO 300
          DO I = 1,NTE
              XX(I) = RAND()
          END DO
C*PT*WARNING* Constant already double-precision
          IF (MODN.EQ.1) XX(NTE) = 0.0d0
          IT = 0
  306     DO I = 1,NTE
              Z(I) = 0.D0
              WX(I) = 0.D0
              DO J = 1,NTO
                  WX(I) = WX(I) + WO(I,J,IP)*XX(J)
              END DO
          END DO
          DO 330 J = 1,NTO
              IF (J.EQ.MS2) GO TO 330
              CALL DGS(NTE,WX,POD(1,J,IP),Z(1))
  330     CONTINUE
c
          DO I = 1,NTE
              XX(I) = XX(I) - Z(I)
          END DO
          CALL DNORMAL(NTE,XX,IDP,WO(1,1,IP))
          IT = IT + 1
          IF (IT.LE.2) GO TO 306
          DO I = 1,NTE
              POD(I,MS2,IP) = XX(I)
          END DO
C*PT*WARNING* Constant already double-precision
          IF (MODN.EQ.1) POD(NTE,MS2,IP) = 0.0d0
  300 CONTINUE
c
c     reorder if mtrunc is less than nlat-1
c        case of odd functions
c
      IF (MODN.EQ.0) THEN
          NSHO(1) = (NLAT-MTRUNC)/2
          NSHO(2) = (NLAT-MTRUNC-1)/2
      ELSE
          NSHO(1) = (NLAT-MTRUNC-1)/2
          NSHO(2) = (NLAT-MTRUNC-2)/2
      END IF
c
      DO 310 MP1 = 1,2
          DO J = 1,NTO
              JS = J + NSHO(MP1)
              IF (JS.GT.NTO) JS = JS - NTO
              DO I = 1,NTE
                  U(I,JS) = POD(I,J,MP1)
              END DO
          END DO
          DO J = 1,NTO
              DO I = 1,NTE
                  POD(I,J,MP1) = U(I,J)
              END DO
          END DO
  310 CONTINUE
c
      CALL DTRUNC(0,NTE,IDP,POD(1,1,1),NTO,IPSO(1,1))
      CALL DTRUNC(0,NTE,IDP,POD(1,1,2),NTO,IPSO(1,2))
c
c     compute the analysis matrices (odd functions)
c
      DO IP = 1,2
          DO I = 1,NTO
              LOCK = 0
              DO J = 1,NTO
C*PT*WARNING* Constant already double-precision
                  SUM = 0.0d0
                  DO K = 1,NTE
                      SUM = SUM + POD(K,I,IP)*WO(K,J,IP)
                  END DO
                  PO(I,J,IP) = POD(I,J,IP)
                  ZO(J,I,IP) = SUM
                  IF (DABS(SUM).GT.EPS .AND. LOCK.EQ.0) THEN
                      LOCK = 1
                      JZSO(I,IP) = J
                  END IF
              END DO
          END DO
      END DO
      RETURN
      END
c
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
c  .                         SPHEREPACK3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c ... file shpe.f
c
c ... files which must be loaded with shpe.f
c
c     sphcom.f, hrfft.f
c
c     the n**2 projection with complement, odd/even
c     factorization and zero truncation on an
c     equally spaced grid as defined in the JCP paper
c     "Generalized discrete spherical harmonic transforms"
c     by Paul N. Swarztrauber and William F. Spotz
c     It is equivalent to a harmonic analysis followed
c     by a synthesis except faster and requires less memory.
c
c     subroutine shpe(nlat,nlon,isym,mtrunc,x,y,idxy,
c    1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
c     shpe projects the array x onto the set of functions represented
c     by a discrete set of spherical harmonics.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c            nlon must beat least 4.
c
c     isym   currently not used.
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.

c            zero.
c
c     x      a two dimensional array that contains the the nlat
c            by nlon array x(i,j) defined at the colatitude point
c            theta(i) = (i-1)*pi/(nlat-1) and longitude point phi(j) =
c            (j-1)*2*pi/nlon.
c
c     idxy   the first dimension of the arrays x and y as they
c            appear in the program that calls shpe. It must be
c            at least nlat.
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpe.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpei. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpe.
c
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpei. It must be at least
c            4*(nlat+1).
c
c     work   a single precision work array that does
c            not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpe. It must be at least
c            max(nlat*nlon,4*(nlat+1)).
c
c     **************************************************************
c
c     output parameters
c
c     y      an nlat by nlon single precision array that contains
c            the projection of x onto the set of functions that
c            can be represented by the discrete set of spherical
c            harmonics. The arrays x(i,j) and y(i,j) are located
c            at colatitude point theta(i) = (i-1)*pi/(nlat-1) and
c            longitude point phi(j) = (j-1)*2*pi/nlon.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of mtrunc
c            = 5  error in the specification of lwshp
c            = 6  error in the specification of liwshp
c            = 7  error in the specification of lwork
c
      SUBROUTINE DSHPE(NLAT,NLON,ISYM,MTRUNC,X,Y,IDXY,WSHP,LWSHP,IWSHP,
     +                LIWSHP,WORK,LWORK,IERROR)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION WSHP
      DOUBLE PRECISION WORK
      DOUBLE PRECISION SN
c
      DIMENSION WSHP(*),IWSHP(*),WORK(*),X(IDXY,NLON),Y(IDXY,NLON)
c
      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
c      ierror = 3
c      if(isym.lt.0 .or. isym.gt.2) return
      IERROR = 4
      MMAX = MIN(NLAT-1,NLON/2)
      IF (MTRUNC.LT.0 .OR. MTRUNC.GT.MMAX) RETURN
      IERROR = 5
      LOG2N = LOG(DBLE(NLON))/LOG(2.0D0)
      LW1 = 2* (NLAT+1)**2
      IF (LWSHP.LT.LW1+NLON+LOG2N) RETURN
      IERROR = 6
      IF (LIWSHP.LT.4* (NLAT+1)) RETURN
      IERROR = 7
      MWRK = MAX(NLAT*NLON,4* (NLAT+1))
      IF (LWORK.LT.MWRK) RETURN
      IERROR = 0
c
      DO J = 1,NLON
          DO I = 1,NLAT
              Y(I,J) = X(I,J)
          END DO
      END DO
      CALL DHRFFTF(NLAT,NLON,Y,IDXY,WSHP(LW1+1),WORK)
c
      NTE = (NLAT+1)/2
      NLOC1 = 2*NTE*NTE
      NLOC2 = NLAT + 1
      IW1 = 1
      IW2 = IW1 + NLOC1
      IW3 = IW2 + NLOC1
      IW4 = IW3 + NLOC1
      JW1 = 1
      JW2 = JW1 + NLOC2
      JW3 = JW2 + NLOC2
      JW4 = JW3 + NLOC2
c
      CALL DSHPE1(NLAT,NLON,ISYM,MTRUNC,Y,Y,IDXY,IERROR,NTE,WSHP(IW1),
     +           WSHP(IW2),WSHP(IW3),WSHP(IW4),IWSHP(JW1),IWSHP(JW2),
     +           IWSHP(JW3),IWSHP(JW4),WORK(JW1),WORK(JW2),WORK(JW3),
     +           WORK(JW4))
c
      CALL DHRFFTB(NLAT,NLON,Y,IDXY,WSHP(LW1+1),WORK)
c
      SN = 1.0D0/NLON
      DO J = 1,NLON
          DO I = 1,NLAT
              Y(I,J) = SN*Y(I,J)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DSHPE1(NLAT,NLON,ISYM,MTRUNC,SX,SY,IDXY,IERROR,IDP,PE,
     +                 PO,ZE,ZO,IPSE,JZSE,IPSO,JZSO,XE,XO,YE,YO)
      DOUBLE PRECISION SX
      DOUBLE PRECISION SY
      DOUBLE PRECISION PE
      DOUBLE PRECISION PO
      DOUBLE PRECISION ZE
      DOUBLE PRECISION ZO
      DOUBLE PRECISION XE
      DOUBLE PRECISION XO
      DOUBLE PRECISION YE
      DOUBLE PRECISION YO
c
      DIMENSION SX(IDXY,NLON),SY(IDXY,NLON),NSHE(2),NSHO(2),
     +          PE(IDP,IDP,2),PO(IDP,IDP,2),ZE(IDP,IDP,2),ZO(IDP,IDP,2),
     +          IPSE(IDP,2),JZSE(IDP,2),IPSO(IDP,2),JZSO(IDP,2),
     +          XE(IDP,2),XO(IDP,2),YE(IDP,2),YO(IDP,2)
c
      NS2 = NLAT/2
      MODN = NLAT - NS2 - NS2
      NTE = (NLAT+1)/2
      NTO = NLAT - NTE
c
      IF (MODN.EQ.0) THEN
          NSHE(1) = (NLAT-MTRUNC-1)/2
          NSHE(2) = (NLAT-MTRUNC-2)/2
          NSHO(1) = (NLAT-MTRUNC)/2
          NSHO(2) = (NLAT-MTRUNC-1)/2
      ELSE
          NSHE(1) = (NLAT-MTRUNC)/2
          NSHE(2) = (NLAT-MTRUNC-1)/2
          NSHO(1) = (NLAT-MTRUNC-1)/2
          NSHO(2) = (NLAT-MTRUNC-2)/2
      END IF
      MXTR = MIN(NLAT-1,NLON/2,MTRUNC)
      IP = 2
      DO 100 MP1 = 1,MXTR + 1
          IP = 3 - IP
          IF (MXTR.EQ.NLAT-1 .AND. MP1.LE.2) THEN
              DO I = 1,NLAT
                  SY(I,MP1) = SX(I,MP1)
              END DO
              IF (MP1.EQ.2) THEN
                  SY(1,2) = 0.D0
                  SY(NLAT,2) = 0.D0
              END IF
              IF (NLON.GE.3) THEN
                  SY(1,3) = 0.D0
                  SY(NLAT,3) = 0.D0
                  DO I = 2,NLAT - 1
                      SY(I,3) = SX(I,3)
                  END DO
              END IF
              GO TO 100
          END IF
          M = MP1 - 1
          MPM = MAX(1,M+M)
          MS2 = MP1/2
          MRANK = MIN(NLAT-M,NLAT-MS2-MS2)
c      mrank = mxtr+1-ms2-ms2
          NRANK = NLAT - MRANK
          NEM = (MRANK+1)/2 - NSHE(IP)
          NOM = MRANK - (MRANK+1)/2 - NSHO(IP)
          NEC = NTE - NEM
          NOC = NTO - NOM
c
          DO I = 1,NTE
              XE(I,1) = .5D0* (SX(I,MPM)+SX(NLAT+1-I,MPM))
              XO(I,1) = .5D0* (SX(I,MPM)-SX(NLAT+1-I,MPM))
          END DO
          IF (MPM.LT.NLON) THEN
              DO I = 1,NTE
                  XE(I,2) = .5D0* (SX(I,MPM+1)+SX(NLAT+1-I,MPM+1))
                  XO(I,2) = .5D0* (SX(I,MPM+1)-SX(NLAT+1-I,MPM+1))
              END DO
          END IF
          IF (3*NEC.LT.2*NEM .OR. NEM.EQ.0) THEN
              CALL DTMXMX(NTE,NEC,IDP,PE(1,1,IP),NTE,IDP,ZE(1,1,IP),XE,
     +                   YE,IPSE(1,IP),JZSE(1,IP))
              DO I = 1,NTE
                  YE(I,1) = XE(I,1) - YE(I,1)
              END DO
              IF (MPM.LT.NLON .AND. M.NE.0) THEN
                  DO I = 1,NTE
                      YE(I,2) = XE(I,2) - YE(I,2)
                  END DO
              END IF
          ELSE
              CALL DTMXMX(NTE,NEM,IDP,PE(1,NEC+1,IP),NTE,IDP,
     +                   ZE(1,NEC+1,IP),XE,YE,IPSE(NEC+1,IP),
     +                   JZSE(NEC+1,IP))
          END IF
          IF (3*NOC.LT.2*NOM .OR. NOM.EQ.0) THEN
              CALL DTMXMX(NTO,NOC,IDP,PO(1,1,IP),NTO,IDP,ZO(1,1,IP),XO,
     +                   YO,IPSO(1,IP),JZSO(1,IP))
              DO I = 1,NTE
                  YO(I,1) = XO(I,1) - YO(I,1)
              END DO
              IF (MPM.LT.NLON .AND. M.NE.0) THEN
                  DO I = 1,NTE
                      YO(I,2) = XO(I,2) - YO(I,2)
                  END DO
              END IF
          ELSE
              CALL DTMXMX(NTO,NOM,IDP,PO(1,NOC+1,IP),NTO,IDP,
     +                   ZO(1,NOC+1,IP),XO,YO,IPSO(NOC+1,IP),
     +                   JZSO(NOC+1,IP))
          END IF
          DO I = 1,NTE
              SY(I,MPM) = YE(I,1) + YO(I,1)
              SY(NLAT+1-I,MPM) = YE(I,1) - YO(I,1)
          END DO
          IF (MPM.LT.NLON .AND. M.NE.0) THEN
              DO I = 1,NTE
                  SY(I,MPM+1) = YE(I,2) + YO(I,2)
                  SY(NLAT+1-I,MPM+1) = YE(I,2) - YO(I,2)
              END DO
          END IF
  100 CONTINUE
      JS = MXTR + MXTR + 2
      DO J = JS,NLON
          DO I = 1,NLAT
              SY(I,J) = 0.D0
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DMXM(LR,LC,LD,A,MC,MD,B,ND,C)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION A(LD,*),B(MD,*),C(ND,*)

      DO I = 1,LR
          DO J = 1,MC
              C(I,J) = 0.D0
              DO K = 1,LC
                  C(I,J) = C(I,J) + A(I,K)*B(K,J)
              END DO
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DSMXM(LR,LC,LD,A,MC,MD,B,ND,C)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DIMENSION A(LD,*),B(MD,*),C(ND,*)

      DO I = 1,LR
          DO J = 1,MC
              C(I,J) = 0.D0
              DO K = 1,LC
                  C(I,J) = C(I,J) + A(I,K)*B(K,J)
              END DO
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DMXMX(LR,LC,LD,A,MC,MD,B,X,Y)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION SUM1
      DOUBLE PRECISION SUM2
      DIMENSION A(LD,*),B(MD,*),X(LD,2),Y(LD,2)

      DO K = 1,LR
          Y(K,1) = 0.D0
          Y(K,2) = 0.D0
      END DO
c
      IF (LC.LE.0) RETURN
      DO I = 1,LC
          SUM1 = 0.D0
          SUM2 = 0.D0
          DO J = 1,MC
              SUM1 = SUM1 + B(I,J)*X(J,1)
              SUM2 = SUM2 + B(I,J)*X(J,2)
          END DO
          DO K = 1,LR
              Y(K,1) = Y(K,1) + SUM1*A(K,I)
              Y(K,2) = Y(K,2) + SUM2*A(K,I)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DDMXMX(LR,LC,LD,A,MC,MD,B,X,Y)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION A(LD,*),B(MD,*),X(LD,2),Y(LD,2),SUM1,SUM2

      DO K = 1,LR
          Y(K,1) = 0.D0
          Y(K,2) = 0.D0
      END DO
c
      IF (LC.LE.0) RETURN
      DO I = 1,LC
          SUM1 = 0.D0
          SUM2 = 0.D0
          DO J = 1,MC
              SUM1 = SUM1 + B(I,J)*X(J,1)
              SUM2 = SUM2 + B(I,J)*X(J,2)
          END DO
          DO K = 1,LR
              Y(K,1) = Y(K,1) + SUM1*A(K,I)
              Y(K,2) = Y(K,2) + SUM2*A(K,I)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DTMXMX(LR,LC,LD,A,MC,MD,B,X,Y,IS,JS)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION SUM1
      DOUBLE PRECISION SUM2
      DIMENSION A(LD,*),B(MD,*),X(LD,2),Y(LD,2),IS(*),JS(*)
c
      KMX = MIN(LR+1,LD)
      DO K = 1,KMX
          Y(K,1) = 0.D0
          Y(K,2) = 0.D0
      END DO
      IF (LC.LE.0) RETURN
c
      DO I = 1,LC
          SUM1 = 0.D0
          SUM2 = 0.D0
          DO J = JS(I),MC
              SUM1 = SUM1 + B(J,I)*X(J,1)
              SUM2 = SUM2 + B(J,I)*X(J,2)
          END DO
          DO K = IS(I),LR
              Y(K,1) = Y(K,1) + SUM1*A(K,I)
              Y(K,2) = Y(K,2) + SUM2*A(K,I)
          END DO
      END DO
      RETURN
      END
      SUBROUTINE DTRUNC(IRC,N,IDP,A,NRC,IJS)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION A,EPS
C*PT*WARNING* Constant already double-precision
      PARAMETER (EPS=5.d-8)
      DIMENSION A(IDP,*),IJS(N)
c
c     irc = 0 for columns , or irc = 1 for rows
c
      IF (IRC.NE.0) GO TO 30
      DO 20 J = 1,NRC
          DO I = 1,N
              IJS(J) = I
              IF (DABS(A(I,J)).GT.EPS) GO TO 20
          END DO
   20 CONTINUE
      RETURN
   30 DO 50 I = 1,NRC
          DO J = 1,N
              IJS(I) = J
              IF (ABS(A(I,J)).GT.EPS) GO TO 50
          END DO
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DGS(N,X,Y,Z)
      DIMENSION X(N),Y(N),Z(N)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X,Y,Z,SUM
c
c     accumulate innerproducts of x with respect to y.
c
      SUM = 0.D0
      DO I = 1,N
          SUM = SUM + X(I)*Y(I)
      END DO
      DO I = 1,N
          Z(I) = Z(I) + SUM*Y(I)
      END DO
      RETURN
      END
      SUBROUTINE DNORMAL(N,X,ID,Q)
      DIMENSION X(N),Q(ID,N)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X,Q,SUM,SQS
c
c     normalize x
c
      SQS = 0.D0
      DO I = 1,N
          SUM = 0.D0
          DO J = 1,N
              SUM = SUM + Q(I,J)*X(J)
          END DO
          SQS = SQS + SUM*X(I)
      END DO
c
      SQS = DSQRT(SQS)
      DO I = 1,N
          X(I) = X(I)/SQS
      END DO
      RETURN
      END
      SUBROUTINE DCOE(MOE,N,X,DMAX)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X(N),DMAX

      NH = (N+1)/2
      DMAX = 0.D0
      IF (MOE.NE.0) GO TO 1
      DO I = 1,NH
          DMAX = MAX(DMAX,DABS(X(I)-X(N-I+1)))
          X(I) = .5D0* (X(I)+X(N-I+1))
          X(N-I+1) = X(I)
      END DO
      RETURN
    1 DO I = 1,NH
          DMAX = MAX(DMAX,DABS(X(I)+X(N-I+1)))
          X(I) = .5D0* (X(I)-X(N-I+1))
          X(N-I+1) = -X(I)
      END DO
      IF (MOD(N,2).NE.0) X(NH) = 0.D0
      RETURN
      END
c     subroutine dlfkp(m,n,cp)
c
c     subroutine dlfkp computes the coefficients in the trigonometric
c     expansion of the normalized associated legendre functions:
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                        *sin(theta)**m/(2**n*factorial(n)) times the
c                        (n+m)th derivative of (x**2-1)**n with respect
c                        to x=cos(theta)
c
c     where theta is colatitude.
c
c     subroutine dlfkp computes the coefficients cp(k) in the
c     following trigonometric expansion of pbar(m,n,theta).
c
c            1) for n even and m even, pbar(m,n,theta) =
c               .5*cp(1) plus the sum from k=1 to k=n/2
c               of cp(k)*cos(2*k*th)
c
c            2) for n even and m odd, pbar(m,n,theta) =
c               the sum from k=1 to k=n/2 of
c               cp(k)*sin(2*k*th)
c
c            3) for n odd and m even, pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*cos((2*k-1)*th)
c
c            4) for n odd and m odd,  pbar(m,n,theta) =
c               the sum from k=1 to k=(n+1)/2 of
c               cp(k)*sin((2*k-1)*th)
c
c     input parameters
c
c     m      is the order of pbar(n,m,theta). m can be any integer
c            however pbar(n,m,theta) = 0  if abs(m) is greater than
c            n and pbar(n,m,theta) = (-1)**m*pbar(n,-m,theta) for
c            negative m.
c
c     n      nonnegative integer specifying the degree of
c            pbar(n,m,theta)
c
c     output parameters
c
c     cp     a double precision array that contains the fourier
c            coefficients for pbar(m,n,theta). the length of the
c            array depends on the parity of m and n
c
c                  parity            length of cp
c
c               n even m even           n/2+1
c               n even m odd             n/2
c               n odd  m even          (n+1)/2
c               n odd  m odd           (n+1)/2
c
c
c ****************************************************************
      SUBROUTINE DDLFKP(M,N,CP)
C*PT*WARNING* Already double-precision
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
      SUBROUTINE DDLFTP(M,N,THETA,CP,PB)
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
c
      SUBROUTINE DDSVDC(X,LDX,N,P,S,E,U,LDU,V,LDV,WORK,JOB,INFO)
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X(LDX,1),S(1),E(1),U(LDU,1),V(LDV,1),WORK(1)
c
c
c     dsvdc is a subroutine to reduce a double precision nxp matrix x
c     by orthogonal transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         double precision(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by dsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u.
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v.
c                   (see below).
c
c         work      double precision(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    return the first min(n,p) singular
c                                  vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         double precision(mm), where mm=min(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         double precision(p),
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         double precision(ldu,k), where ldu.ge.n.  if
c                                   joba.eq.1 then k.eq.n, if joba.ge.2
c                                   then k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.eq.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         double precision(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if job.eq.0.  if p.le.n,
c                   then v may be identified with x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = trans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (trans(u)
c                   is the transpose of u).  thus the singular
c                   values of x and b are the same.
c
c     linpack. this version dated 08/14/78 .
c              correction made to shift 2/84.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dsvdc uses the following functions and subprograms.
c
c     external drot
c     blas daxpy,dddot,dscal,dswap,ddnrm2,drotg
c     fortran dabs,dmax1,max0,min0,mod,dsqrt
c
c     internal variables
c
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,MM,
     +        MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
C*PT*WARNING* Already double-precision
c      double precision dddot,t,r
      DOUBLE PRECISION DDDOT,T
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION B,C,CS,EL,EMM1,F,G,DDNRM2,SCALE,SHIFT,SL,SM,SN,
     +                 SMM1,T1,TEST,ZTEST
      LOGICAL WANTU,WANTV
c
c
c     set the maximum number of iterations.
c
      MAXIT = 30
c
c     determine what is to be computed.
c
      WANTU = .false.
      WANTV = .false.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU.GT.1) NCU = MIN0(N,P)
      IF (JOBU.NE.0) WANTU = .true.
      IF (MOD(JOB,10).NE.0) WANTV = .true.
c
c     reduce x to bidiagonal form, storing the diagonal elements
c     in s and the super-diagonal elements in e.
c
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU.LT.1) GO TO 170
      DO 160 L = 1,LU
          LP1 = L + 1
          IF (L.GT.NCT) GO TO 20
c
c           compute the transformation for the l-th column and
c           place the l-th diagonal in s(l).
c
          S(L) = DDNRM2(N-L+1,X(L,L),1)
C*PT*WARNING* Constant already double-precision
          IF (S(L).EQ.0.0d0) GO TO 10
C*PT*WARNING* Constant already double-precision
          IF (X(L,L).NE.0.0d0) S(L) = DSIGN(S(L),X(L,L))
C*PT*WARNING* Constant already double-precision
          CALL DDSCAL(N-L+1,1.0d0/S(L),X(L,L),1)
C*PT*WARNING* Constant already double-precision
          X(L,L) = 1.0d0 + X(L,L)
   10     CONTINUE
          S(L) = -S(L)
   20     CONTINUE
          IF (P.LT.LP1) GO TO 50
          DO 40 J = LP1,P
              IF (L.GT.NCT) GO TO 30
C*PT*WARNING* Constant already double-precision
              IF (S(L).EQ.0.0d0) GO TO 30
c
c              apply the transformation.
c
              T = -DDDOT(N-L+1,X(L,L),1,X(L,J),1)/X(L,L)
              CALL DDAXPY(N-L+1,T,X(L,L),1,X(L,J),1)
   30         CONTINUE
c
c           place the l-th row of x into  e for the
c           subsequent calculation of the row transformation.
c
              E(J) = X(L,J)
   40     CONTINUE
   50     CONTINUE
          IF (.NOT.WANTU .OR. L.GT.NCT) GO TO 70
c
c           place the transformation in u for subsequent back
c           multiplication.
c
          DO 60 I = L,N
              U(I,L) = X(I,L)
   60     CONTINUE
   70     CONTINUE
          IF (L.GT.NRT) GO TO 150
c
c           compute the l-th row transformation and place the
c           l-th super-diagonal in e(l).
c
          E(L) = DDNRM2(P-L,E(LP1),1)
C*PT*WARNING* Constant already double-precision
          IF (E(L).EQ.0.0d0) GO TO 80
C*PT*WARNING* Constant already double-precision
          IF (E(LP1).NE.0.0d0) E(L) = DSIGN(E(L),E(LP1))
C*PT*WARNING* Constant already double-precision
          CALL DDSCAL(P-L,1.0d0/E(L),E(LP1),1)
C*PT*WARNING* Constant already double-precision
          E(LP1) = 1.0d0 + E(LP1)
   80     CONTINUE
          E(L) = -E(L)
C*PT*WARNING* Constant already double-precision
          IF (LP1.GT.N .OR. E(L).EQ.0.0d0) GO TO 120
c
c              apply the transformation.
c
          DO 90 I = LP1,N
C*PT*WARNING* Constant already double-precision
              WORK(I) = 0.0d0
   90     CONTINUE
          DO 100 J = LP1,P
              CALL DDAXPY(N-L,E(J),X(LP1,J),1,WORK(LP1),1)
  100     CONTINUE
          DO 110 J = LP1,P
              CALL DDAXPY(N-L,-E(J)/E(LP1),WORK(LP1),1,X(LP1,J),1)
  110     CONTINUE
  120     CONTINUE
          IF (.NOT.WANTV) GO TO 140
c
c              place the transformation in v for subsequent
c              back multiplication.
c
          DO 130 I = LP1,P
              V(I,L) = E(I)
  130     CONTINUE
  140     CONTINUE
  150     CONTINUE
  160 CONTINUE
  170 CONTINUE
c
c     set up the final bidiagonal matrix or order m.
c
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT.LT.P) S(NCTP1) = X(NCTP1,NCTP1)
C*PT*WARNING* Constant already double-precision
      IF (N.LT.M) S(M) = 0.0d0
      IF (NRTP1.LT.M) E(NRTP1) = X(NRTP1,M)
C*PT*WARNING* Constant already double-precision
      E(M) = 0.0d0
c
c     if required, generate u.
c
      IF (.NOT.WANTU) GO TO 300
      IF (NCU.LT.NCTP1) GO TO 200
      DO 190 J = NCTP1,NCU
          DO 180 I = 1,N
C*PT*WARNING* Constant already double-precision
              U(I,J) = 0.0d0
  180     CONTINUE
C*PT*WARNING* Constant already double-precision
          U(J,J) = 1.0d0
  190 CONTINUE
  200 CONTINUE
      IF (NCT.LT.1) GO TO 290
      DO 280 LL = 1,NCT
          L = NCT - LL + 1
C*PT*WARNING* Constant already double-precision
          IF (S(L).EQ.0.0d0) GO TO 250
          LP1 = L + 1
          IF (NCU.LT.LP1) GO TO 220
          DO 210 J = LP1,NCU
              T = -DDDOT(N-L+1,U(L,L),1,U(L,J),1)/U(L,L)
              CALL DDAXPY(N-L+1,T,U(L,L),1,U(L,J),1)
  210     CONTINUE
  220     CONTINUE
C*PT*WARNING* Constant already double-precision
          CALL DDSCAL(N-L+1,-1.0d0,U(L,L),1)
C*PT*WARNING* Constant already double-precision
          U(L,L) = 1.0d0 + U(L,L)
          LM1 = L - 1
          IF (LM1.LT.1) GO TO 240
          DO 230 I = 1,LM1
C*PT*WARNING* Constant already double-precision
              U(I,L) = 0.0d0
  230     CONTINUE
  240     CONTINUE
          GO TO 270
  250     CONTINUE
          DO 260 I = 1,N
C*PT*WARNING* Constant already double-precision
              U(I,L) = 0.0d0
  260     CONTINUE
C*PT*WARNING* Constant already double-precision
          U(L,L) = 1.0d0
  270     CONTINUE
  280 CONTINUE
  290 CONTINUE
  300 CONTINUE
c
c     if it is required, generate v.
c
      IF (.NOT.WANTV) GO TO 350
      DO 340 LL = 1,P
          L = P - LL + 1
          LP1 = L + 1
          IF (L.GT.NRT) GO TO 320
C*PT*WARNING* Constant already double-precision
          IF (E(L).EQ.0.0d0) GO TO 320
          DO 310 J = LP1,P
              T = -DDDOT(P-L,V(LP1,L),1,V(LP1,J),1)/V(LP1,L)
              CALL DDAXPY(P-L,T,V(LP1,L),1,V(LP1,J),1)
  310     CONTINUE
  320     CONTINUE
          DO 330 I = 1,P
C*PT*WARNING* Constant already double-precision
              V(I,L) = 0.0d0
  330     CONTINUE
C*PT*WARNING* Constant already double-precision
          V(L,L) = 1.0d0
  340 CONTINUE
  350 CONTINUE
c
c     main iteration loop for the singular values.
c
      MM = M
      ITER = 0
  360 CONTINUE
c
c        quit if all the singular values have been found.
c
c     ...exit
      IF (M.EQ.0) GO TO 620
c
c        if too many iterations have been performed, set
c        flag and return.
c
      IF (ITER.LT.MAXIT) GO TO 370
      INFO = M
c     ......exit
      GO TO 620
  370 CONTINUE
c
c        this section of the program inspects for
c        negligible elements in the s and e arrays.  on
c        completion the variables kase and l are set as follows.
c
c           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m
c           kase = 2     if s(l) is negligible and l.lt.m
c           kase = 3     if e(l-1) is negligible, l.lt.m, and
c                        s(l), ..., s(m) are not negligible (qr step).
c           kase = 4     if e(m-1) is negligible (convergence).
c
      DO 390 LL = 1,M
          L = M - LL
c        ...exit
          IF (L.EQ.0) GO TO 400
          TEST = DABS(S(L)) + DABS(S(L+1))
          ZTEST = TEST + DABS(E(L))
          IF (ZTEST.NE.TEST) GO TO 380
C*PT*WARNING* Constant already double-precision
          E(L) = 0.0d0
c        ......exit
          GO TO 400
  380     CONTINUE
  390 CONTINUE
  400 CONTINUE
      IF (L.NE.M-1) GO TO 410
      KASE = 4
      GO TO 480
  410 CONTINUE
      LP1 = L + 1
      MP1 = M + 1
      DO 430 LLS = LP1,MP1
          LS = M - LLS + LP1
c           ...exit
          IF (LS.EQ.L) GO TO 440
C*PT*WARNING* Constant already double-precision
          TEST = 0.0d0
          IF (LS.NE.M) TEST = TEST + DABS(E(LS))
          IF (LS.NE.L+1) TEST = TEST + DABS(E(LS-1))
          ZTEST = TEST + DABS(S(LS))
          IF (ZTEST.NE.TEST) GO TO 420
C*PT*WARNING* Constant already double-precision
          S(LS) = 0.0d0
c           ......exit
          GO TO 440
  420     CONTINUE
  430 CONTINUE
  440 CONTINUE
      IF (LS.NE.L) GO TO 450
      KASE = 3
      GO TO 470
  450 CONTINUE
      IF (LS.NE.M) GO TO 460
      KASE = 1
      GO TO 470
  460 CONTINUE
      KASE = 2
      L = LS
  470 CONTINUE
  480 CONTINUE
      L = L + 1
c
c        perform the task indicated by kase.
c
      GO TO (490,520,540,570) KASE
c
c        deflate negligible s(m).
c
  490 CONTINUE
      MM1 = M - 1
      F = E(M-1)
C*PT*WARNING* Constant already double-precision
      E(M-1) = 0.0d0
      DO 510 KK = L,MM1
          K = MM1 - KK + L
          T1 = S(K)
          CALL DDROTG(T1,F,CS,SN)
          S(K) = T1
          IF (K.EQ.L) GO TO 500
          F = -SN*E(K-1)
          E(K-1) = CS*E(K-1)
  500     CONTINUE
          IF (WANTV) CALL DDROT(P,V(1,K),1,V(1,M),1,CS,SN)
  510 CONTINUE
      GO TO 610
c
c        split at negligible s(l).
c
  520 CONTINUE
      F = E(L-1)
C*PT*WARNING* Constant already double-precision
      E(L-1) = 0.0d0
      DO 530 K = L,M
          T1 = S(K)
          CALL DDROTG(T1,F,CS,SN)
          S(K) = T1
          F = -SN*E(K)
          E(K) = CS*E(K)
          IF (WANTU) CALL DDROT(N,U(1,K),1,U(1,L-1),1,CS,SN)
  530 CONTINUE
      GO TO 610
c
c        perform one qr step.
c
  540 CONTINUE
c
c           calculate the shift.
c
      SCALE = DMAX1(DABS(S(M)),DABS(S(M-1)),DABS(E(M-1)),DABS(S(L)),
     +        DABS(E(L)))
      SM = S(M)/SCALE
      SMM1 = S(M-1)/SCALE
      EMM1 = E(M-1)/SCALE
      SL = S(L)/SCALE
      EL = E(L)/SCALE
C*PT*WARNING* Constant already double-precision
      B = ((SMM1+SM)* (SMM1-SM)+EMM1**2)/2.0d0
      C = (SM*EMM1)**2
C*PT*WARNING* Constant already double-precision
      SHIFT = 0.0d0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (B.EQ.0.0d0 .AND. C.EQ.0.0d0) GO TO 550
      SHIFT = DSQRT(B**2+C)
C*PT*WARNING* Constant already double-precision
      IF (B.LT.0.0d0) SHIFT = -SHIFT
      SHIFT = C/ (B+SHIFT)
  550 CONTINUE
      F = (SL+SM)* (SL-SM) + SHIFT
      G = SL*EL
c
c           chase zeros.
c
      MM1 = M - 1
      DO 560 K = L,MM1
          CALL DDROTG(F,G,CS,SN)
          IF (K.NE.L) E(K-1) = F
          F = CS*S(K) + SN*E(K)
          E(K) = CS*E(K) - SN*S(K)
          G = SN*S(K+1)
          S(K+1) = CS*S(K+1)
          IF (WANTV) CALL DDROT(P,V(1,K),1,V(1,K+1),1,CS,SN)
          CALL DDROTG(F,G,CS,SN)
          S(K) = F
          F = CS*E(K) + SN*S(K+1)
          S(K+1) = -SN*E(K) + CS*S(K+1)
          G = SN*E(K+1)
          E(K+1) = CS*E(K+1)
          IF (WANTU .AND. K.LT.N) CALL DDROT(N,U(1,K),1,U(1,K+1),1,CS,
     +                                       SN)
  560 CONTINUE
      E(M-1) = F
      ITER = ITER + 1
      GO TO 610
c
c        convergence.
c
  570 CONTINUE
C*PT*WARNING* Constant already double-precision
c
c           make the singular value  positive.
c
      IF (S(L).GE.0.0d0) GO TO 580
      S(L) = -S(L)
C*PT*WARNING* Constant already double-precision
      IF (WANTV) CALL DDSCAL(P,-1.0d0,V(1,L),1)
  580 CONTINUE
c
c           order the singular value.
c
  590 IF (L.EQ.MM) GO TO 600
c           ...exit
      IF (S(L).GE.S(L+1)) GO TO 600
      T = S(L)
      S(L) = S(L+1)
      S(L+1) = T
      IF (WANTV .AND. L.LT.P) CALL DDSWAP(P,V(1,L),1,V(1,L+1),1)
      IF (WANTU .AND. L.LT.N) CALL DDSWAP(N,U(1,L),1,U(1,L+1),1)
      L = L + 1
      GO TO 590
  600 CONTINUE
      ITER = 0
      M = M - 1
  610 CONTINUE
      GO TO 360
  620 CONTINUE
      RETURN
      END
      SUBROUTINE DDAXPY(N,DA,DX,INCX,DY,INCY)
C*PT*WARNING* Already double-precision
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      DOUBLE PRECISION DX(*),DY(*),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
c
      IF (N.LE.0) RETURN
C*PT*WARNING* Constant already double-precision
      IF (DA.EQ.0.0d0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DY(IY) = DY(IY) + DA*DX(IX)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 M = MOD(N,4)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF (N.LT.4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
          DY(I) = DY(I) + DA*DX(I)
          DY(I+1) = DY(I+1) + DA*DX(I+1)
          DY(I+2) = DY(I+2) + DA*DX(I+2)
          DY(I+3) = DY(I+3) + DA*DX(I+3)
   50 CONTINUE
      RETURN
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DDDOT(N,DX,INCX,DY,INCY)
C*PT*WARNING* Already double-precision
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C*PT*WARNING* Constant already double-precision
c
      DDDOT = 0.0d0
C*PT*WARNING* Constant already double-precision
      DTEMP = 0.0d0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DTEMP + DX(IX)*DY(IY)
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      DDDOT = DTEMP
      RETURN
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF (N.LT.5) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DTEMP = DTEMP + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     +            DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50 CONTINUE
   60 DDDOT = DTEMP
      RETURN
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DDNRM2(N,X,INCX)
*     .. Scalar Arguments ..
      INTEGER INCX,N
C*PT*WARNING* Already double-precision
*     .. Array Arguments ..
      DOUBLE PRECISION X(*)
C*PT*WARNING* Already double-precision
*     ..
*
*  DDNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DDNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
*     .. Local Scalars ..
      INTEGER IX
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ABSXI,NORM,SCALE,SSQ
*     .. Intrinsic Functions ..
      INTRINSIC ABS,SQRT
*     ..
*     .. Executable Statements ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          NORM = ZERO
      ELSE IF (N.EQ.1) THEN
          NORM = ABS(X(1))
      ELSE
          SCALE = ZERO
          SSQ = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (X(IX).NE.ZERO) THEN
                  ABSXI = ABS(X(IX))
                  IF (SCALE.LT.ABSXI) THEN
                      SSQ = ONE + SSQ* (SCALE/ABSXI)**2
                      SCALE = ABSXI
                  ELSE
                      SSQ = SSQ + (ABSXI/SCALE)**2
                  END IF
              END IF
   10     CONTINUE
          NORM = SCALE*SQRT(SSQ)
      END IF
*
      DDNRM2 = NORM
      RETURN
*
*     End of DDNRM2.
*
      END
      SUBROUTINE DDROT(N,DX,INCX,DY,INCY,C,S)
C*PT*WARNING* Already double-precision
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      DOUBLE PRECISION DX(*),DY(*),DTEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
c
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = C*DX(IX) + S*DY(IY)
          DY(IY) = C*DY(IY) - S*DX(IX)
          DX(IX) = DTEMP
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
c
c       code for both increments equal to 1
c
   20 DO 30 I = 1,N
          DTEMP = C*DX(I) + S*DY(I)
          DY(I) = C*DY(I) - S*DX(I)
          DX(I) = DTEMP
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DDROTG(DA,DB,C,S)
C*PT*WARNING* Already double-precision
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      DOUBLE PRECISION DA,DB,C,S,ROE,SCALE,R,Z
c
      ROE = DB
      IF (DABS(DA).GT.DABS(DB)) ROE = DA
      SCALE = DABS(DA) + DABS(DB)
C*PT*WARNING* Constant already double-precision
      IF (SCALE.NE.0.0d0) GO TO 10
C*PT*WARNING* Constant already double-precision
      C = 1.0d0
C*PT*WARNING* Constant already double-precision
      S = 0.0d0
C*PT*WARNING* Constant already double-precision
      R = 0.0d0
C*PT*WARNING* Constant already double-precision
      Z = 0.0d0
      GO TO 20
   10 R = SCALE*DSQRT((DA/SCALE)**2+ (DB/SCALE)**2)
C*PT*WARNING* Constant already double-precision
      R = DSIGN(1.0d0,ROE)*R
      C = DA/R
      S = DB/R
C*PT*WARNING* Constant already double-precision
      Z = 1.0d0
      IF (DABS(DA).GT.DABS(DB)) Z = S
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (DABS(DB).GE.DABS(DA) .AND. C.NE.0.0d0) Z = 1.0d0/C
   20 DA = R
      DB = Z
      RETURN
      END
      SUBROUTINE DDSCAL(N,DA,DX,INCX)
C*PT*WARNING* Already double-precision
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      DOUBLE PRECISION DA,DX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
c
      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) GO TO 20
c
c        code for increment not equal to 1
c
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
          DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DX(I) = DA*DX(I)
   30 CONTINUE
      IF (N.LT.5) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DX(I) = DA*DX(I)
          DX(I+1) = DA*DX(I+1)
          DX(I+2) = DA*DX(I+2)
          DX(I+3) = DA*DX(I+3)
          DX(I+4) = DA*DX(I+4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DDSWAP(N,DX,INCX,DY,INCY)
C*PT*WARNING* Already double-precision
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
c
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DX(IX)
          DX(IX) = DY(IY)
          DY(IY) = DTEMP
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 M = MOD(N,3)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DX(I)
          DX(I) = DY(I)
          DY(I) = DTEMP
   30 CONTINUE
      IF (N.LT.3) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
          DTEMP = DX(I)
          DX(I) = DY(I)
          DY(I) = DTEMP
          DTEMP = DX(I+1)
          DX(I+1) = DY(I+1)
          DY(I+1) = DTEMP
          DTEMP = DX(I+2)
          DX(I+2) = DY(I+2)
          DY(I+2) = DTEMP
   50 CONTINUE
      RETURN
      END
