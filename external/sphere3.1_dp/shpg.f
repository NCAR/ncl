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
c                           August 2003
c
c ... in file shpg.f
c
c     this file contains code and documentation for subroutines
c     shpgi and shpg.
c
c ... files which must be loaded with shpg.f
c
c     hrfft.f
c
c     shpgi initializes the arrays wshp and iwshp for subsequent
c     use in subroutine shpg, which performs the harmonic projection
c     which is equivalent to a harmonic analysis followed by
c     harmonic synthesis but faster and with less memory.
c     (see description of subroutine shpg below).
c
c     subroutine shpgi(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,
c    1 liwshp,work,lwork,ierror)
c
c     shpgi initializes arrays wshp and iwshp for repeated use
c     by subroutine shpg ....
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
c            nlon must be at least 4.
c
c     isym   currently not used, no equatorial symmetries assumed,
c            only whole sphere computations.
c
c     mtrunc the highest longitudinal wave number retained in the
c            projection. It must be less than or equal to
c            the minimum of nlat-1 and nlon/2. The first wave
c            number is zero. For example, if wave numbers 0 and
c            1 are desired then mtrunc = 1.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpgi. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpgi. It must be at least
c            4*(nlat+1).
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpgi. It must be at least
c            1.25*(nlat+1)**2+7*nlat+8.
c
c     **************************************************************
c
c     output parameters
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpg.
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpg.
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
      SUBROUTINE DSHPGI(NLAT,NLON,ISYM,MTRUNC,WSHP,LWSHP,IWSHP,LIWSHP,
     +                 WORK,LWORK,IERROR)
      DOUBLE PRECISION WSHP
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION WORK(*)
      DIMENSION WSHP(*),IWSHP(*)
c
      IERROR = 1
      IF (NLAT.LT.1) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
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
      KW4 = KW3 + 2*NTE
      KW5 = KW4 + 2*NTE
      KW6 = KW5 + NTE
      KW7 = KW6 + NTE
      KW8 = KW7 + 4*NTE
      KW9 = KW8 + 2*NTE
      KW10 = KW9 + NLOC1
      KW11 = KW10 + NLOC1
      KTOT = KW11 + NTE*NTE
c
      CALL DSHPGI1(NLAT,NLON,ISYM,MTRUNC,NTE,IERROR,WSHP(IW1),WSHP(IW2),
     +            WSHP(IW3),WSHP(IW4),IWSHP(JW1),IWSHP(JW2),IWSHP(JW3),
     +            IWSHP(JW4),WORK(KW1),WORK(KW2),WORK(KW3),WORK(KW4),
     +            WORK(KW5),WORK(KW6),WORK(KW7),WORK(KW8),WORK(KW9),
     +            WORK(KW10),WORK(KW11))
      RETURN
      END
      SUBROUTINE DSHPGI1(NLAT,NLON,ISYM,MTRUNC,IDP,IERROR,PE,PO,ZE,ZO,
     +                  IPSE,JZSE,IPSO,JZSO,CP,WX,THET,GWTS,XX,Z,A,B,
     +                  PED,POD,U)
      DOUBLE PRECISION PE
      DOUBLE PRECISION PO
      DOUBLE PRECISION ZE
      DOUBLE PRECISION ZO
      DOUBLE PRECISION ZORT
      DOUBLE PRECISION TUSL
      DOUBLE PRECISION TOE
      DOUBLE PRECISION DFN
      DOUBLE PRECISION RAND
      DOUBLE PRECISION DMAX
      DOUBLE PRECISION SUM1
C*PT*WARNING* Already double-precision
c
      DOUBLE PRECISION SUM,EPS,A1,B1,C1,WORK
C*PT*WARNING* Constant already double-precision
      PARAMETER (EPS=5.0d-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP(IDP),WX(IDP),THET(NLAT),GWTS(NLAT),XX(IDP),
     +                 Z(IDP),A(4*IDP),B(2*IDP),PED(IDP,IDP,2),
     +                 POD(IDP,IDP,2),U(IDP,IDP)
c
      DIMENSION PE(IDP,IDP,2),PO(IDP,IDP,2),ZE(IDP,IDP,2),ZO(IDP,IDP,2),
     +          IPSE(IDP,2),JZSE(IDP,2),IPSO(IDP,2),JZSO(IDP,2),NSHE(2),
     +          NSHO(2)
      DIMENSION ZORT(64,64,2)
c
      NS2 = NLAT/2
      MODN = NLAT - NS2 - NS2
      NTE = (NLAT+1)/2
      NTO = NLAT - NTE
      TUSL = 0.D0
      TOE = 0.D0
c
c     compute gauss grid distribution
c
      LWORK = NLAT + 1
      CALL DGAQDP(NLAT,THET,GWTS,WORK,LWORK,IERR)
      IF (IERR.NE.0) WRITE (*,FMT=160) IERR
  160 FORMAT (' error in gaqd =',i5)
      DO I = 1,NTO
          GWTS(I) = GWTS(I) + GWTS(I)
      END DO
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
          NEM = (NLAT-M+1)/2
          NEC = NTE - NEM
c
c     compute associated legendre functions
c
          IF (M.LE.1) THEN
              DO 205 J = 1,NEM
                  N = J + J + M - 2
                  CALL DDLFKG(M,N,CP)
                  DO I = 1,NTE
                      CALL DDLFTG(M,N,THET(I),CP,PED(I,J+NEC,IP))
                  END DO
  205         CONTINUE
c
          ELSE
c
              DO 207 J = 1,NEM
                  N = J + J + M - 2
                  IF (M.GT.1 .AND. N.GT.MXTR) THEN
                      DO I = 1,NTE
                          U(I,J+NEC) = PED(I,J+NEC,IP)
                      END DO
                      GO TO 207
                  END IF
                  A1 = B(N-1)*A(N+M-3)/A(N+M-1)
                  B1 = A(N-M+1)/A(N+M-1)
                  IF (N-M.LE.1) THEN
                      DO I = 1,NTE
                          U(I,J+NEC) = A1*PED(I,J+NEC-1,IP) -
     +                                 B1*PED(I,J+NEC,IP)
                      END DO
                  ELSE
                      C1 = B(N-1)*A(N-M-1)/A(N+M-1)
                      DO I = 1,NTE
                          U(I,J+NEC) = A1*PED(I,J+NEC-1,IP) -
     +                                 B1*PED(I,J+NEC,IP) +
     +                                 C1*U(I,J+NEC-1)
                      END DO
                  END IF
  207         CONTINUE
              DO J = 1,NEM
                  DO I = 1,NTE
                      PED(I,J+NEC,IP) = U(I,J+NEC)
                  END DO
              END DO
          END IF
          IF (NEC.LE.0) GO TO 200
c
c     generate orthogonal vector
c
          DO I = 1,NTE
              XX(I) = RAND()
          END DO
c
          IT = 0
  201     DO I = 1,NTE
C*PT*WARNING* Constant already double-precision
              Z(I) = 0.0d0
              WX(I) = GWTS(I)*XX(I)
          END DO
          DO 220 J = 1,NTE
              IF (J.EQ.NEC) GO TO 220
              CALL DGS(NTE,WX,PED(1,J,IP),Z)
  220     CONTINUE
c
          DO I = 1,NTE
              XX(I) = XX(I) - Z(I)
          END DO
          CALL DNORMAL(NTE,XX,IDP,GWTS)
          IT = IT + 1
          IF (IT.LE.2) GO TO 201
          DO I = 1,NTE
              PED(I,NEC,IP) = XX(I)
          END DO
  200 CONTINUE
c
c     reorder if mtrunc is less than nlat-1
c         case of even functions
c
      NMX = NLAT - MXTR
      IF (MODN.EQ.1) THEN
          NSHE(1) = NMX/2
          NSHE(2) = (NMX-1)/2
      ELSE
          NSHE(1) = (NMX-1)/2
          NSHE(2) = NMX/2
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
                  SUM = PED(J,I,IP)*GWTS(J)
                  ZE(J,I,IP) = SUM
                  PE(I,J,IP) = PED(I,J,IP)
                  IF (DABS(SUM).GT.EPS .AND. LOCK.EQ.0) THEN
                      LOCK = 1
                      JZSE(I,IP) = J
                  END IF
              END DO
          END DO
  250 CONTINUE
c
c     check orthogonality of pe(i,j,mp1)  mp1=1,2
c
      DO IP = 1,2
          DMAX = 0.D0
          DO I = 1,NTE
              DO J = 1,NTE
                  SUM1 = 0.D0
                  DO K = 1,NTE
                      SUM1 = SUM1 + ZE(K,I,IP)*PE(K,J,IP)
                  END DO
                  ZO(I,J,IP) = SUM1
                  IF (I.NE.J) THEN
                      DMAX = MAX(DMAX,ABS(SUM1))
                  ELSE
                      DMAX = MAX(DMAX,ABS(SUM1-1.0D0))
                  END IF
              END DO
          END DO
      END DO
c
c     compute n**2 basis (odd functions)
c
      IP = 2
      DO 300 MP1 = 1,MXTR + 1
          IP = 3 - IP
          M = MP1 - 1
          MS2 = MP1/2
          NEM = (NLAT-M+1)/2
          NOM = NLAT - M - NEM
          NOC = NTO - NOM
c
c     compute associated legendre functions
c
          IF (M.LE.1) THEN
              DO 305 J = 1,NOM
                  N = J + J + M - 1
                  CALL DDLFKG(M,N,CP)
                  DO I = 1,NTE
                      CALL DDLFTG(M,N,THET(I),CP,POD(I,J+NOC,IP))
                  END DO
C*PT*WARNING* Constant already double-precision
                  IF (MODN.GT.0) POD(NTE,J+NOC,IP) = 0.0d0
  305         CONTINUE
c
          ELSE
c
              DO 307 J = 1,NOM
                  N = J + J + M - 1
                  IF (M.GT.1 .AND. N.GT.MXTR) THEN
                      DO I = 1,NTE
                          U(I,J+NOC) = POD(I,J+NOC,IP)
                      END DO
                      GO TO 304
                  END IF
                  A1 = B(N-1)*A(N+M-3)/A(N+M-1)
                  B1 = A(N-M+1)/A(N+M-1)
                  IF (N-M.LE.1) THEN
                      DO I = 1,NTE
                          U(I,J+NOC) = A1*POD(I,J+NOC-1,IP) -
     +                                 B1*POD(I,J+NOC,IP)
                      END DO
                  ELSE
                      C1 = B(N-1)*A(N-M-1)/A(N+M-1)
                      DO I = 1,NTE
                          U(I,J+NOC) = A1*POD(I,J+NOC-1,IP) -
     +                                 B1*POD(I,J+NOC,IP) +
     +                                 C1*U(I,J+NOC-1)
                      END DO
                  END IF
C*PT*WARNING* Constant already double-precision
  304             IF (MODN.EQ.1) U(NTE,J+NOC) = 0.0d0
  307         CONTINUE
              DO J = 1,NOM
                  DO I = 1,NTE
                      POD(I,J+NOC,IP) = U(I,J+NOC)
                  END DO
              END DO
          END IF
c
          IF (NOC.LE.0) GO TO 300
          DO I = 1,NTE
              XX(I) = RAND()
          END DO
C*PT*WARNING* Constant already double-precision
          IF (MODN.EQ.1) XX(NTE) = 0.0d0
          IT = 0
  306     DO I = 1,NTE
              Z(I) = 0.D0
              WX(I) = GWTS(I)*XX(I)
          END DO
          DO 330 J = 1,NTO
              IF (J.EQ.NOC) GO TO 330
              CALL DGS(NTE,WX,POD(1,J,IP),Z(1))
  330     CONTINUE
c
          DO I = 1,NTE
              XX(I) = XX(I) - Z(I)
          END DO
          CALL DNORMAL(NTE,XX,IDP,GWTS)
          IT = IT + 1
          IF (IT.LE.2) GO TO 306
          DO I = 1,NTE
              POD(I,NOC,IP) = XX(I)
          END DO
C*PT*WARNING* Constant already double-precision
          IF (MODN.EQ.1) POD(NTE,NOC,IP) = 0.0d0
  300 CONTINUE
c
      NMX = NLAT - MXTR
      IF (MODN.EQ.1) THEN
          NSHO(1) = (NMX-1)/2
          NSHO(2) = NMX/2
      ELSE
          NSHO(1) = NMX/2
          NSHO(2) = (NMX-1)/2
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
                  SUM = POD(J,I,IP)*GWTS(J)
                  ZO(J,I,IP) = SUM
                  PO(I,J,IP) = POD(I,J,IP)
                  IF (DABS(SUM).GT.EPS .AND. LOCK.EQ.0) THEN
                      LOCK = 1
                      JZSO(I,IP) = J
                  END IF
              END DO
          END DO
      END DO
c
c     check orthogonality of po(i,j,mp1)  mp1=1,2
c
      DO IP = 1,2
          DMAX = 0.D0
          DO I = 1,NTO
              DO J = 1,NTO
                  SUM1 = 0.D0
                  DO K = 1,NTO
                      SUM1 = SUM1 + ZO(K,I,IP)*PO(K,J,IP)
                  END DO
                  ZORT(I,J,IP) = SUM1
                  IF (I.NE.J) THEN
                      DMAX = MAX(DMAX,ABS(SUM1))
                  ELSE
                      DMAX = MAX(DMAX,ABS(SUM1-1.0D0))
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
c ... file shpg.f
c
c ... files which must be loaded with shpg.f
c
c     hrfft.f
c
c     shpg computes the harmonic projection, which is
c     equivalent to a harmonic analysis (forward) followed
c     by a harmonic synthesis (backward transform).
c     shpg uses the n**2 projection or complement when appropriate
c     as well as  odd/even factorization and zero truncation on an
c     on a Gaussian distributed grid as defined in the JCP paper
c     "Generalized discrete spherical harmonic transforms"
c     by Paul N. Swarztrauber and William F. Spotz
c     J. Comp. Phys., 159(2000) pp. 213-230.
c
c     subroutine shpg(nlat,nlon,isym,mtrunc,x,y,idxy,
c    1        wshp,lwshp,iwshp,liwshp,work,lwork,ierror)
c
c     shpg projects the array x onto the set of functions represented
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
c            nlon must be at least 4.
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
c            appear in the program that calls shpg. It must be
c            at least nlat.
c
c     wshp   a single precision array that must be saved for
c            repeated use by subroutine shpg.
c
c     lwshp  the dimension of the array wshp as it appears in the
c            program that calls shpgi. It must be at least
c            2*(nlat+1)**2+nlon+log2(nlon)
c
c     iwshp  an integer array that must be saved for repeated
c            use by subroutine shpg.
c
c
c     liwshp the dimension of the array iwshp as it appears in the
c            program that calls shpgi. It must be at least
c            4*(nlat+1).
c
c     work   a single precision work array that does
c            not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shpg. It must be at least
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
      SUBROUTINE DSHPG(NLAT,NLON,ISYM,MTRUNC,X,Y,IDXY,WSHP,LWSHP,IWSHP,
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
      IF (NLAT.LT.1) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
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
      CALL DSHPG1(NLAT,NLON,ISYM,MTRUNC,Y,Y,IDXY,IERROR,NTE,WSHP(IW1),
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
      SUBROUTINE DSHPG1(NLAT,NLON,ISYM,MTRUNC,SX,SY,IDXY,IERROR,IDP,PE,
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
      MXTR = MIN(NLAT-1,NLON/2,MTRUNC)
      NMX = NLAT - MXTR
      IF (MODN.EQ.1) THEN
          NSHE(1) = NMX/2
          NSHE(2) = (NMX-1)/2
          NSHO(1) = (NMX-1)/2
          NSHO(2) = NMX/2
      ELSE
          NSHE(1) = (NMX-1)/2
          NSHE(2) = NMX/2
          NSHO(1) = NMX/2
          NSHO(2) = (NMX-1)/2
      END IF
c
      IP = 2
      DO 100 MP1 = 1,MXTR + 1
          IP = 3 - IP
          IF (MXTR.EQ.NLAT-1 .AND. MP1.EQ.1) THEN
              DO I = 1,NLAT
                  SY(I,MP1) = SX(I,MP1)
              END DO
c      if(mp1.eq.2) then
c      sy(1,2) = 0.
c      sy(nlat,2) = 0.
c      end if
c      if(nlon.ge.3) then
c      sy(1,3) = 0.
c      sy(nlat,3) = 0.
c      do i=2,nlat-1
c      sy(i,3) = sx(i,3)
c      end do
c      end if
              GO TO 100
          END IF
          M = MP1 - 1
          MPM = MAX(1,M+M)
          MS2 = MP1/2
c      mrank = min(nlat-m,nlat-ms2-ms2)
c      nrank = nlat-mrank
c      nem = (mrank+1)/2-nshe(ip)
c      nom = mrank-(mrank+1)/2-nsho(ip)
          NEM = (NLAT-M+1)/2 - NSHE(IP)
          NOM = (NLAT-M)/2 - NSHO(IP)
          NEC = NTE - NEM
          NOC = NTO - NOM
          DO I = 1,NTE
              XE(I,1) = .5D0* (SX(I,MPM)+SX(NLAT+1-I,MPM))
              XO(I,1) = .5D0* (SX(I,MPM)-SX(NLAT+1-I,MPM))
          END DO
c      if(modn.eq.1) then
c      xe(nte,1) = sx(nte,mpm)
c      xo(nte,1) = 0.
c      end if
          IF (MPM.LT.NLON) THEN
              DO I = 1,NTE
                  XE(I,2) = .5D0* (SX(I,MPM+1)+SX(NLAT+1-I,MPM+1))
                  XO(I,2) = .5D0* (SX(I,MPM+1)-SX(NLAT+1-I,MPM+1))
              END DO
c      if(modn.eq.1) then
c      xe(nte,2) = sx(nte,mpm+1)
c      xo(nte,2) = 0.
c      end if
          END IF
          LAG = 0
          IF (M.EQ.0 .OR. MPM.EQ.NLON) LAG = 1
          IF (3*NEC.LT.2*NEM .OR. NEM.EQ.0) THEN
              CALL DTMXMX(LAG,NTE,NEC,IDP,PE(1,1,IP),NTE,IDP,ZE(1,1,IP),
     +                   XE,YE,IPSE(1,IP),JZSE(1,IP))
              DO I = 1,NTE
                  YE(I,1) = XE(I,1) - YE(I,1)
              END DO
              IF (MPM.LT.NLON .AND. M.NE.0) THEN
                  DO I = 1,NTE
                      YE(I,2) = XE(I,2) - YE(I,2)
                  END DO
              END IF
          ELSE
              CALL DTMXMX(LAG,NTE,NEM,IDP,PE(1,NEC+1,IP),NTE,IDP,
     +                   ZE(1,NEC+1,IP),XE,YE,IPSE(NEC+1,IP),
     +                   JZSE(NEC+1,IP))
          END IF
          IF (3*NOC.LT.2*NOM .OR. NOM.EQ.0) THEN
              CALL DTMXMX(LAG,NTO,NOC,IDP,PO(1,1,IP),NTO,IDP,ZO(1,1,IP),
     +                   XO,YO,IPSO(1,IP),JZSO(1,IP))
              DO I = 1,NTO
                  YO(I,1) = XO(I,1) - YO(I,1)
              END DO
              IF (MPM.LT.NLON .AND. M.NE.0) THEN
                  DO I = 1,NTO
                      YO(I,2) = XO(I,2) - YO(I,2)
                  END DO
              END IF
          ELSE
              CALL DTMXMX(LAG,NTO,NOM,IDP,PO(1,NOC+1,IP),NTO,IDP,
     +                   ZO(1,NOC+1,IP),XO,YO,IPSO(NOC+1,IP),
     +                   JZSO(NOC+1,IP))
          END IF
          DO I = 1,NTO
              SY(I,MPM) = YE(I,1) + YO(I,1)
              SY(NLAT+1-I,MPM) = YE(I,1) - YO(I,1)
          END DO
          IF (NTE.GT.NTO) SY(NTE,MPM) = YE(NTE,1)
          IF (MPM.LT.NLON .AND. M.NE.0) THEN
              DO I = 1,NTO
                  SY(I,MPM+1) = YE(I,2) + YO(I,2)
                  SY(NLAT+1-I,MPM+1) = YE(I,2) - YO(I,2)
              END DO
              IF (NTE.GT.NTO) SY(NTE,MPM+1) = YE(NTE,2)
          END IF
  100 CONTINUE
c
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
      SUBROUTINE DTMXMX(LAG,LR,LC,LD,A,MC,MD,B,X,Y,IS,JS)
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION SUM1
      DOUBLE PRECISION SUM2
      DIMENSION A(LD,*),B(MD,*),X(LD,2),Y(LD,2),IS(*),JS(*)
c
      KMX = MIN(LR+1,LD)
      IF (LAG.EQ.1) THEN
          DO K = 1,KMX
              Y(K,1) = 0.D0
          END DO
c      if(lc.eq.0) then
c      do k=1,lr
c      y(k,1) = x(k,1)
c      end do
c      return
c      end if
          IF (LC.LE.0) RETURN
          DO I = 1,LC
              SUM1 = 0.D0
              DO J = JS(I),MC
                  SUM1 = SUM1 + B(J,I)*X(J,1)
              END DO
              DO K = IS(I),LR
                  Y(K,1) = Y(K,1) + SUM1*A(K,I)
              END DO
          END DO
          RETURN
      END IF
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
      DIMENSION X(N),Q(N)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X,Q,SQS
c
c     normalize x
c
      SQS = 0.D0
      DO I = 1,N
c      sum = 0.
c      do j=1,n
c      sum = sum+q(i,j)*x(j)
c      end do
c      sqs = sqs+sum*x(i)
          SQS = SQS + Q(I)*X(I)*X(I)
      END DO
c
      IF (SQS.NE.0) GO TO 4
      WRITE (*,FMT=3)
    3 FORMAT (' norm of z is zero in subroutine normal')
      RETURN
    4 SQS = DSQRT(SQS)
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
c     subroutine dlfkg(m,n,cp)
c
c     subroutine dlfkg computes the coefficients in the trigonometric
c     expansion of the normalized associated legendre functions:
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                        *sin(theta)**m/(2**n*factorial(n)) times the
c                        (n+m)th derivative of (x**2-1)**n with respect
c                        to x=cos(theta)
c
c     where theta is colatitude.
c
c     subroutine dlfkg computes the coefficients cp(k) in the
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
      SUBROUTINE DDLFKG(M,N,CP)
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
      SUBROUTINE DDLFTG(M,N,THETA,CP,PB)
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
      SUBROUTINE DGAQDP(NLAT,THETA,WTS,W,LWORK,IERROR)
      DOUBLE PRECISION EPS
      DOUBLE PRECISION DDZEPP
      DOUBLE PRECISION SGND
C*PT*WARNING* Already double-precision
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             April 2002
c
c     gauss points and weights are computed using the fourier-newton
c     described in "on computing the points and weights for
c     gauss-legendre quadrature", paul n. swarztrauber, siam journal
c     on scientific computing that has been accepted for publication.
c     This routine is faster and more accurate than older program
c     with the same name.
c
C*PL*ERROR* Comment line too long
c     subroutine gaqdp computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     w       unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a double precision array with length nlat
c             containing the gaussian colatitudes in
c             increasing radians on the interval (0,pi).
c
c     wts     a double precision array with lenght nlat
c             containing the gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      DOUBLE PRECISION THETA(NLAT),WTS(NLAT),X,PI,PIS2,DTHETA,DTHALF,
     +                 CMAX,ZPREV,ZLAST,ZERO,ZHOLD,PB,DPB,DCOR,SUM,W,CZ
c
c     check work space length
c
      IERROR = 1
      IF (NLAT.LE.0) RETURN
      IERROR = 0
c
c     compute weights and points analytically when nlat=1,2
c
      IF (NLAT.EQ.1) THEN
C*PT*WARNING* Constant already double-precision
          THETA(1) = DACOS(0.0d0)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 2.0d0
          RETURN
      END IF
      IF (NLAT.EQ.2) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          X = DSQRT(1.0d0/3.0d0)
          THETA(1) = DACOS(X)
          THETA(2) = DACOS(-X)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 1.0d0
C*PT*WARNING* Constant already double-precision
          WTS(2) = 1.0d0
          RETURN
      END IF
C*PT*WARNING* Constant already double-precision
      EPS = SQRT(DDZEPP(1.0d0))
      EPS = EPS*SQRT(EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PIS2 = 2.0d0*DATAN(1.0d0)
      PI = PIS2 + PIS2
      MNLAT = MOD(NLAT,2)
      NS2 = NLAT/2
      NHALF = (NLAT+1)/2
      IDX = NS2 + 2
c
      CALL DCPDP1(NLAT,CZ,THETA(NS2+1),WTS(NS2+1))
c
      DTHETA = PIS2/NHALF
C*PT*WARNING* Constant already double-precision
      DTHALF = DTHETA/2.0d0
C*PT*WARNING* Constant already double-precision
      CMAX = .2d0*DTHETA
c
c     estimate first point next to theta = pi/2
c
      IF (MNLAT.NE.0) THEN
          ZERO = PIS2 - DTHETA
          ZPREV = PIS2
          NIX = NHALF - 1
      ELSE
          ZERO = PIS2 - DTHALF
          NIX = NHALF
      END IF
    9 IT = 0
   10 IT = IT + 1
      ZLAST = ZERO
c
c     newton iterations
c
      CALL DTPDP1(NLAT,ZERO,CZ,THETA(NS2+1),WTS(NS2+1),PB,DPB)
      DCOR = PB/DPB
      SGND = 1.0D0
C*PT*WARNING* Constant already double-precision
      IF (DCOR.NE.0.0d0) SGND = DCOR/DABS(DCOR)
      DCOR = SGND*MIN(DABS(DCOR),CMAX)
      ZERO = ZERO - DCOR
      IF (DABS(ZERO-ZLAST).GT.EPS*DABS(ZERO)) GO TO 10
      THETA(NIX) = ZERO
      ZHOLD = ZERO
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c
c     yakimiw's formula permits using old pb and dpb
c
      WTS(NIX) = (NLAT+NLAT+1)/ (DPB+PB*DCOS(ZLAST)/DSIN(ZLAST))**2
      NIX = NIX - 1
      IF (NIX.EQ.0) GO TO 30
      IF (NIX.EQ.NHALF-1) ZERO = 3.0D0*ZERO - PI
      IF (NIX.LT.NHALF-1) ZERO = ZERO + ZERO - ZPREV
      ZPREV = ZHOLD
      GO TO 9
c
c     extend points and weights via symmetries
c
   30 IF (MNLAT.NE.0) THEN
          THETA(NHALF) = PIS2
          CALL DTPDP1(NLAT,PIS2,CZ,THETA(NS2+1),WTS(NS2+1),PB,DPB)
          WTS(NHALF) = (NLAT+NLAT+1)/ (DPB*DPB)
      END IF
      DO I = 1,NS2
          WTS(NLAT-I+1) = WTS(I)
          THETA(NLAT-I+1) = PI - THETA(I)
      END DO
C*PT*WARNING* Constant already double-precision
      SUM = 0.0d0
      DO I = 1,NLAT
          SUM = SUM + WTS(I)
      END DO
      DO I = 1,NLAT
C*PT*WARNING* Constant already double-precision
          WTS(I) = 2.0d0*WTS(I)/SUM
      END DO
      RETURN
      END
      SUBROUTINE DCPDP1(N,CZ,CP,DCP)
C*PT*WARNING* Already double-precision
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative.
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant
c     coefficient is returned in cz.
c
      DOUBLE PRECISION CP(N/2+1),DCP(N/2+1),T1,T2,T3,T4,CZ

      NCP = (N+1)/2
C*PT*WARNING* Constant already double-precision
      T1 = -1.0d0
C*PT*WARNING* Constant already double-precision
      T2 = N + 1.0d0
C*PT*WARNING* Constant already double-precision
      T3 = 0.0d0
C*PT*WARNING* Constant already double-precision
      T4 = N + N + 1.0d0
      IF (MOD(N,2).EQ.0) THEN
C*PT*WARNING* Constant already double-precision
          CP(NCP) = 1.0d0
          DO J = NCP,2,-1
C*PT*WARNING* Constant already double-precision
              T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
              T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
              T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
              T4 = T4 - 2.0d0
              CP(J-1) = (T1*T2)/ (T3*T4)*CP(J)
          END DO
C*PT*WARNING* Constant already double-precision
          T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
          T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
          T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
          T4 = T4 - 2.0d0
          CZ = (T1*T2)/ (T3*T4)*CP(1)
          DO J = 1,NCP
              DCP(J) = (J+J)*CP(J)
          END DO
      ELSE
C*PT*WARNING* Constant already double-precision
          CP(NCP) = 1.0d0
          DO J = NCP - 1,1,-1
C*PT*WARNING* Constant already double-precision
              T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
              T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
              T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
              T4 = T4 - 2.0d0
              CP(J) = (T1*T2)/ (T3*T4)*CP(J+1)
          END DO
          DO J = 1,NCP
              DCP(J) = (J+J-1)*CP(J)
          END DO
      END IF
      RETURN
      END
      SUBROUTINE DTPDP1(N,THETA,CZ,CP,DCP,PB,DPB)
C*PT*WARNING* Already double-precision
c
c     computes pn(theta) and its derivative dpb(theta) with
c     respect to theta
c
      DOUBLE PRECISION CP(N/2+1),DCP(N/2+1),CZ,PB,DPB,FN,THETA,CDT,SDT,
     +                 CTH,STH,CHH
c
      FN = N
      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      IF (MOD(N,2).EQ.0) THEN
c
c     n even
c
          KDO = N/2
C*PT*WARNING* Constant already double-precision
          PB = .5d0*CZ
C*PT*WARNING* Constant already double-precision
          DPB = 0.0d0
          IF (N.GT.0) THEN
              CTH = CDT
              STH = SDT
              DO 170 K = 1,KDO
c      pb = pb+cp(k)*cos(2*k*theta)
                  PB = PB + CP(K)*CTH
c      dpb = dpb-(k+k)*cp(k)*sin(2*k*theta)
                  DPB = DPB - DCP(K)*STH
                  CHH = CDT*CTH - SDT*STH
                  STH = SDT*CTH + CDT*STH
                  CTH = CHH
  170         CONTINUE
          END IF
      ELSE
c
c     n odd
c
          KDO = (N+1)/2
C*PT*WARNING* Constant already double-precision
          PB = 0.0d0
C*PT*WARNING* Constant already double-precision
          DPB = 0.0d0
          CTH = DCOS(THETA)
          STH = DSIN(THETA)
          DO 190 K = 1,KDO
c      pb = pb+cp(k)*cos((2*k-1)*theta)
              PB = PB + CP(K)*CTH
c      dpb = dpb-(k+k-1)*cp(k)*sin((2*k-1)*theta)
              DPB = DPB - DCP(K)*STH
              CHH = CDT*CTH - SDT*STH
              STH = SDT*CTH + CDT*STH
              CTH = CHH
  190     CONTINUE
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDZEPP(X)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
c
c     estimate unit roundoff in quantities of size x.
c
      DOUBLE PRECISION A,B,C,EPS
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      A = 4.0d0/3.0d0
C*PT*WARNING* Constant already double-precision
   10 B = A - 1.0d0
      C = B + B + B
C*PT*WARNING* Constant already double-precision
      EPS = ABS(C-1.0d0)
C*PT*WARNING* Constant already double-precision
      IF (EPS.EQ.0.0d0) GO TO 10
      DDZEPP = EPS*DABS(X)
      RETURN
      END
