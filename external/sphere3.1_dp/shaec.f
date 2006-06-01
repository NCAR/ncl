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
c ... file shaec.f
c
c     this file contains code and documentation for subroutines
c     shaec and shaeci
c
c ... files which must be loaded with shaec.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshaec,lshaec,work,lwork,ierror)
c
c     subroutine shaec performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on an equally spaced grid.  the
c     associated legendre functions are recomputed rather than stored
c     as they are in subroutine shaes.  the analysis is described
c     below at output parameters a,b.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shaec,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the colatitude
c            point theta(i) = (i-1)*pi/(nlat-1) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.
c
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shaec.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shaec.  jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shaec. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshaec an array which must be initialized by subroutine shaeci.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shaec. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                    nlat*(nt*nlon+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                    l2*(nt*nlon+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shsec. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     2. the normalized z functions for m even
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       the integral from tau = 0 to tau = pi of
c                       cos(k*theta)*cos(k*tau)*pbar(m,n,tau)*sin(tau)
c                       (first and last terms in this sum are divided
c                       by 2)
c
c     3. the normalized z functions for m odd
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       of the integral from tau = 0 to tau = pi of
c                       sin(k*theta)*sin(k*tau)*pbar(m,n,tau)*sin(tau)
c
c     4. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon
c                       of g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon
c                       of g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c     5. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       c(m+1,i)*zbar(m,n,theta(i))
c                       (first and last terms in this sum are
c                       divided by 2)
c
c     b(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       s(m+1,i)*zbar(m,n,theta(i))
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshaec
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
c
c     subroutine shaeci initializes the array wshaec which can then
c     be used repeatedly by subroutine shaec.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaeci. the array wshaec is an output
c            parameter which is described below. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
C*PL*ERROR* Comment line too long
c     dwork  a double precision dwork array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shaeci.  ldwork  must be at least
c            nlat+1.
c
c
c     output parameters
c
c     wshaec an array which is initialized for use by subroutine shaec.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshaec
c            = 4  error in the specification of ldwork
c
c
c *******************************************************************
      SUBROUTINE DSHAEC(NLAT,NLON,ISYM,NT,G,IDG,JDG,A,B,MDAB,NDAB,
     +                  WSHAEC,LSHAEC,WORK,LWORK,IERROR)
      DOUBLE PRECISION G
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSHAEC
      DOUBLE PRECISION WORK
      DIMENSION G(IDG,JDG,*),A(MDAB,NDAB,*),B(MDAB,NDAB,*),WSHAEC(*),
     +          WORK(*)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IF (ISYM.LT.0 .OR. ISYM.GT.2) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IF ((ISYM.EQ.0.AND.IDG.LT.NLAT) .OR.
     +    (ISYM.NE.0.AND.IDG.LT. (NLAT+1)/2)) RETURN
      IERROR = 6
      IF (JDG.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT,NLON/2+1)
      IF (MDAB.LT.MMAX) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
      IMID = (NLAT+1)/2
      LZZ1 = 2*NLAT*IMID
      LABC = 3* ((MMAX-2)* (NLAT+NLAT-MMAX-1))/2
      IF (LSHAEC.LT.LZZ1+LABC+NLON+15) RETURN
      IERROR = 10
      LS = NLAT
      IF (ISYM.GT.0) LS = IMID
      NLN = NT*LS*NLON
      IF (LWORK.LT.NLN+MAX0(LS*NLON,3*NLAT*IMID)) RETURN
      IERROR = 0
      IST = 0
      IF (ISYM.EQ.0) IST = IMID
      IW1 = LZZ1 + LABC + 1
      CALL DSHAEC1(NLAT,ISYM,NT,G,IDG,JDG,A,B,MDAB,NDAB,IMID,LS,NLON,
     +            WORK,WORK(IST+1),WORK(NLN+1),WORK(NLN+1),WSHAEC,
     +            WSHAEC(IW1))
      RETURN
      END
      SUBROUTINE DSHAEC1(NLAT,ISYM,NT,G,IDGS,JDGS,A,B,MDAB,NDAB,IMID,
     +                   IDG,JDG,GE,GO,WORK,ZB,WZFIN,WHRFFT)
      DOUBLE PRECISION G
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION GE
      DOUBLE PRECISION GO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION ZB
      DOUBLE PRECISION WZFIN
      DOUBLE PRECISION WHRFFT
      DOUBLE PRECISION TSN
      DOUBLE PRECISION FSN
c
c     whrfft must have at least nlon+15 locations
c     wzfin must have 2*l*(nlat+1)/2 + ((l-3)*l+2)/2 locations
c     zb must have 3*l*(nlat+1)/2 locations
c     work must have ls*nlon locations
c
      DIMENSION G(IDGS,JDGS,1),A(MDAB,NDAB,1),B(MDAB,NDAB,1),
     +          GE(IDG,JDG,1),GO(IDG,JDG,1),ZB(IMID,NLAT,3),WZFIN(1),
     +          WHRFFT(1),WORK(1)

      LS = IDG
      NLON = JDG
      MMAX = MIN0(NLAT,NLON/2+1)
      MDO = MMAX
      IF (MDO+MDO-1.GT.NLON) MDO = MMAX - 1
      NLP1 = NLAT + 1
      TSN = 2.D0/NLON
      FSN = 4.D0/NLON
      MODL = MOD(NLAT,2)
      IMM1 = IMID
      IF (MODL.NE.0) IMM1 = IMID - 1
      IF (ISYM.NE.0) GO TO 15
      DO 5 K = 1,NT
          DO 5 I = 1,IMM1
              DO 5 J = 1,NLON
                  GE(I,J,K) = TSN* (G(I,J,K)+G(NLP1-I,J,K))
                  GO(I,J,K) = TSN* (G(I,J,K)-G(NLP1-I,J,K))
    5 CONTINUE
      GO TO 30
   15 DO 20 K = 1,NT
          DO 20 I = 1,IMM1
              DO 20 J = 1,NLON
                  GE(I,J,K) = FSN*G(I,J,K)
   20 CONTINUE
      IF (ISYM.EQ.1) GO TO 27
   30 IF (MODL.EQ.0) GO TO 27
      DO 25 K = 1,NT
          DO 25 J = 1,NLON
              GE(IMID,J,K) = TSN*G(IMID,J,K)
   25 CONTINUE
   27 DO 35 K = 1,NT
          CALL DHRFFTF(LS,NLON,GE(1,1,K),LS,WHRFFT,WORK)
          IF (MOD(NLON,2).NE.0) GO TO 35
          DO 36 I = 1,LS
              GE(I,NLON,K) = .5D0*GE(I,NLON,K)
   36     CONTINUE
   35 CONTINUE
      DO 40 K = 1,NT
          DO 40 MP1 = 1,MMAX
              DO 40 NP1 = MP1,NLAT
                  A(MP1,NP1,K) = 0.D0
                  B(MP1,NP1,K) = 0.D0
   40 CONTINUE
      IF (ISYM.EQ.1) GO TO 145
      CALL DZFIN(2,NLAT,NLON,0,ZB,I3,WZFIN)
      DO 110 K = 1,NT
          DO 110 I = 1,IMID
              DO 110 NP1 = 1,NLAT,2
                  A(1,NP1,K) = A(1,NP1,K) + ZB(I,NP1,I3)*GE(I,1,K)
  110 CONTINUE
      NDO = NLAT
      IF (MOD(NLAT,2).EQ.0) NDO = NLAT - 1
      DO 120 MP1 = 2,MDO
          M = MP1 - 1
          CALL DZFIN(2,NLAT,NLON,M,ZB,I3,WZFIN)
          DO 120 K = 1,NT
              DO 120 I = 1,IMID
                  DO 120 NP1 = MP1,NDO,2
                      A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                               ZB(I,NP1,I3)*GE(I,2*MP1-2,K)
                      B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                               ZB(I,NP1,I3)*GE(I,2*MP1-1,K)
  120 CONTINUE
      IF (MDO.EQ.MMAX .OR. MMAX.GT.NDO) GO TO 135
      CALL DZFIN(2,NLAT,NLON,MDO,ZB,I3,WZFIN)
      DO 130 K = 1,NT
          DO 130 I = 1,IMID
              DO 130 NP1 = MMAX,NDO,2
                  A(MMAX,NP1,K) = A(MMAX,NP1,K) +
     +                            ZB(I,NP1,I3)*GE(I,2*MMAX-2,K)
  130 CONTINUE
  135 IF (ISYM.EQ.2) RETURN
  145 CALL DZFIN(1,NLAT,NLON,0,ZB,I3,WZFIN)
      DO 150 K = 1,NT
          DO 150 I = 1,IMM1
              DO 150 NP1 = 2,NLAT,2
                  A(1,NP1,K) = A(1,NP1,K) + ZB(I,NP1,I3)*GO(I,1,K)
  150 CONTINUE
      NDO = NLAT
      IF (MOD(NLAT,2).NE.0) NDO = NLAT - 1
      DO 160 MP1 = 2,MDO
          M = MP1 - 1
          MP2 = MP1 + 1
          CALL DZFIN(1,NLAT,NLON,M,ZB,I3,WZFIN)
          DO 160 K = 1,NT
              DO 160 I = 1,IMM1
                  DO 160 NP1 = MP2,NDO,2
                      A(MP1,NP1,K) = A(MP1,NP1,K) +
     +                               ZB(I,NP1,I3)*GO(I,2*MP1-2,K)
                      B(MP1,NP1,K) = B(MP1,NP1,K) +
     +                               ZB(I,NP1,I3)*GO(I,2*MP1-1,K)
  160 CONTINUE
      MP2 = MMAX + 1
      IF (MDO.EQ.MMAX .OR. MP2.GT.NDO) RETURN
      CALL DZFIN(1,NLAT,NLON,MDO,ZB,I3,WZFIN)
      DO 170 K = 1,NT
          DO 170 I = 1,IMM1
              DO 170 NP1 = MP2,NDO,2
                  A(MMAX,NP1,K) = A(MMAX,NP1,K) +
     +                            ZB(I,NP1,I3)*GO(I,2*MMAX-2,K)
  170 CONTINUE
      RETURN
      END

      SUBROUTINE DSHAECI(NLAT,NLON,WSHAEC,LSHAEC,DWORK,LDWORK,IERROR)
      DOUBLE PRECISION WSHAEC
      DIMENSION WSHAEC(LSHAEC)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.4) RETURN
      IERROR = 3
      IMID = (NLAT+1)/2
      MMAX = MIN0(NLAT,NLON/2+1)
      LZZ1 = 2*NLAT*IMID
      LABC = 3* ((MMAX-2)* (NLAT+NLAT-MMAX-1))/2
      IF (LSHAEC.LT.LZZ1+LABC+NLON+15) RETURN
      IERROR = 4
      IF (LDWORK.LT.NLAT+1) RETURN
      IERROR = 0
      CALL DZFINIT(NLAT,NLON,WSHAEC,DWORK)
      IW1 = LZZ1 + LABC + 1
      CALL DHRFFTI(NLON,WSHAEC(IW1))
      RETURN
      END
