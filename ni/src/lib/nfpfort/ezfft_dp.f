c =======fftpack.f subset==============================================
c     subroutine ezfftf(n,r,azero,a,b,wsave)
c
c     subroutine ezfftf computes the fourier coefficients of a real
c     perodic sequence (fourier analysis). the transform is defined
c     below at output parameters azero,a and b. ezfftf is a simplified
c     but slower version of rfftf.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is must efficient when n is the product of small primes.
c
c     r       a real array of length n which contains the sequence
c             to be transformed. r is not destroyed.
c
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             in the program that calls ezfftf. the wsave array must be
c             initialized by calling subroutine ezffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by ezfftf and ezfftb.
c
c     output parameters
c
c     azero   the sum from i=1 to i=n of r(i)/n
c
c     a,b     for n even b(n/2)=0. and a(n/2) is the sum from i=1 to
c             i=n of (-1)**(i-1)*r(i)/n
c
c             for n even define kmax=n/2-1
c             for n odd  define kmax=(n-1)/2
c
c             then for  k=1,...,kmax
c
c                  a(k) equals the sum from i=1 to i=n of
c
c                       2./n*r(i)*cos(k*(i-1)*2*pi/n)
c
c                  b(k) equals the sum from i=1 to i=n of
c
c                       2./n*r(i)*sin(k*(i-1)*2*pi/n)
c
c
      SUBROUTINE DEZFFTF(N,R,AZERO,A,B,WSAVE)
      DOUBLE PRECISION R
      DOUBLE PRECISION AZERO
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION CF
      DOUBLE PRECISION CFM
      DIMENSION R(*),A(*),B(*),WSAVE(*)
c
      IF (N-2.LE.0) THEN
          IF (N-2.NE.0) THEN
              AZERO = R(1)
              RETURN
          END IF
          AZERO = .5D0* (R(1)+R(2))
          A(1) = .5D0* (R(1)-R(2))
          RETURN
      END IF
      DO I = 1,N
          WSAVE(I) = R(I)
      END DO
      CALL DRFFTF(N,WSAVE,WSAVE(N+1))
      CF = 2.D0/DBLE(N)
      CFM = -CF
      AZERO = .5D0*CF*WSAVE(1)
      NS2 = (N+1)/2
      NS2M = NS2 - 1
      DO I = 1,NS2M
          A(I) = CF*WSAVE(2*I)
          B(I) = CFM*WSAVE(2*I+1)
      END DO
C      B(NS2) = 0.D0
      IF (MOD(N,2).EQ.1) RETURN
      A(NS2) = .5D0*CF*WSAVE(N)
      B(NS2) = 0.D0
      RETURN
      END
c
c =====================================================================
c     subroutine rfftf(n,r,wsave)
c
c     subroutine rfftf computes the fourier coefficients of a real
c     perodic sequence (fourier analysis). the transform is defined
c     below at output parameter r.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is most efficient when n is a product of small primes.
c             n may change so long as different work arrays are provided
c
c     r       a real array of length n which contains the sequence
c             to be transformed
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls rfftf. the wsave array must be
c             initialized by calling subroutine rffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by rfftf and rfftb.
c
c
c     output parameters
c
c     r       r(1) = the sum from i=1 to i=n of r(i)
c
c             if n is even set l =n/2   , if n is odd set l = (n+1)/2
c
c               then for k = 2,...,l
c
c                  r(2*k-2) = the sum from i = 1 to i = n of
c
c                       r(i)*cos((k-1)*(i-1)*2*pi/n)
c
c                  r(2*k-1) = the sum from i = 1 to i = n of
c
c                      -r(i)*sin((k-1)*(i-1)*2*pi/n)
c
c             if n is even
c
c                  r(n) = the sum from i = 1 to i = n of
c
c                       (-1)**(i-1)*r(i)
c
c      *****  note
c                  this transform is unnormalized since a call of rfftf
c                  followed by a call of rfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of rfftf or rfftb.
c
      SUBROUTINE DRFFTF(N,R,WSAVE)
      DOUBLE PRECISION R
      DOUBLE PRECISION WSAVE
      DIMENSION R(*),WSAVE(*)
c
      IF (N.EQ.1) RETURN
      CALL DRFFTF1OLD(N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
c
      SUBROUTINE DRFFTF1OLD(N,C,CH,WA,IFAC)
      DOUBLE PRECISION C
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA
      DIMENSION CH(*),C(*),WA(*),IFAC(*)

      NF = IFAC(2)
      DO K1 = 1,NF
          KH = NF - K1
       END DO
      NA = 1
      L2 = N
      IW = N
      DO K1 = 1,NF
          KH = NF - K1
          IP = IFAC(KH+3)
          L1 = L2/IP
          IDO = N/L2
          IDL1 = IDO*L1
          IW = IW - (IP-1)*IDO
          NA = 1 - NA
          IF (IP.EQ.4) THEN
              IX2 = IW + IDO
              IX3 = IX2 + IDO
              IF (NA.EQ.0) THEN
                  CALL DRADF4(IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
                  GO TO 110
              END IF
              CALL DRADF4(IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
              GO TO 110
          END IF
          IF (IP.EQ.2) THEN
              IF (NA.EQ.0) THEN
                  CALL DRADF2(IDO,L1,C,CH,WA(IW))
                  GO TO 110
              END IF
              CALL DRADF2(IDO,L1,CH,C,WA(IW))
              GO TO 110
          END IF
  104     CONTINUE
          IF (IP.EQ.3) THEN
              IX2 = IW + IDO
              IF (NA.EQ.0) THEN
                  CALL DRADF3(IDO,L1,C,CH,WA(IW),WA(IX2))
                  GO TO 110
              END IF
              CALL DRADF3(IDO,L1,CH,C,WA(IW),WA(IX2))
              GO TO 110
          END IF
  106     CONTINUE
          IF (IP.EQ.5) THEN
              IX2 = IW + IDO
              IX3 = IX2 + IDO
              IX4 = IX3 + IDO
              IF (NA.EQ.0) THEN
                  CALL DRADF5(IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3),
     +                WA(IX4))
                  GO TO 110
              END IF
              CALL DRADF5(IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
              GO TO 110
          END IF
  108     CONTINUE
          IF (IDO.EQ.1) NA = 1 - NA
          IF (NA.EQ.0) THEN
              CALL DRADFG(IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
              NA = 1
          ELSE
              CALL DRADFG(IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
              NA = 0
          END IF
  110     CONTINUE
          L2 = L1
      END DO
      IF (NA.EQ.1) RETURN
      DO I = 1,N
          C(I) = CH(I)
      END DO
      RETURN
      END
c ==============================================================
c     subroutine ezfftb(n,r,azero,a,b,wsave)
c
c     subroutine ezfftb computes a real perodic sequence from its
c     fourier coefficients (fourier synthesis). the transform is
c     defined below at output parameter r. ezfftb is a simplified
c     but slower version of rfftb.
c
c     input parameters
c
c     n       the length of the output array r.  the method is most
c             efficient when n is the product of small primes.
c
c     azero   the constant fourier coefficient
c
c     a,b     arrays which contain the remaining fourier coefficients
c             these arrays are not destroyed.
c
c             the length of these arrays depends on whether n is even or
c             odd.
c
c             if n is even n/2    locations are required
c             if n is odd (n-1)/2 locations are required
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             in the program that calls ezfftb. the wsave array must be
c             initialized by calling subroutine ezffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by ezfftf and ezfftb.
c
c
c     output parameters
c
c     r       if n is even define kmax=n/2
c             if n is odd  define kmax=(n-1)/2
c
c             then for i=1,...,n
c
c                  r(i)=azero plus the sum from k=1 to k=kmax of
c
c                  a(k)*cos(k*(i-1)*2*pi/n)+b(k)*sin(k*(i-1)*2*pi/n)
c
c     ********************* complex notation **************************
c
c             for j=1,...,n
c
c             r(j) equals the sum from k=-kmax to k=kmax of
c
c                  c(k)*exp(i*k*(j-1)*2*pi/n)
c
c             where
c
c                  c(k) = .5*cmplx(a(k),-b(k))   for k=1,...,kmax
c
c                  c(-k) = conjg(c(k))
c
c                  c(0) = azero
c
c                       and i=sqrt(-1)
c
c     *************** amplitude - phase notation ***********************
c
c             for i=1,...,n
c
c             r(i) equals azero plus the sum from k=1 to k=kmax of
c
c                  alpha(k)*cos(k*(i-1)*2*pi/n+beta(k))
c
c             where
c
c                  alpha(k) = sqrt(a(k)*a(k)+b(k)*b(k))
c
c                  cos(beta(k))=a(k)/alpha(k)
c
c                  sin(beta(k))=-b(k)/alpha(k)
c
      SUBROUTINE DEZFFTB(N,R,AZERO,A,B,WSAVE)
      DOUBLE PRECISION R
      DOUBLE PRECISION AZERO
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION WSAVE
      DIMENSION R(*),A(*),B(*),WSAVE(*)
c
      IF (N-2.LE.0) THEN
          IF (N-2.NE.0) THEN
              R(1) = AZERO
              RETURN
          END IF
          R(1) = AZERO + A(1)
          R(2) = AZERO - A(1)
          RETURN
      END IF
      NS2 = (N-1)/2
      DO I = 1,NS2
          R(2*I) = .5D0*A(I)
          R(2*I+1) = -.5D0*B(I)
      END DO
      R(1) = AZERO
      IF (MOD(N,2).EQ.0) R(N) = A(NS2+1)
      CALL DRFFTB(N,R,WSAVE(N+1))
      RETURN
      END
c
c =====================================================================
c     subroutine rfftb(n,r,wsave)
c
c     subroutine rfftb computes the real perodic sequence from its
c     fourier coefficients (fourier synthesis). the transform is defined
c     below at output parameter r.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is most efficient when n is a product of small primes.
c             n may change so long as different work arrays are provided
c
c     r       a real array of length n which contains the sequence
c             to be transformed
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls rfftb. the wsave array must be
c             initialized by calling subroutine rffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by rfftf and rfftb.
c
c
c     output parameters
c
c     r       for n even and for i = 1,...,n
c
c                  r(i) = r(1)+(-1)**(i-1)*r(n)
c
c                       plus the sum from k=2 to k=n/2 of
c
c                        2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                       -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c             for n odd and for i = 1,...,n
c
c                  r(i) = r(1) plus the sum from k=2 to k=(n+1)/2 of
c
c                       2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                      -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c      *****  note
c                  this transform is unnormalized since a call of rfftf
c                  followed by a call of rfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of rfftb or rfftf.
c
c
      SUBROUTINE DRFFTB(N,R,WSAVE)
      DOUBLE PRECISION R
      DOUBLE PRECISION WSAVE
      DIMENSION R(*),WSAVE(*)
c
      IF (N.EQ.1) RETURN
      CALL DRFFTB1OLD(N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
c
c =====================================================================
      SUBROUTINE DRFFTB1OLD(N,C,CH,WA,IFAC)
      DOUBLE PRECISION C
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA
      DIMENSION CH(*),C(*),WA(*),IFAC(*)

      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO K1 = 1,NF
          IP = IFAC(K1+2)
          L2 = IP*L1
          IDO = N/L2
          IDL1 = IDO*L1
          IF (IP.EQ.4) THEN
              IX2 = IW + IDO
              IX3 = IX2 + IDO
              IF (NA.EQ.0) THEN
                  CALL DRADB4(IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
              ELSE
                  CALL DRADB4(IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
              END IF
              NA = 1 - NA
          ELSE
              IF (IP.EQ.2) THEN
                  IF (NA.EQ.0) THEN
                      CALL DRADB2(IDO,L1,C,CH,WA(IW))
                  ELSE
                      CALL DRADB2(IDO,L1,CH,C,WA(IW))
                  END IF
                  NA = 1 - NA
              ELSE
                  IF (IP.EQ.3) THEN
                      IX2 = IW + IDO
                      IF (NA.EQ.0) THEN
                          CALL DRADB3(IDO,L1,C,CH,WA(IW),WA(IX2))
                      ELSE
                          CALL DRADB3(IDO,L1,CH,C,WA(IW),WA(IX2))
                      END IF
                      NA = 1 - NA
                  ELSE
                      IF (IP.EQ.5) THEN
                          IX2 = IW + IDO
                          IX3 = IX2 + IDO
                          IX4 = IX3 + IDO
                          IF (NA.EQ.0) THEN
                              CALL DRADB5(IDO,L1,C,CH,WA(IW),WA(IX2),
     +                                   WA(IX3),WA(IX4))
                          ELSE
                              CALL DRADB5(IDO,L1,CH,C,WA(IW),WA(IX2),
     +                                   WA(IX3),WA(IX4))
                          END IF
                          NA = 1 - NA
                      ELSE
                          IF (NA.EQ.0) THEN
                              CALL DRADBG(IDO,IP,L1,IDL1,C,C,C,CH,CH,
     +                                   WA(IW))
                          ELSE
                              CALL DRADBG(IDO,IP,L1,IDL1,CH,CH,CH,C,C,
     +                                   WA(IW))
                          END IF
                          IF (IDO.EQ.1) NA = 1 - NA
                      END IF
                  END IF
              END IF
          END IF
          L1 = L2
          IW = IW + (IP-1)*IDO
      END DO
      IF (NA.EQ.0) RETURN
      DO I = 1,N
          C(I) = CH(I)
      END DO
      RETURN
      END
c ==============================================================
c     subroutine ezffti(n,wsave)
c
c     subroutine ezffti initializes the array wsave which is used in
c     both ezfftf and ezfftb. the prime factorization of n together with
c     a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             the same work array can be used for both ezfftf and ezfftb
c             as long as n remains unchanged. different wsave arrays
c             are required for different values of n.
c
      SUBROUTINE DEZFFTI(N,WSAVE)
      DOUBLE PRECISION WSAVE
      DIMENSION WSAVE(*)
c
      IF (N.EQ.1) RETURN
      CALL DEZFFT1(N,WSAVE(2*N+1),WSAVE(3*N+1))
      RETURN
      END
c
      SUBROUTINE DEZFFT1(N,WA,IFAC)
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION ARGH
      DOUBLE PRECISION ARG1
      DOUBLE PRECISION CH1
      DOUBLE PRECISION SH1
      DOUBLE PRECISION DCH1
      DOUBLE PRECISION DSH1
      DOUBLE PRECISION CH1H
      DIMENSION WA(*),IFAC(*),NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/

      TPI = 8.0D0*ATAN(1.0D0)
      NL = N
      NF = 0
      J = 0
  101 CONTINUE
      J = J + 1
      IF (J-4.LE.0) THEN
          NTRY = NTRYH(J)
      ELSE
          NTRY = NTRY + 2
      END IF
  104 CONTINUE
      NQ = NL/NTRY
      NR = NL - NTRY*NQ
      IF (NR.NE.0) GO TO 101
      NF = NF + 1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY.EQ.2) THEN
          IF (NF.NE.1) THEN
              DO I = 2,NF
                  IB = NF - I + 2
                  IFAC(IB+2) = IFAC(IB+1)
              END DO
              IFAC(3) = 2
          END IF
      END IF
      IF (NL.NE.1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      ARGH = TPI/DBLE(N)
      IS = 0
      NFM1 = NF - 1
      L1 = 1
      IF (NFM1.EQ.0) RETURN
      DO K1 = 1,NFM1
          IP = IFAC(K1+2)
          L2 = L1*IP
          IDO = N/L2
          IPM = IP - 1
          ARG1 = DBLE(L1)*ARGH
          CH1 = 1.D0
          SH1 = 0.D0
          DCH1 = COS(ARG1)
          DSH1 = SIN(ARG1)
          DO J = 1,IPM
              CH1H = DCH1*CH1 - DSH1*SH1
              SH1 = DCH1*SH1 + DSH1*CH1
              CH1 = CH1H
              I = IS + 2
              WA(I-1) = CH1
              WA(I) = SH1
              IF (IDO.GE.5) THEN
                  DO II = 5,IDO,2
                      I = I + 2
                      WA(I-1) = CH1*WA(I-3) - SH1*WA(I-2)
                      WA(I) = CH1*WA(I-2) + SH1*WA(I-3)
                  END DO
              END IF
              IS = IS + IDO
          END DO
          L1 = L2
      END DO
      RETURN
      END
c ==========================================================
c     subroutine rffti(n,wsave)
c
c     subroutine rffti initializes the array wsave which is used in
c     both rfftf and rfftb. the prime factorization of n together with
c     a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             the same work array can be used for both rfftf and rfftb
c             as long as n remains unchanged. different wsave arrays
c             are required for different values of n. the contents of
c             wsave must not be changed between calls of rfftf or rfftb.
c
      SUBROUTINE DRFFTI(N,WSAVE)
      DOUBLE PRECISION WSAVE
      DIMENSION WSAVE(*)
c
      IF (N.EQ.1) RETURN
      CALL DRFFTI1OLD(N,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
c
c ==========================================================
c      real function pimach (dum)
c     pi=3.1415926535897932384626433832795028841971693993751058209749446
c
c      pimach = 4.*atan(1.0)
c      return
c      end
c
      SUBROUTINE DRFFTI1OLD(N,WA,IFAC)
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION DUM
      DOUBLE PRECISION ARGH
      DOUBLE PRECISION ARGLD
      DOUBLE PRECISION FI
      DOUBLE PRECISION ARG
      DIMENSION WA(*),IFAC(*),NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/

      NL = N
      NF = 0
      J = 0
  101 CONTINUE
      J = J + 1
      IF (J-4.LE.0) THEN
          NTRY = NTRYH(J)
      ELSE
          NTRY = NTRY + 2
      END IF
  104 CONTINUE
      NQ = NL/NTRY
      NR = NL - NTRY*NQ
      IF (NR.NE.0) GO TO 101
      NF = NF + 1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY.EQ.2) THEN
          IF (NF.NE.1) THEN
              DO I = 2,NF
                  IB = NF - I + 2
                  IFAC(IB+2) = IFAC(IB+1)
              END DO
              IFAC(3) = 2
          END IF
      END IF
      IF (NL.NE.1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      TPI = 2.0D0*DPIMACH(DUM)
      ARGH = TPI/DBLE(N)
      IS = 0
      NFM1 = NF - 1
      L1 = 1
      IF (NFM1.EQ.0) RETURN
      DO K1 = 1,NFM1
          IP = IFAC(K1+2)
          LD = 0
          L2 = L1*IP
          IDO = N/L2
          IPM = IP - 1
          DO J = 1,IPM
              LD = LD + L1
              I = IS
              ARGLD = DBLE(LD)*ARGH
              FI = 0.D0
              DO II = 3,IDO,2
                  I = I + 2
                  FI = FI + 1.D0
                  ARG = FI*ARGLD
                  WA(I-1) = COS(ARG)
                  WA(I) = SIN(ARG)
              END DO
              IS = IS + IDO
          END DO
          L1 = L2
      END DO
      RETURN
      END
c
      SUBROUTINE DRADF2(IDO,L1,CC,CH,WA1)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TI2
      DIMENSION CH(IDO,2,L1),CC(IDO,L1,2),WA1(*)

      DO K = 1,L1
          CH(1,1,K) = CC(1,K,1) + CC(1,K,2)
          CH(IDO,2,K) = CC(1,K,1) - CC(1,K,2)
      END DO
      IF (IDO-2.GE.0) THEN
          IF (IDO-2.NE.0) THEN
              IDP2 = IDO + 2
              DO K = 1,L1
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      TR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
                      TI2 = WA1(I-2)*CC(I,K,2) - WA1(I-1)*CC(I-1,K,2)
                      CH(I,1,K) = CC(I,K,1) + TI2
                      CH(IC,2,K) = TI2 - CC(I,K,1)
                      CH(I-1,1,K) = CC(I-1,K,1) + TR2
                      CH(IC-1,2,K) = CC(I-1,K,1) - TR2
                  END DO
              END DO
              IF (MOD(IDO,2).EQ.1) RETURN
          END IF
          DO K = 1,L1
              CH(1,2,K) = -CC(IDO,K,2)
              CH(IDO,1,K) = CC(IDO,K,1)
          END DO
      END IF
      RETURN
      END
c
      SUBROUTINE DRADF3(IDO,L1,CC,CH,WA1,WA2)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION TAUR
      DOUBLE PRECISION TAUI
      DOUBLE PRECISION CR2
      DOUBLE PRECISION DR2
      DOUBLE PRECISION DI2
      DOUBLE PRECISION DR3
      DOUBLE PRECISION DI3
      DOUBLE PRECISION CI2
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TR3
      DOUBLE PRECISION TI3
      DIMENSION CH(IDO,3,L1),CC(IDO,L1,3),WA1(*),WA2(*)
      DATA TAUR,TAUI/-.5D0,.866025403784439D0/

      DO K = 1,L1
          CR2 = CC(1,K,2) + CC(1,K,3)
          CH(1,1,K) = CC(1,K,1) + CR2
          CH(1,3,K) = TAUI* (CC(1,K,3)-CC(1,K,2))
          CH(IDO,2,K) = CC(1,K,1) + TAUR*CR2
      END DO
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO K = 1,L1
          DO I = 3,IDO,2
              IC = IDP2 - I
              DR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
              DI2 = WA1(I-2)*CC(I,K,2) - WA1(I-1)*CC(I-1,K,2)
              DR3 = WA2(I-2)*CC(I-1,K,3) + WA2(I-1)*CC(I,K,3)
              DI3 = WA2(I-2)*CC(I,K,3) - WA2(I-1)*CC(I-1,K,3)
              CR2 = DR2 + DR3
              CI2 = DI2 + DI3
              CH(I-1,1,K) = CC(I-1,K,1) + CR2
              CH(I,1,K) = CC(I,K,1) + CI2
              TR2 = CC(I-1,K,1) + TAUR*CR2
              TI2 = CC(I,K,1) + TAUR*CI2
              TR3 = TAUI* (DI2-DI3)
              TI3 = TAUI* (DR3-DR2)
              CH(I-1,3,K) = TR2 + TR3
              CH(IC-1,2,K) = TR2 - TR3
              CH(I,3,K) = TI2 + TI3
              CH(IC,2,K) = TI3 - TI2
          END DO
      END DO
      RETURN
      END
c
      SUBROUTINE DRADF4(IDO,L1,CC,CH,WA1,WA2,WA3)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION HSQT2
      DOUBLE PRECISION TR1
      DOUBLE PRECISION TR2
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CR3
      DOUBLE PRECISION CI3
      DOUBLE PRECISION CR4
      DOUBLE PRECISION CI4
      DOUBLE PRECISION TR4
      DOUBLE PRECISION TI1
      DOUBLE PRECISION TI4
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TI3
      DOUBLE PRECISION TR3
      DIMENSION CC(IDO,L1,4),CH(IDO,4,L1),WA1(*),WA2(*),WA3(*)
      DATA HSQT2/.7071067811865475D0/

      DO K = 1,L1
          TR1 = CC(1,K,2) + CC(1,K,4)
          TR2 = CC(1,K,1) + CC(1,K,3)
          CH(1,1,K) = TR1 + TR2
          CH(IDO,4,K) = TR2 - TR1
          CH(IDO,2,K) = CC(1,K,1) - CC(1,K,3)
          CH(1,3,K) = CC(1,K,4) - CC(1,K,2)
      END DO
      IF (IDO-2.GE.0) THEN
          IF (IDO-2.NE.0) THEN
              IDP2 = IDO + 2
              DO K = 1,L1
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      CR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
                      CI2 = WA1(I-2)*CC(I,K,2) - WA1(I-1)*CC(I-1,K,2)
                      CR3 = WA2(I-2)*CC(I-1,K,3) + WA2(I-1)*CC(I,K,3)
                      CI3 = WA2(I-2)*CC(I,K,3) - WA2(I-1)*CC(I-1,K,3)
                      CR4 = WA3(I-2)*CC(I-1,K,4) + WA3(I-1)*CC(I,K,4)
                      CI4 = WA3(I-2)*CC(I,K,4) - WA3(I-1)*CC(I-1,K,4)
                      TR1 = CR2 + CR4
                      TR4 = CR4 - CR2
                      TI1 = CI2 + CI4
                      TI4 = CI2 - CI4
                      TI2 = CC(I,K,1) + CI3
                      TI3 = CC(I,K,1) - CI3
                      TR2 = CC(I-1,K,1) + CR3
                      TR3 = CC(I-1,K,1) - CR3
                      CH(I-1,1,K) = TR1 + TR2
                      CH(IC-1,4,K) = TR2 - TR1
                      CH(I,1,K) = TI1 + TI2
                      CH(IC,4,K) = TI1 - TI2
                      CH(I-1,3,K) = TI4 + TR3
                      CH(IC-1,2,K) = TR3 - TI4
                      CH(I,3,K) = TR4 + TI3
                      CH(IC,2,K) = TR4 - TI3
                  END DO
              END DO
              IF (MOD(IDO,2).EQ.1) RETURN
          END IF
          DO K = 1,L1
              TI1 = -HSQT2* (CC(IDO,K,2)+CC(IDO,K,4))
              TR1 = HSQT2* (CC(IDO,K,2)-CC(IDO,K,4))
              CH(IDO,1,K) = TR1 + CC(IDO,K,1)
              CH(IDO,3,K) = CC(IDO,K,1) - TR1
              CH(1,2,K) = TI1 - CC(IDO,K,3)
              CH(1,4,K) = TI1 + CC(IDO,K,3)
          END DO
      END IF
      RETURN
      END
c
      SUBROUTINE DRADF5(IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION WA4
      DOUBLE PRECISION TR11
      DOUBLE PRECISION TI11
      DOUBLE PRECISION TR12
      DOUBLE PRECISION TI12
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CI5
      DOUBLE PRECISION CR3
      DOUBLE PRECISION CI4
      DOUBLE PRECISION DR2
      DOUBLE PRECISION DI2
      DOUBLE PRECISION DR3
      DOUBLE PRECISION DI3
      DOUBLE PRECISION DR4
      DOUBLE PRECISION DI4
      DOUBLE PRECISION DR5
      DOUBLE PRECISION DI5
      DOUBLE PRECISION CR5
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CR4
      DOUBLE PRECISION CI3
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TR3
      DOUBLE PRECISION TI3
      DOUBLE PRECISION TR5
      DOUBLE PRECISION TI5
      DOUBLE PRECISION TR4
      DOUBLE PRECISION TI4
      DIMENSION CC(IDO,L1,5),CH(IDO,5,L1),WA1(*),WA2(*),WA3(*),WA4(*)
      DATA TR11,TI11,TR12,TI12/.309016994374947D0,.951056516295154D0,
     +     -.809016994374947D0,.587785252292473D0/

      DO K = 1,L1
          CR2 = CC(1,K,5) + CC(1,K,2)
          CI5 = CC(1,K,5) - CC(1,K,2)
          CR3 = CC(1,K,4) + CC(1,K,3)
          CI4 = CC(1,K,4) - CC(1,K,3)
          CH(1,1,K) = CC(1,K,1) + CR2 + CR3
          CH(IDO,2,K) = CC(1,K,1) + TR11*CR2 + TR12*CR3
          CH(1,3,K) = TI11*CI5 + TI12*CI4
          CH(IDO,4,K) = CC(1,K,1) + TR12*CR2 + TR11*CR3
          CH(1,5,K) = TI12*CI5 - TI11*CI4
      END DO
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO K = 1,L1
          DO I = 3,IDO,2
              IC = IDP2 - I
              DR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
              DI2 = WA1(I-2)*CC(I,K,2) - WA1(I-1)*CC(I-1,K,2)
              DR3 = WA2(I-2)*CC(I-1,K,3) + WA2(I-1)*CC(I,K,3)
              DI3 = WA2(I-2)*CC(I,K,3) - WA2(I-1)*CC(I-1,K,3)
              DR4 = WA3(I-2)*CC(I-1,K,4) + WA3(I-1)*CC(I,K,4)
              DI4 = WA3(I-2)*CC(I,K,4) - WA3(I-1)*CC(I-1,K,4)
              DR5 = WA4(I-2)*CC(I-1,K,5) + WA4(I-1)*CC(I,K,5)
              DI5 = WA4(I-2)*CC(I,K,5) - WA4(I-1)*CC(I-1,K,5)
              CR2 = DR2 + DR5
              CI5 = DR5 - DR2
              CR5 = DI2 - DI5
              CI2 = DI2 + DI5
              CR3 = DR3 + DR4
              CI4 = DR4 - DR3
              CR4 = DI3 - DI4
              CI3 = DI3 + DI4
              CH(I-1,1,K) = CC(I-1,K,1) + CR2 + CR3
              CH(I,1,K) = CC(I,K,1) + CI2 + CI3
              TR2 = CC(I-1,K,1) + TR11*CR2 + TR12*CR3
              TI2 = CC(I,K,1) + TR11*CI2 + TR12*CI3
              TR3 = CC(I-1,K,1) + TR12*CR2 + TR11*CR3
              TI3 = CC(I,K,1) + TR12*CI2 + TR11*CI3
              TR5 = TI11*CR5 + TI12*CR4
              TI5 = TI11*CI5 + TI12*CI4
              TR4 = TI12*CR5 - TI11*CR4
              TI4 = TI12*CI5 - TI11*CI4
              CH(I-1,3,K) = TR2 + TR5
              CH(IC-1,2,K) = TR2 - TR5
              CH(I,3,K) = TI2 + TI5
              CH(IC,2,K) = TI5 - TI2
              CH(I-1,5,K) = TR3 + TR4
              CH(IC-1,4,K) = TR3 - TR4
              CH(I,5,K) = TI3 + TI4
              CH(IC,4,K) = TI4 - TI3
          END DO
      END DO
      RETURN
      END
c
      SUBROUTINE DRADFG(IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DOUBLE PRECISION CC
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION CH
      DOUBLE PRECISION CH2
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION DUM
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DCP
      DOUBLE PRECISION DSP
      DOUBLE PRECISION AR1
      DOUBLE PRECISION AI1
      DOUBLE PRECISION AR1H
      DOUBLE PRECISION DC2
      DOUBLE PRECISION DS2
      DOUBLE PRECISION AR2
      DOUBLE PRECISION AI2
      DOUBLE PRECISION AR2H
      DIMENSION CH(IDO,L1,IP),CC(IDO,IP,L1),C1(IDO,L1,IP),C2(IDL1,IP),
     +          CH2(IDL1,IP),WA(*)

      TPI = 2.0D0*DPIMACH(DUM)
      ARG = TPI/DBLE(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP + 2
      IDP2 = IDO + 2
      NBD = (IDO-1)/2
      IF (IDO.NE.1) THEN
          DO IK = 1,IDL1
              CH2(IK,1) = C2(IK,1)
          END DO
          DO J = 2,IP
              DO K = 1,L1
                  CH(1,K,J) = C1(1,K,J)
              END DO
          END DO
          IF (NBD.LE.L1) THEN
              IS = -IDO
              DO J = 2,IP
                  IS = IS + IDO
                  IDIJ = IS
                  DO I = 3,IDO,2
                      IDIJ = IDIJ + 2
                      DO K = 1,L1
                          CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J) +
     +                                  WA(IDIJ)*C1(I,K,J)
                          CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J) -
     +                                WA(IDIJ)*C1(I-1,K,J)
                      END DO
                  END DO
              END DO
          ELSE
              IS = -IDO
              DO J = 2,IP
                  IS = IS + IDO
                  DO K = 1,L1
                      IDIJ = IS
                      DO I = 3,IDO,2
                          IDIJ = IDIJ + 2
                          CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J) +
     +                                  WA(IDIJ)*C1(I,K,J)
                          CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J) -
     +                                WA(IDIJ)*C1(I-1,K,J)
                      END DO
                  END DO
              END DO
          END IF
          IF (NBD.GE.L1) THEN
              DO J = 2,IPPH
                  JC = IPP2 - J
                  DO K = 1,L1
                      DO I = 3,IDO,2
                          C1(I-1,K,J) = CH(I-1,K,J) + CH(I-1,K,JC)
                          C1(I-1,K,JC) = CH(I,K,J) - CH(I,K,JC)
                          C1(I,K,J) = CH(I,K,J) + CH(I,K,JC)
                          C1(I,K,JC) = CH(I-1,K,JC) - CH(I-1,K,J)
                      END DO
                  END DO
              END DO
              GO TO 121
          END IF
          DO J = 2,IPPH
              JC = IPP2 - J
              DO I = 3,IDO,2
                  DO K = 1,L1
                      C1(I-1,K,J) = CH(I-1,K,J) + CH(I-1,K,JC)
                      C1(I-1,K,JC) = CH(I,K,J) - CH(I,K,JC)
                      C1(I,K,J) = CH(I,K,J) + CH(I,K,JC)
                      C1(I,K,JC) = CH(I-1,K,JC) - CH(I-1,K,J)
                  END DO
              END DO
          END DO
          GO TO 121
      END IF
      DO IK = 1,IDL1
          C2(IK,1) = CH2(IK,1)
      END DO
  121 CONTINUE
      DO J = 2,IPPH
          JC = IPP2 - J
          DO K = 1,L1
              C1(1,K,J) = CH(1,K,J) + CH(1,K,JC)
              C1(1,K,JC) = CH(1,K,JC) - CH(1,K,J)
          END DO
      END DO
c
      AR1 = 1.D0
      AI1 = 0.D0
      DO L = 2,IPPH
          LC = IPP2 - L
          AR1H = DCP*AR1 - DSP*AI1
          AI1 = DCP*AI1 + DSP*AR1
          AR1 = AR1H
          DO IK = 1,IDL1
              CH2(IK,L) = C2(IK,1) + AR1*C2(IK,2)
              CH2(IK,LC) = AI1*C2(IK,IP)
          END DO
          DC2 = AR1
          DS2 = AI1
          AR2 = AR1
          AI2 = AI1
          DO J = 3,IPPH
              JC = IPP2 - J
              AR2H = DC2*AR2 - DS2*AI2
              AI2 = DC2*AI2 + DS2*AR2
              AR2 = AR2H
              DO IK = 1,IDL1
                  CH2(IK,L) = CH2(IK,L) + AR2*C2(IK,J)
                  CH2(IK,LC) = CH2(IK,LC) + AI2*C2(IK,JC)
              END DO
          END DO
      END DO
      DO J = 2,IPPH
          DO IK = 1,IDL1
              CH2(IK,1) = CH2(IK,1) + C2(IK,J)
          END DO
      END DO
c
      IF (IDO.GE.L1) THEN
          DO K = 1,L1
              DO I = 1,IDO
                  CC(I,1,K) = CH(I,K,1)
              END DO
          END DO
      ELSE
          DO I = 1,IDO
              DO K = 1,L1
                  CC(I,1,K) = CH(I,K,1)
              END DO
          END DO
      END IF
      DO J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO K = 1,L1
              CC(IDO,J2-2,K) = CH(1,K,J)
              CC(1,J2-1,K) = CH(1,K,JC)
          END DO
      END DO
      IF (IDO.EQ.1) RETURN
      IF (NBD.GE.L1) THEN
          DO J = 2,IPPH
              JC = IPP2 - J
              J2 = J + J
              DO K = 1,L1
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      CC(I-1,J2-1,K) = CH(I-1,K,J) + CH(I-1,K,JC)
                      CC(IC-1,J2-2,K) = CH(I-1,K,J) - CH(I-1,K,JC)
                      CC(I,J2-1,K) = CH(I,K,J) + CH(I,K,JC)
                      CC(IC,J2-2,K) = CH(I,K,JC) - CH(I,K,J)
                  END DO
              END DO
          END DO
          RETURN
      END IF
      DO J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO I = 3,IDO,2
              IC = IDP2 - I
              DO K = 1,L1
                  CC(I-1,J2-1,K) = CH(I-1,K,J) + CH(I-1,K,JC)
                  CC(IC-1,J2-2,K) = CH(I-1,K,J) - CH(I-1,K,JC)
                  CC(I,J2-1,K) = CH(I,K,J) + CH(I,K,JC)
                  CC(IC,J2-2,K) = CH(I,K,JC) - CH(I,K,J)
              END DO
          END DO
      END DO
      RETURN
      END
c
      SUBROUTINE DRADB2(IDO,L1,CC,CH,WA1)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TI2
      DIMENSION CC(IDO,2,L1),CH(IDO,L1,2),WA1(*)

      DO K = 1,L1
          CH(1,K,1) = CC(1,1,K) + CC(IDO,2,K)
          CH(1,K,2) = CC(1,1,K) - CC(IDO,2,K)
      END DO
      IF (IDO-2.GE.0) THEN
          IF (IDO-2.NE.0) THEN
              IDP2 = IDO + 2
              DO K = 1,L1
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      CH(I-1,K,1) = CC(I-1,1,K) + CC(IC-1,2,K)
                      TR2 = CC(I-1,1,K) - CC(IC-1,2,K)
                      CH(I,K,1) = CC(I,1,K) - CC(IC,2,K)
                      TI2 = CC(I,1,K) + CC(IC,2,K)
                      CH(I-1,K,2) = WA1(I-2)*TR2 - WA1(I-1)*TI2
                      CH(I,K,2) = WA1(I-2)*TI2 + WA1(I-1)*TR2
                  END DO
              END DO
              IF (MOD(IDO,2).EQ.1) RETURN
          END IF
          DO K = 1,L1
              CH(IDO,K,1) = CC(IDO,1,K) + CC(IDO,1,K)
              CH(IDO,K,2) = - (CC(1,2,K)+CC(1,2,K))
          END DO
      END IF
      RETURN
      END
c
      SUBROUTINE DRADB3(IDO,L1,CC,CH,WA1,WA2)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION TAUR
      DOUBLE PRECISION TAUI
      DOUBLE PRECISION TR2
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CI3
      DOUBLE PRECISION TI2
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CR3
      DOUBLE PRECISION DR2
      DOUBLE PRECISION DR3
      DOUBLE PRECISION DI2
      DOUBLE PRECISION DI3
      DIMENSION CC(IDO,3,L1),CH(IDO,L1,3),WA1(*),WA2(*)
      DATA TAUR,TAUI/-.5D0,.866025403784439D0/

      DO K = 1,L1
          TR2 = CC(IDO,2,K) + CC(IDO,2,K)
          CR2 = CC(1,1,K) + TAUR*TR2
          CH(1,K,1) = CC(1,1,K) + TR2
          CI3 = TAUI* (CC(1,3,K)+CC(1,3,K))
          CH(1,K,2) = CR2 - CI3
          CH(1,K,3) = CR2 + CI3
      END DO
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO K = 1,L1
          DO I = 3,IDO,2
              IC = IDP2 - I
              TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
              CR2 = CC(I-1,1,K) + TAUR*TR2
              CH(I-1,K,1) = CC(I-1,1,K) + TR2
              TI2 = CC(I,3,K) - CC(IC,2,K)
              CI2 = CC(I,1,K) + TAUR*TI2
              CH(I,K,1) = CC(I,1,K) + TI2
              CR3 = TAUI* (CC(I-1,3,K)-CC(IC-1,2,K))
              CI3 = TAUI* (CC(I,3,K)+CC(IC,2,K))
              DR2 = CR2 - CI3
              DR3 = CR2 + CI3
              DI2 = CI2 + CR3
              DI3 = CI2 - CR3
              CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
              CH(I,K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
              CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
              CH(I,K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
          END DO
      END DO
      RETURN
      END
c
      SUBROUTINE DRADB4(IDO,L1,CC,CH,WA1,WA2,WA3)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION SQRT2
      DOUBLE PRECISION TR1
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TR3
      DOUBLE PRECISION TR4
      DOUBLE PRECISION TI1
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TI3
      DOUBLE PRECISION TI4
      DOUBLE PRECISION CR3
      DOUBLE PRECISION CI3
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CR4
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CI4
      DIMENSION CC(IDO,4,L1),CH(IDO,L1,4),WA1(*),WA2(*),WA3(*)
      DATA SQRT2/1.414213562373095D0/

      DO K = 1,L1
          TR1 = CC(1,1,K) - CC(IDO,4,K)
          TR2 = CC(1,1,K) + CC(IDO,4,K)
          TR3 = CC(IDO,2,K) + CC(IDO,2,K)
          TR4 = CC(1,3,K) + CC(1,3,K)
          CH(1,K,1) = TR2 + TR3
          CH(1,K,2) = TR1 - TR4
          CH(1,K,3) = TR2 - TR3
          CH(1,K,4) = TR1 + TR4
      END DO
      IF (IDO-2.GE.0) THEN
          IF (IDO-2.NE.0) THEN
              IDP2 = IDO + 2
              DO K = 1,L1
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      TI1 = CC(I,1,K) + CC(IC,4,K)
                      TI2 = CC(I,1,K) - CC(IC,4,K)
                      TI3 = CC(I,3,K) - CC(IC,2,K)
                      TR4 = CC(I,3,K) + CC(IC,2,K)
                      TR1 = CC(I-1,1,K) - CC(IC-1,4,K)
                      TR2 = CC(I-1,1,K) + CC(IC-1,4,K)
                      TI4 = CC(I-1,3,K) - CC(IC-1,2,K)
                      TR3 = CC(I-1,3,K) + CC(IC-1,2,K)
                      CH(I-1,K,1) = TR2 + TR3
                      CR3 = TR2 - TR3
                      CH(I,K,1) = TI2 + TI3
                      CI3 = TI2 - TI3
                      CR2 = TR1 - TR4
                      CR4 = TR1 + TR4
                      CI2 = TI1 + TI4
                      CI4 = TI1 - TI4
                      CH(I-1,K,2) = WA1(I-2)*CR2 - WA1(I-1)*CI2
                      CH(I,K,2) = WA1(I-2)*CI2 + WA1(I-1)*CR2
                      CH(I-1,K,3) = WA2(I-2)*CR3 - WA2(I-1)*CI3
                      CH(I,K,3) = WA2(I-2)*CI3 + WA2(I-1)*CR3
                      CH(I-1,K,4) = WA3(I-2)*CR4 - WA3(I-1)*CI4
                      CH(I,K,4) = WA3(I-2)*CI4 + WA3(I-1)*CR4
                  END DO
              END DO
              IF (MOD(IDO,2).EQ.1) RETURN
          END IF
          DO K = 1,L1
              TI1 = CC(1,2,K) + CC(1,4,K)
              TI2 = CC(1,4,K) - CC(1,2,K)
              TR1 = CC(IDO,1,K) - CC(IDO,3,K)
              TR2 = CC(IDO,1,K) + CC(IDO,3,K)
              CH(IDO,K,1) = TR2 + TR2
              CH(IDO,K,2) = SQRT2* (TR1-TI1)
              CH(IDO,K,3) = TI2 + TI2
              CH(IDO,K,4) = -SQRT2* (TR1+TI1)
          END DO
      END IF
      RETURN
      END
c
      SUBROUTINE DRADB5(IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION WA4
      DOUBLE PRECISION TR11
      DOUBLE PRECISION TI11
      DOUBLE PRECISION TR12
      DOUBLE PRECISION TI12
      DOUBLE PRECISION TI5
      DOUBLE PRECISION TI4
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TR3
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CR3
      DOUBLE PRECISION CI5
      DOUBLE PRECISION CI4
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TI3
      DOUBLE PRECISION TR5
      DOUBLE PRECISION TR4
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CI3
      DOUBLE PRECISION CR5
      DOUBLE PRECISION CR4
      DOUBLE PRECISION DR3
      DOUBLE PRECISION DR4
      DOUBLE PRECISION DI3
      DOUBLE PRECISION DI4
      DOUBLE PRECISION DR5
      DOUBLE PRECISION DR2
      DOUBLE PRECISION DI5
      DOUBLE PRECISION DI2
      DIMENSION CC(IDO,5,L1),CH(IDO,L1,5),WA1(*),WA2(*),WA3(*),WA4(*)
      DATA TR11,TI11,TR12,TI12/.309016994374947D0,.951056516295154D0,
     +     -.809016994374947D0,.587785252292473D0/

      DO K = 1,L1
          TI5 = CC(1,3,K) + CC(1,3,K)
          TI4 = CC(1,5,K) + CC(1,5,K)
          TR2 = CC(IDO,2,K) + CC(IDO,2,K)
          TR3 = CC(IDO,4,K) + CC(IDO,4,K)
          CH(1,K,1) = CC(1,1,K) + TR2 + TR3
          CR2 = CC(1,1,K) + TR11*TR2 + TR12*TR3
          CR3 = CC(1,1,K) + TR12*TR2 + TR11*TR3
          CI5 = TI11*TI5 + TI12*TI4
          CI4 = TI12*TI5 - TI11*TI4
          CH(1,K,2) = CR2 - CI5
          CH(1,K,3) = CR3 - CI4
          CH(1,K,4) = CR3 + CI4
          CH(1,K,5) = CR2 + CI5
      END DO
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO K = 1,L1
          DO I = 3,IDO,2
              IC = IDP2 - I
              TI5 = CC(I,3,K) + CC(IC,2,K)
              TI2 = CC(I,3,K) - CC(IC,2,K)
              TI4 = CC(I,5,K) + CC(IC,4,K)
              TI3 = CC(I,5,K) - CC(IC,4,K)
              TR5 = CC(I-1,3,K) - CC(IC-1,2,K)
              TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
              TR4 = CC(I-1,5,K) - CC(IC-1,4,K)
              TR3 = CC(I-1,5,K) + CC(IC-1,4,K)
              CH(I-1,K,1) = CC(I-1,1,K) + TR2 + TR3
              CH(I,K,1) = CC(I,1,K) + TI2 + TI3
              CR2 = CC(I-1,1,K) + TR11*TR2 + TR12*TR3
              CI2 = CC(I,1,K) + TR11*TI2 + TR12*TI3
              CR3 = CC(I-1,1,K) + TR12*TR2 + TR11*TR3
              CI3 = CC(I,1,K) + TR12*TI2 + TR11*TI3
              CR5 = TI11*TR5 + TI12*TR4
              CI5 = TI11*TI5 + TI12*TI4
              CR4 = TI12*TR5 - TI11*TR4
              CI4 = TI12*TI5 - TI11*TI4
              DR3 = CR3 - CI4
              DR4 = CR3 + CI4
              DI3 = CI3 + CR4
              DI4 = CI3 - CR4
              DR5 = CR2 + CI5
              DR2 = CR2 - CI5
              DI5 = CI2 - CR5
              DI2 = CI2 + CR5
              CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
              CH(I,K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
              CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
              CH(I,K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
              CH(I-1,K,4) = WA3(I-2)*DR4 - WA3(I-1)*DI4
              CH(I,K,4) = WA3(I-2)*DI4 + WA3(I-1)*DR4
              CH(I-1,K,5) = WA4(I-2)*DR5 - WA4(I-1)*DI5
              CH(I,K,5) = WA4(I-2)*DI5 + WA4(I-1)*DR5
          END DO
      END DO
      RETURN
      END
c
      SUBROUTINE DRADBG(IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DOUBLE PRECISION CC
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION CH
      DOUBLE PRECISION CH2
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION DUM
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DCP
      DOUBLE PRECISION DSP
      DOUBLE PRECISION AR1
      DOUBLE PRECISION AI1
      DOUBLE PRECISION AR1H
      DOUBLE PRECISION DC2
      DOUBLE PRECISION DS2
      DOUBLE PRECISION AR2
      DOUBLE PRECISION AI2
      DOUBLE PRECISION AR2H
      DIMENSION CH(IDO,L1,IP),CC(IDO,IP,L1),C1(IDO,L1,IP),C2(IDL1,IP),
     +          CH2(IDL1,IP),WA(*)

      TPI = 2.0D0*DPIMACH(DUM)
      ARG = TPI/DBLE(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO + 2
      NBD = (IDO-1)/2
      IPP2 = IP + 2
      IPPH = (IP+1)/2
      IF (IDO.GE.L1) THEN
          DO K = 1,L1
              DO I = 1,IDO
                  CH(I,K,1) = CC(I,1,K)
              END DO
          END DO
      ELSE
          DO I = 1,IDO
              DO K = 1,L1
                  CH(I,K,1) = CC(I,1,K)
              END DO
          END DO
      END IF
      DO J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO K = 1,L1
              CH(1,K,J) = CC(IDO,J2-2,K) + CC(IDO,J2-2,K)
              CH(1,K,JC) = CC(1,J2-1,K) + CC(1,J2-1,K)
          END DO
      END DO
      IF (IDO.NE.1) THEN
          IF (NBD.GE.L1) THEN
              DO J = 2,IPPH
                  JC = IPP2 - J
                  DO K = 1,L1
                      DO I = 3,IDO,2
                          IC = IDP2 - I
                          CH(I-1,K,J) = CC(I-1,2*J-1,K) +
     +                                  CC(IC-1,2*J-2,K)
                          CH(I-1,K,JC) = CC(I-1,2*J-1,K) -
     +                                   CC(IC-1,2*J-2,K)
                          CH(I,K,J) = CC(I,2*J-1,K) - CC(IC,2*J-2,K)
                          CH(I,K,JC) = CC(I,2*J-1,K) + CC(IC,2*J-2,K)
                      END DO
                  END DO
              END DO
          ELSE
              DO J = 2,IPPH
                  JC = IPP2 - J
                  DO I = 3,IDO,2
                      IC = IDP2 - I
                      DO K = 1,L1
                          CH(I-1,K,J) = CC(I-1,2*J-1,K) +
     +                                  CC(IC-1,2*J-2,K)
                          CH(I-1,K,JC) = CC(I-1,2*J-1,K) -
     +                                   CC(IC-1,2*J-2,K)
                          CH(I,K,J) = CC(I,2*J-1,K) - CC(IC,2*J-2,K)
                          CH(I,K,JC) = CC(I,2*J-1,K) + CC(IC,2*J-2,K)
                      END DO
                  END DO
              END DO
          END IF
      END IF
      AR1 = 1.D0
      AI1 = 0.D0
      DO L = 2,IPPH
          LC = IPP2 - L
          AR1H = DCP*AR1 - DSP*AI1
          AI1 = DCP*AI1 + DSP*AR1
          AR1 = AR1H
          DO IK = 1,IDL1
              C2(IK,L) = CH2(IK,1) + AR1*CH2(IK,2)
              C2(IK,LC) = AI1*CH2(IK,IP)
          END DO
          DC2 = AR1
          DS2 = AI1
          AR2 = AR1
          AI2 = AI1
          DO J = 3,IPPH
              JC = IPP2 - J
              AR2H = DC2*AR2 - DS2*AI2
              AI2 = DC2*AI2 + DS2*AR2
              AR2 = AR2H
              DO IK = 1,IDL1
                  C2(IK,L) = C2(IK,L) + AR2*CH2(IK,J)
                  C2(IK,LC) = C2(IK,LC) + AI2*CH2(IK,JC)
              END DO
          END DO
      END DO
      DO J = 2,IPPH
          DO IK = 1,IDL1
              CH2(IK,1) = CH2(IK,1) + CH2(IK,J)
          END DO
      END DO
      DO J = 2,IPPH
          JC = IPP2 - J
          DO K = 1,L1
              CH(1,K,J) = C1(1,K,J) - C1(1,K,JC)
              CH(1,K,JC) = C1(1,K,J) + C1(1,K,JC)
          END DO
      END DO
      IF (IDO.NE.1) THEN
          IF (NBD.GE.L1) THEN
              DO J = 2,IPPH
                  JC = IPP2 - J
                  DO K = 1,L1
                      DO I = 3,IDO,2
                          CH(I-1,K,J) = C1(I-1,K,J) - C1(I,K,JC)
                          CH(I-1,K,JC) = C1(I-1,K,J) + C1(I,K,JC)
                          CH(I,K,J) = C1(I,K,J) + C1(I-1,K,JC)
                          CH(I,K,JC) = C1(I,K,J) - C1(I-1,K,JC)
                      END DO
                  END DO
              END DO
          ELSE
              DO J = 2,IPPH
                  JC = IPP2 - J
                  DO I = 3,IDO,2
                      DO K = 1,L1
                          CH(I-1,K,J) = C1(I-1,K,J) - C1(I,K,JC)
                          CH(I-1,K,JC) = C1(I-1,K,J) + C1(I,K,JC)
                          CH(I,K,J) = C1(I,K,J) + C1(I-1,K,JC)
                          CH(I,K,JC) = C1(I,K,J) - C1(I-1,K,JC)
                      END DO
                  END DO
              END DO
          END IF
      END IF
      IF (IDO.EQ.1) RETURN
      DO IK = 1,IDL1
          C2(IK,1) = CH2(IK,1)
      END DO
      DO J = 2,IP
          DO K = 1,L1
              C1(1,K,J) = CH(1,K,J)
          END DO
      END DO
      IF (NBD.LE.L1) THEN
          IS = -IDO
          DO J = 2,IP
              IS = IS + IDO
              IDIJ = IS
              DO I = 3,IDO,2
                  IDIJ = IDIJ + 2
                  DO K = 1,L1
                      C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J) -
     +                              WA(IDIJ)*CH(I,K,J)
                      C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J) +
     +                            WA(IDIJ)*CH(I-1,K,J)
                  END DO
              END DO
          END DO
      ELSE
          IS = -IDO
          DO J = 2,IP
              IS = IS + IDO
              DO K = 1,L1
                  IDIJ = IS
                  DO I = 3,IDO,2
                      IDIJ = IDIJ + 2
                      C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J) -
     +                              WA(IDIJ)*CH(I,K,J)
                      C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J) +
     +                            WA(IDIJ)*CH(I-1,K,J)
                  END DO
              END DO
          END DO
      END IF
      RETURN
      END
