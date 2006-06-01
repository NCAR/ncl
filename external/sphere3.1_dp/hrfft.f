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
c ... file hrfft.f
c
c     this file contains a multiple fft package for spherepack3.0.
c     it includes code and documentation for performing fast fourier
c     transforms (see subroutines hrffti,hrfftf and hrfftb)
c
c **********************************************************************
c
c     subroutine hrffti(n,wsave)
c
c     subroutine hrffti initializes the array wsave which is used in
c     both hrfftf and hrfftb. the prime factorization of n together
c     with a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             the same work array can be used for both hrfftf and
c             hrfftb as long as n remains unchanged. different wsave
c             arrays are required for different values of n. the
c             contents of wsave must not be changed between calls
c             of hrfftf or hrfftb.
c
c **********************************************************************
c
c     subroutine hrfftf(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftf computes the fourier coefficients of m real
c     perodic sequences (fourier analysis); i.e. hrfftf computes the
c     real fft of m sequences each with length n. the transform is
c     defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains m
c             sequences each with length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftf. mdimr must be
c             greater than or equal to m.
c
c
c     wsave   a work array with at least least 2*n+15 locations
c             in the program that calls hrfftf. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c
c             r(j,1) = the sum from i=1 to i=n of r(j,i)
c
c             if n is even set l =n/2   , if n is odd set l = (n+1)/2
c
c               then for k = 2,...,l
c
c                  r(j,2*k-2) = the sum from i = 1 to i = n of
c
c                       r(j,i)*cos((k-1)*(i-1)*2*pi/n)
c
c                  r(j,2*k-1) = the sum from i = 1 to i = n of
c
c                      -r(j,i)*sin((k-1)*(i-1)*2*pi/n)
c
c             if n is even
c
c                  r(j,n) = the sum from i = 1 to i = n of
c
c                       (-1)**(i-1)*r(j,i)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftf or hrfftb.
c
c     work    a real work array with m*n locations that does
c             not have to be saved.
c
c **********************************************************************
c
c     subroutine hrfftb(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftb computes the real perodic sequence of m
c     sequences from their fourier coefficients (fourier synthesis).
c     the transform is defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains
c             the fourier coefficients of m sequences each with
c             length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftb. mdimr must be
c             greater than or equal to m.
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls hrfftb. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c
c             for n even and for i = 1,...,n
c
c                  r(j,i) = r(j,1)+(-1)**(i-1)*r(j,n)
c
c                       plus the sum from k=2 to k=n/2 of
c
c                        2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                       -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c             for n odd and for i = 1,...,n
c
c                  r(j,i) = r(j,1) plus the sum from k=2 to k=(n+1)/2 of
c
c                       2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                      -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftb or hrfftf.
c
c     work    a real work array with m*n locations that does not
c             have to be saved
c
c **********************************************************************
c
c
c
      SUBROUTINE DHRFFTI(N,WSAVE)
      DOUBLE PRECISION WSAVE
      DOUBLE PRECISION TFFT
      DIMENSION WSAVE(N+15)
      COMMON /HRF/TFFT

      TFFT = 0.D0
      IF (N.EQ.1) RETURN
      CALL DHRFTI1(N,WSAVE(1),WSAVE(N+1))
      RETURN
      END
      SUBROUTINE DHRFTI1(N,WA,FAC)
      DOUBLE PRECISION WA
      DOUBLE PRECISION FAC
      DOUBLE PRECISION FI
c
c     a multiple fft package for spherepack
c
      DIMENSION WA(N),FAC(15),NTRYH(4)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TPI,ARGH,ARGLD,ARG
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/

      NL = N
      NF = 0
      J = 0
  101 J = J + 1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY + 2
  104 NQ = NL/NTRY
      NR = NL - NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF + 1
      FAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY.NE.2) GO TO 107
      IF (NF.EQ.1) GO TO 107
      DO 106 I = 2,NF
          IB = NF - I + 2
          FAC(IB+2) = FAC(IB+1)
  106 CONTINUE
      FAC(3) = 2
  107 IF (NL.NE.1) GO TO 104
      FAC(1) = N
      FAC(2) = NF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      TPI = 8.d0*DATAN(1.d0)
      ARGH = TPI/DBLE(N)
      IS = 0
      NFM1 = NF - 1
      L1 = 1
      IF (NFM1.EQ.0) RETURN
      DO 110 K1 = 1,NFM1
          IP = FAC(K1+2)
          LD = 0
          L2 = L1*IP
          IDO = N/L2
          IPM = IP - 1
          DO 109 J = 1,IPM
              LD = LD + L1
              I = IS
              ARGLD = DBLE(LD)*ARGH
              FI = 0.D0
              DO 108 II = 3,IDO,2
                  I = I + 2
                  FI = FI + 1.D0
                  ARG = FI*ARGLD
                  WA(I-1) = DCOS(ARG)
                  WA(I) = DSIN(ARG)
  108         CONTINUE
              IS = IS + IDO
  109     CONTINUE
          L1 = L2
  110 CONTINUE
      RETURN
      END
      SUBROUTINE DHRFFTF(M,N,R,MDIMR,WHRFFT,WORK)
      DOUBLE PRECISION R
      DOUBLE PRECISION WHRFFT
      DOUBLE PRECISION WORK
      DOUBLE PRECISION TFFT
c
c     a multiple fft package for spherepack
c
      DIMENSION R(MDIMR,N),WORK(1),WHRFFT(N+15)
      COMMON /HRF/TFFT

      IF (N.EQ.1) RETURN
c     tstart = second(dum)
      CALL DHRFTF1(M,N,R,MDIMR,WORK,WHRFFT,WHRFFT(N+1))
c     tfft = tfft+second(dum)-tstart
      RETURN
      END
      SUBROUTINE DHRFTF1(M,N,C,MDIMC,CH,WA,FAC)
      DOUBLE PRECISION C
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA
      DOUBLE PRECISION FAC
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(M,N),C(MDIMC,N),WA(N),FAC(15)

      NF = FAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1 = 1,NF
          KH = NF - K1
          IP = FAC(KH+3)
          L1 = L2/IP
          IDO = N/L2
          IDL1 = IDO*L1
          IW = IW - (IP-1)*IDO
          NA = 1 - NA
          IF (IP.NE.4) GO TO 102
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IF (NA.NE.0) GO TO 101
          CALL DHRADF4(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2),WA(IX3))
          GO TO 110
  101     CALL DHRADF4(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2),WA(IX3))
          GO TO 110
  102     IF (IP.NE.2) GO TO 104
          IF (NA.NE.0) GO TO 103
          CALL DHRADF2(M,IDO,L1,C,MDIMC,CH,M,WA(IW))
          GO TO 110
  103     CALL DHRADF2(M,IDO,L1,CH,M,C,MDIMC,WA(IW))
          GO TO 110
  104     IF (IP.NE.3) GO TO 106
          IX2 = IW + IDO
          IF (NA.NE.0) GO TO 105
          CALL DHRADF3(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2))
          GO TO 110
  105     CALL DHRADF3(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2))
          GO TO 110
  106     IF (IP.NE.5) GO TO 108
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IX4 = IX3 + IDO
          IF (NA.NE.0) GO TO 107
          CALL DHRADF5(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2),WA(IX3),
     +                WA(IX4))
          GO TO 110
  107     CALL DHRADF5(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2),WA(IX3),
     +                WA(IX4))
          GO TO 110
  108     IF (IDO.EQ.1) NA = 1 - NA
          IF (NA.NE.0) GO TO 109
          CALL DHRADFG(M,IDO,IP,L1,IDL1,C,C,C,MDIMC,CH,CH,M,WA(IW))
          NA = 1
          GO TO 110
  109     CALL DHRADFG(M,IDO,IP,L1,IDL1,CH,CH,CH,M,C,C,MDIMC,WA(IW))
          NA = 0
  110     L2 = L1
  111 CONTINUE
      IF (NA.EQ.1) RETURN
      DO 112 J = 1,N
          DO 112 I = 1,M
              C(I,J) = CH(I,J)
  112 CONTINUE
      RETURN
      END
      SUBROUTINE DHRADF4(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2,WA3)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION HSQT2
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,L1,4),CH(MDIMCH,IDO,4,L1),WA1(IDO),
     +          WA2(IDO),WA3(IDO)

      HSQT2 = SQRT(2.D0)/2.D0
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,1,K) = (CC(M,1,K,2)+CC(M,1,K,4)) +
     +                      (CC(M,1,K,1)+CC(M,1,K,3))
              CH(M,IDO,4,K) = (CC(M,1,K,1)+CC(M,1,K,3)) -
     +                        (CC(M,1,K,2)+CC(M,1,K,4))
              CH(M,IDO,2,K) = CC(M,1,K,1) - CC(M,1,K,3)
              CH(M,1,3,K) = CC(M,1,K,4) - CC(M,1,K,2)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              DO 1003 M = 1,MP
                  CH(M,I-1,1,K) = ((WA1(I-2)*CC(M,I-1,K,
     +                            2)+WA1(I-1)*CC(M,I,K,2))+
     +                            (WA3(I-2)*CC(M,I-1,K,4)+WA3(I-1)*CC(M,
     +                            I,K,4))) + (CC(M,I-1,K,1)+
     +                            (WA2(I-2)*CC(M,I-1,K,3)+WA2(I-1)*CC(M,
     +                            I,K,3)))
                  CH(M,IC-1,4,K) = (CC(M,I-1,K,1)+
     +                             (WA2(I-2)*CC(M,I-1,K,3)+WA2(I-
     +                             1)*CC(M,I,K,3))) -
     +                             ((WA1(I-2)*CC(M,I-1,K,
     +                             2)+WA1(I-1)*CC(M,I,K,2))+
     +                             (WA3(I-2)*CC(M,I-1,K,
     +                             4)+WA3(I-1)*CC(M,I,K,4)))
                  CH(M,I,1,K) = ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                          I-1,K,2))+ (WA3(I-2)*CC(M,I,K,
     +                          4)-WA3(I-1)*CC(M,I-1,K,4))) +
     +                          (CC(M,I,K,1)+ (WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,3)))
                  CH(M,IC,4,K) = ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                           I-1,K,2))+ (WA3(I-2)*CC(M,I,K,
     +                           4)-WA3(I-1)*CC(M,I-1,K,4))) -
     +                           (CC(M,I,K,1)+ (WA2(I-2)*CC(M,I,K,
     +                           3)-WA2(I-1)*CC(M,I-1,K,3)))
                  CH(M,I-1,3,K) = ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                            I-1,K,2))- (WA3(I-2)*CC(M,I,K,
     +                            4)-WA3(I-1)*CC(M,I-1,K,4))) +
     +                            (CC(M,I-1,K,1)- (WA2(I-2)*CC(M,I-1,K,
     +                            3)+WA2(I-1)*CC(M,I,K,3)))
                  CH(M,IC-1,2,K) = (CC(M,I-1,K,1)-
     +                             (WA2(I-2)*CC(M,I-1,K,3)+WA2(I-
     +                             1)*CC(M,I,K,3))) -
     +                             ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                             I-1,K,2))- (WA3(I-2)*CC(M,I,K,
     +                             4)-WA3(I-1)*CC(M,I-1,K,4)))
                  CH(M,I,3,K) = ((WA3(I-2)*CC(M,I-1,K,4)+WA3(I-1)*CC(M,
     +                          I,K,4))- (WA1(I-2)*CC(M,I-1,K,
     +                          2)+WA1(I-1)*CC(M,I,K,2))) +
     +                          (CC(M,I,K,1)- (WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,3)))
                  CH(M,IC,2,K) = ((WA3(I-2)*CC(M,I-1,K,4)+WA3(I-1)*CC(M,
     +                           I,K,4))- (WA1(I-2)*CC(M,I-1,K,
     +                           2)+WA1(I-1)*CC(M,I,K,2))) -
     +                           (CC(M,I,K,1)- (WA2(I-2)*CC(M,I,K,
     +                           3)-WA2(I-1)*CC(M,I-1,K,3)))
 1003         CONTINUE
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 CONTINUE
      DO 106 K = 1,L1
          DO 1006 M = 1,MP
              CH(M,IDO,1,K) = (HSQT2* (CC(M,IDO,K,2)-CC(M,IDO,K,4))) +
     +                        CC(M,IDO,K,1)
              CH(M,IDO,3,K) = CC(M,IDO,K,1) -
     +                        (HSQT2* (CC(M,IDO,K,2)-CC(M,IDO,K,4)))
              CH(M,1,2,K) = (-HSQT2* (CC(M,IDO,K,2)+CC(M,IDO,K,4))) -
     +                      CC(M,IDO,K,3)
              CH(M,1,4,K) = (-HSQT2* (CC(M,IDO,K,2)+CC(M,IDO,K,4))) +
     +                      CC(M,IDO,K,3)
 1006     CONTINUE
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE DHRADF2(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(MDIMCH,IDO,2,L1),CC(MDIMCC,IDO,L1,2),WA1(IDO)

      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,1,K) = CC(M,1,K,1) + CC(M,1,K,2)
              CH(M,IDO,2,K) = CC(M,1,K,1) - CC(M,1,K,2)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              DO 1003 M = 1,MP
                  CH(M,I,1,K) = CC(M,I,K,1) +
     +                          (WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*
     +                          CC(M,I-1,K,2))
                  CH(M,IC,2,K) = (WA1(I-2)*CC(M,I,K,2)-
     +                           WA1(I-1)*CC(M,I-1,K,2)) - CC(M,I,K,1)
                  CH(M,I-1,1,K) = CC(M,I-1,K,1) +
     +                            (WA1(I-2)*CC(M,I-1,K,2)+
     +                            WA1(I-1)*CC(M,I,K,2))
                  CH(M,IC-1,2,K) = CC(M,I-1,K,1) -
     +                             (WA1(I-2)*CC(M,I-1,K,2)+
     +                             WA1(I-1)*CC(M,I,K,2))
 1003         CONTINUE
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 DO 106 K = 1,L1
          DO 1006 M = 1,MP
              CH(M,1,2,K) = -CC(M,IDO,K,2)
              CH(M,IDO,1,K) = CC(M,IDO,K,1)
 1006     CONTINUE
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE DHRADF3(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION TAUR
      DOUBLE PRECISION TAUI
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(MDIMCH,IDO,3,L1),CC(MDIMCC,IDO,L1,3),WA1(IDO),
     +          WA2(IDO)

      ARG = 2.D0*DPIMACH()/3.D0
      TAUR = COS(ARG)
      TAUI = SIN(ARG)
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,1,K) = CC(M,1,K,1) + (CC(M,1,K,2)+CC(M,1,K,3))
              CH(M,1,3,K) = TAUI* (CC(M,1,K,3)-CC(M,1,K,2))
              CH(M,IDO,2,K) = CC(M,1,K,1) +
     +                        TAUR* (CC(M,1,K,2)+CC(M,1,K,3))
 1001     CONTINUE
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,1,K) = CC(M,I-1,K,1) +
     +                            ((WA1(I-2)*CC(M,I-1,K,2)+WA1(I-
     +                            1)*CC(M,I,K,2))+ (WA2(I-2)*CC(M,I-1,K,
     +                            3)+WA2(I-1)*CC(M,I,K,3)))
                  CH(M,I,1,K) = CC(M,I,K,1) +
     +                          ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                          I-1,K,2))+ (WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,3)))
                  CH(M,I-1,3,K) = (CC(M,I-1,K,1)+
     +                            TAUR* ((WA1(I-2)*CC(M,I-1,K,
     +                            2)+WA1(I-1)*CC(M,I,K,
     +                            2))+ (WA2(I-2)*CC(M,I-1,K,
     +                            3)+WA2(I-1)*CC(M,I,K,3)))) +
     +                            (TAUI* ((WA1(I-2)*CC(M,I,K,
     +                            2)-WA1(I-1)*CC(M,I-1,K,
     +                            2))- (WA2(I-2)*CC(M,I,K,
     +                            3)-WA2(I-1)*CC(M,I-1,K,3))))
                  CH(M,IC-1,2,K) = (CC(M,I-1,K,1)+
     +                             TAUR* ((WA1(I-2)*CC(M,I-1,K,
     +                             2)+WA1(I-1)*CC(M,I,K,
     +                             2))+ (WA2(I-2)*CC(M,I-1,K,
     +                             3)+WA2(I-1)*CC(M,I,K,3)))) -
     +                             (TAUI* ((WA1(I-2)*CC(M,I,K,
     +                             2)-WA1(I-1)*CC(M,I-1,K,
     +                             2))- (WA2(I-2)*CC(M,I,K,
     +                             3)-WA2(I-1)*CC(M,I-1,K,3))))
                  CH(M,I,3,K) = (CC(M,I,K,1)+
     +                          TAUR* ((WA1(I-2)*CC(M,I,K,2)-WA1(I-
     +                          1)*CC(M,I-1,K,2))+ (WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,3)))) +
     +                          (TAUI* ((WA2(I-2)*CC(M,I-1,K,
     +                          3)+WA2(I-1)*CC(M,I,K,
     +                          3))- (WA1(I-2)*CC(M,I-1,K,
     +                          2)+WA1(I-1)*CC(M,I,K,2))))
                  CH(M,IC,2,K) = (TAUI* ((WA2(I-2)*CC(M,I-1,K,
     +                           3)+WA2(I-1)*CC(M,I,K,
     +                           3))- (WA1(I-2)*CC(M,I-1,K,
     +                           2)+WA1(I-1)*CC(M,I,K,2)))) -
     +                           (CC(M,I,K,1)+TAUR* ((WA1(I-2)*CC(M,I,K,
     +                           2)-WA1(I-1)*CC(M,I-1,K,
     +                           2))+ (WA2(I-2)*CC(M,I,K,
     +                           3)-WA2(I-1)*CC(M,I-1,K,3))))
 1002         CONTINUE
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE DHRADF5(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2,WA3,WA4)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION WA4
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION TR11
      DOUBLE PRECISION TI11
      DOUBLE PRECISION TR12
      DOUBLE PRECISION TI12
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,L1,5),CH(MDIMCH,IDO,5,L1),WA1(IDO),
     +          WA2(IDO),WA3(IDO),WA4(IDO)

      ARG = 2.D0*DPIMACH()/5.D0
      TR11 = COS(ARG)
      TI11 = SIN(ARG)
      TR12 = COS(2.D0*ARG)
      TI12 = SIN(2.D0*ARG)
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,1,K) = CC(M,1,K,1) + (CC(M,1,K,5)+CC(M,1,K,2)) +
     +                      (CC(M,1,K,4)+CC(M,1,K,3))
              CH(M,IDO,2,K) = CC(M,1,K,1) +
     +                        TR11* (CC(M,1,K,5)+CC(M,1,K,2)) +
     +                        TR12* (CC(M,1,K,4)+CC(M,1,K,3))
              CH(M,1,3,K) = TI11* (CC(M,1,K,5)-CC(M,1,K,2)) +
     +                      TI12* (CC(M,1,K,4)-CC(M,1,K,3))
              CH(M,IDO,4,K) = CC(M,1,K,1) +
     +                        TR12* (CC(M,1,K,5)+CC(M,1,K,2)) +
     +                        TR11* (CC(M,1,K,4)+CC(M,1,K,3))
              CH(M,1,5,K) = TI12* (CC(M,1,K,5)-CC(M,1,K,2)) -
     +                      TI11* (CC(M,1,K,4)-CC(M,1,K,3))
 1001     CONTINUE
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,1,K) = CC(M,I-1,K,1) +
     +                            ((WA1(I-2)*CC(M,I-1,K,2)+WA1(I-
     +                            1)*CC(M,I,K,2))+ (WA4(I-2)*CC(M,I-1,K,
     +                            5)+WA4(I-1)*CC(M,I,K,5))) +
     +                            ((WA2(I-2)*CC(M,I-1,K,
     +                            3)+WA2(I-1)*CC(M,I,K,3))+
     +                            (WA3(I-2)*CC(M,I-1,K,4)+WA3(I-1)*CC(M,
     +                            I,K,4)))
                  CH(M,I,1,K) = CC(M,I,K,1) +
     +                          ((WA1(I-2)*CC(M,I,K,2)-WA1(I-1)*CC(M,
     +                          I-1,K,2))+ (WA4(I-2)*CC(M,I,K,
     +                          5)-WA4(I-1)*CC(M,I-1,K,5))) +
     +                          ((WA2(I-2)*CC(M,I,K,3)-WA2(I-1)*CC(M,
     +                          I-1,K,3))+ (WA3(I-2)*CC(M,I,K,
     +                          4)-WA3(I-1)*CC(M,I-1,K,4)))
                  CH(M,I-1,3,K) = CC(M,I-1,K,1) +
     +                            TR11* (WA1(I-2)*CC(M,I-1,K,2)+
     +                            WA1(I-1)*CC(M,I,K,2)+
     +                            WA4(I-2)*CC(M,I-1,K,5)+
     +                            WA4(I-1)*CC(M,I,K,5)) +
     +                            TR12* (WA2(I-2)*CC(M,I-1,K,3)+
     +                            WA2(I-1)*CC(M,I,K,3)+
     +                            WA3(I-2)*CC(M,I-1,K,4)+
     +                            WA3(I-1)*CC(M,I,K,4)) +
     +                            TI11* (WA1(I-2)*CC(M,I,K,2)-
     +                            WA1(I-1)*CC(M,I-1,K,2)-
     +                            (WA4(I-2)*CC(M,I,K,5)-WA4(I-1)*CC(M,
     +                            I-1,K,5))) + TI12*
     +                            (WA2(I-2)*CC(M,I,K,3)-
     +                            WA2(I-1)*CC(M,I-1,K,3)-
     +                            (WA3(I-2)*CC(M,I,K,4)-WA3(I-1)*CC(M,
     +                            I-1,K,4)))
                  CH(M,IC-1,2,K) = CC(M,I-1,K,1) +
     +                             TR11* (WA1(I-2)*CC(M,I-1,K,2)+
     +                             WA1(I-1)*CC(M,I,K,2)+
     +                             WA4(I-2)*CC(M,I-1,K,5)+
     +                             WA4(I-1)*CC(M,I,K,5)) +
     +                             TR12* (WA2(I-2)*CC(M,I-1,K,3)+
     +                             WA2(I-1)*CC(M,I,K,3)+
     +                             WA3(I-2)*CC(M,I-1,K,4)+
     +                             WA3(I-1)*CC(M,I,K,4)) -
     +                             (TI11* (WA1(I-2)*CC(M,I,K,
     +                             2)-WA1(I-1)*CC(M,I-1,K,
     +                             2)- (WA4(I-2)*CC(M,I,K,
     +                             5)-WA4(I-1)*CC(M,I-1,K,5)))+
     +                             TI12* (WA2(I-2)*CC(M,I,K,
     +                             3)-WA2(I-1)*CC(M,I-1,K,
     +                             3)- (WA3(I-2)*CC(M,I,K,
     +                             4)-WA3(I-1)*CC(M,I-1,K,4))))
                  CH(M,I,3,K) = (CC(M,I,K,1)+
     +                          TR11* ((WA1(I-2)*CC(M,I,K,2)-WA1(I-
     +                          1)*CC(M,I-1,K,2))+ (WA4(I-2)*CC(M,I,K,
     +                          5)-WA4(I-1)*CC(M,I-1,K,5)))+
     +                          TR12* ((WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,
     +                          3))+ (WA3(I-2)*CC(M,I,K,
     +                          4)-WA3(I-1)*CC(M,I-1,K,4)))) +
     +                          (TI11* ((WA4(I-2)*CC(M,I-1,K,
     +                          5)+WA4(I-1)*CC(M,I,K,
     +                          5))- (WA1(I-2)*CC(M,I-1,K,
     +                          2)+WA1(I-1)*CC(M,I,K,2)))+
     +                          TI12* ((WA3(I-2)*CC(M,I-1,K,
     +                          4)+WA3(I-1)*CC(M,I,K,
     +                          4))- (WA2(I-2)*CC(M,I-1,K,
     +                          3)+WA2(I-1)*CC(M,I,K,3))))
                  CH(M,IC,2,K) = (TI11* ((WA4(I-2)*CC(M,I-1,K,
     +                           5)+WA4(I-1)*CC(M,I,K,
     +                           5))- (WA1(I-2)*CC(M,I-1,K,
     +                           2)+WA1(I-1)*CC(M,I,K,2)))+
     +                           TI12* ((WA3(I-2)*CC(M,I-1,K,
     +                           4)+WA3(I-1)*CC(M,I,K,
     +                           4))- (WA2(I-2)*CC(M,I-1,K,
     +                           3)+WA2(I-1)*CC(M,I,K,3)))) -
     +                           (CC(M,I,K,1)+TR11* ((WA1(I-2)*CC(M,I,K,
     +                           2)-WA1(I-1)*CC(M,I-1,K,
     +                           2))+ (WA4(I-2)*CC(M,I,K,
     +                           5)-WA4(I-1)*CC(M,I-1,K,5)))+
     +                           TR12* ((WA2(I-2)*CC(M,I,K,
     +                           3)-WA2(I-1)*CC(M,I-1,K,
     +                           3))+ (WA3(I-2)*CC(M,I,K,
     +                           4)-WA3(I-1)*CC(M,I-1,K,4))))
                  CH(M,I-1,5,K) = (CC(M,I-1,K,1)+
     +                            TR12* ((WA1(I-2)*CC(M,I-1,K,
     +                            2)+WA1(I-1)*CC(M,I,K,
     +                            2))+ (WA4(I-2)*CC(M,I-1,K,
     +                            5)+WA4(I-1)*CC(M,I,K,5)))+
     +                            TR11* ((WA2(I-2)*CC(M,I-1,K,
     +                            3)+WA2(I-1)*CC(M,I,K,
     +                            3))+ (WA3(I-2)*CC(M,I-1,K,
     +                            4)+WA3(I-1)*CC(M,I,K,4)))) +
     +                            (TI12* ((WA1(I-2)*CC(M,I,K,
     +                            2)-WA1(I-1)*CC(M,I-1,K,
     +                            2))- (WA4(I-2)*CC(M,I,K,
     +                            5)-WA4(I-1)*CC(M,I-1,K,5)))-
     +                            TI11* ((WA2(I-2)*CC(M,I,K,
     +                            3)-WA2(I-1)*CC(M,I-1,K,
     +                            3))- (WA3(I-2)*CC(M,I,K,
     +                            4)-WA3(I-1)*CC(M,I-1,K,4))))
                  CH(M,IC-1,4,K) = (CC(M,I-1,K,1)+
     +                             TR12* ((WA1(I-2)*CC(M,I-1,K,
     +                             2)+WA1(I-1)*CC(M,I,K,
     +                             2))+ (WA4(I-2)*CC(M,I-1,K,
     +                             5)+WA4(I-1)*CC(M,I,K,5)))+
     +                             TR11* ((WA2(I-2)*CC(M,I-1,K,
     +                             3)+WA2(I-1)*CC(M,I,K,
     +                             3))+ (WA3(I-2)*CC(M,I-1,K,
     +                             4)+WA3(I-1)*CC(M,I,K,4)))) -
     +                             (TI12* ((WA1(I-2)*CC(M,I,K,
     +                             2)-WA1(I-1)*CC(M,I-1,K,
     +                             2))- (WA4(I-2)*CC(M,I,K,
     +                             5)-WA4(I-1)*CC(M,I-1,K,5)))-
     +                             TI11* ((WA2(I-2)*CC(M,I,K,
     +                             3)-WA2(I-1)*CC(M,I-1,K,
     +                             3))- (WA3(I-2)*CC(M,I,K,
     +                             4)-WA3(I-1)*CC(M,I-1,K,4))))
                  CH(M,I,5,K) = (CC(M,I,K,1)+
     +                          TR12* ((WA1(I-2)*CC(M,I,K,2)-WA1(I-
     +                          1)*CC(M,I-1,K,2))+ (WA4(I-2)*CC(M,I,K,
     +                          5)-WA4(I-1)*CC(M,I-1,K,5)))+
     +                          TR11* ((WA2(I-2)*CC(M,I,K,
     +                          3)-WA2(I-1)*CC(M,I-1,K,
     +                          3))+ (WA3(I-2)*CC(M,I,K,
     +                          4)-WA3(I-1)*CC(M,I-1,K,4)))) +
     +                          (TI12* ((WA4(I-2)*CC(M,I-1,K,
     +                          5)+WA4(I-1)*CC(M,I,K,
     +                          5))- (WA1(I-2)*CC(M,I-1,K,
     +                          2)+WA1(I-1)*CC(M,I,K,2)))-
     +                          TI11* ((WA3(I-2)*CC(M,I-1,K,
     +                          4)+WA3(I-1)*CC(M,I,K,
     +                          4))- (WA2(I-2)*CC(M,I-1,K,
     +                          3)+WA2(I-1)*CC(M,I,K,3))))
                  CH(M,IC,4,K) = (TI12* ((WA4(I-2)*CC(M,I-1,K,
     +                           5)+WA4(I-1)*CC(M,I,K,
     +                           5))- (WA1(I-2)*CC(M,I-1,K,
     +                           2)+WA1(I-1)*CC(M,I,K,2)))-
     +                           TI11* ((WA3(I-2)*CC(M,I-1,K,
     +                           4)+WA3(I-1)*CC(M,I,K,
     +                           4))- (WA2(I-2)*CC(M,I-1,K,
     +                           3)+WA2(I-1)*CC(M,I,K,3)))) -
     +                           (CC(M,I,K,1)+TR12* ((WA1(I-2)*CC(M,I,K,
     +                           2)-WA1(I-1)*CC(M,I-1,K,
     +                           2))+ (WA4(I-2)*CC(M,I,K,
     +                           5)-WA4(I-1)*CC(M,I-1,K,5)))+
     +                           TR11* ((WA2(I-2)*CC(M,I,K,
     +                           3)-WA2(I-1)*CC(M,I-1,K,
     +                           3))+ (WA3(I-2)*CC(M,I,K,
     +                           4)-WA3(I-1)*CC(M,I-1,K,4))))
 1002         CONTINUE
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE DHRADFG(MP,IDO,IP,L1,IDL1,CC,C1,C2,MDIMCC,CH,CH2,
     +                   MDIMCH,WA)
      DOUBLE PRECISION CC
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION CH
      DOUBLE PRECISION CH2
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DPIMACH
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
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(MDIMCH,IDO,L1,IP),CC(MDIMCC,IDO,IP,L1),
     +          C1(MDIMCC,IDO,L1,IP),C2(MDIMCC,IDL1,IP),
     +          CH2(MDIMCH,IDL1,IP),WA(IDO)

      TPI = 2.D0*DPIMACH()
      ARG = TPI/DBLE(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP + 2
      IDP2 = IDO + 2
      NBD = (IDO-1)/2
      IF (IDO.EQ.1) GO TO 119
      DO 101 IK = 1,IDL1
          DO 1001 M = 1,MP
              CH2(M,IK,1) = C2(M,IK,1)
 1001     CONTINUE
  101 CONTINUE
      DO 103 J = 2,IP
          DO 102 K = 1,L1
              DO 1002 M = 1,MP
                  CH(M,1,K,J) = C1(M,1,K,J)
 1002         CONTINUE
  102     CONTINUE
  103 CONTINUE
      IF (NBD.GT.L1) GO TO 107
      IS = -IDO
      DO 106 J = 2,IP
          IS = IS + IDO
          IDIJ = IS
          DO 105 I = 3,IDO,2
              IDIJ = IDIJ + 2
              DO 104 K = 1,L1
                  DO 1004 M = 1,MP
                      CH(M,I-1,K,J) = WA(IDIJ-1)*C1(M,I-1,K,J) +
     +                                WA(IDIJ)*C1(M,I,K,J)
                      CH(M,I,K,J) = WA(IDIJ-1)*C1(M,I,K,J) -
     +                              WA(IDIJ)*C1(M,I-1,K,J)
 1004             CONTINUE
  104         CONTINUE
  105     CONTINUE
  106 CONTINUE
      GO TO 111
  107 IS = -IDO
      DO 110 J = 2,IP
          IS = IS + IDO
          DO 109 K = 1,L1
              IDIJ = IS
              DO 108 I = 3,IDO,2
                  IDIJ = IDIJ + 2
                  DO 1008 M = 1,MP
                      CH(M,I-1,K,J) = WA(IDIJ-1)*C1(M,I-1,K,J) +
     +                                WA(IDIJ)*C1(M,I,K,J)
                      CH(M,I,K,J) = WA(IDIJ-1)*C1(M,I,K,J) -
     +                              WA(IDIJ)*C1(M,I-1,K,J)
 1008             CONTINUE
  108         CONTINUE
  109     CONTINUE
  110 CONTINUE
  111 IF (NBD.LT.L1) GO TO 115
      DO 114 J = 2,IPPH
          JC = IPP2 - J
          DO 113 K = 1,L1
              DO 112 I = 3,IDO,2
                  DO 1012 M = 1,MP
                      C1(M,I-1,K,J) = CH(M,I-1,K,J) + CH(M,I-1,K,JC)
                      C1(M,I-1,K,JC) = CH(M,I,K,J) - CH(M,I,K,JC)
                      C1(M,I,K,J) = CH(M,I,K,J) + CH(M,I,K,JC)
                      C1(M,I,K,JC) = CH(M,I-1,K,JC) - CH(M,I-1,K,J)
 1012             CONTINUE
  112         CONTINUE
  113     CONTINUE
  114 CONTINUE
      GO TO 121
  115 DO 118 J = 2,IPPH
          JC = IPP2 - J
          DO 117 I = 3,IDO,2
              DO 116 K = 1,L1
                  DO 1016 M = 1,MP
                      C1(M,I-1,K,J) = CH(M,I-1,K,J) + CH(M,I-1,K,JC)
                      C1(M,I-1,K,JC) = CH(M,I,K,J) - CH(M,I,K,JC)
                      C1(M,I,K,J) = CH(M,I,K,J) + CH(M,I,K,JC)
                      C1(M,I,K,JC) = CH(M,I-1,K,JC) - CH(M,I-1,K,J)
 1016             CONTINUE
  116         CONTINUE
  117     CONTINUE
  118 CONTINUE
      GO TO 121
  119 DO 120 IK = 1,IDL1
          DO 1020 M = 1,MP
              C2(M,IK,1) = CH2(M,IK,1)
 1020     CONTINUE
  120 CONTINUE
  121 DO 123 J = 2,IPPH
          JC = IPP2 - J
          DO 122 K = 1,L1
              DO 1022 M = 1,MP
                  C1(M,1,K,J) = CH(M,1,K,J) + CH(M,1,K,JC)
                  C1(M,1,K,JC) = CH(M,1,K,JC) - CH(M,1,K,J)
 1022         CONTINUE
  122     CONTINUE
  123 CONTINUE
c
      AR1 = 1.D0
      AI1 = 0.D0
      DO 127 L = 2,IPPH
          LC = IPP2 - L
          AR1H = DCP*AR1 - DSP*AI1
          AI1 = DCP*AI1 + DSP*AR1
          AR1 = AR1H
          DO 124 IK = 1,IDL1
              DO 1024 M = 1,MP
                  CH2(M,IK,L) = C2(M,IK,1) + AR1*C2(M,IK,2)
                  CH2(M,IK,LC) = AI1*C2(M,IK,IP)
 1024         CONTINUE
  124     CONTINUE
          DC2 = AR1
          DS2 = AI1
          AR2 = AR1
          AI2 = AI1
          DO 126 J = 3,IPPH
              JC = IPP2 - J
              AR2H = DC2*AR2 - DS2*AI2
              AI2 = DC2*AI2 + DS2*AR2
              AR2 = AR2H
              DO 125 IK = 1,IDL1
                  DO 1025 M = 1,MP
                      CH2(M,IK,L) = CH2(M,IK,L) + AR2*C2(M,IK,J)
                      CH2(M,IK,LC) = CH2(M,IK,LC) + AI2*C2(M,IK,JC)
 1025             CONTINUE
  125         CONTINUE
  126     CONTINUE
  127 CONTINUE
      DO 129 J = 2,IPPH
          DO 128 IK = 1,IDL1
              DO 1028 M = 1,MP
                  CH2(M,IK,1) = CH2(M,IK,1) + C2(M,IK,J)
 1028         CONTINUE
  128     CONTINUE
  129 CONTINUE
c
      IF (IDO.LT.L1) GO TO 132
      DO 131 K = 1,L1
          DO 130 I = 1,IDO
              DO 1030 M = 1,MP
                  CC(M,I,1,K) = CH(M,I,K,1)
 1030         CONTINUE
  130     CONTINUE
  131 CONTINUE
      GO TO 135
  132 DO 134 I = 1,IDO
          DO 133 K = 1,L1
              DO 1033 M = 1,MP
                  CC(M,I,1,K) = CH(M,I,K,1)
 1033         CONTINUE
  133     CONTINUE
  134 CONTINUE
  135 DO 137 J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO 136 K = 1,L1
              DO 1036 M = 1,MP
                  CC(M,IDO,J2-2,K) = CH(M,1,K,J)
                  CC(M,1,J2-1,K) = CH(M,1,K,JC)
 1036         CONTINUE
  136     CONTINUE
  137 CONTINUE
      IF (IDO.EQ.1) RETURN
      IF (NBD.LT.L1) GO TO 141
      DO 140 J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO 139 K = 1,L1
              DO 138 I = 3,IDO,2
                  IC = IDP2 - I
                  DO 1038 M = 1,MP
                      CC(M,I-1,J2-1,K) = CH(M,I-1,K,J) + CH(M,I-1,K,JC)
                      CC(M,IC-1,J2-2,K) = CH(M,I-1,K,J) - CH(M,I-1,K,JC)
                      CC(M,I,J2-1,K) = CH(M,I,K,J) + CH(M,I,K,JC)
                      CC(M,IC,J2-2,K) = CH(M,I,K,JC) - CH(M,I,K,J)
 1038             CONTINUE
  138         CONTINUE
  139     CONTINUE
  140 CONTINUE
      RETURN
  141 DO 144 J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO 143 I = 3,IDO,2
              IC = IDP2 - I
              DO 142 K = 1,L1
                  DO 1042 M = 1,MP
                      CC(M,I-1,J2-1,K) = CH(M,I-1,K,J) + CH(M,I-1,K,JC)
                      CC(M,IC-1,J2-2,K) = CH(M,I-1,K,J) - CH(M,I-1,K,JC)
                      CC(M,I,J2-1,K) = CH(M,I,K,J) + CH(M,I,K,JC)
                      CC(M,IC,J2-2,K) = CH(M,I,K,JC) - CH(M,I,K,J)
 1042             CONTINUE
  142         CONTINUE
  143     CONTINUE
  144 CONTINUE
      RETURN
      END
      FUNCTION DPIMACH()
      DOUBLE PRECISION DPIMACH

      DPIMACH = 3.14159265358979D0
      RETURN
      END
      SUBROUTINE DHRFFTB(M,N,R,MDIMR,WHRFFT,WORK)
      DOUBLE PRECISION R
      DOUBLE PRECISION WHRFFT
      DOUBLE PRECISION WORK
      DOUBLE PRECISION TFFT
c
c     a multiple fft package for spherepack
c
      DIMENSION R(MDIMR,N),WORK(1),WHRFFT(N+15)
      COMMON /HRF/TFFT

      IF (N.EQ.1) RETURN
c     tstart = second(dum)
      CALL DHRFTB1(M,N,R,MDIMR,WORK,WHRFFT,WHRFFT(N+1))
c     tfft = tfft+second(dum)-tstart
      RETURN
      END
      SUBROUTINE DHRFTB1(M,N,C,MDIMC,CH,WA,FAC)
      DOUBLE PRECISION C
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA
      DOUBLE PRECISION FAC
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(M,N),C(MDIMC,N),WA(N),FAC(15)

      NF = FAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1 = 1,NF
          IP = FAC(K1+2)
          L2 = IP*L1
          IDO = N/L2
          IDL1 = IDO*L1
          IF (IP.NE.4) GO TO 103
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IF (NA.NE.0) GO TO 101
          CALL DHRADB4(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2),WA(IX3))
          GO TO 102
  101     CALL DHRADB4(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2),WA(IX3))
  102     NA = 1 - NA
          GO TO 115
  103     IF (IP.NE.2) GO TO 106
          IF (NA.NE.0) GO TO 104
          CALL DHRADB2(M,IDO,L1,C,MDIMC,CH,M,WA(IW))
          GO TO 105
  104     CALL DHRADB2(M,IDO,L1,CH,M,C,MDIMC,WA(IW))
  105     NA = 1 - NA
          GO TO 115
  106     IF (IP.NE.3) GO TO 109
          IX2 = IW + IDO
          IF (NA.NE.0) GO TO 107
          CALL DHRADB3(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2))
          GO TO 108
  107     CALL DHRADB3(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2))
  108     NA = 1 - NA
          GO TO 115
  109     IF (IP.NE.5) GO TO 112
          IX2 = IW + IDO
          IX3 = IX2 + IDO
          IX4 = IX3 + IDO
          IF (NA.NE.0) GO TO 110
          CALL DHRADB5(M,IDO,L1,C,MDIMC,CH,M,WA(IW),WA(IX2),WA(IX3),
     +                WA(IX4))
          GO TO 111
  110     CALL DHRADB5(M,IDO,L1,CH,M,C,MDIMC,WA(IW),WA(IX2),WA(IX3),
     +                WA(IX4))
  111     NA = 1 - NA
          GO TO 115
  112     IF (NA.NE.0) GO TO 113
          CALL DHRADBG(M,IDO,IP,L1,IDL1,C,C,C,MDIMC,CH,CH,M,WA(IW))
          GO TO 114
  113     CALL DHRADBG(M,IDO,IP,L1,IDL1,CH,CH,CH,M,C,C,MDIMC,WA(IW))
  114     IF (IDO.EQ.1) NA = 1 - NA
  115     L1 = L2
          IW = IW + (IP-1)*IDO
  116 CONTINUE
      IF (NA.EQ.0) RETURN
      DO 117 J = 1,N
          DO 117 I = 1,M
              C(I,J) = CH(I,J)
  117 CONTINUE
      RETURN
      END
      SUBROUTINE DHRADBG(MP,IDO,IP,L1,IDL1,CC,C1,C2,MDIMCC,CH,CH2,
     +                   MDIMCH,WA)
      DOUBLE PRECISION CC
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION CH
      DOUBLE PRECISION CH2
      DOUBLE PRECISION WA
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DPIMACH
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
c
c     a multiple fft package for spherepack
c
      DIMENSION CH(MDIMCH,IDO,L1,IP),CC(MDIMCC,IDO,IP,L1),
     +          C1(MDIMCC,IDO,L1,IP),C2(MDIMCC,IDL1,IP),
     +          CH2(MDIMCH,IDL1,IP),WA(IDO)

      TPI = 2.D0*DPIMACH()
      ARG = TPI/DBLE(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO + 2
      NBD = (IDO-1)/2
      IPP2 = IP + 2
      IPPH = (IP+1)/2
      IF (IDO.LT.L1) GO TO 103
      DO 102 K = 1,L1
          DO 101 I = 1,IDO
              DO 1001 M = 1,MP
                  CH(M,I,K,1) = CC(M,I,1,K)
 1001         CONTINUE
  101     CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 I = 1,IDO
          DO 104 K = 1,L1
              DO 1004 M = 1,MP
                  CH(M,I,K,1) = CC(M,I,1,K)
 1004         CONTINUE
  104     CONTINUE
  105 CONTINUE
  106 DO 108 J = 2,IPPH
          JC = IPP2 - J
          J2 = J + J
          DO 107 K = 1,L1
              DO 1007 M = 1,MP
                  CH(M,1,K,J) = CC(M,IDO,J2-2,K) + CC(M,IDO,J2-2,K)
                  CH(M,1,K,JC) = CC(M,1,J2-1,K) + CC(M,1,J2-1,K)
 1007         CONTINUE
  107     CONTINUE
  108 CONTINUE
      IF (IDO.EQ.1) GO TO 116
      IF (NBD.LT.L1) GO TO 112
      DO 111 J = 2,IPPH
          JC = IPP2 - J
          DO 110 K = 1,L1
              DO 109 I = 3,IDO,2
                  IC = IDP2 - I
                  DO 1009 M = 1,MP
                      CH(M,I-1,K,J) = CC(M,I-1,2*J-1,K) +
     +                                CC(M,IC-1,2*J-2,K)
                      CH(M,I-1,K,JC) = CC(M,I-1,2*J-1,K) -
     +                                 CC(M,IC-1,2*J-2,K)
                      CH(M,I,K,J) = CC(M,I,2*J-1,K) - CC(M,IC,2*J-2,K)
                      CH(M,I,K,JC) = CC(M,I,2*J-1,K) + CC(M,IC,2*J-2,K)
 1009             CONTINUE
  109         CONTINUE
  110     CONTINUE
  111 CONTINUE
      GO TO 116
  112 DO 115 J = 2,IPPH
          JC = IPP2 - J
          DO 114 I = 3,IDO,2
              IC = IDP2 - I
              DO 113 K = 1,L1
                  DO 1013 M = 1,MP
                      CH(M,I-1,K,J) = CC(M,I-1,2*J-1,K) +
     +                                CC(M,IC-1,2*J-2,K)
                      CH(M,I-1,K,JC) = CC(M,I-1,2*J-1,K) -
     +                                 CC(M,IC-1,2*J-2,K)
                      CH(M,I,K,J) = CC(M,I,2*J-1,K) - CC(M,IC,2*J-2,K)
                      CH(M,I,K,JC) = CC(M,I,2*J-1,K) + CC(M,IC,2*J-2,K)
 1013             CONTINUE
  113         CONTINUE
  114     CONTINUE
  115 CONTINUE
  116 AR1 = 1.D0
      AI1 = 0.D0
      DO 120 L = 2,IPPH
          LC = IPP2 - L
          AR1H = DCP*AR1 - DSP*AI1
          AI1 = DCP*AI1 + DSP*AR1
          AR1 = AR1H
          DO 117 IK = 1,IDL1
              DO 1017 M = 1,MP
                  C2(M,IK,L) = CH2(M,IK,1) + AR1*CH2(M,IK,2)
                  C2(M,IK,LC) = AI1*CH2(M,IK,IP)
 1017         CONTINUE
  117     CONTINUE
          DC2 = AR1
          DS2 = AI1
          AR2 = AR1
          AI2 = AI1
          DO 119 J = 3,IPPH
              JC = IPP2 - J
              AR2H = DC2*AR2 - DS2*AI2
              AI2 = DC2*AI2 + DS2*AR2
              AR2 = AR2H
              DO 118 IK = 1,IDL1
                  DO 1018 M = 1,MP
                      C2(M,IK,L) = C2(M,IK,L) + AR2*CH2(M,IK,J)
                      C2(M,IK,LC) = C2(M,IK,LC) + AI2*CH2(M,IK,JC)
 1018             CONTINUE
  118         CONTINUE
  119     CONTINUE
  120 CONTINUE
      DO 122 J = 2,IPPH
          DO 121 IK = 1,IDL1
              DO 1021 M = 1,MP
                  CH2(M,IK,1) = CH2(M,IK,1) + CH2(M,IK,J)
 1021         CONTINUE
  121     CONTINUE
  122 CONTINUE
      DO 124 J = 2,IPPH
          JC = IPP2 - J
          DO 123 K = 1,L1
              DO 1023 M = 1,MP
                  CH(M,1,K,J) = C1(M,1,K,J) - C1(M,1,K,JC)
                  CH(M,1,K,JC) = C1(M,1,K,J) + C1(M,1,K,JC)
 1023         CONTINUE
  123     CONTINUE
  124 CONTINUE
      IF (IDO.EQ.1) GO TO 132
      IF (NBD.LT.L1) GO TO 128
      DO 127 J = 2,IPPH
          JC = IPP2 - J
          DO 126 K = 1,L1
              DO 125 I = 3,IDO,2
                  DO 1025 M = 1,MP
                      CH(M,I-1,K,J) = C1(M,I-1,K,J) - C1(M,I,K,JC)
                      CH(M,I-1,K,JC) = C1(M,I-1,K,J) + C1(M,I,K,JC)
                      CH(M,I,K,J) = C1(M,I,K,J) + C1(M,I-1,K,JC)
                      CH(M,I,K,JC) = C1(M,I,K,J) - C1(M,I-1,K,JC)
 1025             CONTINUE
  125         CONTINUE
  126     CONTINUE
  127 CONTINUE
      GO TO 132
  128 DO 131 J = 2,IPPH
          JC = IPP2 - J
          DO 130 I = 3,IDO,2
              DO 129 K = 1,L1
                  DO 1029 M = 1,MP
                      CH(M,I-1,K,J) = C1(M,I-1,K,J) - C1(M,I,K,JC)
                      CH(M,I-1,K,JC) = C1(M,I-1,K,J) + C1(M,I,K,JC)
                      CH(M,I,K,J) = C1(M,I,K,J) + C1(M,I-1,K,JC)
                      CH(M,I,K,JC) = C1(M,I,K,J) - C1(M,I-1,K,JC)
 1029             CONTINUE
  129         CONTINUE
  130     CONTINUE
  131 CONTINUE
  132 CONTINUE
      IF (IDO.EQ.1) RETURN
      DO 133 IK = 1,IDL1
          DO 1033 M = 1,MP
              C2(M,IK,1) = CH2(M,IK,1)
 1033     CONTINUE
  133 CONTINUE
      DO 135 J = 2,IP
          DO 134 K = 1,L1
              DO 1034 M = 1,MP
                  C1(M,1,K,J) = CH(M,1,K,J)
 1034         CONTINUE
  134     CONTINUE
  135 CONTINUE
      IF (NBD.GT.L1) GO TO 139
      IS = -IDO
      DO 138 J = 2,IP
          IS = IS + IDO
          IDIJ = IS
          DO 137 I = 3,IDO,2
              IDIJ = IDIJ + 2
              DO 136 K = 1,L1
                  DO 1036 M = 1,MP
                      C1(M,I-1,K,J) = WA(IDIJ-1)*CH(M,I-1,K,J) -
     +                                WA(IDIJ)*CH(M,I,K,J)
                      C1(M,I,K,J) = WA(IDIJ-1)*CH(M,I,K,J) +
     +                              WA(IDIJ)*CH(M,I-1,K,J)
 1036             CONTINUE
  136         CONTINUE
  137     CONTINUE
  138 CONTINUE
      GO TO 143
  139 IS = -IDO
      DO 142 J = 2,IP
          IS = IS + IDO
          DO 141 K = 1,L1
              IDIJ = IS
              DO 140 I = 3,IDO,2
                  IDIJ = IDIJ + 2
                  DO 1040 M = 1,MP
                      C1(M,I-1,K,J) = WA(IDIJ-1)*CH(M,I-1,K,J) -
     +                                WA(IDIJ)*CH(M,I,K,J)
                      C1(M,I,K,J) = WA(IDIJ-1)*CH(M,I,K,J) +
     +                              WA(IDIJ)*CH(M,I-1,K,J)
 1040             CONTINUE
  140         CONTINUE
  141     CONTINUE
  142 CONTINUE
  143 RETURN
      END
      SUBROUTINE DHRADB4(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2,WA3)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION SQRT2
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,4,L1),CH(MDIMCH,IDO,L1,4),WA1(IDO),
     +          WA2(IDO),WA3(IDO)

      SQRT2 = SQRT(2.D0)
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,K,3) = (CC(M,1,1,K)+CC(M,IDO,4,K)) -
     +                      (CC(M,IDO,2,K)+CC(M,IDO,2,K))
              CH(M,1,K,1) = (CC(M,1,1,K)+CC(M,IDO,4,K)) +
     +                      (CC(M,IDO,2,K)+CC(M,IDO,2,K))
              CH(M,1,K,4) = (CC(M,1,1,K)-CC(M,IDO,4,K)) +
     +                      (CC(M,1,3,K)+CC(M,1,3,K))
              CH(M,1,K,2) = (CC(M,1,1,K)-CC(M,IDO,4,K)) -
     +                      (CC(M,1,3,K)+CC(M,1,3,K))
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,K,1) = (CC(M,I-1,1,K)+CC(M,IC-1,4,K)) +
     +                            (CC(M,I-1,3,K)+CC(M,IC-1,2,K))
                  CH(M,I,K,1) = (CC(M,I,1,K)-CC(M,IC,4,K)) +
     +                          (CC(M,I,3,K)-CC(M,IC,2,K))
                  CH(M,I-1,K,2) = WA1(I-2)* ((CC(M,I-1,1,K)-CC(M,IC-1,4,
     +                            K))- (CC(M,I,3,K)+CC(M,IC,2,K))) -
     +                            WA1(I-1)* ((CC(M,I,1,K)+CC(M,IC,4,K))+
     +                            (CC(M,I-1,3,K)-CC(M,IC-1,2,K)))
                  CH(M,I,K,2) = WA1(I-2)* ((CC(M,I,1,K)+CC(M,IC,4,K))+
     +                          (CC(M,I-1,3,K)-CC(M,IC-1,2,K))) +
     +                          WA1(I-1)* ((CC(M,I-1,1,K)-CC(M,IC-1,4,
     +                          K))- (CC(M,I,3,K)+CC(M,IC,2,K)))
                  CH(M,I-1,K,3) = WA2(I-2)* ((CC(M,I-1,1,K)+CC(M,IC-1,4,
     +                            K))- (CC(M,I-1,3,K)+CC(M,IC-1,2,K))) -
     +                            WA2(I-1)* ((CC(M,I,1,K)-CC(M,IC,4,K))-
     +                            (CC(M,I,3,K)-CC(M,IC,2,K)))
                  CH(M,I,K,3) = WA2(I-2)* ((CC(M,I,1,K)-CC(M,IC,4,K))-
     +                          (CC(M,I,3,K)-CC(M,IC,2,K))) +
     +                          WA2(I-1)* ((CC(M,I-1,1,K)+CC(M,IC-1,4,
     +                          K))- (CC(M,I-1,3,K)+CC(M,IC-1,2,K)))
                  CH(M,I-1,K,4) = WA3(I-2)* ((CC(M,I-1,1,K)-CC(M,IC-1,4,
     +                            K))+ (CC(M,I,3,K)+CC(M,IC,2,K))) -
     +                            WA3(I-1)* ((CC(M,I,1,K)+CC(M,IC,4,K))-
     +                            (CC(M,I-1,3,K)-CC(M,IC-1,2,K)))
                  CH(M,I,K,4) = WA3(I-2)* ((CC(M,I,1,K)+CC(M,IC,4,K))-
     +                          (CC(M,I-1,3,K)-CC(M,IC-1,2,K))) +
     +                          WA3(I-1)* ((CC(M,I-1,1,K)-CC(M,IC-1,4,
     +                          K))+ (CC(M,I,3,K)+CC(M,IC,2,K)))
 1002         CONTINUE
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 CONTINUE
      DO 106 K = 1,L1
          DO 1003 M = 1,MP
              CH(M,IDO,K,1) = (CC(M,IDO,1,K)+CC(M,IDO,3,K)) +
     +                        (CC(M,IDO,1,K)+CC(M,IDO,3,K))
              CH(M,IDO,K,2) = SQRT2* ((CC(M,IDO,1,K)-CC(M,IDO,3,K))-
     +                        (CC(M,1,2,K)+CC(M,1,4,K)))
              CH(M,IDO,K,3) = (CC(M,1,4,K)-CC(M,1,2,K)) +
     +                        (CC(M,1,4,K)-CC(M,1,2,K))
              CH(M,IDO,K,4) = -SQRT2* ((CC(M,IDO,1,K)-CC(M,IDO,3,K))+
     +                        (CC(M,1,2,K)+CC(M,1,4,K)))
 1003     CONTINUE
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE DHRADB2(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,2,L1),CH(MDIMCH,IDO,L1,2),WA1(IDO)

      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,K,1) = CC(M,1,1,K) + CC(M,IDO,2,K)
              CH(M,1,K,2) = CC(M,1,1,K) - CC(M,IDO,2,K)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,K,1) = CC(M,I-1,1,K) + CC(M,IC-1,2,K)
                  CH(M,I,K,1) = CC(M,I,1,K) - CC(M,IC,2,K)
                  CH(M,I-1,K,2) = WA1(I-2)* (CC(M,I-1,1,K)-
     +                            CC(M,IC-1,2,K)) -
     +                            WA1(I-1)* (CC(M,I,1,K)+CC(M,IC,2,K))
                  CH(M,I,K,2) = WA1(I-2)* (CC(M,I,1,K)+CC(M,IC,2,K)) +
     +                          WA1(I-1)* (CC(M,I-1,1,K)-CC(M,IC-1,2,K))
 1002         CONTINUE
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 DO 106 K = 1,L1
          DO 1003 M = 1,MP
              CH(M,IDO,K,1) = CC(M,IDO,1,K) + CC(M,IDO,1,K)
              CH(M,IDO,K,2) = - (CC(M,1,2,K)+CC(M,1,2,K))
 1003     CONTINUE
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE DHRADB3(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION TAUR
      DOUBLE PRECISION TAUI
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,3,L1),CH(MDIMCH,IDO,L1,3),WA1(IDO),
     +          WA2(IDO)

      ARG = 2.D0*DPIMACH()/3.D0
      TAUR = COS(ARG)
      TAUI = SIN(ARG)
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,K,1) = CC(M,1,1,K) + 2.D0*CC(M,IDO,2,K)
              CH(M,1,K,2) = CC(M,1,1,K) + (2.D0*TAUR)*CC(M,IDO,2,K) -
     +                      (2.D0*TAUI)*CC(M,1,3,K)
              CH(M,1,K,3) = CC(M,1,1,K) + (2.D0*TAUR)*CC(M,IDO,2,K) +
     +                      2.D0*TAUI*CC(M,1,3,K)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,K,1) = CC(M,I-1,1,K) +
     +                            (CC(M,I-1,3,K)+CC(M,IC-1,2,K))
                  CH(M,I,K,1) = CC(M,I,1,K) + (CC(M,I,3,K)-CC(M,IC,2,K))
                  CH(M,I-1,K,2) = WA1(I-2)* ((CC(M,I-1,1,K)+TAUR* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K)))-
     +                            (TAUI* (CC(M,I,3,K)+CC(M,IC,2,K)))) -
     +                            WA1(I-1)* ((CC(M,I,1,K)+TAUR* (CC(M,I,
     +                            3,K)-CC(M,IC,2,K)))+
     +                            (TAUI* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))))
                  CH(M,I,K,2) = WA1(I-2)* ((CC(M,I,1,K)+TAUR* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K)))+ (TAUI* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K)))) +
     +                          WA1(I-1)* ((CC(M,I-1,1,K)+TAUR* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K)))-
     +                          (TAUI* (CC(M,I,3,K)+CC(M,IC,2,K))))
                  CH(M,I-1,K,3) = WA2(I-2)* ((CC(M,I-1,1,K)+TAUR* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K)))+
     +                            (TAUI* (CC(M,I,3,K)+CC(M,IC,2,K)))) -
     +                            WA2(I-1)* ((CC(M,I,1,K)+TAUR* (CC(M,I,
     +                            3,K)-CC(M,IC,2,K)))-
     +                            (TAUI* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))))
                  CH(M,I,K,3) = WA2(I-2)* ((CC(M,I,1,K)+TAUR* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K)))- (TAUI* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K)))) +
     +                          WA2(I-1)* ((CC(M,I-1,1,K)+TAUR* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K)))+
     +                          (TAUI* (CC(M,I,3,K)+CC(M,IC,2,K))))
 1002         CONTINUE
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE DHRADB5(MP,IDO,L1,CC,MDIMCC,CH,MDIMCH,WA1,WA2,WA3,WA4)
      DOUBLE PRECISION CC
      DOUBLE PRECISION CH
      DOUBLE PRECISION WA1
      DOUBLE PRECISION WA2
      DOUBLE PRECISION WA3
      DOUBLE PRECISION WA4
      DOUBLE PRECISION ARG
      DOUBLE PRECISION DPIMACH
      DOUBLE PRECISION TR11
      DOUBLE PRECISION TI11
      DOUBLE PRECISION TR12
      DOUBLE PRECISION TI12
c
c     a multiple fft package for spherepack
c
      DIMENSION CC(MDIMCC,IDO,5,L1),CH(MDIMCH,IDO,L1,5),WA1(IDO),
     +          WA2(IDO),WA3(IDO),WA4(IDO)

      ARG = 2.D0*DPIMACH()/5.D0
      TR11 = COS(ARG)
      TI11 = SIN(ARG)
      TR12 = COS(2.D0*ARG)
      TI12 = SIN(2.D0*ARG)
      DO 101 K = 1,L1
          DO 1001 M = 1,MP
              CH(M,1,K,1) = CC(M,1,1,K) + 2.D0*CC(M,IDO,2,K) +
     +                      2.D0*CC(M,IDO,4,K)
              CH(M,1,K,2) = (CC(M,1,1,K)+TR11*2.D0*CC(M,IDO,2,K)+
     +                      TR12*2.D0*CC(M,IDO,4,K)) -
     +                      (TI11*2.D0*CC(M,1,3,K)+
     +                      TI12*2.D0*CC(M,1,5,K))
              CH(M,1,K,3) = (CC(M,1,1,K)+TR12*2.D0*CC(M,IDO,2,K)+
     +                      TR11*2.D0*CC(M,IDO,4,K)) -
     +                      (TI12*2.D0*CC(M,1,3,K)-
     +                      TI11*2.D0*CC(M,1,5,K))
              CH(M,1,K,4) = (CC(M,1,1,K)+TR12*2.D0*CC(M,IDO,2,K)+
     +                      TR11*2.D0*CC(M,IDO,4,K)) +
     +                      (TI12*2.D0*CC(M,1,3,K)-
     +                      TI11*2.D0*CC(M,1,5,K))
              CH(M,1,K,5) = (CC(M,1,1,K)+TR11*2.D0*CC(M,IDO,2,K)+
     +                      TR12*2.D0*CC(M,IDO,4,K)) +
     +                      (TI11*2.D0*CC(M,1,3,K)+
     +                      TI12*2.D0*CC(M,1,5,K))
 1001     CONTINUE
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              DO 1002 M = 1,MP
                  CH(M,I-1,K,1) = CC(M,I-1,1,K) +
     +                            (CC(M,I-1,3,K)+CC(M,IC-1,2,K)) +
     +                            (CC(M,I-1,5,K)+CC(M,IC-1,4,K))
                  CH(M,I,K,1) = CC(M,I,1,K) +
     +                          (CC(M,I,3,K)-CC(M,IC,2,K)) +
     +                          (CC(M,I,5,K)-CC(M,IC,4,K))
                  CH(M,I-1,K,2) = WA1(I-2)* ((CC(M,I-1,1,K)+TR11* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K))+TR12* (CC(M,
     +                            I-1,5,K)+CC(M,IC-1,4,K)))-
     +                            (TI11* (CC(M,I,3,K)+CC(M,IC,2,
     +                            K))+TI12* (CC(M,I,5,K)+CC(M,IC,4,K))))
     +                            - WA1(I-1)* ((CC(M,I,1,K)+TR11* (CC(M,
     +                            I,3,K)-CC(M,IC,2,K))+TR12* (CC(M,I,5,
     +                            K)-CC(M,IC,4,K)))+
     +                            (TI11* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))+TI12* (CC(M,I-1,5,K)-CC(M,IC-1,4,
     +                            K))))
                  CH(M,I,K,2) = WA1(I-2)* ((CC(M,I,1,K)+TR11* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K))+TR12* (CC(M,I,5,
     +                          K)-CC(M,IC,4,K)))+ (TI11* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K))+TI12* (CC(M,I-1,5,
     +                          K)-CC(M,IC-1,4,K)))) +
     +                          WA1(I-1)* ((CC(M,I-1,1,K)+TR11* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K))+TR12* (CC(M,
     +                          I-1,5,K)+CC(M,IC-1,4,K)))-
     +                          (TI11* (CC(M,I,3,K)+CC(M,IC,2,
     +                          K))+TI12* (CC(M,I,5,K)+CC(M,IC,4,K))))
                  CH(M,I-1,K,3) = WA2(I-2)* ((CC(M,I-1,1,K)+TR12* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K))+TR11* (CC(M,
     +                            I-1,5,K)+CC(M,IC-1,4,K)))-
     +                            (TI12* (CC(M,I,3,K)+CC(M,IC,2,
     +                            K))-TI11* (CC(M,I,5,K)+CC(M,IC,4,K))))
     +                            - WA2(I-1)* ((CC(M,I,1,K)+TR12* (CC(M,
     +                            I,3,K)-CC(M,IC,2,K))+TR11* (CC(M,I,5,
     +                            K)-CC(M,IC,4,K)))+
     +                            (TI12* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))-TI11* (CC(M,I-1,5,K)-CC(M,IC-1,4,
     +                            K))))
                  CH(M,I,K,3) = WA2(I-2)* ((CC(M,I,1,K)+TR12* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K))+TR11* (CC(M,I,5,
     +                          K)-CC(M,IC,4,K)))+ (TI12* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K))-TI11* (CC(M,I-1,5,
     +                          K)-CC(M,IC-1,4,K)))) +
     +                          WA2(I-1)* ((CC(M,I-1,1,K)+TR12* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K))+TR11* (CC(M,
     +                          I-1,5,K)+CC(M,IC-1,4,K)))-
     +                          (TI12* (CC(M,I,3,K)+CC(M,IC,2,
     +                          K))-TI11* (CC(M,I,5,K)+CC(M,IC,4,K))))
                  CH(M,I-1,K,4) = WA3(I-2)* ((CC(M,I-1,1,K)+TR12* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K))+TR11* (CC(M,
     +                            I-1,5,K)+CC(M,IC-1,4,K)))+
     +                            (TI12* (CC(M,I,3,K)+CC(M,IC,2,
     +                            K))-TI11* (CC(M,I,5,K)+CC(M,IC,4,K))))
     +                            - WA3(I-1)* ((CC(M,I,1,K)+TR12* (CC(M,
     +                            I,3,K)-CC(M,IC,2,K))+TR11* (CC(M,I,5,
     +                            K)-CC(M,IC,4,K)))-
     +                            (TI12* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))-TI11* (CC(M,I-1,5,K)-CC(M,IC-1,4,
     +                            K))))
                  CH(M,I,K,4) = WA3(I-2)* ((CC(M,I,1,K)+TR12* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K))+TR11* (CC(M,I,5,
     +                          K)-CC(M,IC,4,K)))- (TI12* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K))-TI11* (CC(M,I-1,5,
     +                          K)-CC(M,IC-1,4,K)))) +
     +                          WA3(I-1)* ((CC(M,I-1,1,K)+TR12* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K))+TR11* (CC(M,
     +                          I-1,5,K)+CC(M,IC-1,4,K)))+
     +                          (TI12* (CC(M,I,3,K)+CC(M,IC,2,
     +                          K))-TI11* (CC(M,I,5,K)+CC(M,IC,4,K))))
                  CH(M,I-1,K,5) = WA4(I-2)* ((CC(M,I-1,1,K)+TR11* (CC(M,
     +                            I-1,3,K)+CC(M,IC-1,2,K))+TR12* (CC(M,
     +                            I-1,5,K)+CC(M,IC-1,4,K)))+
     +                            (TI11* (CC(M,I,3,K)+CC(M,IC,2,
     +                            K))+TI12* (CC(M,I,5,K)+CC(M,IC,4,K))))
     +                            - WA4(I-1)* ((CC(M,I,1,K)+TR11* (CC(M,
     +                            I,3,K)-CC(M,IC,2,K))+TR12* (CC(M,I,5,
     +                            K)-CC(M,IC,4,K)))-
     +                            (TI11* (CC(M,I-1,3,K)-CC(M,IC-1,2,
     +                            K))+TI12* (CC(M,I-1,5,K)-CC(M,IC-1,4,
     +                            K))))
                  CH(M,I,K,5) = WA4(I-2)* ((CC(M,I,1,K)+TR11* (CC(M,I,3,
     +                          K)-CC(M,IC,2,K))+TR12* (CC(M,I,5,
     +                          K)-CC(M,IC,4,K)))- (TI11* (CC(M,I-1,3,
     +                          K)-CC(M,IC-1,2,K))+TI12* (CC(M,I-1,5,
     +                          K)-CC(M,IC-1,4,K)))) +
     +                          WA4(I-1)* ((CC(M,I-1,1,K)+TR11* (CC(M,
     +                          I-1,3,K)+CC(M,IC-1,2,K))+TR12* (CC(M,
     +                          I-1,5,K)+CC(M,IC-1,4,K)))+
     +                          (TI11* (CC(M,I,3,K)+CC(M,IC,2,
     +                          K))+TI12* (CC(M,I,5,K)+CC(M,IC,4,K))))
 1002         CONTINUE
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
