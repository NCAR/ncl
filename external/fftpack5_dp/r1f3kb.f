CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: r1f3kb.f,v 1.2 2006-11-21 01:10:18 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DR1F3KB(IDO,L1,CC,IN1,CH,IN2,WA1,WA2)
      DOUBLE PRECISION ARG
      DOUBLE PRECISION TAUR
      DOUBLE PRECISION TAUI
      DOUBLE PRECISION CC(IN1,IDO,3,L1),CH(IN2,IDO,L1,3),WA1(IDO),
     +                 WA2(IDO)
C
      ARG = 2.D0*4.D0*ATAN(1.0D0)/3.D0
      TAUR = COS(ARG)
      TAUI = SIN(ARG)
      DO 101 K = 1,L1
          CH(1,1,K,1) = CC(1,1,1,K) + 2.D0*CC(1,IDO,2,K)
          CH(1,1,K,2) = CC(1,1,1,K) + (2.D0*TAUR)*CC(1,IDO,2,K) -
     +                  (2.D0*TAUI)*CC(1,1,3,K)
          CH(1,1,K,3) = CC(1,1,1,K) + (2.D0*TAUR)*CC(1,IDO,2,K) +
     +                  2.D0*TAUI*CC(1,1,3,K)
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              CH(1,I-1,K,1) = CC(1,I-1,1,K) +
     +                        (CC(1,I-1,3,K)+CC(1,IC-1,2,K))
              CH(1,I,K,1) = CC(1,I,1,K) + (CC(1,I,3,K)-CC(1,IC,2,K))
              CH(1,I-1,K,2) = WA1(I-2)* ((CC(1,I-1,1,K)+TAUR* (CC(1,I-1,
     +                        3,K)+CC(1,IC-1,2,K)))-
     +                        (TAUI* (CC(1,I,3,K)+CC(1,IC,2,K)))) -
     +                        WA1(I-1)* ((CC(1,I,1,K)+TAUR* (CC(1,I,3,
     +                        K)-CC(1,IC,2,K)))+ (TAUI* (CC(1,I-1,3,
     +                        K)-CC(1,IC-1,2,K))))
              CH(1,I,K,2) = WA1(I-2)* ((CC(1,I,1,K)+TAUR* (CC(1,I,3,
     +                      K)-CC(1,IC,2,K)))+ (TAUI* (CC(1,I-1,3,
     +                      K)-CC(1,IC-1,2,K)))) +
     +                      WA1(I-1)* ((CC(1,I-1,1,K)+TAUR* (CC(1,I-1,3,
     +                      K)+CC(1,IC-1,2,K)))- (TAUI* (CC(1,I,3,
     +                      K)+CC(1,IC,2,K))))
              CH(1,I-1,K,3) = WA2(I-2)* ((CC(1,I-1,1,K)+TAUR* (CC(1,I-1,
     +                        3,K)+CC(1,IC-1,2,K)))+
     +                        (TAUI* (CC(1,I,3,K)+CC(1,IC,2,K)))) -
     +                        WA2(I-1)* ((CC(1,I,1,K)+TAUR* (CC(1,I,3,
     +                        K)-CC(1,IC,2,K)))- (TAUI* (CC(1,I-1,3,
     +                        K)-CC(1,IC-1,2,K))))
              CH(1,I,K,3) = WA2(I-2)* ((CC(1,I,1,K)+TAUR* (CC(1,I,3,
     +                      K)-CC(1,IC,2,K)))- (TAUI* (CC(1,I-1,3,
     +                      K)-CC(1,IC-1,2,K)))) +
     +                      WA2(I-1)* ((CC(1,I-1,1,K)+TAUR* (CC(1,I-1,3,
     +                      K)+CC(1,IC-1,2,K)))+ (TAUI* (CC(1,I,3,
     +                      K)+CC(1,IC,2,K))))
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
