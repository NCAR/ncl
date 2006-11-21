CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: mradb2.f,v 1.2 2006-11-21 01:10:17 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DMRADB2(M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1)
      DOUBLE PRECISION CC(IN1,IDO,2,L1),CH(IN2,IDO,L1,2),WA1(IDO)
C
      M1D = (M-1)*IM1 + 1
      M2S = 1 - IM2
      DO 101 K = 1,L1
          M2 = M2S
          DO 1001 M1 = 1,M1D,IM1
              M2 = M2 + IM2
              CH(M2,1,K,1) = CC(M1,1,1,K) + CC(M1,IDO,2,K)
              CH(M2,1,K,2) = CC(M1,1,1,K) - CC(M1,IDO,2,K)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              M2 = M2S
              DO 1002 M1 = 1,M1D,IM1
                  M2 = M2 + IM2
                  CH(M2,I-1,K,1) = CC(M1,I-1,1,K) + CC(M1,IC-1,2,K)
                  CH(M2,I,K,1) = CC(M1,I,1,K) - CC(M1,IC,2,K)
                  CH(M2,I-1,K,2) = WA1(I-2)*
     +                             (CC(M1,I-1,1,K)-CC(M1,IC-1,2,K)) -
     +                             WA1(I-1)* (CC(M1,I,1,K)+
     +                             CC(M1,IC,2,K))
                  CH(M2,I,K,2) = WA1(I-2)* (CC(M1,I,1,K)+
     +                           CC(M1,IC,2,K)) + WA1(I-1)*
     +                           (CC(M1,I-1,1,K)-CC(M1,IC-1,2,K))
 1002         CONTINUE
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 DO 106 K = 1,L1
          M2 = M2S
          DO 1003 M1 = 1,M1D,IM1
              M2 = M2 + IM2
              CH(M2,IDO,K,1) = CC(M1,IDO,1,K) + CC(M1,IDO,1,K)
              CH(M2,IDO,K,2) = - (CC(M1,1,2,K)+CC(M1,1,2,K))
 1003     CONTINUE
  106 CONTINUE
  107 RETURN
      END
