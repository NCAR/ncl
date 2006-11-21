CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: r1f4kf.f,v 1.2 2006-11-21 01:10:19 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DR1F4KF(IDO,L1,CC,IN1,CH,IN2,WA1,WA2,WA3)
      DOUBLE PRECISION HSQT2
      DOUBLE PRECISION CC(IN1,IDO,L1,4),CH(IN2,IDO,4,L1),WA1(IDO),
     +                 WA2(IDO),WA3(IDO)
C
      HSQT2 = SQRT(2.D0)/2.D0
      DO 101 K = 1,L1
          CH(1,1,1,K) = (CC(1,1,K,2)+CC(1,1,K,4)) +
     +                  (CC(1,1,K,1)+CC(1,1,K,3))
          CH(1,IDO,4,K) = (CC(1,1,K,1)+CC(1,1,K,3)) -
     +                    (CC(1,1,K,2)+CC(1,1,K,4))
          CH(1,IDO,2,K) = CC(1,1,K,1) - CC(1,1,K,3)
          CH(1,1,3,K) = CC(1,1,K,4) - CC(1,1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO + 2
      DO 104 K = 1,L1
          DO 103 I = 3,IDO,2
              IC = IDP2 - I
              CH(1,I-1,1,K) = ((WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,
     +                        K,2))+ (WA3(I-2)*CC(1,I-1,K,
     +                        4)+WA3(I-1)*CC(1,I,K,4))) +
     +                        (CC(1,I-1,K,1)+ (WA2(I-2)*CC(1,I-1,K,
     +                        3)+WA2(I-1)*CC(1,I,K,3)))
              CH(1,IC-1,4,K) = (CC(1,I-1,K,1)+
     +                         (WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,
     +                         K,3))) - ((WA1(I-2)*CC(1,I-1,K,
     +                         2)+WA1(I-1)*CC(1,I,K,2))+
     +                         (WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,
     +                         K,4)))
              CH(1,I,1,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,
     +                      2))+ (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,
     +                      I-1,K,4))) + (CC(1,I,K,1)+
     +                      (WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,
     +                      3)))
              CH(1,IC,4,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,
     +                       2))+ (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,
     +                       I-1,K,4))) - (CC(1,I,K,1)+
     +                       (WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,
     +                       3)))
              CH(1,I-1,3,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,
     +                        K,2))- (WA3(I-2)*CC(1,I,K,
     +                        4)-WA3(I-1)*CC(1,I-1,K,4))) +
     +                        (CC(1,I-1,K,1)- (WA2(I-2)*CC(1,I-1,K,
     +                        3)+WA2(I-1)*CC(1,I,K,3)))
              CH(1,IC-1,2,K) = (CC(1,I-1,K,1)-
     +                         (WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,
     +                         K,3))) - ((WA1(I-2)*CC(1,I,K,
     +                         2)-WA1(I-1)*CC(1,I-1,K,2))-
     +                         (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,
     +                         K,4)))
              CH(1,I,3,K) = ((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,
     +                      4))- (WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,
     +                      I,K,2))) + (CC(1,I,K,1)-
     +                      (WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,
     +                      3)))
              CH(1,IC,2,K) = ((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,
     +                       4))- (WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,
     +                       I,K,2))) - (CC(1,I,K,1)-
     +                       (WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,
     +                       3)))
  103     CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2).EQ.1) RETURN
  105 CONTINUE
      DO 106 K = 1,L1
          CH(1,IDO,1,K) = (HSQT2* (CC(1,IDO,K,2)-CC(1,IDO,K,4))) +
     +                    CC(1,IDO,K,1)
          CH(1,IDO,3,K) = CC(1,IDO,K,1) -
     +                    (HSQT2* (CC(1,IDO,K,2)-CC(1,IDO,K,4)))
          CH(1,1,2,K) = (-HSQT2* (CC(1,IDO,K,2)+CC(1,IDO,K,4))) -
     +                  CC(1,IDO,K,3)
          CH(1,1,4,K) = (-HSQT2* (CC(1,IDO,K,2)+CC(1,IDO,K,4))) +
     +                  CC(1,IDO,K,3)
  106 CONTINUE
  107 RETURN
      END
