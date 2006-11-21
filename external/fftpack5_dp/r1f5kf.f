CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: r1f5kf.f,v 1.2 2006-11-21 01:10:19 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DR1F5KF(IDO,L1,CC,IN1,CH,IN2,WA1,WA2,WA3,WA4)
      DOUBLE PRECISION ARG
      DOUBLE PRECISION TR11
      DOUBLE PRECISION TI11
      DOUBLE PRECISION TR12
      DOUBLE PRECISION TI12
      DOUBLE PRECISION CC(IN1,IDO,L1,5),CH(IN2,IDO,5,L1),WA1(IDO),
     +                 WA2(IDO),WA3(IDO),WA4(IDO)
C
      ARG = 2.D0*4.D0*ATAN(1.0D0)/5.D0
      TR11 = COS(ARG)
      TI11 = SIN(ARG)
      TR12 = COS(2.D0*ARG)
      TI12 = SIN(2.D0*ARG)
      DO 101 K = 1,L1
          CH(1,1,1,K) = CC(1,1,K,1) + (CC(1,1,K,5)+CC(1,1,K,2)) +
     +                  (CC(1,1,K,4)+CC(1,1,K,3))
          CH(1,IDO,2,K) = CC(1,1,K,1) +
     +                    TR11* (CC(1,1,K,5)+CC(1,1,K,2)) +
     +                    TR12* (CC(1,1,K,4)+CC(1,1,K,3))
          CH(1,1,3,K) = TI11* (CC(1,1,K,5)-CC(1,1,K,2)) +
     +                  TI12* (CC(1,1,K,4)-CC(1,1,K,3))
          CH(1,IDO,4,K) = CC(1,1,K,1) +
     +                    TR12* (CC(1,1,K,5)+CC(1,1,K,2)) +
     +                    TR11* (CC(1,1,K,4)+CC(1,1,K,3))
          CH(1,1,5,K) = TI12* (CC(1,1,K,5)-CC(1,1,K,2)) -
     +                  TI11* (CC(1,1,K,4)-CC(1,1,K,3))
  101 CONTINUE
      IF (IDO.EQ.1) RETURN
      IDP2 = IDO + 2
      DO 103 K = 1,L1
          DO 102 I = 3,IDO,2
              IC = IDP2 - I
              CH(1,I-1,1,K) = CC(1,I-1,K,1) +
     +                        ((WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,
     +                        K,2))+ (WA4(I-2)*CC(1,I-1,K,
     +                        5)+WA4(I-1)*CC(1,I,K,5))) +
     +                        ((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,
     +                        K,3))+ (WA3(I-2)*CC(1,I-1,K,
     +                        4)+WA3(I-1)*CC(1,I,K,4)))
              CH(1,I,1,K) = CC(1,I,K,1) +
     +                      ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,
     +                      2))+ (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                      I-1,K,5))) + ((WA2(I-2)*CC(1,I,K,
     +                      3)-WA2(I-1)*CC(1,I-1,K,3))+
     +                      (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,
     +                      4)))
              CH(1,I-1,3,K) = CC(1,I-1,K,1) +
     +                        TR11* (WA1(I-2)*CC(1,I-1,K,2)+
     +                        WA1(I-1)*CC(1,I,K,2)+
     +                        WA4(I-2)*CC(1,I-1,K,5)+
     +                        WA4(I-1)*CC(1,I,K,5)) +
     +                        TR12* (WA2(I-2)*CC(1,I-1,K,3)+
     +                        WA2(I-1)*CC(1,I,K,3)+
     +                        WA3(I-2)*CC(1,I-1,K,4)+
     +                        WA3(I-1)*CC(1,I,K,4)) +
     +                        TI11* (WA1(I-2)*CC(1,I,K,2)-
     +                        WA1(I-1)*CC(1,I-1,K,2)-
     +                        (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,I-1,K,
     +                        5))) + TI12* (WA2(I-2)*CC(1,I,K,3)-
     +                        WA2(I-1)*CC(1,I-1,K,3)-
     +                        (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,
     +                        4)))
              CH(1,IC-1,2,K) = CC(1,I-1,K,1) +
     +                         TR11* (WA1(I-2)*CC(1,I-1,K,2)+
     +                         WA1(I-1)*CC(1,I,K,2)+
     +                         WA4(I-2)*CC(1,I-1,K,5)+
     +                         WA4(I-1)*CC(1,I,K,5)) +
     +                         TR12* (WA2(I-2)*CC(1,I-1,K,3)+
     +                         WA2(I-1)*CC(1,I,K,3)+
     +                         WA3(I-2)*CC(1,I-1,K,4)+
     +                         WA3(I-1)*CC(1,I,K,4)) -
     +                         (TI11* (WA1(I-2)*CC(1,I,K,
     +                         2)-WA1(I-1)*CC(1,I-1,K,
     +                         2)- (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                         I-1,K,5)))+TI12* (WA2(I-2)*CC(1,I,K,
     +                         3)-WA2(I-1)*CC(1,I-1,K,
     +                         3)- (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,
     +                         I-1,K,4))))
              CH(1,I,3,K) = (CC(1,I,K,1)+TR11*
     +                      ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,
     +                      2))+ (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                      I-1,K,5)))+TR12* ((WA2(I-2)*CC(1,I,K,
     +                      3)-WA2(I-1)*CC(1,I-1,K,3))+ (WA3(I-2)*CC(1,
     +                      I,K,4)-WA3(I-1)*CC(1,I-1,K,4)))) +
     +                      (TI11* ((WA4(I-2)*CC(1,I-1,K,
     +                      5)+WA4(I-1)*CC(1,I,K,5))- (WA1(I-2)*CC(1,
     +                      I-1,K,2)+WA1(I-1)*CC(1,I,K,2)))+
     +                      TI12* ((WA3(I-2)*CC(1,I-1,K,
     +                      4)+WA3(I-1)*CC(1,I,K,4))- (WA2(I-2)*CC(1,
     +                      I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))
              CH(1,IC,2,K) = (TI11* ((WA4(I-2)*CC(1,I-1,K,
     +                       5)+WA4(I-1)*CC(1,I,K,5))- (WA1(I-2)*CC(1,
     +                       I-1,K,2)+WA1(I-1)*CC(1,I,K,2)))+
     +                       TI12* ((WA3(I-2)*CC(1,I-1,K,
     +                       4)+WA3(I-1)*CC(1,I,K,4))- (WA2(I-2)*CC(1,
     +                       I-1,K,3)+WA2(I-1)*CC(1,I,K,3)))) -
     +                       (CC(1,I,K,1)+TR11* ((WA1(I-2)*CC(1,I,K,
     +                       2)-WA1(I-1)*CC(1,I-1,K,2))+ (WA4(I-2)*CC(1,
     +                       I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))+
     +                       TR12* ((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,
     +                       I-1,K,3))+ (WA3(I-2)*CC(1,I,K,
     +                       4)-WA3(I-1)*CC(1,I-1,K,4))))
              CH(1,I-1,5,K) = (CC(1,I-1,K,1)+
     +                        TR12* ((WA1(I-2)*CC(1,I-1,K,
     +                        2)+WA1(I-1)*CC(1,I,K,2))+ (WA4(I-2)*CC(1,
     +                        I-1,K,5)+WA4(I-1)*CC(1,I,K,5)))+
     +                        TR11* ((WA2(I-2)*CC(1,I-1,K,
     +                        3)+WA2(I-1)*CC(1,I,K,3))+ (WA3(I-2)*CC(1,
     +                        I-1,K,4)+WA3(I-1)*CC(1,I,K,4)))) +
     +                        (TI12* ((WA1(I-2)*CC(1,I,K,
     +                        2)-WA1(I-1)*CC(1,I-1,K,
     +                        2))- (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                        I-1,K,5)))-TI11* ((WA2(I-2)*CC(1,I,K,
     +                        3)-WA2(I-1)*CC(1,I-1,K,
     +                        3))- (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,
     +                        I-1,K,4))))
              CH(1,IC-1,4,K) = (CC(1,I-1,K,1)+
     +                         TR12* ((WA1(I-2)*CC(1,I-1,K,
     +                         2)+WA1(I-1)*CC(1,I,K,2))+ (WA4(I-2)*CC(1,
     +                         I-1,K,5)+WA4(I-1)*CC(1,I,K,5)))+
     +                         TR11* ((WA2(I-2)*CC(1,I-1,K,
     +                         3)+WA2(I-1)*CC(1,I,K,3))+ (WA3(I-2)*CC(1,
     +                         I-1,K,4)+WA3(I-1)*CC(1,I,K,4)))) -
     +                         (TI12* ((WA1(I-2)*CC(1,I,K,
     +                         2)-WA1(I-1)*CC(1,I-1,K,
     +                         2))- (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                         I-1,K,5)))-TI11* ((WA2(I-2)*CC(1,I,K,
     +                         3)-WA2(I-1)*CC(1,I-1,K,
     +                         3))- (WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,
     +                         I-1,K,4))))
              CH(1,I,5,K) = (CC(1,I,K,1)+TR12*
     +                      ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,
     +                      2))+ (WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,
     +                      I-1,K,5)))+TR11* ((WA2(I-2)*CC(1,I,K,
     +                      3)-WA2(I-1)*CC(1,I-1,K,3))+ (WA3(I-2)*CC(1,
     +                      I,K,4)-WA3(I-1)*CC(1,I-1,K,4)))) +
     +                      (TI12* ((WA4(I-2)*CC(1,I-1,K,
     +                      5)+WA4(I-1)*CC(1,I,K,5))- (WA1(I-2)*CC(1,
     +                      I-1,K,2)+WA1(I-1)*CC(1,I,K,2)))-
     +                      TI11* ((WA3(I-2)*CC(1,I-1,K,
     +                      4)+WA3(I-1)*CC(1,I,K,4))- (WA2(I-2)*CC(1,
     +                      I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))
              CH(1,IC,4,K) = (TI12* ((WA4(I-2)*CC(1,I-1,K,
     +                       5)+WA4(I-1)*CC(1,I,K,5))- (WA1(I-2)*CC(1,
     +                       I-1,K,2)+WA1(I-1)*CC(1,I,K,2)))-
     +                       TI11* ((WA3(I-2)*CC(1,I-1,K,
     +                       4)+WA3(I-1)*CC(1,I,K,4))- (WA2(I-2)*CC(1,
     +                       I-1,K,3)+WA2(I-1)*CC(1,I,K,3)))) -
     +                       (CC(1,I,K,1)+TR12* ((WA1(I-2)*CC(1,I,K,
     +                       2)-WA1(I-1)*CC(1,I-1,K,2))+ (WA4(I-2)*CC(1,
     +                       I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))+
     +                       TR11* ((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,
     +                       I-1,K,3))+ (WA3(I-2)*CC(1,I,K,
     +                       4)-WA3(I-1)*CC(1,I-1,K,4))))
  102     CONTINUE
  103 CONTINUE
      RETURN
      END
