CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: mradb5.f,v 1.1 2006-10-27 16:16:30 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADB5 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,
     1       WA1,WA2,WA3,WA4)
      REAL   CC(IN1,IDO,5,L1)    ,CH(IN2,IDO,L1,5),
     1       WA1(IDO)     ,WA2(IDO)     ,WA3(IDO)     ,WA4(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      ARG=2.*4.*ATAN(1.0)/5.
      TR11=COS(ARG)
      TI11=SIN(ARG)
      TR12=COS(2.*ARG)
      TI12=SIN(2.*ARG)
      DO 101 K=1,L1
      M2 = M2S
      DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,K,1) = CC(M1,1,1,K)+2.*CC(M1,IDO,2,K)+2.*CC(M1,IDO,4,K)
         CH(M2,1,K,2) = (CC(M1,1,1,K)+TR11*2.*CC(M1,IDO,2,K)
     1   +TR12*2.*CC(M1,IDO,4,K))-(TI11*2.*CC(M1,1,3,K)
     1   +TI12*2.*CC(M1,1,5,K))
         CH(M2,1,K,3) = (CC(M1,1,1,K)+TR12*2.*CC(M1,IDO,2,K)
     1   +TR11*2.*CC(M1,IDO,4,K))-(TI12*2.*CC(M1,1,3,K)
     1   -TI11*2.*CC(M1,1,5,K))
         CH(M2,1,K,4) = (CC(M1,1,1,K)+TR12*2.*CC(M1,IDO,2,K)
     1   +TR11*2.*CC(M1,IDO,4,K))+(TI12*2.*CC(M1,1,3,K)
     1   -TI11*2.*CC(M1,1,5,K))
         CH(M2,1,K,5) = (CC(M1,1,1,K)+TR11*2.*CC(M1,IDO,2,K)
     1   +TR12*2.*CC(M1,IDO,4,K))+(TI11*2.*CC(M1,1,3,K)
     1   +TI12*2.*CC(M1,1,5,K))
 1001          CONTINUE
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
      DO 1002 M1=1,M1D,IM1
        M2 = M2+IM2
        CH(M2,I-1,K,1) = CC(M1,I-1,1,K)+(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K))
        CH(M2,I,K,1) = CC(M1,I,1,K)+(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +(CC(M1,I,5,K)-CC(M1,IC,4,K))
        CH(M2,I-1,K,2) = WA1(I-2)*((CC(M1,I-1,1,K)+TR11*
     1  (CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))+TR12
     1  *(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA1(I-1)*((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))+(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,2) = WA1(I-2)*((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)
     1  -CC(M1,IC,2,K))+TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI11*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))+TI12
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))+WA1(I-1)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)
     1  +CC(M1,IC-1,2,K))+TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))
     1  -(TI11*(CC(M1,I,3,K)+CC(M1,IC,2,K))+TI12
     1  *(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,3) = WA2(I-2)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1 -WA2(I-1)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,3) = WA2(I-2)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA2(I-1)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,4) = WA3(I-2)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA3(I-1)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  -(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,4) = WA3(I-2)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  -(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA3(I-1)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,5) = WA4(I-2)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA4(I-1)
     1  *((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))-(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,5) = WA4(I-2)
     1  *((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))-(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA4(I-1)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
 1002      CONTINUE
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
