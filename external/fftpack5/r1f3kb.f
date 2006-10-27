CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: r1f3kb.f,v 1.1 2006-10-27 16:16:31 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F3KB (IDO,L1,CC,IN1,CH,IN2,WA1,WA2)
      REAL       CC(IN1,IDO,3,L1)    ,CH(IN2,IDO,L1,3),
     1           WA1(IDO)   ,WA2(IDO)
C
      ARG=2.*4.*ATAN(1.0)/3.
      TAUR=COS(ARG)
      TAUI=SIN(ARG)
      DO 101 K=1,L1
         CH(1,1,K,1) = CC(1,1,1,K)+2.*CC(1,IDO,2,K)
         CH(1,1,K,2) = CC(1,1,1,K)+(2.*TAUR)*CC(1,IDO,2,K)
     1   -(2.*TAUI)*CC(1,1,3,K)
         CH(1,1,K,3) = CC(1,1,1,K)+(2.*TAUR)*CC(1,IDO,2,K)
     1   +2.*TAUI*CC(1,1,3,K)
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
        CH(1,I-1,K,1) = CC(1,I-1,1,K)+(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
        CH(1,I,K,1) = CC(1,I,1,K)+(CC(1,I,3,K)-CC(1,IC,2,K))
        CH(1,I-1,K,2) = WA1(I-2)*
     1 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))-
     * (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
     2                   -WA1(I-1)*
     3 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))+
     * (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
            CH(1,I,K,2) = WA1(I-2)*
     4 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))+
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
     5                  +WA1(I-1)*
     6 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))-
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
              CH(1,I-1,K,3) = WA2(I-2)*
     7 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))+
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
     8   -WA2(I-1)*
     9 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))-
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
            CH(1,I,K,3) = WA2(I-2)*
     1 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))-
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
     2                 +WA2(I-1)*
     3 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))+
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
