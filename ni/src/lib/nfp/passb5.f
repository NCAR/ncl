      SUBROUTINE PASSB5(IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
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
      DOUBLE PRECISION TI2
      DOUBLE PRECISION TI4
      DOUBLE PRECISION TI3
      DOUBLE PRECISION TR5
      DOUBLE PRECISION TR2
      DOUBLE PRECISION TR4
      DOUBLE PRECISION TR3
      DOUBLE PRECISION CR2
      DOUBLE PRECISION CI2
      DOUBLE PRECISION CR3
      DOUBLE PRECISION CI3
      DOUBLE PRECISION CR5
      DOUBLE PRECISION CI5
      DOUBLE PRECISION CR4
      DOUBLE PRECISION CI4
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

      IF (IDO.NE.2) GO TO 102
      DO 101 K = 1,L1
          TI5 = CC(2,2,K) - CC(2,5,K)
          TI2 = CC(2,2,K) + CC(2,5,K)
          TI4 = CC(2,3,K) - CC(2,4,K)
          TI3 = CC(2,3,K) + CC(2,4,K)
          TR5 = CC(1,2,K) - CC(1,5,K)
          TR2 = CC(1,2,K) + CC(1,5,K)
          TR4 = CC(1,3,K) - CC(1,4,K)
          TR3 = CC(1,3,K) + CC(1,4,K)
          CH(1,K,1) = CC(1,1,K) + TR2 + TR3
          CH(2,K,1) = CC(2,1,K) + TI2 + TI3
          CR2 = CC(1,1,K) + TR11*TR2 + TR12*TR3
          CI2 = CC(2,1,K) + TR11*TI2 + TR12*TI3
          CR3 = CC(1,1,K) + TR12*TR2 + TR11*TR3
          CI3 = CC(2,1,K) + TR12*TI2 + TR11*TI3
          CR5 = TI11*TR5 + TI12*TR4
          CI5 = TI11*TI5 + TI12*TI4
          CR4 = TI12*TR5 - TI11*TR4
          CI4 = TI12*TI5 - TI11*TI4
          CH(1,K,2) = CR2 - CI5
          CH(1,K,5) = CR2 + CI5
          CH(2,K,2) = CI2 + CR5
          CH(2,K,3) = CI3 + CR4
          CH(1,K,3) = CR3 - CI4
          CH(1,K,4) = CR3 + CI4
          CH(2,K,4) = CI3 - CR4
          CH(2,K,5) = CI2 - CR5
  101 CONTINUE
      RETURN
  102 DO 104 K = 1,L1
          DO 103 I = 2,IDO,2
              TI5 = CC(I,2,K) - CC(I,5,K)
              TI2 = CC(I,2,K) + CC(I,5,K)
              TI4 = CC(I,3,K) - CC(I,4,K)
              TI3 = CC(I,3,K) + CC(I,4,K)
              TR5 = CC(I-1,2,K) - CC(I-1,5,K)
              TR2 = CC(I-1,2,K) + CC(I-1,5,K)
              TR4 = CC(I-1,3,K) - CC(I-1,4,K)
              TR3 = CC(I-1,3,K) + CC(I-1,4,K)
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
              CH(I-1,K,2) = WA1(I-1)*DR2 - WA1(I)*DI2
              CH(I,K,2) = WA1(I-1)*DI2 + WA1(I)*DR2
              CH(I-1,K,3) = WA2(I-1)*DR3 - WA2(I)*DI3
              CH(I,K,3) = WA2(I-1)*DI3 + WA2(I)*DR3
              CH(I-1,K,4) = WA3(I-1)*DR4 - WA3(I)*DI4
              CH(I,K,4) = WA3(I-1)*DI4 + WA3(I)*DR4
              CH(I-1,K,5) = WA4(I-1)*DR5 - WA4(I)*DI5
              CH(I,K,5) = WA4(I-1)*DI5 + WA4(I)*DR5
  103     CONTINUE
  104 CONTINUE
      RETURN
      END
