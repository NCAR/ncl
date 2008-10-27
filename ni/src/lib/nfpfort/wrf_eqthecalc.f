c                                                                     c
c*********************************************************************c
c                                                                     c
      SUBROUTINE DEQTHECALC(QVP,TMK,PRS,ETH,MIY,MJX,MKZH)
      DOUBLE PRECISION QVP
      DOUBLE PRECISION TMK
      DOUBLE PRECISION PRS
      DOUBLE PRECISION ETH
      DOUBLE PRECISION Q
      DOUBLE PRECISION T
      DOUBLE PRECISION P
      DOUBLE PRECISION E
      DOUBLE PRECISION TLCL
c
c water vapor mixing ratio [g/kg]
      DIMENSION QVP(MIY,MJX,MKZH)
c temperature [K]
      DIMENSION TMK(MIY,MJX,MKZH)
c pressure [mb]
      DIMENSION PRS(MIY,MJX,MKZH)
c equivalent potential temperature [K]
      DIMENSION ETH(MIY,MJX,MKZH)
c
c     thermodynamic constants
      DOUBLE PRECISION RGAS,RGASMD,CP,CPMD,GAMMA,GAMMAMD,THTECON1,
     +                 THTECON2,THTECON3,EPS,TLCLC1,TLCLC2,TLCLC3,TLCLC4
c
c   Define constants. 
c   Many are taken from Bolton (1980, MWR 108,1046-1053).
c
cJ/K/kg
      RGAS = 287.04D0
c rgas_moist=rgas*(1.+rgasmd*qvp)
      RGASMD = .608D0
c J/K/kg  Note: not using Bolton's value of 1005.7
      CP = 1004.D0
c cp_moist=cp*(1.+cpmd*qvp)
      CPMD = .887D0
      GAMMA = RGAS/CP
c gamma_moist=gamma*(1.+gammamd*qvp)
      GAMMAMD = RGASMD - CPMD
c K
      THTECON1 = 3376.D0
      THTECON2 = 2.54D0
      THTECON3 = .81D0
      TLCLC1 = 2840.D0
      TLCLC2 = 3.5D0
      TLCLC3 = 4.805D0
      TLCLC4 = 55.D0
      EPS = 0.622D0
c
c
      DO 1000 K = 1,MKZH
          DO 1000 J = 1,MJX - 1
              DO 1000 I = 1,MIY - 1
                  Q = MAX(QVP(I,J,K),1.D-15)
                  T = TMK(I,J,K)
                  P = PRS(I,J,K)
                  E = Q*P/ (EPS+Q)
                  TLCL = TLCLC1/ (LOG(T**TLCLC2/E)-TLCLC3) + TLCLC4
                  ETH(I,J,K) = T* (1000.D0/P)**
     +                         (GAMMA* (1.D0+GAMMAMD*Q))*
     +                         EXP((THTECON1/TLCL-THTECON2)*Q*
     +                         (1.D0+THTECON3*Q))
 1000 CONTINUE
      RETURN
      END
