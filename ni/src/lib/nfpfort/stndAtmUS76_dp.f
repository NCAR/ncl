C NCLFORTSTART
      SUBROUTINE DSTDATMZ(NZ,Z,TT,DD,PP)
      IMPLICIT NONE
c                                   INPUT
      INTEGER NZ
      DOUBLE PRECISION Z(NZ)
c                                   OUTPUT
      DOUBLE PRECISION TT(NZ), DD(NZ), PP(NZ)
C NCLEND

C NCL:  tdp = stdAtmUsa76_z2tdp (z)

      INTEGER N,LFLAG

c multiple level interface for DSS "STDZ2P"
C  .  computes pressure, temperature, and density from input heights
C  .  based on US STANDARD ATMOSPHERE, 1976
C  .  values not valid above 84852 m.
C  Original source: NCAR, Data Support Section
C  .  http://dss.ucar.edu/libraries/meteorology/

c input only in meters from NCL
      LFLAG = 0

      DO N = 1,NZ
          IF (Z(N).GE.0.D0 .AND. Z(N).LE.84852.D0) THEN
              CALL STDZ2P(PP(N),Z(N),LFLAG,TT(N),DD(N))
          ELSE
              TT(N) = -999.D0
              DD(N) = -999.D0
              PP(N) = -999.D0
          END IF
      END DO

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DSTDATMP(NP,P,TT,DD,ZZ)
      IMPLICIT NONE
c                                   INPUT
      INTEGER NP
      DOUBLE PRECISION P(NP)
c                                   OUTPUT
      DOUBLE PRECISION TT(NP), DD(NP), ZZ(NP)
C NCLEND

C NCL:  tdz = stdAtmUsa76_p2tdz (p)

      INTEGER N
      DOUBLE PRECISION ZFT

c multiple level interface for DSS "STDP2Z"
C .   computes height, temperature, and density from input pressures
C .   based on US STANDARD ATMOSPHERE, 1976
C .   values not valid above 84852 m.
c .   do not return 'zft'
C  Original source: NCAR, Data Support Section
C  .  http://dss.ucar.edu/libraries/meteorology/

      DO N = 1,NP
          IF (P(N).GE.0.00373D0 .AND. P(N).LE.1013.25D0) THEN
              CALL STDP2Z(P(N),ZZ(N),ZFT,TT(N),DD(N))
          ELSE
              TT(N) = -999.D0
              DD(N) = -999.D0
              ZZ(N) = -999.D0
          END IF
      END DO

      RETURN
      END
C --------------------------------------------------
      SUBROUTINE STDZ2P(P,H1,L,TCENT,RHO)
      IMPLICIT NONE
      INTEGER L
      DOUBLE PRECISION P,H1,TCENT,RHO
C
C COMPUTES PRESSURE, TEMPERATURE, AND DENSITY VALUES FROM INPUT HEIGHTS
C  BASED ON US STANDARD ATMOSPHERE, 1976
C  VALUES NOT VALID ABOVE 84852 KM.
C
C INPUT
C       H1    - HEIGHT IN FEET OR METERS
C       L     - UNITS FLAG - 0=METERS, 1=FEET
C OUTPUT
C       P     - PRESSURE IN MB
C       TCENT - TEMPERATURE IN DEG C
C       RHO   - DENSITY IN KG/M3
C
      INTEGER I
      DOUBLE PRECISION HBASE(10),HTOP(10),TB(10),GRAD(10),ABST(10)
      DOUBLE PRECISION PB(11),RB(10),ABSZ,PZERP,G,R,CHECK,T,H,PZERO

      DATA HBASE/0.D0,1.1D4,2.D4,3.2D4,4.7D4,5.1D4,7.1D4,3*84852.0D0/
      DATA HTOP/1.1D4,2.D4,3.2D4,4.7D4,5.1D4,7.1D4,4*84852.0D0/
      DATA TB/15.D0,2*-56.5D0,-44.5D0,2*-2.5D0,-58.5D0,3*-86.2D0/
      DATA GRAD/-.0065D0,0.D0,0.001D0,0.0028D0,0.D0,-.0028D0,-.002D0,
     +     3*0.D0/
c djs DATA CHECK/0.D0/
C
C     H1 IS ALTITUDE: IF IN METERS SET L<=0.  IF IN FEET SET L>=1
C
c djs IF (CHECK.EQ.0.D0) THEN
          ABSZ  = 273.15D0
          PZERO = 1013.250D0
          G     = 980.665D0
          R     = 83143200.D0/28.9644D0
          PB(1) = PZERO

          DO I = 1,8
              ABST(I) = TB(I) + ABSZ
          END DO

          DO I = 1,8
              IF (GRAD(I).NE.0) THEN
                  PB(I+1) = PB(I)* (ABST(I+1)/ABST(I))**
     +                      (100.D0*G/ (-GRAD(I)*R))
              ELSE
                  PB(I+1) = PB(I)*2.7182818D0**
     +                      ((-100.D0*G/ (R*ABST(I)))*
     +                      (HTOP(I)-HBASE(I)))
              END IF
              RB(I) = 1000.D0*PB(I)/ (R*ABST(I))*1000.D0
          END DO
          CHECK = 10.D0
c djs END IF

      IF (L.GE.1) THEN
          H = H1*0.3048D0
      ELSE
          H = H1
      END IF

      DO I = 1,8
          IF (H.LE.HBASE(I+1)) THEN
              IF (GRAD(I).NE.0.D0) THEN
                  P = PB(I)* ((H-HBASE(I))*GRAD(I)/ABST(I)+1)**
     +                (-100.D0*G/ (GRAD(I)*R))
              ELSE
C                                 ISOTHERMAL LAYER
                  P = PB(I)*EXP((H-HBASE(I))* (-G)/ (0.01D0*R*ABST(I)))
              END IF

              T     = ABST(I) + GRAD(I)* (H-HBASE(I))
              TCENT = T - ABSZ
              RHO   = 1.D6*P/ (R*T)
              RETURN
          END IF
      END DO

      RETURN
      END
C --------------------------------------------------
      SUBROUTINE STDP2Z(P,H,HFT,TCENT,DENS)
      IMPLICIT NONE
      DOUBLE PRECISION P,H,HFT,TCENT,DENS
C
C COMPUTES HEIGHT, TEMPERATURE, AND DENSITY VALUES FROM INPUT PRESSURES
C  BASED ON US STANDARD ATMOSPHERE, 1976
C  VALUES NOT VALID ABOVE 84852 KM.
C
C INPUT
C       P     - PRESSURE IN MB
C OUTPUT
C       H     - HEIGHT IN METERS
C       HFT   - HEIGHT IN FEET
C       TCENT - TEMPERATURE IN DEG C
C       DENS  - DENSITY IN KG/M3
C
      INTEGER I
      DOUBLE PRECISION HBASE(10),HTOP(10),TB(10),GRAD(10),ABST(10),
     +                 PB(10),RB(10),ABSZ,G,R,T
      DATA HBASE/0.D0,1.1D4,2.D4,3.2D4,4.7D4,5.1D4,7.1D4,3*84852.0D0/
      DATA HTOP/1.1D4,2.D4,3.2D4,4.7D4,5.1D4,7.1D4,4*84852.0D0/
      DATA GRAD/-.0065D0,0.D0,0.001D0,0.0028D0,0.D0,-.0028D0,-.002D0,
     +     3*0.D0/
      DATA ABST/288.15D0,216.65D0,216.65D0,228.65D0,270.65D0,270.65D0,
     +     214.65D0,186.95D0,2*0.D0/
      DATA PB/1013.25D0,0.22632064D+03,0.54748887D+02,0.86801870D+01,
     +     0.11090631D+01,0.66938874D+00,0.39564205D-01,0.37338360D-02,
     +     2*0.D0/

      ABSZ = 273.15D0
      G = 980.665D0
      R = 83143200.D0/28.9644D0
      IF (P.LE.PB(1)) THEN
          DO I = 1,8
              IF (P.GE.PB(I+1)) GO TO 37
          END DO
      END IF


   37 CONTINUE
      IF (GRAD(I).NE.0.D0) THEN
          H = ABST(I)/ (-GRAD(I))* (1.D0-
     +        (P/PB(I))** (-GRAD(I)*R/ (100.D0*G))) + HBASE(I)
      ELSE
C  ISOTHERMAL LAYER
          H = HBASE(I) + 0.01D0*R*ABST(I)/G*DLOG(PB(I)/P)
      END IF

      T = ABST(I) + GRAD(I)* (H-HBASE(I))
      TCENT = T - ABSZ
      DENS = 1000*1000.D0*P/ (R*T)
      HFT = H/0.3048D0

      RETURN
      END
