C NCLFORTSTART
      SUBROUTINE DZPSIDRV(MLON,NLAT,KLEV,V,LAT,P,PS,VMSG,ZMPSI)
      IMPLICIT NONE

c NCL:  zmpsi = zon_mpsi(v,lat,p,ps)
c                                                        INPUT
      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION V(MLON,NLAT,KLEV),LAT(NLAT),P(KLEV),
     +                 PS(MLON,NLAT),VMSG
c                                                        OUTPUT
      DOUBLE PRECISION ZMPSI(NLAT,KLEV)
C NCLEND
c                                                        LOCAL
      DOUBLE PRECISION PTMP(2*KLEV+1),DP(2*KLEV+1),VVPROF(2*KLEV+1)
      DOUBLE PRECISION VBAR(NLAT,KLEV),VTMP(MLON,NLAT,KLEV)

      CALL DZONMPSI(MLON,NLAT,KLEV,V,LAT,P,PS,VMSG,ZMPSI,PTMP,DP,VTMP,
     +              VBAR,VVPROF)

      RETURN
      END
c --
      SUBROUTINE DZONMPSI(MLON,NLAT,KLEV,V,LAT,P,PS,VMSG,ZMPSI,PTMP,DP,
     +                    VTMP,VBAR,VVPROF)
      IMPLICIT NONE

      INTEGER MLON,NLAT,KLEV
      DOUBLE PRECISION V(MLON,NLAT,KLEV),LAT(NLAT),P(KLEV),
     +                 PS(MLON,NLAT),VMSG
c                                                        output
      DOUBLE PRECISION ZMPSI(NLAT,KLEV)
c                                             temporary / local
      DOUBLE PRECISION PTMP(0:2*KLEV),DP(0:2*KLEV),VVPROF(0:2*KLEV)
      DOUBLE PRECISION VTMP(MLON,NLAT,KLEV),VBAR(NLAT,KLEV)

      DOUBLE PRECISION PI,G,A,C,RAD,CON,PTOP,PBOT
      INTEGER ML,NL,KL,KNT,KFLAG

      G   = 9.80616D0
      A   = 6.37122D6
      PI  = 4.D0*ATAN(1.D0)
      RAD = PI/180.D0
      CON = 2.D0*PI*A/G

      PTOP = 500.D0
      PBOT = 100500.D0

c calculate press at all levels [even half levels]

      KNT = 0
      DO KL = 1,2*KLEV - 1,2
          KNT = KNT + 1
          PTMP(KL) = P(KNT)
      END DO

      DO KL = 2,2*KLEV - 2,2
          PTMP(KL) = (PTMP(KL+1)+PTMP(KL-1))*0.5D0
      END DO

      PTMP(0) = PTOP
      PTMP(2*KLEV) = PBOT

c dp at all levels

      DP(0) = 0.D0
      DO KL = 1,2*KLEV - 1
          DP(KL) = PTMP(KL+1) - PTMP(KL-1)
      END DO
      DP(2*KLEV) = 0.D0

c make copy; set any p > ps to msg

      KNT = 0
      DO ML = 1,MLON
          DO NL = 1,NLAT
              DO KL = 1,KLEV
                  IF (P(KL).GT.PS(ML,NL)) THEN
                      KNT = KNT + 1
                      VTMP(ML,NL,KL) = VMSG
                  ELSE
                      VTMP(ML,NL,KL) = V(ML,NL,KL)
                  END IF
              END DO
          END DO
      END DO

c compute zonal mean v using the vtmp variable

      DO NL = 1,NLAT
          DO KL = 1,KLEV
              KNT = 0
              VBAR(NL,KL) = 0.0D0
              DO ML = 1,MLON
                  IF (VTMP(ML,NL,KL).NE.VMSG) THEN
                      KNT = KNT + 1
                      VBAR(NL,KL) = VBAR(NL,KL) + VTMP(ML,NL,KL)
                  END IF
              END DO
              IF (KNT.GT.0) THEN
                  VBAR(NL,KL) = VBAR(NL,KL)/DBLE(KNT)
              ELSE
                  VBAR(NL,KL) = VMSG
              END IF
          END DO
      END DO

c compute mpsi at each latitude [reuse ptmp]

      DO NL = 1,NLAT
          C = CON*COS(LAT(NL)*RAD)

          PTMP(0) = 0.0D0
          DO KL = 1,2*KLEV
              PTMP(KL) = VMSG
          END DO

          DO KL = 0,2*KLEV,2
              VVPROF(KL) = 0.0D0
          END DO

          KNT = 0
          DO KL = 1,2*KLEV - 1,2
              KNT = KNT + 1
              VVPROF(KL) = VBAR(NL,KNT)
          END DO

c integrate from top of atmosphere down for each level where vbar
c is not missing

          DO KL = 1,2*KLEV - 1,2
              KFLAG = KL
              IF (VVPROF(KL).EQ.VMSG) GO TO 10
              PTMP(KL+1) = PTMP(KL-1) - C*VVPROF(KL)*DP(KL)
          END DO
   10     CONTINUE

c impose lower boundary condition to ensure the zmpsi is 0
c at the bottom boundary

          PTMP(KFLAG+1) = -PTMP(KFLAG-1)

c streamfunction is obtained as average from its' values
c at the intermediate half levels. The minus sign before
c PTMP in the last loop is to conform to a CSM convention.

          DO KL = 1,KFLAG,2
              PTMP(KL) = (PTMP(KL+1)+PTMP(KL-1))*0.5D0
          END DO

          KNT = 0
          DO KL = 1,2*KLEV - 1,2
              KNT = KNT + 1
              IF(PTMP(KL).NE.VMSG) THEN
                 ZMPSI(NL,KNT) = -PTMP(KL)
              ELSE
                 ZMPSI(NL,KNT) = PTMP(KL)
              END IF
          END DO
      END DO

      RETURN
      END
