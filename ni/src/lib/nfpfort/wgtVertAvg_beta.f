C NCLFORTSTART
      SUBROUTINE DWVBETAP1(MLON,NLAT,KLEV,P,X,XMSG,PSFC,IPUNIT,
     +                     IOPT,PTOP,PBOT,XVB,IER)
      IMPLICIT NONE

C     NCL:    x_wva =  wgtVertBeta (p, x, psfc, ipunit, iopt)

C                                                 INPUT
      INTEGER MLON,NLAT,KLEV,IPUNIT,IOPT,IER
      DOUBLE PRECISION P(KLEV),X(MLON,NLAT,KLEV),PSFC(MLON,NLAT),XMSG,
     +                 PTOP,PBOT
C                                                 OUTPUT
      DOUBLE PRECISION XVB(MLON,NLAT)
C NCLEND

C WEIGHTED VERTICAL SUM or AVERAGE USING BETA FACTORS
c nomenclature:
c .   p      - pressure levels
c .   x      - quantity to be weighted by pressure interval
c .   klev   - number of levels
c .   xmsg   - missing data
c .   psfc   - surface pressure
c .   ipunit - units flag for pressure:   0={mb,hPa}  1={Pa}
c .   wva    - result
c .   ier    - error code
c                                                 local
c -------------------------------------------------------------------
c Reference:
c Climate Diagnostics from Global Analyses: Conservation of Mass ....
c Kevin Trenberth
c July, 1991,  pp 707-
c -------------------------------------------------------------------
      INTEGER ML,NL,KL,PFLAG,KK,KLVL
      DOUBLE PRECISION PWRK(KLEV),XWRK(KLEV)
c                                                 initialize return
      DO NL = 1,NLAT
          DO ML = 1,MLON
              XVB(ML,NL) = XMSG
          END DO
      END DO
c                                     top-to-bot (+1) or bot-to-top (-1)
      PFLAG = 1
      IF (P(2).LT.P(1)) PFLAG = -1

      DO NL = 1,NLAT
          DO ML = 1,MLON
              KLVL = 0
c                                              ensure top to bot order
c                                              strip missing levels
              IF (PFLAG.EQ.1) THEN
                  DO KL = 1,KLEV
                      IF (X(ML,NL,KL).NE.XMSG) THEN
                          KLVL = KLVL + 1
                          PWRK(KLVL) = P(KL)
                          XWRK(KLVL) = X(ML,NL,KL)
                      END IF
                  END DO
              ELSE
                  DO KL = 1,KLEV
                      IF (X(ML,NL,KLEV+1-KL).NE.XMSG) THEN
                          KLVL = KLVL + 1
                          PWRK(KLVL) = P(KLEV+1-KL)
                          XWRK(KLVL) = X(ML,NL,KLEV+1-KL)
                      END IF
                  END DO
              END IF

              IF (KLVL.GE.2) THEN
                  CALL DWVBETAP(KLVL,PWRK,XWRK,PSFC(ML,NL),IPUNIT,IOPT,
     +                          PTOP,PBOT,XVB(ML,NL),IER)
              END IF

          END DO
      END DO

      RETURN
      END
C     ===================
C NCLFORTSTART
      SUBROUTINE DWVBETAP3(MLON,NLAT,KLEV,P,X,XMSG,PSFC,IPUNIT,
     +                     IOPT,PTOP,PBOT,XVB,IER)
      IMPLICIT NONE

C     NCL:    x_wva =  wgtVertBeta (p, x, psfc, ipunit, iopt)

C                                                 INPUT
      INTEGER MLON,NLAT,KLEV,IPUNIT,IOPT,IER
      DOUBLE PRECISION P(MLON,NLAT,KLEV),X(MLON,NLAT,KLEV),
     +                 PSFC(MLON,NLAT),XMSG,PTOP,PBOT
C                                                 OUTPUT
      DOUBLE PRECISION XVB(MLON,NLAT)
C NCLEND

      INTEGER ML,NL,KL,PFLAG,KK,KLVL
      DOUBLE PRECISION PWRK(KLEV),XWRK(KLEV)
c                                                 initialize return
      DO NL = 1,NLAT
          DO ML = 1,MLON
              XVB(ML,NL) = XMSG
          END DO
      END DO
c                                    top-to-bot (+1) or bot-to-top (-1)
      PFLAG = 1
      IF (P(1,2,2).LT.P(1,1,1)) PFLAG = -1

      DO NL = 1,NLAT
          DO ML = 1,MLON
              KLVL = 0
c                                            ensure top to bot order
c                                            strip missing levels
              IF (PFLAG.EQ.1) THEN
                  DO KL = 1,KLEV
                      IF (X(ML,NL,KL).NE.XMSG) THEN
                          KLVL = KLVL + 1
                          PWRK(KLVL) = P(ML,NL,KL)
                          XWRK(KLVL) = X(ML,NL,KL)
                      END IF
                  END DO
              ELSE
                  DO KL = 1,KLEV
                      IF (X(ML,NL,KLEV+1-KL).NE.XMSG) THEN
                          KLVL = KLVL + 1
                          PWRK(KLVL) = P(ML,NL,KLEV+1-KL)
                          XWRK(KLVL) = X(ML,NL,KLEV+1-KL)
                      END IF
                  END DO
              END IF

              IF (KLVL.GE.3) THEN
                  CALL DWVBETAP(KLVL,PWRK,XWRK,PSFC(ML,NL),IPUNIT,IOPT,
     +                          PTOP,PBOT,XVB(ML,NL),IER)
              END IF

          END DO
      END DO

      RETURN
      END
C     ===================
      SUBROUTINE DWVBETAP(KLVL,P,XP,PSFC,IPUNIT,IOPT,PTOP,PBOT,XVB,KER)
      IMPLICIT NONE
C                                                 arguments
      INTEGER KLVL,IPUNIT,IOPT,KER
      DOUBLE PRECISION P(KLVL),XP(KLVL),PSFC,PBOT,PTOP,XVB
C                                                 local
      INTEGER KL,KK,KLMAX
      DOUBLE PRECISION PP(0:2*KLVL),XPP(0:2*KLVL),DPP(0:2*KLVL),
     +                 BETA(0:2*KLVL),WSUM,PTOPF,PBOTF,PBOTX


      KER = 0
C                                                 fixed  limits
      PTOPF = 0.0D0
      PBOTF = 1100.D0
      IF (IPUNIT.GT.0) THEN
          PBOTF = PBOTF*100.D0
      END IF

      KK = 0
      KLMAX = 2*KLVL
      PP(0) = PTOPF
      XPP(0) = XP(1)
      DO KL = 1,KLMAX - 1,2
          KK = KK + 1
          PP(KL) = P(KK)
          XPP(KL) = XP(KK)
      END DO
      PP(KLMAX) = PBOTF
      XPP(KLMAX) = XP(KLVL)
c                                      xpp(2,4,..,klmax-2) not used
      DO KL = 2,KLMAX - 2,2
          PP(KL) = (PP(KL-1)+PP(KL+1))*0.5D0
          XPP(KL) = (XPP(KL-1)+XPP(KL+1))*0.5D0
      END DO
C                                                  fixed layer thickness
      DPP(0) = PP(2)
      DO KL = 1,KLMAX - 1
          DPP(KL) = PP(KL+1) - PP(KL-1)
      END DO
      DPP(KLMAX) = PBOTF - PP(KLMAX-1)

      DO KL = 1,KLMAX
          BETA(KL) = 0.0D0
      END DO

      PBOTX = PSFC
      IF (PBOT.LT.PSFC) PBOTX = PBOT

      DO KL = 1,KLMAX - 1,2
          IF (PBOTX.LT.PP(KL+1) .AND. PBOTX.GE.PP(KL-1)) THEN
              BETA(KL) = (PBOTX-PP(KL-1))/ (PP(KL+1)-PP(KL-1))

          ELSE IF (PTOP.GT.PP(KL-1) .AND. PTOP.LE.PP(KL+1)) THEN
              BETA(KL) = (PTOP-PP(KL-1))/ (PP(KL+1)-PP(KL-1))

          ELSE IF (PP(KL).LT.PBOTX .AND. PP(KL).GE.PTOP) THEN
              BETA(KL) = 1.0D0
          END IF

      END DO

c calculate the weghted sum [integration] or weighted average
c only odd levels are used

      XVB = 0.0D0
      WSUM = 0.0D0
      DO KL = 1,KLMAX,2
          XVB = XVB + XPP(KL)*BETA(KL)*DPP(KL)
          WSUM = WSUM + BETA(KL)*DPP(KL)
      END DO

      IF (IOPT.EQ.1) THEN
          IF (WSUM.GT.0.D0) THEN
              XVB = XVB/WSUM
          END IF
      END IF

      RETURN
      END
