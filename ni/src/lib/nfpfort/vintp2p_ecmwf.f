C NCLFORTSTART
      SUBROUTINE VINTP2PECMWF(DATI,DATO,PRESI,PLEVI,PLEVO 
     +                       ,INTYP,PSFC,SPVL,KXTRP,IMAX,NLAT 
     +                       ,NLEVI,NLEVIP1,NLEVO,VARFLG,TBOT,PHIS)
      IMPLICIT NONE

      INTEGER INTYP,IMAX,NLAT,NLEVI,NLEVIP1,NLEVO,KXTRP,VARFLG
C
      DOUBLE PRECISION DATI(IMAX,NLAT,NLEVI),DATO(IMAX,NLAT,NLEVO),
     +                 PRESI(IMAX,NLAT,NLEVI),PLEVI(NLEVIP1),
     +                 PLEVO(NLEVO),PSFC(IMAX,NLAT), SPVL

      DOUBLE PRECISION TBOT(IMAX,NLAT),PHIS(IMAX,NLAT)
C NCLEND
C
C****     THIS ROUTINE INTERPLOATES DATA at pressure points
C****     TO PRESSURE COORDINATES USING PRESSURE SURFACES AS THE
C****     COORDINATE SURFACE WHERE THE INTERPOLATION IS DONE.  THE
C****     TYPE OF INTERPOLATION IS CURRENTLY A VARIANT OF TRANSFORMED
C****     PRESSURE COORDINATES WITH THE  INTERPOLATION TYPE
C****     SPECIFIED BY INTYP.
C
C****     This routine was cannibalized from VINTH2PECMWF, not by
C****     Dennis Shea, but by Scientist X who wishes to remain anonymous.
C****     The calculation of pressures on hybrid surfaces was replaced
C****     by the input pressure field pressi. Otherwise it is the same.
C
C****     ON INPUT-
C****        DATI    - 3 DIMENSIONAL ARRAY (I,J,KI) CONTAINING DATA
C****                  ON HYBRID SURFACES  WHERE I IS LONGTIUDE, J
C****                  IS LATITUDE AND K IS THE VERTICAL HYBRID
C****                  COORDINATE.  THE VERTICAL DATA RUN TOP TO BOTTOM.
C****                  SIGMA DATA WITH THE DATA ORDERED TOP TO BOTTOM.
C****        presi   - 3 dimensional array (I,J,KI) containing
C****                  pressure values of grid points
C****                  where I is longtiude, J
C****                  is latitude and K is the vertical grid.
C****                  The vertical data run top to bottom.
C****        PLEVI -  1 DIMENSIONAL ARRAY TO HOLD PRESSURE VALUES
C****                  OF pressure data FOR A VERTICAL COLUMN
C****                  SLICE
C****        PLEVO   - 1 dimensional LIST OF OUTPUT PRESSURE SURFACES
C****                  IN MB
C****                  LOW TO HIGH PRESSURE
C****        INTYP   - A FLAG INDICATING INTERPOLATION FOR EACH
C****                  FIELD (1 - LINEAR,2 - LOG ,3 - LOG LOG)
C****                  WHERE EACH INTERPOLATION IS DONE IN TRANSFORMED
C****                  PRESSURE COORDINATES.
C****        PSFC    - MODEL SFC PRESSURE IN PASCALS (WILL BE CONVERTED
C****                  TO MB)
C****        VCOLI   - ARRAY TO STORE A LONGITUDINAL VERTICAL SLICE OF
C****                  INPUT DATA (IMAX BY NLEVI).
C****        VCOLO   - SAME BUT FOR OUTPUT DATA (IMAX BY NLEVO)
C****        IMAX    - LONGITUDINAL DIMENSION OF THE DATA.
C****        NLAT    - LATITUDINAL DIMENSION OF THE DATA.
C****        NLEVI   - NO. OF LEVELS FOR THE HYBRID DATA
C****        NLEVO   - NUMBER OF OUTPUT LEVELS FOR PRESSURE DATA
C****        KXTRP   - FLAG WHICH INDICATES WHETHER OR NOT
C****                  EXTRAPOLATION WILL BE USED WHEN THE OUTPUT
C****                  PRESSURE SURFACE IS BELOW THE LOWEST LEVEL
C****                  OF THE MODEL.
C****                     0 - DON'T EXTRAPOLATE USE SPECIAL VALUE SPVL
C****                     1 - EXTRAPOLATE DATA using ECMWF formulation
c****                         below PSFC
C****        SPVL    - SPECIAL VALUE TO USE WHEN DATA IS NOT
C****                  EXTRAPOLATED
C****        varflg  - flag which indicates the name of the variable
c****                  -1 means geopotential (Z)
c****                  +1 means geopotential (T)
c****                   0 any other variable
c****        tbot    - temperature at level closest to ground
c****        phis    - surface geopotential
C
C****     ON OUTPUT-
C****        DATO  - 3 DIMENSIONAL ARRAY TO HOLD DATA INTERPOLATED
C****                TO PRESSURE SURFACES.
C
      INTEGER I,J,K,KP,KPI,IPRINT
      DOUBLE PRECISION TSTAR,HGT,ALNP,T0,TPLAT,TPRIME0,ALPHA,
     +                 ALPH,PSFCMB

c for ecmwf extrapolation
      DOUBLE PRECISION RD,GINV
      PARAMETER (RD=287.04D0)
      PARAMETER (GINV=1.D0/9.80616D0)
      PARAMETER (ALPHA=0.0065D0*RD*GINV)
C
C
C
C****     STATEMENT FCN. FOR DOUBLE LOG. INTERP ON PRESSURE SURFACES
C****     PRESUMES PRESSURE IS IN MB
C
      DOUBLE PRECISION A1, A2LN
      A2LN(A1) = LOG(LOG(A1+2.72D0))
C
C****     STATEMENT FCN. FOR DOUBLE LOG. INTERP ON SIGMA SURFACES.
C****     SETS UP ROUGH UPPER BPOUND SIMILAR TO STATEMENT FCN FOR
C****     PRESSURE. I.E.    FIXED VALUE LN(LN(P) = LN(LN(FIXED VAL)
C****     AT .001 SIGMA OR ABOUT 1 MB
C
C     A2LN(A1)=LOG(LOG(A1+1.001))

C****

C
      DO 70 J = 1,NLAT
          DO 60 I = 1,IMAX
c =======================================DJS special case===
              IF (PSFC(I,J).EQ.SPVL) THEN
                  DO K = 1,NLEVO
                      DATO(I,J,K) = SPVL
                  END DO
                  GO TO 60
              END IF
c =========================================================

C
C****     GET PRESSURE VALUES FOR HYBRID SURFACES FOR THIS POINT
C****     AND FOR THE TYPE OF MODEL SURFACE THE DATA IS ON.
C****     INTERFACE DATA STARTS AT THE SECOND INTERFACE LEVEL SO
C****     IF THE DATA IS ON THOSE LEVELS START THE
C
              DO K = 1,NLEVI
                  KPI = K
                  PLEVI(K) = presi(I,J,K)
              END DO
C
C****     CALL P2HBD TO PERFORM VERTICAL INTERP. THEN TRANSFER DATA TO
C****     THE OUTPUT ARRAY
C
              DO 50 K = 1,NLEVO
C
C****     CHECK FOR BRACKETING LEVEL KP WILL BE THE INPUT LEVEL THAT
C****     IS THE UPPER PORTION OF 2 INPUT BRACKETING LEVELS.
C
C****     IF BRANCH FOR MODEL TOP
C
                  IF (PLEVO(K).LE.PLEVI(1)) THEN
                      KP = 1
                      GO TO 30
C
C****     IF BRANCH FOR LEVEL BELOW LOWEST HYBRID LEVEL
C
                  ELSE IF (PLEVO(K).GT.PLEVI(NLEVI)) THEN
                      IF (KXTRP.EQ.0) THEN
                          DATO(I,J,K) = SPVL
                          GO TO 40
                      ELSE IF (VARFLG.GT.0) THEN
c Variable is "T" and ECMWF extrapolation is desired
                          PSFCMB = PSFC(I,J)*0.01D0
                          TSTAR = DATI(I,J,NLEVI)*
     +                            (1.D0+ALPHA* (PSFCMB/PLEVI(NLEVI)-1))
                          HGT = PHIS(I,J)*GINV
                          IF (HGT.LT.2000.D0) THEN
                              ALNP = ALPHA*LOG(PLEVO(K)/PSFCMB)
                          ELSE
                              T0 = TSTAR + 0.0065D0*HGT
                              TPLAT = MIN(T0,298.D0)
                              IF (HGT.LE.2500.D0) THEN
                                  TPRIME0 = 0.002D0*
     +                                      ((2500.D0-HGT)*T0+ (HGT-
     +                                      2000.D0)*TPLAT)
                              ELSE
                                  TPRIME0 = TPLAT
                              END IF
                              IF (TPRIME0.LT.TSTAR) THEN
                                  ALNP = 0.D0
                              ELSE
                                  ALNP = RD* (TPRIME0-TSTAR)/PHIS(I,J)*
     +                                   LOG(PLEVO(K)/PSFCMB)
                              END IF
                          END IF
                          DATO(I,J,K) = TSTAR* (1.D0+ALNP+.5D0*ALNP**2+
     +                                  1.D0/6.D0*ALNP**3)
                          GO TO 40

                      ELSE IF (VARFLG.LT.0) THEN
c Variable is "Z" and ECMWF extrapolation is desired
                          PSFCMB = PSFC(I,J)*0.01D0
                          HGT = PHIS(I,J)*GINV
                          TSTAR = TBOT(I,J)* (1.D0+
     +                            ALPHA* (PSFCMB/PLEVI(NLEVI)-1.D0))
                          T0 = TSTAR + 0.0065D0*HGT

                          IF (TSTAR.LE.290.5D0 .AND. T0.GT.290.5D0) THEN
                              ALPH = RD/PHIS(I,J)* (290.5D0-TSTAR)
                          ELSE IF (TSTAR.GT.290.5D0 .AND.
     +                             T0.GT.290.5D0) THEN
                              ALPH = 0
                              TSTAR = 0.5D0* (290.5D0+TSTAR)
                          ELSE
                              ALPH = ALPHA
                          END IF

                          IF (TSTAR.LT.255.D0) THEN
                              TSTAR = 0.5D0* (TSTAR+255.D0)
                          END IF
                          ALNP = ALPH*LOG(PLEVO(K)/PSFCMB)
                          DATO(I,J,K) = HGT - RD*TSTAR*GINV*
     +                                  LOG(PLEVO(K)/PSFCMB)*
     +                                  (1.D0+.5D0*ALNP+
     +                                  1.D0/6.D0*ALNP**2)
                          GO TO 40
                      ELSE
C Use lowest sigma layer
                          DATO(I,J,K) = DATI(I,J,NLEVI)
                          GO TO 40
                      END IF
C
C****     IF BRANCH FOR TO CHECK IF OUTPUT LEVEL IN BETWEEN
C****     2 LOWEST HYBRID LEVELS
C
                  ELSE IF (PLEVO(K).GE.PLEVI(NLEVI-1)) THEN
                      KP = NLEVI - 1
                      GO TO 30
C
C****     IF BRANCH FOR MODEL INTERIOR
C****     LOOP THROUGH INPUT LEVELS TILL YOU ARE BRACKETING
C****     OUTPUT LEVEL
C
                  ELSE
                      KP = 0
   20                 CONTINUE
                      KP = KP + 1
                      IF (PLEVO(K).LE.PLEVI(KP+1)) GO TO 30
                      IF (KP.GT.NLEVI) THEN
                          WRITE (6,FMT=25) KP,NLEVI
   25                     FORMAT (' KP.GT.KLEVI IN P2HBD.  KP,KLEVI= ',
     +                           2I5)
C            CALL DPRNT(' PLEVI',PLEVI,NLEVIP1,1,1,1)
C            CALL DPRNT(' PLEVO',PLEVO,NLEVO,1,1,1)
C            CALL ABORT(' KP.GT.NLEVI IN P2HBD')
                      END IF
                      GO TO 20
                  END IF
   30             CONTINUE
C
C****     LEVEL BRACKETED PICK TYPE OF INTERP.
C
C
C****     LINEAR INTERP.
C
                  IF (INTYP.EQ.1) THEN
                      DATO(I,J,K) = DATI(I,J,KP) +
     +                              (DATI(I,J,KP+1)-DATI(I,J,KP))*
     +                              (PLEVO(K)-PLEVI(KP))/
     +                              (PLEVI(KP+1)-PLEVI(KP))
C
C****     LOG INTERPOLATION.
C
                  ELSE IF (INTYP.EQ.2) THEN
                      IPRINT = 1
C      IF (I.EQ.1.AND.IPRINT.EQ.1) THEN
C         PRINT 101,I,J,K,KP,ILEV
C  101    FORMAT('  IN S2HBD I,J,K,KP,ILEV ',5I3)
C         PRINT 102,DATI(I,J,KP),DATI(I,J,KP+1),PLEVO(K),
C     *             PLEVI(KP),PLEVI(KP+1)
C  102    FORMAT(' DATI(KP),DATI(KP+1),PLEVO(K),',
C     *          'PLEVI(KP),PLEVI(KP+1) ',
C     *          /,1X,1P5E12.5)
C      ENDIF
                      DATO(I,J,K) = DATI(I,J,KP) +
     +                              (DATI(I,J,KP+1)-DATI(I,J,KP))*
     +                              LOG(PLEVO(K)/PLEVI(KP))/
     +                              LOG(PLEVI(KP+1)/PLEVI(KP))
C
C****     FOR LOG LOG INTERP. NOTE A2LN IS A STATEMENT FCN.
C
                  ELSE IF (INTYP.EQ.3) THEN
                      DATO(I,J,K) = DATI(I,J,KP) +
     +                              (DATI(I,J,KP+1)-DATI(I,J,KP))*
     +                              (A2LN(PLEVO(K))-A2LN(PLEVI(KP)))/
     +                              (A2LN(PLEVI(KP+1))-A2LN(PLEVI(KP)))
                  END IF
   40             CONTINUE
C
   50         CONTINUE
   60     CONTINUE
   70 CONTINUE
      RETURN
      END
