      SUBROUTINE VINTH2P(DATI,DATO,HBCOFA,HBCOFB,P0,PLEVI,PLEVO,INTYP,
     +                   ILEV,PSFC,SPVL,KXTRP,IMAX,NLAT,NLEVI,NLEVIP1,
     +                   NLEVO)
C
C****     THIS ROUTINE INTERPLOATES CCM2/3 HYBRID COORDINATE DATA
C****     TO PRESSURE COORDINATES USING PRESSURE SURFACES AS THE
C****     COORDINATE SURFACE WHERE THE INTERPOLATION IS DONE.  THE
C****     TYPE OF INTERPOLATION IS CURRENTLY A VARIANT OF TRANSFORMED
C****     PRESSURE COORDINATES WITH THE  INTERPOLATION TYPE
C****     SPECIFIED BY INTYP.  ALL HYBRID COORDINATE VALUES ARE
C****     TRANSFORMED TO PRESSURE VALUES. WHERE THE
C****     FORMULA FOR THE PRESSURE OF A HYBRID SURFACE IS;
C****          P(K) = HBCOFA(LEVH,K)*P0 + HBCOFB(LEVH,K)*PSFC
C****     WHERE,
C****          HBCOFA - IS THE "A" OR PRESSURE HYBRID COEF
C****          LEVH   - IS THE LAYER SURFACE (INTERFACE=1 MIDPOINT=2)
C****          P0     - IS THE BASE PRESSURE IN MB
C****          K      - THE LEVEL INDEX (RUNNING FROM TOP TO BOTTOM)
C****          HBCOFB - IS THE "B" OR SIGMA COEFICIENT
C****          P(K)   - IS THE PRESSURE OF A HYBRID SURFACE IN MB.
C****          PSFC   - IS THE SURFACE PRESSURE IN PASCALS
C****                   (MB = .01*PASCALS
C
C****     FOR HYBRID DATA AT LEVEL INTERFACES SINCE THERE IS ONE
C****     MORE VERTICAL LEVEL FOR INTERFACES THAN FOR LEVEL MIDPOINTS
C****     IT IS ASSUNMED THAT THE FIRST INTERFACE LEVEL WITH A DATA
C****     VALUE IS THE SECOND LEVEL FROM THE TOP.
C
C****     ON INPUT-
C****        DATI    - 3 DIMENSIONAL ARRAY (I,J,KI) CONTAINING DATA
C****                  ON HYBRID SURFACES  WHERE I IS LONGTIUDE, J
C****                  IS LATITUDE AND K IS THE VERTICAL HYBRID
C****                  COORDINATE.  THE VERTICAL DATA RUN TOP TO BOTTOM.
C****                  SIGMA DATA WITH THE DATA ORDERED TOP TO BOTTOM.
C****        HBCOFA  - 2 DIMENSIONAL ARRAY CONTAINING "A" OR PRESSURE
C****                  COEFICIENTS FOR COMPUTING PRESSURE AT A LEVEL.
C****                  ARRAY IS 2XNLEVIP1.  THE 1ST INDEX TAKES ON
C****                  THE VALUE OF EITHER
C****                   1 - FOR LEVEL INTERFACES (OR 1/2 LEVELS) OR;
C****                   2 - FOR LEVEL MIDPOINTS  (OR FULL LEVELS WHERE
C****                       VIRTUALLY ALL VARIABLES ARE LOCATED)
C****                  NOTE THAT COEFICIENTS ARE SCALED TO YIELD A
C****                  PRESSURE IN MB.  THEY ARE ORDERED FROM TOP
C****                  OF THE MODEL TO THE BOTTOM.
C****        HBCOFB  - SAME AS HCOFA BUT FOR THE "B" OR SIGMA COEFICIENT
C****        P0      - BASE PRESSURE IN MB FOR COMPUTING PRESSURE
C****                  OF A HYBRID COORDINATE LEVEL
C****        PLEVI -  1 DIMENSIONAL ARRAY TO HOLD PRESSURE VALUES
C****                  OF HYBRID SURFACES FOR A VERTICAL COLUMN
C****                  SLICE
C****        PLEVO   - LIST OF OUTPUT PRESSURE SURFACES IN MB
C****                  LOW TO HIGH PRESSURE
C****        INTYP   - A FLAG INDICATING INTERPOLATION FOR EACH
C****                  FIELD (1 - LINEAR,2 - LOG ,3 - LOG LOG)
C****                  WHERE EACH INTERPOLATION IS DONE IN TRANSFORMED
C****                  PRESSURE COORDINATES.
C****        ILEV    - FLAG TO SHOW WHETHER FIELD IS ON LEVEL INTERFACE
C****                  1/2 LEVEL WHICH HAS A VALUE OF 1 OR A LEVEL
C****                  MIDPOINT A FULL LEVEL (WHERE ALMOST ALL VARIABLES
C****                  ARE LOCATED
C****        PSFC    - MODEL SFC PRESSURE IN PASCALS (WILL BE CONVERTED
C****                  TO MB)
C****        VCOLI   - ARRAY TO STORE A LONGITUDINAL VERTICAL SLICE OF
C****                  INPUT DATA (IMAX BY NLEVI).
C****        VCOLO   - SAME BUT FOR OUTPUT DATA (IMAX BY NLEVO)
C****        IMAX    - LONGITUDINAL DIMENSION OF THE DATA.
C****        NLAT    - LATITUDINAL DIMENSION OF THE DATA.
C****        NLEVI   - NO. OF LEVELS FOR THE HYBRID DATA
C****        NLEVIP1 - NLEVI + 1
C****        NLEVO   - NUMBER OF OUTPUT LEVELS FOR PRESSURE DATA
C****        KXTRP   - FLAG WHICH INDICATES WHETHER OR NOT
C****                  EXTRAPOLATION WILL BE USED WHEN THE OUTPUT
C****                  PRESSURE SURFACE IS BELOW THE LOWEST LEVEL
C****                  OF THE MODEL.
C****                     0 - DON'T EXTRAPOLATE USE SPECIAL VALUE SPVL
C****                     1 - EXTRAPOLATE DATA
C****        SPVL    - SPECIAL VALUE TO USE WHEN DATA IS NOT
C****                  EXTRAPOLATED
C
C****     ON OUTPUT-
C****        DATO  - 3 DIMENSIONAL ARRAY TO HOLD DATA INTERPOLATED
C****                TO PRESSURE SURFACES.
C
      IMPLICIT NONE
      DOUBLE PRECISION SPVL,DATI,DATO,HBCOFA,HBCOFB,PLEVI,PLEVO,PSFC,P0
      DOUBLE PRECISION A2LN,A1
      INTEGER INTYP,ILEV,IMAX,NLAT,NLEVI,NLEVIP1,NLEVO,KXTRP
      INTEGER I,J,K,KP,KPI,IPRINT
C
      DIMENSION DATI(IMAX,NLAT,NLEVI),DATO(IMAX,NLAT,NLEVO),
     +          HBCOFA(NLEVIP1),HBCOFB(NLEVIP1),PLEVI(NLEVIP1),
     +          PLEVO(NLEVO),PSFC(IMAX,NLAT)
C
C
C
C****     STATEMENT FCN. FOR DOUBLE ALOG. INTERP ON PRESSURE SURFACES
C****     PRESUMES PRESSURE IS IN MB
C
      A2LN(A1) = DLOG(DLOG(A1+2.72D0))
C
C****     STATEMENT FCN. FOR DOUBLE ALOG. INTERP ON SIGMA SURFACES.
C****     SETS UP ROUGH UPPER BPOUND SIMILAR TO STATEMENT FCN FOR
C****     PRESSURE. I.E.    FIXED VALUE LN(LN(P) = LN(LN(FIXED VAL)
C****     AT .001 SIGMA OR ABOUT 1 MB
C
C     A2LN(A1)=ALOG(ALOG(A1+1.001))

C
C****
C****
C
      DO 70 J = 1,NLAT
          DO 60 I = 1,IMAX
c ==========================================================
           if (psfc(i,j).eq.spvl) then
               do k=1,nlevo
                  dato(i,j,k) = spvl
               end do
               go to 60  
           end if
c ===========================================================

C
C****     GET PRESSURE VALUES FOR HYBRID SURFACES FOR THIS POINT
C****     AND FOR THE TYPE OF MODEL SURFACE THE DATA IS ON.
C****     INTERFACE DATA STARTS AT THE SECOND INTERFACE LEVEL SO
C****     IF THE DATA IS ON THOSE LEVELS START THE
C
              DO K = 1,NLEVI
                  KPI = K
                  PLEVI(K) = (HBCOFA(KPI)*P0) +
     +                       HBCOFB(KPI)* (PSFC(I,J)*.01D0)
              END DO
              IPRINT = 1
C      IF (I.EQ.1.AND.J.EQ.1.AND.IPRINT.EQ.1) THEN
C         PRINT 601
C  601    FORMAT(/,' PRINTOUT FROM VINTHB ')
C         PRINT 602,I,J,NLEVI,NLEVO,IMAX,ILEV
C  602    FORMAT(' I,J,NLEVI,NLEVO,IMAX,ILEV ',6I4)
C         PRINT 603,PSFC(I,J),P0
C  603    FORMAT(' PSFC,P0 ',1P2E12.5)
C         CALL PRNT('HBCOFA',HBCOFA,2,NLEVIP1,1,1)
C         CALL PRNT('HBCOFB',HBCOFB,2,NLEVIP1,1,1)
C         CALL PRNT(' PLEVI',PLEVI,NLEVI,1,1,1)
C         CALL PRNT(' PLEVO',PLEVO,NLEVO,1,1,1)
C      ENDIF
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
                      ELSE
                          KP = NLEVI - 1
                          GO TO 30
                      END IF
C
C****     IF BRANCH FOR TO CHECK IF OUTPOT LEVEL IN BETWEEN
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
C            CALL PRNT(' PLEVI',PLEVI,NLEVIP1,1,1,1)
C            CALL PRNT(' PLEVO',PLEVO,NLEVO,1,1,1)
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
     +                              DLOG(PLEVO(K)/PLEVI(KP))/
     +                              DLOG(PLEVI(KP+1)/PLEVI(KP))
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
      SUBROUTINE PRNT(IFLD,A,IM,JM,IS,JS)
      DOUBLE PRECISION A
      CHARACTER*(*) IFLD
      DIMENSION A(IM,JM)

      PRINT 10,IFLD
   10 FORMAT (1X,/,' FIELD ',A)
      DO 20 J = 1,JM,JS
          PRINT 13,J
   13     FORMAT (' J=',I4)
          PRINT 15, (A(I,J),I=1,IM,IS)
   15     FORMAT (1X,1P,10D12.5)
   20 CONTINUE
      RETURN
      END
