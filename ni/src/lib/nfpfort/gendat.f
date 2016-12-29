      SUBROUTINE DGENDAT(DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH,ISEED)
      DOUBLE PRECISION DATA
      DOUBLE PRECISION DLOW
      DOUBLE PRECISION DHGH
      DOUBLE PRECISION CCNT
      DOUBLE PRECISION FOVM
      DOUBLE PRECISION FOVN
      DOUBLE PRECISION DFRAN
      DOUBLE PRECISION DMIN
      DOUBLE PRECISION DMAX
      DOUBLE PRECISION TEMP
C
C This is a routine to generate test data for two-dimensional graphics
C routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
C the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
C of data having approximately "MLOW" lows and "MHGH" highs, a minimum
C value of exactly "DLOW" and a maximum value of exactly "DHGH".
C
C "MLOW" and "MHGH" are each forced to be greater than or equal to 1
C and less than or equal to 25.
C
C "ISEED" is forced to be 0 if it is < 0 or > 100.
C
C The function used is a sum of exponentials.
C
      DIMENSION DATA(IDIM,1),CCNT(3,50)
C
      FOVM = 9.D0/DBLE(M)
      FOVN = 9.D0/DBLE(N)
C
C This is all being done inside NCL C wrapper, so redundant, but safe.
C
      IF(ISEED.LT.0.OR.ISEED.GT.100) THEN
        JSEED = 0
      ELSE
        JSEED = ISEED
      ENDIF
      NLOW = MAX0(1,MIN0(25,MLOW))
      NHGH = MAX0(1,MIN0(25,MHGH))
      NCNT = NLOW + NHGH
C
      DO 101 K = 1,NCNT
          CCNT(1,K) = 1.D0 + (DBLE(M)-1.D0)*DFRAN(JSEED)
          CCNT(2,K) = 1.D0 + (DBLE(N)-1.D0)*DFRAN(JSEED)
          IF (K.LE.NLOW) THEN
              CCNT(3,K) = -1.D0
          ELSE
              CCNT(3,K) = +1.D0
          END IF
  101 CONTINUE
C
      DMIN = +1.D36
      DMAX = -1.D36
      DO 104 J = 1,N
          DO 103 I = 1,M
              DATA(I,J) = .5D0* (DLOW+DHGH)
              DO 102 K = 1,NCNT
                  TEMP = - ((FOVM* (DBLE(I)-CCNT(1,K)))**2+
     +                   (FOVN* (DBLE(J)-CCNT(2,K)))**2)
                  IF (TEMP.GE.-20.D0) DATA(I,J) = DATA(I,J) +
     +                .5D0* (DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102         CONTINUE
              DMIN = DMIN1(DMIN,DATA(I,J))
              DMAX = DMAX1(DMAX,DATA(I,J))
  103     CONTINUE
  104 CONTINUE
C
      DO 106 J = 1,N
          DO 105 I = 1,M
              DATA(I,J) = (DATA(I,J)-DMIN)/ (DMAX-DMIN)* (DHGH-DLOW) +
     +                    DLOW
  105     CONTINUE
  106 CONTINUE
C
      RETURN
C
      END


      FUNCTION DFRAN(ISEQ)
      DOUBLE PRECISION DFRAN
      DOUBLE PRECISION RSEQ
 
      DIMENSION RSEQ(100)
C      SAVE ISEQ
      DATA RSEQ/.749D0,.973D0,.666D0,.804D0,.081D0,.483D0,.919D0,.903D0,
     +     .951D0,.960D0,.039D0,.269D0,.270D0,.756D0,.222D0,.478D0,
     +     .621D0,.063D0,.550D0,.798D0,.027D0,.569D0,.149D0,.697D0,
     +     .451D0,.738D0,.508D0,.041D0,.266D0,.249D0,.019D0,.191D0,
     +     .266D0,.625D0,.492D0,.940D0,.508D0,.406D0,.972D0,.311D0,
     +     .757D0,.378D0,.299D0,.536D0,.619D0,.844D0,.342D0,.295D0,
     +     .447D0,.499D0,.688D0,.193D0,.225D0,.520D0,.954D0,.749D0,
     +     .997D0,.693D0,.217D0,.273D0,.961D0,.948D0,.902D0,.104D0,
     +     .495D0,.257D0,.524D0,.100D0,.492D0,.347D0,.981D0,.019D0,
     +     .225D0,.806D0,.678D0,.710D0,.235D0,.600D0,.994D0,.758D0,
     +     .682D0,.373D0,.009D0,.469D0,.203D0,.730D0,.588D0,.603D0,
     +     .213D0,.495D0,.884D0,.032D0,.185D0,.127D0,.010D0,.180D0,
     +     .689D0,.354D0,.372D0,.429D0/
C      DATA ISEQ/0/

      ISEQ = MOD(ISEQ,100) + 1
      DFRAN = RSEQ(ISEQ)
      RETURN
      END
