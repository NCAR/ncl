
      PROGRAM AGEX09
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define the data arrays.
C
      DIMENSION XDAT(400),YDAT(400)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,400
        XDAT(I)=(REAL(I)-1.)/399.
  101 CONTINUE
C
      CALL GENDAT (YDAT(  1),200,200,1,3,3,+.01,+10.)
      CALL GENDAT (YDAT(201),200,200,1,3,3,-10.,-.01)
C
C The y data ranges over both positive and negative values.
C It is desired that both ranges be represented on the same
C graph and that each be shown logarithmically, ignoring
C values in the range -.01 to +.01, in which we have no
C interest.  First we map each y datum into its absolute
C value (.01 if the absolute value is too small).  Then we
C take the base-10 logarithm, add 2.0001 (so as to be sure
C of getting a positive number), and re-attach the original
C sign.  We can plot the resulting y data on a linear y axis.
C
      DO 102 I=1,400
        YDAT(I)=SIGN(ALOG10(MAX(ABS(YDAT(I)),.01))+2.0001,
     +               YDAT(I))
  102 CONTINUE
C
C In order that the labels on the y axis should show the
C original values of the y data, we change the user-system-
C to-label-system mapping on both y axes and force major
C ticks to be spaced logarithmically in the
C label system (which will be defined by the subroutine
C AGUTOL in such a way as to re-create numbers in the
C original range).
C
      CALL AGSETI ('LEFT/FUNCTION.',1)
      CALL AGSETI ('LEFT/MAJOR/TYPE.',2)
C
      CALL AGSETI ('RIGHT/FUNCTION.',1)
      CALL AGSETI ('RIGHT/MAJOR/TYPE.',2)
C
C Change the left-axis label to reflect what's going on.
C
      CALL AGSETC ('LABEL/NAME.','L')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.',
     +             'LOG SCALING, POSITIVE AND NEGATIVE$')
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the curve.
C
      CALL EZXY (XDAT,YDAT,400,'EXAMPLE 9$')
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END
      SUBROUTINE AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP)
C
C Left or right axis.
C
      IF (FUNS.EQ.1.) THEN
        IF (IDMA.LT.0) THEN
          VOTP=SIGN(ALOG10(MAX(ABS(VINP),.01))+2.0001,VINP)
        ELSE
          VOTP=SIGN(10.**(ABS(VINP)-2.0001),VINP)
        END IF
C
C All others.
C
      ELSE
        VOTP=VINP
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,
     +                                  CHRE,MCIE,NCIE)
C
      CHARACTER*(*) CHRM,CHRE
C
C Modify the left-axis numeric label marking the value "0.".
C
      IF (IAXS.EQ.1.AND.VILS.EQ.0.) THEN
        CHRM(1:1)=' '
        NCIM=1
        IPXM=0
        NCIE=0
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
C
C This is a routine to generate test data for two-dimensional
C graphics routines.  Given an array "DATA", dimensioned
C "IDIM,1", it fills the sub-array ((DATA(I,J),I=1,M),J=1,N)
C with a two-dimensional field of data having approximately
C "MLOW" lows and "MHGH" highs, a minimum value of exactly
C "DLOW" and a maximum value of exactly "DHGH".
C
C "MLOW" and "MHGH" are each forced to be greater than or
C equal to 1 and less than or equal to 25.
C
C The function used is a sum of exponentials.
C
      DIMENSION DATA(IDIM,1),CCNT(3,50)
C
      FOVM=9./REAL(M)
      FOVN=9./REAL(N)
C
      NLOW=MAX(1,MIN(25,MLOW))
      NHGH=MAX(1,MIN(25,MHGH))
      NCNT=NLOW+NHGH
C
      DO 101 K=1,NCNT
        CCNT(1,K)=1.+(REAL(M)-1.)*FRAN()
        CCNT(2,K)=1.+(REAL(N)-1.)*FRAN()
        IF (K.LE.NLOW) THEN
          CCNT(3,K)=-1.
        ELSE
          CCNT(3,K)=+1.
        END IF
  101 CONTINUE
C
      DMIN=+1.E36
      DMAX=-1.E36
      DO 104 J=1,N
        DO 103 I=1,M
          DATA(I,J)=.5*(DLOW+DHGH)
          DO 102 K=1,NCNT
            TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     +             (FOVN*(REAL(J)-CCNT(2,K)))**2)
            IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +          .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102     CONTINUE
          DMIN=MIN(DMIN,DATA(I,J))
          DMAX=MAX(DMAX,DATA(I,J))
  103   CONTINUE
  104 CONTINUE
C
      DO 106 J=1,N
        DO 105 I=1,M
          DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*
     +                               (DHGH-DLOW)+DLOW
  105   CONTINUE
  106 CONTINUE
C
      RETURN
C
      END
      FUNCTION FRAN()
C
C Pseudo-random-number generator.
C
        DOUBLE PRECISION X
        SAVE X
        DATA X / 2.718281828459045D0 /
        X=MOD(9821.D0*X+.211327D0,1.D0)
        FRAN=REAL(X)
        RETURN
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
