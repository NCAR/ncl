C
C	$Id: cpexcc.f,v 1.1.1.1 1992-04-17 22:33:10 ncargd Exp $
C
      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
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
C The function used is a sum of exponentials.
C
        DIMENSION DATA(IDIM,1),CCNT(3,50)
C
        FOVM=9./FLOAT(M)
        FOVN=9./FLOAT(N)
C
        NLOW=MAX0(1,MIN0(25,MLOW))
        NHGH=MAX0(1,MIN0(25,MHGH))
        NCNT=NLOW+NHGH
C
        DO 101 K=1,NCNT
          CCNT(1,K)=1.+(FLOAT(M)-1.)*FRAN()
          CCNT(2,K)=1.+(FLOAT(N)-1.)*FRAN()
          IF (K.LE.NLOW) THEN
            CCNT(3,K)=-1.
          ELSE
            CCNT(3,K)=+1.
          END IF
  101   CONTINUE
C
        DMIN=+1.E36
        DMAX=-1.E36
        DO 104 J=1,N
          DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
              TEMP=-((FOVM*(FLOAT(I)-CCNT(1,K)))**2+
     +               (FOVN*(FLOAT(J)-CCNT(2,K)))**2)
              IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +            .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102       CONTINUE
            DMIN=AMIN1(DMIN,DATA(I,J))
            DMAX=AMAX1(DMAX,DATA(I,J))
  103     CONTINUE
  104   CONTINUE
C
        DO 106 J=1,N
          DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
  105     CONTINUE
  106   CONTINUE
C
        RETURN
C
      END
      FUNCTION FRAN ()
        DIMENSION RSEQ (100)
        SAVE ISEQ
        DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +              .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +              .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +              .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +              .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +              .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +              .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +              .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +              .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +              .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
        DATA ISEQ / 0 /
        ISEQ=MOD(ISEQ,100)+1
        FRAN=RSEQ(ISEQ)
        RETURN
      END
      SUBROUTINE CAPSAP (LABL,TIME,IAMA,LAMA)
C
        DIMENSION IAMA(*)
C
        CHARACTER*(*) LABL
C
C Compute and print the time required to draw the contour plot and how
C much space was used in the various arrays.
C
        TIME=SECOND(DUMI)-TIME
        PRINT * , 'PLOT TITLE WAS ',LABL
        PRINT * , 'TIME TO DRAW PLOT WAS  ',TIME
        CALL CPGETI ('IWU - INTEGER WORKSPACE USAGE',IIWU)
        CALL CPGETI ('RWU - REAL WORKSPACE USAGE',IRWU)
        PRINT * , 'INTEGER WORKSPACE USED ',IIWU
        PRINT * , '   REAL WORKSPACE USED ',IRWU
        IF (LAMA.NE.0) THEN
          IAMU=LAMA-(IAMA(6)-IAMA(5)-1)
          PRINT * , '   AREA MAP SPACE USED ',IAMU
        END IF
C
C Done.
C
        RETURN
C
      END
      SUBROUTINE LABTOP (LABL,SIZE)
C
        CHARACTER*(*) LABL
C
C Put a label just above the top of the plot.  The SET call is re-done
C to allow for the use of fractional coordinates, and the text extent
C capabilities of the package PLOTCHAR are used to determine the label
C position.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        SZFS=SIZE*(XVPR-XVPL)
        CALL    SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PCGETI ('QU - QUALITY FLAG',IQUA)
        CALL PCSETI ('QU - QUALITY FLAG',0)
        CALL PCSETI ('TE - TEXT EXTENT COMPUTATION FLAG',1)
        CALL PLCHHQ (.5,.5,LABL,SZFS,360.,0.)
        CALL PCGETR ('DB - DISTANCE TO BOTTOM OF STRING',DBOS)
        CALL PLCHHQ (.5*(XVPL+XVPR),YVPT+SZFS+DBOS,LABL,SZFS,0.,0.)
        CALL PCSETI ('QU - QUALITY FLAG',IQUA)
        CALL    SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Done.
C
        RETURN
C
      END
      SUBROUTINE BNDARY
C
C Draw a line showing where the edge of the plotter frame is.
C
        CALL PLOTIF (0.,0.,0)
        CALL PLOTIF (1.,0.,1)
        CALL PLOTIF (1.,1.,1)
        CALL PLOTIF (0.,1.,1)
        CALL PLOTIF (0.,0.,1)
        CALL PLOTIF (0.,0.,2)
C
C Done.
C
        RETURN
C
      END
      SUBROUTINE DFCLRS
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (1,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
      FUNCTION SECOND (DUMI)
        SAVE IFLG
        DATA IFLG / 0 /
        IF (IFLG.EQ.0) THEN
          IFLG=1
          PRINT * , '**************************************************'
          PRINT * , '** THE DUMMY FUNCTION "SECOND" HAS BEEN CALLED. **'
          PRINT * , '** A ZERO WILL BE RETURNED AS ITS VALUE.  THIS  **'
          PRINT * , '** MEANS THAT TIMING INFORMATION PRINTED WILL   **'
          PRINT * , '** BE WRONG.  TO GET ACTUAL TIMINGS, YOU SHOULD **'
          PRINT * , '** REPLACE THIS FUNCTION WITH ONE HAVING AS ITS **'
          PRINT * , '** VALUE THE ELAPSED CPU TIME, IN SECONDS.  ON  **'
          PRINT * , '** THE CRAY, UNDER COS, THE SYSTEM FUNCTION     **'
          PRINT * , '** CALLED "SECOND" DOES THIS, SO YOU CAN JUST   **'
          PRINT * , '** DELETE THE DUMMY ONE.                        **'
          PRINT * , '**************************************************'
        END IF
        SECOND=0.
        RETURN
      END
