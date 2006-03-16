
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
  101   CONTINUE
C
        DMIN=+1.E36
        DMAX=-1.E36
        DO 104 J=1,N
          DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
              TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     +               (FOVN*(REAL(J)-CCNT(2,K)))**2)
              IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +            .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102       CONTINUE
            DMIN=MIN(DMIN,DATA(I,J))
            DMAX=MAX(DMAX,DATA(I,J))
  103     CONTINUE
  104   CONTINUE
C
        DO 106 J=1,N
          DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
c	    write(*,*) 'i,j,data: ', i,j,data(i,j)
  105     CONTINUE
  106   CONTINUE
C
        RETURN
C
      END
C
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
C
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
C
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
C
      SUBROUTINE DFCLRS(IWKID)
C
C Define a set of RGB color triples for colors 0 through 15.
C
      PARAMETER (NCLRS=16)
      DIMENSION RGBV(3,NCLRS)
C
C Define the RGB color triples needed below.
C
      DATA RGBV / 
     +     0.00 , 0.00 , 0.00 ,
     +     1.00 , 1.00 , 1.00 ,
     +     0.70 , 0.70 , 0.70 ,
     +     0.75 , 0.50 , 1.00 ,
     +     0.50 , 0.00 , 1.00 ,
     +     0.00 , 0.00 , 1.00 ,
     +     0.00 , 0.50 , 1.00 ,
     +     0.00 , 1.00 , 1.00 ,
     +     0.00 , 1.00 , 0.60 ,
     +     0.00 , 1.00 , 0.00 ,
     +     0.70 , 1.00 , 0.00 ,
     +     1.00 , 1.00 , 0.00 ,
     +     1.00 , 0.75 , 0.00 ,
     +     1.00 , 0.38 , 0.38 ,
     +     1.00 , 0.00 , 0.38 ,
     +     1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
      DO 101 I=1,NCLRS
         CALL GSCR (IWKID,I-1,RGBV(1,I),RGBV(2,I),RGBV(3,I))
 101  CONTINUE
C
C Done.
C
        RETURN
C
      END
C
      SUBROUTINE GENARA (A,B,ID,JD)
C
C     This subroutine generates a smooth array in output array B.
C     The array is dependent on the random number function RANDNO.
C     RANDNO can be replaced by a random number generator on a
C     given machine, or for consistency across machines, the
C     supplied function RANDNO may be used.  RANDNO reads its random
C     numbers from the file RANFDAT.
C
      DIMENSION A(ID,JD), B(ID,JD)
      PARAMETER (PI=3.14159 , TWOPI=2.*PI , EPS=PI/6.)
C
      NN=(ID+JD)/10
      AA=1.
      DI=ID-4
      DJ=JD-4
C
    2 DO 5 K=1,NN
      II=3.+DI*RANDNO()
      JJ=3.+DJ*RANDNO()
      DO 4 J=1,JD
      JE=ABS(J-JJ)
      DO 3 I=1,ID
      IE=ABS(I-II)
      EE=MAX(IE,JE)
      A(I,J)=A(I,J)+AA*(.8**EE)
    3 CONTINUE
    4 CONTINUE
    5 CONTINUE
C
      IF (AA.NE.1.) GO TO 6
      AA=-1.
      GO TO 2
C
    6 DO 8 J=1,JD
      JM1=MAX(1,J-1)
      JP1=MIN(JD,J+1)
      DO 7 I=1,ID
      IM1=MAX(1,I-1)
      IP1=MIN(ID,I+1)
      B(I,J)=(4.*A(I,J)+2.*(A(I,JM1)+A(IM1,J)+A(IP1,J)+A(I,JP1))
     1                 +A(IM1,JM1)+A(IP1,JM1)+A(IM1,JP1)+A(IP1,JP1))/16.
    7 CONTINUE
    8 CONTINUE
      RETURN
      END
C
      SUBROUTINE OPENR (IUNIT)
      CHARACTER*128 FILENM
      DATA FILENM / ' ' /
      SAVE IOPEN
      DATA IOPEN / 0 /
      IF (IOPEN.EQ.0) THEN
      CALL GNGPAT (FILENM,'database',ISTAT)
      IF (ISTAT .NE. -1) THEN
          DO 101 I=1,119
              IF (FILENM(I:I).EQ.CHAR(0)) THEN
                  FILENM(I:I+8)='/ranfdata'
                  GOTO 104
              ENDIF
 101      CONTINUE
         GO TO 105
      ELSE
          DO 102 I=2,128
              LENEM=I
              IF (FILENM(I:I).EQ.CHAR(0)) GO TO 103
 102      CONTINUE
 103      PRINT * , 'OPENR - ',FILENM(1:LENEM-1)
          STOP
      ENDIF
 104  OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='FORMATTED',
     +                                                       ERR=105)
      IOPEN=1
      END IF
      RETURN
 105  WRITE (6,*) 'ERROR OPENING RANFDATA DATA FILE - FILE NAME: ',
     +             FILENM
      STOP
      END
C
      FUNCTION RANDNO()
C
C     This function is used to produce random numbers for the
C     GENARA calls.  The random numbers are read from file
C     RANFDAT, and in this way consistency is maintained across
C     machines (the results on one machine should be the same
C     as on another.)  If consistency is not required, the
C     function RANDNO can be replaced by a local random number
C     generator.  RANFDAT contains 2000 random numbers in
C     250 card-image lines in format 8F10.8 .
C
      SAVE ICNT,A
      DIMENSION A(2000)
      DATA ICNT/0/
C
      ICNT = ICNT+1
C
C     Read in random numbers if this is the first function call.
C
      IF (ICNT .EQ. 1) THEN
C Modification for UNIX Version.
	CALL OPENR(8)
C End of modification for UNIX Version.
        DO 3 I=1,250
        INDX = 8*(I-1)+1
        JNDX = INDX+7
        READ(8,501) (A(LL),LL=INDX,JNDX)
    3   CONTINUE
      END IF
C
      RANDNO = A(ICNT)
      RETURN
C
  500 FORMAT(' RANDNO--ERROR IN READ')
  501 FORMAT(8F10.8)
C
      END
