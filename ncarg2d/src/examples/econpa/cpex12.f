
        PROGRAM CPEX12
C
C This program demonstrates the use of the new dashed-line package
C DASHPACK, which became a part of NCAR Graphics in August, 1994.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(37,27)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(25000)
C
C Declare a couple of character temporaries in which to manipulate the
C numeric labels for the contour lines.
C
        CHARACTER*69 CTM1
        CHARACTER*138 CTM2
C
C Declare the routine which will draw contour lines but avoid drawing
C them through labels.
C
        EXTERNAL CPDRPL
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,37,37,27,13,13,-512.148,489.834)
C
C Change the range of the data somewhat.
C
        DO 102 I=1,37
          DO 101 J=1,27
            ZDAT(I,J)=ZDAT(I,J)+10000.
  101     CONTINUE
  102   CONTINUE
C
C Put explanatory labels at the top and bottom of the plot.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.930),'A CONPACK EXAMPLE',.018,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.897),'Using the New Dashed-Line Pack
     +age DASHPACK',.015,0.,0.)
C
        CALL PLCHHQ (CFUX(.5),CFUY(.093),'Note that contour labels are w
     +ritten by PLCHHQ and that they can be made to:C:bend with the line
     + (which works best when, as here, the lines are very smooth).',
     +                                                       .010,0.,0.)
C
C Tell CONPACK to choose contour levels for itself and to use more than
C the usual number of them.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',32)
C
C Tell CONPACK to use characters a little smaller than the default size
C in the contour-line labels.
C
        CALL CPSETR ('DPS - DASH PATTERN CHARACTER SIZE',.0075)
C
C Tell CONPACK to use the new dash package (because 'DPU' is negative)
C and to use 1 repetition of the dash pattern between each line label
C and the next (because the absolute value of 'DPU' is 1).
C
        CALL CPSETI ('DPU - DASH PACKAGE USE FLAG',-1)
C
C Tell CONPACK to use gaps and solids that are half the default size
C when drawing the contour lines.
C
        CALL CPSETR ('DPV - DASH PATTERN VECTOR SIZE',.0025)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label, another high/low label, or the edge.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',7)
C
C Force the use of an exponent form in all numeric labels.
C
        CALL CPSETI ('NEU - NUMERIC EXPONENT USE FLAG',0)
C
C Tell CONPACK to smooth the contour lines.
C
        CALL CPSETR ('T2D - TENSION ON 2D SPLINES',2.5)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,37,37,27,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels and numeric labels for them,
C so that the labels can be manipulated as required by DASHPACK.
C
        CALL CPPKLB (ZDAT,RWRK,IWRK)
C
C Find out how many levels were chosen and loop through all the levels.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
C
        DO 104 IOCL=1,NOCL
C
C Set 'PAI' to the index of the next level.
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',IOCL)
C
C Pick up the contour level value and use it to determine whether to
C use a dashed line, indicating a value below 10000, or a solid line,
C indicating a value above 10000.
C
          CALL CPGETR ('CLV - CONTOUR LEVEL',CLEV)
C
          IF (CLEV.LT.10000.) THEN
            CALL CPSETC ('CLD - CONTOUR LEVEL DASH PATTERN',
     +                                  '$_$_$_$_$_$_$_$_$_$_$_$_$_$_$')
          ELSE
            CALL CPSETC ('CLD - CONTOUR LEVEL DASH PATTERN',
     +                                  '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
          END IF
C
C Retrieve the chosen numeric label for the level and preprocess it to
C include characters that tell DASHPACK where it is permitted to break
C the label.  (Each piece is written with a separate call to PLCHHQ and
C the overall effect is to make the label bend with the curve.)  This
C is done in such a way as to leave function codes with the characters
C they affect.  This code assumes that the default function-code control
C character (a colon) is being used by PLOTCHAR and that the default
C break character (a vertical bar) is being used by DASHPACK; it could
C be made more general by interrogating PLOTCHAR and DASHPACK to see
C what characters are really in use.
C
          CALL CPGETC ('LLT - LINE LABEL TEXT',CTM1)
C
          IF (CTM1.NE.' ') THEN
C
C J is the index of the last character examined in the input string, K
C is the index of the last character stored in the output string, and
C L is a flag indicating whether or not a '10:S:' has been seen in the
C input (so we can avoid putting break characters in the middle of the
C exponent).
C
            J=0
            K=0
            L=0
C
C What follows is a simple loop to copy characters from the input
C string to the output string, inserting break characters where they
C are needed.
C
  103       IF (J.LE.64) THEN
              IF (CTM1(J+1:J+5).EQ.':L1:4') THEN
                CTM2(K+1:K+6)=':L1:4|'
                J=J+5
                K=K+6
                GO TO 103
              ELSE IF (CTM1(J+1:J+5).EQ.'10:S:') THEN
                CTM2(K+1:K+6)='1|0:S:'
                J=J+5
                K=K+6
                L=1
                GO TO 103
              ELSE IF (CTM1(J+1:J+3).EQ.':N:') THEN
                CTM2(K+1:K+3)=':N:'
                J=J+3
                K=K+3
              ELSE IF (CTM1(J+1:J+1).NE.' ') THEN
                CTM2(K+1:K+1)=CTM1(J+1:J+1)
                J=J+1
                K=K+1
                IF (L.EQ.0.AND.CTM1(J+1:J+1).NE.' ') THEN
                  CTM2(K+1:K+1)='|'
                  K=K+1
                END IF
                GO TO 103
              END IF
            END IF
C
C Done - pass the string with break characters in it back to CONPACK.
C
            CALL CPSETC ('LLT - LINE LABEL TEXT',CTM2(1:K))
C
          END IF
C
C End of loop through contour levels.
C
  104   CONTINUE
C
C Draw the default background.
C
        CALL CPBACK (ZDAT,RWRK,IWRK)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,25000)
C
C Put label boxes into the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, omitting parts inside label boxes.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
C
C Add high, low, and informational labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END



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
  105     CONTINUE
  106   CONTINUE
C
        RETURN
C
      END



      FUNCTION FRAN()
        DOUBLE PRECISION X
        SAVE X
        DATA X / 2.718281828459045D0 /
        X=MOD(9821.D0*X+.211327D0,1.D0)
        FRAN=REAL(X)
        RETURN
      END
