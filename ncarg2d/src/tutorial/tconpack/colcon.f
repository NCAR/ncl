        PARAMETER (MREG=50,NREG=50)
        REAL ZREG(MREG,NREG)

        EXTERNAL COLOR

C Get data array
        CALL GENDAT(ZREG,MREG,MREG,NREG,13,18,13.,18.)

C Open GKS, and turn clipping off
        CALL OPNGKS
        CALL GSCLIP(0)

C Call Conpack color fill routine

        CALL COLCON(ZREG,MREG,NREG,-15,COLOR,
     +          'CE',-90.,90.,-180.,180.,0.,0.)

C Close frame and close GKS
        CALL FRAME
        CALL CLSGKS

        STOP
        END

        SUBROUTINE COLCON(ZREG,MREG,NREG,NCL,COLOR,
     +          PROJ,RLATMN,RLATMX,RLONMN,RLONMX,PLAT,PLON)

        PARAMETER (LRWK=5000,LIWK=5000,LMAP=200000,NWRK=15000,NOGRPS=5)
        REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
        INTEGER MREG,NREG,IWRK(LIWK), LFIN(35)
        INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)
        CHARACTER*4 PROJ
        CHARACTER*10 LBLS(35)

        EXTERNAL FILL
        EXTERNAL MASK
        EXTERNAL COLOR
C
C Set color fill to solid
C
        CALL GSFAIS (1)
C
C Initialize Areas
C
        CALL ARINAM(MAP, LMAP)
C
C Initialize Ezmap and add to area map
C
        CALL MAPSTR ('GR',0.)
        CALL MAPSTC ('OU','CO')
        CALL MAPROJ(PROJ,PLAT,PLON,0.0)
        IF (PROJ.EQ.'SV') CALL MAPSTR ('SA',10.)
        CALL MAPSET('CO',RLATMN,RLONMN,RLATMX,RLONMX)
        CALL MAPINT
        CALL MAPBLA(MAP)
C
C Initialize Conpack and add to area map
C
        CALL CPSETI('SET - DO-SET-CALL FLAG',0)
        CALL CPSETI('MAP - MAPPING FLAG',1)
        CALL CPSETR('ORV - OUT OF RANGE FLAG',1.E12)
        CALL CPSETR('XC1 - X COORDINATE AT INDEX 1',RLONMN)
        CALL CPSETR('XCM - X COORDINATE AT INDEX M',RLONMX)
        CALL CPSETR('YC1 - Y COORDINATE AT INDEX 1',RLATMN)
        CALL CPSETR('YCN - Y COORDINATE AT INDEX N',RLATMX)
        CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',0)
        CALL CPSETI('HLB - HIGH/LOW LABEL BOX FLAG',2)
        CALL CPSETI('HLC - HIGH/LOW LABEL COLOR INDEX',1)
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
        CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
        CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C Choose a color for every contour level
        CALL CPGETI('NCL',NCLL)
        CALL COLOR (NCLL+1)
C Fill contours and areas over land
        CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, FILL)

C Draw continental outlines, labels, and masked contours
        CALL MAPLOT
        CALL CPLBDR(ZREG,RWRK,IWRK)
        CALL CPCLDM(ZREG,RWRK,IWRK,MAP,MASK)

C
C Draw and fill a label bar
C
        CALL GETSET(XMIN,XMAX,YMIN,YMAX,DUM1,DUM2,DUM3,DUM4,IDUM)
        YBOT = YMIN/3.0
        YTOP = YMIN - YMIN/3.0 
        CALL CPGETR('ZMN - Z DATA ARRAY DIMENSION N',ZMN)
        WRITE (LBLS(1), '(F8.3)') ZMN
        DO 10, I=1,NCLL
           LFIN(I)=I+2
           CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
           CALL CPGETR('CLV - CONTOUR LEVEL VALUES',CLV)
           WRITE (LBLS(I+1), '(F8.3)') CLV
 10     CONTINUE
        LFIN(NCLL+1)=NCLL+3
        WRITE (LBLS(NCLL+2),'(A4)') 'LAND'
        LFIN(NCLL+2)=2
        CALL LBLBAR(0,XMIN,XMAX,YBOT,YTOP,NCLL+2,1.,.5,LFIN,1,LBLS,
     +          NCLL+2,1)
        
        RETURN
        END

        SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
        DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)

        IDMAP=-1
        IDCONT=-1

        DO 10, I=1,NGRPS
          IF (IGRP(I).EQ.1) IDMAP=IAREA(I)
          IF (IGRP(I).EQ.3) IDCONT=IAREA(I)
 10     CONTINUE

C If the area is defined by 2 or fewer points, return to ARSCAM
        IF (NWRK .LE. 3) RETURN

C Check if the area is over the map 
        IF ((IDMAP .GT. 0) .AND. (IDCONT .GT. 0)) THEN
C If the area is over water, fill the contours with colors depending
C on their level
           IF (MAPACI(IDMAP).EQ.1) THEN
              CALL GSFACI(IDCONT+2)
              CALL GFA(NWRK-1,XWRK,YWRK)
C If the area is over land, fill with gray
           ELSE 
              CALL GSFACI(2)
              CALL GFA(NWRK-1,XWRK,YWRK)
           ENDIF
        ENDIF        
        RETURN
        END

        SUBROUTINE MASK(XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)

        INTEGER IAREA(NGRPS),IGRP(NGRPS)
        REAL XWRK(NWRK),YWRK(NWRK)

        IDMAP=-1
        IDCONT=-1

        DO 10, I=1,NGRPS
          IF (IGRP(I).EQ.1) IDMAP=IAREA(I)
          IF (IGRP(I).EQ.3) IDCONT=IAREA(I)
 10     CONTINUE

C If the line is defined by 1 or fewer points, return to CPCLDM
        IF (NWRK .LT. 2) RETURN

C Draw the line if the area is over the map, and not over a label, or
C over land.

        IF ((IDMAP.GT.0).AND.(IDCONT.GT.0).AND.(MAPACI(IDMAP).EQ.1))
     +          THEN
          CALL CURVE(XWRK,YWRK,NWRK)
        ENDIF
        
        RETURN
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
          DO 105 I=1,M-1
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
  105     CONTINUE
  106   CONTINUE
C
        DO 107 J=1,N
          DATA(M,J)=(DATA(M,J)+DATA(1,J))/2.0
  107   CONTINUE

        DO 108 J=1,N
          DATA(M-1,J)=(DATA(M-1,J)+DATA(M,J)+DATA(1,J))/3.0
  108   CONTINUE

        DO 109 J=1,N
          DATA(M-2,J)=(DATA(M-2,J)+DATA(M-1,J)+DATA(M,J)+DATA(1,J))/4.0
  109   CONTINUE
C
C
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

        SUBROUTINE COLOR (N)

C BACKGROUND COLOR
C BLACK
        CALL GSCR(1,0,0.,0.,0.)
C First foreground color is white
        CALL GSCR(1,1,1.,1.,1.)
C Second foreground color is gray
        CALL GSCR(1,2,.75,.75,.75)
C Choose other foreground colors spaced equally around the spectrum
        ICNT=0
        HUES=360./N
C REDLN is intended to be the line between red and violet values
	REDLN=36.0
	LAP=INT(REDLN/HUES)
        DO 10, I=1,N
          XHUE=I*HUES
          CALL HLSRGB(XHUE,60.,75.,RED,GREEN,BLUE)
C Sort colors so that the redest is first, and violetest is last
          IF (XHUE.LE.REDLN) THEN
            CALL GSCR(1,(N+2)-(LAP-I),RED,GREEN,BLUE)
            ICNT=ICNT+1
          ELSE
            CALL GSCR(1,I-ICNT+2,RED,GREEN,BLUE)
          ENDIF
 10     CONTINUE

        RETURN
        END


