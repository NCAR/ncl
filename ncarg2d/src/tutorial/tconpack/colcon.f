
      PROGRAM COLCON
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

      PARAMETER (MREG=50,NREG=50)
      REAL ZREG(MREG,NREG)
C     
      EXTERNAL COLOR
C
C Get data array.
C
        CALL GENDAT(ZREG,MREG,MREG,NREG,15,13,13.,18.)
C
C Open GKS, and turn clipping off.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
        CALL GSCLIP(0)
C
C Call Conpack color fill routine.
C
        CALL TCOLCON(ZREG,MREG,NREG,-15,COLOR,
     +       'CE',-90.,90.,-180.,180.,0.,0.,IWKID)
C
C Close frame and close GKS.
C
        CALL FRAME
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
        STOP
C
        END

      SUBROUTINE TCOLCON(ZREG,MREG,NREG,NCL,COLOR,
     +     PROJ,RLATMN,RLATMX,RLONMN,RLONMX,PLAT,PLON,IWKID)

      PARAMETER (LRWK=5000,LIWK=5000,LMAP=200000,NWRK=15000,NOGRPS=5)
      INTEGER MREG,NREG,IWRK(LIWK), LFIN(35)
      REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
      INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)
      CHARACTER*4 PROJ
      CHARACTER*10 LBLS(35)

      EXTERNAL FILL
      EXTERNAL MASK
      EXTERNAL COLOR
C
C Set color fill to solid.
C
      CALL GSFAIS (1)
C
C Initialize Areas.
C
      CALL ARINAM(MAP, LMAP)
C
C Initialize Ezmap and add to area map.
C
      CALL MAPSTR ('GR',0.)
      CALL MAPSTC ('OU','CO')
      CALL MAPROJ(PROJ,PLAT,PLON,0.0)
      IF (PROJ.EQ.'SV') CALL MAPSTR ('SA',10.)
      CALL MAPSET('CO',RLATMN,RLONMN,RLATMX,RLONMX)
      CALL MAPINT
      CALL MAPBLA(MAP)
C
C Initialize Conpack and add to area map.
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
C
C Choose a color for every contour level.
C
      CALL CPGETI('NCL',NCLL)
      CALL COLOR (NCLL+1,IWKID)
C
C Fill contours and areas over land.
C
      CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, FILL)
C
C Draw continental outlines, labels, and masked contours.
C
      CALL MAPLOT
      CALL CPLBDR(ZREG,RWRK,IWRK)
      CALL CPCLDM(ZREG,RWRK,IWRK,MAP,MASK)
C
C Draw and fill a label bar.
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
 10   CONTINUE
      LFIN(NCLL+1)=NCLL+3
      WRITE (LBLS(NCLL+2),'(A4)') 'LAND'
      LFIN(NCLL+2)=2
      CALL LBLBAR(0,XMIN,XMAX,YBOT,YTOP,NCLL+2,1.,.5,LFIN,1,LBLS,
     +     NCLL+2,1)
      
C
      RETURN
C
      END



      SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
      DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)

      IDMAP=-1
      IDCONT=-1

      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.1) IDMAP=IAREA(I)
         IF (IGRP(I).EQ.3) IDCONT=IAREA(I)
 10   CONTINUE
C
C If the area is defined by 2 or fewer points, return to ARSCAM.
C
      IF (NWRK .LE. 3) RETURN
C
C Check if the area is over the map 
C
      IF ((IDMAP .GT. 0) .AND. (IDCONT .GT. 0)) THEN
C
C If the area is over water, fill the contours with colors depending
C on their level.
C
         IF (MAPACI(IDMAP).EQ.1) THEN
            CALL GSFACI(IDCONT+2)
            CALL GFA(NWRK-1,XWRK,YWRK)
C
C If the area is over land, fill with gray.
C
         ELSE 
            CALL GSFACI(2)
            CALL GFA(NWRK-1,XWRK,YWRK)
         ENDIF
      ENDIF        
C
      RETURN
C
      END

      SUBROUTINE MASK(XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
      INTEGER IAREA(NGRPS),IGRP(NGRPS)
      REAL XWRK(NWRK),YWRK(NWRK)
C
      IDMAP=-1
      IDCONT=-1
C
      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.1) IDMAP=IAREA(I)
         IF (IGRP(I).EQ.3) IDCONT=IAREA(I)
 10   CONTINUE
C
C If the line is defined by 1 or fewer points, return to CPCLDM
C
      IF (NWRK .LT. 2) RETURN
C
C Draw the line if the area is over the map, and not over a label, or
C over land.
C
      IF ((IDMAP.GT.0).AND.(IDCONT.GT.0).AND.(MAPACI(IDMAP).EQ.1))
     +     THEN
         CALL CURVE(XWRK,YWRK,NWRK)
      ENDIF
C
      RETURN
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
C This version has been modified to make the data more nearly simulate
C global data.  All values in the top row (which maps to the North Pole)
C are the same.  All values in the bottom row (which maps to the South
C Pole) are the same.  Each value in the last column of a row matches
C the value in the first column of the row.
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
 101  CONTINUE
C
      AABR=0.
      AATR=0.
C
      DO 104 I=1,M
         DO 103 J=1,N
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
               TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     +              (FOVN*(REAL(J)-CCNT(2,K)))**2)
               IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +              .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
 102        CONTINUE
 103     CONTINUE
         AABR=AABR+DATA(I,1)
         AATR=AATR+DATA(I,N)
 104  CONTINUE
C
      AABR=AABR/REAL(M)
      AATR=AATR/REAL(M)
C
      DO 108 J=1,N
         IF (J.LE.N/5) THEN
            P=REAL(J-1)/REAL(N/5-1)
            DO 105 I=1,M/2
               Q=1.-MAX(0.,.5-.5*REAL(I-1)/REAL(M/5-1))
               DATL=DATA(    I,J)
               DATR=DATA(M+1-I,J)
               DATA(    I,J)=P*(Q*DATL+(1.-Q)*DATR)+(1.-P)*AABR
               DATA(M+1-I,J)=P*(Q*DATR+(1.-Q)*DATL)+(1.-P)*AABR
 105        CONTINUE
         ELSE IF (J.GE.N+1-N/5) THEN
            P=REAL(N-J)/REAL(N/5-1)
            DO 106 I=1,M/2
               Q=1.-MAX(0.,.5-.5*REAL(I-1)/REAL(M/5-1))
               DATL=DATA(    I,J)
               DATR=DATA(M+1-I,J)
               DATA(    I,J)=P*(Q*DATL+(1.-Q)*DATR)+(1.-P)*AATR
               DATA(M+1-I,J)=P*(Q*DATR+(1.-Q)*DATL)+(1.-P)*AATR
 106        CONTINUE
         ELSE
            DO 107 I=1,M/2
               Q=1.-MAX(0.,.5-.5*REAL(I-1)/REAL(M/5-1))
               DATL=DATA(    I,J)
               DATR=DATA(M+1-I,J)
               DATA(    I,J)=Q*DATL+(1.-Q)*DATR
               DATA(M+1-I,J)=Q*DATR+(1.-Q)*DATL
 107        CONTINUE
         END IF
 108  CONTINUE
C
      DMIN=+1.E36
      DMAX=-1.E36
C
      DO 110 J=1,N
         DO 109 I=1,M
            DMIN=MIN(DMIN,DATA(I,J))
            DMAX=MAX(DMAX,DATA(I,J))
 109     CONTINUE
 110  CONTINUE
C
      DO 112 J=1,N
         DO 111 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
 111     CONTINUE
 112  CONTINUE
C
      RETURN
C
      END

      FUNCTION FRAN ()
      DIMENSION RSEQ (100)
      SAVE ISEQ
      DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +     .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +     .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +     .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +     .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +     .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +     .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +     .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +     .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +     .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
      DATA ISEQ / 0 /
      ISEQ=MOD(ISEQ,100)+1
      FRAN=RSEQ(ISEQ)
      RETURN
      END

      SUBROUTINE COLOR (N,IWKID)
C
C The background color is black.
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C
C The first foreground color is white.
C
      CALL GSCR(IWKID,1,1.,1.,1.)
C
C The second foreground color is gray.
C
      CALL GSCR(IWKID,2,.75,.75,.75)
C
C Choose other foreground colors spaced equally around the spectrum.
C
      ICNT=0
      HUES=360./N
C
C REDLN is intended to be the line between red and violet values.
C
      REDLN=36.0
      LAP=INT(REDLN/HUES)
      DO 10, I=1,N
         XHUE=I*HUES
         CALL HLSRGB(XHUE,60.,75.,RED,GREEN,BLUE)
C
C Sort colors so that the reddest is first, and the most violet is last.
C
         IF (XHUE.LE.REDLN) THEN
            CALL GSCR(IWKID,(N+2)-(LAP-I),RED,GREEN,BLUE)
            ICNT=ICNT+1
         ELSE
            CALL GSCR(IWKID,I-ICNT+2,RED,GREEN,BLUE)
         ENDIF
 10   CONTINUE
C
      RETURN
C
      END
