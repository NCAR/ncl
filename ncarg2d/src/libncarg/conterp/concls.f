C
C	$Id: concls.f,v 1.5 2008-07-27 00:16:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONCLS (ZD,NDP)
C
C  GENERATE CONTOUR LEVELS BASED ON THE INPUT DATA
C
      DIMENSION       ZD(1)
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONR18/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONR19/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR20/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
      SAVE
C
C  IF NOT USER SET COMPUTE CONTOUR LEVELS
C
      IF (.NOT.CON) GO TO  150
C
C  OTHERWISE GET HI AND LOW CONTOURS FOR MESSAGE
C
      HI = CL(1)
      FLO = CL(1)
      DO  110 I=1,NCL
         IF (HI .GE. CL(I)) GO TO  100
         HI = CL(I)
         GO TO  110
  100    IF (FLO .LE. CL(I)) GO TO  110
         FLO = CL(I)
  110 CONTINUE
C
C GET INCREMENT IF EQUAL SPACED CONTOURS
C
      IF (NCL .NE. 1) GO TO  120
      FINC = 0.
      RETURN
  120 FINC = ABS(CL(1)-CL(2))
      IF (NCL .EQ. 2) RETURN
      DO  130 I=3,NCL
         IF (FINC .NE. ABS(CL(I-1)-CL(I))) GO TO  140
  130 CONTINUE
      RETURN
  140 FINC = -1.
      RETURN
C
C  FIND HIGHEST AND LOWEST INPUT VALUES
C
  150 IF (CHILO) GO TO  180
      FLO = ZD(1)
      HI = ZD(1)
      DO  170 I=2,NDP
         IF (FLO .LE. ZD(I)) GO TO  160
         FLO = ZD(I)
         GO TO  170
  160    IF (HI .GE. ZD(I)) GO TO  170
         HI = ZD(I)
  170 CONTINUE
C
C  CALCULATE THE CONTOUR LEVEL INTERVAL
C
  180 IF (CINC) GO TO  200
      FINC = (HI-FLO)/15.
      IF (FINC .NE. 0.) GO TO  190
      CALL SETER (' CONCLS - CONSTANT INPUT FIELD',1,1)
      RETURN
C
C  ROUND FINC TO NICE NUMBER
C
  190 P = 10.**(INT(ALOG10(FINC)+500.)-500)
      FINC = REAL(INT(FINC/P+0.1))*P
C
C  ROUND THE LOW VALUE TO START AT A NICE NUMBER
C
  200 IF (CHILO) GO TO  210
      FLO = REAL(INT(FLO/FINC))*FINC
C
C  COMPUTE THE CONTOUR LEVELS
C
C  TEST IF BREAK POINT WITHIN RANGE OF HI TO FLO
C
  210 IF (BPSIZ.GE.FLO .AND. BPSIZ.LE.HI) GO TO  240
C
C  BREAK POINT OUT OF RANGE SO GENERATE CONTOURS BASED ON FLO
C
      DO  220 I=1,30
         CV = FLO+REAL(I-1)*FINC
         ICUR = I
         CL(I) = CV
         IF (CV .GE. HI) GO TO  230
  220 CONTINUE
  230 NCL = ICUR
      HI = CV
      RETURN
C
C  BREAK POINT WITHIN RANGE SO BASE CONTOURS ON IT
C
  240 DO  250 I=1,30
         CV = BPSIZ-REAL(I-1)*FINC
         IND = (30-I)+1
         CL(IND) = CV
         ICUR = I
         IF (CV .LE. FLO) GO TO  260
  250 CONTINUE
C
C  PUT THE CONTOURS IN THE CORRECT ORDER
C
  260 DO  270 I=1,ICUR
         IND = (30-ICUR)+I
         CL(I) = CL(IND)
  270 CONTINUE
C
C  ADD THE GREATER THAN BREAK POINT CONTOURS
C
      IEND = 30-ICUR
      ISAV = ICUR+1
      DO  280 I=1,IEND
         CV = BPSIZ+REAL(I)*FINC
         CL(ISAV) = CV
         ISAV = ISAV+1
         IF (CV .GE. HI) GO TO  290
  280 CONTINUE
C
C  SET NUMBER OF CONTOUR LEVELS AND UPDATE THE HIGH VALUE
C
  290 NCL = ISAV-1
      HI = CV
      RETURN
      END
