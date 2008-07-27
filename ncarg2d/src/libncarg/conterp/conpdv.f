C
C	$Id: conpdv.f,v 1.5 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONPDV (XD,YD,ZD,NDP)
C
C  PLOT THE DATA VALUES ON THE CONTOUR MAP
C  CURRENTLY UP TO 10 CHARACTERS FOR EACH VALUE ARE DISPLAYED
C
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
        COMMON /RANINT/ IRANMJ, IRANMN, IRANTX
        COMMON /RAQINT/ IRAQMJ, IRAQMN, IRAQTX
        COMMON /RASINT/ IRASMJ, IRASMN, IRASTX
C
C
      CHARACTER*10  ISTR
      DIMENSION       XD(1)      ,YD(1)      ,ZD(1)
C
        SAVE
C
C  DATA TO CONVERT 0-32767 COORIDNATES TO 1-1024 VALUES
C
        DATA TRANS/32./
C
C  SET INTENSITY
C
        IF (INDAT .NE. 1) THEN
          CALL GSTXCI (INDAT)
        ELSE
            CALL GSTXCI (IRANTX)
      ENDIF
C
C  SET FORMAT IF NONE SPECIFIED
C
      IF (LEN .NE. 0) GO TO  110
        FORM = '(G10.3)'
      LEN = LEND
      IFMT = IFMTD
C
C  LOOP AND PLOT ALL VALUES
C
  110 DO  120 K=1,NDP
         CALL FL2INT (XD(K),YD(K),MX,MY)
         MX = INT(REAL(MX)/TRANS)+1
         MY = INT(REAL(MY)/TRANS)+1
       WRITE(ISTR,FORM)ZD(K)
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
      CALL GQCNTN(IER,ICN)
      CALL GSELNT(0)
      XC = CPUX(MX)
      YC = CPUY(MY)
C
         CALL WTSTR(XC,YC,ISTR,ISIZEP,0,0)
      CALL GSELNT(ICN)
  120 CONTINUE
        IF (INDAT .NE. 1) THEN
            CALL GSTXCI (IRANTX)
        ENDIF
      RETURN
      END
