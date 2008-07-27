C
C	$Id: constp.f,v 1.5 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONSTP (XD,YD,NDP)
C
C  COMPUTE STEP SIZE IN X AND Y DIRECTION
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
C
C
      DIMENSION       XD(1)      ,YD(1)
C
        SAVE
C
C  FIND SMALLEST AND LARGST X AND Y
C
      XST = XD(1)
      XED = XD(1)
      YST = YD(1)
      YED = YD(1)
      DO  130 I=2,NDP
         IF (XST .LE. XD(I)) GO TO  100
         XST = XD(I)
         GO TO  110
  100    IF (XED .GE. XD(I)) GO TO  110
         XED = XD(I)
  110    IF (YST .LE. YD(I)) GO TO  120
         YST = YD(I)
         GO TO  130
  120    IF (YED .GE. YD(I)) GO TO  130
         YED = YD(I)
  130 CONTINUE
C
C  COMPUTE STEP SIZE
C
      XRG = (ABS(XED-XST))
      YRG = (ABS(YED-YST))
      SQRG = XRG
      IF (SQRG .LT. YRG) SQRG = YRG
      STPSZ = SQRG/REAL(IGRAD-1)
C
C  COMPUTE PARAMETERS FOR SET CALL
C
      DIFX = XRG/SQRG
      DIFY = YRG/SQRG
      PXST = .5-(BORD*DIFX)/2.
      PXED = .5+(BORD*DIFX)/2.
      PYST = .5-(BORD*DIFY)/2.
      PYED = .5+(BORD*DIFY)/2.
      XRG = XRG/REAL(ITICK)
      YRG = YRG/REAL(ITICK)
C
C  TEST IF THE ASPECT RATIO FOR THE COORDINATES IS REASONABLE.
C     REASONABLE IS CURRENTLY DEFINED AS 5 TO 1.
C     IF IT IS NOT REASONABLE THEN A POOR PLOT MAY BE GENERATED
C     SO IT IS NICE THE WARN THE USER WHEN THIS HAPPENS.
C
      TEST = XRG/YRG
      IF (TEST.LE.5. .AND. TEST.GE.0.2) RETURN
C
C  WARN THE USER ON THE STANDARD OUTPUT UNIT THAT THE PLOT MAY
C  NOT BE TOO GOOD.
C
C  SET RECOVERY MODE
C
      CALL ENTSR(IROLD,IREC)
C
C  FLAG THE ERROR
C
      CALL SETER(' ASPECT RATIO OF X AND Y GREATER THAN 5 TO 1',
     1           1,1)
C
      CALL EPRIN
C
C  CLEAR THE ERROR
C
      CALL ERROF
C
C  RESET USER ERROR MODE
C
      CALL ENTSR(IDUM,IROLD)
C
      RETURN
      END
