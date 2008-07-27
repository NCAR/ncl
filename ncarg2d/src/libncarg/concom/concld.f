C
C	$Id: concld.f,v 1.5 2008-07-27 00:16:54 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONCLD (ICASE,IOOP)
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
      COMMON /CONRA9/ ICOORD(500),  NP       ,MXXY       ,TR         ,
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
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
      INTEGER         GOOP
C
        SAVE
        DATA GOOP/0/
C
C  STATEMENT FUNCTIONS FOR CONTOUR PLACEMENT WITHIN CELLS
C
      CX(W1,W2) = STPSZ*( (W1-CONV)/(W1-W2) )
      CY(W1,W2) = STPSZ*( (W1-CONV)/(W1-W2) )
      IC = ICASE
      ICASE = 0
C
C  SPECIAL PROCESSING IF SHIELDING ACTIVATED
C
      IF (.NOT.SHIELD) GO TO 1
C
C  CHECK IF ANY CELL CORNER CONTAINS A SPECIAL VALUE
C       IF SO THEN FLAG AND RETURN
C
      IF (TR.NE.SPVAL.AND.BR.NE.SPVAL.AND.TL.NE.SPVAL.AND.BL.NE.SPVAL)
     1      GO TO 1
C
C  SPECIAL VALUE IN CELL FLAG AND RETURN
C
      ICASE = -1
      RETURN
C
C  IF CURRENT BR VALUE LESS THAN CONTOUR THEN NEIGHBOR WILL BE WHERE
C  CONTOUR IS DRAWN.
C
 1    CONTINUE
C
      IF (BR.LT.CONV) GO TO    90
C
C  CURRENT LOCATION IS WHERE CONTOUR WILL BE DRAWN
C
C  TEST FOR VERTICAL CONTOUR BREAK
C
      IF (BL.GE.CONV) GO TO    60
C
C  VERTICAL CONTOUR BREAK
C
C  CASE 1 LEFT NEIGHBOR LESS THAN CONTOUR LEVEL AND CURRENT
C  LOCATION GE CONTOUR VALUE
C
      IF (TR.GE.CONV) GO TO    40
C
C  CASE 1A CONTOUR LOWER RIGHT
C
C
C  CONTOUR FROM UPPER RIGHT
C
      XO = XC-CX(BR,TR)
      YO = YC
      YN = YC-CY(BR,BL)
      XN = XC
      NC = 1
      IOC = 4
      IF (IC.NE.3) GO TO    10
      ICASE = IOC
      XN = XO
      YN = YO
      RETURN
   10 IF (IOOP.NE.GOOP) GO TO    20
      IF (IC.NE.2) GO TO    30
   20 ICASE = NC
      RETURN
C
C CASE 1B  CONTOR UPPER LEFT
C
   30 XN = XC-STPSZ
      YN = YC-STPSZ+CY(TL,TR)
      XO = XC-STPSZ+CX(TL,BL)
      YO = YC-STPSZ
      IOC = 2
      NC = 3
      GO TO   180
C
C  CONTOURS FROM ABOVE AND UPPER LEFT
C
   40 IF (TL.LT.CONV) GO TO    50
C
C  CASE 1C CONTOUR LOWER LEFT
C
      XO = XC-STPSZ+CX(TL,BL)
      YO = YC-STPSZ
      YN = YC-CY(BR,BL)
      XN = XC
      NC = 1
      IOC = 2
      GO TO   180
C
C CASE 1D CONTOUR FROM ABOVE
C
   50 XO = XC-STPSZ
      YO = YC-CY(TR,TL)
      YN = YC-CY(BR,BL)
      XN = XC
      NC = 1
      IOC = 3
      GO TO   180
C
C
C  TEST FOR HORIZONTAL CONTOUR BREAK
C
   60 IF (TR.LT.CONV) GO TO    70
      IF (TL.GE.CONV) GO TO   200
C
C  CASE 2A CONTOUR UPPER LEFT
C
      XO = XC-STPSZ
      YO = YC-CY(TR,TL)
      XN = XC-CX(BL,TL)
      YN = YC-STPSZ
      NC = 2
      IOC = 3
      GO TO   180
C
   70 IF (TL.LT.CONV) GO TO    80
C
C  CASE 2B CONTOUR FROM UPPER RIGHT
C
      XO = XC-STPSZ
      YO = YC-STPSZ+CY(TL,TR)
      XN = XC-CX(BR,TR)
      YN = YC
      NC = 4
      IOC = 3
      GO TO   180
C
C  CASE 2C CONTOUR FROM LEFT TO RIGHT
C
   80 XO = XC-CX(BL,TL)
      YO = YC-STPSZ
      XN = XC-CX(BR,TR)
      YN = YC
      NC = 4
      IOC = 2
      GO TO   180
C
C
C  CURRENT BR VALUE LESS THAN CONTOUR
C
C
   90 IF (BL.LT.CONV) GO TO   150
C
C  VERTICAL CONTOUR BREAK
C
C  CASE 3 CURRENT SPACE LESS THAN CONTOUR LEVEL AND LEFT
C  NEIGHBOR GE CONTOUR LEVEL
C
      IF (TL.GE.CONV) GO TO   130
C
C  CASE 3A CONTOUR LOWER LEFT
C
      XO = XC-CX(BL,TL)
      YO = YC-STPSZ
      YN = YC-STPSZ+CY(BL,BR)
      XN = XC
      NC = 1
      IOC = 2
      IF (IC.NE.3) GO TO   100
      ICASE = IOC
      XN = XO
      YN = YO
      RETURN
  100 IF (IOOP.NE.GOOP) GO TO   110
      IF (IC.NE.4) GO TO   120
  110 ICASE = NC
      RETURN
C
C  CASE 3B CONTOUR UPPERRIGHT
C
  120 XO = XC-STPSZ
      YO = YC-CY(TR,TL)
      XN = XC-STPSZ+CX(TR,BR)
      YN = YC
      NC = 4
      IOC = 3
      GO TO   180
C
  130 IF (TR.GE.CONV) GO TO   140
C
C  CASE 3C CONTOUR FROM ABOVE
C
      XO = XC-STPSZ
      YO = YC-STPSZ+CY(TL,TR)
      YN = YC-STPSZ+CY(BL,BR)
      XN = XC
      NC = 1
      IOC = 3
      GO TO   180
C
C  CASE 3D CONTOUR LOWER RIGHT
C
  140 XO = XC-STPSZ+CX(TR,BR)
      YO = YC
      YN = YC-STPSZ+CY(BL,BR)
      XN = XC
      NC = 1
      IOC = 4
      GO TO   180
C
C
C
C  TEST FOR HORIZONTAL BREAK POINT
C
  150 IF (TR.GE.CONV) GO TO   160
C
      IF (TL.LT.CONV) GO TO   200
C
C  CASE 4A CONTOUR UPPER LEFT
C
      XN = XC-STPSZ+CX(TL,BL)
      YN = YC-STPSZ
      XO = XC-STPSZ
      YO = YC-STPSZ+CY(TL,TR)
      NC = 2
      IOC = 3
      GO TO   180
C
  160 IF (TL.GE.CONV) GO TO   170
C
C  CASE 4B CONTOUR UPPER RIGHT
C
      XO = XC-STPSZ
      YO = YC-CY(TR,TL)
      XN = XC-STPSZ+CX(TR,BR)
      YN = YC
      NC = 4
      IOC = 3
      GO TO   180
C
C  CASE 4C CONTOUR FROM LEFT TO RIGHT
C
  170 YO = YC-STPSZ
      XO = XC-STPSZ+CX(TL,BL)
      XN = XC-STPSZ+CX(TR,BR)
      YN = YC
      NC = 4
      IOC = 2
C
C  DRAW THE CONTOUR LINES NOT ALREADY TAKEN CARE OF
C
  180 IF (ABS(IC-NC).NE.2) GO TO   190
      ICASE = IOC
      XN = XO
      YN = YO
      RETURN
  190 ICASE = NC
  200 RETURN
      END
