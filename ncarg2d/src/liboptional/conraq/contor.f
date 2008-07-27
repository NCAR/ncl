C
C	$Id: contor.f,v 1.4 2008-07-27 00:23:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONTOR (X,Y,IY,Z)
C
C  GENERATE CONTOUR LINES FOR ALL LEVELS WHICH APPLY
C  TO THE CURRENT LOCATION IN THE VIRTURAL GRID.
C
C INPUT
C       X-INPUT X COORD.
C       Y-INPUT Y COORD.
C       IY-VALUE OF DATA LOCATION ABOVE Z
C       Z-DATA VALUE AT LOC
C
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
      SAVE
C
C  STATEMENT FUNCTIONS FOR MAPPING COORIDNATES TO DIFFIRENT POINTS
C
C     FX(XXX,YYY) = XXX
C     FY(XXX,YYY) = YYY
C
C  STATEMENT FUNCTIONS FOR CONTOUR PLACEMENT WITHIN CELLS
C
      CX(W1,W2) = STPSZ*( (W1-CV)/(W1-W2) )
      CY(W1,W2) = STPSZ*( (W1-CV)/(W1-W2) )
C
C  SET UP CELL VALUES
C
      PVIY = PV(IY+1)
      PVIYM1 = PV(IY)
C
C LOOP THROUGH ALL CONTOUR LEVELS
C
          DO   130 I=1,NCL
          CV = CL(I)
C
C  IF CURRENT Z VALUE LESS THAN CONTOUR THEN NEIGHBOR WILL BE WHERE
C  CONTOUR IS DRAWN.
C
          IF (Z.LT.CV) GO TO    60
C
C  CURRENT LOCATION IS WHERE CONTOUR WILL BE DRAWN
C
C  TEST FOR VERTICAL CONTOUR BREAK
C
          IF (OLDZ.GE.CV) GO TO    30
C
C  VERTICAL CONTOUR BREAK
C
C  CASE 1 LEFT NEIGHBOR LESS THAN CONTOUR LEVEL AND CURRENT
C  LOCATION GE CONTOUR VALUE
C
          IF (PVIY.GE.CV) GO TO    10
C
C  CASE 1A CONTOUR LOWER LEFT
C
C
C  CONTOUR FROM UPPER RIGHT
C
          XO = X-CX(Z,PVIY)
          YO = Y
          YN = Y-CY(Z,OLDZ)
          XN = X
          XO1 = FX(XO,YO)
          YO1 = FY(XO,YO)
          CALL FL2INT (XO1,YO1,IXO,IYO)
          CALL PLOTIT (IXO,IYO,0)
          XN1 = FX(XN,YN)
          YN1 = FY(XN,YN)
          CALL FL2INT (XN1,YN1,IXN,IYN)
          CALL PLOTIT (IXN,IYN,1)
          IF (PVIYM1.LT.CV) GO TO   130
C
C CASE 1B  CONTOR UPPER LEFT
C
          XO = X-STPSZ
          YO = Y-STPSZ+CY(PVIYM1,PVIY)
          XN = X-STPSZ+CX(PVIYM1,OLDZ)
          YN = Y-STPSZ
          GO TO   120
C
C  CONTOURS FROM ABOVE AND UPPER LEFT
C
   10     IF (PVIYM1.LT.CV) GO TO    20
C
C  CASE 1C CONTOUR LOWER LEFT
C
          XO = X-STPSZ+CX(PVIYM1,OLDZ)
          YO = Y-STPSZ
          YN = Y-CY(Z,OLDZ)
          XN = X
          GO TO   120
C
C CASE 1D CONTOUR FROM ABOVE
C
   20     XO = X-STPSZ
          YO = Y-CY(PVIY,PVIYM1)
          YN = Y-CY(Z,OLDZ)
          XN = X
          GO TO   120
C
C
C  TEST FOR HORIZONTAL CONTOUR BREAK
C
   30     IF (PVIY.LT.CV) GO TO    40
          IF (PVIYM1.GE.CV) GO TO   130
C
C  CASE 2A CONTOUR UPPER LEFT
C
          XO = X-STPSZ
          YO = Y-CY(PVIY,PVIYM1)
          XN = X-CX(OLDZ,PVIYM1)
          YN = Y-STPSZ
          GO TO   120
C
   40     IF (PVIYM1.LT.CV) GO TO    50
C
C  CASE 2B CONTOUR FROM UPPER RIGHT
C
          XO = X-STPSZ
          YO = Y-STPSZ+CY(PVIYM1,PVIY)
          XN = X-CX(Z,PVIY)
          YN = Y
          GO TO   120
C
C  CASE 2C CONTOUR FROM LEFT TO RIGHT
C
   50     XO = X-CX(OLDZ,PVIYM1)
          YO = Y-STPSZ
          XN = X-CX(Z,PVIY)
          YN = Y
          GO TO   120
C
C
C  CURRENT Z VALUE LESS THAN CONTOUR
C
C
   60     IF (OLDZ.LT.CV) GO TO    90
C
C  VERTICAL CONTOUR BREAK
C
C  CASE 3 CURRENT SPACE LESS THAN CONTOUR LEVEL AND LEFT
C  NEIGHBOR GE CONTOUR LEVEL
C
          IF (PVIYM1.GE.CV) GO TO    70
C
C  CASE 3A CONTOUR LOWER LEFT
C
          XO = X-CX(OLDZ,PVIYM1)
          YO = Y-STPSZ
          YN = Y-STPSZ+CY(OLDZ,Z)
          XN = X
          XO1 = FX(XO,YO)
          YO1 = FY(XO,YO)
          CALL FL2INT (XO1,YO1,IXO,IYO)
          CALL PLOTIT (IXO,IYO,0)
          XN1 = FX(XN,YN)
          YN1 = FY(XN,YN)
          CALL FL2INT (XN1,YN1,IXN,IYN)
          CALL PLOTIT (IXN,IYN,1)
          IF (PVIY.LT.CV) GO TO   130
C
C  CASE 3B CONTOUR UPPERRIGHT
C
          XO = X-STPSZ
          YO = Y-CY(PVIY,PVIYM1)
          XN = X-STPSZ+CX(PVIY,Z)
          YN = Y
          GO TO   120
C
   70     IF (PVIY.GE.CV) GO TO    80
C
C  CASE 3C CONTOUR FROM ABOVE
C
          XO = X-STPSZ
          YO = Y-STPSZ+CY(PVIYM1,PVIY)
          YN = Y-STPSZ+CY(OLDZ,Z)
          XN = X
          GO TO   120
C
C  CASE 3D CONTOUR LOWER RIGHT
C
   80     XO = X-STPSZ+CX(PVIY,Z)
          YO = Y
          YN = Y-STPSZ+CY(OLDZ,Z)
          XN = X
          GO TO   120
C
C
C
C  TEST FOR HORIZONTAL BREAK POINT
C
   90     IF (PVIY.GE.CV) GO TO   100
C
          IF (PVIYM1.LT.CV) GO TO   140
C
C  CASE 4A CONTOUR UPPER LEFT
C
          XO = X-STPSZ+CX(PVIYM1,OLDZ)
          YO = Y-STPSZ
          XN = X-STPSZ
          YN = Y-STPSZ+CY(PVIYM1,PVIY)
          GO TO   120
C
  100     IF (PVIYM1.GE.CV) GO TO   110
C
C  CASE 4B CONTOUR UPPER RIGHT
C
          XO = X-STPSZ
          YO = Y-CY(PVIY,PVIYM1)
          XN = X-STPSZ+CX(PVIY,Z)
          YN = Y
          GO TO   120
C
C  CASE 4C CONTOUR FROM LEFT TO RIGHT
C
  110     YO = Y-STPSZ
          XO = X-STPSZ+CX(PVIYM1,OLDZ)
          XN = X-STPSZ+CX(PVIY,Z)
          YN = Y
C
C  DRAW THE CONTOUR LINES NOT ALREADY TAKEN CARE OF
C
  120     XO1 = FX(XO,YO)
          YO1 = FY(XO,YO)
          CALL FL2INT (XO1,YO1,IXO,IYO)
          CALL PLOTIT (IXO,IYO,0)
          XN1 = FX(XN,YN)
          YN1 = FY(XN,YN)
          CALL FL2INT (XN1,YN1,IXN,IYN)
          CALL PLOTIT (IXN,IYN,1)
C
C
  130     CONTINUE
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
  140 PV(IY) = OLDZ
      OLDZ = Z
      RETURN
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980     ADDED CONRAQ TO ULIB                            *
C*  AUGUST 1980   CHANGED ACCESS CARD DOCUMENTATION               *
C*  DECEMBER 1980 MODIFIED COMMENT CARD DOCUMENTATION             *
C*  MARCH 1983    ADDED ASPECT RATIO ERROR                        *
C*  JULY 1983     ADDED LINEAR INTERPOLATION                      *
C*                REMOVED 7600 ACCESS CARDS                       *
C*  JULY 1984     CONVERTED TO FORTRAN77 AND GKS                  *
C*  JUNE 1988   CHANGED THREE COMMON BLOCK NAMES TO GET RID OF    *
C*              WARNING MESSAGES FROM SEGLDR ON THE CRAY          *
C*  MARCH 1990  CORRECTED USE OF SET CALLS                        *
C*                                                                *
C******************************************************************
C
      END
