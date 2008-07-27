C
C	$Id: condrw.f,v 1.4 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONDRW (SCRARR)
C
C  DRAW ALL CONTOURS AT THIS LEVEL
C  IF NOT EXTRAPOLATING
C         SEARCH CONVEX HULL FOR CONTOURS INTERSECTING IT AND DRAW THEM
C         SEARCH INTERIOR AND DRAW ALL REMAINING UNDRAWN CONTOURS
C
C  IF EXTRAPOLATING
C         SEARCH FROM X START TO X END AND Y START TO Y END FOR ALL
C         CONTOURS AT THIS LEVEL
C
C  INPUT
C       SCRARR SCRATCH ARRAY USED FOR FAST CONTOURING
C       VIA COMMON BLOCKS BELOW
C           CONV-THE CURRENT CONTOUR LEVEL
C           ITLOC-THE CONVEX HULL BOUNDRIES RELATIVE TO THE SCRATCH
C                 ARRAY, SCRARR
C           PV-REAL Y COOORDINATES OF THE CONVEX HULL RELATIVE TO THE
C                 USERS COORDINATE SPACE
C           IXMAX,IYMAX-MAXINUM X AND Y COORDINATES RELATIVE TO THE
C                 SCRATCH ARRAY, SCRARR
C           XMAX,YMAX-MAXIMUM X AND Y COORDINATES RELATIVE TO USERS
C                 COORDINATE SPACE
C
C  OUTPUT
C    CONTOUR LINES OUTPUT TO PLOTTER FILE
C
C  NOTE
C       THIS ROUTINE WILL DETECT AND CORRECT FOR CONRAN ERROR 9
C
      DIMENSION       SCRARR(1)
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
C
C  FLAGS TO ALLOW COMPRESSION OF CONTOUR STORAGE IF IT IS EXAUSTED
C
        DATA ICOMP,NOCOMP/1,0/
C
C  STATEMENT FUNCTION TO MAKE ARRAY ACCESS SEEM LIKE MATRIX ACCESS
C
      SCRTCH(IXX,IYY) = SCRARR(IYY+(IXX-1)*IYMAX)
C
C  CLEAR THE CONTOUR STORAGE LIST
C
      NP = 0
C
C  SCAN X BOARDERS FOR INTERSECTIONS
C
      JX = 2
      ICASE = 1
      X = XST+STPSZ
C
C  IF NOT EXTRAPOLATING BRANCH
C
   10 IF (.NOT.EXTRAP) GO TO    20
      JY = 2
      JYE = IYMAX
      Y = YST+STPSZ
      GO TO    30
C
C  NOT EXTRAPOLATING
C
   20 JY = ITLOC(JX*2-1)
      IF (JY.EQ.0) GO TO    60
      JYE = ITLOC(JX*2)+1
      IF (JYE.GT.IYMAX) JYE = IYMAX
      Y = PV(JX*2-1)
      IF (JY.GE.2) GO TO 30
      JY = 2
      Y = YST+STPSZ
   30 TL = SCRTCH(JX-1,JY-1)
      BL = SCRTCH(JX,JY-1)
   40 TR = SCRTCH(JX-1,JY)
      BR = SCRTCH(JX,JY)
      CALL CONGEN (X,Y,NOCOMP,SCRARR,ICASE)
C
C  TEST IF CONTOUR STORAGE EXAUSTED
C
      IF (NERRO(NERR).NE.10) GO TO    50
      CALL EPRIN
      CALL ERROF
      RETURN
C
C  MOVE TO NEW CELL
C
   50 TL = TR
      BL = BR
      JY = JY+1
      Y = Y+STPSZ
      IF (JY.LE.JYE) GO TO    40
   60 IF (JX.EQ.IXMAX) GO TO    70
      JX = IXMAX
      ICASE = 3
      X = XMAX
      GO TO    10
C
C  SCAN Y BOARDERS
C
   70 IPOS = 1
      ICASE = 4
   80 JX = 3
      X = XST+STPSZ+STPSZ
C
C  IF NOT EXTRAPOLATING BRANCH
C
   90 IF (.NOT.EXTRAP) GO TO   100
      JY = 2
      Y = YST+STPSZ
      IF (IPOS.NE.0) GO TO   110
      JY = IYMAX
      Y = YED
      GO TO   110
C
C  NOT EXTRAPOLATING
C
 100    JY = ITLOC(JX*2 - IPOS )
      IF (JY.EQ.0) GO TO   120
        JY = JY + IPOS
      Y = PV(JX*2 - IPOS) + STPSZ*(1*IPOS)
  110 TL = SCRTCH(JX-1,JY-1)
      BL = SCRTCH(JX,JY-1)
      TR = SCRTCH(JX-1,JY)
      BR = SCRTCH(JX,JY)
      CALL CONGEN (X,Y,NOCOMP,SCRARR,ICASE)
C
C  TEST IF CONTOUR STORAGE EXAUSTED
C
      IF (NERRO(NERR).NE.10) GO TO   120
      CALL EPRIN
      CALL ERROF
      RETURN
C
C  MOVE TO NEW CELL
C
  120 JX = JX+1
      X = X+STPSZ
      IF (JX.LE.IXMAX-1) GO TO    90
      IF (IPOS.EQ.0) GO TO   130
      IPOS = 0
      ICASE = 2
      GO TO    80
C
C  BOARDER SEARCH DONE CONTOUR INTERIOR
C
C  INITIALIZE THE SEARCH
C
  130 JX = 3
      ICASE = 0
      X = XST+STPSZ+STPSZ
      JXE = IXMAX-1
C
C  IF EXTRAPOLATING GO FROM BORDER TO BORDER
C
  140 IF (.NOT.EXTRAP) GO TO   150
      JY = 3
      JYE = IYMAX-1
      Y = YST+STPSZ+STPSZ
      GO TO   160
C
C  NOT EXTRAPOLATING STAY IN HULL
C
  150 JY = ITLOC(JX*2 - 1)+2
      IF (JY.EQ.2) GO TO   190
      JYE = ITLOC(JX*2)-1
      Y = PV(JX*2 - 1)+STPSZ+STPSZ
C
  160 IF (JY.GT.JYE) GO TO   190
      TL = SCRTCH(JX-1,JY-1)
      BL = SCRTCH(JX,JY-1)
  170 TR = SCRTCH(JX-1,JY)
      BR = SCRTCH(JX,JY)
      CALL CONGEN (X,Y,ICOMP,SCRARR,ICASE)
C
C  TEST IF CONTOUR STORAGE EXAUSTED
C
      IF (NERRO(NERR).NE.10) GO TO   180
      CALL EPRIN
      CALL ERROF
      RETURN
C
C  MOVE TO NEW CELL
C
  180 JY = JY+1
      Y = Y+STPSZ
      TL = TR
      BL = BR
      IF (JY.LE.JYE) GO TO   170
C
C  PROCESS EACH ROW OF INTERIOR
C
  190 X = X+STPSZ
      JX = JX+1
      IF (JX.LE.JXE) GO TO   140
C
      RETURN
      END
