C
C	$Id: conlod.f,v 1.4 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONLOD (XD,YD,ZD,NDP,WK,IWK,SCRARR)
C
C******************************************************************
C*                                                                *
C*   THIS FILE IS A PACKAGE OF SUPPORT ROUTINES FOR THE ULIB      *
C*   FILES CONRAN AND CONRAS.  SEE THOSE FILES FOR AN             *
C*   EXPLAINATION OF THE ENTRY POINTS.                            *
C*                                                                *
C******************************************************************
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
      COMMON /CONR14/LINEAR
      LOGICAL LINEAR
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
      DIMENSION       SCRARR(1)
C
        SAVE
C
C  IFR - FLAG TO REGISTER FIRST PASS IN Y DIRECTION
C
C  LOAD THE SCRATCH SPACE AND CONVEX HULL POINTERS
C  ITLOC IS THE LIST OF CONVEX HULL POINTERS RELATIVE TO THE SCARTCH
C  SPACE.
C  PV IS THE LIST OF CONVEX HULL POINTERS RELATIVE TO USER COORDINATES
C
C  INITALIZE THE SPECIAL VALUE FEATURE
C
      X = (XED-XST)/2. + XST
      Y = (YED-YST)/2. + YST
      IF(LINEAR) GO TO 1
      SPVAL = CONCOM(X,Y,XD,YD,ZD,NDP,WK,IWK,IT)
        GO TO 2
 1    SPVAL = CONLCM(X,Y,XD,YD,ZD,NDP,WK,IWK,IT)
 2    CONTINUE
C
C  INITIALIZE THE SEARCH
C
      IYMAX = 0
      IFR = 1
      JX = 1
      X = XST
   10 JY = 1
      Y = YST
C
C  SET HULL POINTERS FOR THIS COLUMN TO NULL
C
      ITLOC(JX*2-1) = 0
      ITLOC(JX*2) = 0
C
C  FLAG START OF COLUMN
C
      LOOP = 1
C
C  GET INTERPOLATED VALUE
C
  20  IF (LINEAR) GO TO 3
      RVAL = CONCOM(X,Y,XD,YD,ZD,NDP,WK,IWK,IT)
      GO TO 4
  3   RVAL = CONLCM(X,Y,XD,YD,ZD,NDP,WK,IWK,IT)
  4   CONTINUE
      SCRARR(JY+(JX-1)*IYMAX) = RVAL
      IF (RVAL.GT.SPVAL) SPVAL = RVAL
C
C  IF OUTSIDE CONVEX HULL BRANCH
C
      IF (IT.GT.NTNL) GO TO    30
C
C  IF OUTSIDE TRIANGLES AND USING LINEAR INTERPLOATION THEN BRANCH
C
      IF(LINEAR.AND.IT.GT.NT) GO TO 30
C
C  IF FIRST OF COLUMN IN HULL CONTINUE THROUGH
C
      IF (LOOP.NE.1) GO TO    40
C
C  SET HULL POINTERS
C
      PV(JX*2-1) = Y
      ITLOC(JX*2-1) = JY
C
C  SET FLAG TO LOOK FOR END OF HULL IN COLUMN
C
      LOOP = 2
C
C  GO FOR NEXT ENTRY
C
      GO TO    40
C
C  TEST FOR END OF CONVEX HULL ON THIS ROW
C
   30 IF (LOOP.NE.2) GO TO    40
C
C  END OF HULL SET POINTERS FOR END OF HULL AND FLAG IT VIA LOOP
C
      LOOP = 0
      ITLOC(JX*2) = JY-1
      PV(JX*2) = Y-STPSZ
C
C  GET NEXT ELEMENT IN ROW IF NOT OUTSIDE ENCLOSING RECTANGULAR
C  BOARDER
C
   40 Y = Y+STPSZ
      JY = JY+1
      IF (Y.LE.YED) GO TO    20
C
C  TEST FOR FIRST COLUMN
C
      IF (IFR.NE.1) GO TO    50
C
C  FIRST COLUMN OVER SET MAX Y VALUES
C
      IYMAX = JY-1
      YMAX = Y-STPSZ
      IFR = 0
C
C  IF HULL WENT TO EDGE OF RECTANGULAR BOARDER SET HULL POINTERS HERE
C
   50 IF (LOOP.NE.2) GO TO    60
      PV(JX*2) = Y-STPSZ
      ITLOC(JX*2) = JY-1
C
C  END OF COLUMN GET NEXT ONE
C
   60 X = X+STPSZ
      JX = JX+1
C
C  IF NOT END OF WORK CONTINUE WITH NEXT COLUMN
C
      IF (X.LE.XED) GO TO    10
C
C  END OF WORK SET MAX X VALUES
C
      IXMAX = JX-1
      XMAX = X-STPSZ
      RETURN
      END
