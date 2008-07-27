C
C	$Id: conpms.f,v 1.5 2008-07-27 00:23:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONPMS (SCRARR)
C
C THIS ROUTINE FINDS RELATIVE MINIMUMS AND MAXIMUMS.  A RELATIVE MINIMUM
C (OR MAXIMUM) IS DEFINED TO BE THE LOWEST (OR HIGHEST) POINT WITHIN
C A CERTAIN NEIGHBORHOOD OF THE POINT.  THE NEIGHBORHOOD USED HERE
C IS + OR - IXRG IN THE X DIRECTION AND + OR - IYRG IN THE Y DIRECTION.
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
C
      DIMENSION SCRARR(*)
        CHARACTER*10    IA
C
        SAVE
C
C  CONVERT FROM 0-32767 TO 1-1024
C
        DATA ICONV/32/
C
C  ACCESSING FUNCTION INTO SCRARR
C
      SCRTCH(IXX,IYY) = SCRARR(IYY+(IXX-1)*IYMAX)
C
C  GRAPHICS MAPPING FUNCTIONS
C
C     FX(XXX,YYY) = XXX
C     FY(XXX,YYY) = YYY
C
C  MAPPING FROM INTEGER TO USER INPUT FLOATING POINT
C
      CONVX(IXX) = XST + REAL(IXX-1)*STPSZ
      CONVY(IYY) = YST + REAL(IYY-1)*STPSZ
C
C  SET INTENSITY TO HIGH
C
      IF (INDAT .NE. 1 ) THEN
            CALL GSTXCI(INDAT)
        ELSE
            CALL GSTXCI (IRASTX)
        ENDIF
C
C  COMPUTE THE SEARCH RANGE FOR MIN AND MAX DETERMINATION
C
      IXRG = MIN(15,MAX(2,INT(REAL(IXMAX)/8.)))
      IYRG = MIN(15,MAX(2,INT(REAL(IYMAX)/8.)))
C
C  LOOP THROUGH ALL ROWS OF THE DATA SEARCHING FOR AN IMMEDIATE MIN OR
C  MAX.
C
      IX = 1
C
C  SCAN A ROW
C
C  IF EXTRAPOLATING DONT LIMIT ROW SCANS
C
   10 IF (.NOT.EXTRAP) GO TO    20
      IYST = 1
      IYED = IYMAX
      IY = 1
      GO TO    30
C
C  NOT EXTRAPOLATING STAY IN HULL BOUNDRIES
C
   20 IYST = ITLOC(IX*2-1)
      IYED = ITLOC(IX*2)
      IF (IYST.EQ.0) GO TO   240
      IY = IYST
   30 VAL = SCRTCH(IX,IY)
C
C  SEARCH FOR A MIN
C
C
C  BRANCH IF NOT FIRST ON A ROW
C
      IF (IY.NE.IYST) GO TO    40
      IF (VAL.GE.SCRTCH(IX,IY+1)) GO TO   130
      IF (VAL.GE.SCRTCH(IX,IY+2)) GO TO   130
      GO TO    60
C
C  BRANCH IF NOT LAST ON ROW
C
   40 IF (IY.NE.IYED) GO TO    50
      IF (VAL.GE.SCRTCH(IX,IY-1)) GO TO   140
      IF (VAL.GE.SCRTCH(IX,IY-2)) GO TO   140
      GO TO    60
C
C  IN MIDDLE OF ROW
C
   50 IF (VAL.GE.SCRTCH(IX,IY+1)) GO TO   150
      IF (VAL.GE.SCRTCH(IX,IY-1)) GO TO   150
C
C  POSSIBLE MIN FOUND SEARCH NEIGHBORHOOD
C
   60 IXST = MAX(1,IX-IXRG)
      IXSTOP = MIN(IXMAX,IX+IXRG)
C
C IF NOT EXTRAPOLATING BRANCH
C
   70 IF (.NOT.EXTRAP) GO TO    80
      IYSRS = 1
      IYSRE = IYMAX
      GO TO    90
C
C  NOT EXTRAPOLATING STAY IN CONVEX HULL
C
   80 IYSRS = ITLOC(IXST*2-1)
      IYSRE = ITLOC(IXST*2)
      IF (IYSRS.EQ.0) GO TO   120
C
   90 IYSRS = MAX(IYSRS,IY-IYRG)
      IYSRE = MIN(IYSRE,IY+IYRG)
C
  100 CUR = SCRTCH(IXST,IYSRS)
      IF (VAL.LT.CUR) GO TO   110
      IF (VAL.GT.CUR) GO TO   230
      IF (IX.EQ.IXST .AND. IY.EQ.IYSRS) GO TO   110
      GO TO   230
C
C  SUCCESS SO FAR TRY NEXT SPACE
C
  110 IYSRS = IYSRS+1
      IF (IYSRS.LE.IYSRE) GO TO   100
  120 IXST = IXST+1
      IF (IXST.LE.IXSTOP) GO TO    70
C
C  SUCCESS, WE HAVE FOUND A RELATIVE MIN
C
      X = CONVX(IX)
      Y = CONVY(IY)
      X1 = FX(X,Y)
      CALL FL2INT (X1,FY(X,Y),MX,MY)
      MX = MX/ICONV
      MY = MY/ICONV
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      XC = CPUX(MX)
      YC = CPUY(MY)
      CALL PWRTM(XC,YC,'L',ISIZEM,0,0)
C
C
      CALL CONECD (VAL,IA,NC)
      MY = MY - 2*ISIZEM
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      YC = CPUY(MY)
      CALL PWRTM(XC,YC,IA(1:NC),ISIZEM,0,0)
C
C
      GO TO   230
C
C  SEARCH FOR A LOCAL MAXIMUM
C
C  IF FIRST LOC ON A ROW
C
  130 IF (VAL.LE.SCRTCH(IX,IY+1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY+2)) GO TO   230
      GO TO   160
C
C  IF LAST ON ROW
C
  140 IF (VAL.LE.SCRTCH(IX,IY-1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY-2)) GO TO   230
      GO TO   160
C
C  IN MIDDLE OF ROW
C
  150 IF (VAL.LE.SCRTCH(IX,IY+1)) GO TO   230
      IF (VAL.LE.SCRTCH(IX,IY-1)) GO TO   230
C
C  POSSIBLE MIN FOUND SEARCH NEIGHBORHOOD
C
  160 IXST = MAX(1,IX-IXRG)
      IXSTOP = MIN(IXMAX,IX+IXRG)
  170 IF (.NOT.EXTRAP) GO TO   180
      IYSRS = 1
      IYSRE = IYMAX
      GO TO   190
C
C  NOT EXTRAPOLATING STAY IN CONVEX HULL
C
  180 IYSRS = ITLOC(IXST*2-1)
      IYSRE = ITLOC(IXST*2)
      IF (IYSRS.EQ.0) GO TO   220
C
  190 IYSRS = MAX(IYSRS,IY-IYRG)
      IYSRE = MIN(IYSRE,IY+IYRG)
C
  200 CUR = SCRTCH(IXST,IYSRS)
      IF (VAL.GT.CUR) GO TO   210
      IF (VAL.LT.CUR) GO TO   230
      IF (IX.EQ.IXST .AND. IY.EQ.IYSRS) GO TO   210
      GO TO   230
C
C  SUCCESS SO FAR TRY NEXT SPACE
C
  210 IYSRS = IYSRS+1
      IF (IYSRS.LE.IYSRE) GO TO   200
  220 IXST = IXST+1
      IF (IXST.LE.IXSTOP) GO TO   170
C
C  SUCCESS WE HAVE A MAXIMUM
C
      X = CONVX(IX)
      Y = CONVY(IY)
      X1 = FX(X,Y)
      CALL FL2INT (X1,FY(X,Y),MX,MY)
      MX = MX/ICONV
      MY = MY/ICONV
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      XC = CPUX(MX)
      YC = CPUY(MY)
      CALL PWRTM(XC,YC,'H',ISIZEM,0,0)
C
C
      CALL CONECD (VAL,IA,NC)
      MY = MY - 2*ISIZEM
C
C  POSITION STRINGS PROPERLY IF COORDS ARE IN PAU'S
C
C
C
      YC = CPUY(MY)
      CALL PWRTM(XC,YC,IA(1:NC),ISIZEM,0,0)
C
C
C  END OF SEARCH AT THIS LOCATION TRY NEXT
C
  230 IY = IY+1
      IF (IY.LE.IYED) GO TO    30
  240 IX = IX+1
      IF (IX.LE.IXMAX) GO TO    10
C
C
      RETURN
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980   ADDED CONRAN TO ULIB                              *
C*  AUGUST 1980 CHANGED ACCESS CARD DOCUMENTATION                 *
C*  DECEMBER 1980 MODIFIED COMMENT CARD DOCUMENTATION             *
C*  MARCH 1983  ADDED ASPECT RATIO ERROR                          *
C*  JULY 1983  ADDED SHIELDING AND LINEAR INTERPOLATION           *
C*             REMOVED 7600 ACCESS CARDS                          *
C*  JULY 1984  CONVERTED TO STANDARD FORTRAN77 AND GKS        *
C*  JUNE 1988   CHANGED THREE COMMON BLOCK NAMES TO GET RID OF    *
C*              WARNING MESSAGES FROM SEGLDR ON THE CRAY          *
C*  MARCH 1990  CORRECTED USE OF SET CALLS                        *
C*                                                                *
C******************************************************************
C
      END
