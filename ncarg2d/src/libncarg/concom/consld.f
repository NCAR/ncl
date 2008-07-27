C
C	$Id: consld.f,v 1.5 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE CONSLD (SCRARR)
C
C  THIS ROUTINE IS USED TO GENERATE A SHIELD WHERE CONTOUR
C  DRAWING IS ALLOWED.
C
C  THE ROUTINE TAKES THE SILHOUETTE INFORMATION FROM COMMON BLOCK
C  CONR13 AND TRANSFORMS THIS INTO A SHIELD TO BE USED IN THE
C  SCRATCH ARRAY PASSED IN BY THE USER (THE SCRATCH ARRAY HOLDS THE
C  GRIDED DATA FROM THE INTERPOLATION).
C
C  INPUT
C       SCRARR-THE SCRATCH ARRAY HOLDING THE INTERPOLATED DATA
C
C
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONRA9/ ICOORD(500),  NP       ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
C
C  INCREASE THE RESOLUTION OF THE SHIELD PROFILE
C
      DIMENSION SCRARR(1)
C
        SAVE
      DATA RESINC/8.0/
C
C  STATEMENT FUNCTION TO MAKE ARRAY ACCESS SEEM LIKE MATRIX ACCESS
C
C
C
      IGADDR(XXX,YYY) =
     1 INT((YYY-YST)/STPSZ+.5)+(INT((XXX-XST)/STPSZ+.5))*IYMAX+1
C
C  SET THE SPECIAL VALUE
C
      SPVAL = SPVAL * 2.
C
C  SET THE USER ARRAY LOCATIONS TO TEMPORARY POINTERS
C
C  LOOP FOR ALL SHIELD ELEMENTS
C
      DO 100 IC = 1,ICOUNT
C
C       ASSIGN LINE SEGMENT END POINTS
C
        X1 = XVS(IC)
        Y1 = YVS(IC)
        IF (IC .EQ. ICOUNT) GO TO 10
                X2 = XVS(IC+1)
                Y2 = YVS(IC+1)
                GO TO 15
 10             CONTINUE
                X2 = XVS(1)
                Y2 = YVS(1)
 15     CONTINUE
C
C  INSURE THAT ALL POINTS ARE IN THE CONVEX HULL
C
      IF (X1.GT.XED) X1 = XED
      IF (X1.LT.XST) X1 = XST
      IF (X2.GT.XED) X2 = XED
      IF (X2.LT.XST) X2 = XST
      IF (Y1.GT.YED) Y1 = YED
      IF (Y1.LT.YST) Y1 = YST
      IF (Y2.GT.YED) Y2 = YED
      IF (Y2.LT.YST) Y2 = YST
C
C       SET THE START OF THE LINE SEGMENT SCRATCH LOCATION TO
C       THE SPECIAL VALUE
C
        II = IGADDR(X1,Y1)
        SCRARR(II) = SPVAL
C
C       FIND THE LENGTH OF THE LINE SEGMENT
C
        DIST = SQRT(((X2-X1)**2)+((Y2-Y1)**2))
C
C       IF LENGTH SHORTER THAN STEP SIZE THEN THERE IS NOTHING TO DO
C
        IF (DIST .LE. STPSZ) GO TO 100
C
C       SET UP LOOP TO SET ALL CELLS ON THE LINE SEGMENT
C
        NSTPS = (DIST/STPSZ)*RESINC
        XSTP = (X2-X1)/REAL(NSTPS)
        YSTP = (Y2-Y1)/REAL(NSTPS)
        X = X1
        Y = Y1
        DO 20 K = 1,NSTPS
                X = X + XSTP
                Y = Y + YSTP
                II = IGADDR(X,Y)
                SCRARR(II) = SPVAL
 20     CONTINUE
C
 100  CONTINUE
C
C  FILL THE SHIELDED AREAS
C       FOR EACH COLUMN THE ELEMENTS ARE SET TO SPVAL IF FILL IS TRUE.
C       THE VALUE OF FILL IS NEGATED EVERY TIME A SPVAL IS ENCOUNTERED,
C       AND THAT CELL REMAINS UNCHANGED.
C
C       LOOP THROUGH THE GRID
C
        DO 39 I = 1,IXMAX
C
C               GET THE START AND END FOR THE COLUMN
C
                IYS = (I-1)*IYMAX+1
                IYE = I*IYMAX
C
C               ADVANCE IN THE FORWARD DIRECTION
C
                DO 32 J = IYS,IYE
C
C                       IF NOT SPVAL THEN SET CELL AS APPROPIATE
C
                        IF (SCRARR(J).EQ.SPVAL) GO TO 33
                                SCRARR(J) = SPVAL
 32             CONTINUE
                GO TO 39
C
C               ADVANCE IN THE BACKWARD DIRECTION
C
 33             CONTINUE
                DO 34 J = 1,IYMAX
                        NJ =IYE+1-J
C                       IF NOT SPVAL THEN SET CELL AS APPROPIATE
C
                        IF (SCRARR(NJ).EQ.SPVAL) GO TO 39
                                SCRARR(NJ) = SPVAL
 34             CONTINUE
 39     CONTINUE
C
      RETURN
      END
