C
C $Id: draws.f,v 1.6 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DRAWS (MX1,MY1,MX2,MY2,IDRAW,IMARK)
C
C THIS ROUTINE DRAWS THE VISIBLE PART OF THE LINE CONNECTING
C (MX1,MY1) AND (MX2,MY2).  IF IDRAW .NE. 0, THE LINE IS DRAWN.
C IF IMARK .NE. 0, THE VISIBILITY ARRAY IS MARKED.
C
      LOGICAL         VIS1       ,VIS2
      DIMENSION       PXS(2)     ,PYS(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DATA STEEP/5./
      DATA MX,MY/0,0/
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL SRFABD
C
C MAKE LINE LEFT TO RIGHT.
C
      MMX1 = MX1
      MMY1 = MY1
      MMX2 = MX2
      MMY2 = MY2
      IF (MMX1.EQ.NSPVAL .OR. MMX2.EQ.NSPVAL) RETURN
      IF (MMX1 .GT. MMX2) GO TO  10
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO  20
   10 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
   20 IF (NUPPER .LT. 0) GO TO 180
C
C CHECK UPPER VISIBILITY.
C
      VIS1 = NY1 .GE. (LIMU(NX1)-1)
      VIS2 = NY2 .GE. (LIMU(NX2)-1)
C
C VIS1 AND VIS2 TRUE MEANS VISIBLE.
C
      IF (VIS1 .AND. VIS2) GO TO 120
C
C VIS1 AND VIS2 FALSE MEANS INVISIBLE.
C
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 180
C
C FIND CHANGE POINT.
C
      IF (NX1 .EQ. NX2) GO TO 110
      DY = REAL(NY2-NY1)/REAL(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO  60
      DO  30 K=NX1P1,NX2
         MX = K
         MY = FNY1+REAL(K-NX1)*DY
         IF (MY .GT. LIMU(K)) GO TO  40
   30 CONTINUE
   40 IF (ABS(DY) .GE. STEEP) GO TO  90
   50 NX1 = MX
      NY1 = MY
      GO TO 120
   60 DO  70 K=NX1P1,NX2
         MX = K
         MY = FNY1+REAL(K-NX1)*DY
         IF (MY .LE. LIMU(K)) GO TO  80
   70 CONTINUE
   80 IF (ABS(DY) .GE. STEEP) GO TO 100
   81 NX2 = MX-1
      NY2 = MY
      GO TO 120
   90 IF (LIMU(MX) .EQ. 0) GO TO  50
      NX1 = MX
      NY1 = LIMU(NX1)
      GO TO 120
  100 IF (LIMU(MX-1).EQ.0) GO TO 81
      NX2 = MX-1
      NY2 = LIMU(NX2)
      GO TO 120
  110 IF (VIS1) NY2 = MIN(LIMU(NX1),LIMU(NX2))
      IF (VIS2) NY1 = MIN(LIMU(NX1),LIMU(NX2))
  120 IF (IDRAW .EQ. 0) GO TO 150
C
C DRAW VISIBLE PART OF LINE.
C
      IF (IROT) 130,140,130
  130 CONTINUE
      PXS(1) = REAL(NY1)
      PXS(2) = REAL(NY2)
      PYS(1) = REAL(1024-NX1)
      PYS(2) = REAL(1024-NX2)
      CALL GPL (2,PXS,PYS)
      GO TO 150
  140 CONTINUE
      PXS(1) = REAL(NX1)
      PXS(2) = REAL(NX2)
      PYS(1) = REAL(NY1)
      PYS(2) = REAL(NY2)
      CALL GPL (2,PXS,PYS)
  150 IF (IMARK .EQ. 0) GO TO 180
      IF (NX1 .EQ. NX2) GO TO 170
      DY = REAL(NY2-NY1)/REAL(NX2-NX1)
      FNY1 = NY1
      DO 160 K=NX1,NX2
         LTEMP = FNY1+REAL(K-NX1)*DY
         IF (LTEMP .GT. LIMU(K)) LIMU(K) = LTEMP
  160 CONTINUE
      GO TO 180
  170 LTEMP = MAX(NY1,NY2)
      IF (LTEMP .GT. LIMU(NX1)) LIMU(NX1) = LTEMP
  180 IF (NUPPER) 190,190,370
C
C SAME IDEA AS ABOVE, BUT FOR LOWER SIDE.
C
  190 IF (MMX1 .GT. MMX2) GO TO 200
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO 210
  200 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
  210 VIS1 = NY1 .LE. (LIML(NX1)+1)
      VIS2 = NY2 .LE. (LIML(NX2)+1)
      IF (VIS1 .AND. VIS2) GO TO 310
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 300
      DY = REAL(NY2-NY1)/REAL(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO 250
      DO 220 K=NX1P1,NX2
         MX = K
         MY = FNY1+REAL(K-NX1)*DY
         IF (MY .LT. LIML(K)) GO TO 230
  220 CONTINUE
  230 IF (ABS(DY) .GE. STEEP) GO TO 280
  240 NX1 = MX
      NY1 = MY
      GO TO 310
  250 DO 260 K=NX1P1,NX2
         MX = K
         MY = FNY1+REAL(K-NX1)*DY
         IF (MY .GE. LIML(K)) GO TO 270
  260 CONTINUE
  270 IF (ABS(DY) .GE. STEEP) GO TO 290
  271 NX2 = MX-1
      NY2 = MY
      GO TO 310
  280 IF (LIML(MX) .EQ. 1024) GO TO 240
      NX1 = MX
      NY1 = LIML(NX1)
      GO TO 310
  290 IF (LIML(MX-1).EQ.1024) GO TO 271
      NX2 = MX-1
      NY2 = LIML(NX2)
      GO TO 310
  300 IF (VIS1) NY2 = MAX(LIML(NX1),LIML(NX2))
      IF (VIS2) NY1 = MAX(LIML(NX1),LIML(NX2))
  310 IF (IDRAW .EQ. 0) GO TO 340
      IF (IROT) 320,330,320
  320 CONTINUE
      PXS(1) = REAL(NY1)
      PXS(2) = REAL(NY2)
      PYS(1) = REAL(1024-NX1)
      PYS(2) = REAL(1024-NX2)
      CALL GPL (2,PXS,PYS)
      GO TO 340
  330 CONTINUE
      PXS(1) = REAL(NX1)
      PXS(2) = REAL(NX2)
      PYS(1) = REAL(NY1)
      PYS(2) = REAL(NY2)
      CALL GPL (2,PXS,PYS)
  340 IF (IMARK .EQ. 0) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 360
      DY = REAL(NY2-NY1)/REAL(NX2-NX1)
      FNY1 = NY1
      DO 350 K=NX1,NX2
         LTEMP = FNY1+REAL(K-NX1)*DY
         IF (LTEMP .LT. LIML(K)) LIML(K) = LTEMP
  350 CONTINUE
      RETURN
  360 LTEMP = MIN(NY1,NY2)
      IF (LTEMP .LT. LIML(NX1)) LIML(NX1) = LTEMP
  370 RETURN
      END
