C
C $Id: perim3.f,v 1.4 2000-07-12 16:26:45 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PERIM3 (MAGR1,MINI1,MAGR2,MINI2,IWHICH,VAR)
      SAVE
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
      COMMON /PRM31/  Q          ,L
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
C
C THRINT COMMON BLOCK IS USED FOR SETTING COLOR INTENSITY
C
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
      DIMENSION       LASF(13)
C
      TICK(T) = AMAX1(UUMAX-UUMIN,VVMAX-VVMIN,WWMAX-WWMIN)*T/1024.
C
C INQUIRE LINE COLOR INDEX AND SET ASF TO INDIVIDUAL
C
      CALL GQPLCI (IERR, IPLCI)
      CALL GQASF (IERR, LASF)
      LSV3  = LASF(3)
      LASF(3) = 1
      CALL GSASF (LASF)
C
      MGR1 = MAGR1
      MN1 = MINI1-1
      MGR2 = MAGR2
      MN2 = MINI2-1
      MN1P1 = MAX0(MN1+1,1)
      MN2P1 = MAX0(MN2+1,1)
      L = MIN0(3,MAX0(1,IWHICH))
      Q = VAR
C
C PICK BOUNDS
C
      GO TO ( 10, 30, 40),L
   10 XMIN = VVMIN
      XMAX = VVMAX
      DELYL = TICK(TMAGV)
      DELYS = TICK(TMINV)
   20 YMIN = WWMIN
      YMAX = WWMAX
      DELXL = TICK(TMAGW)
      DELXS = TICK(TMINW)
      GO TO  50
   30 XMIN = UUMIN
      XMAX = UUMAX
      DELYL = TICK(TMAGU)
      DELYS = TICK(TMINU)
      GO TO  20
   40 XMIN = UUMIN
      XMAX = UUMAX
      DELYL = TICK(TMAGU)
      DELYS = TICK(TMINU)
      YMIN = VVMIN
      YMAX = VVMAX
      DELXL = TICK(TMAGV)
      DELXS = TICK(TMINV)
C
C PERIM
C
   50 CALL LINE3W (XMIN,YMIN,XMAX,YMIN)
      CALL LINE3W (XMAX,YMIN,XMAX,YMAX)
      CALL LINE3W (XMAX,YMAX,XMIN,YMAX)
      CALL LINE3W (XMIN,YMAX,XMIN,YMIN)
      IF (MGR1 .LT. 1) GO TO  90
      DX = (XMAX-XMIN)/AMAX0(MGR1*(MN1P1),1)
      DO  80 I=1,MGR1
C
C MINORS
C
         IF (MN1 .LE. 0) GO TO  70
C
         CALL GSPLCI (ITHRMN)
C
         DO  60 J=1,MN1
            X = XMIN+FLOAT(MN1P1*(I-1)+J)*DX
            CALL LINE3W (X,YMIN,X,YMIN+DELYS)
            CALL LINE3W (X,YMAX,X,YMAX-DELYS)
   60    CONTINUE
   70    IF (I .GE. MGR1) GO TO  90
C
         CALL GSPLCI (ITHRMJ)
C
         X = XMIN+FLOAT(MN1P1*I)*DX
C
C MAJORS
C
         CALL LINE3W (X,YMIN,X,YMIN+DELYL)
         CALL LINE3W (X,YMAX,X,YMAX-DELYL)
   80 CONTINUE
   90 IF (MGR2 .LT. 1) GO TO 130
      DY = (YMAX-YMIN)/AMAX0(MGR2*(MN2P1),1)
      DO 120 J=1,MGR2
         IF (MN2 .LE. 0) GO TO 110
C
         CALL GSPLCI (ITHRMN)
C
         DO 100 I=1,MN2
            Y = YMIN+FLOAT(MN2P1*(J-1)+I)*DY
            CALL LINE3W (XMIN,Y,XMIN+DELXS,Y)
            CALL LINE3W (XMAX,Y,XMAX-DELXS,Y)
  100    CONTINUE
  110    IF (J .GE. MGR2) GO TO 130
C
         CALL GSPLCI (ITHRMJ)
C
         Y = YMIN+FLOAT(MN2P1*J)*DY
         CALL LINE3W (XMIN,Y,XMIN+DELXL,Y)
         CALL LINE3W (XMAX,Y,XMAX-DELXL,Y)
  120 CONTINUE
C
C RESTORE ASF AND COLOR INDEX TO ORIGINAL
C
  130 LASF(3) = LSV3
      CALL GSASF (LASF)
      CALL GSPLCI (IPLCI)
      RETURN
      END
