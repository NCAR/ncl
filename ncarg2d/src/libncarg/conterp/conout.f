C
C	$Id: conout.f,v 1.2 2000-07-12 16:22:57 haley Exp $
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
      SUBROUTINE CONOUT (IVER)
C
C  LIST OUT ALL THE CONRAN OPTION VALUES ON THE LINE PRINTER
C
C  THE VALUE OF IVER DETERMINES WHICH ENTRY POINT CALLED THIS ROUTINE
C
C 1. CONRAQ
C 2. CONRAN
C 3. CONRAS
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
      SAVE
C
C  GET THE STANDARD OUTPUT UNIT TO WRITE THE OPTION VALUE LIST
C
      IUNIT = I1MACH(2)
C
C  PRINT OUT HEADER AND ALL OPTIONS WHICH APPLY TO CALLING VERSION
C
      GO TO ( 100, 110, 120),IVER
  100 WRITE (IUNIT,1001)
      GO TO  130
  110 WRITE (IUNIT,1002)
      GO TO  130
  120 WRITE (IUNIT,1003)
  130 WRITE (IUNIT,1004)
C
C  PERIMETER
C
      WRITE (IUNIT,1005)
      IF (PER) GO TO  140
      WRITE (IUNIT,1006)
      GO TO  150
  140 WRITE (IUNIT,1007)
C
C  GRID
C
  150 WRITE (IUNIT,1008)
      IF (GRD) GO TO  160
      WRITE (IUNIT,1009)
      GO TO  170
  160 WRITE (IUNIT,1010)
C
C  SCALING OF DATA ON FRAME
C
  170 WRITE (IUNIT,1011)
      GO TO ( 180, 190, 200),ISCALE+1
  180 WRITE (IUNIT,1012)
      GO TO  210
  190 WRITE (IUNIT,1013)
      GO TO  210
  200 WRITE (IUNIT,1014)
C
C  SAME DATA ANOTHER PLOT
C
  210 WRITE (IUNIT,1015)
      IF (REPEAT) GO TO  220
      WRITE (IUNIT,1016)
      GO TO  230
  220 WRITE (IUNIT,1017)
C
C  SHIELDING
C
  230 WRITE(IUNIT,2000)
      IF (SHIELD) GO TO 231
      WRITE(IUNIT,2001)
      GO TO 232
  231 WRITE(IUNIT,2002)
C
C  INTERPOLATION
C
  232 WRITE(IUNIT,2003)
      IF (LINEAR) GO TO 233
      WRITE(IUNIT,2004)
      GO TO 234
  233 WRITE(IUNIT,2005)
C
C  PLOT THE SHIELD
C
  234 WRITE(IUNIT,2006)
      IF (SLDPLT) GO TO 235
      WRITE(IUNIT,2007)
      GO TO 236
  235 WRITE(IUNIT,2008)
C
C  EXTRAPOLATION
C
  236 WRITE (IUNIT,1018)
      IF (EXTRAP) GO TO  240
      WRITE (IUNIT,1019)
      GO TO  250
  240 WRITE (IUNIT,1020)
C
C  STEP SIZE OR RESOLUTION OF THE GRID
C
  250 WRITE (IUNIT,1021) IGRAD
C
C  MESSAGE AT BOTTOM OF PLOT
C
      WRITE (IUNIT,1022)
      IF (MESS) GO TO  260
      WRITE (IUNIT,1023)
      GO TO  270
  260 WRITE (IUNIT,1024)
C
C  TITLE AT TOP OF PLOT
C
  270 WRITE (IUNIT,1025)
      IF (TITLE) GO TO  280
      WRITE (IUNIT,1026)
      GO TO  290
  280 WRITE (IUNIT,1027)
C
C  SIZE OF TITLE
C
  290 WRITE (IUNIT,1028) ITLSIZ
C
C  PRINT TITLE
C
      IF (ICNT.EQ.0 .OR. .NOT.TITLE) GO TO  310
      ICC = 100
      IF (ICC .GT. ICNT) ICC = ICNT
      WRITE (IUNIT,1029) ISTRNG
C
C  DATA POINTS USED FOR PARTIAL DERIVATIVE ESTIMATION
C
  310 WRITE (IUNIT,1030) NCP
C
C  LOOK AT TRIANGLES SWITCH
C
      WRITE (IUNIT,1031)
      IF (LOOK) GO TO  320
      WRITE (IUNIT,1032)
      GO TO  330
  320 WRITE (IUNIT,1033)
C
C  ADVANCE FRAME BEFORE PLOTTING TRIANGULATION
C
  330 WRITE (IUNIT,1034)
      IF (FRADV) GO TO  340
      WRITE (IUNIT,1035)
      GO TO  350
  340 WRITE (IUNIT,1036)
C
C  TRIANGLES ONLY PLOT
C
  350 WRITE (IUNIT,1037)
      IF (EXTRI) GO TO  360
      WRITE (IUNIT,1038)
      GO TO  370
  360 WRITE (IUNIT,1039)
C
C  PLOT THE INPUT DATA VALUES
C
  370 WRITE (IUNIT,1040)
      IF (PLDVLS) GO TO  380
      WRITE (IUNIT,1041)
      GO TO  390
  380 WRITE (IUNIT,1042)
C
C  FORMAT OF THE PLOTTED INPUT DATA
C
  390 WRITE (IUNIT,1043)
      IF (LEN .NE. 0) GO TO  400
      WRITE (IUNIT,1044)
      GO TO  420
  400 WRITE (IUNIT,1045) FORM
C
C  SIZE OF THE PLOTTED DATA VALUES
C
  420 WRITE (IUNIT,1046) ISIZEP
C
C  INTENSITY SETTINGS
C
      WRITE (IUNIT,1047)
      WRITE (IUNIT,1048) INMAJ,INMIN,INLAB,INDAT
C
C  DISTLAY CONTOUR SETTING
C
      WRITE (IUNIT,1049)
      IF (CON) GO TO  430
      WRITE (IUNIT,1050)
      GO TO  440
  430 WRITE (IUNIT,1051) NCL,(CL(I),I=1,NCL)
C
C  CONTOUR INCREMENT
C
  440 WRITE (IUNIT,1052)
      IF (CINC) GO TO  450
      WRITE (IUNIT,1053)
      GO TO  460
  450 WRITE (IUNIT,1054) FINC
C
C  CONTOUR HIGH AND LOW VALUES
C
  460 WRITE (IUNIT,1055)
      IF (CHILO) GO TO  470
      WRITE (IUNIT,1056)
      GO TO  480
  470 WRITE (IUNIT,1057) HI,FLO
C
C  CALL CONOT2 IF NOT QUICK VERSION
C
  480 IF (IVER .NE. 1) CALL CONOT2 (IVER,IUNIT)
C
C  THE ROUTINE CONOT2 WAS GENERATED TO ELIMINATE COMPILER ERRORS
C  RESULTING FROM TOO MANY FORMAT STATEMENTS IN ONE SUBROUTINE
C
      RETURN
C
C
 1001 FORMAT (1X,'CONRAQ')
 1002 FORMAT (1X,'CONRAN')
 1003 FORMAT (1X,'CONRAS')
 1004 FORMAT ('+',6X,'-OPTION VALUE SETTINGS',/
     1        ,7X,'ALL NON-PWRIT VALUES APPLY TO THE UNSCALED DATA')
 1005 FORMAT (5X,'PERIMETER, PER=')
 1006 FORMAT ('+',19X,'OFF')
 1007 FORMAT ('+',19X,'ON')
 1008 FORMAT (5X,'GRID, GRD=')
 1009 FORMAT ('+',14X,'OFF')
 1010 FORMAT ('+',14X,'ON')
 1011 FORMAT (5X,'SCALING OF PLOT ON FRAME, SCA=')
 1012 FORMAT ('+',34X,'ON')
 1013 FORMAT ('+',34X,'OFF')
 1014 FORMAT ('+',34X,'PRI')
 1015 FORMAT (5X,'SAME DATA FOR ANOTHER PLOT, REP=')
 1016 FORMAT ('+',36X,'OFF')
 1017 FORMAT ('+',36X,'ON')
 1018 FORMAT (5X,'EXTRAPOLATION, EXT=')
 1019 FORMAT ('+',23X,'OFF')
 1020 FORMAT ('+',23X,'ON')
 1021 FORMAT (5X,'RESOLUTION, SSZ=',I4)
 1022 FORMAT (5X,'MESSAGE, MES=')
 1023 FORMAT ('+',17X,'OFF')
 1024 FORMAT ('+',17X,'ON')
 1025 FORMAT (5X,'TITLE, TLE=')
 1026 FORMAT ('+',15X,'OFF')
 1027 FORMAT ('+',15X,'ON')
 1028 FORMAT (5X,'TITLE SIZE IN PWRIT UNITS, STL=',I4)
 1029 FORMAT (5X,'TITLE=',A64)
 1030 FORMAT (5X,'DATA POINTS USED FOR PARTIAL DERIVATIVE',
     1' ESTIMATION,  NCP=',I4)
 1031 FORMAT (5X,'LOOK AT TRIANGLES, TRI=')
 1032 FORMAT ('+',27X,'OFF')
 1033 FORMAT ('+',27X,'ON')
 1034 FORMAT (5X,'ADVANCE FRAME BEFORE PLOTTING TRIANGULATION,',
     1' TFR=')
 1035 FORMAT ('+',53X,'OFF')
 1036 FORMAT ('+',53X,'ON')
 1037 FORMAT (5X,'TRIANGULATION ONLY PLOT, TOP=')
 1038 FORMAT ('+',33X,'OFF')
 1039 FORMAT ('+',33X,'ON')
 1040 FORMAT (5X,'PLOT THE INPUT DATA VALUES, PDV=')
 1041 FORMAT ('+',36X,'OFF')
 1042 FORMAT ('+',36X,'ON')
 1043 FORMAT (5X,'FORMAT OF THE PLOTTED INPUT DATA, FMT=')
 1044 FORMAT ('+',42X,'(G10.3)')
 1045 FORMAT ('+',42X,A10)
 1046 FORMAT (5X,'SIZE OF THE PLOTTED DATA VALUES IN PWRIT',
     1' UNITS, SPD=',I4)
 1047 FORMAT (5X,'COLOR (INTENSITY) INDICES FOLLOW.',
     1' FOR CONRAQ MAJOR CONTOURS ARE ONLY USED')
 1048 FORMAT (10X,'MAJOR CONTOUR LINES, MAJ=',I4,/
     1        ,10X,'MINOR CONTOUR LINES, MIN=',I4,/
     2        ,10X,'TITLE AND MESSAGE, LAB=',I4,/
     3        ,10X,'PLOTTED DATA VALUES, DAT=',I4)
 1049 FORMAT (5X,'CONTOUR LEVELS,  CON=')
 1050 FORMAT ('+',25X,'OFF')
 1051 FORMAT ('+',25X,'ON, NCL=',I4,' ARRAY='/(10(2X,F10.3)))
 1052 FORMAT (5X,'CONTOUR INCREMENT, CIL=')
 1053 FORMAT ('+',27X,'OFF')
 1054 FORMAT ('+',27X,'ON, INCREMENT=',G10.3)
 1055 FORMAT (5X,'CONTOUR HIGH AND LOW VALUES, CHL=')
 1056 FORMAT ('+',37X,'OFF')
 1057 FORMAT ('+',37X,'ON, HI=',G10.3,' FLO=',G10.3)
 2000 FORMAT (5X,'SHIELDING, SLD=')
 2001 FORMAT ('+',19X,'OFF')
 2002 FORMAT ('+',19X,'ON')
 2003 FORMAT (5X,'INTERPOLATION, ITP=')
 2004 FORMAT ('+',23X,'C1 SURFACE')
 2005 FORMAT ('+',23X,'LINEAR')
 2006 FORMAT (5X,'PLOT THE SHIELD, SPT=')
 2007 FORMAT ('+',25X,'OFF')
 2008 FORMAT ('+',25X,'ON')
C
      END
