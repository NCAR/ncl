C
C	$Id: conot2.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONOT2 (IVER,IUNIT)
C
C  OUTPUT THE OPTION VALUES TO THE LINE PRINTER
C
C  CONTINUE FOR CONRAN AND CONRAS
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
C  LABEL THE CONTOURS
C
      WRITE (IUNIT,1001)
      IF (LABON) GO TO  100
      WRITE (IUNIT,1002)
      GO TO  110
  100 WRITE (IUNIT,1003)
C
C  LABEL SIZE
C
  110 WRITE (IUNIT,1004) ISIZEL
C
C  SCALE DATA ON CONTOURS
C
      WRITE (IUNIT,1005)
      IF (SCALE .NE. 1.) GO TO  120
      WRITE (IUNIT,1006)
      GO TO  130
  120 WRITE (IUNIT,1007) SCALE
C
C  TENSION FACTOR
C
  130 WRITE (IUNIT,1008) TENS
C
C  PLOT RELATIVE MINS AND MAXS
C
      WRITE (IUNIT,1009)
      IF (PMIMX) GO TO  140
      WRITE (IUNIT,1010)
      GO TO  150
  140 WRITE (IUNIT,1011)
C
C  SIZE OF MINIMUM AND MAXIMUM LABELS
C
  150 WRITE (IUNIT,1012) ISIZEM
C
C  DASH PATTERN
C
      WRITE (IUNIT,1013)
      IF (IDASH(1:1) .EQ. ' ') GO TO  170
      WRITE (IUNIT,1014) IDASH
      GO TO  180
  170 WRITE (IUNIT,1015)
  180 IF (EDASH(1:1) .EQ. ' ') GO TO  200
      WRITE (IUNIT,1016) EDASH
      GO TO  210
  200 WRITE (IUNIT,1017)
  210 IF (NDASH(1:1) .EQ. ' ') GO TO  230
      WRITE (IUNIT,1018) NDASH
      GO TO  240
  230 WRITE (IUNIT,1019)
C
C  DASH PATTERN BREAK POINT
C
  240 WRITE (IUNIT,1020) BPSIZ
C
C  PRINT MINOR LINE GAP
C
      ITT = MINGAP-1
      WRITE (IUNIT,1021) ITT
      RETURN
C
 1001 FORMAT (5X,'LABEL THE CONTOURS, LAB=')
 1002 FORMAT ('+',28X,'OFF')
 1003 FORMAT ('+',28X,'ON')
 1004 FORMAT (5X,'CONTOUR LABEL SIZE IN PWRIT UNITS, LSZ=',I4)
 1005 FORMAT (5X,'SCALE THE DATA ON CONTOUR LINES, SDC=')
 1006 FORMAT ('+',41X,'OFF')
 1007 FORMAT ('+','ON, SCALE FACTOR=',G10.3)
 1008 FORMAT (5X,'TENSION FACTOR (USED FOR SMOOTH AND SUPER), TEN=',
     1        F6.2)
 1009 FORMAT (5X,'PLOT RELATIVE MINIMUMS AND MAXIMUMS, PMM=')
 1010 FORMAT ('+',45X,'OFF')
 1011 FORMAT ('+',45X,'ON')
 1012 FORMAT (5X,'SIZE OF MIN AND MAX LABELS IN PWRIT UNITS SML=',
     1        I4)
 1013 FORMAT (5X,'DASH PATTERN GTR=GREATER, EQU=EQUAL, LSS=LESS')
 1014 FORMAT (10X,'GTR=',A10)
 1015 FORMAT (10X,'GTR=$$$$$$$$$$')
 1016 FORMAT (10X,'EQU=',A10)
 1017 FORMAT (10X,'EQU=$$$$$$$$$$')
 1018 FORMAT (10X,'LSS=',A10)
 1019 FORMAT (10X,'LSS=$$$$$$$$$$')
 1020 FORMAT (5X,'DASH PATTERN BREAK POINT, DBP=',G10.3)
 1021 FORMAT (5X,'MINOR LINE COUNT=',I3)
C
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980   ADDED CONTERP TO ULIB                             *
C*  AUGUST 1980 FIXED THE FOLLOWING PROBLEMS                      *
C*              1.PLOTTING OF INPUT DATA VALUES                   *
C*              2.SETTING OF MINIMUM INTENSITY IN ALL OPTION      *
C*              3.SETTING OF EQU FLAG IN CONTOUR DASH PATTERN     *
C*              4.TURNING OFF OF SIZE OF PLOTTED DATA OPTION      *
C*  DECEMBER 1980 FIXED CONTOUR SELECTION ALGORITHM  AND MOVED IN *
C*                DASH PACKAGE COMMON BLOCK INTPR
C*  MARCH 1981  FIXED NON-PORTABLE STATEMENT ORDERING IN CONSET   *
C*  APRIL 1981  FIXED OPTION LISTING ROUTINE                      *
C*              ADDED MINOR LINE COUNT OPTION                     *
C*  JULY 1983   ADDED LINEAR INTERPOLATION AND SHIELDING          *
C*  JULY 1984     CONVERTED TO STANDARD FORTRAN77 AND GKS         *
C*  AUGUST 1985 DELETED LOC (MACHINE DEPENDENT FUNCTION), CHANGED *
C*              COMMON /CONR13/                                   *
C*  JUNE 1988   CHANGED THREE COMMON BLOCK NAMES TO GET RID OF    *
C*              WARNING MESSAGES FROM SEGLDR ON THE CRAY          *
C*                                                                *
C******************************************************************
C
      END
