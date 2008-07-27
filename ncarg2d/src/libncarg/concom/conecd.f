C
C	$Id: conecd.f,v 1.5 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONECD (VAL,IOUT,NUSED)
C
C  ENCODE A NUMBER IN THE LEAST AMOUNT OF SPACE
C  ON INPUT
C       VAL THE NUMBER TO BE ENCODED
C  ON OUTPUT
C       IOUT CHARACTER STRING FILLED WITH THE ENCODED RESULT, MUST BE ABLE TO
C            HOLD UP TO 9 CHARACTERS.
C
C     NUSED NUMBER OF CHARACTERS IN IOUT
C
C  VALUE INPUT WILL BE SCALED BY SCALE IN CONRA2
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
        CHARACTER*(*)   IOUT
        CHARACTER*9     CHTMP
        CHARACTER*6     IFMT1
        CHARACTER*1     IT
C
        SAVE
C
      V = VAL
C
C  IF VAL EQUALS ZERO EASY PROCESSING
C
      IF (V.NE.0.) GO TO    20
        IOUT = '0.0'
      NUSED = 3
      RETURN
C
C  SCALE VALUE
C
   20 V = V*SCALE
C
C  GET SIZE OF NUMBER
C
      LOG = INT(ALOG10(ABS(V))+.1)
      IF (ABS(LOG).GT.4) GO TO    60
C
C  COMPUTE FLOATING POINT FIELD
C
      NS = ABS(LOG)+3
      ND = 1
      IF (LOG.GT.0) GO TO    40
C
C  LOG = 0 TEST FOR FRACTIONAL PART ONLY
C
      IF (ALOG10( ABS(V) ).GE.0.) GO TO    30
C
C  NUMBER LT 1 BUT GREATER THAN ZERO IN ABSOLUTE VALUE
C
      NS = 4
      ND = 1
      GO TO    40
C
C  NUMBER LESS THAN 10 BUT GE 1
C
   30 ND = 1
      NS = 4
C
C  BUILD THE FORMAT
C
   40 IF (V.LT.0) NS = NS+1
        IFMT1 = '(F . )'
C
C  INSERT THE FLOATING POINT FORMAT SIZE
C
        WRITE(IT,'(I1)')NS
        IFMT1(3:3) = IT
        WRITE(IT,'(I1)')ND
        IFMT1(5:5) = IT
C
C  ENCODE THE DESIRED NUMBER
C
        WRITE(CHTMP,IFMT1)V
        IOUT = CHTMP
      NUSED = NS
      RETURN
C
C  DATA LARGER THAN A NICE SIZE FORCE IT TO BE ENCODED
C
  60  WRITE(CHTMP,'(E8.3)')V
      IOUT = CHTMP
      NUSED = 8
      RETURN
C
C******************************************************************
C*                                                                *
C*                   REVISION HISTORY                             *
C*                                                                *
C*  JUNE 1980   ADDED CONCOM TO ULIB                              *
C*  AUGUST 1980 FIXED BOARDER CONTOUR DETECTION                   *
C*  DECEMBER 1980 FIXED ERROR TRAP, CONTOUR REORDERING ALGORITHM  *
C*                AND ERROR MESSAGE 10                            *
C*  AUGUST 1983 ADDED LINEAR INTERPOLATION AND SHIELDING          *
C*  JULY 1984 CONVERTED TO FORTRAN77 AND GKS                      *
C*  AUGUST 1985 DELETED (MACHINE DEPENDENT) FUNCTION LOC; CHANGED *
C*              COMMON /CONR13/                                   *
C*  JUNE 1988   CHANGED THREE COMMON BLOCK NAMES TO GET RID OF    *
C*              WARNING MESSAGES FROM SEGLDR ON THE CRAY          *
C*                                                                *
C******************************************************************
C
      END
