C
C	$Id: conop3.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONOP3 (IOPT,ARRAY,ISIZE)
C
C SET THE CONTRAN OPTIONS
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C     ARRAY-   REAL ARRAY OF DIMENSION ISIZE
C     ISIZE-   SIZE OF ARRAY
C
C  SET COMMON DATA EQUAL TO INPUT DATA
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
C
C
C  INTPR IS THE DASH PACKAGE COMMON BLOCK INTERFACE
C  NP11 IS NP IN ALL OTHER INTPR DEFINITIONS; NAME CHANGE BECAUSE OF
C  CONFLICT
C
      COMMON /INTPR/  IPAU       ,FPART      ,TENSN      ,NP11       ,
     1                SMALL      ,L1         ,ADDLR      ,ADDTB      ,
     2                MLLINE     ,ICLOSE
        DIMENSION ARRAY(ISIZE)
        CHARACTER*7  IOPT
        CHARACTER*2  TAG, OPT
C
C
        SAVE
C
C  DETERMINE THE OPTION DESIRED
C
      TAG = IOPT(1:2)
        IF (IOPT(3:3) .EQ. '=') THEN
            OPT = IOPT(4:5)
        ELSE
            OPT = IOPT(5:6)
        ENDIF
C
C  CON CONTOUR LEVELS  CHECK VALUE OF SWITCH
C
      IF (TAG .EQ. 'CO') THEN
C
C  SWITCH = ON SET CONTOUR LEVELS
C
          IF (OPT .EQ. 'ON') THEN
              IF (CHILO .OR. CINC) GOTO  140
C
C  TEST IF NUMBER OF CONTOURS IS ACCEPTABLE
C
              IF (ISIZE .GT. 30)
     1    CALL SETER (' CONOP3-NUMBER OF CONTOUR LEVELS EXCEEDS 30',
     2                 1,1)
      DO  200 I=1,ISIZE
       CL(I) = ARRAY(I)
  200 CONTINUE
      CON = .TRUE.
      NCL = ISIZE
      RETURN
C
C  SWITCH = OFF CLEAR CONTOUR LEVEL ARRAY (PROGRAM SELECTS)
C
            ELSEIF (OPT .EQ. 'OF') THEN
              CON = .FALSE.
              RETURN
            ELSE
                  GOTO 120
          ENDIF
C
C  CONTOUR HI LO OPTION FOUND GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'CH') THEN
C
C  SWITCH ON SET HI AND FLO
C
            IF (OPT .EQ. 'ON') THEN
              IF (CON) GOTO  140
                  HI = ARRAY(1)
                  FLO = ARRAY(2)
                  CHILO = .TRUE.
                  RETURN
C
C  SWITCH OFF CLEAR FLAG
C
            ELSEIF (OPT .EQ. 'OF') THEN
                CHILO = .FALSE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  CONTOUR INCREMENT OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'CI') THEN
C
C  SWITCH ON SET INCREMENT
C
          IF (OPT .EQ. 'ON') THEN
              IF (CON) GOTO  140
              CINC = .TRUE.
              FINC = ARRAY(1)
              RETURN
C
C  SWITCH OFF CLEAR FLAG
C
          ELSEIF (OPT .EQ. 'OF') THEN
              CINC = .FALSE.
              RETURN
        ELSE
                GOTO 120
          ENDIF
C
C  SCALE THE DATA PLOTTED ON THE CONTOURS AND MIN MAX POINTS
C
      ELSEIF (TAG .EQ. 'SD') THEN
C
C  SWTICH ON GET SCALE FACTOR
C
            IF (OPT .EQ. 'ON') THEN
                SCALE = ARRAY(1)
                RETURN
C
C  SWTICH OFF SET FOR NO SCALING
C
            ELSEIF (OPT .EQ. 'OF') THEN
                SCALE = 1.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SET THE TENSION VALUE FOR SMOOTHING
C
      ELSEIF (TAG .EQ. 'TE') THEN
C
C  SWTICH ON SET TENSION FACTOR
C
            IF (OPT .EQ. 'ON') THEN
                TENSN = ARRAY(1)
                RETURN
C
C  SWTICH OFF SET TO DEFAULT TENSION
C
            ELSEIF (OPT .EQ. 'OF') THEN
                TENSN = TENS
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  DASH PATTERN BREAK POINT SWITCH
C
      ELSEIF (TAG .EQ. 'DB') THEN
C
C  SWITCH ON GET USERS BREAKPOINT
C
            IF (OPT .EQ. 'ON') THEN
                BPSIZ = ARRAY(1)
                RETURN
C
C  SWITCH OFF SET TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                BPSIZ = 0.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SHIELD OPTION
C
        ELSEIF (TAG .EQ. 'SL') THEN
C
C  TURN SHIELDING ON AND SET THE SHIELD COORD POINTERS
C
            IF (OPT .EQ. 'ON') THEN
                  NISIZE = ISIZE/2
                  CALL CONSSD(ARRAY(1),ARRAY(NISIZE+1),NISIZE)
                  RETURN
C
C  DEACTIVATE SHIELDING
C
            ELSEIF (OPT .EQ. 'OF') THEN
                  ICOUNT = 0
                  SHIELD = .FALSE.
                  RETURN
            ELSE
                  GOTO 120
            ENDIF
        ELSE
            GOTO 120
        ENDIF
C
C  ERROR UNDEFINED OPTION DETECTED
C
  120 CALL SETER (' CONOP3-UNDEFINED OPTION',1,1)
      RETURN
C
C  ILLEGAL USE OF CON WITH CIL OR CHL
C
  140 CALL SETER
     1('CONOP3-ILLEGAL USE OF CON OPTION WITH CIL OR CHL OPTION',
     2 1,1)
      RETURN
        END
