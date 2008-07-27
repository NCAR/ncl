C
C	$Id: conop4.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONOP4 (IOPT,ARRAY,ISIZE,IFORT)
C
C SET THE CONTRAN OPTIONS
C
C INPUT
C     IOPT  --  CHARACTER STRING OF OPTION VALUE
C     ARRAY --  CHARACTER INPUT DATA
C     ISIZE --  INTEGER INPUT
C     IFORT --  INTEGER.  THIS VALUE IS USED ONLY WHEN IOPT IS
C               "FMT=ON".  IN THIS CASE, IFORT IS THE TOTAL NUMBER
C               OF CHARACTERS TO BE PROCESSED BY THE FORMAT
C               STATEMENT.  FOR EXAMPLE, FOR THE FORMAT "F10.3",
C               IFORT SHOULD BE SET TO 10.
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
        CHARACTER*(*)  ARRAY
        CHARACTER*7  IOPT
        CHARACTER*2  TAG, OPT
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
C  TITLE OPTION GET VALUE OF SWITCH
C
      IF (TAG .EQ. 'TL') THEN
C
C  SWITCH ON GET TITLE AND COUNT FROM INPUT
C
            IF (OPT .EQ. 'ON') THEN
              TITLE = .TRUE.
                ISTRNG = ARRAY
                  ICNT = ISIZE
              RETURN
C
C  SWITCH OFF OPTION DEACTIVATED
C
            ELSEIF (OPT .EQ. 'OF') THEN
              TITLE = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  CHANGE DATA VALUE FORMAT
C
      ELSEIF (TAG .EQ. 'FM') THEN
C
C  SWITCH ON GET USER FORMAT
C
          IF (OPT .EQ. 'ON') THEN
                FORM = ARRAY
              LEN = ISIZE
              IFMT = IFORT
              RETURN
C
C SWITCH OFF SET FORMAT TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                FORM = '(G10.3)'
                  LEN = LEND
              IFMT = IFMTD
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  DASH PATTERN OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'DA') THEN
C
C  SWITCH OFF DEFAULT PATTERNS
C
          IF (OPT .EQ. 'OF') THEN
                  NDASH = '$$$$$$$$$$'
                  EDASH = '$$$$$$$$$$'
                  IDASH = '$$$$$$$$$$'
                  RETURN
C
C  SWITCH ALL SET GTR,LSS,AND EQU TO SAME VALUE
C
          ELSEIF (OPT .EQ. 'AL') THEN
                  IDASH = ARRAY
                  EDASH = ARRAY
                  NDASH = ARRAY
                  RETURN
C
C  SWITCH SET TO POS CHANGE POS DASH PATTERN
C
          ELSEIF (OPT .EQ. 'GT') THEN
                  IDASH = ARRAY
                  RETURN
C
C  SWITCH SET TO NEG SET NEG DASH PATTERN
C
          ELSEIF (OPT .EQ. 'LS') THEN
                  NDASH = ARRAY
                  RETURN
C
C  SWITCH SET TO EQU SET EQUAL DASH PATTERN
C
          ELSEIF (OPT .EQ. 'EQ') THEN
                EDASH = ARRAY
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
  120 CALL SETER (' CONOP4-UNDEFINED OPTION',1,1)
      RETURN
        END
