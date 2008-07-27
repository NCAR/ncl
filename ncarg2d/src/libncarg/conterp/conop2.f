C
C	$Id: conop2.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONOP2 (IOPT,ISIZE)
C
C SET THE CONTRAN OPTIONS
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C     ISIZE-  INTEGER INPUT
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
        COMMON /RANINT/ IRANMJ, IRANMN, IRANTX
        COMMON /RAQINT/ IRAQMJ, IRAQMN, IRAQTX
        COMMON /RASINT/ IRASMJ, IRASMN, IRASTX
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
C  SET RESOLUTION OF VIRTUAL GRID
C
      IF (TAG .EQ. 'SS') THEN
C
C  SWITCH = ON SET RESOLUTION OF VIRTUAL GRID
C
            IF (OPT .EQ. 'ON') THEN
              IGRAD = ISIZE
              RETURN
C
C  SWITCH = OFF RESET RESOLUTION TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
              IGRAD = IG
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  NCP OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'NC') THEN
C
C  SWITCH ON GET VALUE FOR NUMBER OF SURROUNDING DATA POINTS TO USE
C
            IF (OPT .EQ. 'ON') THEN
              NCP = ISIZE
              RETURN
C
C  SWITCH OFF SET TO DEFAULT VALUE
C
            ELSEIF (OPT .EQ. 'OF') THEN
              NCP = 4
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  INTENSITY OPTION FOUND GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'IN') THEN
C
C  SWITCH OFF SET DEFAULT VALUES
C
            IF (OPT .EQ. 'OF') THEN
                  IRANMJ = 1
                  IRANMN = 1
                  IRANTX = 1
                  IRASMJ = 1
                  IRASMN = 1
                  IRASTX = 1
                  IRAQMJ = 1
                  IRAQMN = 1
                  IRAQTX = 1
              INMAJ = 1
              INMIN = 1
              INDAT = 1
              INLAB = 1
              RETURN
C
C  SET PLOTTED DATA INTENSITY
C
            ELSEIF (OPT .EQ. 'DA') THEN
              INDAT = ISIZE
              RETURN
C
C SET TITLE AND MESSAGE INTENSITY
C
            ELSEIF (OPT .EQ. 'LA') THEN
              INLAB = ISIZE
                  IRANTX = ISIZE
                  IRASTX = ISIZE
                  IRAQTX = ISIZE
              RETURN
C
C  SET ALL INTENSITIES TO THE SAME VALUE
C
            ELSEIF (OPT .EQ. 'AL') THEN
                  IRANMJ = ISIZE
                  IRANMN = ISIZE
                  IRANTX = ISIZE
                  IRASMJ = ISIZE
                  IRASMN = ISIZE
                  IRASTX = ISIZE
                  IRAQMJ = ISIZE
                  IRAQMN = ISIZE
                  IRAQTX = ISIZE
              INMAJ = ISIZE
                INMIN = ISIZE
              INLAB = ISIZE
              INDAT = ISIZE
              RETURN
C
C  SET MAJOR LINE INTENSITY
C
            ELSEIF (OPT .EQ. 'MA') THEN
                  IRANMJ = ISIZE
                  IRASMJ = ISIZE
                  IRAQMJ = ISIZE
              INMAJ = ISIZE
              RETURN
C
C  SET MINOR LINE INTENSITY
C
            ELSEIF (OPT .EQ. 'MI') THEN
                  IRANMN = ISIZE
                  IRASMN = ISIZE
                  IRAQMN = ISIZE
                INMIN = ISIZE
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  LABEL SIZE OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'LS') THEN
C
C  SWITCH ON GET USER LABEL SIZE
C
            IF (OPT .EQ. 'ON') THEN
                ISIZEL = ISIZE
                RETURN
C
C  SWITCH OFF SET LABEL SIZE TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                ISIZEL = 9
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SET SIZES OF MINIMUM AND MAXIMUM LABELS
C
      ELSEIF (TAG .EQ. 'SM') THEN
C
C  SWTICH ON GET USERS SIZE
C
            IF (OPT .EQ. 'ON') THEN
                ISIZEM = ISIZE
                RETURN
C
C  SWTICH OFF SET TO DEFAULT VALUE
C
            ELSEIF (OPT .EQ. 'OF') THEN
                ISIZEM = 15
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SET SIZE OF THE PLOTTED DATA
C
      ELSEIF (TAG .EQ. 'SP') THEN
C
C  SWTICH ON GET USERS SIZE
C
            IF (OPT .EQ. 'ON') THEN
                ISIZEP = ISIZE
                RETURN
C
C  SWTICH OFF SET TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                ISIZEP = 8
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  TITLE SIZE SWITCH
C
      ELSEIF (TAG .EQ. 'ST') THEN
C
C  SWITCH ON SET THE TITLE SIZE
C
            IF (OPT .EQ. 'ON') THEN
                ITLSIZ = ISIZE
                RETURN
C
C  SWITCH OFF SET TITLE SIZE TO DEFAULT VALUE
C
            ELSEIF (OPT .EQ. 'OF') THEN
                ITLSIZ = 16
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  MINOR LINE COUNT OPTION
C
      ELSEIF (TAG .EQ. 'MI') THEN
C
C  SET MINOR LINE COUNT
C
            IF (OPT .EQ. 'ON') THEN
                MINGAP = ISIZE+1
                RETURN
C
C  SET MINOR LINE COUNT TO DEFAULT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                MINGAP = 3
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
  120 CALL SETER (' CONOP2 - UNDEFINED OPTION',1,1)
      RETURN
        END
