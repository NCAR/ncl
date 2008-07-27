C
C	$Id: conop1.f,v 1.4 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONOP1 (IOPT)
C
C SET THE CONTRAN OPTIONS
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
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
C
        SAVE
C
C  DETERMINE OPTION AND ITS VALUE
C
        TAG = IOPT(1:2)
        IF (IOPT(3:3) .EQ. '=') THEN
                OPT = IOPT(4:5)
        ELSE
            OPT = IOPT(5:6)
        ENDIF
C
C  REP FOUND CHECK VALUE OF SWITCH
C
      IF (TAG .EQ. 'RE') THEN
C
C  SWITCH = ON CONTOUR SAME DATA
C
            IF (OPT .EQ. 'ON') THEN
              REPEAT = .TRUE.
              RETURN
C
C  SWITCH = OFF CONTOUR NEW DATA
C
            ELSEIF (OPT .EQ. 'OF') THEN
              REPEAT = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  EXTRAPOLATION FLAG
C
      ELSEIF (TAG .EQ. 'EX') THEN
C
C  SWITCH = ON EXTRAPOLATE WHEN CONTOURING
C
            IF (OPT .EQ. 'ON') THEN
              EXTRAP = .TRUE.
              RETURN
C
C  SWITCH = OFF INTERPOLATE ONLY
C
            ELSEIF (OPT .EQ. 'OF') THEN
              EXTRAP = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  PER FOUND SET PERIMETER
C
      ELSEIF (TAG .EQ. 'PE') THEN
C
C  SWITCH = ON DRAW PERIMETERS
C
            IF (OPT .EQ. 'ON') THEN
              PER = .TRUE.
C
C  TURN GRID OFF, USER WANTS PERIMETER
C
             GRD = .FALSE.
             RETURN
C
C  SWITCH = OFF DO NOT DRAW PERIMETERS
C
            ELSEIF (OPT .EQ. 'OF') THEN
             PER = .FALSE.
             RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  DEF FOUND SET ALL OPTIONS TO DEFAULT (NO SWITCHES)
C
        ELSEIF (TAG .EQ. 'DE') THEN
      PER = .TRUE.
      LISTOP = .FALSE.
      PMIMX = .FALSE.
      SCALE = 1.
      TENSN = TENS
      EXTRAP = .FALSE.
      TITLE = .FALSE.
      ITLSIZ = 16
      REPEAT = .FALSE.
      MESS = .TRUE.
      CON = .FALSE.
      CINC = .FALSE.
      CHILO = .FALSE.
      IGRAD = IG
      ISCALE = 0
      NCP = 4
      LOOK = .FALSE.
      GRD = .FALSE.
      PLDVLS = .FALSE.
      INMAJ = 1
      INMIN = 1
      INDAT = 1
      INLAB = 1
      IRANMJ = 1
      IRANMN = 1
      IRANTX = 1
      IRASMJ = 1
      IRASMN = 1
      IRASTX = 1
      IRAQMJ = 1
      IRAQMN = 1
      IRAQTX = 1
      BPSIZ = 0.
      LABON = .TRUE.
      ISIZEL = 9
      ISIZEP = 8
      ISIZEM = 15
      FRADV = .TRUE.
      EXTRI = .FALSE.
      MINGAP = 3
      LINEAR = .FALSE.
      ICOUNT = 0
      SHIELD = .FALSE.
      SLDPLT = .FALSE.
C
C  SET DEFAULT DASH PATTERN
C
         IDASH = '$$$$$$$$$$'
         NDASH = '$$$$$$$$$$'
         EDASH = '$$$$$$$$$$'
C
C  SET DEFAULT FORMAT
C
         FORM = '(G10.3)'
      RETURN
C
C  MES FOUND TEST VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'ME') THEN
C
C  ACTIVATE CONRAN MESSAGE
C
            IF (OPT .EQ. 'ON') THEN
              MESS = .TRUE.
              RETURN
C
C  TURN OFF CONRAN MESSAGE
C
            ELSEIF (OPT .EQ. 'OF') THEN
              MESS = .FALSE.
              RETURN
          ELSE
                  GOTO 120
          ENDIF
C
C  SCALING OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'SC') THEN
C
C  SET VALUE OF SCALE FLAG
C
            IF (OPT .EQ. 'ON') THEN
                  ISCALE = 0
                  RETURN
            ELSEIF (OPT .EQ. 'OF') THEN
                  ISCALE = 1
                  RETURN
            ELSEIF (OPT .EQ. 'PR') THEN
                  ISCALE = 2
                  RETURN
            ELSE
                  GOTO 120
            ENDIF
C
C  TRIANGLE FLAG GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'TR') THEN
C
C  SWITCH ON
C
            IF (OPT .EQ. 'ON') THEN
              LOOK = .TRUE.
              RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
              LOOK = .FALSE.
              RETURN
          ELSE
                  GOTO 120
          ENDIF
C
C  PLOT DATA VALUES FLAG GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'PD') THEN
C
C SWITCH ON
C
            IF (OPT .EQ. 'ON') THEN
              PLDVLS = .TRUE.
              RETURN
C
C  SWITCH OFF
C
            ELSEIF (OPT .EQ. 'OF') THEN
              PLDVLS = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  GRID OPTION ACTIVATED GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'GR') THEN
C
C  SWITCH ON SET GRID FLAG
C
            IF (OPT .EQ. 'ON') THEN
              GRD = .TRUE.
C
C  TURN PER OFF USER WANTS GRID
C
              PER = .FALSE.
              RETURN
C
C  SWITCH OFF CLEAR GRID FLAG
C
            ELSEIF (OPT .EQ. 'OF') THEN
              GRD = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  LABEL PLOTTING FLAG GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'LA') THEN
C
C  SWITCH ON LABEL CONTOURS
C
            IF (OPT .EQ. 'ON') THEN
              LABON = .TRUE.
                RETURN
C
C  SWITCH OFF DON"T LABEL CONTOURS
C
            ELSEIF (OPT .EQ. 'OF') THEN
              LABON = .FALSE.
              RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  PLOT THE RELATIVE MIN"S AND MAX"S
C
      ELSEIF (TAG .EQ. 'PM') THEN
C
C  SWTICH ON PLOT THE INFO
C
            IF (OPT .EQ. 'ON') THEN
              PMIMX = .TRUE.
                RETURN
C
C  SWTICH OFF DO NOT PLOT THE INFO
C
            ELSEIF (OPT .EQ. 'OF') THEN
              PMIMX = .FALSE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  ADVANCE FRAME BEFORE TRIANGULATION PLOT
C
      ELSEIF (TAG .EQ. 'TF') THEN
C
C  SWITCH ON ADVANCE FRAME
C
          IF (OPT .EQ. 'ON') THEN
                FRADV = .TRUE.
                RETURN
C
C  SWITCH OFF DO NOT ADVANCE FRAME
C
            ELSEIF (OPT .EQ. 'OF') THEN
                FRADV = .FALSE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  EXIT AFTER TRIANGULATION
C
      ELSEIF (TAG .EQ. 'TO') THEN
C
C  SWITCH ON EXIT AFTER TRIANGULATION
C
          IF (OPT .EQ. 'ON') THEN
                EXTRI = .TRUE.
                LOOK = .TRUE.
                FRADV = .FALSE.
                RETURN
C
C  SWITCH OFF DO NOT EXIT AFTER TRIANGULATION
C
          ELSEIF (OPT .EQ. 'OF') THEN
                FRADV = .TRUE.
                LOOK = .FALSE.
                EXTRI = .FALSE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  LIST OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'LO') THEN
C
C  ON SET LIST OPTIONS FLAG
C
            IF (OPT .EQ. 'ON') THEN
              LISTOP = .TRUE.
                RETURN
C
C  TURN OFF LIST OPTIONS FLAG
C
          ELSEIF (OPT .EQ. 'OF') THEN
              LISTOP = .FALSE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SET THE INTERPOLATION SCHEME
C
      ELSEIF (TAG .EQ. 'IT') THEN
C
C  SET TO C1 SURFACE
C
            IF (OPT .EQ. 'C1') THEN
              LINEAR = .FALSE.
                RETURN
C
C  SET TO LINEAR INTERPOLATION
C
            ELSEIF (OPT .EQ. 'LI') THEN
                LINEAR = .TRUE.
                RETURN
          ELSE
                  GOTO 120
            ENDIF
C
C  SET THE SHIELD PLOT FLAG
C
      ELSEIF (TAG .EQ. 'PS') THEN
C
C  TURN ON SHIELD PLOT
C
            IF (OPT .EQ. 'ON') THEN
                SLDPLT = .TRUE.
                RETURN
C
C  TURN OFF SHIELD PLOT
C
            ELSEIF (OPT .EQ. 'OF') THEN
                SLDPLT = .FALSE.
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
  120 CALL SETER (' CONOP1 -- UNDEFINED OPTION',1,1)
      RETURN
C
        END
