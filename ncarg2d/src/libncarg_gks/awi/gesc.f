C
C	$Id: gesc.f,v 1.1.1.1 1992-04-17 22:33:39 ncargd Exp $
C
      SUBROUTINE GESC(FCTID,LIDR,IDR,MLODR,LODR,ODR)
      INTEGER EESC
      PARAMETER (EESC=11)
C
C  Details on all GKS COMMON variables are in the GKS BLOCKDATA.
      COMMON/GKINTR/ NOPWK , NACWK , WCONID, NUMSEG,
     +               SEGS(100)     , CURSEG
      INTEGER        NOPWK , NACWK , WCONID, NUMSEG, SEGS  , CURSEG
      COMMON/GKOPDT/ OPS   , KSLEV , WK    , LSWK(2)       ,
     +               MOPWK , MACWK , MNT
      INTEGER        OPS   , WK
      COMMON/GKSTAT/ SOPWK(2)      , SACWK(1)      , CPLI  , CLN   ,
     +               CLWSC , CPLCI , CLNA  , CLWSCA, CPLCIA, CPMI  ,
     +               CMK   , CMKS  , CPMCI , CMKA  , CMKSA , CPMCIA,
     +               CTXI  , CTXFP(2)      , CCHXP , CCHSP , CTXCI ,
     +               CTXFPA, CCHXPA, CCHSPA, CTXCIA, CCHH  , CCHUP(2),
     +               CTXP  , CTXAL(2)      , CFAI  , CFAIS , CFASI ,
     +               CFACI , CFAISA, CFASIA, CFACIA, CPA(2), CPARF(2),
     +               CNT   , LSNT(2)       , NTWN(2,4)     , NTVP(2,4),
     +               CCLIP , SWKTP(2)      , NOPICT, NWKTP , MODEF
      INTEGER        SOPWK , SACWK , CPLI  , CLN   , CPLCI , CLNA  ,
     +               CLWSCA, CPLCIA, CPMI  , CMK   , CPMCI , CMKA  ,
     +               CMKSA , CPMCIA, CTXI  , CTXFP , CTXCI , CTXFPA,
     +               CCHXPA, CCHSPA, CTXCIA, CTXP  , CTXAL , CFAI  ,
     +               CFAIS , CFASI , CFACI , CFAISA, CFASIA, CFACIA,
     +               CNT   , LSNT  , CCLIP , SWKTP , NOPICT, NWKTP ,
     +               MODEF
      REAL           NTWN  , NTVP
      COMMON/GKEROR/ ERS   , ERF
      COMMON/GKENUM/ GBUNDL, GINDIV, GGKCL , GGKOP , GWSOP , GWSAC ,
     +               GSGOP , GOUTPT, GINPUT, GOUTIN, GWISS , GMO   ,
     +               GMI
      INTEGER        GBUNDL, GINDIV, GGKCL , GGKOP , GWSOP , GWSAC ,
     +               GSGOP , GOUTPT, GINPUT, GOUTIN, GWISS , GMO   ,
     +               GMI   , ERS   , ERF
      COMMON/GKSNAM/ GNAM(109)
      CHARACTER*6    GNAM
      COMMON/GKSIN1/ FCODE , CONT  , IL1   , IL2   , ID(128)       ,
     +               RL1   , RL2   , RX(128)       , RY(128)       ,
     +               STRL1 , STRL2 , RERR
      COMMON/GKSIN2/ STR
      INTEGER        FCODE , CONT  , RL1   , RL2   , STRL1 , STRL2 ,
     +               RERR
      CHARACTER*80   STR
C
      INTEGER FCTID,LIDR,FCODEO,CONTO
      CHARACTER*(*) IDR(LIDR),ODR(MLODR)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(8,EESC,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the input data record is of the proper length.
C
      LEN1 = LEN(IDR(1))
      IF (LEN1 .NE. 80) THEN
        ERS = 1
        CALL GERHND(182,EESC,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the function ID is supported.
C
      IF (FCTID.LT.0.AND.FCTID.NE.-1392.AND.FCTID.NE.-1391.AND.
     -                   FCTID.NE.-1393.AND.FCTID.NE.-1394.AND.
     -                   FCTID.NE.-1395) THEN
        ERS = 1
        CALL GERHND(180,EESC,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Process legal escape function ID'S:
C      -1391  --  Metafile name
C      -1392  --  FLASH4 support
C      -1393  --  Picture name
C      -1394  --  FLASH support
C      -1395  --  Cause a pause in ctrans processing.
C
      IF (FCTID .EQ. -1395) THEN
C
C  Put out a CGM ESCAPE element that will cause ctrans to pause.
C
        FCODE = 93
        CONT  = 0
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        IL1   = 1
        IL2   = 1
        ID(1) = FCTID
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
      ELSE IF (FCTID.EQ.-1394) THEN
C
C  This ESCAPE function supports the interaction of issuing
C  picture initialization and the GFLASH package.
C
        IF (IDR(1)(1:1) .EQ. '1') THEN
          MODEF = 1
        ELSE IF (IDR(1)(1:1) .EQ. '0') THEN
          MODEF = 0
        ELSE IF (IDR(1)(1:1) .EQ. '2') THEN
C
C  Put out new picture initialization if the picture is empty.
C
          IF (NOPICT .LE. 0) THEN
            FCODEO = FCODE
            CONTO  = CONT
            FCODE = 91
            CONT  =  0
            CALL G01WDR
            FCODE  = FCODEO
            CONT   = CONTO
            NOPICT = 1
          ENDIF
        ENDIF
      ELSE IF (FCTID.EQ.-1393) THEN
C
C  Picture name.
C
        FCODE = 92
        CONT = 0
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        IF (NOPICT .GT. 0) THEN
          ERS = 1
          RERR = -108
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
      ELSE IF (FCTID.EQ.-1392) THEN
C
C  FLASH4 support.
C
        FCODE = 83
        CONT = 0
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ELSE
          NUMSEG = NUMSEG+1
          SEGS(NUMSEG) = ID(1)
        ENDIF
      ELSE IF (FCTID.EQ.-1391) THEN
C
C  File name for output metafile.
C
        FCODE = 90
        CONT = 0
        STRL1 = 80
        STRL2 = 80
        STR(1:80) = IDR(1)
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EESC,ERF)
          ERS = 0
          RETURN
        ENDIF
C
C  Set flag to indicate that the current picture is empty.
C
        NOPICT = 0
      ELSE
C
C  Send ESCAPE element to CGM.
C
        FCODE = 6
        IL1 = 2
        IL2 = 2
        ID(1) = FCTID
        ID(2) = LIDR
C
C  Send over the data record if there is one (recall that the
C  string length of STR is divisible by 80).
C
        IF (LIDR.GE.1) THEN
          IF (LIDR.EQ.1) THEN
            CONT = 0
            STRL1 = 80
            STRL2 = 80
            STR(1:80) = IDR(1)
            CALL GZTOWK
            IF (RERR.NE.0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
          ELSE
C
C  Send over the data record 80 characters at a time.
C
            CONT = 1
            STRL1 = 80*LIDR
            STRL2 = 80
            LDRM1 = LIDR-1
            DO 200 I=1,LDRM1
              STR(1:80) = IDR(I)
              CALL GZTOWK
              IF (RERR.NE.0) THEN
                ERS = 1
                CALL GERHND(RERR,EESC,ERF)
                ERS = 0
                RETURN
              ENDIF
  200       CONTINUE
            CONT = 0
            STR(1:80) = IDR(LIDR)
            CALL GZTOWK
            IF (RERR.NE.0) THEN
              ERS = 1
              CALL GERHND(RERR,EESC,ERF)
              ERS = 0
              RETURN
            ENDIF
          ENDIF
        ELSE
          CONT = 0
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EESC,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
