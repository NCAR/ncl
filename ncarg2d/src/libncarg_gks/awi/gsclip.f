C
C	$Id: gsclip.f,v 1.1.1.1 1992-04-17 22:33:49 ncargd Exp $
C
      SUBROUTINE GSCLIP(CLSW)
      INTEGER ESCLIP
      PARAMETER (ESCLIP=53)
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
      INTEGER CLSW
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(8,ESCLIP,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK THAT CLSW IS IN RANGE
C
      IF (CLSW.LT.0.OR.CLSW.GT.1) THEN
      ERS = 1
      CALL GERHND(2000,ESCLIP,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     SET CLIPPING INDICATOR IN THE GKS STATE LIST
C
      CCLIP = CLSW
C
C     INVOKE THE WORKSTATION INTERFACE.
C     SEND THE CLIPPING INDICATOR IN ID(1) AND
C     THE VIEWPORT OF THE CURRENT NORMALIZATION TRANSFORMATION
C     IN RX,RY
C
      FCODE = 61
      CONT  = 0
      IL1 = 1
      IL2 = 1
      ID(1) = CLSW
      RL1   = 2
      RL2   = 2
      ICNT = CNT+1
      RX(1) = NTVP(ICNT,1)
      RX(2) = NTVP(ICNT,2)
      RY(1) = NTVP(ICNT,3)
      RY(2) = NTVP(ICNT,4)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ESCLIP,ERF)
      ERS = 0
      ENDIF
      RETURN
      END
