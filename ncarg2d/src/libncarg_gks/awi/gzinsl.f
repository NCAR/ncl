C
C	$Id: gzinsl.f,v 1.1.1.1 1992-04-17 22:33:53 ncargd Exp $
C
      SUBROUTINE GZINSL
C
C     THIS SUBROUTINE INITIALIZES THE GKS STATE LIST
C
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
      SOPWK (1)  = -1
      SOPWK (2)  = -1
      SACWK (1)  = -1
      SWKTP (1)  = -1
      SWKTP (2)  = -1
      CTXFP (1)  = 1
      CTXFP (2)  = 0
      CCHUP (1)  = 0.
      CCHUP (2)  = 1.
      CTXAL (1)  = 0
      CTXAL (2)  = 0
      CPA   (1)  = 1.
      CPA   (2)  = 1.
      CPARF (1)  = 0.
      CPARF (2)  = 0.
      LSNT  (1)  = 0
      LSNT  (2)  = 1
      NTWN(1,1)  = 0.
      NTWN(1,2)  = 1.
      NTWN(1,3)  = 0.
      NTWN(1,4)  = 1.
      NTWN(2,1)  = 0.
      NTWN(2,2)  = 1.
      NTWN(2,3)  = 0.
      NTWN(2,4)  = 1.
      NTVP(1,1)  = 0.
      NTVP(1,2)  = 1.
      NTVP(1,3)  = 0.
      NTVP(1,4)  = 1.
      NTVP(2,1)  = 0.
      NTVP(2,2)  = 1.
      NTVP(2,3)  = 0.
      NTVP(2,4)  = 1.
      CPLI       = 1
      CLN        = 1
      CLWSC      = 1.
      CPLCI      = 1
      CLNA       = GINDIV
      CLWSCA     = GINDIV
      CPLCIA     = GINDIV
      CPMI       = 1
      CMK        = 3
      CMKS       = 1.
      CPMCI      = 1
      CMKA       = GINDIV
      CMKSA      = GINDIV
      CPMCIA     = GINDIV
      CTXI       = 1
      CCHXP      = 1.0
      CCHSP      = 0.
      CTXCI      = 1
      CTXFPA     = GINDIV
      CCHXPA     = GINDIV
      CCHSPA     = GINDIV
      CTXCIA     = GINDIV
      CCHH       = .01
      CTXP       = 0
      CFAI       = 1
      CFAIS      = 0
      CFASI      = 1
      CFACI      = 1
      CFAISA     = GINDIV
      CFASIA     = GINDIV
      CFACIA     = GINDIV
      CNT        = 0
      CCLIP      = 1
      RETURN
      END
