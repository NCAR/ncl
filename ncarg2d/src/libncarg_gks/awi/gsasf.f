C
C	$Id: gsasf.f,v 1.1.1.1 1992-04-17 22:33:48 ncargd Exp $
C
      SUBROUTINE GSASF(LASF)
      INTEGER ESASF
      PARAMETER (ESASF=41)
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
      INTEGER LASF(13)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(8,ESASF,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF FLAGS ARE 0 OR 1
      DO   200 I=1,13
      IF (LASF(I).NE.0.AND.LASF(I).NE.1) THEN
      ERS = 1
      CALL GERHND(2000,ESASF,ERF)
      ERS = 0
      ENDIF
  200 CONTINUE
C
C     SET THE CURRENT ASPECT SOURCE FLAGS IN THE GKS STATE LIST
C
      CLNA   = LASF( 1)
      CLWSCA = LASF( 2)
      CPLCIA = LASF( 3)
      CMKA   = LASF( 4)
      CMKSA  = LASF( 5)
      CPMCIA = LASF( 6)
      CTXFPA = LASF( 7)
      CCHXPA = LASF( 8)
      CCHSPA = LASF( 9)
      CTXCIA = LASF(10)
      CFAISA = LASF(11)
      CFASIA = LASF(12)
      CFACIA = LASF(13)
C
C     INVOKE THE WORKSTATION INTERFACE
C
      FCODE = 43
      CONT  = 0
      IL1   = 13
      IL2   = 13
      DO   201 I=1,13
      ID(I) = LASF(I)
  201 CONTINUE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ESASF,ERF)
      ERS = 0
      ENDIF
      RETURN
      END
