C
C	$Id: geclks.f,v 1.1.1.1 1992-04-17 22:33:35 ncargd Exp $
C
      SUBROUTINE GECLKS
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
C
C     UPDATE ALL OPEN WORKSTATIONS
C
      IF (OPS.GE.GWSOP) THEN
      IF (NOPWK.NE.0) THEN
      DO   200 I=1,NOPWK
      CALL GUWK(SOPWK(I),1)
  200 CONTINUE
      ENDIF
      ENDIF
C
C     CLOSE THE OPEN SEGMENT IF IN STATE GSGOP
C
      IF (OPS.EQ.GSGOP) THEN
      CALL GQOPSG(IER,ISGNE)
      IF (IER.EQ.0) THEN
      CALL GCLSG
      ENDIF
      ENDIF
C
C     DEACTIVATE ALL ACTIVE WORKSTATIONS
C
      IF (OPS.EQ.GWSAC) THEN
      IF (NACWK.NE.0) THEN
      NACWKT = NACWK
      DO   201 I=1,NACWKT
      CALL GDAWK(SACWK(NACWK))
  201 CONTINUE
      ENDIF
      ENDIF
C
C     CLOSE ALL OPEN WORKSTATIONS
C
      IF (OPS.GE.GWSOP) THEN
      IF (NOPWK.NE.0) THEN
      NOPWKT = NOPWK
      DO   202 I=1,NOPWKT
      CALL GCLWK(SOPWK(NOPWK))
  202 CONTINUE
      ENDIF
      ENDIF
C
C     MARK GKS CLOSED
C
      OPS = GGKCL
      RETURN
      END
