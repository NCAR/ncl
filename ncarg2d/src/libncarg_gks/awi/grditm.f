C
C	$Id: grditm.f,v 1.1.1.1 1992-04-17 22:33:48 ncargd Exp $
C
      SUBROUTINE GRDITM (WKID,MLDR,LDR,DATREC)
      INTEGER ERDITM
      PARAMETER (ERDITM=103)
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
      INTEGER WKID,LDR
      CHARACTER*80 DATREC(MLDR)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(7,ERDITM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION ID IS VALID
      CALL GZCKWK(20,ERDITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE WORKSTATION IS CURRENTLY OPEN
      CALL GZCKWK(25,ERDITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     SET FUNCTION CODE AND PUT OUT WKID, TYPE, LDR, AND THE DATA
C     RECORD IN STR.
C
      FCODE = 103
      CONT  = 0
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = MLDR
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ERDITM,ERF)
      ERS = 0
      RETURN
      ELSE
C
C     READ RETURNED DATA RECORD
C
      LDR = ID(3)
C
C     CHECK ON RETURNED LENGTH
C
      IF (LDR.GT.MLDR) THEN
      ERS = 1
      CALL GERHND(2001,ERDITM,ERF)
      ERS = 0
      RETURN
      ENDIF
      DATREC(1) = STR(1:80)
      IF (LDR.GT.1) THEN
      INDX = 1
  200 CONTINUE
      INDX = INDX+1
      CALL GZFMWK
      DATREC(INDX) = STR(1:80)
      IF (CONT.EQ.0.OR.INDX.GE.LDR) GO TO   201
      GO TO   200
  201 CONTINUE
      ENDIF
      ENDIF
      RETURN
      END
