C
C	$Id: gwitm.f,v 1.1.1.1 1992-04-17 22:33:52 ncargd Exp $
C
      SUBROUTINE GWITM (WKID,TYPE,LDR,DATREC)
      INTEGER EWITM
      PARAMETER (EWITM=101)
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
      INTEGER WKID,TYPE,LDR
      CHARACTER*80 DATREC(LDR)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(5,EWITM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION ID IS VALID
      CALL GZCKWK(20,EWITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK IF ITEM TYPE IS VALID (GKS INTERPRETABLE ITEMS ARE ILLEGAL)
C
      IF (TYPE.LE.100) THEN
      ERS = 1
      CALL GERHND(160,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     CHECK IF ITEM LENGTH IS VALID
C
      IF (LDR.LT.1) THEN
      ERS = 1
      CALL GERHND(161,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
C     CHECK IF THE WORKSTATION IS CURRENTLY ACTIVE
      CALL GZCKWK(30,EWITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     SET FUNCTION CODE AND PUT OUT WKID, TYPE, LDR, AND THE DATA
C     RECORD IN STR.
C
      FCODE = 101
      IL1 = 3
      IL2 = 3
      ID(1) = WKID
      ID(2) = TYPE
      ID(3) = LDR
C
C     SEND OVER THE DATA RECORD IF THERE IS ONE (RECALL THAT THE
C     STRING LENGTH OF STR IS DIVISIBLE BY 80).
C
      IF (LDR.GE.1) THEN
      IF (LDR.EQ.1) THEN
      CONT = 0
      STRL1 = 80
      STRL2 = 80
      STR(1:80) = DATREC(1)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ELSE
C
C     SEND OVER THE DATA RECORD 80 CHARACTERS AT A TIME
C
      CONT = 1
      STRL1 = 80*LDR
      STRL2 = 80
      LDRM1 = LDR-1
      DO   200 I=1,LDRM1
      STR(1:80) = DATREC(I)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
  200 CONTINUE
      CONT = 0
      STR(1:80) = DATREC(LDR)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      ELSE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EWITM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      RETURN
      END
