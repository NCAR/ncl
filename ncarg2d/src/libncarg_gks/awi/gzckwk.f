C
C	$Id: gzckwk.f,v 1.1.1.1 1992-04-17 22:33:52 ncargd Exp $
C
      SUBROUTINE GZCKWK(NUM,ENAM,WKID,WTYPE,IER)
C
C     THIS SUBROUTINE PROVIDES THE CHECKING FOR ERROR NUMBERS
C     20,22,23,24,25,29, AND 30.
C
C     INPUT:
C       NUM   -- ERROR NUMBER TO CHECK FOR
C       ENAM  -- INDEX OF NAME OF CALLING PROGRAM (THIS IS NON-ZERO
C                ONLY FOR NON-INQUIRY FUNCTIONS, IN WHICH CASE GERHND
C                IS CALLED).
C       WKID  -- WORKSTATION IDENTIFIER (WHERE APPLICABLE)
C       WTYPE -- WORKSTATION TYPE (WHERE APPLICABLE)
C
C     OUTPUT:
C       IER   -- 0 IF NO ERROR, THE ERROR NUMBER IF ERROR FOUND
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
      INTEGER ENAM,WKID,WTYPE
      IER = 0
      IF (NUM.EQ.20) THEN
C     CHECK IF WKID IS VALID
      IF (WKID.LT.0) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      ELSE IF (NUM.EQ.22) THEN
C     CHECK IF WTYPE IS VALID
      INDX = 0
  200 CONTINUE
      INDX = INDX+1
      IF (INDX.GT.WK) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      GO TO   201
      ELSE
      IF (LSWK(INDX).EQ.WTYPE) THEN
      GO TO   201
      ENDIF
      ENDIF
      GO TO   200
  201 CONTINUE
      ELSE IF (NUM.EQ.23) THEN
C     CHECK IF WTYPE IS VALID
      INDX = 0
  202 CONTINUE
      INDX = INDX+1
      IF (INDX.GT.WK) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      GO TO   203
      ELSE
      IF (LSWK(INDX).EQ.WTYPE) THEN
      GO TO   203
      ENDIF
      ENDIF
      GO TO   202
  203 CONTINUE
      ELSE IF (NUM.EQ.24) THEN
C     CHECK IF THE WORKSTATION IS CURRENTLY OPEN
      IF (NOPWK.GT.0) THEN
      DO   204 I=1,NOPWK
      IF (SOPWK(I).EQ.WKID) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
  204 CONTINUE
      ENDIF
      ELSE IF (NUM.EQ.25) THEN
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      INDX = 0
  205 CONTINUE
      INDX = INDX+1
      IF (INDX.GT.MOPWK) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      GO TO   206
      ELSE
      IF (SOPWK(INDX).EQ.WKID) THEN
      GO TO   206
      ENDIF
      ENDIF
      GO TO   205
  206 CONTINUE
      ELSE IF (NUM.EQ.29) THEN
C     CHECK IF THE WORKSTATION IS CURRENTLY ACTIVE
      DO   207 I=1,MACWK
      IF (SACWK(I).EQ.WKID) THEN
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
  207 CONTINUE
      ELSE IF (NUM.EQ.30) THEN
C     CHECK IF THE WORKSTATION IS CURRENTLY ACTIVE
      INDX = 0
  208 CONTINUE
      INDX = INDX+1
      IF (SACWK(INDX).EQ.WKID) GO TO   209
      IF (INDX.GT.MACWK) THEN
      IER = NUM
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(NUM,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      GO TO   209
      ENDIF
      GO TO   208
  209 CONTINUE
      ENDIF
      RETURN
      END
