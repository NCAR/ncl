C
C	$Id: gzckst.f,v 1.1.1.1 1992-04-17 22:33:52 ncargd Exp $
C
      SUBROUTINE GZCKST(NUM,ENAM,IER)
C
C     THIS SUBROUTINE PROVIDES THE CHECKING FOR ERROR NUMBERS
C     1 THROUGH 8.
C
C     INPUT:
C       NUM   -- ERROR NUMBER TO CHECK FOR
C       ENAM  -- INDEX OF NAME OF CALLING PROGRAM (THIS IS NON-ZERO
C                ONLY FOR NON-INQUIRY FUNCTIONS, IN WHICH CASE GERHND
C                IS CALLED).
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
      INTEGER ENAM
      RERR = 0
      IER = 0
      GO TO (10,20,30,40,50,60,70,80) NUM
   10 CONTINUE
      IF (OPS.NE.GGKCL) THEN
      IER = 1
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(1,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   20 CONTINUE
      IF (OPS.NE.GGKOP) THEN
      IER = 2
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(2,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   30 CONTINUE
      IF (OPS.NE.GWSAC) THEN
      IER = 3
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(3,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   40 CONTINUE
      IF (OPS.NE.GSGOP) THEN
      IER = 4
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(4,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   50 CONTINUE
      IF (OPS.NE.GWSAC.AND.OPS.NE.GSGOP) THEN
      IER = 5
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(5,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   60 CONTINUE
      IF (OPS.NE.GWSAC.AND.OPS.NE.GWSOP) THEN
      IER = 6
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(6,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   70 CONTINUE
      IF (OPS.NE.GWSOP.AND.OPS.NE.GWSAC.AND.OPS.NE.GSGOP) THEN
      IER = 7
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(7,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      GO TO 100
   80 CONTINUE
      IF (OPS.NE.GGKOP.AND.OPS.NE.GWSOP.AND.OPS.NE.GWSAC.AND.OPS.NE.GSGO
     +P) THEN
      IER = 8
      IF (ENAM.GE.0) THEN
      ERS = 1
      CALL GERHND(8,ENAM,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
  100 CONTINUE
      RETURN
      END
