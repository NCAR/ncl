C
C	$Id: gswn.f,v 1.1.1.1 1992-04-17 22:33:52 ncargd Exp $
C
      SUBROUTINE GSWN (TNR,XMIN,XMAX,YMIN,YMAX)
      INTEGER ESWN
      PARAMETER (ESWN=49)
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
      INTEGER TNR
      REAL XMIN,XMAX,YMIN,YMAX
      DATA IFRST/0/
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(8,ESWN,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK THAT THE NORMALIZATION TRANSFORMATION NUMBER IS VALID
C
      IF (TNR.LT.1.OR.TNR.GT.MNT) THEN
      ERS = 1
      CALL GERHND(50,ESWN,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     CHECK THAT THE RECTANGLE DEFINITION IS VALID
C
      IF (XMAX.LE.XMIN.OR.YMAX.LE.YMIN) THEN
      IF (IFRST.EQ.0) THEN
      WRITE(ERF,333)
  333 FORMAT(' WARNING - OTHER GKS IMPLEMENTATIONS MAY NOT ALLOW WINDOW
     +XMIN > XMAX OR WINDOW YMIN > YMAX (AS THIS ONE DOES)')
      ENDIF
      ENDIF
C
C     SET THE NORMALIZATION TRANSFORMATION WINDOW IN THE GKS STATE LIST
C
      INR = TNR+1
      NTWN(INR,1) = XMIN
      NTWN(INR,2) = XMAX
      NTWN(INR,3) = YMIN
      NTWN(INR,4) = YMAX
C
C     REESTABLISH CHARACTER HEIGHT AND UP VECTOR, AND
C     PATTERN SIZE AND REFERENCE POINT IF NORMALIZATION
C     TRANSFORMATION NUMBER TNR IS THE CURRENT ONE.
C
      CALL GQCNTN(IER,ICUR)
      IF (TNR.EQ.ICUR) THEN
      CALL GSCHH(CCHH)
      CALL GSCHUP(CCHUP(1),CCHUP(2))
      CALL GSPA(CPA(1),CPA(2))
      CALL GSPARF(CPARF(1),CPARF(2))
      ENDIF
C
C     SET FLAG TO INDICATE THAT GSWN HAS BEEN CALLED
C
      IFRST = 1
      RETURN
      END
