C
C	$Id: gzputs.f,v 1.1.1.1 1992-04-17 22:33:53 ncargd Exp $
C
      SUBROUTINE GZPUTS(ST,IER)
C
C     THIS SUBROUTINE FILLS THE STRING CHARACTER
C     VARIABLE OF THE WORKSTATION
C     INTERFACE COMMON BLOCK AND INVOKES THE WORKSTATION
C     INTERFACE SUBROUTINE (N IS THE LENGTH OF STRING ST)
C     IF AN ERROR IS RETURNED FROM THE WORKSTATION INTERFACE,
C     IER IS SET TO THAT ERROR NUMBER AND A RETURN IS EXECUTED.
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
      CHARACTER*(*) ST
      N = LEN(ST)
      J = (N-1)/80
      STRL1 = N
      IF (J.EQ.0) THEN
C
C     CASE WHERE THERE IS NO CONTINUATION
C
      CONT = 0
      STRL2 = N
      STR(1:N) = ST(1:N)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
      ELSE
C
C     CASE WITH CONTINATION
C
      CONT = 1
      STRL2 = 80
C
C     LOOP THROUGH PARTS WITH CONTINUATION FLAG SET
C
      DO   200 K=1,J
      KPNT = (K-1)*80
      STR(1:80) = ST(KPNT+1:KPNT+80+1)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
  200 CONTINUE
C
C     PUT OUT LAST PART OF STRING WITH CONTINUATION FLAG SET TO LAST
C
      CONT = 0
      STRL2 = N-J*80
      KPNT = J*80
      STR(1:STRL2) = ST(KPNT+1:KPNT+STRL2+1)
      CALL GZTOWK
      ENDIF
      RETURN
      END
