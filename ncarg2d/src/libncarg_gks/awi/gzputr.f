C
C	$Id: gzputr.f,v 1.1.1.1 1992-04-17 22:33:53 ncargd Exp $
C
      SUBROUTINE GZPUTR(N,P,Q,ICONV,IER)
C
C     THIS SUBROUTINE FILLS THE REAL ARRAYS OF THE WORKSTATION
C     INTERFACE COMMON BLOCK AND INVOKES THE WORKSTATION
C     INTERFACE SUBROUTINE.  IF ICONV=1, P,Q ARE ASSUMED TO BE
C     IN WORLD COORDINATES, AND ARE TRANSFORMED TO NDC
C     COORDINATES.  IF AN ERROR IS RETURNED FROM THE WORKSTATION
C     INTERFACE, IER IS SET TO THAT ERROR AND A RETURN IS EXECUTED.
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
      INTEGER N
      REAL P(N),Q(N),RP(128),RQ(128)
      J = (N-1)/128
      RL1 = N
      IF (J.EQ.0) THEN
C
C     CASE WHERE THERE IS NO CONTINUATION
C
      CONT = 0
      RL2 = N
      IF (ICONV.EQ.1) THEN
      CALL GZW2NX(N,P,RP)
      CALL GZW2NY(N,Q,RQ)
      DO   200 I=1,N
      RX(I) = RP(I)
      RY(I) = RQ(I)
  200 CONTINUE
      ELSE
      DO   201 I=1,N
      RX(I) = P(I)
      RY(I) = Q(I)
  201 CONTINUE
      ENDIF
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
      RL2 = 128
C
C     LOOP THROUGH PARTS WITH CONTINUATION FLAG SET
C
      IF (ICONV.EQ.1) THEN
      DO   202 K=1,J
      KPNT = (K-1)*128+1
      CALL GZW2NX(128,P(KPNT),RX)
      CALL GZW2NY(128,Q(KPNT),RY)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
  202 CONTINUE
      ELSE
      DO   203 K=1,J
      KPNT = (K-1)*128
      DO   204 I=1,128
      RX(I) = P(KPNT+I)
      RY(I) = Q(KPNT+I)
  204 CONTINUE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
  203 CONTINUE
      ENDIF
C
C     PUT OUT LAST PART OF ARRAY WITH CONTINUATION FLAG SET TO LAST
C
      CONT = 0
      RL2 = N-J*128
      KPNT = J*128
      IF (ICONV.EQ.1) THEN
      CALL GZW2NX(RL2,P(KPNT+1),RX)
      CALL GZW2NY(RL2,Q(KPNT+1),RY)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
      ELSE
      DO   205 I=1,RL2
      RX(I) = P(KPNT+I)
      RY(I) = Q(KPNT+I)
  205 CONTINUE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      IER = RERR
      RETURN
      ENDIF
      ENDIF
      ENDIF
      RETURN
      END
