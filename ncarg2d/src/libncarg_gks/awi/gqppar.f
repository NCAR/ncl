C
C	$Id: gqppar.f,v 1.1.1.1 1992-04-17 22:33:46 ncargd Exp $
C
      SUBROUTINE GQPPAR(WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY)
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
      INTEGER WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY(NMX,MMX)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C     CHECK THAT THE WORKSTATION TYPE IS VALID
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C     CHECK IF INDEX IS POSITIVE
C
      IF (PPAI.LT.1) THEN
      ERRIND = 79
      GO TO 100
      ENDIF
C     INVOKE INTERFACE
      FCODE = -120
      CONT  = 0
      IL1   = 4
      IL2   = 4
      ID(1) = WTYPE
      ID(2) = PPAI
      ID(3) = NMX
      ID(4) = MMX
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERRIND = RERR
      GOTO 100
      ENDIF
      N = ID(5)
      M = ID(6)
C
C     BRING OVER PATTERN ARRAY
C
      INDX = (N*M-1)/128
      IF (INDX.EQ.0) THEN
      CALL GZFMWK
      INDX = 0
      DO   200 J=1,M
      DO   201 I=1,N
      INDX = INDX+1
      PARRAY(I,J) = ID(INDX)
  201 CONTINUE
  200 CONTINUE
      ELSE
      CALL GZFMWK
      INDX = 0
      DO   202 J=1,M
      DO   203 I=1,N
      INDX = INDX+1
      PARRAY(I,J) = ID(INDX)
      JMD = MOD(INDX,128)
      IF (JMD.EQ.0.AND.CONT.EQ.1) THEN
      CALL GZFMWK
      INDX = 0
      ENDIF
  203 CONTINUE
  202 CONTINUE
      ENDIF
      RETURN
  100 CONTINUE
      N = -1
      M = -1
      PARRAY(1,1) = -1
      RETURN
      END
