C
C	$Id: gqwkt.f,v 1.1.1.1 1992-04-17 22:33:48 ncargd Exp $
C
      SUBROUTINE GQWKT(WKID,ERRIND,TUS,RWINDO,CWINDO,
     +                 RVIEWP,CVIEWP)
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
      INTEGER WKID,ERRIND,TUS
      REAL    RWINDO(4),CWINDO(4),RVIEWP(4),CVIEWP(4)
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C     CHECK THAT THE WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C     INVOKE INTERFACE
      FCODE = -202
      CONT  = 0
      IL1   = 1
      IL2   = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERRIND = RERR
      GOTO 100
      ENDIF
      TUS = ID(2)
      RWINDO(1) = RX(1)
      RWINDO(2) = RX(2)
      RWINDO(3) = RX(3)
      RWINDO(4) = RX(4)
      CWINDO(1) = RX(5)
      CWINDO(2) = RX(6)
      CWINDO(3) = RX(7)
      CWINDO(4) = RX(8)
      RVIEWP(1) = RY(1)
      RVIEWP(2) = RY(2)
      RVIEWP(3) = RY(3)
      RVIEWP(4) = RY(4)
      CVIEWP(1) = RY(5)
      CVIEWP(2) = RY(6)
      CVIEWP(3) = RY(7)
      CVIEWP(4) = RY(8)
      RETURN
  100 CONTINUE
      TUS = -1
      RWINDO(1) = 0.
      RWINDO(2) = 0.
      RWINDO(3) = 0.
      RWINDO(4) = 0.
      CWINDO(1) = 0.
      CWINDO(2) = 0.
      CWINDO(3) = 0.
      CWINDO(4) = 0.
      RVIEWP(1) = 0.
      RVIEWP(2) = 0.
      RVIEWP(3) = 0.
      RVIEWP(4) = 0.
      CVIEWP(1) = 0.
      CVIEWP(2) = 0.
      CVIEWP(3) = 0.
      CVIEWP(4) = 0.
      RETURN
      END
