C
C	$Id: gacwk.f,v 1.1.1.1 1992-04-17 22:33:34 ncargd Exp $
C
      SUBROUTINE GACWK(WKID)
C
      INTEGER EACWK
      PARAMETER (EACWK=4)
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
      INTEGER WKID
C     CHECK THAT GKS IS IN THE PROPER STATE
      CALL GZCKST(6,EACWK,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      CALL GZCKWK(25,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE WORKSTATION IS CURRENTLY ACTIVE
      CALL GZCKWK(29,EACWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK IF THERE IS ROOM FOR ANOTHER ACTIVE WORKSTATION
C
      IF (NACWK.GE.MACWK) THEN
      ERS = 1
      CALL GERHND(-101,EACWK,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     SET GKS OPERATING STATE TO WSAC
C
      OPS = GWSAC
C
C     ADD THE WORKSTATION IDENTIFIER TO THE SET OF ACTIVE WORKSTATIONS
C     IN THE GKS STATE LIST
C
      NACWK = NACWK+1
      SACWK(NACWK) = WKID
C
C     DETERMINE THE WORKSTATION TYPE
C
      DO   200 I=1,NOPWK
      IF (SOPWK(I).EQ.WKID) THEN
      NWKTP = SWKTP(I)
      GO TO 10
      ENDIF
  200 CONTINUE
   10 CONTINUE
C
C     INVOKE THE WORKSTATION INTERFACE SO THAT THE WORKSTATION
C     CAN BE MARKED ACTIVE IN THE WORKSTATION STATE LIST
C
      FCODE = -2
      CONT  = 0
      IL1 = 1
      IL2 = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EACWK,ERF)
      ERS = 0
      RETURN
      ENDIF
      RETURN
      END
