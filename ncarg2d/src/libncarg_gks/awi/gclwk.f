C
C	$Id: gclwk.f,v 1.1.1.1 1992-04-17 22:33:34 ncargd Exp $
C
      SUBROUTINE GCLWK(WKID)
      INTEGER ECLWK
      PARAMETER (ECLWK=3)
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
      CALL GZCKST(7,ECLWK,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,ECLWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      CALL GZCKWK(25,ECLWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE WORKSTATION IS CURRENTLY ACTIVE
      CALL GZCKWK(29,ECLWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     INVOKE THE WORKSTATION INTERFACE (DO THIS BEFORE FLAGGING
C     THE WORKSTATION AS CLOSED)
C
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
C     MAKE SURE WISS IS CLOSED BEFORE THE METAFILE WORKSTATION
C
      IF (NWKTP.EQ.3.AND.SOPWK(2).EQ.-1) THEN
      ERS = 1
      CALL GERHND(-104,ECLWK,ERF)
      ERS = 0
      RETURN
      ENDIF
      IF (NWKTP.EQ.1) THEN
      FCODE = 0
      CONT  = 0
      IL1  = 1
      IL2  = 1
      ID(1) = WKID
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECLWK,ERF)
      ERS = 0
      ENDIF
      ENDIF
C
C     REMOVE THE WORKSTATION IDENTIFIER FROM THE SET
C     OF OPEN WORKSTATIONS IN THE GKS STATE LIST
C
      IF (NOPWK.EQ.1) THEN
      SOPWK(1) = -1
      SWKTP(1) = -1
      NOPWK = 0
      ELSE
      DO   201 I=1,NOPWK
      IF (SOPWK(I).EQ.WKID) THEN
      IF (I.EQ.NOPWK) THEN
      SOPWK(NOPWK) = -1
      SWKTP(NOPWK) = -1
      NOPWK = NOPWK-1
      ELSE
      NM1 = NOPWK-1
      DO   202 J=I,NM1
      SOPWK(J) = SOPWK(J+1)
      SWKTP(J) = SWKTP(J+1)
  202 CONTINUE
      SOPWK(NOPWK) = -1
      SWKTP(NOPWK) = -1
      NOPWK = NOPWK-1
      ENDIF
      ENDIF
  201 CONTINUE
      ENDIF
C
C     SET GKS TO STATE GKOP IF NO WORKSTATIONS REMAIN OPEN
C
      IF (SOPWK(1).EQ.-1) THEN
      OPS = GGKOP
      ENDIF
C
C  Indicate that the metafile picture is empty.
C
      IF (NWKTP .EQ. 1) THEN
        NOPICT = 0
      ENDIF
C
      RETURN
      END
