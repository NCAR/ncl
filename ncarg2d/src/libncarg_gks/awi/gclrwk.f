C
C	$Id: gclrwk.f,v 1.1.1.1 1992-04-17 22:33:34 ncargd Exp $
C
      SUBROUTINE GCLRWK(WKID,COFL)
      INTEGER ECLRWK
      PARAMETER (ECLRWK=6)
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
      INTEGER WKID,COFL,FCODEO,CONTO
C     CHECK THAT GKS IS IN THE PROPER STATE
      CALL GZCKST(6,ECLRWK,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      CALL GZCKWK(25,ECLRWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     PUT OUT NEW PICTURE INITIALIZATION IF THE PICTURE IS EMPTY.
C
          IF (NOPICT .LE. 0) THEN
            FCODEO = FCODE
            CONTO  = CONT
            FCODE = 91
            CONT  =  0
            CALL G01WDR
            FCODE  = FCODEO
            CONT   = CONTO
            NOPICT = 1
          ENDIF
C
C     INVOKE THE WORKSTATION INTERFACE
C
      FCODE = 1
      CONT  = 0
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = COFL
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECLRWK,ERF)
      ERS = 0
      ENDIF
C
C     SET FLAG TO INDICATE THAT THE CURRENT PICTURE IS EMPTY.
C
      NOPICT = 0
      RETURN
      END
