C
C	$Id: gqtxx.f,v 1.1.1.1 1992-04-17 22:33:47 ncargd Exp $
C
      SUBROUTINE GQTXX(WKID,PX,PY,STRX,ERRIND,CPX,CPY,
     +           TXEXPX,TXEXPY)
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
      INTEGER WKID,ERRIND
      REAL    PX,PY,CPX,CPY,TXEXPX(4),TXEXPY(4)
      CHARACTER*(*) STRX
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
      FCODE = -293
      IL1   = 6
      IL2   = 6
      RL1   = 7
      RL2   = 7
      ID(1) = WKID
C     INDIVIDUAL SETTINGS
      ID(2) = CTXFP(1)
      ID(3) = CTXFP(2)
      ID(4) = CTXP
      ID(5) = CTXAL(1)
      ID(6) = CTXAL(2)
C     BUNDLED SETTINGS
      ID(7) = CTXI
      ID(8) = CTXFPA
      ID(9) = CCHXPA
      ID(8) = CCHSPA
      RX(1) = CCHXP
      RX(2) = CCHSP
      RX(3) = CCHH
      RX(4) = CCHUP(1)
      RX(5) = CCHUP(2)
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      RX(6) = PXD
      RX(7) = PYD
C     PUT OUT CHARACTER STRING ALONG WITH OTHER INTERFACE VARIABLES
      CALL GZPUTS(STRX,RERR)
      IF (RERR.NE.0) THEN
      ERRIND = RERR
      GOTO 100
      ENDIF
C
C     RECIEVE DESIRED INFORMATION IN NDC SPACE, AND CONVERT TO WC
C
      CALL GZN2WX(1,RX(8),CPX)
      CALL GZN2WY(1,RX(9),CPY)
      CALL GZN2WX(1,RX(10),TXEXPX(1))
      CALL GZN2WX(1,RX(11),TXEXPX(2))
      CALL GZN2WX(1,RX(12),TXEXPX(3))
      CALL GZN2WX(1,RX(13),TXEXPX(4))
      CALL GZN2WY(1,RX(14),TXEXPY(1))
      CALL GZN2WY(1,RX(15),TXEXPY(2))
      CALL GZN2WY(1,RX(16),TXEXPY(3))
      CALL GZN2WY(1,RX(17),TXEXPY(4))
      RETURN
  100 CONTINUE
      CPX = 1.E20
      CPY = 1.E20
      TXEXPX(1) =  0.
      TXEXPX(2) =  0.
      TXEXPX(3) =  0.
      TXEXPX(4) =  0.
      TXEXPY(1) =  0.
      TXEXPY(2) =  0.
      TXEXPY(3) =  0.
      TXEXPY(4) =  0.
      RETURN
      END
