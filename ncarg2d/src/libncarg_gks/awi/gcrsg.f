C
C	$Id: gcrsg.f,v 1.1.1.1 1992-04-17 22:33:35 ncargd Exp $
C
      SUBROUTINE GCRSG(SGNA)
      INTEGER ECRSG
      PARAMETER (ECRSG=56)
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
      INTEGER SGNA
C
C     THIS SUBROUTINE IS HERE SOLELY AS SUPPORT FOR THE SPPS GFLASn
C     ENTRIES.  FULL SEGMENTATION IS NOT A PART OF THE NCAR GKS
C     PACKAGE AT THIS TIME.  THE NCAR PACKAGE IS NON-STANDARD TO THE
C     EXTENT THAT CERTAIN SEGMENTATION FUNCTIONS ARE SUPPORTED, BUT
C     NOT ALL LEVEL 1 FUNCTIONS ARE SUPPORTED.  THIS SUBROUTINE SHOULD
C     BE CONSIDERED A USER ENTRY POINT ONLY BY WAY OF THE GFLASn
C     CALLS--IT SHOULD NEVER BE CALLED DIRECTLY BY THE USER.
C
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(3,ECRSG,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK THAT THE SEGMENT NAME IS VALID.
C
      IF (SGNA.LT.0.OR.SGNA.GT.99) THEN
      ERS = 1
      CALL GERHND(120,ECRSG,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     CHECK IF THE SEGMENT NAME IS ALREADY IN USE.
C
      DO   200 I=1,NUMSEG
      IF (SEGS(I).EQ.SGNA) THEN
      ERS = 1
      CALL GERHND(121,ECRSG,ERF)
      ERS = 0
      RETURN
      ENDIF
  200 CONTINUE
C
C     SET GKS STATE TO SEGMENT OPEN
C
      OPS = GSGOP
C
C     ADD SEGMENT NAME TO THOSE IN USE.
C
      NUMSEG = NUMSEG+1
      SEGS(NUMSEG) = SGNA
      CURSEG = SGNA
C
C     INVOKE WORKSTATION INTERFACE
C
      FCODE = 80
      CONT  = 0
      IL1 = 2
      IL2 = 2
      ID(1) = WCONID
      ID(2) = SGNA
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECRSG,ERF)
      ERS = 0
      ENDIF
      RETURN
      END
