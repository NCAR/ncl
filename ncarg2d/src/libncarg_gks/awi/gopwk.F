C
C	$Id: gopwk.F,v 1.1.1.1 1992-04-17 22:33:34 ncargd Exp $
C
      SUBROUTINE GOPWK(WKID,CONID,WTYPE)
      INTEGER EOPWK
      PARAMETER (EOPWK=2)
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
      INTEGER WKID,CONID,WTYPE
      LOGICAL IOPEN
      INTEGER LASF(13)
C     CHECK THAT GKS IS IN THE PROPER STATE
      CALL GZCKST(8,EOPWK,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,EOPWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK THAT THE CONNECTION IDENTIFIER IS VALID
C
      IF (CONID.EQ.5.OR.CONID.EQ.6) THEN
      ERS = 1
      CALL GERHND(21,EOPWK,ERF)
      ERS = 0
      RETURN
      ENDIF
C     CHECK THAT THE WORKSTATION TYPE IS VALID
      CALL GZCKWK(22,EOPWK,IDUM,WTYPE,IER)
      IF (IER .NE. 0) RETURN
C     CHECK IF THE WORKSTATION IS CURRENTLY OPEN
      CALL GZCKWK(24,EOPWK,WKID,WTYPE,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK IF THERE IS ROOM FOR ANOTHER OPEN WORKSTATION
C
      IF (NOPWK.GE.MOPWK) THEN
      ERS = 1
      CALL GERHND(26,EOPWK,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     CHECK IF THE SPECIFIED CONNECTION IDENTIFIER IS CURRENTLY
C     OPEN
C
#if defined(u370) && defined(unix)
C     INQUIRE (CONID,OPENED=IOPEN)
C     IF (IOPEN) THEN
C     ERS = 1
C     CALL GERHND(26,EOPWK,ERF)
C     ERS = 0
C     RETURN
C     ENDIF
#else
      INQUIRE (CONID,OPENED=IOPEN)
      IF (IOPEN) THEN
      ERS = 1
      CALL GERHND(26,EOPWK,ERF)
      ERS = 0
      RETURN
      ENDIF
#endif
C
C     MAKE SURE THAT THE METAFILE WORKSTATION IS OPEN IF TRYING
C     TO OPEN WISS (THIS IMPLEMENTATION REQUIRES THAT THE
C     METAFILE BE OPENED BEFORE WISS)
C
      IF (WTYPE.EQ.3) THEN
      WCONID = CONID
      IF (SOPWK(1).EQ.-1.OR.SWKTP(1).NE.1) THEN
      ERS = 1
      CALL GERHND(-103,EOPWK,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
C
C     SET OPERATING STATE TO WSOP IF IN STATE GKOP
C
      IF (OPS.EQ.GGKOP) THEN
      OPS = GWSOP
      ENDIF
C
C     ADD THE WORKSTATION IDENTIFIER TO THE SET OF OPEN WORKSTATIONS
C
      NOPWK = NOPWK+1
      SOPWK(NOPWK) = WKID
      SWKTP(NOPWK) = WTYPE
C
C     PASS INFORMATION ACROSS THE WORKSTATION INTERFACE
C
      IF (WTYPE.EQ.1) THEN
      FCODE = -3
      CONT  = 0
      IL1   = 3
      IL2   = 3
      ID(1) = WKID
      ID(2) = CONID
      ID(3) = WTYPE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,EOPWK,ERF)
      ERS = 0
      ENDIF
      ENDIF
C
C  Establish the current attribute settings for non-WISS
C  workstations.
C
      IF (WTYPE .NE. 3) THEN
        CALL GSCLIP (CCLIP)
        CALL GSPLI  (CPLI )
        CALL GSLN   (CLN  )
        CALL GSLWSC (CLWSC)
        CALL GSPLCI (CPLCI)
        CALL GSPMI  (CPMI )
        CALL GSMK   (CMK  )
        CALL GSMKSC (CMKS )
        CALL GSPMCI (CPMCI)
        CALL GSTXI  (CTXI )
        CALL GSTXFP (CTXFP(1),CTXFP(2))
        CALL GSCHXP (CCHXP)
        CALL GSCHSP (CCHSP)
        CALL GSTXCI (CTXCI)
        CALL GSCHH  (CCHH )
        CALL GSCHUP (CCHUP(1),CCHUP(2))
        CALL GSTXP  (CTXP )
        CALL GSTXAL (CTXAL(1),CTXAL(2))
        CALL GSFAI  (CFAI )
        CALL GSFAIS (CFAIS)
        CALL GSFASI (CFASI)
        CALL GSFACI (CFACI)
        CALL GSPA   (CPA  (1),CPA  (2))
        CALL GSPARF (CPARF(1),CPARF(2))
        LASF( 1) = CLNA
        LASF( 2) = CLWSCA
        LASF( 3) = CPLCIA
        LASF( 4) = CMKA
        LASF( 5) = CMKSA
        LASF( 6) = CPMCIA
        LASF( 7) = CTXFPA
        LASF( 8) = CCHXPA
        LASF( 9) = CCHSPA
        LASF(10) = CTXCIA
        LASF(11) = CFAISA
        LASF(12) = CFASIA
        LASF(13) = CFACIA
        CALL GSASF (LASF)
      ENDIF
C
      RETURN
      END
