C
C	$Id: gqpxa.f,v 1.1.1.1 1992-04-17 22:33:46 ncargd Exp $
C
      SUBROUTINE GQPXA(WKID,PX,PY,DIMX,DIMY,NCS,NRS,DX,DY,
     +                 ERRIND,INVVAL,COLIA)
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
      INTEGER WKID,DX,DY,DIMX,ERRIND,INVVAL,COLIA(DIMX,DY)
      REAL    PX,PY
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C     CHECK THAT THE WORKSTATION IDENTIFIER IS VALID
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C     CHECK IF THE SPECIFIED WORKSTATION IS OPEN
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C     CHECK IF ARRAY IS LARGE ENOUGH
      IF (DX.GT.DIMX.OR.DY.GT.DIMY) THEN
      ERRIND = 2001
      GOTO 100
      ENDIF
C     INVOKE INTERFACE
      FCODE = -291
      CONT  = 0
      IL1   = 7
      IL2   = 7
      RL1   = 1
      RL2   = 1
      ID(1) = WKID
      ID(2) = DIMX
      ID(3) = DIMY
      ID(4) = NCS
      ID(5) = NRS
      ID(6) = DX
      ID(7) = DY
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      RX(1) = PXD
      RY(1) = PYD
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERRIND = RERR
      GOTO 100
      ENDIF
      INVVAL = ID(8)
C
C     BRING OVER THE COLOR INDEX ARRAY
C
      INDX = (DX*DY-1)/128
      IF (INDX.EQ.0) THEN
      CALL GZFMWK
      INDX = 0
      DO   200 J=1,DY
      DO   201 I=1,DX
      INDX = INDX+1
      COLIA(I,J) = ID(INDX)
  201 CONTINUE
  200 CONTINUE
      ELSE
      CALL GZFMWK
      INDX = 0
      DO   202 J=1,DY
      DO   203 I=1,DX
      INDX = INDX+1
      COLIA(I,J) = ID(INDX)
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
      INVVAL = -1
      COLIA(1,1) = -1
      RETURN
      END
