C
C	$Id: gca.f,v 1.1.1.1 1992-04-17 22:33:34 ncargd Exp $
C
      SUBROUTINE GCA(PX,PY,QX,QY,DIMX,DIMY,NCS,NRS,DX,DY,COLIA)
      INTEGER ECA
      PARAMETER (ECA=16)
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
      INTEGER DIMX,DIMY,DX,DY,COLIA(DIMX,*)
      REAL PX,PY,QX,QY
C     CHECK IF GKS IS IN PROPER STATE
      CALL GZCKST(5,ECA,IER)
      IF (IER .NE. 0) RETURN
C
C     CHECK THAT THE INDICES OF THE COLOR ARRAY ARE VALID
C
      IF (DIMX.LE.0.OR.DIMY.LE.0.OR.NRS+DY-1.GT.DIMY.OR.NCS+DX-1.GT.DIMX
     +) THEN
      ERS = 1
      CALL GERHND(91,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
C
      PXN = PX
      PYN = PY
      QXN = QX
      QYN = QY
C
C     ADJUST CORNER POINTS AS PER NCS AND NRS (THIS CODE TO BE USED
C     IF YOU INTERPRET P AND Q TO BE IDENTIFIED WITH THE EXTREME
C     POINTS OF THE COLOR INDEX ARRAY.
C
C     DELX = ABS(PX-QX)/DIMX
C     DELY = ABS(PY-QY)/DIMY
C     IF (PX.LT.QX) THEN
C     IF (PY.LT.QY) THEN
C     PXN = PX+(NCS-1)*DELX
C     PYN = PY+(NRS-1)*DELY
C     QXN = PXN+DX*DELX
C     QYN = PYN+DY*DELY
C     ELSE IF (QY.LT.PY) THEN
C     PXN = PX+(NCS-1)*DELX
C     PYN = PY-(NRS-1)*DELY
C     QXN = PXN+DX*DELX
C     QYN = PYN-DY*DELY
C     ELSE
C     RETURN
C     ENDIF
C     ELSE IF (QX.LT.PX) THEN
C     IF (PY.LT.QY) THEN
C     PXN = PX-(NCS-1)*DELX
C     PYN = PY+(NRS-1)*DELY
C     QXN = PXN-DX*DELX
C     QYN = PYN+DY*DELY
C     ELSE IF (QY.LT.PY) THEN
C     PXN = PX-(NCS-1)*DELX
C     PYN = PY-(NRS-1)*DELY
C     QXN = PXN-DX*DELX
C     QYN = PYN-DY*DELY
C     ELSE
C     RETURN
C     ENDIF
C     ELSE
C     RETURN
C     ENDIF
C
C     FIND A THIRD CORNER
C
      OXN = PXN
      OYN = QYN
C
C     CONVERT CORNERS TO NDC SPACE.
C
      CALL GZW2NX(1,OXN,OXP)
      CALL GZW2NX(1,PXN,PXP)
      CALL GZW2NX(1,QXN,QXP)
      CALL GZW2NY(1,OYN,OYP)
      CALL GZW2NY(1,PYN,PYP)
      CALL GZW2NY(1,QYN,QYP)
C
C     THIS SUBROUTINE PASSES DATA ACROSS THE INTERFACE WITH A
C     MINIMUM OF TWO INVOCATIONS OF THE WORKSTATION INTERFACE.
C     IN THE FIRST INVOCATION OF THE INTERFACE, RX(1) IS SET
C     TO PXP (IN NDC SPACE), RY(1) IS SET TO PYP (IN NDC SPACE),
C     RX(2) IS SET TO QXP (IN NDC SPACE), RY(2) IS SET TO QYP (IN
C     NDC SPACE), RX(3) IS SET TO OXP (IN NDC SPACE) AND RY(3)
C     IS SET TO OYP (IN NDC SPACE); ID(1) IS SET TO DX,
C     AND ID(2) IS SET TO DY.
C     IN SUBSEQUENT INVOCATIONS OF THE INTERFACE, THE RELEVANT PART OF
C     THE COLOR INDEX ARRAY (OF LENGTH DX*DY) IS SENT VIA
C     COMMON ARRAY ID.
C
      FCODE = 15
      CONT  =  1
      IL1   =  2
      IL2   =  2
      ID(1) =  DX
      ID(2) =  DY
      RL1   =   3
      RL2   =   3
      RX(1) = PXP
      RY(1) = PYP
      RX(2) = QXP
      RY(2) = QYP
      RX(3) = OXP
      RY(3) = OYP
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
C
C     INTERFACE CALLS FOR COLOR INDEX ARRAY
C
      N = DX*DY
      J = (N-1)/128
      IL1 = N
      IF (J.EQ.0) THEN
C
C     CASE WHERE THERE IS NO CONTINUATION
C
      CONT = 0
      IL2 = N
      INDX = 0
      DO   200 K=1,DY
      DO   201 I=1,DX
      INDX  = INDX+1
      ID(INDX) = COLIA(NCS+I-1,NRS+K-1)
  201 CONTINUE
  200 CONTINUE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
      ELSE
C
C     CASE WITH CONTINATION
C
      CONT = 1
      IL2 = 128
      INDX = 0
      DO   202 M=1,DY
      DO   203 L=1,DX
      INDX = INDX+1
      JM = MOD(INDX,128)
      IF (JM.EQ.0.AND.INDX.LT.N) THEN
      ID(128) = COLIA(NCS+L-1,NRS+M-1)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
      ELSE IF (JM.NE.0.AND.INDX.LT.N) THEN
      ID(JM) = COLIA(NCS+L-1,NRS+M-1)
      ELSE
C
C     FINAL CASE WHERE INDX=N
C
      CONT = 0
      IF (JM.EQ.0) THEN
      ID(128) = COLIA(NCS+L-1,NRS+M-1)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
      ELSE
      ID(JM) = COLIA(NCS+L-1,NRS+M-1)
      IL2 = MOD(N,128)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
      ERS = 1
      CALL GERHND(RERR,ECA,ERF)
      ERS = 0
      RETURN
      ENDIF
      ENDIF
      ENDIF
  203 CONTINUE
  202 CONTINUE
      ENDIF
      RETURN
      END
