C
C	$Id: gschh.f,v 1.1.1.1 1992-04-17 22:33:48 ncargd Exp $
C
      SUBROUTINE GSCHH (CHH)
      INTEGER ESCHH
      PARAMETER (ESCHH=31)
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
      REAL CHH
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHH,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the character height is valid.
C
      IF (CHH.LE.0.) THEN
        ERS = 1
        CALL GERHND(78,ESCHH,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current character height in the GKS state list.
C  (CHH remains in world coordinates here).
C
      CCHH = CHH
C
C  Transform CHH to NDC space before invoking the interface.
C
      ICNT = CNT+1
      CHHX = CCHH*(NTVP(ICNT,2)-NTVP(ICNT,1))
     +           /(NTWN(ICNT,2)-NTWN(ICNT,1))
      CHHY = CCHH*(NTVP(ICNT,4)-NTVP(ICNT,3))
     +           /(NTWN(ICNT,4)-NTWN(ICNT,3))
C
C  Invoke the workstation interface.  Two real vectors are
C  passed through the interface.  The first vector is
C  passed via (RX(1),RY(1)) and is a vector parallel to the
C  current character up vector with length equal to the
C  recently defined character height (the character height
C  having been transformed to NDC space).  The second vector
C  is passed via (RX(2),RY(2)) and is a vector parallel to
C  the character base vector scaled in accordance with the
C  appropriate aspect ratio.
C
      FCODE = 34
      CONT  = 0
      RL1   = 2
      RL2   = 2
      SCL = 1./SQRT(CCHUP(1)*CCHUP(1)+CCHUP(2)*CCHUP(2))
      RX(1) = CHHX*SCL*CCHUP(1)
      RY(1) = CHHY*SCL*CCHUP(2)
      RX(2) = CHHX*SCL*CCHUP(2)
      RY(2) = -CHHY*SCL*CCHUP(1)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHH,ERF)
        ERS = 0
      ENDIF
      RETURN
      END
