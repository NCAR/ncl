C
C $Id: pcffgp.f,v 1.1 1992-11-17 18:46:21 kennison Exp $
C
      SUBROUTINE PCFFGP(NUM,IOFF,PFLG,X,Y)
C
C  Get NUM packets from the fontcap starting at bit offset IOFF.
C  Store the packet(s) in the arrays PFLG, X, and Y.
C
C
      PARAMETER (NUMNTR=29)
      COMMON /PCMTRC/TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER        TYPFLG, CHRSTR, CHREND, FRIGHT, FTOP  , FCAPOV,
     +               FCAP  , FXHOV , FXH   , FHALF , FBASE , FBOT  ,
     +               FCHSWD, FCVSWD, FLLX  , FLLY  , FURX  , FURY  ,
     +               FLLEX , FLLEY , FUREX , FUREY , TABPNT, XBITWD, 
     +               YBITWD, XBIAS , YBIAS , PKFLWD, LSTPNT
      INTEGER FNINFO(NUMNTR)
      EQUIVALENCE (FNINFO,TYPFLG)
      SAVE   /PCMTRC/
C
      PARAMETER (IFCLEN=3000, ICLEN=150)
      COMMON /PCINDX/IBFC(IFCLEN)    ,SFLGS(ICLEN)   ,CHRPNT(128),
     +               IXC(ICLEN)      ,IYC(ICLEN)     ,XC(ICLEN)  ,
     +               YC(ICLEN)       ,OUTLIN         ,SCALE
      INTEGER        IBFC            ,SFLGS          ,CHRPNT     , 
     +               IXC             ,IYC            ,OUTLIN
      REAL           XC              ,YC             ,SCALE
      SAVE   /PCINDX/
C
      INTEGER PFLG(*),X(*),Y(*)
C
      CALL GBYTES(IBFC,PFLG,IOFF,PKFLWD,XBITWD+YBITWD,NUM)
      CALL GBYTES(IBFC,X,IOFF+PKFLWD,XBITWD,YBITWD+PKFLWD,NUM)
      CALL GBYTES(IBFC,Y,IOFF+PKFLWD+XBITWD,YBITWD,PKFLWD+XBITWD,NUM)
C
      RETURN
      END
