C
C $Id: pcffme.f,v 1.1 1992-11-17 18:46:25 kennison Exp $
C
      SUBROUTINE PCFFME (CHGT)
C
C Extract the informational part of the fontcap.
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
C  Type flag.
C
      CALL GBYTES(IBFC,TYPFLG,0,16,0,1)
C
C  Other fields.
C
      CALL GBYTES(IBFC,FNINFO(2),336,16,0,NUMNTR-1)
C
C  Generate negative numbers if required.
C
      DO 80 I=2,NUMNTR
        CALL PCGNEG (FNINFO(I),ITMP)
        FNINFO(I) = ITMP
   80 CONTINUE
C
C  Get the byte pointers for the characters.
C
      NUMCHR = CHREND-CHRSTR+1
      CALL GBYTES(IBFC,CHRPNT,8*TABPNT,16,0,NUMCHR)
C
      RETURN
      END
