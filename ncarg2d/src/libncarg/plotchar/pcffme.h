C
C	$Id: pcffme.h,v 1.1 1992-11-19 01:34:45 fred Exp $
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
