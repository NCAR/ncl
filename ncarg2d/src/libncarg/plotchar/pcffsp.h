C
C	$Id: pcffsp.h,v 1.1 1992-11-19 01:34:49 fred Exp $
C
C
      PARAMETER (IBZL = 129)
      COMMON /PCBZSP/BCNTLX(4), BCNTLY(4), BZXC(IBZL), BZYC(IBZL)
      REAL           BCNTLX   , BCNTLY   , BZXC      , BZYC
      SAVE   /PCBZSP/
