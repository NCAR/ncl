C
C	$Id: pcffdx.h,v 1.1 1992-11-19 01:34:33 fred Exp $
C
C
      PARAMETER (IFCLEN=3000, ICLEN=150)
      COMMON /PCINDX/IBFC(IFCLEN)    ,SFLGS(ICLEN)   ,CHRPNT(128),
     +               IXC(ICLEN)      ,IYC(ICLEN)     ,XC(ICLEN)  ,
     +               YC(ICLEN)       ,OUTLIN         ,SCALE
      INTEGER        IBFC            ,SFLGS          ,CHRPNT     , 
     +               IXC             ,IYC            ,OUTLIN
      REAL           XC              ,YC             ,SCALE
      SAVE   /PCINDX/
