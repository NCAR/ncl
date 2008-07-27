C
C $Id: g01wsl.h,v 1.5 2008-07-27 03:55:38 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C

      COMMON  /G01WSL/  MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN  ,
     +                  MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS   ,
     +                  RWINDO(4)       ,CWINDO(4)      ,
     +                  RWKVP (4)       ,CWKVP (4)      ,
     +                  MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI(256),
     +                  SRED(256)       ,SGREEN(256)    ,SBLUE(256),
     +                  MRCREC(4)       ,MRCLIP ,NPFLG
        INTEGER         MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN
        INTEGER         MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS
        INTEGER         MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI
        REAL            RWINDO          ,CWINDO
        REAL            RWKVP           ,CWKVP
        REAL            SRED            ,SGREEN         ,SBLUE
        INTEGER         MRCREC  ,MRCLIP ,NPFLG
