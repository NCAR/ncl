C
C $Id: gwiwsl.h,v 1.4 2008-07-27 03:55:38 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      COMMON  /GWIWSL/  MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN  ,
     +                  MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS   ,
     +                  RWINDO(4)       ,CWINDO(4)      ,
     +                  RWKVP (4)       ,CWKVP (4)      ,
     +                  MRCREC(4)       ,MRCLIP
        INTEGER         MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN
        INTEGER         MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS
        REAL            RWINDO          ,CWINDO
        REAL            RWKVP           ,CWKVP
        INTEGER         MRCREC  ,MRCLIP
