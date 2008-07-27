C
C $Id: gwiadf.h,v 1.4 2008-07-27 03:55:38 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      COMMON  /GWIADF/  MDPLIX  ,MDLTYP ,ADLWSC ,MDPLCI ,
     +                  MDPMIX  ,MDMTYP ,ADMSZS ,MDPMCI ,
     +                  MDTXIX  ,MDTXP  ,MDTXAL(2)      ,MDCHH  ,
     +                  MDCHOV(4)       ,MDTXFO ,MDTXPR ,ADCHXP ,
     +                  ADCHSP  ,MDTXCI ,
     +                  MDFAIX  ,MDPASZ(4)       ,MDPARF(2)     ,
     +                  MDFAIS  ,MDFASI  ,MDFACI  ,
     +                  MDASF(13)
        INTEGER         MDPLIX  ,MDLTYP ,MDPLCI
        REAL            ADLWSC
        INTEGER         MDPMIX  ,MDMTYP ,MDPMCI
        REAL            ADMSZS
        INTEGER         MDTXIX  ,MDTXP  ,MDTXAL ,MDTXFO
        INTEGER         MDTXPR  ,MDTXCI ,MDCHH  ,MDCHOV
        REAL            ADCHXP  ,ADCHSP
        INTEGER         MDFAIX  ,MDPASZ ,MDPARF ,MDFAIS ,MDFASI
        INTEGER         MDFACI  ,MDASF
