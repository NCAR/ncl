C
C $Id: gwiarq.h,v 1.5 2008-07-27 03:55:38 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C
C  The common blocks here are divided so that 
C  conversion to double precision can be simplified.
C  The filler variables are never used.
C
      COMMON  /GWIARQI/ MRPLIX  ,MRLTYP ,MRDM03 ,MRPLCI ,
     +                  MRPMIX  ,MRMTYP ,MRDM07 ,MRPMCI ,
     +                  MRTXIX  ,MRTXP  ,MRTXAL(2)      ,MRCHH  ,
     +                  MRCHOV(4)       ,MRTXFO ,MRTXPR ,MRDM16 ,
     +                  MRDM17  ,MRTXCI ,
     +                  MRFAIX  ,MRPASZ(4)      ,MRPARF(2)      ,
     +                  MRFAIS  ,MRFASI ,MRFACI ,MRASF(13)
      INTEGER           MRPLIX  ,MRLTYP ,MRPLCI ,MRPMIX ,MRMTYP ,
     +                  MRPMCI  ,MRTXIX ,MRTXP  ,MRTXAL ,MRTXFO ,
     +                  MRTXPR  ,MRTXCI ,MRCHH  ,MRCHOV ,MRFAIX ,
     +                  MRPASZ  ,MRPARF ,MRFAIS ,MRFASI ,MRFACI ,
     +                  MRASF   ,MRAEQV(45)
C
C  Integer fillers.
C
      INTEGER           MRDM03  ,MRDM07 ,MRDM16 ,MRDM17
C
      COMMON  /GWIARQR/ ARDM01  ,ARDM02 ,ARLWSC ,ARDM04 ,
     +                  ARDM05  ,ARDM06 ,ARMSZS ,ARDM08 ,
     +                  ARDM09  ,ARDM10 ,ARDM11(2)      ,ARDM12 ,
     +                  ARDM13(4)       ,ARDM14 ,ARDM15 ,ARCHXP ,
     +                  ARCHSP  ,ARDM18 ,ARDM19 ,ARDM20(4)      ,
     +                  ARDM21(2)       ,ARDM22 ,ARDM23 ,ARDM24 ,
     +                  ARDM25(13)
      REAL              ARLWSC  ,ARMSZS ,ARCHXP ,ARCHSP ,ARAEQV(45)
C
C  Floating point fillers.
C
      REAL              ARDM01  ,ARDM02 ,ARDM04 ,ARDM05 ,ARDM06 ,
     +                  ARDM08  ,ARDM09 ,ARDM10 ,ARDM11 ,ARDM12 ,
     +                  ARDM13  ,ARDM14 ,ARDM15 ,ARDM18 ,ARDM19 ,
     +                  ARDM20  ,ARDM21 ,ARDM22 ,ARDM23 ,ARDM24 ,
     +                  ARDM25
      EQUIVALENCE     (MRPLIX, MRAEQV)
      EQUIVALENCE     (ARDM01, ARAEQV)
