C
C $Id: gwiast.h,v 1.5 2008-07-27 03:55:38 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C
C  The common blocks here are divided so that 
C  conversion to double precision can be simplified.
C  The filler variables are never used.
C
      COMMON  /GWIASTI/ MSPLIX  ,MSLTYP ,MSDM03 ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,MSDM07 ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,MSDM16 ,
     +                  MSDM17  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,MSASF(13)
      INTEGER           MSPLIX  ,MSLTYP ,MSPLCI ,MSPMIX ,MSMTYP ,
     +                  MSPMCI  ,MSTXIX ,MSTXP  ,MSTXAL ,MSTXFO ,
     +                  MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV ,MSFAIX ,
     +                  MSPASZ  ,MSPARF ,MSFAIS ,MSFASI ,MSFACI ,
     +                  MSASF   ,MSAEQV(45)
C
C  Integer fillers.
C
      INTEGER           MSDM03  ,MSDM07 ,MSDM16 ,MSDM17
C
      COMMON  /GWIASTR/ ASDM01  ,ASDM02 ,ASLWSC ,ASDM04 ,
     +                  ASDM05  ,ASDM06 ,ASMSZS ,ASDM08 ,
     +                  ASDM09  ,ASDM10 ,ASDM11(2)      ,ASDM12 ,
     +                  ASDM13(4)       ,ASDM14 ,ASDM15 ,ASCHXP ,
     +                  ASCHSP  ,ASDM18 ,ASDM19 ,ASDM20(4)      ,
     +                  ASDM21(2)       ,ASDM22 ,ASDM23 ,ASDM24 ,
     +                  ASDM25(13)
      REAL              ASLWSC  ,ASMSZS ,ASCHXP ,ASCHSP ,ASAEQV(45)
C
C  Floating point fillers.
C
      REAL              ASDM01  ,ASDM02 ,ASDM04 ,ASDM05 ,ASDM06 ,
     +                  ASDM08  ,ASDM09 ,ASDM10 ,ASDM11 ,ASDM12 ,
     +                  ASDM13  ,ASDM14 ,ASDM15 ,ASDM18 ,ASDM19 ,
     +                  ASDM20  ,ASDM21 ,ASDM22 ,ASDM23 ,ASDM24 ,
     +                  ASDM25
      EQUIVALENCE     (MSPLIX, MSAEQV)
      EQUIVALENCE     (ASDM01, ASAEQV)
