C
C $Id: gwiast.h,v 1.4 2006-03-15 01:56:18 fred Exp $
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
