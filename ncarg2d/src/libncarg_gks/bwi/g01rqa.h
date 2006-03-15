C
C $Id: g01rqa.h,v 1.2 2006-03-15 01:56:17 fred Exp $
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
      COMMON  /G01RQAI/ MRPLIX  ,MRLTYP ,MRDM03 ,MRPLCI ,
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
      COMMON  /G01RQAR/ ARDM01  ,ARDM02 ,ARLWSC ,ARDM04 ,
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
