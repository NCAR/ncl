C
C $Id: g01arq.h,v 1.2 2000-07-12 16:50:41 haley Exp $
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      COMMON  /G01ARQ/  MRPLIX  ,MRLTYP ,ARLWSC ,MRPLCI ,
     +                  MRPMIX  ,MRMTYP ,ARMSZS ,MRPMCI ,
     +                  MRTXIX  ,MRTXP  ,MRTXAL(2)      ,MRCHH  ,
     +                  MRCHOV(4)       ,MRTXFO ,MRTXPR ,ARCHXP ,
     +                  ARCHSP  ,MRTXCI ,
     +                  MRFAIX  ,MRPASZ(4)      ,MRPARF(2)      ,
     +                  MRFAIS  ,MRFASI ,MRFACI ,
     +                  MRASF(13)
        INTEGER         MRPLIX  ,MRLTYP ,MRPLCI
        REAL            ARLWSC
        INTEGER         MRPMIX  ,MRMTYP ,MRPMCI
        REAL            ARMSZS
        INTEGER         MRTXIX  ,MRTXP  ,MRTXAL ,MRTXFO
        INTEGER         MRTXPR  ,MRTXCI ,MRCHH  ,MRCHOV
        REAL            ARCHXP  ,ARCHSP
        INTEGER         MRFAIX  ,MRPASZ ,MRPARF ,MRFAIS ,MRFASI
        INTEGER         MRFACI  ,MRASF
        INTEGER         MRAEQV(45)
        REAL            ARAEQV(45)
        EQUIVALENCE     (MRPLIX, MRAEQV, ARAEQV)
