C
C $Id: gwiast.h,v 1.3 2000-08-22 03:45:42 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
      COMMON  /GWIAST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,ASMSZS ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,ASCHXP ,
     +                  ASCHSP  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,
     +                  MSASF(13)
        INTEGER         MSPLIX  ,MSLTYP ,MSPLCI
        REAL            ASLWSC
        INTEGER         MSPMIX  ,MSMTYP ,MSPMCI
        REAL            ASMSZS
        INTEGER         MSTXIX  ,MSTXP  ,MSTXAL ,MSTXFO
        INTEGER         MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV
        REAL            ASCHXP  ,ASCHSP
        INTEGER         MSFAIX  ,MSPASZ ,MSPARF ,MSFAIS ,MSFASI
        INTEGER         MSFACI  ,MSASF
        INTEGER         MSAEQV(45)
        REAL            ASAEQV(45)
        EQUIVALENCE     (MSPLIX, MSAEQV, ASAEQV)
