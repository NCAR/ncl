C
C $Id: g01ast.h,v 1.3 2000-08-22 03:45:41 haley Exp $
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

      COMMON  /G01AST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,ASMSZS ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,ASCHXP ,
     +                  ASCHSP  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,
     +                  MSASF(13)
        REAL            ASLWSC  ,ASMSZS ,ASCHXP ,ASCHSP ,ASAEQV(45)
        INTEGER         MSPLIX  ,MSLTYP ,MSPLCI ,MSPMIX ,MSMTYP ,
     +                  MSPMCI  ,MSTXIX ,MSTXP  ,MSTXAL ,MSTXFO ,
     +                  MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV ,MSFAIX ,
     +                  MSPASZ  ,MSPARF ,MSFAIS ,MSFASI ,MSFACI ,
     +                  MSASF   ,MSAEQV(45)
        EQUIVALENCE     (MSPLIX, MSAEQV, ASAEQV)
