C
C	$Id: conbd.f,v 1.5 2008-04-04 21:03:00 kennison Exp $
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
      SUBROUTINE CONBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA CONBDX
      COMMON /CONRE1/ IOFFP      ,SPVAL
      COMMON /CONRE4/ ISIZEL     ,ISIZEM     ,ISIZEP     ,NREP       ,
     1                NCRT       ,ILAB       ,NULBLL     ,IOFFD      ,
     2                EXT        ,IOFFM      ,ISOLID     ,NLA        ,
     3                NLM        ,XLT        ,YBT        ,SIDE
      DATA IOFFP,SPVAL/0,0.0/
      DATA ISIZEM,ISIZEP,NLA,NLM,XLT,YBT,SIDE,ISOLID/
     1        2,     1,   16, 40,.05,.05,   .9, 1023/
      DATA ISIZEL,NREP,NCRT/
     1       1,    6,   4 /
      DATA EXT,IOFFD,NULBLL,IOFFM,ILAB/.25,0,3,0,1/
C
C REVISION HISTORY---
C
C JANUARY 1978     DELETED REFERENCES TO THE  *COSY  CARDS AND
C                  ADDED REVISION HISTORY
C FEBURARY 1979    MADE CODE CONFORM TO FORTRAN 66 STANDARDS
C JANUARY 1980     CHANGED LIBRARY NAME FROM NSSL TO
C                  PORTLIB FOR MOVE TO PORTLIB
C MAY 1980         CONRECQCK ON PORTLIB WAS DISCOVERED TO BE ESSENTIALLY
C                  THE WRONG VERSION.  EXTENSIVE MODIFICATIONS TO CLGEN,
C                  MAXMIN, CONREC TO MAKE IT PORTABLE (INVOLVED REMOVAL
C                  OF PWRT CALLS, ENCODE STATEMENTS, INAPPROPRIATE CON-
C                  STANTS, ETC.).
C JUNE 1984        UPDATED TO FORTRAN 77 AND TO GKS
C MARCH 1990       CORRECTED THE USE OF SET CALLS
C-----------------------------------------------------------------------
C
      END
