C
C	$Id: threbd.f,v 1.2 2000-07-12 16:26:46 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
      BLOCKDATA THREBD
      COMMON /TEMPRT/ RZERO
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
      DATA RZERO/0./
      DATA NLX,NBY,NRX,NTY/10,10,1010,1010/
      DATA TMAGU,TMINU,TMAGV,TMINV,TMAGW,TMINW/12.,8.,12.,8.,12.,8./
      DATA ITHRMJ,ITHRMN,ITHRTX/ 1,1,1/
      END
