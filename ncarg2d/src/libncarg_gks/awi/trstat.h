C
C $Id: trstat.h,v 1.3 2000-07-12 16:40:08 haley Exp $
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
      COMMON /GKSCOM/ POLIDX, LINTYP, LINWTH, LINCOL, MARIDX,
     1                MARSIZ, MARCOL, TXTIDX, INTSTL, PATIDX, FILCOL, 
     2                MARTYP, HORIZ , VERT  , PATH  , CHIGHT, XU    , 
     3                YU    , XB    , YB    , TXTCOL, FINDEX, CEXPN , 
     4                CSPACE, FILIDX, TXTPRE, HATIDX, FILRPT, ASFSRF, 
     5                ASFSDF, GASFSF, IGSGCP
      INTEGER         ASFMAX
      PARAMETER      (ASFMAX=18)
      REAL            LINWTH, MARSIZ, CEXPN , CSPACE
      INTEGER         POLIDX, LINTYP, LINCOL, MARIDX, 
     1                MARCOL, TXTIDX, INTSTL, PATIDX, FILCOL, MARTYP, 
     2                HORIZ , VERT  , PATH  , XU    , YU    , XB    , 
     3                YB    , TXTCOL, FINDEX, CHIGHT, FILIDX,
     4                TXTPRE, HATIDX, FILRPT(2)     ,
     5                ASFSRF(ASFMAX), ASFSDF(ASFMAX), GASFSF(13),
     6                IGSGCP
     
