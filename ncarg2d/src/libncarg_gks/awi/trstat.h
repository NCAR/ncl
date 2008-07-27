C
C $Id: trstat.h,v 1.5 2008-07-27 03:55:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
     
