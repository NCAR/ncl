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
     
