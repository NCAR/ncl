C
C	$Id: clipit.f,v 1.3 2000-07-12 17:04:33 haley Exp $
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
       SUBROUTINE CLIPIT(XIN,YIN,XOUT,YOUT,DRAW)
C
C  NO CLIPPING.
C
C  INPUT
C       XIN(2) - INTEGER ARRAY DEFINING THE X COORDINATE ENDPOINTS OF
C                THE LINE TO BE CLIPPED.
C       YIN(2) - INTEGER ARRAY DEFINING THE Y COORDINATE ENDPOINTS OF
C                THE LINE TO BE CLIPPED.
C  OUTPUT
C       XOUT(2) - INTEGER ARRAY DEFINING THE X COORDINATE ENDPOINTS OF
C                 THE CLIPPED LINE.
C       YOUT(2) - INTEGER ARRAY DEFINING THE Y COORDINATE ENDPOINTS OF
C                 THE CLIPPED LINE.
C       DRAW - LOGICAL FLAG INDICATING IF ANY PART OF THE LINE IS
C              VISIBLE.  TRUE-XOUT, YOUT VALID.  FALSE-NO LINE SEGMENT.
C
      INTEGER XIN(2), YIN(2), XOUT(2), YOUT(2)
      LOGICAL DRAW
C
      INTEGER X1, Y1, X2, Y2
      LOGICAL ACCEPT, REJECT, DONE
C
      COMMON /PGKSCOM/ CLPDAT, CLPFLG, POLIDX, LINTYP, LINWTH, LINCOL,
     1                LINRGB, MARIDX, MARSIZ, MARCOL, MARRGB, TXTIDX,
     2                INTSTL, PATIDX, FILCOL, FILRGB, MARTYP, HORIZ ,
     3                VERT  , PATH  , CHIGHT, XU    , YU    , XB    ,
     4                YB    , TXTCOL, FINDEX, CEXPN , CSPACE, CURIDX,
     5                CURIMP, CURINT, COLMOD, FILIDX, TXTRGB, PROPN ,
     6                FIRSTX, FIRSTY, LASTX , LASTY , TRATIO, CBV   ,
     7                CUV   , CHP   , CVP   , CCSPAC, CHHORZ, CDV   ,
     8                PMRSIZ, CLPX  , CLPY  , CLPP  , DEFREP, DEFLEN,
     9                VDCECR, TRANVN, TXTPRE, HATIDX, FILRPT, ASFSRF,
     A                ASFSDF, MAPMOD, VERSOK, PCBFST, CPGLEN, CLPNUL,
     B                MTDLEN
      INTEGER         CMPMAX, ASFMAX
      PARAMETER      (CMPMAX=256    , ASFMAX=18)
      LOGICAL         CLPFLG, PROPN , CHHORZ, DEFREP, CLPNUL, MAPMOD,
     1                VERSOK, PCBFST
      REAL            LINWTH, MARSIZ, CEXPN , CSPACE, TRATIO, CBV(2),
     1                CUV(2), CDV(2), CCSPAC
      INTEGER         CLPDAT(4)     , POLIDX, LINTYP, LINCOL, LINRGB(3),
     1                MARIDX, MARCOL, MARRGB(3)     , TXTIDX, INTSTL,
     2                PATIDX, FILCOL, FILRGB(3)     , MARTYP, HORIZ ,
     3                VERT  , PATH  , XU    , YU    , XB    , YB    ,
     4                TXTCOL, FINDEX, CURIMP(CMPMAX), CURINT(CMPMAX*3),
     5                CHIGHT, COLMOD, FILIDX, TXTRGB(3)     , CURIDX,
     6                FIRSTX, FIRSTY, LASTX , LASTY , CHP   , CVP   ,
     7                PMRSIZ, CLPX  , CLPY  , CLPP  , DEFLEN, TRANVN,
     8                TXTPRE, HATIDX, FILRPT(2)     , VDCECR(4)     ,
     9                ASFSRF(ASFMAX), ASFSDF(ASFMAX), CPGLEN, MTDLEN
      COMMON /GKSCHR/ MTDESC
      CHARACTER*80    MTDESC
C
C  INITIALIZE THE ALGORITHM
C
      ACCEPT = .TRUE.
      REJECT = .FALSE.
      DONE = .TRUE.
      X1 = XIN(1)
      Y1 = YIN(1)
      X2 = XIN(2)
      Y2 = YIN(2)
C
C   LOOP UNTIL A DECISION IS MADE ABOUT THE VISIBLE LINE SEGMENT
C
C
C  SET THE DRAW FLAG AND THE COORDINATES BASED ON THE ACCEPT FLAG
C
      DRAW = ACCEPT
      IF (ACCEPT) THEN
        XOUT(1) = X1
        XOUT(2) = X2
        YOUT(1) = Y1
        YOUT(2) = Y2
      END IF
C
      RETURN
      END
