C
C	$Id: gxmdef.f,v 1.3 2000-08-22 15:08:25 haley Exp $
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
      SUBROUTINE GXMDEF
C
C  Set all the current values to default.
C
      include 'trdefl.h'
      include 'trstat.h'
C
      INTEGER  II, TLNCOL, TTXCOL, TFLCOL
C
C  Set the default polyline information.
C
      POLIDX = POLIDF
      LINTYP = LINTDF
      LINWTH = LINWDF
      TLNCOL = TLNCDF
C
C  Set the default polymarker information.
C
      MARIDX = MARIDF
      MARTYP = MARTDF
      MARSIZ = MARSDF
C
C  Set the default TEXT information.
C
      HORIZ  = HORIDF
      VERT   = VERTDF
      PATH   = PATHDF
      CHIGHT = CHIGDF
      XU = XUDF
      YU = YUDF
      XB = XBDF
      YB = YBDF
      TXTIDX = TXTIDF
      TTXCOL = TTXCDF
      TXTPRE = TXTPDF
      CEXPN  = CEXPDF
      CSPACE = CSPADF
C
C  Set the default POLYGON information.
C
      FILIDX = FILIDF
      INTSTL = INTSDF
      HATIDX = HATIDF
      PATIDX = PATIDF
      FILRPT(1) = FILRDF(1)
      FILRPT(2) = FILRPT(2)
      TFLCOL = TFLCDF
      FILCOL = TFLCOL
      TXTCOL = TTXCOL
      LINCOL = TLNCOL
C
C  Set the aspect source flags.
C
      DO 160 II=1,ASFMAX
        ASFSRF(II) = ASFSDF(II)
 160  CONTINUE
C
      RETURN
      END
