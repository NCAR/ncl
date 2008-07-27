C
C	$Id: gxmdef.f,v 1.4 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
