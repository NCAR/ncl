C
C	$Id: gxopdf.f,v 1.4 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GXOPDF
C
C  This routine sets the default operand sizes before any processing.
C
      include 'trpars.h'
C
C  SET THE DEFAULT OPERAND SIZES
C
      MOPLEN = MOPDLN
      MCICPR = MCIDPR
      MFLCPR = MFLDPR
      MIXCPR = MIXDPR
      MWHCPR = MWHDPR
      MENCPR = MENDPR
C
      RETURN
      END
