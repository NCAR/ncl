C
C	$Id: gxopdf.f,v 1.1 1993-01-09 02:04:01 fred Exp $
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
