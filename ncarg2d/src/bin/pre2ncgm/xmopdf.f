C
C	$Id: xmopdf.f,v 1.5 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE XMOPDF
C
C  THIS ROUTINE SETS THE DEFAULT OPERAND SIZES BEFORE ANY PROCESSING
C  OF THE METAFILE
C
C
      COMMON /PTRPARS/  MOPLEN, MCICPR, MCDCPR, MFLCPR, MIXCPR, MWHCPR,
     1                 MOPDLN, MCIDPR, MCDDPR, MFLDPR, MIXDPR, MWHDPR,
     2                 BYTSIZ, CONFLG, MENDPR, MENCPR
      INTEGER MOPCLL, MOPIDL, MSCLEN, MLGFLG, MCOLEN, MLOLEN
      PARAMETER (MOPCLL=4, MOPIDL=7, MCOLEN=1, MSCLEN=5, MLOLEN=15,
     1           MLGFLG=31)
       INTEGER         MOPLEN, MCICPR, MCDCPR, MFLCPR, MIXCPR, MWHCPR,
     1                 MOPDLN, MCIDPR, MCDDPR, MFLDPR, MIXDPR, MWHDPR,
     2                 BYTSIZ, CONFLG, MENDPR, MENCPR
C
C  SET THE DEFAULT OPERAND SIZES
C
      MOPLEN = MOPDLN
      MCICPR = MCIDPR
      MCDCPR = MCDDPR
      MFLCPR = MFLDPR
      MIXCPR = MIXDPR
      MWHCPR = MWHDPR
      MENCPR = MENDPR
C
      RETURN
      END
