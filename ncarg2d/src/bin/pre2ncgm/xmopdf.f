C
C	$Id: xmopdf.f,v 1.3 2000-07-12 17:04:38 haley Exp $
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
