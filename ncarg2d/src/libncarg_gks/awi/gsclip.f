C
C	$Id: gsclip.f,v 1.3 2000-07-12 16:39:56 haley Exp $
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
      SUBROUTINE GSCLIP(CLSW)
C
C  Set clipping indicator.
C
      INTEGER ESCLIP
      PARAMETER (ESCLIP=53)
C
      include 'gkscom.h'
C
      INTEGER CLSW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCLIP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that CLSW is in range.
C
      IF (CLSW.LT.0.OR.CLSW.GT.1) THEN
        ERS = 1
        CALL GERHND(2000,ESCLIP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set clipping indicator in the GKS state list.
C
      CCLIP = CLSW
C
C  Invoke the workstation interface.  Send the clipping indicator
C  in ID(1) and the viewport of the current normalization transformation
C  in RX, RY.
C
      FCODE = 61
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = CLSW
      RL1   = 2
      RL2   = 2
      ICNT = CNT+1
      RX(1) = NTVP(ICNT,1)
      RX(2) = NTVP(ICNT,2)
      RY(1) = NTVP(ICNT,3)
      RY(2) = NTVP(ICNT,4)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCLIP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
