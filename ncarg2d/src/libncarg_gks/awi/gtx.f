C
C	$Id: gtx.f,v 1.5 2000-07-12 16:40:01 haley Exp $
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
      SUBROUTINE GTX(PX,PY,CHARS)
C
C  TEXT
C
      INTEGER ETX
      PARAMETER (ETX=14)
C
      include 'gkscom.h'
C
      REAL PX,PY
      CHARACTER*(*) CHARS
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,ETX,IER)
      IF (IER .NE. 0) RETURN
C
C  Transform position coordinates to NDC space and set up
C  the real arrays in the workstation interface common block.
C
      CALL GZROI(0)
C
      RL1 = 1
      RL2 = 1
      CALL GZW2NX(1,PX,PXN)
      RX(1) = PXN
      CALL GZW2NY(1,PY,PYN)
      RY(1) = PYN
C
C  Set function code and put out the character arrays across the
C  workstation interface.  The continuation flag signals
C  continuation of the character array, the position coordinates
C  are picked up on the first invocation of the workstation
C  interface.
C
      FCODE = 13
      CALL GZPUTS(CHARS,IER)
      RERR = IER
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ETX,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
