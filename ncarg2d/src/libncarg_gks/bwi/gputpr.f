C
C	$Id: gputpr.f,v 1.4 2000-08-22 15:09:42 haley Exp $
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
      SUBROUTINE GPUTPR (BUFFER,BITS,COUNT,GKSERR)
C
C  Put the operand string into the metafile buffer.
C
C  INPUT
C    BUFFER -- List of operands to move.
C    BITS   -- Precision of the operands.
C    COUNT  -- Number of operands in the buffer.
C
C  OUTPUT
C    GKSERR -- Error status.
C
C  All data is type integer unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION BUFFER(*)
C
      include 'g01prm.h'
      include 'g01ins.h'
C
      DATA ALLOK /0/
C
      CTEMP = COUNT
      STRT = 1
C
   10 CONTINUE
C
C  Determine the number of operand words left in the current partition.
C
      WCBYT = (MCCBYT*8)/BITS
C
C  Compute and move the allowed number of operands.
C
      MOVIT = MIN0(WCBYT,CTEMP)
      CALL GMFLOD(BUFFER(STRT),BITS,MOVIT,GKSERR)
      IF (GKSERR .NE. ALLOK) RETURN
C
C  Check if another partition has to be started.
C
      CTEMP = CTEMP - MOVIT
      MCCBYT = MCCBYT - (MOVIT*BITS)/8
      IF (CTEMP .NE. 0) THEN
C
C  New partition required, so set up the element.
C
        STRT = STRT + MOVIT
C
C  Take the remainder of bytes left in current partition 
C  (they must be used).
C
        TCBYT = MCNBYT + MCCBYT
        CALL GMPART(TCBYT,GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
C
C  Move more operands into new partition.
C
        GO TO 10
      END IF
C
      RETURN
      END
