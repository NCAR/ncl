C
C	$Id: gupdvr.f,v 1.6 2006-03-30 00:45:04 fred Exp $
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
        SUBROUTINE GUPDVR (RNVAL, IPOINT, IPRIM)
C
C  Update attribute deferral variables for a real-valued attribute.
C
C
C  Parameters:
C
C    INPUT
C      RNVAL   - New value for the attribute item.
C      IPOINT  - Pointer into deferral control variable structures, 
C                and into the index array that gives the start of the
C                primitive in the attribute equivalencing array, 
C                real version.
C      IPRIM   - Output primitive type associated with the attribute 
C                (1=POLYLINE, 2=POLYMARKER, 3=TEXT, 4=FILL AREA).  
C                used to index into other of the control variable 
C                structures.
C
C      OUTPUT
C        Adjustment of attribute deferral control parameters in COMMON.        
C
      include 'g01prm.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01adc.h'
C
      REAL    RNVAL(*)
      INTEGER IPOINT, IPRIM
C
      LOGICAL CHGNS
      INTEGER I, IOFF, LNGVAL
C
C  Offset and length of attribute in equivalencing structures.
C
      IOFF   = ABS(IP2AEA(IPOINT))
      LNGVAL = IL2AEA(IPOINT)
C
C  Is the new value different from the last sent?
C
      CHGNS = RNVAL(1).NE.ASAEQV(IOFF)
      DO 10 I=2,LNGVAL
        CHGNS = CHGNS.OR. RNVAL(I).NE.ASAEQV(IOFF+I-1)
   10 CONTINUE
C
      IF (CHGNS) THEN
C
C  New and last sent differ -- set aggregate change-pending parameter.
C
        AGPEND(IPRIM) = .TRUE.
      END IF
C
C  Copy new value to requested.
C
      ARAEQV(IOFF) = RNVAL(1)
      DO 30 I=2,LNGVAL
        ARAEQV(IOFF+I-1) = RNVAL(I)
   30 CONTINUE
C
C  Mark value change.
C
      VALCHG(IPOINT) = CHGNS
C
      RETURN
      END
