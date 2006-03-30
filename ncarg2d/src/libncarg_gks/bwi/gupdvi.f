C
C	$Id: gupdvi.f,v 1.6 2006-03-30 00:45:04 fred Exp $
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
      SUBROUTINE GUPDVI (INVAL, IPOINT, IPRIM)
C
C  Update the attribute deferral variables for an integer-valued
C  attribute.
C
C  Parameters:
C    INPUT
C      INVAL  -- New value for the attribute item.
C      IPOINT -- Pointer into the deferral control variable
C                structures, and into the index array that
C                gives the start of the primitive in the attribute
C                equivalencing array, integer version.
C      IPRIM  -- Output primitive type associated with
C                the attribute (1=POLYLINE, 2=POLYMARKER,
C                3=TEXT, 4=FILL AREA).  Used to index into
C                other of the control variable structures.
C	
C      OUTPUT
C        Adjustment of attribute deferral control parameters
C        in common.
C
C
      include 'g01prm.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01adc.h'
C
      INTEGER INVAL(*), IPOINT, IPRIM
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
      CHGNS = INVAL(1).NE.MSAEQV(IOFF)
      DO 10 I=2,LNGVAL
        CHGNS = CHGNS.OR. INVAL(I).NE.MSAEQV(IOFF+I-1)
   10 CONTINUE
C
      IF (CHGNS) THEN
C
C  New and last sent differ.
C
C  Set aggregate change-pending parameter.
C
        AGPEND(IPRIM) = .TRUE.
      END IF
C
C  Copy new value to requested.
C
      MRAEQV(IOFF) = INVAL(1)
      DO 30 I=2,LNGVAL
        MRAEQV(IOFF+I-1) = INVAL(I)
   30 CONTINUE
C
C  Mark value change.
C
      VALCHG(IPOINT) = CHGNS
C
        RETURN
        END
