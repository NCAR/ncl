C
C	$Id: gwpdvi.f,v 1.5 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWPDVI (INVAL, IPOINT, IPRIM)
C
C  Update attribute deferral variables for an integer-valued attribute.
C
C
C  PARAMETERS:
C    INPUT
C      INVAL   - New value for the attribute item.
C      IPOINT  - Pointer into deferral control variable structures, 
C                and into index array that gives start of primitive 
C                in attribute equivalencing array, integer version.
C      IPRIM   - Output primitive type associated with the attribute 
C                (1=POLYLINE, 2=POLYMARKER, 3=TEXT, 4=FILL AREA).  
C                Used to index into other of the control variable 
C                structures.
C
C    OUTPUT
C      Adjustment of attribute deferral control parameters in comMON.
C
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadc.h'
C
      INTEGER INVAL(*), IPOINT, IPRIM
C
      LOGICAL CHGNS
      INTEGER I, IOFF, LNGVAL
C
      SAVE
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
