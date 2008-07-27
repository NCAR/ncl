C
C	$Id: gtx.f,v 1.9 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C  Transform the position coordinates to NDC space and set up
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
C  Set the function code and put out the character arrays across the
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
