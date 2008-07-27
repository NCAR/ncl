C
C	$Id: gsmksc.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSMKSC (MSZSF)
C
C  SET MARKER SIZE SCALE FACTOR
C
      INTEGER ESMKSC
      PARAMETER (ESMKSC=24)
C
      include 'gkscom.h'
C
      REAL MSZSF
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESMKSC,IER)
      IF (IER .NE. 0) RETURN
C
C  Set the current marker scale factor in the GKS state list.
C
      CMKS = MSZSF
C
C  Invoke the workstation interface.
C
      FCODE = 27
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = MSZSF
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESMKSC,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
