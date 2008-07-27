C
C	$Id: gslwsc.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSLWSC (LWIDTH)
C
C  SET LINEWIDTH SCALE FACTOR
C
      INTEGER ESLWSC
      PARAMETER (ESLWSC=20)
C
      include 'gkscom.h'
C
      REAL LWIDTH
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESLWSC,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the scale factor is valid.
C
      IF (LWIDTH .LT. 0.) THEN
        ERS = 1
        CALL GERHND(65,ESLWSC,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current linewidth scale factor.
C
      CLWSC = LWIDTH
C
C  Invoke the workstation interface.
C
      FCODE = 23
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = LWIDTH
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESLWSC,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
