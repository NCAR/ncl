C
C	$Id: gschsp.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSCHSP (CHSP)
C
C  SET CHARACTER SPACING
C
      INTEGER ESCHSP
      PARAMETER (ESCHSP=29)
C
      include 'gkscom.h'
C
      REAL CHSP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHSP,IER)
      IF (IER .NE. 0) RETURN
C
C  Set the current character spacing in the GKS state list.
C
      CCHSP = CHSP
C
C  Invoke the workstation interface.
C
      FCODE = 32
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = CHSP
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHSP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
