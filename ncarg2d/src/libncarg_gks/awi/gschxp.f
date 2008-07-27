C
C	$Id: gschxp.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSCHXP (CHXP)
C
C  SET CHARACTER EXPANSION FACTOR
C
      INTEGER ESCHXP
      PARAMETER (ESCHXP=28)
C
      include 'gkscom.h'
C
      REAL CHXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHXP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the expansion factor is valid.
C
      IF (CHXP.LE.0.) THEN
        ERS = 1
        CALL GERHND(77,ESCHXP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current expansion factor in the GKS state list.
C
      CCHXP = CHXP
C
C  Invoke the workstation interface.
C
      FCODE = 31
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = CHXP
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHXP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
