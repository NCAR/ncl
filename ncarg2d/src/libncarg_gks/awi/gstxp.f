C
C	$Id: gstxp.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSTXP (TXP)
C
C  SET TEXT PATH
C
      INTEGER ESTXP
      PARAMETER (ESTXP=33)
C
      include 'gkscom.h'
C
      INTEGER TXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESTXP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the path is valid.
C
      IF (TXP.LT.0 .OR. TXP.GT.3) THEN
        ERS = 1
        CALL GERHND(2000,ESTXP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current text path in the GKS state list.
C
      CTXP = TXP
C
C  Invoke the workstation interface.
C
      FCODE = 35
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = TXP
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESTXP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
