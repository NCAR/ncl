C
C	$Id: gstxi.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSTXI (INDEX)
C
C  SET TEXT INDEX
C
      INTEGER ESTXI
      PARAMETER (ESTXI=26)
C
      include 'gkscom.h'
C
      INTEGER INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESTXI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid (0A specific).
C
      IF (INDEX.LE.0 .OR. INDEX.GT.2) THEN
        ERS = 1
        CALL GERHND(72,ESTXI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current text index in the GKS state list.
C
      CTXI = INDEX
C
C  Invoke the workstation interface.
C
      FCODE = 29
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = INDEX
      CALL GZTOWK
      IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
        ERS = 1
        CALL GERHND(RERR,ESTXI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
