C
C	$Id: gspmi.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSPMI (INDEX)
C
C  SET POLYMARKER INDEX
C
      INTEGER ESPMI
      PARAMETER (ESPMI=22)
C
      include 'gkscom.h'
C
      INTEGER INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESPMI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid (level 0A specific).
C
      IF (INDEX.LT.1 .OR. INDEX.GT.5) THEN
        ERS = 1
        CALL GERHND(66,ESPMI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current polymarker index in the GKS state list.
C
      CPMI = INDEX
C
C  Invoke the workstation interface.
C
      FCODE = 25
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = INDEX
      CALL GZTOWK
      IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
        ERS = 1
        CALL GERHND(RERR,ESPMI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
