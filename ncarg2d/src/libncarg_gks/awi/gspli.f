C
C	$Id: gspli.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSPLI(INDEX)
C
C  SET POLYLINE INDEX
C
      INTEGER ESPLI
      PARAMETER (ESPLI=18)
C
      include 'gkscom.h'
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESPLI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid (this will have to be changed
C  for implementations higher than 0A).
C
      IF (INDEX.LE.0 .OR. INDEX.GT.5) THEN
        ERS = 1
        CALL GERHND(60,ESPLI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current index in the GKS state list.
C
      CPLI = INDEX
C
C  Invoke the workstation interface.
C
      FCODE = 21
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = INDEX
      CALL GZTOWK
      IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
        ERS = 1
        CALL GERHND(RERR,ESPLI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
