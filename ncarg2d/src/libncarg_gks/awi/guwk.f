C
C	$Id: guwk.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GUWK(WKID,REGFL)
C
C  UPDATE WORKSTATION
C
      INTEGER EUWK
      PARAMETER (EUWK=8)
C
      include 'gkscom.h'
C
      INTEGER WKID,REGFL
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EUWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,EUWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,EUWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if REGFL is in bounds.
C
      IF (REGFL.LT.0 .OR. REGFL.GT.1) THEN
        ERS = 1
        CALL GERHND(2000,EUWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Invoke the workstation interface.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 3
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = REGFL
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,EUWK,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
