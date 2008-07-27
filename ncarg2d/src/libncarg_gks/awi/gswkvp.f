C
C	$Id: gswkvp.f,v 1.5 2008-07-27 00:21:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSWKVP (WKID,XMIN,XMAX,YMIN,YMAX)
C
C  SET WORKSTATION VIEWPORT
C
      INTEGER ESWKVP
      PARAMETER (ESWKVP=55)
C
      include 'gkscom.h'
C
      INTEGER WKID
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESWKVP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation identifier is valid.
C
      CALL GZCKWK(20,ESWKVP,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ESWKVP,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the rectangle definition is valid.
C
      IF (XMAX.LE.XMIN .OR. YMAX.LE.YMIN) THEN
        ERS = 1
        CALL GERHND(51,ESWKVP,ERF)
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
      FCODE = 72
      CONT  = 0
      CALL GZROI(0)
      IL1  = 1
      IL2  = 1
      ID(1) = WKID
      RL1 = 2
      RL2 = 2
      RX(1) = XMIN
      RX(2) = XMAX
      RY(1) = YMIN
      RY(2) = YMAX
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESWKVP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
