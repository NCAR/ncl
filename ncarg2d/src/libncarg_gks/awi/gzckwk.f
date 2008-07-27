C
C	$Id: gzckwk.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZCKWK(NUM,ENAM,WKID,WTYPE,IER)
C
C  This subroutine provides the checking for error numbers:
C     20 -- SPECIFIED WORKSTATION IDENTIFIER IS INVALID
C     22 -- SPECIFIED WORKSTATION TYPE IS INVALID
C     23 -- SPECIFIED WORKSTATION TYPE DOES NOT EXIST
C     24 -- SPECIFIED WORKSTATION IS OPEN
C     25 -- SPECIFIED WORKSTATION IS NOT OPEN
C     29 -- SPECIFIED WORKSTATION IS ACTIVE
C     30 -- SPECIFIED WORKSTATION IS NOT ACTIVE
C
C  INPUT:
C    NUM   -- Error number to check for.
C    ENAM  -- Index of name of calling program (this is non-zero
C             only for non-inquiry functions, in which case GERHND
C             is called).
C    WKID  -- Workstation identifier (where applicable).
C    WTYPE -- Workstation type (where applicable).
C
C  OUTPUT:
C    IER   -- 0 If no error and the error number if an error was found.        
C
      include 'gkscom.h'
C
      INTEGER ENAM,WKID,WTYPE
      IER = 0
      IF (NUM .EQ. 20) THEN
C
C  Check if WKID is valid.
C
        IF (WKID .LT. 0) THEN
          IER = NUM
          IF (ENAM .GE. 0) THEN
            ERS = 1
            CALL GERHND(NUM,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ELSE IF (NUM .EQ. 22) THEN
C
C  Check if WTYPE is valid.
C
        INDX = 0
  200   CONTINUE
        INDX = INDX+1
        IF (INDX .GT. WK) THEN
          IER = NUM
          IF (ENAM.GE.0) THEN
            ERS = 1
            CALL GERHND(NUM,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
          GO TO   201
        ELSE
          IF (LSWK(INDX) .EQ. WTYPE) THEN
            GO TO   201
          ENDIF
        ENDIF
        GO TO   200
  201   CONTINUE
      ELSE IF (NUM .EQ. 23) THEN
C
C  Check if WTYPE exists.
C
        INDX = 0
  202   CONTINUE
        INDX = INDX+1
        IF (INDX .GT. WK) THEN
          IER = NUM
          IF (ENAM .GE. 0) THEN
            ERS = 1
            CALL GERHND(NUM,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
          GO TO   203
        ELSE
          IF (LSWK(INDX) .EQ. WTYPE) THEN
            GO TO   203
          ENDIF
        ENDIF
        GO TO 202
  203   CONTINUE
      ELSE IF (NUM .EQ. 24) THEN
C
C  Check if the workstation is currently open.
C
        IF (NOPWK .GT. 0) THEN
          DO 204 I=1,NOPWK
            IF (SOPWK(I) .EQ. WKID) THEN
              IER = NUM
              IF (ENAM .GE. 0) THEN
                ERS = 1
                CALL GERHND(NUM,ENAM,ERF)
                ERS = 0
                RETURN
              ENDIF
            ENDIF
  204     CONTINUE
        ENDIF
      ELSE IF (NUM.EQ.25) THEN
C
C  Check if the specified workstation is open.
C
        INDX = 0
  205   CONTINUE
        INDX = INDX+1
        IF (INDX .GT. MOPWK) THEN
          IER = NUM
          IF (ENAM .GE. 0) THEN
            ERS = 1
            CALL GERHND(NUM,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
          GO TO 206
        ELSE
          IF (SOPWK(INDX) .EQ. WKID) THEN
            GO TO 206
          ENDIF
        ENDIF
        GO TO 205
  206   CONTINUE
      ELSE IF (NUM.EQ.29) THEN
C
C  Check if the workstation is currently active.
C
        DO 207 I=1,MACWK
          IF (SACWK(I) .EQ. WKID) THEN
            IF (ENAM .GE. 0) THEN
              ERS = 1
              CALL GERHND(NUM,ENAM,ERF)
              ERS = 0
              RETURN
            ENDIF
          ENDIF
  207   CONTINUE
      ELSE IF (NUM .EQ. 30) THEN
C
C  Check if the workstation is not active.
C
        INDX = 1
  208   CONTINUE
        IF (SACWK(INDX) .EQ. WKID) GO TO 209
        INDX = INDX+1
        IF (INDX .GT. MACWK) THEN
          IER = NUM
          IF (ENAM .GE. 0) THEN
            ERS = 1
            CALL GERHND(NUM,ENAM,ERF)
            ERS = 0
            RETURN
          ENDIF
          GO TO   209
        ENDIF
        GO TO   208
  209   CONTINUE
      ENDIF
C
      RETURN
      END
