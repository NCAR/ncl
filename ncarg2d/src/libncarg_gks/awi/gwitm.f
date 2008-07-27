C
C	$Id: gwitm.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWITM (WKID,TYPE,LDR,DATREC)
C
C  WRITE ITEM TO GKSM
C
      INTEGER EWITM
      PARAMETER (EWITM=101)
C
      include 'gkscom.h'
C
      INTEGER WKID,TYPE,LDR
      CHARACTER*80 DATREC(LDR)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EWITM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation ID is valid.
C
      CALL GZCKWK(20,EWITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if item type is valid (GKS interpretable items are illegal).
C
      IF (TYPE .LE. 100) THEN
        ERS = 1
        CALL GERHND(160,EWITM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if item length is valid.
C
      IF (LDR .LT. 1) THEN
        ERS = 1
        CALL GERHND(161,EWITM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the workstation is currently active.
C
      CALL GZCKWK(30,EWITM,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Set function code and put out WKID, TYPE, LDR, and the data
C  record in STR.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 101
      CALL GZROI(0)
      IL1 = 3
      IL2 = 3
      ID(1) = WKID
      ID(2) = TYPE
      ID(3) = LDR
C
C  Send over the data record if there is one (recall that the
C  string length of STR is divisible by 80).
C
      IF (LDR .GE. 1) THEN
        IF (LDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = DATREC(1)
          CALL GZTOWK
          CUFLAG = -1
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EWITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ELSE
C
C  Send over the data record 80 characters at a time.
C
          CONT = 1
          STRL1 = 80*LDR
          STRL2 = 80
          LDRM1 = LDR-1
          DO 200 I=1,LDRM1
            IF (I .GT. 1) IL2 = 0
            STR(1:80) = DATREC(I)
            CALL GZTOWK
            CUFLAG = -1
            IF (RERR .NE. 0) THEN
              ERS = 1
              CALL GERHND(RERR,EWITM,ERF)
              ERS = 0
              RETURN
            ENDIF
  200     CONTINUE
          CONT = 0
          STR(1:80) = DATREC(LDR)
          CALL GZTOWK
          CUFLAG = -1
          IF (RERR .NE. 0) THEN
            ERS = 1
            CALL GERHND(RERR,EWITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ELSE
        CALL GZTOWK
        CUFLAG = -1
        IF (RERR .NE. 0) THEN
          ERS = 1
          CALL GERHND(RERR,EWITM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
C
      RETURN
      END
