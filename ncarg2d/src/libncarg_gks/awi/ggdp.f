C
C	$Id: ggdp.f,v 1.5 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GGDP(N,PX,PY,PRIMID,LDR,DATREC)
C
C  GENERALIZED DRAWING PRIMITIVE
C
      INTEGER EGDP
      PARAMETER (EGDP=17)
C
      include 'gkscom.h'
C
      INTEGER N,PRIMID,LDR
      REAL PX(N),PY(N)
      CHARACTER*80 DATREC(LDR)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EGDP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.1)) THEN
        ERS = 1
        CALL GERHND(100,EGDP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space.
C  ID(1) contains the GDP identifier.
C  If ID(2)=0, then no data record will be shipped.  If
C  ID(2)=1, then the data record will be shipped via
C  the first workstation invocation subsequent to the
C  completion of sending the real arrays, i.e. the continuation
C  flag for sending over the real arrays will be set to zero
C  and the data record will be sent across the interface
C  completely independently as the next transmission.
C
      FCODE = 16
C
C  Set up flag in ID(2) to indicate if a data record is to follow
C  sending over the real arrays.
C
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = PRIMID
      IF (LDR.GT.0) THEN
        ID(2) = 1
      ELSE
        ID(2) = 0
      ENDIF
C
C  Send over the real arrays.
C
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,1,IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EGDP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Send over the data record if there is one (recall that the
C  string length of STR is divisible by 80).
C
      IF (ID(2) .EQ. 1) THEN
        IF (LDR. EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = DATREC(1)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EGDP,ERF)
            ERS = 0
            RETURN
          ENDIF
        ELSE
C
C     SEND OVER THE DATA RECORD 80 CHARACTERS AT A TIME
C
          CONT = 1
          STRL1 = 80*LDR
          STRL2 = 80
          LDRM1 = LDR-1
          DO 200 I=1,LDRM1
            IF (I .GT. 1) IL2 = 0
            STR(1:80) = DATREC(I)
            CALL GZTOWK
            IF (RERR.NE.0) THEN
              ERS = 1
              CALL GERHND(RERR,EGDP,ERF)
              ERS = 0
              RETURN
            ENDIF
  200     CONTINUE
          CONT = 0
          STR(1:80) = DATREC(LDR)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EGDP,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
