C
C	$Id: gzputr.f,v 1.5 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZPUTR(NPTOT,N,P,Q,ICONV,IER)
C
C  This subroutine fills the real arrays of the workstation
C  interface common block and invokes the workstation
C  interface subroutine.  If ICONV=1, P,Q are assumed to be
C  in world coordinates, and are transformed to NDC
C  coordinates.  If an error is returned from the workstation
C  interface, IER is set to that error and a return is executed.
C  NPTOT is the total number of points to be put out in a sequence
C  of calls to this subroutine; N is the number of points to be
C  put out with each individual call.
C
      include 'gkscom.h'
C
      SAVE
      INTEGER N
      REAL P(N),Q(N),RP(128),RQ(128)
      DATA NSBTOT/0/
C
      RL1 = NPTOT
      IFRST = 0
      NSBTOT = NSBTOT+N
      J = (N-1)/128
      IF (J .EQ. 0) THEN
C
C  Case where there is no continuation.
C
        CONT = 0
        NSBTOT = 0
        RL2 = N
        IF (ICONV .EQ. 1) THEN
          CALL GZW2NX(N,P,RP)
          CALL GZW2NY(N,Q,RQ)
          DO 200 I=1,N
            RX(I) = RP(I)
            RY(I) = RQ(I)
  200     CONTINUE
        ELSE
          DO 201 I=1,N
            RX(I) = P(I)
            RY(I) = Q(I)
  201     CONTINUE
        ENDIF
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          IER = RERR
          RETURN
        ENDIF
      ELSE
C
C  Case with contination.
C
        CONT = 1
        RL2 = 128
C
C  Loop through parts with continuation flag set.
C
        IF (ICONV .EQ. 1) THEN
          DO 202 K=1,J
            KPNT = (K-1)*128+1
            CALL GZW2NX(128,P(KPNT),RX)
            CALL GZW2NY(128,Q(KPNT),RY)
            IF (IFRST .GT. 0) THEN
              IL2 = 0
              STRL2 = 0
              IC2 = 0
            ENDIF
            CALL GZTOWK
            IFRST = IFRST+1
            IF (RERR .NE. 0) THEN
              IER = RERR
              RETURN
            ENDIF
  202     CONTINUE
        ELSE
          DO 203 K=1,J
            KPNT = (K-1)*128
            DO 204 I=1,128
              RX(I) = P(KPNT+I)
              RY(I) = Q(KPNT+I)
  204       CONTINUE
            IF (IFRST .GT. 0) THEN
              IL2 = 0
              STRL2 = 0
              IC2 = 0
            ENDIF
            CALL GZTOWK
            IFRST = IFRST+1
            IF (RERR .NE. 0) THEN
              IER = RERR
              RETURN
            ENDIF
  203     CONTINUE
        ENDIF
C
C  Put out last part of array.
C
        IF (NSBTOT .LT. NPTOT) THEN
          CONT = 1
        ELSE
          CONT = 0
          NSBTOT = 0
        ENDIF
        RL2 = N-J*128
        KPNT = J*128
        IF (ICONV .EQ. 1) THEN
          CALL GZW2NX(RL2,P(KPNT+1),RX)
          CALL GZW2NY(RL2,Q(KPNT+1),RY)
          IF (IFRST .GT. 0) THEN
            IL2 = 0
            STRL2 = 0
            IC2 = 0
          ENDIF
          CALL GZTOWK
          IFRST = IFRST+1
          IF (RERR .NE. 0) THEN
            IER = RERR
            RETURN
          ENDIF
        ELSE
          DO 205 I=1,RL2
            RX(I) = P(KPNT+I)
            RY(I) = Q(KPNT+I)
  205     CONTINUE
          IF (IFRST .GT. 0) THEN
            IL2 = 0
            STRL2 = 0
            IC2 = 0
          ENDIF
          CALL GZTOWK
          IFRST = IFRST+1
          IF (RERR .NE. 0) THEN
            IER = RERR
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
