C
C	$Id: gceldr.f,v 1.6 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCELDR(IOS,STATUS)
C
C  Put out the cell array.  
C
      include 'trpars.h'
      include 'gkscom.h'
C
      INTEGER IOS, STATUS
      INTEGER P(2),Q(2),R(2),PDIM(2)
      INTEGER COLTMP, CELCOP, CELRMD
C
      STATUS = 0
C
C  Get the boundaries of the cell array.  These boundaries 
C  define the lower left point and the upper right point.
C
      CALL GCELDC(P,MOPLEN,2,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
      CALL GCELDC(Q,MOPLEN,2,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
      CALL GCELDC(R,MOPLEN,2,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Get the dimension of the virtual pixel array.
C
      CALL GCELDC(PDIM,MWHCPR,2,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Get the cell array color precision.
C
      CALL GCELDC(CELCOP,MWHCPR,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Compute the precision size.
C
      IF (CELCOP .EQ. 0) THEN
C
C  Use the color precision in effect.
C
        COLTMP = MCICPR
      ELSE
C
C  Use the color precision given by CELCOP.
C
        COLTMP = CELCOP
      END IF
C
C  Get the cell representation mode.
C
      CALL GCELDC(CELRMD,MENCPR,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Set up interface data.
C
      CALL GZROI(0)
      FCODE = 15
      IL1 = 2
      IL2 = 2
      NCOLS = PDIM(1)
      ID(1) = NCOLS
      NROWS = PDIM(2)
      ID(2) = NROWS
      IC1 = ID(1)*ID(2)
      RL1 = 3
      RL2 = 3
      RX(1) = REAL(P(1))/32767.
      RY(1) = REAL(P(2))/32767.
      RX(2) = REAL(Q(1))/32767.
      RY(2) = REAL(Q(2))/32767.
      RX(3) = REAL(R(1))/32767.
      RY(3) = REAL(R(2))/32767.
C
C  Transform the corner points using the current transform matrix.
C
      RX(1) = CURTM(1,1)*RX(1) + CURTM(1,2)*RY(1) + CURTM(1,3)
      RY(1) = CURTM(2,1)*RX(1) + CURTM(2,2)*RY(1) + CURTM(2,3)
      RX(2) = CURTM(1,1)*RX(2) + CURTM(1,2)*RY(2) + CURTM(1,3)
      RY(2) = CURTM(2,1)*RX(2) + CURTM(2,2)*RY(2) + CURTM(2,3)
      RX(3) = CURTM(1,1)*RX(3) + CURTM(1,2)*RY(3) + CURTM(1,3)
      RY(3) = CURTM(2,1)*RX(3) + CURTM(2,2)*RY(3) + CURTM(2,3)
C
      STRL1 = 0
      STRL2 = 0
C
C  Put out color indices.
C
      J = (IC1-1)/128
      IF (J .EQ. 0) THEN
C
C  Case where there is no continuation.
C
        DO 160 I=1,NROWS
          INDX = 1+NCOLS*(I-1)
          CALL GCELDC(IC(INDX),COLTMP,NCOLS,IOS,STATUS)
          IF (MOD(NCOLS,2).NE.0) CALL GCELDC(IDUM,COLTMP,1,IOS,STATUS)
  160   CONTINUE
        CONT = 0
        IC2  = IC1
        CALL GZTOWK
      ELSE
C
C  Case with continuation.
C
        CONT = 1
        IF (NCOLS .LE. 128) THEN
          IC2 = NCOLS
          DO 170 I=1,NROWS
            CALL GCELDC(IC(1),COLTMP,NCOLS,IOS,STATUS)
            IF (I .GT. 1) THEN
              IL2 = 0
              RL2 = 0
            ENDIF
            IF (I .EQ. NROWS) CONT = 0
            CALL GZTOWK
            IF (MOD(NCOLS,2).NE.0) CALL GCELDC(IDUM,COLTMP,1,IOS,STATUS)       
  170     CONTINUE
        ELSE
          JP = NCOLS/128
          JR = MOD(NCOLS,128)
          DO 180 I=1,NROWS
            DO 190 K=1,JP
              CALL GCELDC(IC(1),COLTMP,128,IOS,STATUS)
              IF (I.GT.1 .OR. K.GT.1) THEN
                IL2 = 0
                RL2 = 0
              ENDIF
              IF (K.EQ.JP .AND. I.EQ.NROWS .AND. JR.EQ.0) CONT = 0
              IC2 = 128
              CALL GZTOWK
  190       CONTINUE
            IF (JR .NE. 0) THEN
              CALL GCELDC(IC(1),COLTMP,JR,IOS,STATUS)
              IC2 = JR
              IF (I .EQ. NROWS) CONT = 0
              IL2 = 0
              RL2 = 0
              CALL GZTOWK
            ENDIF
            IF (MOD(NCOLS,2).NE.0) CALL GCELDC(IDUM,COLTMP,1,IOS,STATUS)       
  180     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
