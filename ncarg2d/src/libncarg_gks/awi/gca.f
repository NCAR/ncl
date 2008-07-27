C
C	$Id: gca.f,v 1.7 2008-07-27 00:20:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCA(PX,PY,QX,QY,DIMX,DIMY,NCS,NRS,DX,DY,COLIA)
C
C  CELL ARRAY
C
      INTEGER ECA
      PARAMETER (ECA=16)
C
      include 'gkscom.h'
C
      INTEGER DIMX,DIMY,DX,DY,COLIA(DIMX,*)
      REAL PX,PY,QX,QY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,ECA,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the indices of the color array are valid.
C
      IF (DIMX.LE.0 .OR. DIMY.LE.0 .OR. NRS+DY-1.GT.DIMY .OR. 
     +    NCS+DX-1  .GT. DIMX) THEN
        ERS = 1
        CALL GERHND(91,ECA,ERF)
        ERS = 0
        RETURN
      ENDIF
C
      PXN = PX
      PYN = PY
      QXN = QX
      QYN = QY
C
C  Find a third corner.
C
      OXN = QXN
      OYN = PYN
C
C  Convert the corners to NDC space.
C
      CALL GZW2NX(1,OXN,OXP)
      CALL GZW2NX(1,PXN,PXP)
      CALL GZW2NX(1,QXN,QXP)
      CALL GZW2NY(1,OYN,OYP)
      CALL GZW2NY(1,PYN,PYP)
      CALL GZW2NY(1,QYN,QYP)
C
C  Make the interface calls.
C
      FCODE = 15
      CALL GZROI(0)
      IL1   =  2
      IL2   =  2
      ID(1) =  DX
      ID(2) =  DY
      RL1   =   3
      RL2   =   3
      RX(1) = PXP
      RY(1) = PYP
      RX(2) = QXP
      RY(2) = QYP
      RX(3) = OXP
      RY(3) = OYP
      STRL1 = 0
      STRL2 = 0
C
      N = DX*DY
      J = (N-1)/128
      IC1 = N
      IF (J .EQ. 0) THEN
C
C  Case where there is no continuation.
C
        CONT = 0
        IC2  = N
        INDX = 0
        DO 200 K=1,DY
          DO 201 I=1,DX
            INDX  = INDX+1
            IC(INDX) = COLIA(NCS+I-1,NRS+K-1)
  201     CONTINUE
  200   CONTINUE
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,ECA,ERF)
          ERS = 0
          RETURN
        ENDIF
      ELSE
C
C  Case with continuation.
C
        CONT = 1
        IC1 = N
        IC2 = 128
        INDX = 0
        IFRST = 0
        DO 202 M=1,DY
          DO 203 L=1,DX
            INDX = INDX+1
            JM = MOD(INDX,128)
            IF (JM.EQ.0 .AND. INDX.LT.N) THEN
              IC(128) = COLIA(NCS+L-1,NRS+M-1)
              IF (IFRST .GT .0) THEN
                IL2 = 0
                RL2 = 0
              ENDIF
              CALL GZTOWK
              IFRST = IFRST+1
              CALL GZROI(1)
              IF (RERR.NE.0) THEN
                ERS = 1
                CALL GERHND(RERR,ECA,ERF)
                ERS = 0
                RETURN
              ENDIF
            ELSE IF (JM.NE.0  .AND.  INDX.LT.N) THEN
              IC(JM) = COLIA(NCS+L-1,NRS+M-1)
            ELSE
C
C  Final case where INDX=N.
C
              CONT = 0
              IF (JM .EQ. 0) THEN
                IC(128) = COLIA(NCS+L-1,NRS+M-1)
                IF (IFRST .GT .0) THEN
                  IL2 = 0
                  RL2 = 0
                ENDIF
                CALL GZTOWK
                IFRST = IFRST+1
                CALL GZROI(1)
                IF (RERR .NE. 0) THEN
                  ERS = 1
                  CALL GERHND(RERR,ECA,ERF)
                  ERS = 0
                  RETURN
                ENDIF
              ELSE
                IC(JM) = COLIA(NCS+L-1,NRS+M-1)
                IC2 = MOD(N,128)
                IF (IFRST .GT .0) THEN
                  IL2 = 0
                  RL2 = 0
                ENDIF
                CALL GZTOWK
                IFRST = IFRST+1
                CALL GZROI(1)
                IF (RERR .NE. 0) THEN
                  ERS = 1
                  CALL GERHND(RERR,ECA,ERF)
                  ERS = 0
                  RETURN
                ENDIF
              ENDIF
            ENDIF
  203     CONTINUE
  202   CONTINUE
      ENDIF
C
      RETURN
      END
