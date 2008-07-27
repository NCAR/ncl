C
C	$Id: gupdva.f,v 1.8 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GUPDVA
C
C  Update all attribute deferral variables.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01adc.h'
C
      INTEGER  IPRIM, I, IOFF, JOFF, J
C
C  Copy default attribute context to "SET" context.
C
      CALL G01D2S
C
C  Go through the attribute structures G01RQA and G01AST to
C  reset the attribute context variables.
C
C  POLYLINE attributes.
C
      IPRIM = 1
      AGPEND(IPRIM) = .FALSE.
      DO 100 I=IVPLIX,IVPLCI
        IOFF = ABS(IP2AEA(I))
C
C  Is the requested value different from the sent?
C  The following code reflects the fact that all attribute
C  variables for POLYLINE are stored contiguously.
C
        IF (IP2AEA(I) .GT. 0) THEN
          VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
        ELSE
          VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
        ENDIF
C
C  Update aggregate change parameter.
C
        AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
  100 CONTINUE
C
C  POLYMARKER attributes.
C
      IPRIM = 2
      AGPEND(IPRIM) = .FALSE.
      DO 140 I=IVPMIX,IVPMCI
        IOFF = ABS(IP2AEA(I))
C
C  IS THE REQUESTED VALUE DIFFERENT FROM THE SENT?
C  THE FOLLOWING CODE REFLECTS THE FACT THAT ALL ATTRIBUTE
C  VARIABLES FOR POLYMARKER ARE STORED CONTIGUOUSLY.
C
        IF (IP2AEA(I) .GT. 0) THEN
          VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
        ELSE
          VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
        ENDIF
C
C  UPDATE AGGREGATE CHANGE PARAMETER.
C
        AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
C
  140 CONTINUE
C
C     TEXT ATTRIBUTES
C
      IPRIM = 3
      AGPEND(IPRIM) = .FALSE.
      DO 150 I=IVTXIX,IVTXCI
C
         IOFF = ABS(IP2AEA(I))
         JOFF = IOFF + IL2AEA(I) - 1
C
C        IS THE REQUESTED VALUE DIFFERENT FROM THE SENT?
C
         IF (IP2AEA(I) .GT. 0) THEN
            VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
            DO 110 J=IOFF+1,JOFF
               VALCHG(I) = VALCHG(I) .OR. MSAEQV(J).NE.MRAEQV(J)
  110       CONTINUE
         ELSE
            VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
            DO 120 J=IOFF+1,JOFF
               VALCHG(I) = VALCHG(I) .OR. ASAEQV(J).NE.ARAEQV(J)
  120       CONTINUE
         ENDIF
C
C        UPDATE AGGREGATE CHANGE PARAMETER.
C
         AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
C
  150 CONTINUE
C
C     FILL AREA ATTRIBUTES
C
      IPRIM = 4
      AGPEND(IPRIM) = .FALSE.
      DO 160 I=IVFAIX,IVFACI
C
         IOFF = ABS(IP2AEA(I))
         JOFF = IOFF + IL2AEA(I) - 1
C
C        IS THE REQUESTED VALUE DIFFERENT FROM THE SENT?
C
         IF (IP2AEA(I) .GT. 0) THEN
            VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
            DO 170 J=IOFF+1,JOFF
               VALCHG(I) = VALCHG(I) .OR. MSAEQV(J).NE.MRAEQV(J)
  170       CONTINUE
         ELSE
            VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
            DO 180 J=IOFF+1,JOFF
               VALCHG(I) = VALCHG(I) .OR. ASAEQV(J).NE.ARAEQV(J)
  180       CONTINUE
         ENDIF
C
C        UPDATE AGGREGATE CHANGE PARAMETER.
C
         AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
  160 CONTINUE
C
C  Update aspect source flag context (ID contains the GKS Aspect
C  Source Flag values which are the complements of the CGM values
C  that MRASF contains).
C
      DO 190 I=1,13
         ID(I) = 1-MRASF(I)
  190 CONTINUE
      CALL GUPASF
C
C     DONE.
C
      RETURN
C
      END
