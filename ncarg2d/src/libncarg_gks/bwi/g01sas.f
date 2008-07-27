C
C	$Id: g01sas.f,v 1.7 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01SAS (IPRIM, RERR)
C
C  Send aspect source flags.
C
      INTEGER  IPRIM, RERR
C
      include 'g01prm.h'
      include 'g01ins.h'
      include 'g01adc.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01opc.h'
C
      INTEGER  IX, MIX, NCHANG, ASPAIR(2), NBYTES
C
C  Count the number of changed (CGM) ASFs first.
C
      NCHANG = 0
      DO 10 IX=1,NCGASF
C
C  Get the index of the associated GKS ASF.
C
        MIX = MASMAP(IX)
        IF (MIX.GT.0)  THEN
          IF (ASFCHG(MIX))  NCHANG = NCHANG + 1
        END IF
   10 CONTINUE
      IF (NCHANG .EQ. 0) GO TO 30
C
C  Compute byte length of instruction, put out opcode/length.
C
      NBYTES = 1 + (NCHANG*2*MEFW-1)/8
      CALL GPUTNI (CLASFS, IDASFS, NBYTES, RERR)
      IF (RERR .NE. 0)  GO TO 777
C
C  Put out each changed ASF.
C
      DO 20 IX=1,NCGASF
        MIX = MASMAP(IX)
        IF (MIX.GT.0)  THEN
          IF (ASFCHG(MIX))  THEN
C
C  Form index/value pair for changed ASF, send it.
C
            ASPAIR(1)   = IX - 1
            ASPAIR(2)   = MRASF(MIX)
            CALL GPUTPR (ASPAIR, MEFW, 2, RERR)
            IF (RERR .NE. 0)  RETURN
          END IF
        END IF
 20   CONTINUE
C
C  Copy requested (GKS) ASF to sent, clear change flag.
C
      DO 40 IX=1,NCGASF
        MIX = MASMAP(IX)
        IF (MIX .GT. 0) THEN
          MSASF(MIX)  = MRASF(MIX)
          ASFCHG(MIX) = .FALSE.
        ENDIF
   40 CONTINUE
C
C  Clear aggregate ASF change indicator.
C
   30 CONTINUE
      ANYASF = .FALSE.
C
  777 CONTINUE
C
      RETURN
      END
