C
C	$Id: g01sas.f,v 1.6 2003-02-13 23:58:22 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
