C
C	$Id: gwpdva.f,v 1.3 2000-08-22 15:10:01 haley Exp $
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
      SUBROUTINE GWPDVA
C
C  Update all attribute deferral variables.
C
      include 'gksin.h'
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadc.h'
C
      INTEGER  IPRIM, I, IOFF, JOFF, J
C
      SAVE
C
C  Copy default attribute context to "SET" context.
C
      CALL GWID2S
C
C  Go through the attribute structures GWIARQ and GWIAST to
C  reset the attribute context variables.
C
C  Polyline attributes.
C
      IPRIM = 1
      AGPEND(IPRIM) = .FALSE.
      DO 100 I=IVPLIX,IVPLCI
        IOFF = IABS(IP2AEA(I))
C
C  Is the requested value different from the sent?
C  The following code reflects the fact that all attribute
C  variables for polyline are stored contiguously.
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
C  Polymarker attributes.
C
      IPRIM = 2
      AGPEND(IPRIM) = .FALSE.
      DO 140 I=IVPMIX,IVPMCI
C
        IOFF = IABS(IP2AEA(I))
C
C  Is the requested value different from the sent?
C  The following code reflects the fact that all attribute
C  variables for polymarker are stored contiguously.
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
C
  140 CONTINUE
C
C  Text attributes.
C
      IPRIM = 3
      AGPEND(IPRIM) = .FALSE.
      DO 150 I=IVTXIX,IVTXCI
C
        IOFF = IABS(IP2AEA(I))
        JOFF = IOFF + IL2AEA(I) - 1
C
C  Is the requested value different from the sent?
C
        IF (IP2AEA(I) .GT. 0) THEN
          VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
          DO 110 J=IOFF+1,JOFF
            VALCHG(I) = VALCHG(I) .OR. MSAEQV(J).NE.MRAEQV(J)
  110     CONTINUE
        ELSE
          VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
          DO 120 J=IOFF+1,JOFF
            VALCHG(I) = VALCHG(I) .OR. ASAEQV(J).NE.ARAEQV(J)
  120     CONTINUE
        ENDIF
C
C  Update aggregate change parameter.
C
        AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
C
  150 CONTINUE
C
C  Fill area attributes.
C
      IPRIM = 4
      AGPEND(IPRIM) = .FALSE.
      DO 160 I=IVFAIX,IVFACI
C
        IOFF = IABS(IP2AEA(I))
        JOFF = IOFF + IL2AEA(I) - 1
C
C  Is the requested value different from the sent?
C
        IF (IP2AEA(I) .GT. 0) THEN
          VALCHG(I) = MSAEQV(IOFF).NE.MRAEQV(IOFF)
          DO 170 J=IOFF+1,JOFF
            VALCHG(I) = VALCHG(I) .OR. MSAEQV(J).NE.MRAEQV(J)
  170     CONTINUE
        ELSE
          VALCHG(I) = ASAEQV(IOFF).NE.ARAEQV(IOFF)
          DO 180 J=IOFF+1,JOFF
            VALCHG(I) = VALCHG(I) .OR. ASAEQV(J).NE.ARAEQV(J)
  180     CONTINUE
        ENDIF
C
C  Update aggregate change parameter.
C
        AGPEND(IPRIM) = AGPEND(IPRIM) .OR. VALCHG(I)
  160 CONTINUE
C
C  Update aspect source flag context.
C
      DO 190 I=1,13
        ID(I) = MRASF(I)
  190 CONTINUE
      CALL GWPASF
C
      RETURN
      END
