C
C	$Id: gwiiac.f,v 1.5 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GWIIAC
C
C  Initialize attribute context.
C
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadf.h'
      include 'gwiadc.h'
C
      INTEGER  IX
C
C  Compute the length of each attribute, based on pointers.
C
      DO 5 IX=1,25
        IL2AEA(IX) = ABS(IP2AEA(IX+1)) - ABS(IP2AEA(IX))
    5 CONTINUE
C
C  Copy the default attribute context to "SET" context.
C
      CALL GWID2S
C
C  Copy the default attribute context to "REQUESTED" context.
C
      CALL GWID2R
C
C  Initialize attribute deferral scheme.
C
C
C  Logical change variable for each primitive.
C
      DO 10 IX=1,4
        AGPEND(IX) = .FALSE.
   10 CONTINUE
C
C  Value change variables for each attribute.
C
      DO 20 IX=1,24
        VALCHG(IX) = .FALSE.
   20 CONTINUE
C
C  Aggregate ASF value change variable.
C
      ANYASF = .FALSE.
C
C  Value change variables for each ASF.
C
      DO 30 IX=1,13
        ASFCHG(IX) = .FALSE.
   30 CONTINUE
C
      RETURN
      END
