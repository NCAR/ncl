C
C	$Id: g01iac.f,v 1.7 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01IAC
C
C  Initialize the attribute context, as at open workstation.
C
      include 'g01prm.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01dfa.h'
      include 'g01adc.h'
C
      INTEGER  IX
C
C  Compute the length of each attribute, based on the pointers.
C
      DO 5 IX=1,25
        IL2AEA(IX) = ABS(IP2AEA(IX+1)) - ABS(IP2AEA(IX))
    5 CONTINUE
C
C  Copy default attribute context to "SET" context.
C
      CALL G01D2S
C
C  Copy default attribute context to "REQUESTED" context.
C
      CALL G01D2R
C
C  Initialize attribute deferral scheme.
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
