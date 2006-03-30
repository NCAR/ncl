C
C	$Id: g01iac.f,v 1.6 2006-03-30 00:45:03 fred Exp $
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
