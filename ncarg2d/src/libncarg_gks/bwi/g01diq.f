C
C	$Id: g01diq.f,v 1.5 2006-03-30 00:45:02 fred Exp $
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
      SUBROUTINE G01DIQ
C
C  Workstation description table (WDT) inquiry.
C  (OPCODE -100 thru -199)
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01wdt.h'
      include 'g01ins.h'
C
      INTEGER  ICODE
C
C
C  All defined WDT inquiry codes lie in range -11o thru -128.
C  Only -127 is legal for MO.  The rest are error 31 or 39,
C  except -125 and -126, which are undefined.  
C
      ICODE = ABS(MCODES) - 109
C
C      CODE -110 -111 -112 -113 -114 -115 -116 -117 -118 -119
      GOTO  ( 39,  39,  39,  39,  31,  39,  39,  39,  39,  39,
     +        39,  39,  39,  39,  39,   5,   5, 127,  31     )  ICODE
C      CODE -120 -121 -122 -123 -124 -125 -126 -127 -128
C
C  Fall through is undefined opcode.
C
    5 CONTINUE
      RERR = 320
      RETURN
C
C  Error 31, "SPECIFIED WORKSTATION IS OF CATEGORY MO".
C
   31 CONTINUE
      RERR = 31
      RETURN
C
C  Error 39, "SPECIFIED WORKSTATION IS NEITHER OF CATEGORY OUTPUT
C  NOR OF CATEGORY OUTIN".
C
   39 CONTINUE
      RERR = 39
      RETURN
C
C  Inquire workstation category.
C
  127 CONTINUE
      ID(2) = LWKCAT
C
      RETURN
      END
