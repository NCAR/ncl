C
C $Id: agdshn.f,v 1.5 2006-03-09 22:56:05 kennison Exp $
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
      CHARACTER*16 FUNCTION AGDSHN (IDSH)
C
C The value of this function is the name of the dash pattern numbered
C IDSH - that is to say, the character string 'DASH/PATTERN/n.', where
C n is an integer between 1 and 99, equal to MAX(1,MIN(99,IDSH)).
C
      AGDSHN='DASH/PATTERN/  .'
C
      KDSH=MAX(1,MIN(99,IDSH))
C
      DO 101 I=15,14,-1
        AGDSHN(I:I)=CHAR(ICHAR('0')+MOD(KDSH,10))
        IF (KDSH.LE.9) GO TO 102
        KDSH=KDSH/10
  101 CONTINUE
C
  102 RETURN
C
      END
